##########################################################
#Problem set 1: predicting income
#Authors: Grupo 12
#Script description: Predicting earnings

##########################################################

# Clean the workspace -----------------------------------------------------
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


# Load Packages -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
p_load(rio, # import/export data
       tidyverse, # tidy-data
       caret, # For predictive model assessment
       gridExtra, # arrange plots
       skimr, # summarize data 
       stargazer, #model viz and descriptive statistics
       rvest,
       ggcorrplot,
       ggplot2,
       broom,
       knitr,
       mosaic,
       stats,
       boot
) 

# Cargar datos 
db_geih <- read_rds("stores/clean_GEIH.rds")

# 5. Predicting earnings ---------------------------------------------------------------

#Testing and training
set.seed(123)  # Set set for replicability purposes 

inTrain <- createDataPartition(
  y = db_geih$log_ingtot,  ## the outcome data are needed
  p = .70, ## The percentage of training data
  list = FALSE
)

training <- db_geih %>% 
  filter(row_number() %in% inTrain)

testing  <- db_geih %>% 
  filter(!row_number() %in% inTrain)

# Create data for visualization
split_data <- data.frame(
  Split = factor(c("Training", "Testing")),
  Count = c(nrow(training), nrow(testing)),
  Percentage = c(nrow(training)/nrow(db_geih)*100, nrow(testing)/nrow(db_geih)*100)
)

# Create the visualization
train_test <- ggplot(split_data, aes(x = Split, y = Count)) +
  geom_bar(stat = "identity", fill = "darkblue", width = 0.5) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%\n(n=", Count, ")")), 
            vjust = -0.5, color = "black", size = 4) +
  labs(title = "Train-Test Split Distribution",
       y = "Number of Observations",
       x = "") +
  theme_bw() +
  ylim(0, max(split_data$Count) * 1.2)  # Add some space for labels

ggsave("views/train_test.png", plot = train_test, width = 8, height = 6, dpi = 300)

#Train models

# Define different model formulas
formulas <- list(
  form_1 = log_ingtot  ~  age + female, #simple linear, categorical
  form_2 = log_ingtot  ~  age + I(age^2),  # Adding non-linearity from point 3
  form_3 = log_ingtot ~ female,  # Non conditional from point 4
  form_4 = log_ingtot ~ female + age + I(age^2) + cuentaPropia + estrato1 +
    formal + maxEducLevel + parentesco_jhogar + tiempo_trabajando +
    otro_trabajo + relab + sizeFirm + totalHoursWorked, #conditional model from point 4
  form_5 = log_ingtot  ~  age + I(age^2) + female + totalHoursWorked +I(totalHoursWorked^2), ##Adding non-linearity 
  form_6 = log_ingtot ~ age + I(age^2) + female + totalHoursWorked*sizeFirm,
  form_7 = log_ingtot ~ female + age + I(age^2) + cuentaPropia + estrato1 +
    formal + maxEducLevel + parentesco_jhogar + tiempo_trabajando +
    otro_trabajo + relab + sizeFirm + totalHoursWorked + totalHoursWorked*sizeFirm
) # Interaction effect


# Initialize a list to store RMSE scores
rmse_scores <- data.frame(Model = character(), RMSE = numeric(), stringsAsFactors = FALSE)

db_geih <- db_geih %>% 
  mutate(maxEducLevel = as.factor(round(as.numeric(as.character(maxEducLevel)))))

# Loop through each model, predict, compute RMSE and store results
for (i in seq_along(formulas)) {
  model <- lm(formulas[[i]], data = training)
  predictions <- predict(model, testing)
  score <- RMSE(predictions, testing$log_ingtot)
  rmse_scores <- rbind(rmse_scores, data.frame(Model = names(formulas)[i], RMSE = score))
}

print(rmse_scores)

### 5c Looking for tax evasion individuals with the best performing model

set.seed(123)  # For reproducibility

## create a training and test sample 
trainIndex <- createDataPartition(db_geih$log_ingtot, p = 0.8, list = FALSE)
train_data <- db_geih[trainIndex, ]
test_data <- db_geih[-trainIndex, ]

# we choose the best performing model 
form_7 = lm(log_ingtot ~ female + age + I(age^2) + cuentaPropia + estrato1 +
              formal + maxEducLevel + parentesco_jhogar + tiempo_trabajando +
              otro_trabajo + relab + sizeFirm + totalHoursWorked, data = train_data )

## make predictions on test set 
predictions <- predict(form_7, newdata = test_data)

## compute errors 
errors <- test_data$log_ingtot - predictions

## show distribution of erros
df_errors <- data.frame(errors = errors)

# Crear el histograma con ggplot
dist_errors <- ggplot(df_errors, aes(x = errors)) +
  geom_histogram(fill = "lightblue", bins = 40, color = "black") +
  labs(title = "Distribution of Prediction Errors from log(total income)",
       x = "Error", 
       y = "Frequency") +
  theme_bw()

# Guardar la imagen en la carpeta views
ggsave("views/dist_errors.png", plot = dist_errors, width = 8, height = 6, dpi = 300)


## look for outliers 
## we look for individuals wich observed log_inctot is smaller than its predicted ones 
low <- quantile(errors, 0.001) 


dist_errors_q1 <- ggplot(data = data.frame(errors), aes(x = errors)) +
  geom_histogram(binwidth = 0.2, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Prediction Errors",
       x = "Error",
       y = "Frequency") +
  theme_minimal() + 
  geom_vline(aes(xintercept = low, linetype = "0.1% percentile"), color = "lightblue", linewidth = 1) 

ggsave("views/dist_errors_q1.png", plot = dist_errors_q1, width = 8, height = 6, dpi = 300)



#5d. LOOCV
form_4 = log_ingtot ~ female + age + I(age^2) + cuentaPropia + estrato1 +
  formal + maxEducLevel + parentesco_jhogar + tiempo_trabajando +
  otro_trabajo + relab + sizeFirm + totalHoursWorked

ctrl <- trainControl(
  method = "LOOCV") ## input the method Leave One Out Cross Validation

# Start timing
start_time <- Sys.time()

# Get total number of observations for progress tracking
n_obs <- nrow(db_geih)
cat("Starting LOOCV training with", n_obs, "iterations...\n")

# Train model with progress printing
ctrl$verboseIter <- TRUE  # Enable progress printing
modelo4d <- train(form_4,
                  data = db_geih,
                  method = 'lm', 
                  trControl = ctrl)

# Calculate and display timing
end_time <- Sys.time()
training_time <- difftime(end_time, start_time, units = "mins")
cat("\nLOOCV training completed in:", round(training_time, 2), "minutes\n")
cat("Average time per fold:", round(training_time/n_obs, 4), "minutes\n")
