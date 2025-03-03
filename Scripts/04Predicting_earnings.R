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
  form_1 = log_ingtot  ~  female, #simple linear, categorical
  form_2 = log_ingtot  ~  age + I(age^2),  # Adding non-linearity from point 3
  form_3 = log_ingtot ~ female + age + I(age^2) + cuentaPropia + estrato1 +
    formal + maxEducLevel + parentesco_jhogar + tiempo_trabajando +
    otro_trabajo + relab + sizeFirm + totalHoursWorked + oficio + otro_ingreso + nmenores, #conditional model from point 4
  form_4 = log_ingtot  ~  female * age + I(age^2) + cuentaPropia + cuentaPropia * female + 
    estrato1 + estrato1*female + formal + maxEducLevel + parentesco_jhogar + tiempo_trabajando + 
    otro_trabajo + relab + sizeFirm + totalHoursWorked + oficio + otro_ingreso + nmenores, # Interaction effect
  form_5 = log_ingtot ~ female + age + I(age^2) + I(age^3) + I(age^4) + cuentaPropia + estrato1 +
    formal + maxEducLevel + parentesco_jhogar + tiempo_trabajando +
    otro_trabajo + relab + sizeFirm + totalHoursWorked + oficio + otro_ingreso + nmenores,
  form_6= log_ingtot ~ female + log(age) + cuentaPropia + estrato1 +
    formal + maxEducLevel + parentesco_jhogar + tiempo_trabajando +
    otro_trabajo + relab + sizeFirm + log(totalHoursWorked) + oficio + otro_ingreso + nmenores
)

models <- lapply(formulas, function(f) lm(f, data = db_geih))


# Initialize a list to store RMSE scores
rmse_values <- numeric(length(formulas))

# Loop through each formula
for (i in seq_along(formulas)) {
  model <- lm(formulas[[i]], data = training)
  predictions <- predict(model, newdata = testing)
  rmse_values[i] <- RMSE(predictions, testing$log_ingtot)
  cat("RMSE for model", names(formulas)[i], ":", rmse_values[i], "\n")
}

rmse_values_rounded <- round(rmse_values, 3)

stargazer(models,
          title = "Model comparison",
          type = "text",
          style = "aer",
          header = FALSE,
          omit = c("age", "I(age^2)", "cuentaPropia", "estrato1",
                   "formal", "maxEducLevel", "parentesco_jhogar", "tiempo_trabajando",
                   "otro_trabajo", "relab", "sizeFirm", "totalHoursWorked",
                   "oficio", "otro_ingreso", "nmenores"),
          add.lines = list(
            c("Controls", "NO", "NO", "YES", "YES", "YES", "YES"),
            c("Test set RMSE", rmse_values_rounded),
            c("Degrees of Freedom", "df = 1; 16540", "df = 2; 16539", "df = 119; 16422", 
              "df = 126; 16415", "df = 121; 16420", "df = 118; 16423")
          ),
          notes.align = "l",
          notes.append = TRUE,
          notes = "Controls for age, education, experience, sector, job title, hours worked, firm size and children.",
          column.sep.width = "1pt", # Reduce space between columns
          font.size = "small", # Reduce font size
          single.row = TRUE, # Combine coefficients and standard errors into one row
          out = "models.tex"
)

#Prediction errors
# Fit Model 6
model_6 <- lm(log_ingtot ~ female + log(age) + cuentaPropia + estrato1 +
                formal + maxEducLevel + parentesco_jhogar + tiempo_trabajando +
                otro_trabajo + relab + sizeFirm + log(totalHoursWorked) + oficio + 
                otro_ingreso + nmenores, data = training)

prediction_errors <- testing$log_ingtot - predictions

# Define outliers as observations with errors greater than ±2 standard deviations
outlier_threshold <- 2 * sd(prediction_errors)
outliers <- abs(prediction_errors) > outlier_threshold

# Extract outlier data
outlier_data <- testing[outliers, ]

# Add prediction errors to the outlier data
outlier_data$prediction_error <- prediction_errors[outliers]
outlier_data$predictions <- predictions[outliers]


ggplot(data.frame(prediction_errors), aes(x = prediction_errors)) +
  geom_histogram(bins = 50, fill = "blue", color = "black") +
  geom_vline(xintercept = c(-outlier_threshold, outlier_threshold), color = "red", linetype = "dashed") +
  labs(title = "Distribution of Prediction Errors",
       x = "Prediction Error",
       y = "Frequency") +
  theme_minimal()
ggsave("outlier_hist.pdf")


# Compare the distribution of error terms of a specific variable (e.g., age)
# Define the function to create density plots
create_density_plot <- function(data, outlier_data, x_var, x_label, show_legend = FALSE) {
  p <- ggplot() +
    geom_density(data = data, aes(x = .data[[x_var]], color = "Full Test Set"), size = 1) +
    geom_density(data = outlier_data, aes(x = .data[[x_var]], color = "Outliers"), size = 1) +
    scale_color_manual(name = "Group", values = c("Full Test Set" = "blue", "Outliers" = "red")) +
    theme_minimal()
  
  # Suppress the legend if show_legend is FALSE
  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  }
  
  return(p)
}

a <- create_density_plot(testing, outlier_data, "tiempo_trabajando", "Experiencia")
b <- create_density_plot(testing, outlier_data, "age", "Age")
c <- create_density_plot(testing, outlier_data, "totalHoursWorked", "Total Hours Worked", show_legend = TRUE)
p_grid <- grid.arrange(a, b, c, ncol = 3)
ggsave("Test_var_comparison.pdf", plot = p_grid, width = 8.27)  # A4 size in inches


outlier_summary <- outlier_data[, c("log_ingtot", "female", "age", "maxEducLevel", "totalHoursWorked", "prediction_error")]

# Scatter plot of actual vs. predicted values, highlighting outliers
ggplot() +
  geom_point(data = testing, aes(x = log_ingtot, y = predictions, color = "Normal"), alpha = 0.5) +
  geom_point(data = outlier_data, aes(x = log_ingtot, y = predictions, color = "Outliers"), size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Actual vs. Predicted log_ingtot",
       x = "Actual log_ingtot",
       y = "Predicted log_ingtot") +
  scale_color_manual(name = "Group", values = c("Normal" = "cornflowerblue", "Outliers" = "brown1")) +
  theme_minimal()
ggsave("outlier_dist.pdf")

stargazer(outlier_data,
          type = "text")

#5d. LOOCV
form_5 = log_ingtot ~ female + age + I(age^2) + I(age^3) + I(age^4) + cuentaPropia + estrato1 +
  formal + maxEducLevel + parentesco_jhogar + tiempo_trabajando +
  otro_trabajo + relab + sizeFirm + totalHoursWorked + oficio + otro_ingreso + nmenores
form_6= log_ingtot ~ female + log(age) + cuentaPropia + estrato1 +
  formal + maxEducLevel + parentesco_jhogar + tiempo_trabajando +
  otro_trabajo + relab + sizeFirm + log(totalHoursWorked) + oficio + otro_ingreso + nmenores

ctrl <- trainControl(
  method = "LOOCV") ## input the method Leave One Out Cross Validation

# Start timing
start_time <- Sys.time()

# Get total number of observations for progress tracking
n_obs <- nrow(db_geih)
cat("Starting LOOCV training with", n_obs, "iterations...\n")

# Train model with progress printing
ctrl$verboseIter <- TRUE  # Enable progress printing
modelo4d <- train(form_5,
                  data = db_geih,
                  method = 'lm', 
                  trControl = ctrl)

# Calculate and display timing
end_time <- Sys.time()
training_time <- difftime(end_time, start_time, units = "mins")
cat("\nLOOCV training completed in:", round(training_time, 2), "minutes\n")
cat("Average time per fold:", round(training_time/n_obs, 4), "minutes\n")

#save validation set RMSE
validation_rmse_model_5 <- modelo4d$results$RMSE

##Levarage calculation
# Calculate Cook's distance
cooks_distance_5 <- cooks.distance(model_5)

# Identify influential observations
influential_obs_5 <- which(cooks_distance_5 > 4 / nrow(training)) # Threshold for Cook's distance

###LOOCV modelo 6
ctrl <- trainControl(
  method = "LOOCV") ## input the method Leave One Out Cross Validation

# Start timing
start_time <- Sys.time()

# Get total number of observations for progress tracking
n_obs <- nrow(db_geih)
cat("Starting LOOCV training with", n_obs, "iterations...\n")

# Train model with progress printing
ctrl$verboseIter <- TRUE  # Enable progress printing
modelo6d <- train(form_6,
                  data = db_geih,
                  method = 'lm', 
                  trControl = ctrl)

# Calculate and display timing
end_time <- Sys.time()
training_time <- difftime(end_time, start_time, units = "mins")
cat("\nLOOCV training completed in:", round(training_time, 2), "minutes\n")
cat("Average time per fold:", round(training_time/n_obs, 4), "minutes\n")

##Levarage calculation
# Calculate Cook's distance
cooks_distance_6 <- cooks.distance(model_6)

# Identify influential observations
influential_obs_6 <- which(cooks_distance_6 > 4 / nrow(training)) # Threshold for Cook's distance


#save validation set RMSE
validation_rmse_model_6 <- modelo6d$results$RMSE

test_error_model_5 <- RMSE(predict(model_5, newdata = testing), testing$log_ingtot)
test_error_model_6 <- RMSE(predict(model_6, newdata = testing), testing$log_ingtot)

# Create a comparison table
comparison_table <- data.frame(
  Metric = c("Test RMSE", "Validation Set RMSE (LOOCV)", "Number of Influential Observations"),
  Model_5 = c(test_error_model_5, validation_rmse_model_5, length(influential_obs_5)),
  Model_6 = c(test_error_model_6, validation_rmse_model_6, length(influential_obs_6))
)

performance <- kable(comparison_table, 
                     caption = "Comparison of Model Performance", 
                     col.names = c("Metric", "Model 1", "Model 2"), 
                     format = "latex", 
                     booktabs = TRUE, 
                     align = c("l", "r", "r"), 
                     digits = 7) 

save_kable(performance, "model_performance.tex")
