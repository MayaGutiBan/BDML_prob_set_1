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
ggplot(split_data, aes(x = Split, y = Count)) +
  geom_bar(stat = "identity", fill = "darkblue", width = 0.5) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%\n(n=", Count, ")")), 
            vjust = -0.5, color = "black", size = 4) +
  labs(title = "Train-Test Split Distribution",
       y = "Number of Observations",
       x = "") +
  theme_bw() +
  ylim(0, max(split_data$Count) * 1.2)  # Add some space for labels

#Train models

# Define different model formulas
formulas <- list(
  form_1 = log_ingtot  ~  age + female, #simple linear, categorical
  form_2 = log_ingtot  ~  age + I(age^2),  # Adding non-linearity from point 3
  form_3 = log_ingtot ~ female,  # Non conditional from point 4
  form_4 = log_ingtot ~ female + age + I(age^2) + cuentaPropia + estrato1 +
    formal + maxEducLevel + parentesco_jhogar + tiempo_trabajando +
    otro_trabajo + relab + sizeFirm + totalHoursWorked, #conditional model from point 4
  form_5 = totalHoursWorked ~ log_ingtot * age + gender  # Interaction effect
)

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
                  data = db,
                  method = 'lm', 
                  trControl = ctrl)

# Calculate and display timing
end_time <- Sys.time()
training_time <- difftime(end_time, start_time, units = "mins")
cat("\nLOOCV training completed in:", round(training_time, 2), "minutes\n")
cat("Average time per fold:", round(training_time/n_obs, 4), "minutes\n")
