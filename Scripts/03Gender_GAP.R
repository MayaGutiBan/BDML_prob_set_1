##########################################################
#Problem set 1: predicting income
#Authors: Grupo 12
#Script description: Understanding the gender gap

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
       boot,
       ggbeeswarm,
       ggdist,
       gghalves,
       moments,
       corrplot,
       visdat,
       kabbleExtra
) 

# Cargar datos 
db_geih <- read_rds("stores/GEIH_final.rds")
# 4. The gender earnings GAP ---------------------------------------------------------------
#Unconditional wage gap
# Fit the regression model
uncon_model <- lm(db_geih$log_ingtot ~ db_geih$female)
summary(uncon_model)

# Tidy output with p-values and confidence intervals
tidy(uncon_model)


#Conditional wage gap model
# 4bi. FWL
con_model <- lm(log_ingtot ~ female + age + I(age^2) + cuentaPropia + estrato1 +
                  formal + maxEducLevel + parentesco_jhogar + tiempo_trabajando +
                  otro_trabajo + relab + sizeFirm + totalHoursWorked +
                  oficio + otro_ingreso + nmenores, data = db_geih)
                  
stargazer(uncon_model, con_model,
          title = "Gender wage regression",
          type = "text",
          style = "aer",
          header = FALSE,
          column.labels = c("Unconditional Model", "Conditional Model"),
          omit = c("age", "I(age^2)", "cuentaPropia", "estrato1",
                  "formal", "maxEducLevel", "parentesco_jhogar", "tiempo_trabajando",
                  "otro_trabajo", "relab", "sizeFirm", "totalHoursWorked",
                  "oficio", "otro_ingreso", "nmenores"),
          add.lines = list(c("Controls", "NO", "YES")),
          notes.align = "l",
          notes.append = TRUE,
          notes = "Controls for age, education, experience, sector, job title, hours worked, firm size and children.",
          out = "reg_genderwage.tex") 
# 4bi. FWL
db_geih$female <- as.numeric(as.character(db_geih$female))  # Convert factor to numeric
res_female <- lm(female ~ age + I(age^2) + cuentaPropia + estrato1 +
                   formal + maxEducLevel + parentesco_jhogar + tiempo_trabajando +
                   otro_trabajo + relab + sizeFirm + totalHoursWorked + oficio + otro_ingreso + nmenores, data = db_geih)$residuals
#This isolates the variation in female that is not explained by the other covariates.

res_log_ingtot <- lm(log_ingtot ~ age + I(age^2) + cuentaPropia + estrato1 +
                       formal + maxEducLevel + parentesco_jhogar + tiempo_trabajando +
                       otro_trabajo + relab + sizeFirm + totalHoursWorked + oficio + otro_ingreso + nmenores, 
                     data = db_geih)$residuals
#This isolates the variation in log_ingtot that is not explained by the same set of independent variables.

fwl_model <- lm(res_log_ingtot ~ res_female)
summary(fwl_model)

stargazer(con_model,fwl_model,
          title = "Gender wage regression using FWL theorem",
          type = "text",
          style = "aer",
          header = FALSE,
          column.labels = c("Conditional Model", "Conditional model with FWL"),
          omit = c("age", "I(age^2)", "cuentaPropia", "estrato1",
                  "formal", "maxEducLevel", "parentesco_jhogar", "tiempo_trabajando",
                  "otro_trabajo", "relab", "sizeFirm", "totalHoursWorked",
                  "oficio", "otro_ingreso", "nmenores"),
          add.lines = list(c("Controls", "NO", "YES")),
          notes.align = "l",
          notes.append = TRUE,
          notes = "Controls for age, education, experience, sector, job title, hours worked, firm size and children.",
          out = "reg_FWLgenderwage.tex"
          ) 
## 4bii. FWL with bootstrap

# Load necessary library
library(boot)

# Define bootstrap function, same process of the FWL estimation
fwl_bootstrap <- function(data, indices) {
  boot_data <- data[indices, ] 
 
  res_female <- lm(female ~ age + I(age^2) + cuentaPropia + estrato1 +
                     formal + maxEducLevel + parentesco_jhogar + tiempo_trabajando +
                     otro_trabajo + relab + sizeFirm + totalHoursWorked, data = boot_data)$residuals
   
  res_log_ingtot <- lm(log_ingtot ~ age + I(age^2) + cuentaPropia + estrato1 +
                formal + maxEducLevel + parentesco_jhogar + tiempo_trabajando +
                otro_trabajo + relab + sizeFirm + totalHoursWorked, data = boot_data)$residuals
  
  boot_model <- lm(res_log_ingtot ~ res_female)
  
  # Return the coefficient estimate for female
  return(coef(boot_model)[2])
}

# Run bootstrap with 1000 replications
set.seed(424)  # For reproducibility
boot_results <- boot(data = db_geih, statistic = fwl_bootstrap, R = 1000)

# Get bootstrap estimate and standard error
boot_results
boot.ci(boot_results, type = "perc")

boot_coef <- mean(boot_results$t)  # Mean bootstrap estimate
boot_se <- sd(boot_results$t)  


### 4c. plot the predicted age-wage profile and estimate the implied “peak ages” with the respective confidence intervals 


## predict log inc 

db_geih$predict_loginc <- predict(con_model, newdata = db_geih)

# Function to estimate peak age in each bootstrap sample 
peak_age_boot <- function(data, indices, gender) {
  d <- data[indices, ]
  
  # Subset data by gender (0 = male, 1 = female)
  d_gender <- d[d$female == gender, ]
  
  model <- lm(
    log_ingtot ~ age + I(age^2) + cuentaPropia + estrato1 + 
      formal + maxEducLevel + parentesco_jhogar + tiempo_trabajando + 
      otro_trabajo + relab + sizeFirm + totalHoursWorked, 
    data = d_gender
  )
  
  coef_model <- coef(model)
  b_age <- coef_model["age"]
  b_age_sq <- coef_model["I(age^2)"]
  
  # Calculate peak age (-b_age / (2*b_age_sq))
  peak_age <- -b_age / (2 * b_age_sq)
  return(peak_age)
}

# Run bootstrap for males and females
B <- 1000
boot_male <- boot(db_geih, statistic = peak_age_boot, R = B, gender = 0)
boot_female <- boot(db_geih, statistic = peak_age_boot, R = B, gender = 1)

# Get confidence intervals
ci_male <- boot.ci(boot_male, type = "perc")
ci_female <- boot.ci(boot_female, type = "perc")


##### Age-wage predicted plot with CI by gender 

## etxtract the CI 
ci_female_low <- ci_female$percent[4]
ci_female_up <- ci_female$percent[5]

ci_male_low <- ci_male$percent[4]
ci_male_up <- ci_male$percent[5]

## lablel var for better looking graph 
db_geih$gender <- factor(db_geih$gender, labels = c("Female", "Male"))

## Plot 
ggplot(db_geih, aes(x = age, y = predict_loginc, color = gender)) +
  geom_smooth(method = "loess", se = TRUE) + 
  
  # Confidence Interval (CI) for Peak Age (Male)
  geom_ribbon(aes(xmin = ci_male_low, xmax = ci_male_up, fill = "Male CI"), alpha = 0.3) + 
  # Confidence Interval (CI) for Peak Age (Female)
  geom_ribbon(aes(xmin = ci_female_low, xmax = ci_female_up, fill = "Female CI"), alpha = 0.3) + 
  
  # Peak Age Vertical Lines
  geom_vline(aes(xintercept = boot_male$t0, linetype = "Male Peak Age"), color = "blue", linewidth = 1) +
  geom_vline(aes(xintercept = boot_female$t0, linetype = "Female Peak Age"), color = "red", linewidth = 1) +
  
  # Define Legends
  scale_fill_manual(name = "Confidence Intervals", values = c("Male CI" = "blue", "Female CI" = "red")) +
  scale_linetype_manual(name = "Predicted Peak Age", values = c("Male Peak Age" = "dashed", "Female Peak Age" = "dashed")) +
  scale_color_manual(name = "Gender", values = c("Male" = "blue", "Female" = "red")) +
  
  labs(title = "Predicted Age-Wage Profile by Gender",
       x = "Age",
       y = "Log(Predicted Income)") +
  theme_minimal() +
  guides(
    fill = guide_legend(order = 1), # Ensure CI is in legend
    linetype = guide_legend(order = 2), # Ensure peak age lines are in legend
    color = guide_legend(order = 3) # Gender legend
  )





