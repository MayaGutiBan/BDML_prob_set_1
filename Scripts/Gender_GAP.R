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
                  otro_trabajo + relab + sizeFirm + totalHoursWorked, data = db_geih)
                  
#7495 El mes pasado, ¿recibió pagos por concepto de arriendos y/o pensiones?
#college
#oficio
stargazer(con_model,type="text",digits=7)                
                  
# 4bi. FWL with bootstrap
db_geih$female <- as.numeric(as.character(db_geih$female))  # Convert factor to numeric
res_female <- lm(female ~ age + I(age^2) + cuentaPropia + estrato1 +
                   formal + maxEducLevel + parentesco_jhogar + tiempo_trabajando +
                   otro_trabajo + relab + sizeFirm + totalHoursWorked, data = db_geih)$residuals
#This isolates the variation in female that is not explained by the other covariates.

res_log_ingtot <- lm(log_ingtot ~ age + I(age^2) + cuentaPropia + estrato1 +
                       formal + maxEducLevel + parentesco_jhogar + tiempo_trabajando +
                       otro_trabajo + relab + sizeFirm + totalHoursWorked, 
                     data = db_geih)$residuals
#This isolates the variation in log_ingtot that is not explained by the same set of independent variables.

fwl_model <- lm(res_log_ingtot ~ res_female)
summary(fwl_model)

stargazer(con_model,fwl_model,type="text",digits=7)
