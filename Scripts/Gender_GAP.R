###### Gender GAP Model 

### We find employed persons with wage = 0, so we erease those observations 

df_0 <- df %>% filter(ingtot > 0)

# Create the log of the wage 
df<- df %>%
  mutate(ln_wage = log(ingtot) )

#Create the model 

Model2 <- lm(ln_wage ~ sex, data = df_0)
summary(Model2)

beta_2 <- coef(Model2)

