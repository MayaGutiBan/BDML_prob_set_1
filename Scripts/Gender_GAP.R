###### Gender GAP Model 

### We find employed persons with wage = 0, so we erease those observations 

df_0 <- df %>% filter(ingtot > 0)

low <- quantile(df_0$ln_Inc, 0.01)
up <- quantile(df_0$ln_Inc, 0.99)


d <-ggplot(data= df_0, 
          mapping = aes(y=ln_Inc , x="")) +
  theme_bw() +
  geom_boxplot()  +
  ggtitle("")+
  ylab("Ingreso Total")+
  xlab("")


d <- d + geom_hline(yintercept = low ,linetype="solid",color="red",linewidth=0.7) +
  geom_hline(yintercept = up ,linetype="solid",color="red",linewidth=0.7)

print(d)

# Create the log of the wage 
df<- df %>%
  mutate(ln_wage = log(ingtot) )

#Create the model 

Model2 <- lm(ln_wage ~ sex, data = df_0)
summary(Model2)

beta_2 <- coef(Model2)

