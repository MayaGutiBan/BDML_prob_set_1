##########################################################
#Problem set 1: predicting income
#Authors: Grupo 12
#Script description: Wage age profile and age peak

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
db_geih <- read_rds("stores/clean_GEIH.rds")
head(db_geih, 5)

 # Crear una tabla con kable
skim(db_geih)

sum(is.na(db_geih)) 

stargazer(as.data.frame(db_geih), type="text")
db_geih <- db_geih %>% mutate(age2 = age^2) # Se crea la edad al cuadrado para facilidad más adelante 

# 3. Age-wage profile ---------------------------------------------------------------

# Fit the regression model
reg_1 <- lm(log_ingtot ~ age + age2, data = db_geih)

reg_2 <- lm(log_ingtot ~ age, data = db_geih)


scatter_fit <- ggplot(db_geih, aes(y = log_ingtot, x = age)) +
  geom_point() +  # Agregar puntos
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "blue") +  # Línea cuadrática
  geom_smooth(method = "lm", formula = y ~ poly(x, 1), se = FALSE, color = "red") +
  theme_bw() +  # Tema en blanco y negro
  labs(x = "Age",  
       y = "Log total income")
#       ,title = "Scatter Edad y Salario")  # Etiquetas

ggsave("views/income_age_scatter.png", plot = scatter_fit, width = 8, height = 6, dpi = 300)


stargazer(reg_2, reg_1, type="text",
          covariate.labels=c("age","age2"),
          title = "Tabla 1: Income age regression",
          out = "views/regresion_income_age.tex")


# Calcular el máximo con sus intervalos de confianza con bootstrap

theta_fn_max <- function(data, index) {
  coefs <- coef(lm(log_ingtot ~ age + age2, data = data, subset = index))
  -coefs["age"] / (2 * coefs["age2"])  # Fórmula del vértice
}

set.seed(1001)
boot_max <- boot(db_geih, theta_fn_max, R = 1000)

boot_values <- boot_max$t
mean(boot_values)

# Graficar la distribución
dist_age_peak <- ggplot(data.frame(boot_values), aes(x = boot_values)) +
  geom_histogram(color = "black", fill = "skyblue", bins = 30) +
  geom_vline(xintercept = mean(boot_values), color = "red", linetype = "dashed", size = 1) +
  labs(x = "Age",
       y = "Frequency") +
  theme_bw()

# title = "Distribución Bootstrap de la Edad Óptima",

ggsave("views/dist_age_peak.png", plot = dist_age_peak, width = 8, height = 6, dpi = 300)

boot.ci(boot_max, type = c("perc"))

