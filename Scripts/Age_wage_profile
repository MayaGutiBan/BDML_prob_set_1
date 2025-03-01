##########################################################
#Problem set 1: predicting income
#Authors: Grupo 12
#Script description:

##########################################################

# Clean the workspace -----------------------------------------------------
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo

#set working director
setwd("~/Documents/GitHub")

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
       stats
) 

# Cargar datos 
db_geih <- read_rds("clean_GEIH.rds")
head(db_geih, 5)

# Crear una tabla con kable
skim_result <- skim(db_geih)
kable(skim_result)

sum(is.na(db_geih)) 

stargazer(as.data.frame(db_geih, type="text")
db_geih <- db_geih %>% mutate(lnwage = log(ingtot), age2 = age^2) # Se crea el ln del salario y la edad al cuadrado

# 3. Age-wage profile ---------------------------------------------------------------

# Fit the regression model
reg_1 <- lm(lnwage ~ age + age2, data = db)

reg_2 <- lm(lnwage ~ age, data = db)


scatter <- ggplot(db_geih, aes(y = lnwage, x = age)) +
  geom_point() +  # Agregar puntos
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "blue") +  # Línea cuadrática
  geom_smooth(method = "lm", formula = y ~ poly(x, 1), se = FALSE, color = "red") +
  theme_bw() +  # Tema en blanco y negro
  labs(x = "Age",  
       y = "Total income")
#       ,title = "Scatter Edad y Salario")  # Etiquetas

scatter

stargazer(reg_2, reg_1, type="text",
          covariate.labels=c("age","age2"),
          title = "Tabla 1: Relación edad-salario",
          out = "tabla_regresion.tex")


# Calcular el máximo con sus intervalos de confianza con bootstrap

theta_fn_max <- function(data, index) {
  coefs <- coef(lm(lnwage ~ age + age2, data = data, subset = index))
  -coefs["age"] / (2 * coefs["age2"])  # Fórmula del vértice
}

set.seed(1001)
boot_max <- boot(db_geih, theta_fn_max, R = 1000)

boot_values <- boot_max$t
mean(boot_values)

# Graficar la distribución
ggplot(data.frame(boot_values), aes(x = boot_values)) +
  geom_histogram(color = "black", fill = "skyblue", bins = 30) +
  geom_vline(xintercept = mean(boot_values), color = "red", linetype = "dashed", size = 1) +
  labs(x = "Age",
       y = "Frequency") +
  theme_bw()

# title = "Distribución Bootstrap de la Edad Óptima",

boot.ci(boot_max, type = c("perc"))

