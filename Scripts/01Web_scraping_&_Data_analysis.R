##########################################################
#Problem set 1: predicting income
#Authors: Grupo 12
#Script description: Web scrapping and preliminary data analysis

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
       stats,
       ggbeeswarm,
       ggdist,
       gghalves,
       moments,
       corrplot,
       visdat,
       kabbleExtra
) 
  
# Load data ---------------------------------------------------------------
html <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/page1.html")

html %>% html_table() ## Aparece vacio 

## Primer link se accede a el en network y filtrando por Fetch/XHR 

## Chunk 1 
tabla <-read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html")
my_table <- tabla %>% html_table()


# Chunk 2 
tabla_2 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_2.html")
my_table_2 <- tabla_2 %>% html_table()

tabla_2 %>% html_table()

## loop para sacar los 10 chunks 
## Ya sirvió con los 2 primeros chunks, deberia funcionar para los otros 8 

all_tables <- list()

for (i in 1:10) {
  link <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",i,".html")
  page <- read_html(link)
  
  all_tables[[i]] <- page %>% html_table()
}

summary(all_tables)

# Clean data ---------------------------------------------------------------
#Combine lists, drop under 18 and unemployed
db_geih <- all_tables %>%
  flatten() %>%  # Asegura que la lista no tenga sublistas
  bind_rows() %>% # Une todas las tablas en un solo tibble
  mutate(flag = ifelse(age <= 6, 1, 0)) %>%
  group_by(directorio, secuencia_p) %>%
  mutate(nmenores = sum(flag)) %>%
  select(-flag) %>% 
  ungroup() %>% 
  filter(age>=18 & ocu==1)

# Guardar data limpia
dir.create("stores", showWarnings = FALSE)  # Crea la carpeta si no existe
write.csv(db_geih, "stores/clean_GEIH.csv", row.names = FALSE)
list.files("stores")

#Description variables- age, sex, education, state, race, occupation, industry,

#definiciones: age, clase (=1 urban; =0 rural), college(=1 if terciary educ.;=0 otherwise), cuentaPropia (=1 if self employed; =0 otherwise), 
#depto, directorio(llave de vivienda), estrato1, formal, hoursWorkActu*, hoursWorkUsual,  informal, ingtot, maxEducLevel, mes, oficio, 
#p6050(cual es parentezco jefe de hogar), p6426(tiempo trabajado en empresa, negocio, industria), p6580s1(cuanto recibio por bonificaciones), p6870(tamano empresa), p7040 (otro trabajo), 
#relab(type of occupation), secuencia_p(llave de hogar), sex(=1 male, =0 female), sizeFirm(size of the firm by categories), totalHoursWorked(	total hours worked previous week),
#y_bonificacio~m(	Ingreso monetario en el mes), y_ingLab_m(labor income salaried - nominal monthly - all occ. (includes tips and commission)
#y_salary_m, y_salary_m_hu, y_ingLab_m, y_primaServicios_m, y_ingLab_m_ha, y_total_m, y_total_m_ha

# Cargar datos 
db_geih <- read.csv("stores/clean_GEIH.csv")
head(db_geih, 5)

variables <- c("age", "cuentaPropia", "estrato1", "formal", "ingtot", "maxEducLevel", "p6050", "p6426", "p7040", "p7495", "relab", "sex", "sizeFirm", 
              "totalHoursWorked", "nmenores", "oficio", "college", "regSalud")

variables_ing <- c("ingtot", "y_salary_m", "y_salary_m_hu", "y_ingLab_m", "y_primaServicios_m", "y_ingLab_m_ha", "y_total_m", "y_total_m_ha")

db_geih_ing <- db_geih %>%  
  select(variables_ing)

vis_dat(db_geih_ing)


stargazer(as.data.frame(db_geih[, c("clase", "depto")]), type = "text", summary = TRUE)

#Clase y depto no tienen variabilidad entonces se eliminan
variables_categoricas <- c("cuentaPropia", "estrato1", "formal", "maxEducLevel", "parentesco_jhogar", "otro_trabajo", "relab", "gender", "female", 
                           "H_Head", "sizeFirm", "oficio", "college", "regSalud", "otro_ingreso")
db_geih_1 <- db_geih_1 %>%
  mutate(across(all_of(variables_categoricas), as.factor))

variables_total <- c("age", "cuentaPropia", "estrato1", "formal", "ingtot", "maxEducLevel", "p6050", "p6426", "p7040", "p7495", "relab", "sex", "sizeFirm", 
              "totalHoursWorked", "nmenores", "college", "regSalud", "oficio")

# Inputacion medias de variables numericas, renombrar y crear variables
db_geih_1 <- db_geih %>%  
  select(variables_total) %>% 
  mutate(across(where(is.numeric), ~ replace_na(., ifelse(is.integer(.), as.integer(median(., na.rm = TRUE)), mean(., na.rm = TRUE))))) %>% 
  rename( parentesco_jhogar = "p6050",
          tiempo_trabajando = "p6426", # la variable esta en meses
          otro_trabajo = "p7040",
          otro_ingreso = "p7495",
          gender = "sex") %>% 
  mutate( female = 1-gender, ## 1 if female
          H_Head = ifelse( parentesco_jhogar== 1, 1, 0)) #Household head
          
# Clean data ---------------------------------------------------------------

ceros <- db_geih_1 %>% summarise(across(where(is.numeric), ~ sum(. == 0, na.rm = TRUE)))

stargazer(as.data.frame(db_geih_1), type = "text")
outliers <- db_geih_1 %>% filter(totalHoursWorked > 120)

low <- quantile(db_geih_1$totalHoursWorked, 0.01)
up <- quantile(db_geih_1$totalHoursWorked, 0.99)

dir.create("views", showWarnings = FALSE)  # Crea la carpeta si no existe

boxplot_plot <- ggplot(data = db_geih_1, aes(y = totalHoursWorked, x = "")) +
  theme_bw() +
  geom_boxplot(outlier.shape = NA) +  # Hide outliers to avoid overlap
  geom_jitter(width = 0.2, alpha = 0.4, color = "gray") +  # Add jittered points
  geom_hline(yintercept = low, linetype = "dashed", color = "blue", linewidth = 0.5) +
  geom_hline(yintercept = up, linetype = "dashed", color = "blue", linewidth = 0.5) +
  annotate("text", x = 1, y = low, label = paste0("Low: ", round(low,1)), vjust = -1, color = "blue") +
  annotate("text", x = 1, y = up, label = paste0("Up: ", round(up,1)), vjust = -1, color = "blue") +
  labs(y = "Total Hours Worked", x = "", title = "Boxplot of Total Hours Worked with 1st & 99th Percentile")

# Guardar la imagen en "views"
ggsave("views/boxplot_total_hours_worked.png", plot = boxplot_plot, width = 8, height = 6, dpi = 300)


variables_categoricas <- c("cuentaPropia", "estrato1", "formal", "maxEducLevel", "parentesco_jhogar", "otro_trabajo", "relab", "gender", "female", "H_Head", "sizeFirm")
db_geih_1 <- db_geih_1 %>%
  mutate(across(all_of(variables_categoricas), as.factor))

#ingreso total limpia
db_geih_1 %>%
  filter(ingtot== 0)

db_geih_1 <- db_geih_1 %>% 
  group_by(estrato1) %>% 
  mutate(mean_ingtot = mean(ingtot, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(ingtot = ifelse(ingtot==0, yes =mean_ingtot, no=ingtot),
         log_ingtot = log(ingtot)) 


## we save the final db 
write_rds(db_geih_1,"stores/GEIH_final.rds")

### DATA VISUALIZATION ---------
# Function to create a single boxplot
create_boxplot <- function(data, y_var, y_label) {
  ggplot(data, aes(y = !!sym(y_var), x = "")) +
    ylab(y_label) +
    xlab("")+
    ggdist::stat_halfeye(
      adjust = .5, 
      width = .6, 
      .width = 0, 
      justification = -.3, 
      point_colour = NA) + 
    geom_boxplot(
      width = .5, 
      outlier.shape = NA
    ) +
    geom_point(
      size = 1.5,
      alpha = .01,
      position = position_jitter(
        seed = 1, width = .1
      )
    ) 
}

# Function to create grouped boxplots with color by gender
create_grouped_boxplot <- function(data, x_var, y_var, x_label = NULL, show_legend = FALSE) {
  p <- ggplot(data, aes(x = as.factor(!!sym(x_var)), y = !!sym(y_var))) +
    labs(x = x_label, y = "Total Income (log)") +
    ggdist::stat_halfeye(
      adjust = .5, 
      width = .6, 
      .width = 0, 
      justification = -.3, 
      point_colour = NA) + 
    geom_boxplot(
      width = .5, 
      outlier.shape = NA
    ) +
    geom_point(
      size = 1.5,
      alpha = .01,
      position = position_jitter(
        seed = 1, width = .1
      )
    ) 
    labs(x = x_label, y = "Total Income (log)")
  
  # Hide legend if show_legend is FALSE
  if (!show_legend) {
    p <- p + guides(color = "none")
  }
  return(p)
}

# Create plots
a <- create_boxplot(db_geih_1, "log_ingtot", "Total Income (log)")
b <- create_boxplot(db_geih_1, "age", "Age")
c <- create_boxplot(db_geih_1, "totalHoursWorked", "Total hours worked")
d <- create_boxplot(db_geih_1, "tiempo_trabajando", "Work experience") 
e <- create_grouped_boxplot(db_geih_1, "nmenores", "Children per household") 
f <- create_grouped_boxplot(db_geih_1, "estrato1", "log_ingtot", "Socioeconomic Stratum") 
g <- create_grouped_boxplot(db_geih_1, "female", "log_ingtot", "Gender") 
h <- create_grouped_boxplot(db_geih_1, "cuentaPropia", "log_ingtot", "Self-Employed")
i <- create_grouped_boxplot(db_geih_1, "maxEducLevel", "log_ingtot", "Max Education Level")
j <- create_grouped_boxplot(db_geih_1, "formal", "log_ingtot", "Formal job") 
k <- create_grouped_boxplot(db_geih_1, "otro_trabajo", "log_ingtot", "Second job") 
l <- create_grouped_boxplot(db_geih_1, "sizeFirm", "log_ingtot", "Size of the firm") 

# Arrange all plots in a grid
p_grid <- grid.arrange(a, b, c, d, e, f, ncol = 3)
ggsave("views/histograms.pdf", plot = p_grid, width = 8.27) 

p_grid <- grid.arrange(g, h, i, j, k, l, ncol = 3)
ggsave("views/histograms_1.pdf", plot = p_grid, width = 8.27) 


# Histograms Hours, age, income
a<- ggplot(db_geih_1, aes(x = totalHoursWorked )) +
  geom_histogram(bins = 50, fill = "darkblue") +
  labs(x = "Total Hours Worked", y = "N. Obs") +
  theme_bw() 

b<- ggplot(db_geih_1, aes(x = age )) +
  geom_histogram(bins = 20, fill = "darkblue") +
  labs(x = "Age", y = "N. Obs") +
  theme_bw() 

c <- ggplot(db_geih_1, aes(x = log_ingtot)) +
  geom_histogram(bins = 50, fill = "darkblue") +
  labs(x = "Total Income (log)", y = "N. Obs") +
  theme_bw() 

arrange_plot <- grid.arrange(a,b,c, ncol = 3)
ggsave("views/arrange_2.png", plot = arrange_plot, width = 12, height = 8, dpi = 300)


#Scatterplot matrix- Convert data to long format for easy plotting
df_long <- db_geih_1 %>%
  pivot_longer(cols = c("age", "tiempo_trabajando", "totalHoursWorked"), 
               names_to = "Variable", 
               values_to = "Value")

# Plot multiple scatter plots
scatter_1 <- ggplot(df_long, aes(x = Value, y = log_ingtot)) +
  geom_point(alpha = 0.5, color = "blue") +
  facet_wrap(~ Variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "Total Income vs Other Variables", x = "Variable Value", y = "Total Income (log)")

ggsave("views/totalincome_othervar.png", plot = scatter_1, width = 8, height = 6, dpi = 300)

# Compute a correlation matrix and visualize 
cor_matrix <- db_geih_1 %>%
  select(where(is.numeric)) %>%  # Select only numeric variables
  cor(method = "spearman", use = "pairwise.complete.obs") %>%  # Handle NAs pairwise
  round(1)

corr_plot <- ggcorrplot(cor_matrix, type = "lower", lab = TRUE) +
  theme_minimal() +
  labs(x = "", y = "")

ggsave("views/corr_plot.png", plot = corr_plot, width = 8, height = 6, dpi = 300)

M <- db_geih_1 %>%
  select(-c("ingtot", "mean_ingtot", "gender"))

cor_matrix <- cor(M, use = "pairwise.complete.obs")  # Compute correlation matrix
corrplot(cor_matrix, method = "circle")
ggsave("views/corr_plot_2.png", plot = corr_plot_2, width = 8, height = 6, dpi = 300)


