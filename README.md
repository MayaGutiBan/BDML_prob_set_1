# BDML_prob_set_1

# Problem Set 1: Predicting Income  

## Project Overview  
Income prediction models play a crucial role in economic and public policy analysis, particularly in addressing issues related to tax evasion, social inequality, and fiscal sustainability. In this project, we construct a model of individual **hourly wages** using data from the 2018 **"Medición de Pobreza Monetaria y Desigualdad Report"**, which contains survey information from Bogotá’s **GEIH** dataset.  

## Team Members  
- Carlos Manjarres  
- Juan Felipe Triana  
- Sebastian Trujillo  
- Maya Gutiérrez  

## Repository Structure  
The repository is organized as follows:  
- **`documents/`** – Contains the final report of the project.  
- **`scripts/`** – Includes all R scripts used in the analysis.  
- **`stores/`** – Stores the dataset after web scraping and preprocessing.  
- **`views/`** – Contains visual outputs (figures, tables, and model results).  

## Setup Instructions  
To run this project, you need R and the following packages:  

```r
# Load required libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  rio,        # Import/export data
  tidyverse,  # Data wrangling & visualization
  caret,      # Predictive model assessment
  gridExtra,  # Arrange multiple plots
  skimr,      # Summarize data
  stargazer,  # Model visualization & descriptive stats
  rvest,      # Web scraping
  ggcorrplot, # Correlation plots
  ggplot2,    # Data visualization
  broom,      # Tidying model outputs
  knitr,      # Dynamic report generation
  mosaic,     # Statistical modeling
  stats,      # Basic statistical functions
  boot        # Bootstrap analysis
)


```r

## Running the Scripts  
There is no `main.R` script. Instead, run the scripts in the following order:  

<<<<<<< HEAD
1 **`01Web_scraping_&_Data_analysis.R`** – Scrapes the dataset and performs a preliminary inspection of the data.  ⃣
2 **`02Age_wage_profile.R`** – Estimates the age-wage profile and tests economic theory.  
3 **`03Gender_GAP.R`** – Examines the gender wage gap using the Frisch-Waugh-Lovell (FWL) theorem.  
4 **`04Predicting_earnings.R`** – Implements different models to predict income. 
=======
1️ **`01Web_scraping_&_Data_analysis.R`** – Scrapes the dataset and performs a preliminary inspection of the data.  
2️ **`02Age_wage_profile.R`** – Estimates the age-wage profile and tests economic theory.  
3️ **`03Gender_GAP.R`** – Examines the gender wage gap using the Frisch-Waugh-Lovell (FWL) theorem.  
4️ **`04Predicting_earnings.R`** – Implements different models to predict income. 
>>>>>>> f72b14a6fbd8508f44b57ef1292eac3cd9ecdde5

## Reproducibility

This project is fully reproducible. To replicate the analysis:

Clone this repository: git clone https://github.com/MayaGutiBan/BDML_prob_set_1.git
cd BDML_prob_set_1

## Key results: 



