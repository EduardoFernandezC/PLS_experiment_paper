library(chillR)
library(tidyverse)

# This script will run a PLS analysis with all the data from the experiment (i.e. 32 simulated seasons)

# First, load the pls_treatments() function from source

source("code/00_PLS_treatments_2.R")

# Then, it is necessary to load the weather and phenological data for both seasons

# Weather data

weather_data_daily <- read.csv("data/weather_data_daily_pear.csv")

# Pheno data

pheno_data <- read.csv("data/bio_data_pear.csv")


# Using the pls_treatments_2() function we can apply the PLS analysis by treatment to all data

PLS_all <- pls_treatments_2(weather_data_daily, pheno_data, split_month = 6, runn_mean = 10)

# Plot the results and save the outputs in the specified folder

plot_PLS(PLS_all, "PLS_outputs/pear/pheno/both_seasons_32_treatments")

# Remove the empty directory

unlink("PLS_outputs/pear/pheno/both_seasons_32_treatments", recursive = TRUE)


# Based on the hourly weather data, we sorted the seasons according to the chilling accumulation between
# October 1 and March 31

weather_data_hourly <- read.csv("data/weather_data_hourly.csv")

# Compute chill and heat for the period

temperature_responses <-  weather_data_hourly %>% group_by(Treatment) %>% 
  
  filter(JDay %in% c(274 : 365, 1 : 91)) %>% summarise(CPs = max(chillR::Dynamic_Model(Temp)),
                                                       GDHs = max(chillR::GDH(Temp)))

# Filter the treatments only used in pear

temperature_responses <- filter(temperature_responses, Treatment %in% c(1 : 16, 34 : 46, 59, 63, 67))

# Change the name of treatments in season 2 for further compatibility in the for loop

temperature_responses[17 : 32, "Treatment"] <- 17 :32

# Order the treatments by chill accumulation (low chill to high chill)

ordered_treatments <- temperature_responses[order(temperature_responses$CPs), "Treatment"][[1]]



# This for loop will perform a PLS analysis according to a different number of treatments. In brief,
# it will start with the 3 warmer treatments (plus 2 treatments that did not produce bloom) and will add
# a cooler treatment at a time

# Create the folder warmer to colder

dir.create("PLS_outputs/pear/pheno/warmers")


for (i in 5 : 32){
  
  bio_data_pears_all_chill_i <- pheno_data[pheno_data$Treatment %in% 
                                               ordered_treatments[1 : i], ]
  
  # Having the necessary inputs, we can apply the pls analysis
  
  PLS_pears_all_chill_i <- pls_treatments_2(weather_data_daily, bio_data_pears_all_chill_i)
  
  # For exploratory analysis, we can plot the results with plot_PLS() from chillR
  
  plot_PLS(PLS_pears_all_chill_i,
           paste0("PLS_outputs/pear/pheno/warmers/PLS_pears_Oct_May_",
                  i, "_treatments"))
  
  # Remove the empty folder in your output path
  
  unlink(paste0("PLS_outputs/pear/pheno/warmers/PLS_pears_Oct_May_",
                i, "_treatments"), recursive = TRUE)
}

