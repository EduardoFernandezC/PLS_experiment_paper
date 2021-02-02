library(chillR)
library(tidyverse)

# This script will run a PLS analysis with all the data from the experiment (i.e. 66 simulated seasons)

# First, load the pls_treatments() function from source

source("code/00_PLS_treatments_2.R")

# Then, it is necessary to load the weather and phenological data for both seasons

# Weather data

weather_data_daily <- read.csv("data/weather_data_daily_apple.csv")

# Pheno data

pheno_data <- read.csv("data/bio_data_apple.csv")


# Using the pls_treatments_2() function we can apply the PLS analysis by treatment to all data

PLS_all <- pls_treatments_2(weather_data_daily, pheno_data, split_month = 6, runn_mean = 10)

# Plot the results and save the outputs in the specified folder

plot_PLS(PLS_all, "PLS_outputs/apple/pheno/both_seasons_66_treatments")

# Remove the empty directory

unlink("PLS_outputs/apple/pheno/both_seasons_66_treatments", recursive = TRUE)


# Based on the hourly weather data, we sorted the seasons according to the chilling accumulation between
# October 1 and March 31

weather_data_hourly <- read.csv("data/weather_data_hourly.csv")

# Compute chill and heat for the period

temperature_responses <-  weather_data_hourly %>% group_by(Treatment) %>% 
  
  filter(JDay %in% c(274 : 365, 1 : 91)) %>% summarise(CPs = max(chillR::Dynamic_Model(Temp)),
                                                       GDHs = max(chillR::GDH(Temp)))

# Order the treatments by chill accumulation (low chill to high chill)

ordered_treatments <- temperature_responses[order(temperature_responses$CPs), "Treatment"][[1]]

# Remove treatment 67 since this is only valid for pears

ordered_treatments <- ordered_treatments[which(ordered_treatments != 67)]


# This for loop will perform a PLS analysis according to a different number of treatments. In brief,
# it will start with the 3 warmer treatments (plus 2 treatments that did not produce bloom) and will add
# a cooler treatment at a time

# Create the folder warmer to colder

dir.create("PLS_outputs/apple/pheno/warmers")

for (i in 5 : 66){
  
  bio_data_apple_all_chill_i <- pheno_data[pheno_data$Treatment %in% 
                                               ordered_treatments[1 : i], ]
  
  # Having the necessary inputs, we can apply the pls analysis
  
  PLS_apple_all_chill_i <- pls_treatments_2(weather_data_daily, bio_data_apple_all_chill_i)
  
  # For exploratory analysis, we can plot the results with plot_PLS() from chillR
  
  plot_PLS(PLS_apple_all_chill_i,
           paste0("PLS_outputs/apple/pheno/warmers/PLS_apple_Oct_May_",
                  i, "_treatments"))
  
  # Remove the empty folder in your output path
  
  unlink(paste0("PLS_outputs/apple/pheno/warmers/PLS_apple_Oct_May_",
                i, "_treatments"), recursive = TRUE)
}
