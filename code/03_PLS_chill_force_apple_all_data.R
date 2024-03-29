library(chillR)
library(tidyverse)

# This script will run a PLS analysis with all the data from the experiment (i.e. 66 simulated seasons)
# using chill and heat accumulation as independent variables

# First, load the PLS_chill_force() function from source (modified version)

source("code/00_PLS_chill_force_treatments_2.R")


# Then, it is necessary to load the temperature responses dataframe

temperature_responses_daily <- read.csv("data/temperature_response_apple.csv") 

# Pheno data

pheno_data <- read.csv("data/bio_data_apple.csv")



# Using the PLS_chill_force_treatments() function we can apply the PLS analysis by treatment
# to all data

PLS_all <- PLS_chill_force_treatments_2(temperature_responses_daily, pheno_data,
                                        chill_models = c("Chill_Portions"),
                                        heat_models = c("GDH"))

# Plot the results and save the outputs in the specified folder

plot_PLS(PLS_all, "PLS_outputs/apple/chill_force/PLS_apple_chill_force_S1_S2_66_treatments")


# Based on the hourly weather data, we sorted the seasons according to the chilling accumulation between
# October 1 and March 31

weather_data_hourly <- read.csv("data/weather_data_hourly.csv")

# Compute chill and heat for the period

temperature_responses <-  weather_data_hourly %>% group_by(Treatment) %>% 
  
  filter(JDay %in% c(274 : 365, 1 : 91)) %>% summarise(CPs = max(chillR::Dynamic_Model(Temp)),
                                                       GDHs = max(chillR::GDH(Temp)))

# Order the treatments by chill accumulation (low chill to high chill)

ordered_treatments <- temperature_responses[order(temperature_responses$CPs), "Treatment"][[1]]

# Remove treatment 67 since this treatment is only valid for pear

ordered_treatments <- ordered_treatments[which(ordered_treatments != 67)]


# This for loop will perform a PLS analysis according to a different number of treatments. In brief,
# it will start with the 3 warmer treatments (plus 2 treatments that did not produce bloom) and will add
# a cooler treatment at a time

# Create the folder warmer to colder

dir.create("PLS_outputs/apple/chill_force/warmers")

for (i in 5 : 66){
  
  bio_data_apple_all_chill_i <- pheno_data[pheno_data$Treatment %in% 
                                               ordered_treatments[1 : i], ]
  
  # Having the necessary inputs, we can apply the pls analysis
  
  PLS_apple_all_chill_i <- PLS_chill_force_treatments_2(temperature_responses_daily, bio_data_apple_all_chill_i,
                                                        chill_models = c("Chill_Portions"),
                                                        heat_models = c("GDH"))
  
  
  # For exploratory analysis, we can plot the results with plot_PLS() from chillR
  
  plot_PLS(PLS_apple_all_chill_i,
           paste0("PLS_outputs/apple/chill_force/warmers/", 
                  "PLS_apple_chill_force_S1_S2_", i, "_treatments_Oct_May"))
}


