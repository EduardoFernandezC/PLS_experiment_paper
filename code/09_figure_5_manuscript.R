# This script uses the elements generated in the script '01_PLS_pheno_apple_all_data.R'. I did not
# source it since it will produce the plots again. Make sure it runs until line 160 (skipping the
# lines where the plots are saved to disk). The code below should do the job

eval(parse(text = readLines("code/seasons_1_2/01_PLS_pheno_apple_all_data.R", n = 161)))

# This time, I will include all simulated season in order to have a more robust data set for
# interpolating. Nonetheless, the chilling and heating periods were inferred from the PLS analysis
# using 12 and 15 seasons respectively (the highest variation in temperature)

# Change the type of bio_data_all$Treatment

bio_data_all$Treatment <- as.numeric(bio_data_all$Treatment)


# Add the YEARMODA column to the weather data frame

weather_data_all <- dormancyR::add_YEARMODA(weather_data_all)


# Select the rows for the chilling period (Oct 19 - Jan 04)

chilling_period <- weather_data_all[weather_data_all$YEARMODA %in% 
                                           c(20181019 : 20190104, 20191019 : 20200104), ]


# Repeat the same for the heating period (Jan 16 - Mar 26)

heating_period <- weather_data_all[weather_data_all$YEARMODA %in% 
                                          c(20190116 : 20190326, 20200116 : 20200326), ]

# Summarize the data to obtain the mean temperature during the chilling and forcing phase respectively

chilling_period <- chilling_period %>% mutate(Tmean = (Tmin + Tmax) /2) %>%  group_by(Treatment) %>% 
  
  summarise(Tmean_chilling_period = mean(Tmean))

# The same for heating data frame

heating_period <- heating_period %>% mutate(Tmean = (Tmin + Tmax) /2) %>%  group_by(Treatment) %>% 
  
  summarise(Tmean_heating_period = mean(Tmean))

# Merge both data frames 

mean_temp_phase <- left_join(chilling_period, heating_period, by = "Treatment")

# Add the bloom date

mean_temp_phase <- left_join(mean_temp_phase, bio_data_all , by = "Treatment")

# Remove NAs for kriging surface

mean_temp_phase <- na.omit(mean_temp_phase)

# Implement the Kriging interpolation by using the function Krig() from the fields package.
# This will assume a linear additive model (see Krig() help for more information)

interpolated_surface_apple <- fields::Krig(x = as.matrix(mean_temp_phase[, c("Tmean_chilling_period",
                                                                       "Tmean_heating_period")]),
                                     Y = mean_temp_phase$pheno)


# Generate the plot by using the functionalty from base R

png("figures/apple.png", width = 5.33, height = 3.92, units = "in", res = 600)

par(mar = c(5, 5, 1.5, 2.5))


fields::surface(interpolated_surface_apple,
                xlab = "Mean temperature during\n the chilling phase (°C)",
                ylab = "Mean temperature during\n the heating phase (°C)",
                xlim = c(6, 18.5),
                ylim = c(6, 19.5),
                type = "C",
                las = 1,
                cex.lab = 0.9,
                cex.axis = 0.8,
                col.axis = "grey30",
                legend.shrink = 0.35,
                legend.width = 1,
                asp = 1,
                axis.args = list(cex.axis = 0.6),
                legend.args = list(text = "Bloom date\n (day of the year)",
                                   side = 3, cex = 0.7, line = 0.5))

dev.off()

# Despite the following code means clearing everything from the global environment,
# I recommend this to make sure the code for pear is working nicely

rm(list = ls())






# Repeat the same process but now using the data from the pears

# Read the first lines of the PLS_pear script to get the weather data

eval(parse(text = readLines("code/seasons_1_2/02_PLS_pheno_pear_all_data.R", n = 144)))

# Change the type of bio_data_all$Treatment

bio_data_all$Treatment <- as.numeric(bio_data_all$Treatment)


# Add the YEARMODA column to the weather data frame

weather_data_all <- dormancyR::add_YEARMODA(weather_data_all)


# Select the rows for the chilling period (Oct 19 - Dec 27)

chilling_period <- weather_data_all[weather_data_all$YEARMODA %in% 
                                      c(20181019 : 20181227, 20191019 : 20191227), ]


# Repeat the same for the heating period (Jan 16 - Mar 26)

heating_period <- weather_data_all[weather_data_all$YEARMODA %in% 
                                     c(20190116 : 20190326, 20200116 : 20200326), ]

# Summarize the data to obtain the mean temperature during the chilling and forcing phase respectively

chilling_period <- chilling_period %>% mutate(Tmean = (Tmin + Tmax) /2) %>%  group_by(Treatment) %>% 
  
  summarise(Tmean_chilling_period = mean(Tmean))

# The same for heating data frame

heating_period <- heating_period %>% mutate(Tmean = (Tmin + Tmax) /2) %>%  group_by(Treatment) %>% 
  
  summarise(Tmean_heating_period = mean(Tmean))

# Merge both data frames 

mean_temp_phase <- left_join(chilling_period, heating_period, by = "Treatment")

# Add the bloom date

mean_temp_phase <- left_join(mean_temp_phase, bio_data_all , by = "Treatment")

# Remove NAs for kriging surface

mean_temp_phase <- na.omit(mean_temp_phase)

# Implement the Kriging interpolation by using the function Krig() from the fields package.
# This will assume a linear additive model (see Krig() help for more information)

interpolated_surface_pear <- fields::Krig(x = as.matrix(mean_temp_phase[, c("Tmean_chilling_period",
                                                                       "Tmean_heating_period")]),
                                     Y = mean_temp_phase$pheno)


# Generate the plot by using the functionality from base R

png("figures/pear.png", width = 5.33, height = 3.92, units = "in", res = 600)

par(mar = c(5, 5, 1.5, 2.5))

fields::surface(interpolated_surface_pear,
                xlab = "Mean temperature during\n the chilling phase (°C)",
                ylab = "Mean temperature during\n the heating phase (°C)",
                xlim = c(5.5, 18.5),
                ylim = c(6, 16.5),
                type = "C",
                las = 1,
                cex.lab = 0.9,
                cex.axis = 0.8,
                col.axis = "grey30",
                legend.shrink = 0.35,
                legend.width = 1,
                asp = 1,
                axis.args = list(cex.axis = 0.6),
                legend.args = list(text = "Bloom date\n (day of the year)",
                                   side = 3, cex = 0.7, line = 0.5))

dev.off()


