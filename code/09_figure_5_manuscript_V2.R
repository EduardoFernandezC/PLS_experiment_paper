# Rainbow plot 2.0 ####

# This script uses the elements generated in the script '01_PLS_pheno_apple_all_data.R'. I did not
# source it since it will produce the plots again. Make sure it runs until line 160 (skipping the
# lines where the plots are saved to disk). The code below should do the job

eval(parse(text = readLines("code/seasons_1_2/01_PLS_pheno_apple_all_data.R", n = 163)))

# Apple plot

# This time, I will include only some treatments

bio_data_original <- bio_data_all_apple
wather_data_original <- weather_data_all_apple

ranges <- c("1 : 25", "25 : 45", "35 : 55", "45 : 66")

all_data <- NULL

for (range in ranges){

bio_data_all <- filter(bio_data_original, Treatment %in% 
                         ordered_treatments_all[eval(parse(text = range))])


# Change the type of bio_data_all$Treatment

bio_data_all$Treatment <- as.numeric(bio_data_all$Treatment)

# Select the weather data

weather_data_all <- filter(wather_data_original, Treatment %in% 
                             ordered_treatments_all[eval(parse(text = range))])

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

mean_temp_phase <- left_join(mean_temp_phase, bio_data_all_apple , by = "Treatment")

# Add the class 

mean_temp_phase["Class"] <- range

# Remove NAs for kriging surface

mean_temp_phase <- na.omit(mean_temp_phase)

# Implement the Kriging interpolation by using the function Krig() from the fields package.
# This will assume a linear additive model (see Krig() help for more information)

interpolated_surface_apple <- fields::Krig(x = as.matrix(mean_temp_phase[, c("Tmean_chilling_period",
                                                                             "Tmean_heating_period")]),
                                           Y = mean_temp_phase$pheno)


# # Generate the plot by using the functionalty from base R
# 
# png("figures/apple_40_66.png", width = 5.33, height = 3.92, units = "in", res = 600)
# 
# par(mar = c(5, 5, 1.5, 2.5))
# 
# 
# fields::surface(interpolated_surface_apple,
#                 xlab = "Mean temperature during\n the chilling phase (°C)",
#                 ylab = "Mean temperature during\n the heating phase (°C)",
#                 xlim = c(6, 18.5),
#                 ylim = c(6, 19.5),
#                 type = "C",
#                 las = 1,
#                 cex.lab = 0.9,
#                 cex.axis = 0.8,
#                 col.axis = "grey30",
#                 legend.shrink = 0.35,
#                 legend.width = 1,
#                 asp = 1,
#                 axis.args = list(cex.axis = 0.6),
#                 legend.args = list(text = "Bloom date\n (day of the year)",
#                                    side = 3, cex = 0.7, line = 0.5))
# 
# dev.off()
# 




# Rainbow plot with ggplot2

# Predict the surface from the Krig element

surface <- fields::predictSurface(interpolated_surface_apple)


# Transform the matrix containing the "z" information into a data frame

fill_data <- as.data.frame(surface$z)


# Set the names of the columns as the temperature in the forcing period (y)

colnames(fill_data) <- surface$y


# Add a column for the temperature in the chilling phase (x)

fill_data <- data.frame(Chilling = surface$x,
                        fill_data)


# Use pivot_longer to get the data into ggplot-useful format

fill_data <- pivot_longer(fill_data, -Chilling, names_to = c("X", "Forcing"),
                          values_to = "Bloom", names_sep = "X")[-2]


# Set the column for forcing data as.numeric()

fill_data$Forcing <- as.numeric(fill_data$Forcing)


# Remove missing values from the data frame

fill_data <- na.omit(fill_data)


# Add the column treatments corresponding to the number of treatments used

fill_data["Treatments"] <- range

if (is.null(all_data)) all_data <- fill_data else
  
  all_data <- bind_rows(all_data, fill_data)

# Merge the data for phenology data

if (range == ranges[1]) pheno_apple <- mean_temp_phase else
  
  pheno_apple <- bind_rows(pheno_apple, mean_temp_phase)
}







# Pear ====

# Read the first lines of the PLS_pear script to get the weather data

eval(parse(text = readLines("code/seasons_1_2/02_PLS_pheno_pear_all_data.R", n = 144)))


# This time, I will include only some treatments

bio_data_original_pear <- bio_data_all_pear
wather_data_original_pear <- weather_data_all_pear

ranges <- c("1 : 15", "5 : 20", "10 : 25", "15 : 32")

all_data_pear <- NULL

for (range in ranges){
  
  bio_data_all <- filter(bio_data_original_pear, Treatment %in% 
                           ordered_treatments_all[eval(parse(text = range))])
  
  # Change the type of bio_data_all$Treatment
  
  bio_data_all$Treatment <- as.numeric(bio_data_all$Treatment)
  
  
  # Select the weather data
  
  weather_data_all <- filter(wather_data_original_pear, Treatment %in% 
                               ordered_treatments_all[eval(parse(text = range))])
  
  
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
  
  # Add the class 
  
  mean_temp_phase["Class"] <- range
  
  # Remove NAs for kriging surface
  
  mean_temp_phase <- na.omit(mean_temp_phase)
  
  # Implement the Kriging interpolation by using the function Krig() from the fields package.
  # This will assume a linear additive model (see Krig() help for more information)
  
  interpolated_surface_pear <- fields::Krig(x = as.matrix(mean_temp_phase[, c("Tmean_chilling_period",
                                                                              "Tmean_heating_period")]),
                                            Y = mean_temp_phase$pheno)
  
  
  # # Generate the plot by using the functionalty from base R
  # 
  # png("figures/apple_40_66.png", width = 5.33, height = 3.92, units = "in", res = 600)
  # 
  # par(mar = c(5, 5, 1.5, 2.5))
  # 
  # 
  # fields::surface(interpolated_surface_apple,
  #                 xlab = "Mean temperature during\n the chilling phase (°C)",
  #                 ylab = "Mean temperature during\n the heating phase (°C)",
  #                 xlim = c(6, 18.5),
  #                 ylim = c(6, 19.5),
  #                 type = "C",
  #                 las = 1,
  #                 cex.lab = 0.9,
  #                 cex.axis = 0.8,
  #                 col.axis = "grey30",
  #                 legend.shrink = 0.35,
  #                 legend.width = 1,
  #                 asp = 1,
  #                 axis.args = list(cex.axis = 0.6),
  #                 legend.args = list(text = "Bloom date\n (day of the year)",
  #                                    side = 3, cex = 0.7, line = 0.5))
  # 
  # dev.off()
  # 
  
  
  
  # Rainbow plot with ggplot2
  
  # Predict the surface from the Krig element
  
  surface <- fields::predictSurface(interpolated_surface_pear)
  
  
  # Transform the matrix containing the "z" information into a data frame
  
  fill_data_pear <- as.data.frame(surface$z)
  
  
  # Set the names of the columns as the temperature in the forcing period (y)
  
  colnames(fill_data_pear) <- surface$y
  
  
  # Add a column for the temperature in the chilling phase (x)

  fill_data_pear <- data.frame(Chilling = surface$x,
                               fill_data_pear)
  
  
  # Use pivot_longer to get the data into ggplot-useful format
  
  fill_data_pear <- pivot_longer(fill_data_pear, -Chilling, names_to = c("X", "Forcing"),
                                 values_to = "Bloom", names_sep = "X")[-2]
  
  
  # Set the column for forcing data as.numeric()
  
  fill_data_pear$Forcing <- as.numeric(fill_data_pear$Forcing)
  
  
  # Remove missing values from the data frame
  
  fill_data_pear <- na.omit(fill_data_pear)
  
  
  # Add the column treatments corresponding to the number of treatments used
  
  fill_data_pear["Treatments"] <- range
  
  if (is.null(all_data_pear)) all_data_pear <- fill_data_pear else
    
    all_data_pear <- bind_rows(all_data_pear, fill_data_pear)
  
 
  # Merge the data for phenology data
  
  if (range == ranges[1]) pheno_pear <- mean_temp_phase else
    
    pheno_pear <- bind_rows(pheno_pear, mean_temp_phase)
}


# Create a small data frame for the treatments showing no full bloom (apple and pear)

no_bloom <- data.frame(Chilling = c(12.207191, 18.18628, 11.898434, 18.109173),
                       Forcing = c(18.996014, 19.785561, 18.996014, 19.785561),
                       Treatments = c("1 : 25", "1 : 25", "1 : 15", "1 : 15"),
                       Species = c("Apple", "Apple", "Pear", "Pear"),
                       Class = "Warm")


# Merge the data frames from the Krigging surface

all_data["Species"] <- "Apple"

all_data_pear["Species"] <- "Pear"


# Merging

apple_and_pear <- bind_rows(all_data, all_data_pear)

# Rename the treatments (Warm, Mild-warm, Mild-cold, and cold)

apple_and_pear[apple_and_pear$Treatments %in% c("1 : 15", "1 : 25"), "Class"] <- "Warm"

apple_and_pear[apple_and_pear$Treatments %in% c("5 : 20", "25 : 45"), "Class"] <- "Mild-warm"

apple_and_pear[apple_and_pear$Treatments %in% c("10 : 25", "35 : 55"), "Class"] <- "Mild-cold"

apple_and_pear[apple_and_pear$Treatments %in% c("15 : 32", "45 : 66"), "Class"] <- "Cold"


# Prepare the actual phenology observations to be included in the plot

# Classifying the treatments according the class

# Merging

pheno_all <- bind_rows(pheno_pear, pheno_apple)

# Rename the treatments (Warm, Mild-warm, Mild-cold, and cold)

pheno_all[pheno_all$Class %in% c("1 : 15", "1 : 25"), "Class"] <- "Warm"

pheno_all[pheno_all$Class %in% c("5 : 20", "25 : 45"), "Class"] <- "Mild-warm"

pheno_all[pheno_all$Class %in% c("10 : 25", "35 : 55"), "Class"] <- "Mild-cold"

pheno_all[pheno_all$Class %in% c("15 : 32", "45 : 66"), "Class"] <- "Cold"



# Implement the plot

ggplot(apple_and_pear, aes(Chilling, Forcing)) +
  geom_raster(aes(fill = Bloom)) +
  geom_contour(aes(z = Bloom), data = apple_and_pear, size = 0.7, binwidth = 10,
                      color = "black") +
  geom_point(aes(Tmean_chilling_period, Tmean_heating_period), data = pheno_all,
             size = 0.8) +
  metR::geom_text_contour(aes(z = Bloom),
                          data = apple_and_pear, rotate = FALSE, stroke = 0.1,
                          stroke.color = "white") +
  scale_y_continuous(limits = c(6, 20.5), breaks = seq(6, 18, 3),
                     expand = expansion(add = 0)) +
  scale_x_continuous(limits = c(6, 20.5), breaks = seq(6, 18, 3),
                     expand = expansion(add = 0.1)) +
  scale_fill_gradientn(colors = c("darkblue", "blue", "cyan2",
                                  "green2", "yellow", "red", "red4"),
                       limits = c(40, 145)) +
  geom_point(aes(Chilling, Forcing), data = no_bloom,
             shape = 8, color = "red") +
  labs(x = "Mean temperature during the chilling phase (°C)",
       y = "Mean temperature during the forcing phase (°C)",
       fill = "Bloom date\n(DOY)") +
  facet_grid(factor(Class, levels = c("Warm", "Mild-warm", "Mild-cold", "Cold")) ~ Species) +
  theme_bw() +
  theme(legend.title.align = 0.5,
        strip.background = element_blank(),
        strip.placement = "inside")

ggsave("figures/rainbow_apple_pear_2.png", device = "png", height = 20, width = 17.7, units = "cm",
       dpi = 600)





