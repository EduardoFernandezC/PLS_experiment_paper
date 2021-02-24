library(tidyverse)
library(cowplot)
library(dormancyR)

# Load all the files in the folder for PLS outputs

files <- list.files("PLS_outputs/apple/pheno/warmers/")

# Keep only the csv files

files <- files[which(tools::file_ext(files) == "csv")]

# Define the end of the substr() function

end_treat <- stringr::str_locate(files, "_t")[1 : 62]

names(end_treat) <- files

# Load the data into R with a for loop

for (file in files){
  
  temp_df <- read.csv(paste0("PLS_outputs/apple/pheno/warmers/", file))
  
  temp_df["Treatments"] <- as.numeric(substr(file, 19, end_treat[[file]] - 1))
  
  if (file == files[1])
    
    season_1_2_data <- temp_df else season_1_2_data <- rbind(season_1_2_data, temp_df)}


# Order the seasons data frame according to the treatments

season_1_2_data <- season_1_2_data[order(season_1_2_data$Treatments), ]



# Transform the date column to a proper format for plotting purpose 

# Extract the days and months from the MODA format (e.g. 1001 for O1 October)

days <- substr(season_1_2_data$Date, nchar(season_1_2_data$Date) - 1, nchar(season_1_2_data$Date))

months <- substr(season_1_2_data$Date, 1, nchar(season_1_2_data$Date) - nchar(days))

# Define the vector as_date with the function as.Date(). This will produce dates in the format
# YYYY-MM-DD. Worth nothing that 2020 is going to be used to fill in the YEAR slot... This is the
# year that I generate this script

as_date <- as.Date(paste0(days, "/", months), format = "%d/%m")

# Replace the column "Date" in the main data frame with the dates

season_1_2_data["Date"] <- as_date

# Unfortunately, all dates are for year 2020. This step will identify the months with year + 1 and 
# will fix this difficulty

# Split the column date into year, month, day

season_1_2_data <- tidyr::separate(season_1_2_data, Date, c("Year", "Month", "Day"))

# Using the column JDay, I can identify the end of the year and add 1 to the year rows after 0

season_1_2_data["Year"] <- dplyr::if_else(season_1_2_data$JDay <= 0, as.numeric(season_1_2_data$Year),
                                          as.numeric(season_1_2_data$Year) + 1) 

# Go back to the Date column in Date format

season_1_2_data["Date"] <- as.Date(paste(season_1_2_data$Year, season_1_2_data$Month,
                                         season_1_2_data$Day, sep = "-"))

# Add the column species for further faceting

season_1_2_data["Species"] <- "Apple"

# Subset the number of treatments used for final figure (i.e. 6, 12, 66)

season_1_2_data_a <- season_1_2_data[season_1_2_data$Treatments %in% c(6, 12, 66), ]


# Add the color column. Classify the day according to VIP and then to the value of the coef.

season_1_2_data_a <- season_1_2_data_a %>% mutate(Period = if_else(VIP > 0.8,
                                                                  if_else(Coef > 0,
                                                                          "Chilling",
                                                                          "Heating"),
                                                                  "Non-relevant"))


# Generate the ordered treatments vector

weather_data_hourly <- read.csv("data/weather_data_hourly.csv")

# Compute chill and heat for the period

temperature_responses <-  weather_data_hourly %>% group_by(Treatment) %>% 
  
  filter(JDay %in% c(274 : 365, 1 : 91)) %>% summarise(CPs = max(chillR::Dynamic_Model(Temp)),
                                                       GDHs = max(chillR::GDH(Temp)))

# Order the treatments by chill accumulation (low chill to high chill)

ordered_treatments <- temperature_responses[order(temperature_responses$CPs), "Treatment"][[1]]

# Retain only the treatments used in apple

ordered_treatments_apple <- ordered_treatments[which(ordered_treatments != 67)]



# Load the phenology data for both seasons

pheno_apple <- read.csv("data/bio_data_apple.csv")

# Select the dates of bloom for the specific treatments

pheno_summary_apple <- NULL

for (i in c(6, 12, 66)){
  
  pheno <- pheno_apple[pheno_apple$Treatment %in% ordered_treatments_apple[1 : i], "pheno"]
  
  temp_pheno_df <- data.frame(Species = "Apple",
                              Treatments = as.character(i),
                              median_bloom = round(median(pheno, na.rm = TRUE)),
                              mean_bloom = round(mean(pheno, na.rm = TRUE)),
                              beg_bloom = min(pheno, na.rm = TRUE),
                              end_bloom = max(pheno, na.rm = TRUE),
                              mid_point_bloom = round(mean(c(min(pheno, na.rm = TRUE),
                                                             max(pheno, na.rm = TRUE)))))
  
  if (is.null(pheno_summary_apple)) pheno_summary_apple <- temp_pheno_df else
    
    pheno_summary_apple <- bind_rows(pheno_summary_apple, temp_pheno_df)
  }
  
# Change the labels of the treatments to match the pls plot panels

pheno_summary_apple$Treatments <- factor(pheno_summary_apple$Treatments,
                                         levels = c(6, 12, 66),
                                         labels = c("A", "B", "C"))


# Transform the JDays to actual dates in the rigth format  

pheno_summary_apple <- pheno_summary_apple %>%
  mutate(across(ends_with("bloom"), JDay_to_date, year = 2022)) %>% 
  mutate(across(ends_with("bloom"), as.Date))





## Repeat the same procedure now for pear


# Load all the files in the folder for PLS outputs

files <- list.files("PLS_outputs/pear/pheno/warmers/")

# Keep only the csv files

files <- files[which(tools::file_ext(files) == "csv")]

# Define the end of the substr() function

end_treat <- stringr::str_locate(files, "_t")[1 : 28]

names(end_treat) <- files

# Load the data into R with a for loop

for (file in files){
  
  temp_df <- read.csv(paste0("PLS_outputs/pear/pheno/warmers/", file))
  
  temp_df["Treatments"] <- as.numeric(substr(file, 19, end_treat[[file]] - 1))
  
  if (file == files[1])
    
    season_1_2_data_p <- temp_df else season_1_2_data_p <- rbind(season_1_2_data_p, temp_df)}


# Order the seasons data frame according to the treatments

season_1_2_data_p <- season_1_2_data_p[order(season_1_2_data_p$Treatments), ]



# Transform the date column to a proper format for plotting purpose 

# Extract the days and months from the MODA format (e.g. 1001 for O1 October)

days <- substr(season_1_2_data_p$Date, nchar(season_1_2_data_p$Date) - 1,
               nchar(season_1_2_data_p$Date))

months <- substr(season_1_2_data_p$Date, 1, nchar(season_1_2_data_p$Date) - nchar(days))

# Define the vector as_date with the function as.Date(). This will produce dates in the format
# YYYY-MM-DD. Worth nothing that 2020 is going to be used to fill in the YEAR slot... This is the
# year that I generate this script

as_date <- as.Date(paste0(days, "/", months), format = "%d/%m")

# Replace the column "Date" in the main data frame with the dates

season_1_2_data_p["Date"] <- as_date

# Unfortunately, all dates are for year 2020. This step will identify the months with year + 1 and 
# will fix this difficulty

# Split the column date into year, month, day

season_1_2_data_p <- tidyr::separate(season_1_2_data_p, Date, c("Year", "Month", "Day"))

# Using the column JDay, I can identify the end of the year and add 1 to the year rows after 0

season_1_2_data_p["Year"] <- dplyr::if_else(season_1_2_data_p$JDay <= 0,
                                            as.numeric(season_1_2_data_p$Year),
                                            as.numeric(season_1_2_data_p$Year) + 1)
                                     

# Go back to the Date column in Date format

season_1_2_data_p["Date"] <- as.Date(paste(season_1_2_data_p$Year, season_1_2_data_p$Month,
                                           season_1_2_data_p$Day, sep = "-"))

# Add the column species for further faceting

season_1_2_data_p["Species"] <- "Pear"


# Subset the number of treatments used for final figure (i.e. 8, 15, 32)

season_1_2_data_p_b <- season_1_2_data_p[season_1_2_data_p$Treatments %in% c(8, 15, 32), ]


# Add the color column. Clasify the day according to VIP and then to the value of the coef.

season_1_2_data_p_b <- season_1_2_data_p_b %>% 
  
  mutate(Period = if_else(VIP > 0.8, if_else(Coef > 0, "Chilling", "Heating"), "Non-relevant"))


# Modify the ordered treatments vector to retain only the treatments used in pear

ordered_treatments_pear <- ordered_treatments[which(ordered_treatments %in% c(1 :16, 34 : 46, 59, 63, 67))]


# Load the phenology data for both seasons

pheno_pear <- read.csv("data/bio_data_pear.csv")

# Modify the treatments for second season to match the ordered treatments vector

pheno_pear[17 : 32, "Treatment"] <- c(34 : 46, 59, 63, 67)

# Select the dates of bloom for the specific treatments

pheno_summary_pear <- NULL

for (i in c(8, 15, 32)){
  
  pheno <- pheno_pear[pheno_pear$Treatment %in% ordered_treatments_pear[1 : i], "pheno"]
  
  temp_pheno_df <- data.frame(Species = "Pear",
                              Treatments = as.character(i),
                              median_bloom = round(median(pheno, na.rm = TRUE)),
                              mean_bloom = round(mean(pheno, na.rm = TRUE)),
                              beg_bloom = min(pheno, na.rm = TRUE),
                              end_bloom = max(pheno, na.rm = TRUE),
                              mid_point_bloom = round(mean(c(min(pheno, na.rm = TRUE),
                                                             max(pheno, na.rm = TRUE)))))
  
  if (is.null(pheno_summary_pear)) pheno_summary_pear <- temp_pheno_df else
    
    pheno_summary_pear <- bind_rows(pheno_summary_pear, temp_pheno_df)
}

# Change the labels of the treatments to match the pls plot panels

pheno_summary_pear$Treatments <- factor(pheno_summary_pear$Treatments,
                                         levels = c(8, 15, 32),
                                         labels = c("A", "B", "C"))

# Transform the JDays to actual dates in the rigth format  
pheno_summary_pear <- pheno_summary_pear %>%
  mutate(across(ends_with("bloom"), JDay_to_date, year = 2022)) %>% 
  mutate(across(ends_with("bloom"), as.Date))

# Merge the pear and apple data frame with phenology information

pheno_summary_both <- bind_rows(pheno_summary_apple, pheno_summary_pear)



# Change the name of the number of treatments to have common names between pear and apple datasets

season_1_2_data_a$Treatments <- factor(season_1_2_data_a$Treatments, labels = c("A", "B", "C"))

season_1_2_data_p_b$Treatments <- factor(season_1_2_data_p_b$Treatments, labels = c("A", "B", "C"))


# Merge all data for pls analysis

pls_data <- rbind(season_1_2_data_a, season_1_2_data_p_b)





# Plot the pls outputs

pls <- ggplot() +
  geom_col(data = pls_data, aes(Date, Coef,
                                fill = factor(Period, labels = c("Relevant and positive",
                                                                 "Relevant and negative",
                                                                 "Non-relevant"))),
           width = 1) + 
  geom_rect(aes(xmin = beg_bloom, xmax = end_bloom,
                ymin = -0.48, ymax = -0.4), data = pheno_summary_both,
                size = 0.3, color = "black", alpha = 0) + 
  geom_point(aes(x = median_bloom, y = -0.435),
             shape = "\U273F", data = pheno_summary_both, size = 5,
             color = "deeppink3") +
  scale_x_date(breaks = "1 month", labels = scales::date_format("%b"),
               expand = expansion(add = 1)) +
  scale_y_continuous(expand = expansion(mult = 0.01)) +
  scale_fill_manual(values = c("dodgerblue4", "firebrick",  "grey80")) +
  labs(y = "Standardized model coefficient",
       fill = "VIP score and\n   correlation") +
  facet_grid(factor(Treatments, labels = c("Lowest temperature variation",
                                           "Highest temperature variation",
                                           "All seasons in PLS")) ~ Species) +
  theme_bw() +
  theme(strip.background = element_blank(),
        #strip.text.y = element_blank(),
        strip.text.x = element_text(size = 12),
        legend.position = "none",
        axis.title.x = element_blank(),
        legend.direction = "horizontal",
        plot.margin = margin(0.5, 0.5, 7, 0.5, unit = "cm"))

# get the legend of the PLS plot. For this, legend.position and legend.direction arguments in theme()
# have to be set to "bottom" and "horizontal", respectively. Make sure that legend.direction is
# 'commented' ('#') and legend.position is set to "none" after getting the legend. 

legend <- get_legend(pls)


# Using all data, this step will identify the color of the dots according to the treatments mentioned
# This is useful for highlighting specific dots in the following plot

season_1_2_data <- season_1_2_data %>% mutate(color = if_else(Treatments %in% c(6, 12, 66),
                                                              "red", "black"))

season_1_2_data_p <- season_1_2_data_p %>% mutate(color = if_else(Treatments %in% c(8, 15, 32),
                                                              "red", "black"))


# Merge all data for using the facet option of ggplot2

all_data <- rbind(season_1_2_data, season_1_2_data_p)


# Plot the deviation in temperatures 

temp <- ggplot(all_data, aes(Treatments, Tstdev, color = color, size = color)) +
  geom_point(stat = "summary", shape = 19) +
  scale_x_continuous(breaks = seq(5, 66, 5), expand = expansion(mult = 0.05),
                     labels = function(x) x - 2) +
  scale_y_continuous(breaks = seq(3.3, 4.3, 0.2), expand = expansion(mult = 0.05)) +
  scale_color_manual(values = c("grey30", "red")) +
  scale_size_manual(values = c(0.9, 1.5)) +
  labs(y = "Mean standard deviation\n in temperature (°C)",
       x = "Number of seasons used in PLS analysis") +
  facet_grid(. ~ Species, drop = T, scales = "free_x") +
  theme_bw() +
  theme(strip.text = element_blank(),
        legend.position = "none",
        aspect.ratio = 0.65)


# Merge all plots together

ggdraw() + draw_plot(pls, 0, 0.02, 1, 1, 1) +
  draw_plot(temp, -0.019, -0.375, 1, 1, 0.934) +
  draw_plot(legend, 0.025, -0.215, 1, 1, 1) +
  draw_plot_label(c("4 seasons", "6 seasons", "10 seasons", "13 seasons",
                    "64 seasons", "30 seasons"),
                  x = c(0.5, 0.925, 0.5, 0.925, 0.5, 0.925),
                  y = c(0.9525, 0.9525, 0.74, 0.74, 0.525, 0.525),
                  size = 9,
                  hjust = 1,
                  vjust = 0,
                  fontface = "plain",
                  color = "grey20") +
  draw_plot_label(c("A", "B"),
                  x = 0.05,
                  y = c(0.99, 0.27),
                  size = 13,
                  fontface = "plain")

ggsave("figures/figure_3.png", width = 17.6, height = 23.47, units = "cm", dpi = 600)




