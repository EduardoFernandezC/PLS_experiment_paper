library(tidyverse)
library(dormancyR)


# Load all the files in the folder for PLS outputs

files <- list.files("PLS_outputs/apple/chill_force/warmers/")

# Keep only the csv files

files <- files[which(tools::file_ext(files) == "csv")]

# Define the end of the substr() function

end_treat <- stringr::str_locate(files, "_t")[1 : 62]

names(end_treat) <- files

# Load the data into R with a for loop

for (file in files){
  
  temp_df <- read.csv(paste0("PLS_outputs/apple/chill_force/warmers/",
                             file))
  
  temp_df["Treatments"] <- as.numeric(substr(file, 29, end_treat[[file]] - 1))
  
  if (file == files[1])
    
    season_1_2_data_a <- temp_df else season_1_2_data_a <- rbind(season_1_2_data_a, temp_df)}


# Order the seasons data frame according to the treatments

season_1_2_data_a <- season_1_2_data_a[order(season_1_2_data_a$Treatments), ]



# Transform the date column to a proper format for plotting purpose 

# Extract the days and months from the MODA format (e.g. 1001 for O1 October)

days <- substr(season_1_2_data_a$Date, nchar(season_1_2_data_a$Date) - 1,
               nchar(season_1_2_data_a$Date))

months <- substr(season_1_2_data_a$Date, 1, nchar(season_1_2_data_a$Date) - nchar(days))

# Define the vector as_date with the function as.Date(). This will produce dates in the format
# YYYY-MM-DD. Worth nothing that 2020 is going to be used to fill in the YEAR slot... This is the
# year that I generate this script

as_date <- as.Date(paste0("2020/", days, "/", months), format = "%Y/%d/%m")


# Replace the column "Date" in the main data frame with the dates

season_1_2_data_a["Date"] <- as_date

# Unfortunately, all dates are for year 2020. This step will identify the months with year + 1 and 
# will fix this difficulty

# Split the column date into year, month, day

season_1_2_data_a <- separate(season_1_2_data_a, Date, c("Year", "Month", "Day"))

# Using the column JDay, I can identify the end of the year and add 1 to the year rows after 0

season_1_2_data_a["Year"] <- if_else(season_1_2_data_a$JDay > 0, as.numeric(season_1_2_data_a$Year),
                                   as.numeric(season_1_2_data_a$Year) - 1) 

# Go back to the Date column in Date format

season_1_2_data_a["Date"] <- as.Date(paste(season_1_2_data_a$Year, season_1_2_data_a$Month,
                                         season_1_2_data_a$Day, sep = "-"))

# Add the species column

season_1_2_data_a["Species"] <- "Apple"


# Add the color column. Clasify the day according to VIP and then to the value of the coef.

season_1_2_data_a <- season_1_2_data_a %>% 
  
  mutate(Period = if_else(VIP > 0.8, if_else(Coef > 0, "Relevant and positive",
                                             "Relevant and negative"), "Non-relevant"))

# Filter the dataframe to show only the PLS with the relevant number of treatments

season_1_2_data_a_filtered <- filter(season_1_2_data_a, Treatments == 12)




### The same procedure for pear

# Load all the files in the folder for PLS outputs

files <- list.files("PLS_outputs/pear/chill_force/warmers/")

# Keep only the csv files

files <- files[which(tools::file_ext(files) == "csv")]

# Define the end of the substr() function

end_treat <- stringr::str_locate(files, "_t")[1 : 28]

names(end_treat) <- files

# Load the data into R with a for loop

for (file in files){
  
  temp_df <- read.csv(paste0("PLS_outputs/pear/chill_force/warmers/",
                             file))
  
  temp_df["Treatments"] <- as.numeric(substr(file, 28, end_treat[[file]] - 1))
  
  if (file == files[1])
    
    season_1_2_data_p <- temp_df else season_1_2_data_p <- rbind(season_1_2_data_p, temp_df)}


# Order the seasons data frame according to the treatments

season_1_2_data_p <- season_1_2_data_p[order(season_1_2_data_p$Treatments), ]



# Transform the date column to a proper format for plotting purpose 

# Extract the days and months from the MODA format (e.g. 1001 for O1 October)

days <- substr(season_1_2_data_p$Date, nchar(season_1_2_data_p$Date) - 1, nchar(season_1_2_data_p$Date))

months <- substr(season_1_2_data_p$Date, 1, nchar(season_1_2_data_p$Date) - nchar(days))

# Define the vector as_date with the function as.Date(). This will produce dates in the format
# YYYY-MM-DD. Worth nothing that 2020 is going to be used to fill in the YEAR slot... This is the
# year that I generate this script

as_date <- as.Date(paste0("2020/", days, "/", months), format = "%Y/%d/%m")

# Replace the column "Date" in the main data frame with the dates

season_1_2_data_p["Date"] <- as_date

# Unfortunately, all dates are for year 2020. This step will identify the months with year + 1 and 
# will fix this difficulty

# Split the column date into year, month, day

season_1_2_data_p <- separate(season_1_2_data_p, Date, c("Year", "Month", "Day"))

# Using the column JDay, I can identify the end of the year and add 1 to the year rows after 0

season_1_2_data_p["Year"] <- if_else(season_1_2_data_p$JDay > 0, as.numeric(season_1_2_data_p$Year),
                                   as.numeric(season_1_2_data_p$Year) - 1) 

# Go back to the Date column in Date format

season_1_2_data_p["Date"] <- as.Date(paste(season_1_2_data_p$Year, season_1_2_data_p$Month,
                                         season_1_2_data_p$Day, sep = "-"))

# Add the column for the species

season_1_2_data_p["Species"] <- "Pear"


# Add the color column. Classify the day according to VIP and then to the value of the coef.

season_1_2_data_p <- season_1_2_data_p %>% 
  
  mutate(Period = if_else(VIP > 0.8, if_else(Coef > 0, "Relevant and positive",
                                             "Relevant and negative"), "Non-relevant"))


# Filter the dataframe to show only the PLS with the relevant number of treatments

season_1_2_data_p_filtered <- filter(season_1_2_data_p, Treatments == 15)



# Merge both dataframes for faceting

all_data <- rbind(season_1_2_data_a_filtered, season_1_2_data_p_filtered)



# Load the phenology data for both seasons
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
  mutate(across(ends_with("bloom"), JDay_to_date, year = 2020)) %>% 
  mutate(across(ends_with("bloom"), as.Date))




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
  mutate(across(ends_with("bloom"), JDay_to_date, year = 2020)) %>% 
  mutate(across(ends_with("bloom"), as.Date))

# Merge the pear and apple data frame with phenology information

pheno_summary_both <- bind_rows(pheno_summary_apple, pheno_summary_pear)



# Plotting

ggplot() +
  geom_col(aes(Date, Coef,
               fill = factor(Period, levels = c("Relevant and positive",
                                                "Relevant and negative",
                                                "Non-relevant"))),
           data = all_data, width = 1) + 
  geom_rect(aes(xmin = beg_bloom, xmax = end_bloom,
                ymin = -0.27, ymax = -0.33),
            data = filter(pheno_summary_both, Treatments == "B"),
            size = 0.3, color = "black", alpha = 0) + 
  geom_point(aes(x = median_bloom, y = -0.2975),
             shape = "\U273F",
             data = filter(pheno_summary_both, Treatments == "B"),
             size = 5,
             color = "deeppink3") +
  scale_x_date(breaks = "1 month", labels = scales::date_format("%b"),
               expand = expansion(add = 1)) +
  scale_fill_manual(values = c("dodgerblue4", "firebrick",  "grey80")) +
  labs(y = "Standardized model coefficient",
       x = NULL,
       fill = "VIP score and\n   correlation") +
  facet_grid(factor(Type, labels = c("Full bloom versus daily chill",
                                     "Full bloom versus daily heat")) ~
               factor(Species, labels = c("Apple (10 seasons in PLS)",
                                          "Pear (13 seasons in PLS)"))) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.text = element_text(size = 8),
        legend.position = "bottom")

ggsave("figures/figure_4.png", device = "png", width = 17.6, height = 11.7,
       units = "cm", dpi = 600)



