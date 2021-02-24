# This script produces the figure 2 of the manuscript

library(cowplot)
library(chillR)
library(tidyverse)
library(dormancyR)

# Load the temperature data 

weather_data_hourly <- read.csv("data/weather_data_hourly.csv")

# Summarize into daily extremes

weather_data_daily <- weather_data_hourly %>% group_by(Treatment, Year, Month, Day, JDay) %>% 
  
  summarize(Tmin = min(Temp),
            Tmax = max(Temp))


# Add the YEARMODA column to the weather data frame

weather_data_daily <- dormancyR::add_YEARMODA(weather_data_daily)

# Add the Date column to the weather data frame

weather_data_daily["Date"] <- as.Date(chillR::YEARMODA2Date(weather_data_daily$YEARMODA))

# This part is to compute the mean temperature across treatments for each day of the experiment.
# First, I calculate the running mean over 15 days and then summarized Tmean as well as the range
# of Tmean across treatments.

summary <- weather_data_daily %>% group_by(Treatment) %>% 
  
  mutate(Tmean = runn_mean((Tmin + Tmax) / 2, runn_mean = 15)) %>% 
  
  group_by(Date) %>% 
  
  summarise(Temp = mean(Tmean),
            ymin = min(Tmean),
            ymax = max(Tmean))


# This step is to draw only the ribbon for the days in which we observed variation in Tmean across
# treatments

#summary[summary$Temp != summary$ymax, 'Temp'] <- NA
          

# Draw the plot

# Source the phenology data

# Apple

pheno_apple <- read.csv("data/bio_data_apple.csv")

# The same for pear

pheno_pear <- read.csv("data/bio_data_pear.csv")

# Split the dataframes according to the seasons

bio_data_apple_S1 <- pheno_apple[1 : 33, ]
bio_data_apple_S2 <- pheno_apple[34 : 66, ]

bio_data_pears_S1 <- pheno_pear[1 : 16, ]
bio_data_pears_S2 <- pheno_pear[17 : 32, ]


# Remove NAs

bio_data_apple_S1 <- na.omit(bio_data_apple_S1)
bio_data_apple_S2 <- na.omit(bio_data_apple_S2)
bio_data_pears_S1 <- na.omit(bio_data_pears_S1)
bio_data_pears_S2 <- na.omit(bio_data_pears_S2)

# Summarize for plotting the bars

bio_data_apple_S1 <- bio_data_apple_S1 %>% 
  group_by(Species) %>% 
  summarise(beg_bloom = as.Date(min(JDay_to_date(pheno, year = 2019))),
            end_bloom = as.Date(max(JDay_to_date(pheno, year = 2019))),
            median_bloom = as.Date(JDay_to_date(median(pheno), year = 2019)))


bio_data_apple_S2 <- bio_data_apple_S2 %>% 
  group_by(Species) %>% 
  summarise(beg_bloom = as.Date(min(JDay_to_date(pheno, year = 2020))),
            end_bloom = as.Date(max(JDay_to_date(pheno, year = 2020))),
            median_bloom = as.Date(JDay_to_date(median(pheno), year = 2020)))


bio_data_pears_S1 <- bio_data_pears_S1 %>% 
  group_by(Species) %>% 
  summarise(beg_bloom = as.Date(min(JDay_to_date(pheno, year = 2019))),
            end_bloom = as.Date(max(JDay_to_date(pheno, year = 2019))),
            median_bloom = as.Date(JDay_to_date(median(pheno), year = 2019)))


bio_data_pears_S2 <- bio_data_pears_S2 %>% 
  group_by(Species) %>% 
  summarise(beg_bloom = as.Date(min(JDay_to_date(pheno, year = 2020))),
            end_bloom = as.Date(max(JDay_to_date(pheno, year = 2020))),
            median_bloom = as.Date(JDay_to_date(median(pheno), year = 2020)))



# Plotting

plot <- ggplot() +
  geom_ribbon(aes(Date, Temp, ymin = ymin, ymax = ymax), 
              data = summary, alpha = 0.7, fill = "skyblue") +
  geom_line(aes(Date, Temp), 
            data = summary) +
  
  geom_rect(aes(xmin = beg_bloom, xmax = end_bloom, ymin = 1.4,
                ymax = 2.4), data = bio_data_apple_S1,
            alpha = 0, color = "black") +
  geom_rect(aes(xmin = beg_bloom, xmax = end_bloom, ymin = 0.25,
                ymax = 1.25), data = bio_data_pears_S1,
            alpha = 0, color = "black") +
  geom_rect(aes(xmin = beg_bloom, xmax = end_bloom, ymin = 1.4,
                ymax = 2.4), data = bio_data_apple_S2,
            alpha = 0, color = "black") +
  geom_rect(aes(xmin = beg_bloom, xmax = end_bloom, ymin = 0.25,
                ymax = 1.25), data = bio_data_pears_S2,
            alpha = 0, color = "black") +
  
  geom_point(aes(x = median_bloom, y = 1.95),
             shape = "\U273F", data = bio_data_apple_S1, size = 5,
             color = "deeppink3") +
  geom_point(aes(x = median_bloom, y = 0.8),
             shape = "\U273F", data = bio_data_pears_S1, size = 5,
             color = "deeppink3") +
  geom_point(aes(x = median_bloom, y = 1.95),
             shape = "\U273F", data = bio_data_apple_S2, size = 5,
             color = "deeppink3") +
  geom_point(aes(x = median_bloom, y = 0.8),
             shape = "\U273F", data = bio_data_pears_S2, size = 5,
             color = "deeppink3") +
  
  scale_x_date(limits = as.Date(c("2018-08-01", "2020-06-15")),
               breaks = "3 month",
               labels = scales::date_format("%b %Y"),
               expand = expansion(mult = 0.001)) +
  scale_y_continuous(limits = c(0, 26), expand = expansion(mult = 0),
                     labels = ) +
  labs(x = NULL, y = "Mean temperature (°C)") +
  theme_bw()
  

# Add the labels

ggdraw() + draw_plot(plot, 0, 0, 1, 1, 1) +
  draw_plot_label(c("Apple", "Pear"),
                  x = 0.63,
                  y = c(0.125, 0.08),
                  size = 10,
                  hjust = 0.5,
                  vjust = 0.5,
                  fontface = "plain") +
  draw_line(x = c(0.45, 0.6),
            y = 0.12) +
  draw_line(x = c(0.43, 0.6),
            y = 0.079) +
  draw_line(x = c(0.66, 0.8225),
            y = 0.12) +
  draw_line(x = c(0.66, 0.83525),
            y = 0.079)



# Save the plot into the folder

ggsave("figures/figure_2.png", width = 6.5, height = 4.13, dpi = 600)



## Estimate the greatest difference in temperature

summary[summary$Date < as.Date("2019-08-01"), "Diff"] <- summary[summary$Date < as.Date("2019-08-01"), "ymax"] - 
  
  summary[summary$Date < as.Date("2019-08-01"), "ymin"]


summary[summary$Date >= as.Date("2019-08-01"), "Diff"] <- summary[summary$Date >= as.Date("2019-08-01"), "ymax"] - 
  
  summary[summary$Date >= as.Date("2019-08-01"), "ymin"]

# Subset the dataset according to the season

summary_S1 <- summary[summary$Date < as.Date("2019-08-01"), ]

summary_S2 <- summary[summary$Date >= as.Date("2019-08-01"), ]

# Compute the max difference

summary_S1[which(summary_S1$Diff == max(summary_S1$Diff)), ]

summary_S2[which(summary_S2$Diff == max(summary_S2$Diff)), ]

