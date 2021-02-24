library(cowplot)
library(tidyverse)
library(chillR)

# Based on the hourly weather data, we sorted the seasons according to the chilling accumulation between
# October 1 and March 31

weather_data_hourly <- read.csv("data/weather_data_hourly.csv")

# Compute chill and heat for the period

temperature_responses <-  weather_data_hourly %>% group_by(Treatment) %>% 
  
  filter(JDay %in% c(274 : 365, 1 : 91)) %>% summarise(CPs = max(chillR::Dynamic_Model(Temp)),
                                                       GDHs = max(chillR::GDH(Temp)))


# Add the column season to the temp_response data frame

temperature_responses[1 : 33, "Season"] <- "S1"

temperature_responses[34 : 67, "Season"] <- "S2"


# For illustration purposes, I set the treatments 34 : 67 to 1 : 34

temperature_responses[34 : 67, "Treatment"] <- 1 : 34


# Add the column treatment_season

temperature_responses["Treat_season"] <- paste0(temperature_responses$Treatment, "_",
                                                temperature_responses$Season)

# Order the treatments according to chill accumulation (increasing)

ordered_treatments <- temperature_responses[order(temperature_responses$CPs), "Treat_season"][[1]]


# This script produces the figure 1 of my manuscript. This figure shows the treatments we used 
# in this experiment.

# Load the phenological data to plot the median bloom date for each treatment

pheno_apple <- read.csv("data/bio_data_apple.csv")
  
# Change the treatments for the second season

pheno_apple[34 : 66, "Treatment"] <- 1 : 33

# Add the column treatment_season

pheno_apple[1 : 33, "Treat_season"] <- paste0(pheno_apple$Treatment[1 : 33], "_S1")

pheno_apple[34 : 66, "Treat_season"] <- paste0(pheno_apple$Treatment[34 : 66], "_S2")


# Add the the date based on the JDay

pheno_apple["Date"] <- dormancyR::JDay_to_date(pheno_apple$pheno, 2019, na.rm = TRUE)



# Repeat the same steps as above for pear data

pheno_pear <- read.csv("data/bio_data_pear.csv")

# Change the treatments for the second season

pheno_pear[17 : 32, "Treatment"] <- c(1 : 13, 26, 30, 34)

# Add the column treatment_season

pheno_pear[1 : 16, "Treat_season"] <- paste0(pheno_pear$Treatment[1 : 16], "_S1")

pheno_pear[17 : 32, "Treat_season"] <- paste0(pheno_pear$Treatment[17 : 32], "_S2")


# Add the the date based on the JDay

pheno_pear["Date"] <- dormancyR::JDay_to_date(pheno_pear$pheno, 2019, na.rm = TRUE)


# Merge the apple and pear dataframe

bio_data <- bind_rows(pheno_apple, pheno_pear)

# Add the ID column to the bio data 

bio_data["ID"] <- paste0(bio_data$Treat_season, "_chill")



# Use the weather data hourly to compute chill and heat accumulation. Add the column season as well

weather_data_hourly["Chill"] <- Dynamic_Model(weather_data_hourly$Temp, summ = FALSE)

weather_data_hourly["Heat"] <- GDH(weather_data_hourly$Temp, summ = FALSE)

weather_data_hourly[weather_data_hourly$Treatment %in% c(1 : 33), "Season"] <- "S1"
  
weather_data_hourly[weather_data_hourly$Treatment %in% c(33 : 67), "Season"] <- "S2"

# Change the name for the treatments in the second season

weather_data_hourly[weather_data_hourly$Treatment %in% c(34 : 67), "Treatment"] <- rep(1 : 34, each = 8784)

# Add the treat_season column

weather_data_hourly["Treat_season"] <- paste(weather_data_hourly$Treatment, weather_data_hourly$Season, sep = "_")



# Summarize the data into daily chill and heat

metric_summary <- weather_data_hourly %>% group_by(Treat_season, Season, Year, Month, Day, JDay) %>% 
  summarise(chill = sum(Chill),
            heat = sum(Heat))

metric_summary["Date"] <- as.Date(YEARMODA2Date(metric_summary$Year * 10000 +
                                            metric_summary$Month * 100 + metric_summary$Day))



# Substract 1 year to the date in the second year

metric_summary[metric_summary$Season == "S2", "Date"] <- metric_summary[metric_summary$Season == "S2", "Date"] - 365



# Pivot longer to join the columns chill and heat

metric_summ_long <- pivot_longer(metric_summary, c(chill, heat), names_to = "Metric", values_to = "value")

# Duplicate the treat_season column to get chill and  heat in the y-axis

metric_summ_long["ID"] <- paste(metric_summ_long$Treat_season, metric_summ_long$Metric, sep = "_")


# Extract the last dates to remove the data according to the treatment

last_date <- bio_data %>% group_by(Treat_season) %>% summarise(last_date = max(Date, na.rm = FALSE))

last_date[is.na(last_date$last_date), "last_date"] <- as.Date("2019-05-25")

# Nest the dataframe to delete the rows after blooming

nested <- metric_summ_long %>% group_by(Treat_season) %>% nest()

# Merge the columns in the last_date and the nested data frame

nested <- left_join(nested, last_date)

# Unnest the nested data frame to filter the rows

unnest <- unnest(nested, cols = c(data))

# Set the value for the value column to 0 in case the date is later than the last date

unnest["value_2"] <- if_else(unnest$Date > unnest$last_date + 6, as.numeric(NA), unnest$value)


treatment_ordered <- NULL

for (treat in ordered_treatments){
  
  treatments <- c(paste0(treat, "_chill"), paste0(treat, "_heat"))
  
  treatment_ordered <- c(treatment_ordered, treatments)
}


# Split the data frames into two, to have to panels of treatments

unnest[unnest$ID %in% treatment_ordered[1 : ((length(treatment_ordered) / 2) + 1)], "Panel"] <- "A"
unnest[unnest$ID %in% treatment_ordered[((length(treatment_ordered) / 2) + 2) : 134], "Panel"] <- "B"

# Add a new column for compatibility in the faceting step

unnest[unnest$Panel == "A", "ID_2"] <- as.numeric(as.character(factor(filter(unnest, Panel == "A")$ID,
                                              levels = treatment_ordered[1 : ((length(treatment_ordered) / 2) + 1)],
                                              labels = rep(1 : 34, each = 2))))

unnest[unnest$Panel == "B", "ID_2"] <- as.numeric(as.character(factor(filter(unnest, Panel == "B")$ID,
                                              levels = treatment_ordered[((length(treatment_ordered) / 2) + 2) : 134],
                                              labels = rep(1 : 33, each = 2))))

# add the suffix

unnest$ID_2 <- paste(unnest$ID_2, unnest$Metric, sep = "_")


# Split the bio data

bio_data[bio_data$ID %in% treatment_ordered[1 : ((length(treatment_ordered) / 2) + 1)], "Panel"] <- "A"
bio_data[bio_data$ID %in% treatment_ordered[((length(treatment_ordered) / 2) + 2) : 134], "Panel"] <- "B"


# Add a new column for compatibility in the faceting step

bio_data[bio_data$Panel == "A", "ID_2"] <- as.numeric(as.character(factor(filter(bio_data, Panel == "A")$ID,
                                                                      levels = treatment_ordered[1 : ((length(treatment_ordered) / 2) + 1)],
                                                                      labels = rep(1 : 34, each = 2))))

bio_data[bio_data$Panel == "B", "ID_2"] <- as.numeric(as.character(factor(filter(bio_data, Panel == "B")$ID,
                                                                      levels = treatment_ordered[((length(treatment_ordered) / 2) + 2) : 134],
                                                                      labels = rep(1 : 33, each = 2))))

# add the suffix

bio_data$ID_2 <- paste0(bio_data$ID_2, "_heatt")



# Prepare the y axis

# Add an additional "t" to the IDs for heat to make it coincide the number of characters with chill labels
unnest[str_detect(unnest$ID_2, pattern = "heat"), "ID_2"] <- paste0(unnest[str_detect(unnest$ID_2,
                                                                                       pattern = "heat"),
                                                                            "ID_2"][[1]], "t")

# Define another label column with the same length of character each. This is to have labels in the order of
# 01 and 10 for example. This will probably print the y axis ordered

unnest["ID_3"] <- if_else(nchar(unnest$ID_2) < 8, paste0("0", unnest$ID_2), unnest$ID_2)


# Same for bloom dates

bio_data["ID_3"] <- if_else(nchar(bio_data$ID_2) < 8, paste0("0", bio_data$ID_2), bio_data$ID_2)


# Since plotting all together did not work, I had to generate 2 dataframes accoring to each panel.
# Later, I used cowplot to merge the panels to generate 1 joint figure.

panel_a <- filter(unnest, Panel == "A")

# Draw the main plot

plot_a <- ggplot() +
  geom_tile(aes(Date, ID_3, 
                fill = runn_mean(value_2, 11)),
            data = filter(panel_a, Metric == "chill" & Panel == "A"),
            height = 0.9) +
  scale_fill_gradient2("Daily chill\n   (CP)", low = "white", mid = "skyblue", midpoint = 0.45, high = "blue4",
                       na.value = "white",
                       limits = c(0, 1)) +
  
  ggnewscale::new_scale("fill") +
  
  geom_tile(aes(Date, ID_3, 
                fill = runn_mean(value_2, 11)),
            data = filter(panel_a, Metric == "heat" & Panel == "A"),
            height = 0.9) +
  geom_point(aes(Date, ID_3, shape = Species),
             data = filter(bio_data, Panel == "A"),
             color = "black",
             position = position_nudge(y = 0.5)) +
  scale_fill_gradient2("Daily heat\n   (GDH)", low = "white", mid = "orange", midpoint = 200, high = "red4", 
                       na.value = "white", limits = c(0, 450)) +
  scale_y_discrete(limits = rev(levels(as.factor(panel_a$ID_3)))) +
  scale_shape_manual(values = c(1, 2)) +
  scale_x_date(breaks = "month", labels = function(x) format(x, "%b"),
               expand = expansion(mult = 0),
               limits = as.Date(c("2018-09-29", "2019-05-25"))) +
  labs(y = "Treatment", x = NULL, shape = "Full bloom") +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(5.5, 5.5, 5.5, 45.5), "points"),
        legend.position = "none")

# Do the same for panel B

panel_b <- filter(unnest, Panel == "B")

plot_b <- ggplot() +
  geom_tile(aes(Date, ID_3, 
                fill = runn_mean(value_2, 11)),
            data = filter(panel_b, Metric == "chill"),
            height = 0.9) +
  scale_fill_gradient2("Daily chill\n   (CP)", low = "white", mid = "skyblue", midpoint = 0.45, high = "blue4",
                       na.value = "white",
                       limits = c(0, 1)) +
  
  ggnewscale::new_scale("fill") +
  
  geom_tile(aes(Date, ID_3, 
                fill = runn_mean(value_2, 11)),
            data = filter(panel_b, Metric == "heat"),
            height = 0.9) +
  geom_point(aes(Date, ID_3, shape = Species),
             data = filter(bio_data, Panel == "B"),
             color = "black",
             position = position_nudge(y = 0.5)) +
  scale_fill_gradient2("Daily heat\n   (GDH)", low = "white", mid = "orange", midpoint = 200, high = "red4", 
                       na.value = "white", limits = c(0, 450)) +
  scale_y_discrete(limits = rev(levels(as.factor(panel_b$ID_3)))) +
  scale_shape_manual(values = c(1, 2)) +
  scale_x_date(breaks = "month", labels = function(x) format(x, "%b"),
               expand = expansion(mult = 0),
               limits = as.Date(c("2018-09-29", "2019-05-25"))) +
  labs(y = "Treatment", x = NULL, shape = "Full bloom") +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(5.5, 5.5, 5.5, 45.5), "points"))


# Plot the bloom dates alone to only get the labels in y axis

axis_plot_a <- ggplot(filter(bio_data, Panel == "A"), aes(Date, Treat_season)) +
  geom_point() +
  scale_y_discrete(limits = rev(ordered_treatments[1 : 34]),
                   labels = function(x) paste(34 : 1, substr(x, str_locate(x, pattern = "_")[, 1] + 1,
                                                             str_locate(x, pattern = "_")[, 1] + 2)))
  

axis_plot_b <- ggplot(filter(bio_data, Panel == "B"), aes(Date, Treat_season)) +
  geom_point() +
  scale_y_discrete(limits = rev(ordered_treatments[35 : 67]),
                   labels = function(x) paste(67 : 35, substr(x, str_locate(x, pattern = "_")[, 1] + 1,
                                                             str_locate(x, pattern = "_")[, 1] + 2)))


# Extract the information in the y-axis

axis_a <- ggdraw(get_y_axis(axis_plot_a))

axis_b <- ggdraw(get_y_axis(axis_plot_b))


# Put the axis in the rigth place
ggdraw() + 
  draw_plot(plot_b, 0.415, 0, 0.6, 1, 1) + #0.43
  draw_plot(plot_a, -0.01, 0, 0.4463, 1, 1) +  
  draw_plot(axis_a, -0.89, 0.0105, 1, 1, 0.9645) +
  draw_plot(axis_b, -0.465, 0.0105, 1, 1, 0.9645) +
  
  draw_plot_label("Treatment",
                  x = 0.02, y = 0.5, angle = 90, hjust = 0.5, vjust = 0.5,
                  fontface = "plain", size = 10)
  
ggsave("figures/figure_1.png", device = "png", dpi = 600,
        width = 16.6, height = 22.4, units = "cm")

