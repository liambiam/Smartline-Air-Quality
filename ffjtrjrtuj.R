library(dplyr)
library(ggplot2)
library(lubridate)
library(GGally)
library(gridExtra)
library(patchwork)
library(Amelia)
library(tidyverse)
library(broom)
library(mice)
library(reshape2)
library(sf)
library(rnaturalearth)
library(stringr)
library(rnaturalearthdata)
library(countrycode)
library(ggridges)
library(Matrix)
library(lme4)
library(glmmTMB)
library(forecast)
library(viridis)
library(astsa)

master_data = read.csv('C:/Users/liambiam/Documents/MSc Applied Data Science/MTHM604/Data/master_data/master_data.csv')

columns_to_include <- c('UPRN', 'datetime', 'External...PM2_5', 'External...TVOC', 'analog1Front.Room...PM2_5', 'temp2Front.Room...TVOC', 'SmokeinHouse')
uprns_to_exclude <- c(538, 961, 570, 1627, 69)

master_data_filtered <- master_data %>%
  select(all_of(columns_to_include)) %>%
  filter(!(UPRN %in% uprns_to_exclude)) %>%
  na.omit()

png("histogram.png", width = 800, height = 600, units = "px", pointsize = 12)

# Assuming master_data_filtered is your data frame
hist(master_data_filtered$External...PM2_5, 
     breaks = 100,
     main = "Frequency Histogram of External...PM2_5",
     xlab = "External...PM2_5",
     ylab = "Frequency",
     col = "skyblue",
     border = "black")

# Adjust the figure margins
par(mar = c(5, 4, 4, 2) + 0.1)

# Save the plot as a PNG image
dev.off()


master_data_filtered$datetime <- as.POSIXct(master_data_filtered$datetime, format="%Y-%m-%d %H:%M:%S")

unique_uprn <- unique(master_data_filtered$UPRN)


subset_data <- subset(master_data_filtered)
subset_data <- subset_data %>%
  mutate(hour = lubridate::hour(datetime)) %>%  # Extract hour from datetime
  group_by(hour) %>%
  summarise(
    avg_External_PM2_5 = mean(External...PM2_5),
    avg_External_TVOC = mean(External...TVOC),
    avg_analog1_PM2_5 = mean(analog1Front.Room...PM2_5),
    avg_temp2Front.Room_TVOC = mean(temp2Front.Room...TVOC)
    # Add more summarise functions for additional variables
  )
library(lattice)

# Assuming you have a datetime column named "datetime" in your subset_data
# Replace "avg_External_PM2_5" and "avg_analog1_PM2_5" with the actual column names in your dataset

png_file <- "ablag_plot_allMeanEvI.png"

png(png_file, width = 1000, height = 700) 

# Use the lag2.plot function
lag2.plot(subset_data$avg_External_PM2_5, subset_data$avg_analog1_PM2_5, 12, cex = 1.5)  # Adjust cex value as needed

# Adjust the figure margins more appropriately
par(mar = c(5, 4, 4, 2) + 0.1, oma = c(0, 0, 0, 0))

# Increase text size for axis values and other text elements
par(cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, cex.sub = 1.5)

# Save the plot as a PNG image
dev.off()



# Calculate cross-correlation
cross_correlation <- ccf(subset_data$avg_External_TVOC, subset_data$avg_temp2Front.Room_TVOC, lag.max = 12, plot = FALSE, na.action = na.pass)

# Plot and save the cross-correlation function
plot_filename <- "CCF_VOC_ALL_AVERAGE_EvI.png"
png(plot_filename)
plot(cross_correlation, main = "Cross-Correlation Function Plot")
dev.off()


hourly_avg_values <- master_data_filtered %>%
  group_by(UPRN, datetime) %>%
  summarize(
    avg_External_PM2_5 = mean(External...PM2_5, na.rm = TRUE),
    avg_External_TVOC = mean(External...TVOC, na.rm = TRUE),
    avg_analog1_PM2_5 = mean(analog1Front.Room...PM2_5, na.rm = TRUE),
    avg_temp2_TVOC = mean(temp2Front.Room...TVOC, na.rm = TRUE)
  )



# Function to calculate ccf and return max ACF value and lag position
calculate_max_acf <- function(data, uprn_value) {
  subset_data <- data %>% filter(UPRN == uprn_value)
  
  # Calculate cross-correlation function
  cross_correlation <- ccf(subset_data$avg_External_PM2_5, subset_data$avg_analog1_PM2_5, lag.max = 12, plot = FALSE, na.action = na.pass)
  
  # Get the lag with the maximum ACF value
  max_acf_lag <- which.max(cross_correlation$acf)
  max_acf_value <- cross_correlation$acf[max_acf_lag]
  
  return(list(uprn = uprn_value, max_acf_value = max_acf_value, max_acf_lag = max_acf_lag))
}

# Get unique UPRN values
unique_uprns <- unique(hourly_avg_values$UPRN)

# Initialize a list to store results
result_list <- list()

# Loop through each UPRN, calculate max ACF, and store results
for (uprn_value in unique_uprns) {
  result <- calculate_max_acf(hourly_avg_values, uprn_value)
  result_list[[as.character(uprn_value)]] <- result
  print(paste("Max ACF value for UPRN", uprn_value, ":", result$max_acf_value))
  print(paste("Lag with Max ACF for UPRN", uprn_value, ":", result$max_acf_lag-12))
}




