# MTHM604 PROJECT 1: AIR QUALITY: LINEAR REGRESSION ####

# Project setup ####

# Load libraries
library(tidyverse)
library(lubridate)
library(moments)
library(scales)
library(corrplot)
library(ggcorrplot)
library(ggpmisc)
library(gganimate)
library(ggridges)
library(gridExtra)
library(Hmisc)
library(viridis)


# Import data ####

# Import and preview UPRN table
uprn <- read_csv("C:/Users/liambiam/Documents/MSc Applied Data Science/MTHM604/Data/SurveyAndUprns/uprns.csv")
head(uprn)

# Import, preview and summarise hourly resampled time series data
path = "C:/Users/liambiam/Documents/MSc Applied Data Science/MTHM604/LR/"
master_data <- read_csv(paste(path, "master_data.csv", sep = "/"))
head(master_data)
summary(master_data)

# Process master data
master_data <- master_data %>%
  # Rename time series value columns
  rename(int_pm2_5 = `analog1Front Room - PM2_5`,
         int_voc = `temp2Front Room - TVOC`,
         ext_pm2_5 = `External - PM2_5`,
         ext_voc = `External - TVOC`) %>%
  # Remove PM2.5 data for outlier UPRNs
  gather(key = "series", value = "concentration",
         int_pm2_5, int_voc, ext_pm2_5, ext_voc) %>%
  filter(!(series %in% c("ext_pm2_5", "int_pm2_5") & UPRN %in% c(538, 961))) %>%
  spread(key = series, value = concentration) %>%
  # Join with fuel poverty data and heating data
  left_join(select(uprn, UPRN, FuelPovertyIndicated, Heating), by = "UPRN") %>%
  # Convert categorical values to factors
  mutate(UPRN = as.factor(UPRN),
         Area = as.factor(Area),
         propType0F = as.factor(propType0F),
         across(where(is.character), as.factor)) %>%
  # Sort by UPRN and datetime
  arrange(UPRN, datetime) %>%
  # Add differences
  mutate(diff_int_pm2_5 = int_pm2_5 - lag(int_pm2_5),
         diff_ext_pm2_5 = ext_pm2_5 - lag(ext_pm2_5),
         diff_int_voc = int_voc - lag(int_voc),
         diff_ext_voc = ext_voc - lag(ext_voc)) %>%
  # Count smoking events
  # Defined as change in PM2.5 > 8mug/m3
  mutate(smoking_event = if_else(SmokeinHouse == "Yes",
                                 as.numeric(diff_int_pm2_5 > 8),
                                 NA))
master_data


# Convert measurements from hourly samples to daily averages
master_data_daily <- master_data %>%
  # Add lagged data
  mutate(ext_pm2_5_lag = lag(ext_pm2_5, 1),
         ext_voc_lag = lag(ext_voc, 0)) %>%
  # Group by UPRN, date and factors and summarise
  group_by(UPRN, date = date(datetime),
           HouseholdSize, SmokeinHouse, propType0F, DistToARoad, NumberOfRooms,
           Area, FuelPovertyIndicated, Heating) %>%
  summarise(ext_pm2_5 = mean(ext_pm2_5, na.rm = TRUE),
            int_pm2_5 = mean(int_pm2_5, na.rm = TRUE),
            ext_voc = mean(ext_voc, na.rm = TRUE),
            int_voc = mean(int_voc, na.rm = TRUE),
            ext_pm2_5_lag = mean(ext_pm2_5_lag, na.rm = TRUE),
            ext_voc_lag = mean(ext_voc_lag, na.rm = TRUE),
            smoking_events = sum(smoking_event, na.rm = TRUE),
            .groups = "drop") %>%
  # Replace NaN with NA
  mutate(ext_pm2_5 = replace(ext_pm2_5, NaN, NA),
         int_pm2_5 = replace(int_pm2_5, NaN, NA),
         ext_voc = replace(ext_voc, NaN, NA),
         int_voc = replace(int_voc, NaN, NA),
         ext_pm2_5_lag = replace(ext_pm2_5_lag, NaN, NA),
         ext_voc_lag = replace(ext_voc_lag, NaN, NA)) %>%
  # Add transformed values, add alternative smoking assumption for UPRN 886
  mutate(x_pm = if_else(is.na(ext_pm2_5), NA, log(ext_pm2_5 + 1,10)),
         x_pm_lag = if_else(is.na(ext_pm2_5_lag), NA, log(ext_pm2_5_lag + 1,10)),
         y_pm = if_else(is.na(int_pm2_5), NA, log(int_pm2_5 + 1,10)),
         x_voc = if_else(is.na(ext_voc), NA, log(ext_voc + 1,10)),
         x_voc_lag = if_else(is.na(ext_voc_lag), NA, log(ext_voc_lag + 1,10)),
         y_voc = if_else(is.na(int_voc), NA, log(int_voc + 1,10))) %>%
  # Change distance to A-road to numerical
  mutate(DistToARoad = as.numeric(
    case_when(is.na(DistToARoad) ~ NA,
              is.na(as.numeric(as.character(DistToARoad))) ~ 800,
              .default = as.numeric(as.character(DistToARoad)))))
master_data_daily



#* PM2.5 model 8: model 7 + smoking_events ####
# Fit linear model
pm_lm8 <- lm(y_pm ~ x_pm
             + x_pm*SmokeinHouse + NumberOfRooms + FuelPovertyIndicated
             + DistToARoad + Heating + smoking_events,
             master_data_daily,
             na.action = "na.exclude")
# Diagnostics
lm_selec <- pm_lm8
summary(lm_selec)
AIC(lm_selec)
BIC(lm_selec)
# Residual plots
par(mfrow = c(1, 2))
plot(lm_selec, which = c(1, 2))
par(mfrow = c(1, 1))

master_data_daily$x_pm





# LR Model Train and Test

master_data_no_na <- na.omit(master_data_daily)

set.seed(123)

index <- sample(1:nrow(master_data_no_na), 0.8 * nrow(master_data_no_na))

train_data <- master_data_no_na[index, ]

test_data <- master_data_no_na[-index, ]


pm_lm8_train <- lm(y_pm ~ x_pm
                   + x_pm * SmokeinHouse + NumberOfRooms + FuelPovertyIndicated
                   + DistToARoad + Heating + smoking_events,
                   data = train_data,
                   na.action = "na.exclude")

summary(pm_lm8_train)

# Assuming you have a datetime column in your test_data
# Adjust the following line based on your actual column names
predictions <- predict(pm_lm8_train, newdata = test_data)

mse <- mean((test_data$y_pm - predictions)^2)

# Set up the PNG device
png("predictionsvsactual.png", width = 800, height = 600, units = "px")

# Create the plot
plot(test_data$date, test_data$y_pm, type = "l", col = "blue", lty = 1, ylim = range(test_data$y_pm, predictions))

# Assuming you have a datetime column in your test_data
# Adjust the following line based on your actual column names
lines(test_data$date, predictions, col = "red", lty = 2)

# Add legend
legend("topright", legend = c("Actual", "Predicted"), col = c("blue", "red"), lty = 1:2)

# Close the PNG device
dev.off()


test_data$predictions <- predict(pm_lm8_train, newdata = test_data)


ggplot(test_data, aes(x = y_pm, y = predictions)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # Adding a reference line for perfect predictions
  labs(title = "Scatter Plot of y_pm vs Predictions",
       x = "Actual Values (y_pm)",
       y = "Predicted Values") +
  theme_minimal()

ggplot(test_data, aes(x = y_pm, y = predictions, color = SmokeinHouse)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Scatter Plot of y_pm vs Predictions",
       x = "Actual Values (y_pm)",
       y = "Predicted Values",
       color = "SmokeinHouse") +
  theme_minimal()


residuals <- test_data$y_pm - predictions

ggplot(test_data, aes(x = y_pm, y = residuals)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residual Plot",
       x = "Actual Values",
       y = "Residuals") +
  theme_minimal()



mae <- mean(abs(test_data$y_pm - test_data$predictions))
mse <- mean((test_data$y_pm - test_data$predictions)^2)
rmse <- sqrt(mse)

cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")


ggplot(test_data, aes(x = value, fill = variable)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Actual vs Predicted Values",
       x = "Values",
       y = "Density",
       fill = "Variable") +
  theme_minimal() +
  facet_wrap(~variable, scales = "free_y")


unique_uprns <- unique(test_data$UPRN)

# Assuming the UPRN column is named "UPRN" in your test_data
# Adjust the following line based on your actual column names
uprn_counts <- table(test_data$UPRN)

# Find the UPRN with the most rows
most_common_uprn <- names(uprn_counts)[which.max(uprn_counts)]

cat("UPRN with the most rows:", most_common_uprn, "\n")


uprn_specific <- "1767"

# Filter data for the specific UPRN
subset_data <- subset(test_data, UPRN == uprn_specific)

# Create a line graph
ggplot(subset_data, aes(x = date)) +
  geom_line(aes(y = y_pm, color = "Actual"), size = 1) +
  geom_line(aes(y = predictions, color = "Predicted"), linetype = "dashed", size = 1) +
  labs(title = paste("Actual vs Predicted Time-Series for UPRN:", uprn_specific),
       y = "Values") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal()


rsquared_train <- summary(pm_lm8_train)$r.squared
cat("R-squared of the training model:", rsquared_train, "\n")

rsquared_predictions <- 1 - sum((test_data$y_pm - predictions)^2) / sum((test_data$y_pm - mean(test_data$y_pm))^2)
cat("R-squared based on predicted values:", rsquared_predictions, "\n")



residuals <- test_data$y_pm - predictions

press <- sum(residuals^2)

cat("PRESS:", press, "\n")













#* PM2.5: model diagnostics ####

pm_lm_diag <- tibble("Linear model" = c(1:10),
                    "Description" = c("Outdoor PM2.5",
                                      "+ Smoking",
                                      "+ No. rooms",
                                      "+ Dist. to A-road",
                                      "+ Fuel poverty",
                                      "+ Heating",
                                      "+ Smoking : Outdoor",
                                      "Alt: Smoking events",
                                      "Alt: Transformation",
                                      "Alt: Threshold"),
                    "Adj. R^2" = c(summary(pm_lm1)$adj.r.squared,
                                   summary(pm_lm2)$adj.r.squared,
                                   summary(pm_lm3)$adj.r.squared,
                                   summary(pm_lm4)$adj.r.squared,
                                   summary(pm_lm5)$adj.r.squared,
                                   summary(pm_lm6)$adj.r.squared,
                                   summary(pm_lm7)$adj.r.squared,
                                   summary(pm_lm8)$adj.r.squared,
                                   summary(pm_lm9)$adj.r.squared,
                                   summary(pm_lm10)$adj.r.squared),
                    "AIC" = c(AIC(pm_lm1), AIC(pm_lm2), AIC(pm_lm3),
                              AIC(pm_lm4), AIC(pm_lm5), AIC(pm_lm6),
                              AIC(pm_lm7), 
                              AIC(pm_lm8), AIC(pm_lm9), AIC(pm_lm10)),
                    "BIC" = c(BIC(pm_lm1), BIC(pm_lm2), BIC(pm_lm3),
                              BIC(pm_lm4), BIC(pm_lm5), BIC(pm_lm6),
                              BIC(pm_lm7),
                              BIC(pm_lm8), BIC(pm_lm9), BIC(pm_lm10)))
pm_lm_diag


#### Linear regression: VOC ####

#* VOC model 1: indoor ~ outdoor ####
# Fit linear model
voc_lm1 <- lm(y_voc ~ x_voc,
              master_data_daily,
              na.action = "na.exclude")
# Diagnostics
lm_selec <- voc_lm1
summary(lm_selec)
AIC(lm_selec)
BIC(lm_selec)
# Residual plots
par(mfrow = c(1, 2))
plot(lm_selec, which = c(1, 2))
par(mfrow = c(1, 1))


#* VOC model 2: indoor ~ outdoor_lagged ####
# Fit linear model
voc_lm2 <- lm(y_voc ~ x_voc_lag,
              master_data_daily,
              na.action = "na.exclude")
# Diagnostics
lm_selec <- voc_lm2
summary(lm_selec)
AIC(lm_selec)
BIC(lm_selec)
# Residual plots
par(mfrow = c(1, 2))
plot(lm_selec, which = c(1, 2))
par(mfrow = c(1, 1))


#* VOC model 3: all variables from PM2.5 model 7 ####
# Fit linear model
voc_lm3 <- lm(y_voc ~ x_voc
             + x_voc*SmokeinHouse + NumberOfRooms + DistToARoad
             + FuelPovertyIndicated + Heating,
             master_data_daily,
             na.action = "na.exclude")
# Diagnostics
lm_selec <- voc_lm3
summary(lm_selec)
AIC(lm_selec)
BIC(lm_selec)
# Residual plots
par(mfrow = c(1, 2))
plot(lm_selec, which = c(1, 2))
par(mfrow = c(1, 1))


#* VOC model 4: significant variables ####
# Fit linear model
voc_lm4 <- lm(y_voc ~ x_voc
              + SmokeinHouse + FuelPovertyIndicated,
              master_data_daily,
              na.action = "na.exclude")
# Diagnostics
lm_selec <- voc_lm4
summary(lm_selec)
AIC(lm_selec)
BIC(lm_selec)
# Residual plots
par(mfrow = c(1, 2))
plot(lm_selec, which = c(1, 2))
par(mfrow = c(1, 1))