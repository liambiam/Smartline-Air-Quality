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
uprn <- read_csv("C:/Users/howoo/OneDrive/Documents/Personal/Education/Master's/Exeter/04 MTHM604/02 Projects/01 Smartline/01 Data/SurveyAndUprns/uprns.csv")
head(uprn)

# Import, preview and summarise hourly resampled time series data
path = "C:/Users/howoo/OneDrive/Documents/Personal/Education/Master's/Exeter/04 MTHM604/02 Projects/01 Smartline/01 Data/Cleansed"
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


# Scatter plots ####

# Scatter plot of indoor vs outdoor

# Correlation of indoor and outdoor PM2.5
master_data_daily %>%
  ggplot(aes(x = ext_pm2_5, y = int_pm2_5)) +
  geom_point(alpha = 0.3, size = 1) +
  labs(title = "PM2.5 concentration: indoor vs outdoor, by UPRN and indoor smoking",
       x = "Outdoor PM2.5 concentration, log scale (μg/m3)",
       y = "Indoor PM2.5 concentration, log scale (μg/m3)") +
  scale_x_log10(labels = label_comma(drop0trailing = TRUE),
                breaks = 10^c(-3:2)) +
  scale_y_log10(labels = label_comma(drop0trailing = TRUE),
                breaks = 10^c(-3:2))
ggsave(paste("correlation/pm2.5/pm2.5_scatter", ".png", sep = ""), plot = last_plot())



# Scatter plot of indoor vs outdoor, faceted by smoking
# Set facet labels
smoke_labs <- c("Indoor smoking: No", "Indoor smoking: Yes")
names(smoke_labs) <- c("No", "Yes")
# Correlation of indoor and outdoor PM2.5
master_data_daily %>%
  filter(!is.na(SmokeinHouse)) %>%
  ggplot(aes(x = ext_pm2_5, y = int_pm2_5, col = UPRN)) +
  geom_point(alpha = 0.3, size = 1) +
  labs(title = "PM2.5 concentration: indoor vs outdoor, by UPRN and indoor smoking",
       x = "Outdoor PM2.5 concentration, log scale (μg/m3)",
       y = "Indoor PM2.5 concentration, log scale (μg/m3)") +
  scale_x_log10(labels = label_comma(drop0trailing = TRUE),
                breaks = 10^c(-3:2)) +
  scale_y_log10(labels = label_comma(drop0trailing = TRUE),
                breaks = 10^c(-3:2)) +
  facet_wrap(~ SmokeinHouse, labeller = labeller(SmokeinHouse = smoke_labs))
ggsave(paste("correlation/pm2.5/pm2.5_scatter_uprn_smoking", ".png", sep = ""), plot = last_plot())



# Scatter plot of indoor vs outdoor, faceted by heating
# Correlation of indoor and outdoor PM2.5
master_data_daily %>%
  ggplot(aes(x = ext_pm2_5, y = int_pm2_5, col = Heating)) +
  geom_point(alpha = 0.3, size = 1) +
  labs(title = "PM2.5 concentration: indoor vs outdoor, by UPRN and heating type",
       x = "Outdoor PM2.5 concentration, log scale (μg/m3)",
       y = "Indoor PM2.5 concentration, log scale (μg/m3)") +
  scale_x_log10(labels = label_comma(drop0trailing = TRUE),
                breaks = 10^c(-3:2)) +
  scale_y_log10(labels = label_comma(drop0trailing = TRUE),
                breaks = 10^c(-3:2)) +
  facet_wrap(~ Heating)
ggsave(paste("correlation/pm2.5/pm2.5_scatter_heating", ".png", sep = ""), plot = last_plot())



# Linear regression: PM2.5 ####

#* PM2.5 model 1: indoor ~ outdoor ####
# Fit linear model
pm_lm1 <- lm(y_pm ~ x_pm,
             master_data_daily,
             na.action = "na.exclude")
# Diagnostics
lm_selec <- pm_lm1
summary(lm_selec)
AIC(lm_selec)
BIC(lm_selec)
# Residual plots
par(mfrow = c(1, 2))
plot(lm_selec, which = c(1, 2))
par(mfrow = c(1, 1))


#** Testing lags ####
# Fit linear model
pm_lm1a <- lm(y_pm ~ x_pm_lag,
             master_data_daily,
             na.action = "na.exclude")
# Diagnostics
lm_selec <- pm_lm1a
summary(lm_selec)
AIC(lm_selec)
BIC(lm_selec)
# Residual plots
par(mfrow = c(1, 2))
plot(lm_selec, which = c(1, 2))
par(mfrow = c(1, 1))


#* PM2.5 model 2: indoor ~ outdoor + smoking ####
# Fit linear model
pm_lm2 <- lm(y_pm ~ x_pm + SmokeinHouse,
             master_data_daily,
             na.action = "na.exclude")
# Diagnostics
lm_selec <- pm_lm2
summary(lm_selec)
AIC(lm_selec)
BIC(lm_selec)
# Residual plots
par(mfrow = c(1, 2))
plot(lm_selec, which = c(1, 2))
par(mfrow = c(1, 1))


# # Fit hourly linear model
# # PM2.5
# y = ifelse(is.infinite(log(master_data$int_pm2_5)), NA, log(lag(master_data$int_pm2_5,0) + 1,10))
# x = ifelse(is.infinite(log(master_data$ext_pm2_5)), NA, log(lag(master_data$ext_pm2_5,0) + 1,10))
# # Model 1
# pm_lm1_hr <- lm(y ~ x + SmokeinHouse
#                 + SmokeinHouse + NumberOfRooms + DistToARoad
#                 + FuelPovertyIndicated + Heating,
#              master_data,
#              na.action = "na.exclude")
# # Diagnostics
# lm_selec <- pm_lm1_hr
# summary(lm_selec)
# AIC(lm_selec)
# BIC(lm_selec)



#* PM2.5 model 3: indoor ~ outdoor + smoking + num_rooms ####
# Fit linear model
pm_lm3 <- lm(y_pm ~ x_pm
             + SmokeinHouse + NumberOfRooms,
             master_data_daily,
             na.action = "na.exclude")
# Diagnostics
lm_selec <- pm_lm3
summary(lm_selec)
AIC(lm_selec)
BIC(lm_selec)
# Residual plots
par(mfrow = c(1, 2))
plot(lm_selec, which = c(1, 2))
par(mfrow = c(1, 1))


#** Testing other proxies for property size ####          

# PM2.5 model 4a: indoor ~ outdoor + smoking + household_size
# Fit linear model
pm_lm4a <- lm(y_pm ~ x_pm
              + SmokeinHouse
              + HouseholdSize,
              master_data_daily,
              na.action = "na.exclude")
# Diagnostics
lm_selec <- pm_lm4a
summary(lm_selec)
AIC(lm_selec)
BIC(lm_selec)
# Residual plots
par(mfrow = c(1, 2))
plot(lm_selec, which = c(1, 2))
par(mfrow = c(1, 1))


# PM2.5 model 4b: indoor ~ outdoor + smoking + prop_type
# Fit linear model
pm_lm4b <- lm(y_pm ~ x_pm
              + SmokeinHouse + propType0F,
              master_data_daily,
              na.action = "na.exclude")
# Diagnostics
lm_selec <- pm_lm4b
summary(lm_selec)
AIC(lm_selec)
BIC(lm_selec)
# Residual plots
par(mfrow = c(1, 2))
plot(lm_selec, which = c(1, 2))
par(mfrow = c(1, 1))


# PM2.5 model 4c: indoor ~ outdoor + smoking + prop_type
# Fit linear model
pm_lm4c <- lm(y_pm ~ x_pm
              + SmokeinHouse + NumberOfRooms,
              master_data_daily,
              na.action = "na.exclude")
# Diagnostics
lm_selec <- pm_lm4c
summary(lm_selec)
AIC(lm_selec)
BIC(lm_selec)
# Residual plots
par(mfrow = c(1, 2))
plot(lm_selec, which = c(1, 2))
par(mfrow = c(1, 1))



#* PM2.5 model 4: indoor ~ outdoor + smoking + num_rooms + dist_A_road ####
# Fit linear model
pm_lm4 <- lm(y_pm ~ x_pm
             + SmokeinHouse + NumberOfRooms + DistToARoad,
             master_data_daily,
             na.action = "na.exclude")
# Diagnostics
lm_selec <- pm_lm4
summary(lm_selec)
AIC(lm_selec)
BIC(lm_selec)
# Residual plots
par(mfrow = c(1, 2))
plot(lm_selec, which = c(1, 2))
par(mfrow = c(1, 1))


#* PM2.5 model 5: indoor ~ outdoor + smoking + num_rooms + dist_A_road + fuel_poverty ####
# Fit linear model
pm_lm5 <- lm(y_pm ~ x_pm
             + SmokeinHouse + NumberOfRooms + DistToARoad
             + FuelPovertyIndicated,
             master_data_daily,
             na.action = "na.exclude")
# Diagnostics
lm_selec <- pm_lm5
summary(lm_selec)
AIC(lm_selec)
BIC(lm_selec)
# Residual plots
par(mfrow = c(1, 2))
plot(lm_selec, which = c(1, 2))
par(mfrow = c(1, 1))


#* PM2.5 model 6: indoor ~ outdoor + smoking + num_rooms + dist_A_road + fuel_poverty + heating ####
# Fit linear model
pm_lm6 <- lm(y_pm ~ x_pm
             + SmokeinHouse + NumberOfRooms + DistToARoad
             + FuelPovertyIndicated + Heating,
             master_data_daily,
             na.action = "na.exclude")
# Diagnostics
lm_selec <- pm_lm6
summary(lm_selec)
AIC(lm_selec)
BIC(lm_selec)
# Residual plots
par(mfrow = c(1, 2))
plot(lm_selec, which = c(1, 2))
par(mfrow = c(1, 1))


#* PM2.5 model 7: model 7 + significant interactions ####
# Fit linear model
pm_lm7 <- lm(y_pm ~ x_pm
             + x_pm*SmokeinHouse + NumberOfRooms + DistToARoad
             + FuelPovertyIndicated + Heating,
             master_data_daily,
             na.action = "na.exclude")
# Diagnostics
lm_selec <- pm_lm7
summary(lm_selec)
AIC(lm_selec)
BIC(lm_selec)
# Residual plots
par(mfrow = c(1, 2))
plot(lm_selec, which = c(1, 2))
par(mfrow = c(1, 1))


#** Testing interactions ####
# Fit linear model
pm_lm7a <- lm(y_pm ~ x_pm
             + x_pm*SmokeinHouse + x_pm*NumberOfRooms
             + x_pm*DistToARoad + x_pm*FuelPovertyIndicated
             + x_pm*Heating,
             master_data_daily,
             na.action = "na.exclude")
# Diagnostics
lm_selec <- pm_lm7a
summary(lm_selec)
AIC(lm_selec)
BIC(lm_selec)
# Residual plots
par(mfrow = c(1, 2))
plot(lm_selec, which = c(1, 2))
par(mfrow = c(1, 1))




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


#* PM2.5 model 9: model 8 + transformation of y ####
# Fit linear model
pm_lm9 <- lm(y_pm^0.5 ~ x_pm
             + x_pm*SmokeinHouse + NumberOfRooms + FuelPovertyIndicated
             + DistToARoad + Heating,
             master_data_daily,
             na.action = "na.exclude")
# Diagnostics
lm_selec <- pm_lm9
summary(lm_selec)
AIC(lm_selec)
BIC(lm_selec)
# Residual plots
par(mfrow = c(1, 2))
plot(lm_selec, which = c(1, 2))
par(mfrow = c(1, 1))


#* PM2.5 model 10: model 8 + threshold ####
# Fit linear model
pm_lm10 <- lm(y_pm^0.5 ~ x_pm
              + x_pm*SmokeinHouse + NumberOfRooms + FuelPovertyIndicated
              + DistToARoad + Heating,
              master_data_daily %>% filter(int_pm2_5 > 2),
              na.action = "na.exclude")
# Diagnostics
lm_selec <- pm_lm10
summary(lm_selec)
AIC(lm_selec)
BIC(lm_selec)
# Residual plots
par(mfrow = c(1, 2))
plot(lm_selec, which = c(1, 2))
par(mfrow = c(1, 1))


#* PM2.5: outliers ####

# Plot residuals, faceted by UPRN
lm_selec <- pm_lm7 # Select model
master_data_daily %>%
  mutate(fitted_pm = fitted(lm_selec),
         resid_pm = resid(lm_selec)) %>%
  filter(SmokeinHouse == "Yes") %>%
  ggplot(aes(x = fitted_pm, y = resid_pm, col = UPRN)) +
  geom_point() +
  facet_wrap(~ UPRN)


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