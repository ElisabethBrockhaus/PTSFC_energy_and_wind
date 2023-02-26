rm(list=ls())

library(tseries)
library(zoo)
library(dplyr)
library(tidyr)
library(tibble)
library(lubridate)
library(readr)
library(data.table)
library(arrow)
library(rjson)
library(ggplot2)
library(ggpubr)
library(forecast)
library(crch)
library(lmtest)

getwd()

options("scipen"=100, "digits"=4, dplyr.summarise.inform = FALSE)

#forecast_date <- as.character(today())
forecast_date <- "2023-02-15"


################
# Power demand #
################

### read data ###

timestamps <- fromJSON(file="https://www.smard.de/app/chart_data/410/DE/index_quarterhour.json")[[1]]

# get history
energy_file_path <- "data/energy/gesamtverbrauch_2022_11_06.csv"

if(!file.exists(energy_file_path)){
  energy_data <- data.frame()
  
  for (timestamp in timestamps[1:410]){
    print(as_datetime(timestamp/1000))
    data_source <- paste0("https://www.smard.de/app/chart_data/410/DE/410_DE_quarterhour_", timestamp, ".json")
    energy_list <- fromJSON(file = data_source)[["series"]]
  
    energy_data_raw  <- data.frame(matrix(unlist(energy_list, recursive = FALSE),
                                          nrow = length(energy_list),
                                          byrow = TRUE))
    energy_data_raw$X2[sapply(energy_data_raw$X2, is.null)] <- NA
  
    energy_data_short <- data.frame(matrix(unlist(energy_data_raw),
                                     nrow = length(energy_data_raw$X2),
                                     byrow = FALSE)) %>%
      rename(date_time = X1, gesamt = X2) %>%
      mutate(date_time = as_datetime(date_time/1000))
  
    energy_data <- rbind(energy_data, energy_data_short)
  }
  
  energy_data <- energy_data %>%
    na.omit()
  
  View(energy_data)
  
  write_csv(energy_data, energy_file_path)

} else {
  energy_data <- read_csv(energy_file_path)
} 

# add data after November 6, 2022
for (timestamp in timestamps[411:length(timestamps)]){
  print(as_datetime(timestamp/1000))
  data_source <- paste0("https://www.smard.de/app/chart_data/410/DE/410_DE_quarterhour_", timestamp, ".json")
  energy_list <- fromJSON(file = data_source)[["series"]]
  
  energy_data_raw  <- data.frame(matrix(unlist(energy_list, recursive = FALSE),
                                        nrow = length(energy_list),
                                        byrow = TRUE))
  energy_data_raw$X2[sapply(energy_data_raw$X2, is.null)] <- NA
  
  energy_data_short <- data.frame(matrix(unlist(energy_data_raw),
                                         nrow = length(energy_data_raw$X2),
                                         byrow = FALSE)) %>%
    rename(date_time = X1, gesamt = X2) %>%
    mutate(date_time = as_datetime(date_time/1000))
  
  energy_data <- rbind(energy_data, energy_data_short)
}

# aggregate hourly and convert from MWh to GWh
energy_data <- setDT(energy_data)[,lapply(.SD, sum), by=floor_date(date_time,"hour")] %>%
  na.omit() %>%
  mutate(gesamt = gesamt/1000)

energy_data_future <- energy_data %>%
  dplyr::filter(floor_date > as_datetime(forecast_date))

energy_data <- energy_data %>%
  dplyr::filter(floor_date <= as_datetime(forecast_date))

# add time series differenced once
energy_data <- energy_data %>%
  mutate(gesamt_diff = gesamt - lag(gesamt))


### some plots of raw data ###

# plot energy time series for different time frames
energy_raw_years_plot <- ggplot(energy_data, aes(x=floor_date, y=gesamt)) +
  geom_line() +
  scale_x_datetime() +
  coord_cartesian(xlim = c(as_datetime("2019-02-15"), as_datetime("2023-02-15")),
                  ylim = c(32, 82), expand=F) +
  labs(x = NULL, y = "energy demand [GWh]")
energy_raw_weeks_plot <- ggplot(energy_data, aes(x=floor_date, y=gesamt)) +
  geom_line() +
  scale_x_datetime() +
  coord_cartesian(xlim = c(as_datetime("2023-01-18"), as_datetime("2023-02-15")),
                  ylim = c(39, 77), expand=F) +
  labs(x = NULL, y = "energy demand [GWh]")
energy_raw_plot <- ggarrange(energy_raw_years_plot, energy_raw_weeks_plot,
                             ncol=1, nrow=2, labels = list("A", "B"))
print(energy_raw_plot)
ggsave(energy_raw_plot, filename = "otherFiles/plot_energy_raw.png",  bg = "transparent",
       width = 8.8, height = 4.5)

# plot differenced version of time series
energy_diff_plot <- ggplot(energy_data, aes(x=floor_date, y=gesamt_diff)) +
  geom_line() +
  scale_x_datetime() +
  coord_cartesian(xlim = c(as_datetime("2022-02-15"), as_datetime("2023-02-15")),
                  ylim = c(-5, 9), expand=F) +
  labs(x = NULL, y = "changes in energy demand [GWh]")
print(energy_diff_plot)

# look at (partial) autocorrelation
par(mfrow=c(2,1))
energy_data$gesamt %>% acf(lag=24*7+1, main="acf raw")
energy_data$gesamt %>% pacf(lag=24*7+1, main="pacf raw")
energy_data$gesamt %>% diff(lag=24) %>% acf(lag=24*7+1, main="acf diff 24")
energy_data$gesamt %>% diff(lag=24) %>% pacf(lag=24*7+1, main="pacf diff 24")
par(mfrow=c(1,1))


### holiday/weekend regressor ###

# read holidays from API
holidays <- read.csv("https://www.spiketime.de/feiertagapi/feiertage/csv/2014/2023", sep=";") %>%
  transmute(date = as_date(Datum)) %>%
  unique()

# create dummies for weekends/holidays/both
energy_data <- energy_data %>%
  mutate(is_weekend = ifelse(weekdays(floor_date) %in% c("Samstag", "Sonntag"), 1, 0),
         is_holiday = ifelse(as_date(floor_date) %in% holidays$date, 1, 0),
         is_holiday_weekend = is_weekend*is_holiday)

# compare mean energy demand between the four categories
# non-holiday weekday
mean(energy_data[energy_data$is_weekend+energy_data$is_holiday==0]$gesamt)
# holiday weekday
mean(energy_data[(energy_data$is_weekend==0)&(energy_data$is_holiday==1)]$gesamt)
# non-holiday weekend
mean(energy_data[(energy_data$is_weekend==1)&(energy_data$is_holiday==0)]$gesamt)
# holiday weekend
mean(energy_data[energy_data$is_holiday_weekend==1]$gesamt)

# split data into historic part used for target encoding and model input
energy_data_historic <- energy_data %>%
  filter(floor_date<as_date("2021-09-08")) # used to create alternative regressor to weekend/holiday dummies
energy_data <- energy_data %>%
  filter(floor_date>=as_date("2021-09-08"))

# four each hour of the day calculate mean per category
avg_00 <- avg_01 <- avg_10 <- avg_11 <- rep(NA,24)
for (h in 0:23){
  dat <- energy_data_historic[hour(energy_data_historic$floor_date)==h]
  # average non-holiday weekday
  avg_00[h+1] <- mean(dat[dat$is_weekend+dat$is_holiday==0]$gesamt, na.rm = T)
  # average holiday weekday
  avg_01[h+1] <- mean(dat[(dat$is_weekend==0)&(dat$is_holiday==1)]$gesamt, na.rm = T)
  # average non-holiday weekend
  avg_10[h+1] <- mean(dat[(dat$is_weekend==1)&(dat$is_holiday==0)]$gesamt, na.rm = T)
  # average holiday weekend
  avg_11[h+1] <- mean(dat[dat$is_weekend+dat$is_holiday==2]$gesamt, na.rm = T)
}
avg_data <- cbind(avg_00, avg_01, avg_10, avg_11)
avg_data_long <- data.frame(avg_data) %>%
  rowid_to_column() %>%
  rename(hour = rowid,
         "normal weekday" = avg_00, "holiday weekday" = avg_01,
         "normal weekend" = avg_10, "holiday weekend" = avg_11) %>%
  pivot_longer(cols=!c("hour"), names_to = "day_type", values_to = "value")
# plot day type variable
day_types_plot <- ggplot() +
  geom_line(data=avg_data_long, aes(x=hour, y=value, color=day_type)) +
  scale_color_brewer(palette = "Set1") +
  labs(color='day type') +
  coord_cartesian(ylim=c(38,71), expand=F)
print(day_types_plot)
ggsave(day_types_plot, filename = "otherFiles/plot_day_type.png",  bg = "transparent",
       width = 6, height = 3)

# add day_type variable to the training data depending on weekend and holiday dummy
energy_data$day_type <- 0
for (row in 1:dim(energy_data)[1]){
  energy_data[row, "day_type"] <- avg_data[(hour(energy_data[row,]$floor_date)+1),
                                           paste0("avg_",
                                                  energy_data[row,]$is_weekend,
                                                  energy_data[row,]$is_holiday)]
}

# construct variable to account for yearly periodicity
get_hour_of_year <- function(time) {
  (as.integer(format(time, '%j')) - 1) * 24 + as.integer(format(time, '%H'))
}
season_sin <- sin(2*pi/(365*24) * get_hour_of_year(energy_data$floor_date))
season_cos <- cos(2*pi/(365*24) * get_hour_of_year(energy_data$floor_date))

# compare course of seasonality variables to behavior of energy demand 
plot(energy_data$floor_date, energy_data$gesamt, type="l", xlab="date", ylab="energy demand")
lines(energy_data$floor_date, 12*season_sin+55, col="blue")
lines(energy_data$floor_date, 12*season_cos+55, col="red")
legend("topright", legend=c("energy demand", "12*season_sin+55", "12*season_cos+55"), lty=1, col=c("black", "blue", "red"))


### temperature regressor ###

# get all Wednesdays for which ensemble forecast is available
d <- as_date("2021-09-08")  # 1st Wednesday data set
wednesdays <- gsub("-", "", seq(d, today(), by="week"))

# read forecasts issued on wednesdays and calculate ensemble mean
temperature <- data.frame(fcst_hour=0:120)
for (w in wednesdays){
  try({file <- paste0("../kit-weather-ensemble-point-forecast-karlsruhe/icon-eu-eps_", w, "00_t_2m_Karlsruhe.txt")
  temp_w <- read_delim(file, delim = "|", skip=3, col_types = cols()) %>%
    dplyr::select(!c(`...1`, `...43`)) %>%
    dplyr::rename_all(list(~gsub(" ", "", .))) %>%
    mutate_all(as.numeric) %>%
    transmute(fcst_hour = fcst_hour,
              !!w := rowMeans(dplyr::select(., 1:40), na.rm=T))
  temperature <- temperature %>%
    dplyr::full_join(temp_w, by = "fcst_hour")})
}
View(temperature)

# format the temperature such that it can be merged with the energy data
temperature_long <- temperature %>%
  pivot_longer(cols = starts_with("202"), names_to = "date", values_to = "temp_mean") %>%
  mutate(date = as_datetime(date, format = "%Y%m%d", tz="CET")) %>%
  mutate(floor_date = date + hours(fcst_hour)) %>%
  dplyr::select(floor_date, temp_mean) %>%
  mutate(floor_date = with_tz(floor_date, "UTC")) %>%
  full_join(energy_data %>% dplyr::select(floor_date), by="floor_date") %>%
  arrange(floor_date) %>%
  dplyr::filter(!is.na(floor_date)) %>%
  dplyr::filter(floor_date < as_date(forecast_date) + hours(120)) %>%
  mutate(hour=hour(floor_date)) %>%
  group_by(hour) %>%
  mutate(temp_mean = na.approx(temp_mean, na.rm = F)) %>% # linearly interpolate missing between previous and next available value from same hour
  ungroup() %>%
  dplyr::select(!hour) %>%
  mutate(temp_mean = na.approx(temp_mean, na.rm = F)) # linearly interpolate missing at the end of time series where no next value is available
View(temperature_long)

# combine model input and filter for hours of interest
model_input <- energy_data[,c("floor_date", "gesamt", "day_type")] %>%
  left_join(temperature_long, by="floor_date") %>%
  dplyr::filter(hour(floor_date) %in% c(11,15,19))
x_reg_names <- c("day_type", "temp_mean")


### hyperparameter tuning ###

# grid search over SARIMA orders

# construct data frame with parameter combinations that should be compared
# grid <- data.frame(p=NA, d=NA, q=NA, P=NA, Q=NA, m=NA, AIC=NA, BIC=NA)
# for(p in 1:4){
#   for(d in 0:1){
#     for(q in 1:2){
#       for(P in 1:3){
#         for(Q in 1:2){
#           for(m in c(3, 7*3)){
#             grid[nrow(grid) + 1,] = c(p, d, q, P, Q, m, NA, NA)
#           }
#         }
#       }
#     }
#   }
# }
# grid <- grid[2:dim(grid)[1],]
# rownames(grid) <-1:dim(grid)[1]
# 
# fit model for each parameter combination and save the corresponding AIC/BIC values
# for(row in 1:dim(grid)[1]){
#   try(
#     {model <- model_input$gesamt %>%
#       Arima(order=c(grid[row,p], grid[row,d], grid[row,q]),
#             seasonal=list(order=c(grid[row,P], 0, grid[row,Q]),
#                           period=grid[row,m]),
#             xreg = as.matrix(model_input %>% select(all_of(x_reg_names))))
#     grid[row,c("AIC", "BIC")] = c(model$aic, model$bic)
#     print(grid[row,])
#     rm(model)
#   })
# }
# write_csv(grid, "otherFiles/energy_order_grid_AIC_BIC.csv")

# read results of grid search and choose model which minimizes AIC
grid <- read_csv("otherFiles/energy_order_grid_AIC_BIC.csv")
par_minAIC <- grid[which.min(grid$AIC),]
par_minBIC <- grid[which.min(grid$BIC),]
print(par_minAIC)
print(par_minBIC)


### final model ###

# fit model
energy_model_sarimax_minAIC <- model_input$gesamt %>%
  Arima(order=c(par_minAIC$p,par_minAIC$d,par_minAIC$q),
        seasonal=list(order=c(par_minAIC$P,0,par_minAIC$Q),
                      period=par_minAIC$m),
        xreg = as.matrix(model_input %>% select(all_of(x_reg_names))))
# look at estimated coefficients and corresponding p-values
coeftest(energy_model_sarimax_minAIC)

# get residuals and look for remaining autocorrelation
resid_sarimax <- energy_model_sarimax_minAIC %>% residuals()

par(mfrow=c(2,1))
resid_sarimax %>% na.omit() %>% acf(lag=3*7+1, main="")
resid_sarimax %>% na.omit() %>% pacf(lag=3*7+1, main="")
par(mfrow=c(1,1))

# plot in-sample fit
data_fitted <- data.frame(datetime=model_input$floor_date, true=energy_model_sarimax_minAIC$x, fitted=fitted(energy_model_sarimax_minAIC)) %>%
  pivot_longer(cols=c("true", "fitted"), names_to = "type")
plot_model_fit <- ggplot() +
  geom_line(data = data_fitted, aes(x=datetime, y=value, color=type)) +
  scale_color_brewer(palette = "Set1") +
  coord_cartesian(xlim=c(as_datetime("2022-12-01"), as_datetime("2023-02-15")),
                  ylim=c(43,76), expand=F) +
  theme(legend.position="bottom") +
  xlab(element_blank()) + ylab("energy demand [GWh]")
print(plot_model_fit)


### predictions ###

# construct regressors for forecast period
date_time <- seq(as_datetime(forecast_date), length.out=100, by="hour")
is_weekend_pred <- ifelse(weekdays(date_time) %in% c("Samstag", "Sonntag"), 1, 0)
is_holiday_pred <- ifelse(as_date(date_time) %in% holidays$date, 1, 0)
is_holiday_weekend_pred <- is_weekend_pred*is_holiday_pred
season_sin_pred <- sin(2*pi/(365*24) * get_hour_of_year(date_time))
season_cos_pred <- cos(2*pi/(365*24) * get_hour_of_year(date_time))
day_type_pred <- data.frame(date_time, rep(0, length(date_time)))
colnames(day_type_pred) <- c("floor_date", "day_type")
for (row in 1:length(date_time)){
  day_type_pred[row, "day_type"] <- avg_data[(hour(date_time[row])+1),
                                             paste0("avg_",
                                                    is_weekend_pred[row],
                                                    is_holiday_pred[row])]
}
temp_mean_pred <- temperature_long %>% dplyr::filter(floor_date %in% date_time) %>%
  mutate(floor_date = with_tz(floor_date, "UTC"))
new_x_reg <- day_type_pred %>% full_join(temp_mean_pred, by="floor_date")
model_input_future <- new_x_reg %>%
  dplyr::filter(hour(floor_date) %in% c(11,15,19))

# predict the energy demand using the fitted SARIMAX model
pred <- predict(energy_model_sarimax_minAIC, n.ahead = dim(model_input_future)[1],
                newxreg = model_input_future %>% select(all_of(x_reg_names)))
pred_sarima <- data.frame(model_input_future$floor_date, pred$pred, pred$se) %>%
  rename(date_time = model_input_future.floor_date, q0.5 = pred.pred, se = pred.se) %>%
  mutate(q0.025 = q0.5 + se * qnorm(0.025),
         q0.25 = q0.5 + se * qnorm(0.25),
         q0.75 = q0.5 + se * qnorm(0.75),
         q0.975 = q0.5 + se * qnorm(0.975))

# plot recent history and forecast with quantiles
energy_plot <- ggplot() +
  geom_line(data = model_input, aes(x=floor_date, y=gesamt)) +
  scale_x_datetime() +
  coord_cartesian(xlim = c(as_datetime(forecast_date) - weeks(6), as_datetime(forecast_date) + hours(100)),
                  expand=F) +
  labs(x = NULL, y = "energy demand [GWh]")
energy_plot <- energy_plot +
  geom_line(data=pred_sarima, aes(x=date_time, y=q0.5), color="red") +
  geom_ribbon(data = pred_sarima,
              aes(x=date_time, ymin=q0.25, ymax=q0.75), alpha=.3, fill="red") +
  geom_ribbon(data = pred_sarima,
              aes(x=date_time, ymin=q0.025, ymax=q0.975), alpha=.15, fill="red")
print(energy_plot)

# determine datetimes for which forecasts are wanted
horizons <- c(36, 40, 44, 60, 64, 68)
forecast_datetimes <- as_date(forecast_date) + days(1) + hours(horizons - 1) # correct for UTC
print(with_tz(forecast_datetimes, "CET"))

# Make submission file (rows are horizons, cols are quantile levels)
pred_energy_sarima <- data.frame(forecast_date = forecast_date, 
                                 target = "energy", horizon = paste(horizons, "hour"),
                                 q0.025 = NA, q0.25 = NA, q0.5 = NA, q0.75 = NA, 
                                 q0.975 = NA)
for (q in c("q0.025", "q0.25", "q0.5", "q0.75", "q0.975")){
  pred_energy_sarima[,q] <- pred_sarima[pred_sarima$date_time %in% forecast_datetimes,q]
}
View(pred_energy_sarima)


### baseline model ###

pred_energy_baseline <- energy_data %>%
  dplyr::select(floor_date, gesamt) %>%
  mutate(weekday = weekdays(floor_date), hour = hour(floor_date)) %>%
  group_by(weekday, hour) %>%
  summarise(q0.025 = quantile(gesamt, 0.025),
            q0.25 = quantile(gesamt, 0.25),
            q0.5 = quantile(gesamt, 0.5),
            q0.75 = quantile(gesamt, 0.75),
            q0.975 = quantile(gesamt, 0.975)) %>%
  dplyr::filter((weekday %in% weekdays(forecast_datetimes)) &
                  (hour %in% hour(forecast_datetimes))) %>%
  ungroup() %>%
  mutate(forecast_date = !!forecast_date,
         target = "energy",
         horizon = paste(horizons, "hour")) %>%
  dplyr::select(!c("weekday", "hour")) %>%
  relocate(c(forecast_date, target, horizon))
View(pred_energy_baseline)

pred_baseline <- data.frame(forecast_datetimes, pred_energy_baseline %>% select(starts_with("q")))

energy_plot <- energy_plot +
  geom_line(data=pred_baseline, aes(x=forecast_datetimes, y=q0.5), color="blue") +
  geom_ribbon(data = pred_baseline,
              aes(x=forecast_datetimes, ymin=q0.25, ymax=q0.75), alpha=.3, fill="blue") +
  geom_ribbon(data = pred_baseline,
              aes(x=forecast_datetimes, ymin=q0.025, ymax=q0.975), alpha=.15, fill="blue")
print(energy_plot)


### model evaluation ###

quantile_score <- function(y_true, q_forecast, tau){
  indicator <- ifelse(y_true < q_forecast, 1, 0)
  score <- 2 * (y_true - q_forecast) * (tau - indicator)
  return(score)
}
quantile_levels <- c(0.025,0.25,0.5,0.75,0.975)

qs_sum_sarimax <- qs_sum_baseline <- 0
for (tau in quantile_levels){
  y_true <- energy_data_future[energy_data_future$floor_date == forecast_datetimes[1],]$gesamt
  q_fc_sarimax <- pred_sarima[pred_sarima$date_time == forecast_datetimes[1], paste0("q", tau)]
  q_fc_baseline <- pred_baseline[pred_baseline$forecast_datetimes == forecast_datetimes[1], paste0("q", tau)]
  qs_sum_sarimax <- qs_sum_sarimax + quantile_score(y_true, q_fc_sarimax, tau)
  qs_sum_baseline <- qs_sum_baseline + quantile_score(y_true, q_fc_baseline, tau)
}
qs_sum_sarimax
qs_sum_baseline


###########
# Weather #
###########

data_dir <- "data/weather/"
wind_data_raw <- read_feather(paste0(data_dir, "icon_eps_wind_10m_KA.feather"))
wind_data_raw <- wind_data_raw[!is.na(wind_data_raw$obs),]
wind_data_raw$ens_sd <- sqrt(wind_data_raw$ens_var)

today <- gsub("-", "", forecast_date)

# new_fcst <- read.table(file = paste0("D:/EllasDaten/Uni/Wirtschaftsmathe/ProbabilisticTimeSeriesForecastingChallenge/kit-weather-ensemble-point-forecast-karlsruhe/", 
#                                      "icon-eu-eps_", today, "00_wind_mean_10m_Karlsruhe.txt"), 
#                        sep = "|", 
#                        header = TRUE)
# 
# new_fcst[,1] <- NULL
# new_fcst[,ncol(new_fcst)] <- NULL

quantile_levels <- c(0.025,0.25,0.5,0.75,0.975)
lt_list <- c(36 + (0:4)*12)

# fc_wind_ens <- data.frame(forecast_date = as.Date(today, format = "%Y%M%d"), 
#                           target = "wind", 
#                           horizon = lt_list, 
#                           q0.025 = NA, q0.25 = NA, q0.5 = NA, q0.75 = NA, q0.975 = NA) 
# fc_wind_emos <- fc_wind_ens 

for(this_lt in lt_list){

  wind_data <- subset(wind_data_raw, fcst_hour == this_lt)
}
#   wind_benchmark2 <- crch(obs ~ ens_mean|ens_sd,
#                           data = wind_data,
#                           dist = "gaussian",
#                           left = 0,
#                           truncated = TRUE,
#                           link.scale = "log", 
#                           type = "crps")
#   
#   ens_fc <- new_fcst[new_fcst$fcst_hour == this_lt,][2:ncol(new_fcst)]
#   ens_fc <- as.numeric(ens_fc)
#   
#   fc_wind_ens[which(fc_wind_ens$horizon == this_lt),4:8] <- quantile(ens_fc, quantile_levels)
#   
#   pred_df <- data.frame(ens_mean = mean(ens_fc), ens_sd = sd(ens_fc))
#   
#   wind_benchmark2_loc <- predict(wind_benchmark2,
#                                  pred_df,
#                                  type = "location")
#   wind_benchmark2_sc <- predict(wind_benchmark2,
#                                 pred_df,
#                                 type = "scale")
#   
#   fc_wind_emos[which(fc_wind_emos$horizon == this_lt),4:8] <- qtnorm(quantile_levels, wind_benchmark2_loc, wind_benchmark2_sc, left = 0)
# }
# 
# for(i in 1:nrow(fc_wind_ens)){
#   fc_wind_ens$horizon[i] <- paste(fc_wind_ens$horizon[i], "hour")
#   fc_wind_emos$horizon[i] <- paste(fc_wind_emos$horizon[i], "hour")
# }
# 
# fc_wind_emos <- fc_wind_emos %>%
#   mutate(forecast_date = as.character(forecast_date))
wind_data
fc_wind <- read_csv(paste0("otherFiles/", today, "_wind_pbnn.csv"))
fc_wind
wind_plot <- ggplot() +
  #geom_line(data = wind_data, aes(x=obs_tm, y=obs)) +
  geom_line(data = fc_wind,
            aes(x=max(wind_data$obs_tm)+hours(substr(horizon,1,2)),
                y=q0.5)) +
  geom_ribbon(data = fc_wind,
              aes(x=max(wind_data$obs_tm)+hours(substr(horizon,1,2)),
                  ymin=q0.25, ymax=q0.75), alpha=.3) +
  geom_ribbon(data = fc_wind,
              aes(x=max(wind_data$obs_tm)+hours(substr(horizon,1,2)),
                  ymin=q0.025, ymax=q0.975), alpha=.15) +
  # geom_line(data = fc_wind_emos,
  #           aes(x=max(wind_data$obs_tm)+hours(substr(horizon,1,2)),
  #               y=q0.5)) +
  # geom_ribbon(data = fc_wind_emos,
  #             aes(x=max(wind_data$obs_tm)+hours(substr(horizon,1,2)),
  #                 ymin=q0.25, ymax=q0.75), alpha=.3) +
  # geom_ribbon(data = fc_wind_emos,
  #             aes(x=max(wind_data$obs_tm)+hours(substr(horizon,1,2)),
  #                 ymin=q0.025, ymax=q0.975), alpha=.15) +
  scale_x_datetime() +
  coord_cartesian(xlim = c(now() - weeks(6), now() + hours(100))) +
  labs(x = NULL, y = "windspeed")
print(wind_plot)

fc_wind <- fc_wind %>%
  mutate(forecast_date = as.character(forecast_date))


######################
# aggregate and save #
######################
pred_dax_NA <- data.frame(forecast_date = forecast_date,
                          target = "DAX", horizon = paste(c(1, 2, 5:7), "day"),
                          q0.025 = NA, q0.25 = NA, q0.5 = NA, q0.75 = NA, q0.975 = NA)

pred_all <- rbind(pred_dax_NA, pred_energy_sarima, fc_wind)
pred_all
write_csv(pred_all, file = paste0("submissions/", today, "_PhoebeBuffay.csv"))

