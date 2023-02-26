rm(list=ls())

library(zoo)
library(dplyr)
library(tidyr)
library(tibble)
library(lubridate)
library(readr)
library(data.table)
library(rjson)
library(forecast)

getwd()

options("scipen"=100, "digits"=4, dplyr.summarise.inform = FALSE)

# all submission dates
forecast_dates <- as.character(c(seq(as_date("2022-10-26"), as_date("2022-12-21"), by="week"),
                                 seq(as_date("2023-01-11"), as_date("2023-02-15"), by="week")))
# forecast horizons and quantiles
horizons <- c(36, 40, 44, 60, 64, 68)
quantile_levels <- c(0.025,0.25,0.5,0.75,0.975)

# function for model evaluation
quantile_score <- function(y_true, q_forecast, tau){
  indicator <- ifelse(y_true < q_forecast, 1, 0)
  score <- 2 * (y_true - q_forecast) * (tau - indicator)
  return(score)
}

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

energy_data_raw <- copy(energy_data)


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

# add day_type variable to the training data depending on weekend and holiday dummy
energy_data$day_type <- 0
for (row in 1:dim(energy_data)[1]){
  energy_data[row, "day_type"] <- avg_data[(hour(energy_data[row,]$floor_date)+1),
                                           paste0("avg_",
                                                  energy_data[row,]$is_weekend,
                                                  energy_data[row,]$is_holiday)]
}


### temperature regressor ###

# get all Wednesdays for which ensemble forecast is available
d <- as_date("2021-09-08")  # 1st Wednesday data set
wednesdays <- gsub("-", "", seq(d, max(as_date(forecast_dates)), by="week"))

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
  dplyr::filter(floor_date < max(as_date(forecast_dates)) + hours(120)) %>%
  mutate(hour=hour(floor_date)) %>%
  group_by(hour) %>%
  mutate(temp_mean = na.approx(temp_mean, na.rm = F)) %>% # linearly interpolate missing between previous and next available value from same hour
  ungroup() %>%
  dplyr::select(!hour) %>%
  mutate(temp_mean = na.approx(temp_mean, na.rm = F)) # linearly interpolate missing at the end of time series where no next value is available

# combine model input and filter for hours of interest
model_input_all <- energy_data[,c("floor_date", "gesamt", "day_type")] %>%
  left_join(temperature_long, by="floor_date") %>%
  dplyr::filter(hour(floor_date) %in% c(11,15,19))
x_reg_names <- c("day_type", "temp_mean")

# read results of grid search and choose model which minimizes AIC
grid <- read_csv("otherFiles/energy_order_grid_AIC_BIC.csv")
par_minAIC <- grid[which.min(grid$AIC),]


### models for each forecast_date ###

scores <- data.frame(forecast_date=forecast_dates)
for (forecast_date in forecast_dates){
  print(forecast_date)
  model_input <- model_input_all %>%
    dplyr::filter(floor_date <= as_datetime(forecast_date))
  
  
  ### SARIMAX model ###
  
  # fit model
  energy_model_sarimax_minAIC <- model_input$gesamt %>%
    Arima(order=c(par_minAIC$p,par_minAIC$d,par_minAIC$q),
          seasonal=list(order=c(par_minAIC$P,0,par_minAIC$Q),
                        period=par_minAIC$m),
          xreg = as.matrix(model_input %>% select(all_of(x_reg_names))))
  
  # construct regressors for forecast period
  date_time <- seq(as_datetime(forecast_date), length.out=100, by="hour")
  is_weekend_pred <- ifelse(weekdays(date_time) %in% c("Samstag", "Sonntag"), 1, 0)
  is_holiday_pred <- ifelse(as_date(date_time) %in% holidays$date, 1, 0)
  is_holiday_weekend_pred <- is_weekend_pred*is_holiday_pred
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

  
  ### baseline model ###
  
  # determine datetimes for which forecasts are wanted
  forecast_datetimes <- as_date(forecast_date) + days(1) + hours(horizons - 1) # correct for UTC
  
  # compute forecast
  pred_energy_baseline <- energy_data %>%
    dplyr::select(floor_date, gesamt) %>%
    dplyr::filter(floor_date <= as_datetime(forecast_date)) %>%
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

  pred_baseline <- data.frame(forecast_datetimes, pred_energy_baseline %>% select(starts_with("q")))
  
  
  ### model evaluation ###
  
  # get true energy demand
  energy_data_future <- energy_data_raw %>%
    dplyr::filter(floor_date > as_datetime(forecast_date))
  
  qs_sum_sarimax <- qs_sum_baseline <- 0
  for(fc_dt in forecast_datetimes){
    for (tau in quantile_levels){
      y_true <- energy_data_future[energy_data_future$floor_date == fc_dt,]$gesamt
      q_fc_sarimax <- pred_sarima[pred_sarima$date_time == fc_dt, paste0("q", tau)]
      q_fc_baseline <- pred_baseline[pred_baseline$forecast_datetimes == fc_dt, paste0("q", tau)]
      qs_sum_sarimax <- qs_sum_sarimax + quantile_score(y_true, q_fc_sarimax, tau)
      qs_sum_baseline <- qs_sum_baseline + quantile_score(y_true, q_fc_baseline, tau)
    } 
  }
  scores[scores$forecast_date==forecast_date, "sarimax"] <- qs_sum_sarimax/length(forecast_datetimes)
  scores[scores$forecast_date==forecast_date, "baseline"] <- qs_sum_baseline/length(forecast_datetimes)
}
scores
mean(scores$sarimax)
mean(scores$baseline)

