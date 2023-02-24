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
library(forecast)
library(crch)
library(lmtest)

getwd()

options("scipen"=100, "digits"=4)

forecast_date <- as.character(today())
quantile_levels <- c(0.025,0.25,0.5,0.75,0.975)


################
# Power demand #
################
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

# add recent data
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

energy_data <- setDT(energy_data)[,lapply(.SD, sum), by=floor_date(date_time,"hour")] %>%
  na.omit() %>%
  mutate(gesamt = gesamt/1000)

horizons <- c(36, 40, 44, 60, 64, 68)
get_date_from_horizon <- function(last_ts, horizon){
  return(last_ts + hours(horizon))
}

par(mfrow=c(2,1))
energy_data$gesamt %>% acf(lag=24*7+1, main="acf raw")
energy_data$gesamt %>% pacf(lag=24*7+1, main="pacf raw")
energy_data$gesamt %>% diff(lag=24) %>% acf(lag=24*7+1, main="acf diff 24")
energy_data$gesamt %>% diff(lag=24) %>% pacf(lag=24*7+1, main="pacf diff 24")
par(mfrow=c(1,1))

holidays <- read.csv("https://www.spiketime.de/feiertagapi/feiertage/csv/2014/2023", sep=";") %>%
  transmute(date = as_date(Datum)) %>%
  unique()

energy_data <- energy_data %>%
  mutate(is_weekend = ifelse(weekdays(floor_date) %in% c("Samstag", "Sonntag"), 1, 0),
         is_holiday = ifelse(as_date(floor_date) %in% holidays$date, 1, 0),
         is_holiday_weekend = is_weekend*is_holiday)

# non-holiday weekday
mean(energy_data[energy_data$is_weekend+energy_data$is_holiday==0]$gesamt)
# holiday weekday
mean(energy_data[(energy_data$is_weekend==0)&(energy_data$is_holiday==1)]$gesamt)
# non-holiday weekend
mean(energy_data[(energy_data$is_weekend==1)&(energy_data$is_holiday==0)]$gesamt)
# holiday weekend
mean(energy_data[energy_data$is_holiday_weekend==1]$gesamt)

energy_data_historic <- energy_data %>%
  filter(floor_date<as_date("2021-09-08")) # used to create alternative regressor to weekend/holiday dummies
energy_data <- energy_data %>%
  filter(floor_date>=as_date("2021-09-08"))

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
plot(avg_00, type="l", ylim=c(39,72))
lines(avg_01, col="blue")
lines(avg_10, col="darkgreen")
lines(avg_11, col="orange")
legend("topright", c("normal wd", "holiday wd", "normal we", "holiday we"),
       lty=1, col=c("black", "blue", "darkgreen", "orange"))
avg_data <- cbind(avg_00, avg_01, avg_10, avg_11)

energy_data$day_type <- 0
for (row in 1:dim(energy_data)[1]){
  energy_data[row, "day_type"] <- avg_data[(hour(energy_data[row,]$floor_date)+1),
                                           paste0("avg_",
                                                  energy_data[row,]$is_weekend,
                                                  energy_data[row,]$is_holiday)]
}

get_hour_of_year <- function(time) {
  (as.integer(format(time, '%j')) - 1) * 24 + as.integer(format(time, '%H'))
}
season_sin <- sin(2*pi/(365*24) * get_hour_of_year(energy_data$floor_date))
season_cos <- cos(2*pi/(365*24) * get_hour_of_year(energy_data$floor_date))

d <- as_date("2021-09-08")  # 1st wednesday data set
wednesdays <- gsub("-", "", seq(d, today(), by="week"))

temperature <- data.frame(fcst_hour=0:120)
for (w in wednesdays){
  try({file <- paste0("../kit-weather-ensemble-point-forecast-karlsruhe/icon-eu-eps_", w, "00_t_2m_Karlsruhe.txt")
  temp_w <- read_delim(file, delim = "|", skip=3) %>%
    dplyr::select(!c(`...1`, `...43`)) %>%
    dplyr::rename_all(list(~gsub(" ", "", .))) %>%
    mutate_all(as.numeric) %>%
    transmute(fcst_hour = fcst_hour,
              !!w := rowMeans(dplyr::select(., 1:40), na.rm=T))
  temperature <- temperature %>%
    dplyr::full_join(temp_w, by = "fcst_hour")})
}
View(temperature)

temperature_long <- temperature %>%
  pivot_longer(cols = starts_with("202"), names_to = "date", values_to = "temp_mean") %>%
  mutate(date = as_datetime(date, format = "%Y%m%d", tz="CET")) %>%
  mutate(floor_date = date + hours(fcst_hour)) %>%
  dplyr::select(floor_date, temp_mean) %>%
  mutate(floor_date = with_tz(floor_date, "UTC")) %>%
  full_join(energy_data %>% dplyr::select(floor_date), by="floor_date") %>%
  arrange(floor_date) %>%
  dplyr::filter(!is.na(floor_date)) %>%
  mutate(temp_mean = na.approx(temp_mean))
View(temperature_long)

# temperature_historic <- read_delim("data/energy/temperature/produkt_tu_stunde_20210814_20230214_04177.txt") %>%
#   transmute(floor_date = as_datetime(as.character(MESS_DATUM), format="%Y%m%d%H", tz="CET"),
#             temp_hist=as.numeric(TT_TU)) %>%
#   mutate(floor_date = with_tz(floor_date, "UTC"))

x_reg <- as.data.frame(cbind(energy_data[,c("floor_date", "day_type")])) %>%#,
                             #energy_data[,c("is_weekend", "is_holiday", "is_holiday_weekend")],
                             #season_sin)) #, season_cos)) 
  full_join(temperature_long, by="floor_date") #%>%
  # full_join(temperature_historic, by="floor_date") %>%
  # mutate(temp_mean = coalesce(temp_mean, temp_hist)) %>% # where ever no forecast is available use true value instead
  # dplyr::select(!temp_hist)

x_reg_names <- c("day_type", "temp_mean")

model_input <- energy_data %>%
  full_join(x_reg, by=c("floor_date", "day_type")) %>%
  dplyr::filter(hour(floor_date) %in% c(11,15,19))


# find best order
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
grid <- read_csv("otherFiles/energy_order_grid_AIC_BIC.csv")

par_minAIC <- grid[which.min(grid$AIC),]
par_minBIC <- grid[which.min(grid$BIC),]
print(par_minAIC)

energy_model_sarimax_minAIC <- model_input$gesamt %>%
  Arima(order=c(par_minAIC$p,par_minAIC$d,par_minAIC$q),
        seasonal=list(order=c(par_minAIC$P,0,par_minAIC$Q),
                      period=par_minAIC$m),
        xreg = as.matrix(model_input %>% select(all_of(x_reg_names))))
summary(energy_model_sarimax_minAIC)
coeftest(energy_model_sarimax_minAIC)
resid_sarimax <- energy_model_sarimax_minAIC %>% residuals()

par(mfrow=c(2,1))
resid_sarimax %>% na.omit() %>% acf(lag=3*7+1, main="acf residuals sarima")
resid_sarimax %>% na.omit() %>% pacf(lag=3*7+1, main="pacf residuals sarima")
par(mfrow=c(1,1))

plot(model_input$floor_date,
     energy_model_sarimax_minAIC$x, col='red', type="l",
     xlim=c(energy_data$floor_date[dim(model_input)[1]-1000],energy_data$floor_date[dim(model_input)[1]]))
lines(model_input$floor_date,
      fitted(energy_model_sarimax_minAIC), col='blue')

date_time <- seq(max(energy_data$floor_date)+hours(1), length.out=100, by="hour")
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
                   #is_weekend_pred, is_holiday_pred, is_holiday_weekend_pred,
                   #season_sin_pred, season_cos_pred)
model_input_future <- new_x_reg %>%
  # rename(day_type = day_type_pred, season_sin = season_sin_pred) %>%
  dplyr::filter(hour(floor_date) %in% c(11,15,19))
pred <- predict(energy_model_sarimax_minAIC, n.ahead = 13,
                newxreg = model_input_future %>% select(all_of(x_reg_names)))
pred_sarima <- data.frame(model_input_future$floor_date, pred$pred, pred$se) %>%
  rename(date_time = model_input_future.floor_date, q0.5 = pred.pred, se = pred.se) %>%
  mutate(q0.025 = q0.5 + se * qnorm(0.025),
         q0.25 = q0.5 + se * qnorm(0.25),
         q0.75 = q0.5 + se * qnorm(0.75),
         q0.975 = q0.5 + se * qnorm(0.975))

energy_plot <- ggplot() +
  geom_line(data = model_input, aes(x=floor_date, y=gesamt)) +
  scale_x_datetime() +
  coord_cartesian(xlim = c(now() - weeks(6), now() + hours(100))) +
  labs(x = NULL, y = "Gesamtverbrauch [GWh]")
energy_plot <- energy_plot +
  geom_line(data=pred_sarima, aes(x=date_time, y=q0.5), color="red") +
  geom_ribbon(data = pred_sarima,
              aes(x=date_time, ymin=q0.25, ymax=q0.75), alpha=.3, fill="red") +
  geom_ribbon(data = pred_sarima,
              aes(x=date_time, ymin=q0.025, ymax=q0.975), alpha=.15, fill="red")
print(energy_plot)

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

