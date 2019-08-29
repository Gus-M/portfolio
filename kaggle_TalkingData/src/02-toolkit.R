require(data.table)
require(lubridate)

## Granularizar o campo click_date
strip_date = function(tbl){
  date_info <- tbl[, .(click_day = day(click_time),
                       click_dayofweek = as.POSIXlt(click_time)$wday,
                       click_hour = hour(click_time),
                       click_minue = minute(click_time),
                       click_second = second(click_time))]
  return(cbind(tbl, date_info))
}

## Calcular as médias por IDs
id_means <- function(tbl){
  total_clicks <- nrow(tbl)
  
  #Médias por apps
  app_downloads <- tbl[, .(mean_app_downloads = mean(is_attributed)), by = app]
  app_clicks <- tbl[, .(mean_app_clicks = .N / total_clicks), by = app]
  
  #Médias por dispositivos
  device_downloads <- tbl[, .(mean_device_downloads = mean(is_attributed)), by = device]
  device_clicks <- tbl[, .(mean_device_clicks = .N / total_clicks), by = device]
  
  #Médias por sistema operacionals
  os_downloads <- tbl[, .(mean_os_downloads = mean(is_attributed)), by = os]
  os_clicks <- tbl[, .(mean_os_clicks = .N / total_clicks), by = os]
  
  #Médias por canal
  channel_downloads <- tbl[, .(mean_channel_downloads = mean(is_attributed)), by = channel]
  channel_clicks <- tbl[, .(mean_channel_clicks = .N / total_clicks), by = channel]
  
  return(list(app_downloads, app_clicks, device_downloads, device_clicks,
              os_downloads, os_clicks, channel_downloads, channel_clicks))
}