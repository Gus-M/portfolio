require(tidyverse)
require(lubridate)

## Agrupar todos inputs em um único tibble
merge_inputs_by_row = function(..., is_attributed = NA){
  tib <- bind_rows(...)
  if(is.na(is_attributed)){
    return(tib)
  }
  return(tib %>% filter(is_attributed == !!is_attributed))
}

## Granularizar o campo click_date
strip_date = function(tib){
  tbl_date <- tib %>% 
    transmute(click_day = day(click_time),
              click_dayofweek = as.POSIXlt(click_time)$wday,
              click_hour = hour(click_time),
              click_minute = minute(click_time),
              click_second = second(click_time))
  return(bind_cols(tib, tbl_date) %>% select(-click_time))
}

#Média de downloads por app
mean_app_downloads = function(tbl){
  return(tbl %>% 
           group_by(app) %>%
           summarize(mean_app_downloads = mean(is_attributed)) %>%
           ungroup)
}

#Média de downloads por dispositivo
mean_device_downloads = function(tbl){
  return(tbl %>% 
           group_by(device) %>%
           summarize(mean_device_downloads = mean(is_attributed)) %>%
           ungroup)
}

#Média de downloads por sistema operacional
mean_os_downloads = function(tbl){
  return(tbl %>% 
           group_by(os) %>%
           summarize(mean_os_downloads = mean(is_attributed)) %>%
           ungroup)
}

#Média de downloads por canal
mean_channel_downloads = function(tbl){
  return(tbl %>% 
           group_by(channel) %>%
           summarize(mean_channel_downloads = mean(is_attributed)) %>%
           ungroup)
}

#Média de cliques por aplicativo
mean_app_clicks = function(tbl){
  total_clicks = nrow(tbl)
  tbl_clicks = tbl %>% count(app)
  return(tbl_clicks %>%
           mutate(n = n / total_clicks) %>%
           rename(mean_app_clicks = n))
}

#Média de cliques por dispositivo
mean_device_clicks = function(tbl){
  total_clicks = nrow(tbl)
  tbl_clicks = tbl %>% count(device)
  return(tbl_clicks %>%
           mutate(n = n / total_clicks) %>%
           rename(mean_device_clicks = n))
}

#Média de cliques por sistema operacional
mean_os_clicks = function(tbl){
  total_clicks = nrow(tbl)
  tbl_clicks = tbl %>% count(os)
  return(tbl_clicks %>%
           mutate(n = n / total_clicks) %>%
           rename(mean_os_clicks = n))
}

#Média de cliques por canal
mean_channel_clicks = function(tbl){
  total_clicks = nrow(tbl)
  tbl_clicks = tbl %>% count(channel)
  return(tbl_clicks %>%
           mutate(n = n / total_clicks) %>%
           rename(mean_channel_clicks = n))
}