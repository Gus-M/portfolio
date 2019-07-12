library(tidyverse)
library(lubridate)
library(corrplot)

#Configurando diretórios
cur_dir = rstudioapi::getActiveProject()
input_dir = paste(cur_dir, 'input', sep = '/')
output_dir = paste(cur_dir, 'output', sep = '/')

#Carregando arquivos
files = list.files(input_dir, '.csv')

#Carregando datasets
#Inicialmente, será utilizado o arquivo "train_sample.csv". Posteriormente, será feita uma nova análise em amostras
#aleatórias do dataset "train_csv"
tbl_train = read_csv(paste(input_dir, files[4], sep = '/'))
glimpse(tbl_train)

##Análise estatística dos dados
summary(tbl_train)
table(tbl_train$is_attributed)
# * Neste conjunto, ocorreram apenas 227 registros de download.
# * Essas amostras foram coletadas entre os dias 06/11/2017 (seg) e 09/11/2017 (qui)
# * As variáveis app, device, os e channel representam IDs e serão utilizadas com índices de agregação

#Faremos undersampling nesse dataset, de forma que tenhamos 50/50 amostras de is_attributed
set.seed(1)
tbl_is_attributed_1 <- tbl_train %>%
  filter(is_attributed == 1)

sample_is_attributed_0 <- tbl_train %>%
  filter(is_attributed == 0) %>%
  sample_n(227)

tbl_undersample <- bind_rows(tbl_is_attributed_1, sample_is_attributed_0)

#Extraindo cada valor da data do clique
strip_date = function(click_date){
  return(tibble(click_day = day(click_date),
                click_dayofweek = as.POSIXlt(click_date)$wday,
                click_hour = hour(click_date),
                click_minute = minute(click_date),
                click_second = second(click_date)))
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

#Elaborando novas variáveis: Média de downloads por IDs, Média de clicks por IDs
tbl_undersample <- bind_cols(tbl_undersample, strip_date(tbl_undersample$click_time)) %>%
  left_join(mean_app_downloads(tbl_undersample), by = 'app') %>%
  left_join(mean_device_downloads(tbl_undersample), by = 'device') %>%
  left_join(mean_os_downloads(tbl_undersample), by = 'os') %>%
  left_join(mean_channel_downloads(tbl_undersample), by = 'channel') %>%
  left_join(mean_app_clicks(tbl_undersample), by = 'app') %>%
  left_join(mean_device_clicks(tbl_undersample), by = 'device') %>%
  left_join(mean_os_clicks(tbl_undersample), by = 'os') %>%
  left_join(mean_channel_clicks(tbl_undersample), by = 'channel')

# Correlação entre variáveis
tbl_undersample_corr <- tbl_undersample %>%
  select(-ip, -app, -device, -os, -channel) %>%
  select_if(is.numeric) %>%
  cor()
corrplot(tbl_undersample_corr, method = ('square'))

# * As variáveis de tempo possuem pouca ou nenhuma influência sobre a probabilidade de um usuário realizar um download
# * Em relação aos download, há uma correlação mais forte entre: 
#     + o aplicativo utilizado
#     + o canal de acesso utilizado
# * As variáveis "device" e "os" exercem menos influência na decisão do usuário efetuar download

## Plots
#Downloads por app
ggplot(data = tbl_undersample) +
  geom_histogram(mapping = aes(x = app),
                 bins = 1) +
  scale_x_continuous(position = 'top') +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  xlab('Apps') + ylab('Downloads') +
  facet_grid(is_attributed ~ app)

#Downloads por dispositivo
ggplot(data = tbl_undersample) +
  geom_histogram(mapping = aes(x = device),
                 bins = 1) +
  scale_x_continuous(position = 'top') +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  xlab('Dispositivo') + ylab('Downloads') +
  facet_grid(is_attributed ~ device)

#Downloads por sistema operacional
ggplot(data = tbl_undersample) +
  geom_histogram(mapping = aes(x = os),
                 bins = 1) +
  scale_x_continuous(position = 'top') +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  xlab('Sistema Operacional') + ylab('Downloads') +
  facet_grid(is_attributed ~ os)

#Downloads por canal
ggplot(data = tbl_undersample) +
  geom_histogram(mapping = aes(x = channel),
                 bins = 1) +
  scale_x_continuous(position = 'top') +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  xlab('Canal') + ylab('Downloads') +
  facet_grid(is_attributed ~ channel)

#Downloads por hora do dia
ggplot(data = tbl_undersample) +
  geom_histogram(mapping = aes(x = click_hour),
                 bins = 1) +
  scale_x_continuous(position = 'top') +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  xlab('Horário') + ylab('Downloads') +
  facet_grid(is_attributed ~ click_hour)