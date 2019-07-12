library(caret)
source('src/02-toolkit.R')


#Configurando diretórios
cur_dir = rstudioapi::getActiveProject()
input_dir = paste(cur_dir, 'input', sep = '/')

#Carregando arquivos
files = list.files(input_dir, '.csv')

#system('wc -l input/train.csv')
#Output: 184903891 input/train.csv
#O arquivo de treino possui 184 milhões de observações. Será impossível trabalhar com este volume de dados no meu ambiente.
#Minha idéia é coletar indices aleatórios e extrair pedaços a partir de cada índice.
#Depois agrupar tudo em um único tibble e desenvolver o modelo preditivo a partir daí.
files

#### Pré processamento ####
tbl_train <- read_csv(paste(input_dir, files[1], sep = '/'))
train_sample <- merge_inputs_by_row(tbl_train %>% filter(is_attributed == 1),
                                    tbl_train %>% filter(is_attributed == 0) %>% sample_n(227))
train_sample <- train_sample %>% 
  select(-ip, -attributed_time) %>%
  strip_date()

#### Feature Engineering ####
tbl_dl_app <- mean_app_downloads(train_sample)
tbl_dl_device <- mean_device_downloads(train_sample)
tbl_dl_os <- mean_os_downloads(train_sample)
tbl_dl_channel <- mean_channel_downloads(train_sample)

tbl_click_app <- mean_app_clicks(train_sample)
tbl_click_device <- mean_device_clicks(train_sample)
tbl_click_os <- mean_os_clicks(train_sample)
tbl_click_channel <- mean_channel_clicks(train_sample)

train_sample <- train_sample %>%
  left_join(tbl_dl_app, by = 'app') %>%
  left_join(tbl_dl_device, by = 'device') %>%
  left_join(tbl_dl_os, by = 'os') %>%
  left_join(tbl_dl_channel, by = 'channel') %>%
  left_join(tbl_click_app, by = 'app') %>%
  left_join(tbl_click_device, by = 'device') %>%
  left_join(tbl_click_os, by = 'os') %>%
  left_join(tbl_click_channel, by = 'channel') %>%
  select(-app, -device, -os, -channel)

train_sample <- train_sample %>%
  mutate(is_attributed = as.factor(is_attributed))

#### Modelo ####
formula_v1_all = formula(is_attributed ~ .)
formula_v2_mean_dl = formula(is_attributed ~ mean_app_downloads + mean_channel_downloads + mean_os_downloads + mean_device_downloads)
formula_v3_time_mean_dl = formula(is_attributed ~ click_dayofweek + click_hour + mean_app_downloads + mean_channel_downloads + mean_os_downloads + mean_device_downloads)
formula_v4_mean_click = formula(is_attributed ~ click_mean_app_clicks + mean_channel_clicks + mean_os_clicks + mean_device_clicks)
formula_v5_time_mean_click = formula(is_attributed ~ click_dayofweek + click_hour + mean_app_clicks + mean_channel_clicks + mean_os_clicks + mean_device_clicks)

set.seed(5)
model_v1 = train(formula_v1_all, data = train_sample, method = 'bayesglm')
predict.train(model_v1, train_sample)
