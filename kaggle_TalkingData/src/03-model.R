library(caret)
source('src/02-toolkit.R')

#Configurando diretórios
cur_dir = rstudioapi::getActiveProject()
input_dir = paste(cur_dir, 'input', sep = '/')

#Carregando arquivos
files = list.files(input_dir, '.csv')
system('wc -l input/train.csv')
#Output: 184903891 input/train.csv
#O arquivo de treino possui 184 milhões de observações. Será impossível trabalhar com este volume de dados no meu ambiente.
#Optei por 'marcar' o arquivo completo em 9 partições, de 10% a 90% do total de linhas
start_row_index <- 184903891 * seq(0.1, 0.9, 0.1)

#### Pré processamento ####
## Carga dos dados
#A partir de cada index, serão lidos 2.000.000 registros
data_list <- lapply(start_row_index, function(x){
  read_csv(paste(input_dir, files[5], sep = '/'),
           n_max = 2000000, skip = x, col_names = c('ip', 'app', 'device', 'os',
                                                    'channel', 'click_time', 'attributed_time',
                                                    'is_attributed'))
})

#E então, juntamos os dados extraídos em um único tibble
tbl_train <- merge_inputs_by_row(data_list)
#Verificando a proporção de downloads deste conjunto
table(tbl_train$is_attributed)
#Salvando esse conjunto de dados para uso posterior
data.table::fwrite(tbl_train, 'output/tbl_train.csv')

#Semelhante ao que fizemos durante a análise, faremos "undersampling" deste conjunto,
#de forma a manter a proporção de downloads
train_sample <- tbl_train %>%
  filter(is_attributed == 1)

train_sample <- bind_rows(train_sample, 
                          tbl_train %>% 
                            filter(is_attributed == 0) %>%
                            sample_n(nrow(train_sample)))

## Feature Engineering
#Retirando as variáveis não utilizadas IP e attributed_time
#e Extraindo valores individuais tempo de ocorrência do clique
train_sample <- train_sample %>% 
  select(-ip, -attributed_time) %>%
  strip_date()

#Elaborando features com base em downloads
tbl_dl_app <- mean_app_downloads(train_sample)
tbl_dl_device <- mean_device_downloads(train_sample)
tbl_dl_os <- mean_os_downloads(train_sample)
tbl_dl_channel <- mean_channel_downloads(train_sample)

#Elaborando features com base em cliques
tbl_click_app <- mean_app_clicks(train_sample)
tbl_click_device <- mean_device_clicks(train_sample)
tbl_click_os <- mean_os_clicks(train_sample)
tbl_click_channel <- mean_channel_clicks(train_sample)

#Realizado os cálculos, colocamos a variável target como fator
train_sample <- train_sample %>%
  mutate(is_attributed = as.factor(is_attributed))

#Agrupando os dados extraídos
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

#Salvando esse conjunto de dados para uso posterior
data.table::fwrite(train_sample, 'output/train_sample.csv')

#### Construção do modelo preditivo ####
set.seed(5)

#Treino/teste
t_index = createDataPartition(train_sample$is_attributed, times = 1, p = 0.7, list = F)
train = train_sample[t_index,]
test = train_sample[-t_index,]

#Utilizando a função step para determinar a melhor fórmula
global_formula = formula(is_attributed ~ .)
glm_model = glm(global_formula, data = train, family = 'binomial')
step_model = step(glm_model, direction = 'both')
summary(step_model)

#A melhor fórmula utilizou todas as métricas de média em conjunto com a hora em que ocorreu o clique
step_formula = formula(is_attributed ~ click_hour + mean_app_downloads + 
                         mean_device_downloads + mean_os_downloads + mean_channel_downloads + 
                         mean_app_clicks + mean_device_clicks + mean_os_clicks + mean_channel_clicks)
tr_c = trainControl(method = 'repeatedcv', number = 10, repeats = 10)
step_model = train(step_formula, data = train, method = 'glm', family = 'binomial', trControl = tr_c)

#Guardando o modelo treinado
saveRDS(step_model, 'output/step_model.rds')

#Plot de importância das variáveis
plot(varImp(step_model, scale = F))

#Previsões
step_preds = predict.train(step_model, test)

#Análise dos resultados - Confusion matrix
confusionMatrix(step_preds, test$is_attributed, positive = '1')
#Em um dataset equilibrado, o modelo obteve 91% de accuracy. Entretanto, precisamos separar um 
#dataset desbalanceado para verificar se esse modelo é generalizável.

#Separando o dataset
tbl_unbalanced_test = tbl_train %>% sample_n(1000000)

#Atribuindo features baseadas no dataset de treino
tbl_unbalanced_test <- tbl_unbalanced_test %>% 
  select(-ip, -attributed_time) %>%
  mutate(is_attributed = as.factor(is_attributed)) %>%
  strip_date() %>%
  left_join(tbl_dl_app, by = 'app') %>%
  left_join(tbl_dl_device, by = 'device') %>%
  left_join(tbl_dl_os, by = 'os') %>%
  left_join(tbl_dl_channel, by = 'channel') %>%
  left_join(tbl_click_app, by = 'app') %>%
  left_join(tbl_click_device, by = 'device') %>%
  left_join(tbl_click_os, by = 'os') %>%
  left_join(tbl_click_channel, by = 'channel') %>%
  select(-app, -device, -os, -channel)

#É possível que sejam descobertos novos apps, devices, os e channels não listados durante o treino.
#Nessas situações, o left_join deixará um valor NA
sapply(tbl_unbalanced_test, function(x) sum(is.na(x)))

#Na presença de valores NA, será imputado um valor constante zero
tbl_unbalanced_test <- tbl_unbalanced_test %>%
  replace(is.na(.), 0)

#Realizando novas previsões
unbalanced_preds = predict.train(step_model, tbl_unbalanced_test)

#Análise dos resultados - Confusion Matrix
confusionMatrix(unbalanced_preds, tbl_unbalanced_test$is_attributed, positive = '1')
#96% de accuracy com uma amostra desbalanceada e dados aleatórios do dataset original, sugerindo
#que este modelo foi capaz de detectar características que determinam a ocorrência de um download
#Ainda há bastante margem para melhorar este modelo e reduzir a quantidade de falsos positivos previstos.

#### Submetendo modelo preditivo ao Kaggle ####
# Essa competição utiliza a métrica ROC para avaliar a eficácia do modelo preditivo
tbl_submission <- read_csv(paste(input_dir, files[3], sep = '/'))

tbl_submission <- tbl_submission %>%
  select(-ip) %>%
  strip_date()

tbl_submission <- tbl_submission %>%
  left_join(tbl_dl_app, by = 'app') %>%
  left_join(tbl_dl_device, by = 'device') %>%
  left_join(tbl_dl_os, by = 'os') %>%
  left_join(tbl_dl_channel, by = 'channel') %>%
  left_join(tbl_click_app, by = 'app') %>%
  left_join(tbl_click_device, by = 'device') %>%
  left_join(tbl_click_os, by = 'os') %>%
  left_join(tbl_click_channel, by = 'channel') %>%
  select(-app, -device, -os, -channel)

tbl_submission <- tbl_submission %>%
  replace(is.na(.), 0)

submission_preds = predict.train(step_model, tbl_submission)

tbl_submission <- tbl_submission %>%
  select(click_id)

tbl_submission <- bind_cols(tbl_submission, as.data.frame(submission_preds)) %>%
  rename(is_attributed = submission_preds)
data.table::fwrite(tbl_submission, 'output/submission.csv', quote = F)
#Score: 0.90377