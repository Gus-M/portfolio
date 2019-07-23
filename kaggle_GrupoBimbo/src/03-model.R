source('src/02-toolkit.R')
library(caret)
library(MLmetrics)

#Configurando diretórios
cur_dir = rstudioapi::getActiveProject()
input_dir = paste(cur_dir, 'input', sep = '/')

#Selecionando arquivos
files <- list.files(input_dir, '.csv')

#Produtos
tbl_products <- read_csv(paste(input_dir, files[2], sep = '/'))

#Cidades
tbl_towns <- read_csv(paste(input_dir, files[5], sep = '/'))

#Dados de treino
tbl_train <- fread(paste(input_dir, files[6], sep = '/'), select = c('Semana', 'Agencia_ID', 'Canal_ID', 'Cliente_ID', 'Producto_ID', 'Demanda_uni_equil'))

#Colocando a demanda em escala logarítmica
tbl_train[, Demanda_uni_equil := log1p(Demanda_uni_equil)]

#Serão selecionados 500.000 registros das semanas 5, 6, 7 e 8
set.seed(11)
train_sample <- bind_rows(lapply(5:8, function(x){
  tbl_train[Semana == x][sample(.N, 500000)]
})) %>% as_tibble
rm(tbl_train)

#Calculamos as features
features = calc_train_features(train_sample, tbl_products, tbl_towns)

#Agrupando features
for(f in features){
  train_sample <- train_sample %>% left_join(f)
}

#Retirando variáveis não utilizadas
train_sample <- train_sample %>%
  select(-Semana, -Agencia_ID, -Canal_ID, -Producto_ID, -Cliente_ID, -Town_ID, -State_ID)

#Será utilizado um modelo de regressão linear múltipla. O modelo será treinado com dados
#entre 1.5 desvios padrões da média
mean_threshold <- mean(train_sample$Demanda_uni_equil)
sd_threshold <- sd(train_sample$Demanda_uni_equil)

train_sample <- train_sample %>%
  filter(Demanda_uni_equil >= mean_threshold - (1.5 * sd_threshold) & 
         Demanda_uni_equil <= mean_threshold + (1.5 * sd_threshold))

set.seed(17)
t_index = createDataPartition(train_sample$Demanda_uni_equil, times = 1, p = 0.7, list = F)
train = train_sample[t_index,]
test = train_sample[-t_index,]

model_lm <- lm(Demanda_uni_equil ~ ., data = train)
summary(model_lm)

#Como a demanda sempre será um valor maior ou igual a zero, valores negativos serão substituídos por zero
preds <- predict.lm(model_lm, test)
preds <- replace(preds, preds < 0, 0)

#Como os valores estão em escala logarítmica, testamos a eficâcia do modelo com a função RMSLE
RMSLE(preds, test$Demanda_uni_equil)

#Treinando um modelo stochastic gradient boosting
gbm_tuneGrid = expand.grid(n.trees = c(10, 20, 30),
                           interaction.depth = 1,
                           shrinkage = c(0.1, 1),
                           n.minobsinnode = 100)
gbm_trControl = trainControl(number = 5, search = 'grid', verboseIter = F)
model_gbm = train(Demanda_uni_equil ~ .,
                  data = train,
                  method = 'gbm',
                  tuneGrid = gbm_tuneGrid,
                  trControl = gbm_trControl)
summary(model_gbm)

#Previsões
preds_gbm <- predict.train(model_gbm, test)
preds_gbm <- replace(preds_gbm, preds_gbm < 0, 0)

#Score
RMSLE(preds_gbm, test$Demanda_uni_equil)

#Submetendo modelo preditivo ao kaggle
kaggle_test_data = fread(paste(input_dir, files[4], sep = '/'))
kaggle_test_data <- merge(kaggle_test_data, features[[1]], all.x = T, by = 'Canal_ID')
kaggle_test_data <- merge(kaggle_test_data, features[[2]], all.x = T, by = 'Agencia_ID')
kaggle_test_data <- merge(kaggle_test_data, features[[3]], all.x = T, by = 'Town_ID')
kaggle_test_data <- merge(kaggle_test_data, features[[4]], all.x = T, by = 'Cliente_ID')
kaggle_test_data <- merge(kaggle_test_data, features[[5]], all.x = T, by = 'Producto_ID')

#Verificando valores NA
sapply(kaggle_test_data, function(x) sum(is.na(x)))

#Nas features relacionadas a médias já calculadas, será imputada a mediana.
#Nas demais features binárias, será imputado um valor constante zero
kaggle_test_data[is.na(mean_depot_demanda)]$mean_depot_demanda <- median(kaggle_test_data$mean_depot_demanda, na.rm = T)
kaggle_test_data[is.na(mean_town_demanda)]$mean_town_demanda <- median(kaggle_test_data$mean_town_demanda, na.rm = T)
kaggle_test_data[is.na(mean_demanda_cliente)]$mean_demanda_cliente <- median(kaggle_test_data$mean_demanda_cliente, na.rm = T)
kaggle_test_data[is.na(mean_producto_demanda)]$mean_producto_demanda <- median(kaggle_test_data$mean_producto_demanda, na.rm = T)
kaggle_test_data[is.na(large_scale_depot)]$large_scale_depot <- 0
kaggle_test_data[is.na(heavy_product)]$heavy_product <- 0
kaggle_test_data[is.na(bundled_product)]$bundled_product <- 0

submission_preds_lm <- predict.lm(model_lm, kaggle_test_data)
submission_preds_lm <- replace(submission_preds_lm, submission_preds_lm < 0, 0)

submission_preds_gbm <- predict.train(model_gbm, kaggle_test_data)
submission_preds_gbm <- replace(submission_preds_gbm, submission_preds_gbm < 0, 0)

#Colocando as previsões na escala original
submission_preds_lm <- expm1(submission_preds_lm)
submission_preds_gbm <- expm1(submission_preds_gbm)

#Carregando sample submission e submetendo resultados
submission = fread(paste(input_dir, files[3], sep = '/'))

submission[, Demanda_uni_equil := submission_preds_lm]
fwrite(submission, 'output/sample_submission_lm.csv')
#Score público: 0.94999
#Score privado: 0.95924

submission[, Demanda_uni_equil := submission_preds_gbm]
fwrite(submission, 'output/sample_submission_gbm.csv')
#Score público: 0.91934
#Score privado: 0.92901
