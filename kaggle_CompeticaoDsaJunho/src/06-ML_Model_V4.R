library(data.table)
library(xgboost)
library(caret)

#Configurando diretórios
cur_dir = rstudioapi::getActiveProject()
input_dir = paste(cur_dir, 'input/', sep = '/')
output_dir = paste(cur_dir, 'output/', sep = '/')

#Selecionando arquivos
files <- list.files(input_dir, '*.csv')
dt_train <- fread(paste(output_dir, 'dt_train_modified.csv', sep = ''))

# Testando todas as features que extraí semana passada
spm_train <- as.matrix(dt_train[, .(feature_1, feature_2, feature_3,
                                    feature_unauthorized, feature_avg_purchase,
                                    feature_tot_purchase, feature_un_merchants, feature_months_active)])
spm_train_output <- as.matrix(dt_train[, target])
xgb_train <- xgb.DMatrix(data = spm_train, label = spm_train_output)
xgb_model_1_cv <- xgb.cv(data = xgb_train,
                         eval.metric = 'rmse',
                         objective = 'reg:linear',
                         nfold = 10, max.depth = 3, eta = 0.5296292,
                         nthread = 8, subsample = 0.4605527, colsample_bytree = 0.6124231,
                         min_child_weight = 6, nround = 86)
#O RMSE apenas aumentou desde a primeira iteração. Vou colocar as features numéricas na mesma escala e tentar novamente
summary(dt_train)
dt_train[, feature_tot_purchase := scale(feature_tot_purchase)]
dt_train[, feature_un_merchants := scale(feature_un_merchants)]
dt_train[, feature_months_active := scale(feature_months_active)]

#Tentando novamente
spm_train <- as.matrix(dt_train[, .(feature_1, feature_2, feature_3,
                                    feature_unauthorized, feature_avg_purchase,
                                    feature_tot_purchase, feature_un_merchants, feature_months_active)])
spm_train_output <- as.matrix(dt_train[, target])
xgb_train <- xgb.DMatrix(data = spm_train, label = spm_train_output)
xgb_model_1_cv <- xgb.cv(data = xgb_train,
                         eval.metric = 'rmse',
                         objective = 'reg:linear',
                         nfold = 10, max.depth = 3, eta = 0.5296292,
                         nthread = 8, subsample = 0.4605527, colsample_bytree = 0.6124231,
                         min_child_weight = 6, nround = 86)

# A mesma coisa aconteceu com os valores em escala. Sinto que as variáveis extraídas no primeiro
# dataset não contribuirão o suficiente para desenvolver um modelo melhor. Portanto,
# utilizarei apenas as features do último dataset

#### Primeira tentativa da última versão ####
# Vou realizar o mesmo procedimento do melhor modelo, aplicando RandomSearchCV e utilizando as features
# do melhor modelo. Entretanto, vou normalizar a variável target
target_normalize <- function(x, min_value, max_value){
  return((x - min_value)/(max_value - min_value))
}

target_denormalize <- function(x, min_value, max_value){
  return(x * (max_value - min_value) + min_value)
}

# Testando a função
test_norm <- target_normalize(1:10, 1, 10)
test_norm
test_denorm <- target_denormalize(test_norm, 1, 10)
test_denorm

# Funcionando conforme o esperado
dt_train <- fread(paste(output_dir, 'dt_train_modified_2.csv', sep = ''))
dt_train_target_min <- min(dt_train$target)
dt_train_target_max <- max(dt_train$target)
dt_train[, target := target_normalize(target, dt_train_target_min, dt_train_target_max)]
dt_test <- fread(paste(output_dir, 'dt_test_modified_2.csv', sep = ''))

# Com a variável target normalizada, repetimos a fórmula de melhor resultado com RandomSearchCV
spm_train <- as.matrix(dt_train[, .(feature_2, ft_category_1, ft_last_usage_days, ft_potential_fraud, target)])
fit_Control <- trainControl(method = "repeatedcv", number = 10, repeats = 10, search = "random")
xgb_model_1 <- train(target ~ ., 
                     data = spm_train,
                     method = 'xgbTree',
                     trControl = fit_Control)
xgb_model_1
# Melhores hiperparâmetros encontrados:
# nrounds = 850
# max_depth = 8
# eta = 0.5334386
# gamma = 1.58946
# colsample_bytree = 0.6957163
# min_child_weight = 12
# subsample = 0.612388

spm_test <- as.matrix(dt_test[, .(feature_2, ft_category_1, ft_last_usage_days, ft_potential_fraud)])

xgb_model_1_predictions <- predict.train(xgb_model_1, spm_test)
xgb_model_1_submission = data.table(card_id = dt_test$card_id,
                                    target = target_denormalize(xgb_model_1_predictions, dt_train_target_min, dt_train_target_max))
fwrite(xgb_model_1_submission, paste(output_dir, 'ML_Model_V4_sample_submission_model_1.csv', sep = ''))
#Score: 0.94642

#### Segunda tentativa da última versão ####
# Repetindo a formula com melhor resultado, com mais interações no trainControl
# Sem utilizar normalização da variável target dessa vez
dt_train <- fread(paste(output_dir, 'dt_train_modified_2.csv', sep = ''))
spm_train <- as.matrix(dt_train[, .(feature_2, ft_category_1, ft_last_usage_days, ft_potential_fraud, target)])
fit_Control <- trainControl(method = "repeatedcv", number = 10, repeats = 20, search = "random")
xgb_model_2 <- train(target ~ ., 
                     data = spm_train,
                     method = 'xgbTree',
                     trControl = fit_Control)
xgb_model_2
# Melhores hiperparâmetros encontrados:
# nrounds = 546
# max_depth = 8
# eta = 0.3785148
# gamma = 0.9412845
# colsample_bytree = 04140678
# min_child_weight = 3
# subsample = 0.6182614

xgb_model_2_predictions <- predict.train(xgb_model_2, spm_test)
xgb_model_2_submission = data.table(card_id = dt_test$card_id,
                                    target = xgb_model_2_predictions)
fwrite(xgb_model_2_submission, paste(output_dir, 'ML_Model_V4_sample_submission_model_2.csv', sep = ''))
#Score: 0.98488