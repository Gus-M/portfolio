library(data.table)
library(xgboost)
library(caret)

#Configurando diretórios
cur_dir = rstudioapi::getActiveProject()
input_dir = paste(cur_dir, 'input/', sep = '/')
output_dir = paste(cur_dir, 'output/', sep = '/')

#Selecionando arquivos
files <- list.files(input_dir, '*.csv')
dt_train <- fread(paste(output_dir, 'dt_train_modified_2.csv', sep = ''))
dt_test <- fread(paste(output_dir, 'dt_test_modified_2.csv', sep = ''))

spm_train <- as.matrix(dt_train[, .(feature_1, feature_2, feature_3, ft_auth, ft_used_recently, ft_last_usage_days,
                                    ft_potential_fraud, ft_category_1, ft_category_2, ft_category_3, card_age_days)])
spm_train_output <- as.matrix(dt_train[, target])

xgb_model_1_cv <- xgb.cv(data = spm_train,
                      label = spm_train_output,
                      eval.metric = 'rmse',
                      objective = 'reg:linear',
                      nfold = 10, max.depth = 6, eta = 0.1,
                      nthread = 8, subsample = 1, colsample_bytree = 0.5,
                      lambda = 0.1, alpha = 0.01, min_child_weight = 3, nround = 100)

xgb_train <- xgb.DMatrix(data = spm_train, label = spm_train_output)
xgb_model_1 <- xgboost(data = xgb_train,
                       eval.metric = 'rmse',
                       objective = 'reg:linear',
                       nfold = 10, max.depth = 6, eta = 0.1,
                       nthread = 8, subsample = 1, colsample_bytree = 0.5,
                       lambda = 0.1, alpha = 0.01, min_child_weight = 3, nround = 2000)

spm_test <- as.matrix(dt_test[, .(feature_1, feature_2, feature_3, ft_auth, ft_used_recently, ft_last_usage_days,
                                    ft_potential_fraud, ft_category_1, ft_category_2, ft_category_3, card_age_days)])
xgb_test <- xgb.DMatrix(data = spm_test)
xgb_model_1_predictions <- predict(xgb_model_1, xgb_test)
xgb_model_1_predictions

xgb_model_1_submission = data.table(card_id = dt_test$card_id,
                                    target = xgb_model_1_predictions)
fwrite(xgb_model_1_submission, paste(output_dir, 'ML_Model_V3_sample_submission_model_1.csv', sep = ''))
#Score: 1.23217

# O score obtido foi muito ruim. Vamos buscar as variáveis mais importantes e tentar novamente
importance <- xgb.importance(names(dt_train), model = xgb_model_1)
xgb.plot.importance(importance)

# Fazendo uma nova tentativa utilizando todas as features. Dessa vez, utilizando o pacote caret
# e RandomSearchCV, pois não sei ainda como tunar os hiperparâmetro do xgboost
spm_train <- as.matrix(dt_train[, .(feature_2, ft_category_1, ft_last_usage_days, ft_potential_fraud, target)])

fit_Control <- trainControl(method = "repeatedcv", number = 10, repeats = 10, search = "random")
xgb_model_2 <- train(target ~ ., 
                     data = spm_train,
                     method = 'xgbTree',
                     trControl = fit_Control)
xgb_model_2
# Melhores hiperparâmetros encontrados:
# nrounds = 86
# max_depth = 3
# eta = 0.5296292
# gamma = 4.583197
# colsample_bytree = 0.6124231
# min_child_weight = 6
# subsample = 0.4605527

xgb_model_2_predictions <- predict.train(xgb_model_2, spm_test)
xgb_model_2_submission = data.table(card_id = dt_test$card_id,
                                    target = xgb_model_2_predictions)
fwrite(xgb_model_2_submission, paste(output_dir, 'ML_Model_V3_sample_submission_model_2.csv', sep = ''))
#Score: 0.95595

# Obtive meu melhor score até agora! Como ainda tenho mais uma tentativa, vou tentar utilizar todas as features novamente
# em conjunto com o RandomSearchCV para ver se consigo um resultado melhor
spm_train <- as.matrix(dt_train[, .(feature_1, feature_2, feature_3, ft_auth, ft_used_recently, ft_last_usage_days,
                                    ft_potential_fraud, ft_category_1, ft_category_2, ft_category_3, card_age_days, target)])

fit_Control <- trainControl(method = "repeatedcv", number = 10, repeats = 10, search = "random")
xgb_model_3 <- train(target ~ ., 
                     data = spm_train,
                     method = 'xgbTree',
                     trControl = fit_Control)
xgb_model_3
# Melhores hiperparâmetros encontrados:
# nrounds = 437
# max_depth = 6
# eta = 0.08729803
# gamma = 0.7750195
# colsample_bytree = 0.3320078
# min_child_weight = 2
# subsample = 0.4733438

spm_test <- as.matrix(dt_test[, .(feature_1, feature_2, feature_3, ft_auth, ft_used_recently, ft_last_usage_days,
                                  ft_potential_fraud, ft_category_1, ft_category_2, ft_category_3, card_age_days)])

xgb_model_3_predictions <- predict.train(xgb_model_3, spm_test)
xgb_model_3_submission = data.table(card_id = dt_test$card_id,
                                    target = xgb_model_3_predictions)
# Houve algum problema, pois o modelo não conseguiu prever 900 registros
fwrite(xgb_model_3_submission, paste(output_dir, 'ML_Model_V3_sample_submission_model_3.csv', sep = ''))
#Score: 1.42972
# Utilizar features demais piorou o resultado!

#### Conclusões ####
# Meu melhor resultado até agora foi utilizando apenas as 4 features demonstradas como mais importantes
# pela análise de importância do pacote xgb. Quando tentei gerar um novo modelo utilizando todas as features,
# não foi possível realizar todas as previsões e foi obtido o pior resultado
# A próxima e última etapa que tenho tempo para desenvolver será juntar as features abaixo com as features
# que desenvolvi semana passada, tentando buscar um resultado na competição melhor que 0.95595
#
# feature_2, ft_category_1, ft_last_usage_days, ft_potential_fraud, target