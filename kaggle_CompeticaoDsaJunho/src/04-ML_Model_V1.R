library(data.table)
library(lubridate)
library(caret)

#Configurando diretórios
cur_dir = rstudioapi::getActiveProject()
input_dir = paste(cur_dir, 'input/', sep = '/')
output_dir = paste(cur_dir, 'output/', sep = '/')

#Selecionando arquivos
files <- list.files(input_dir, '*.csv')
dt_train <- fread(paste(output_dir, 'dt_train_modified.csv', sep = ''))
threshold <- sd(dt_train$target) * 2
dt_train <- dt_train[target >= threshold * -1 & target <= threshold, .(feature_1, feature_2, feature_3,
                                                                       feature_unauthorized, feature_avg_purchase,
                                                                       feature_tot_purchase, feature_un_merchants,
                                                                       feature_months_active, target)]

# Particionando dados de treino e teste
train_index <- createDataPartition(dt_train$target, times = 1, p = 0.7, list = F)
train = dt_train[train_index]
test = dt_train[-train_index]

# Fórmulas que quero testar
formula_1 <- formula(target ~ feature_1 + feature_3)
formula_2 <- formula(target ~ feature_tot_purchase + feature_un_merchants)
formula_3 <- formula(target ~ .)

# Train Control
tr_ctr <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

# Treinando o modelo 1
model_1 <- train(formula_1, data = train,
                 method = "glmboost",
                 trControl = tr_ctr)
model_1
pred_model1 <- predict.train(model_1, test)
RMSE(pred_model1, test$target) #1.665696

# Treinando o modelo 2
model_2 <- train(formula_2, data = train,
                 method = "glmboost",
                 trControl = tr_ctr)
model_2
pred_model2 <- predict.train(model_2, test)
RMSE(pred_model2, test$target) #1.660507

# Treinando o modelo 3
model_3 <- train(formula_3, data = train,
                 method = "glmboost",
                 trControl = tr_ctr)
model_3
pred_model3 <- predict.train(model_3, test)
RMSE(pred_model3, test$target) #1.660185

# O modelo 3 apresentou a melhor performance, embora os três ainda estejam apresentando um RMSE extremamente elevado...
# Vamos carregar o dataset de treino, elaborar as novas features, fazer as previsões e submeter os resultados ao Kaggle

# Carregando o histórico
dt_hist <- fread(paste(output_dir, 'dt_hist_modified.csv', sep = ''))

# Carregando o dataset de teste
dt_test <- fread(paste(input_dir, files[2], sep = ''))
dt_test[, first_active_month := ymd(paste(first_active_month, '01', sep = '-'))]
# O registro C_ID_c27b4f80f7 não possui data de ativação!

# Carregando as funções para elaboração das features
prop_unauthorized <- function(C_ID){
  qtd_unauth <- nrow(dt_hist[authorized_flag == 'N' & card_id == C_ID])
  tot <- nrow(dt_hist[card_id == C_ID])
  return(qtd_unauth / tot)
}

avg_norm_purchase_authorized <- function(C_ID){
  return(mean(dt_hist[card_id == C_ID & authorized_flag == 'Y', norm_purchase_amount]))
}

first_usage_authorized <- function(C_ID){
  return(dt_hist[card_id == C_ID & authorized_flag == 'Y', min(purchase_date)])
}

last_usage_authorized <- function(C_ID){
  return(dt_hist[card_id == C_ID & authorized_flag == 'Y', max(purchase_date)])
}

tot_purchases_authorized <- function(C_ID){
  return(nrow(dt_hist[card_id == C_ID & authorized_flag == 'Y']))
}

tot_unique_merchant_authorized <- function(C_ID){
  return(length(dt_hist[card_id == C_ID & authorized_flag == 'Y', unique(merchant_id)]))
}

# Carregando as features
dt_test[, feature_unauthorized := sapply(card_id, prop_unauthorized)]
dt_test[, feature_avg_purchase := sapply(card_id, avg_norm_purchase_authorized)]
dt_test[, feature_tot_purchase := sapply(card_id, tot_purchases_authorized)]
dt_test[, feature_un_merchants := sapply(card_id, tot_unique_merchant_authorized)]
dt_test[, first_card_usage := sapply(card_id, first_usage_authorized)]
dt_test[, last_card_usage := sapply(card_id, last_usage_authorized)]
dt_test[, first_active_month := as_date(first_active_month)]
dt_test[, last_card_usage := as_date(last_card_usage)]
dt_test[, feature_months_active := interval(first_active_month, last_card_usage) %/% months(1)]

#Para cartões sem registro da data de ativação do cartão, utilizaremos a data da primeira transação
dt_test[is.na(feature_months_active), feature_months_active := interval(as_date(first_card_usage), last_card_usage) %/% months(1)]

# Salvando o dataset de teste com as features para uso posterior
fwrite(dt_test, paste(output_dir, 'dt_test_modified.csv', sep = ''))

submit_model1_pred <- predict.train(model_1, dt_test)
submit_model2_pred <- predict.train(model_2, dt_test)
submit_model3_pred <- predict.train(model_3, dt_test)

sample_submission <- fread(paste(input_dir, files[5], sep = ''))

sample_submission_model1 <- copy(sample_submission)
sample_submission_model1[, target := submit_model1_pred]
mean(sample_submission_model1$target)

sample_submission_model2 <- copy(sample_submission)
sample_submission_model2[, target := submit_model2_pred]
mean(sample_submission_model2$target)

sample_submission_model3 <- copy(sample_submission)
sample_submission_model3[, target := submit_model3_pred]
mean(sample_submission_model3$target)
# As médias são bastante semelhantes nos três modelos, sugerindo pouca variação

# Exportando os arquivos e submetendo ao Kaggle
fwrite(sample_submission_model1, paste(output_dir, 'ML_Model_V1_sample_submission_model_1.csv', sep = ''))
# RMSE do Kaggle: 1.17434

fwrite(sample_submission_model2, paste(output_dir, 'ML_Model_V1_sample_submission_model_2.csv', sep = ''))
# RMSE do Kaggle: 1.17222

fwrite(sample_submission_model3, paste(output_dir, 'ML_Model_V1_sample_submission_model_3.csv', sep = ''))
# RMSE do Kaggle: 1.17669

# A fórmula 2 foi a que produziu o melhor resultado.
# A próxima etapa é retornar as análises dos dados e construir novas features. As features que desenvolvi até agora não foram suficientes!