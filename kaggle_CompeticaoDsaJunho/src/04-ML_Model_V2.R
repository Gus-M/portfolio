library(data.table)
library(caret)
library(randomForest)

# Antes de retornar a análise de dados e construção de novas features, quero submeter minhas features atuais em um modelo Random Forest

#Configurando diretórios
cur_dir = rstudioapi::getActiveProject()
input_dir = paste(cur_dir, 'input/', sep = '/')
output_dir = paste(cur_dir, 'output/', sep = '/')

#Selecionando arquivos
files <- list.files(input_dir, '*.csv')
dt_train <- fread(paste(output_dir, 'dt_train_modified.csv', sep = ''))
dt_test <- fread(paste(output_dir, 'dt_test_modified.csv', sep = ''))
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
# formula_1 <- formula(target ~ feature_1 + feature_3)
# formula_2 <- formula(target ~ feature_tot_purchase + feature_un_merchants)
# formula_3 <- formula(target ~ .)

# rf_imp_f1 <- randomForest(formula_1, data = train, ntree = 10, importance = T)
# varImpPlot(rf_imp_f1)
# 
# rf_imp_f2 <- randomForest(formula_2, data = train, ntree = 10, importance = T)
# varImpPlot(rf_imp_f2)
# 
# rf_imp_f3 <- randomForest(formula_3, data = train, ntree = 10, importance = T)
# varImpPlot(rf_imp_f3)

# As minhas features possuem o maior grau de pureza e um baixo nível de MSE, exceto Feature_months_active
# Testarei uma nova fórmula
formula_4 <- formula(target ~ feature_unauthorized + feature_tot_purchase + feature_un_merchants + feature_avg_purchase)

# Train Control
tr_ctr <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

# Treinando o modelo 1
model_1 <- train(formula_4, data = train,
                 method = "glmboost",
                 trControl = tr_ctr)
model_1
pred_model1 <- predict.train(model_1, test)
RMSE(pred_model1, test$target) #1.65865

# Treinando o modelo 2
model_2 <- train(formula_4, data = train,
                 method = "gamboost",
                 trControl = tr_ctr)
model_2
pred_model2 <- predict.train(model_2, test)
RMSE(pred_model2, test$target) #1.654898

# Treinando o modelo 3
model_3 <- train(formula_4, data = train,
                 method = "BstLm",
                 trControl = tr_ctr)
model_3
pred_model3 <- predict.train(model_3, test)
RMSE(pred_model3, test$target) #1.65993

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
fwrite(sample_submission_model1, paste(output_dir, 'ML_Model_V2_sample_submission_model_1.csv', sep = ''))
# RMSE do Kaggle: 1.17771

fwrite(sample_submission_model2, paste(output_dir, 'ML_Model_V2_sample_submission_model_2.csv', sep = ''))
# RMSE do Kaggle: 1.17391

fwrite(sample_submission_model3, paste(output_dir, 'ML_Model_V2_sample_submission_model_3.csv', sep = ''))
# RMSE do Kaggle: 1.17436

# Testamos diferentes modelos e uma nova fórmula, obtendo ganhos pouco significativos
# Voltaremos à etapa inicial e pensar em novas features! 