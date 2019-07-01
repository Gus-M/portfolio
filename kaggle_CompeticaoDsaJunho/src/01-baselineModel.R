library(data.table)
library(caret)

#Configurando diretórios
cur_dir = rstudioapi::getActiveProject()
input_dir = paste(cur_dir, 'input/', sep = '/')
output_dir = paste(cur_dir, 'output/', sep = '/')

#Selecionando arquivos
files <- list.files(input_dir, '*.csv')
tbl_treino <- fread(paste(input_dir, files[3], sep = ''))

#Determinando um threshold para os dados de treino
tbl_treino[, .(mean(target), sd(target))]

#As amostras que selecionarei para treinar os modelos baseline serão de até 2SD o valor da variável target
threshold <- sd(tbl_treino$target) * 2
tbl_treino <- tbl_treino[target >= threshold * -1 & target <= threshold, .(feature_1, feature_2, feature_3, target)]

#Separando dados de treino e teste
train_index <- createDataPartition(tbl_treino$target, times = 1, p = 0.7, list = F)
dt_train = tbl_treino[train_index]
dt_test = tbl_treino[-train_index]

#Fórmula
pred_formula = formula(target ~ feature_1 + feature_2 + feature_3)

#Criando os modelos
model_lm <- train(pred_formula, data = dt_train, method = 'lm')
predictions <- predict.train(model_lm, dt_test)
RMSE(predictions, dt_test$target)
#RMSE usando regressão linear: 1.65
#Podemos melhorar o desempenho utilizando outros modelos, mas este índice de RMSE é um forte indicador de que
#precisamos de mais features para justificar a variável target