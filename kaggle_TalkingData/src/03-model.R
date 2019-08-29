source('src/02-toolkit.R')
library(caret)
library(MLmetrics)

#Carregando arquivos
files = list.files('input', '.csv', full.names = T)
print(system('wc -l input/train.csv', intern = T))
#Output: 184903891 input/train.csv
#O arquivo de treino possui 184 milhões de observações. Será impossível trabalhar com este volume de dados no meu ambiente.
#Optei por 'marcar' o arquivo completo em 9 partições, de 10% a 90% do total de linhas
start_row_index <- 184903891 * seq(0, 0.95, 0.05)

## Carga dos dados
#A partir de cada index, serão lidos 2.000.000 registros
batch_size <- 2000000
data_list <- lapply(start_row_index, function(x){
  fread(files[4], nrows = batch_size, skip = x,
        col.names = c('ip', 'app', 'device', 'os', 'channel',
                      'click_time', 'attributed_time', 'is_attributed'))
})

#E então, juntamos os dados extraídos em um único data frame
tbl_train <- rbindlist(data_list)

#Semelhante ao que fizemos durante a análise, faremos "undersampling" deste conjunto,
#de forma a manter a proporção de downloads
set.seed(10)
train_sample <- tbl_train[is_attributed == 1]
batch_size <- nrow(train_sample)
train_sample <- rbind(train_sample, tbl_train[is_attributed == 0][sample(.N, batch_size)])

## Feature Engineering
#Calculando features baseadas em downloads e cliques
features <- id_means(train_sample)

#Retirando as variáveis não utilizadas IP e attributed_time
drop_unused_cols <- function(tbl){
  unused_cols <- c('ip', 'app', 'device', 'os', 'channel', 'click_time')
  if ('attributed_time' %in% names(tbl)) {
    unused_cols <- c(unused_cols, 'attributed_time')
  }
  return(tbl[, (unused_cols) := NULL])
}

#Agrupando as features calculadas
bind_features <- function(tbl){
  data <- tbl
  data <- merge(data, features[[1]], all.x = T, by = 'app')
  data <- merge(data, features[[2]], all.x = T, by = 'app')
  data <- merge(data, features[[3]], all.x = T, by = 'device')
  data <- merge(data, features[[4]], all.x = T, by = 'device')
  data <- merge(data, features[[5]], all.x = T, by = 'os')
  data <- merge(data, features[[6]], all.x = T, by = 'os')
  data <- merge(data, features[[7]], all.x = T, by = 'channel')
  data <- merge(data, features[[8]], all.x = T, by = 'channel')
  return(data)
}

#É provável que alguns IDs não tenham sido identificados durante o cálculo de features
#Ao tentar associar estes IDs em novos datasets, um valor NA será introduzido
#Estes serão substituídos por zero
impute_constant_NA <- function(tbl){
  data <- apply(tbl, 2, function(x){
    if (any(is.na(x))) {
      x[is.na(x)] <- 0
    }
    return(x)
  })
  return(as.data.table(data))
}

#Realizado os cálculos, colocamos a variável target como fator
convert_target_col <- function(tbl){
  #Faremos a conversão apenas se o dataset informado possuir o registro de download
  if ('is_attributed' %in% names(tbl)) {
    tbl[, is_attributed := as.factor(is_attributed)]
  }
  return(tbl)
}

#Colocamos as etapas de pré-processamento em um pipeline
prep_pipeline <- function(tbl){
  data <- strip_date(tbl)
  data <- bind_features(data)
  data <- drop_unused_cols(data)
  data <- impute_constant_NA(data)
  data <- convert_target_col(data)
  return(data)
}

train_sample <- prep_pipeline(train_sample)

#### Construção do modelo preditivo ####
#Train/test split: 70% dos dados serão utilizados para treino
t_index = createDataPartition(train_sample$is_attributed, times = 1, p = 0.7, list = F)
train = train_sample[t_index,]
test = train_sample[-t_index,]

#Vamos criar um modelo preditivo utilizando regressão logística (glm)
#O primeiro modelo será criado utilizando todas as variáveis disponíveis
global_formula = formula(is_attributed ~ .)
glm_model = glm(global_formula, data = train, family = 'binomial')

#Em seguida, utilizamos a função 'step' para otimizar a fórmula e
#identificar a combinação de atributos que produz o melhor resultado
step_model <- step(glm_model, direction = 'both', trace = 0)
print(summary(step_model))
#A melhor fórmula utilizou todas as métricas de média 
#em conjunto com a hora e dia do clique
best_formula <- step_model$formula

#Utilizaremos a fórmula otimizada para treinar um novo modelo glm
#Também utilizaremos cross validation para otimizar o treinamento do modelo
tr_c = trainControl(method = 'repeatedcv', number = 10, repeats = 10)
glm_model = train(best_formula, data = train, method = 'glm',
                  family = 'binomial', trControl = tr_c)

#Plot de importância das variáveis
plot(varImp(glm_model, scale = T))

#Previsões
valid_preds = predict(glm_model, test)

#Análise dos resultados - Confusion matrix
print(ConfusionMatrix(valid_preds, test$is_attributed))
#Scores:
print(Accuracy(valid_preds, test$is_attributed))
##Accuracy: 91%
#Compara-se todas as previsões e valores reais. Accuracy refere-se 
#a capacidade do modelo classificar corretamente as classes 0 e 1
#Obtivemos um resultado satisfatório neste dataset balanceado 50/50

#Accuracy é uma boa métrica para modelos de classificação, mas pouco útil neste caso em que
#sabemos que menos de 1% dos registros resultarão em um download.
#Num cenário hipotético, se apenas 1% dos registros resultaram em um download, é possível
#obter 99% de Accuracy prevendo que todos os cliques não resultaram em download!

#Queremos um modelo capaz de identificar as ocorrências de um download
#Para isso, utilizamos as métricas: Precision, Recall e F1 Score

print(Recall(test$is_attributed, valid_preds, positive = '1'))
##Recall: 85%
#Das ocorrências reais de download, 86% foram previstas corretamente pelo modelo (12478 / (12478 + 1871))

print(Precision(test$is_attributed, valid_preds, positive = '1'))
##Precision: 96%
#Das ocorrências de download previstas pelo modelo, 96% foram corretas. (12478 / (12478 + 443))

print(F1_Score(test$is_attributed, valid_preds, positive = '1'))
##F1_Score: 90%
#Utilizamos o score F1 para observar um resultado balanceado de sensitivity e specificity
#Dessa forma, conseguimos balancear de maneira mais generalizada a performance do modelo em 
#identificar as ocorrências positivas
#F1 = 2 * ((Precision * Recall) / (Precision + Recall))
#F1 = 2 * ((0.86 * 0.96) / (0.86 + 0.96))
#Dependendo do problema de negócio, realizamos ajustes no modelo para melhorar as métricas acima

#Obtivemos os scores acima através de um dataset balanceado. Faremos o teste deste modelo
#com valores reais, selecionando uma amostra aleatória do dataset original
batch_size <- 1000000
test_sample = prep_pipeline(tbl_train[sample(.N, batch_size)])

#Realizando novas previsões
test_preds = predict(glm_model, test_sample)

#Análise dos resultados - Confusion Matrix
print(ConfusionMatrix(test_preds, test_sample$is_attributed))
#Scores:
print(Accuracy(test_preds, test_sample$is_attributed))
##Accuracy: 97%

print(Recall(test_sample$is_attributed, test_preds, positive = '1'))
##Recall: 85%

print(Precision(test_sample$is_attributed, test_preds, positive = '1'))
##Precision: 6%

print(F1_Score(test_sample$is_attributed, test_preds, positive = '1'))
##F1 Score: 12%

#Quando testamos o modelo preditivo com valores inéditos, percebemos uma melhoria na Accuracy
#do modelo. Recall permaneceu num patamar semelhante ao observado durante o treino e 
#percebemos uma redução significativa de Precision!
#Nosso modelo preditivo classificou a ocorrência de 34107 downloads, 
#mas este datasset possui apenas 2707 downloads

#Retirando variáveis não utilizadas
rm(tbl_train, train, test, train_sample, test_sample, data_list)
invisible(gc())

#A partir deste ponto, precisamos validar se essa performance é satisfatória para o problema de negócio
#Para este problema, vamos determinar a performance do modelo submetendo os resultados da competição
kaggle_data <- prep_pipeline(fread(files[2]))

#Efetuando previsões
submission_preds <- predict(glm_model, kaggle_data)

submission <- data.table(click_id = as.integer(kaggle_data$click_id),
                         is_attributed = submission_preds)
submission <- submission[order(click_id)]

#Gravando arquivo
fwrite(submission, 'output/glm_submission.csv')
#Score: 0.90251