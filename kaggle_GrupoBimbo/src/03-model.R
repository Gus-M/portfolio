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
tbl_train <- fread(paste(input_dir, files[6], sep = '/'),
                   select = c('Semana', 'Agencia_ID', 'Canal_ID', 'Cliente_ID', 'Producto_ID', 'Demanda_uni_equil'))

#Colocando a demanda em escala logarítmica
tbl_train[, Demanda_uni_equil := log1p(Demanda_uni_equil)]

#Serão selecionados 500.000 registros das semanas 5, 6, 7 e 8
set.seed(11)
train_sample <- bind_rows(lapply(5:8, function(x){
  tbl_train[Semana == x][sample(.N, 500000)]
})) %>% as_tibble

#Utilizamos os dados da semana 9 para testar o modelo
test_sample <- tbl_train[Semana == 9][sample(.N, 500000)]
rm(tbl_train)

#Calculamos as features
features = calc_train_features(train_sample, tbl_products, tbl_towns)

#Agrupando features
bind_features <- function(tib){
  #Demanda média por canal
  data <- tib %>% left_join(features[[1]], by = 'Canal_ID')
  
  #Demanda média por distribuidora
  data <- data %>% left_join(features[[2]], by = 'Agencia_ID')
  
  #Demanda média por cidade
  data <- data %>% left_join(features[[3]], by = 'Town_ID')
  
  #Demanda média de produtos e clientes
  data <- data %>% left_join(features[[6]], by = c('Cliente_ID', 'Producto_ID')) %>%
    left_join(features[[4]], by = 'Cliente_ID') %>%
    left_join(features[[5]], by = 'Producto_ID')
    
  #Quantidade de produtos distintos por cliente
  data <- data %>% left_join(features[[7]], by = 'Cliente_ID')
  
  #Retornamos apenas os campos calculados
  return(data %>% 
           select(-Semana, -Agencia_ID, -Canal_ID,
                  -Producto_ID, -Cliente_ID, -Town_ID, -State_ID))
}

#Caso ocorram valores NA após
impute_NA_values <- function(tib){
  #Valores NA na demanda média por produto e cliente serão substituídos na seguinte ordem:
  # * Demanda média do cliente
  # * Demanda média do produto
  data <- as.data.table(tib)
  data[is.na(mean_cliente_producto_demanda), mean_cliente_producto_demanda := mean_demanda_cliente]
  data[is.na(mean_cliente_producto_demanda), mean_cliente_producto_demanda := mean_producto_demanda]
  data$mean_demanda_cliente <- NULL
  data$mean_producto_demanda <- NULL

  #As ocorrências restantes de valores NA serão imputadas pela mediana
  data <- apply(data, 2, function(x){
    if (any(is.na(x))) {
      impute_median <- median(x, na.rm = T)
      x[is.na(x)] <- impute_median
    }
    return(x)
  })
  
  return(data)
}

#Das features calculadas, precisamos colocar 'unique_product_count' numa escala padronizada
scale_features <- function(tib){
  data <- as.data.table(tib)
  min_value <- min(data$unique_product_count)
  max_value <- max(data$unique_product_count)
  
  return(data[, unique_product_count := (unique_product_count - min_value) / (max_value - min_value)])
}

#Pipeline de pré-processamento
prep_pipeline <- function(tib){
  return(bind_features(tib) %>%
           impute_NA_values %>%
           scale_features)
}

## Fórmulas que serão testadas
#Utilizando todas as variáveis
formula_1 <- formula(Demanda_uni_equil ~ .)

#Utilizando apenas a média por cliente
formula_2 <- formula(Demanda_uni_equil ~ mean_cliente_producto_demanda)

#Utilizando todas as médias
formula_3 <- formula(Demanda_uni_equil ~ mean_cliente_producto_demanda + mean_channel_demanda + mean_depot_demanda)

#Utilizando a média por cliente ajustada pelas características dos produtos 
formula_4 <- formula(Demanda_uni_equil ~ mean_cliente_producto_demanda + unique_product_count + heavy_product + bundled_product)

#Utilizando a média por cliente ajustada pelas características das distribuidoras
formula_5 <- formula(Demanda_uni_equil ~ mean_cliente_producto_demanda + mean_depot_demanda + large_scale_depot)

## Datasets
#Utilizando todos os dados de treino
train_sample <- prep_pipeline(train_sample)
test_sample <- prep_pipeline(test_sample)

#Utilizando um subset dos dados de treino
mean_threshold <- mean(train_sample$Demanda_uni_equil)
sd_threshold <- sd(train_sample$Demanda_uni_equil)

#Entre 1.5 desvios padrões da média
train_sample_15 <- train_sample %>%
  filter(Demanda_uni_equil >= mean_threshold - (1.5 * sd_threshold) & 
         Demanda_uni_equil <= mean_threshold + (1.5 * sd_threshold))

#Entre 2.0 desvios padrões da média
train_sample_20 <- train_sample %>%
  filter(Demanda_uni_equil >= mean_threshold - (2 * sd_threshold) & 
           Demanda_uni_equil <= mean_threshold + (2 * sd_threshold))

#Entre 2.5 desvios padrões da média
train_sample_25 <- train_sample %>%
  filter(Demanda_uni_equil >= mean_threshold - (2.5 * sd_threshold) & 
           Demanda_uni_equil <= mean_threshold + (2.5 * sd_threshold))

#Entre 3.0 desvios padrões da média
train_sample_30 <- train_sample %>%
  filter(Demanda_uni_equil >= mean_threshold - (3 * sd_threshold) & 
           Demanda_uni_equil <= mean_threshold + (3 * sd_threshold))

## Selecionando o melhor modelo
train_lm <- function(formulae, train_datasets, test_data){
  
  results <- lapply(train_datasets, function(x){
    sapply(formulae, function(y){
      lm_model <- lm(formula = y, data = x)
      preds <- predict.lm(lm_model, test_data)
      score <- RMSLE(expm1(preds), expm1(test_data$Demanda_uni_equil))
      print(summary(lm_model))
      return(score)
      })
  })

  return(results)
}

train_results <- train_lm(list(formula_1, formula_2, formula_3, formula_4, formula_5),
                          list(train_sample, train_sample_15, train_sample_20, train_sample_25, train_sample_30),
                          test_sample)
train_results
fwrite(train_results, 'output/train_results.csv')
#Os melhores resultados foram obtidos através da fórmula 2 (Demanda_uni_equil ~ mean_cliente_producto_demanda)
model_lm_f1 <- lm(formula_1, data = train_sample)
model_lm_f2 <- lm(formula_2, data = train_sample_15)

preds_f1 <- predict.lm(model_lm_f1, test_sample)
preds_f2 <- predict.lm(model_lm_f2, test_sample)

predicted_test <- data.frame(Demanda = test_sample$Demanda_uni_equil,
                             Predict_f1 = preds_f1,
                             Predict_f2 = preds_f2)
fwrite(predicted_test, 'output/test_predictions.csv')

#Submetendo modelo preditivo ao kaggle
kaggle_test_data = prep_pipeline(fread(paste(input_dir, files[4], sep = '/')))

submission_preds_lm_f1 <- predict.lm(model_lm_f1, kaggle_test_data)
submission_preds_lm_f1 <- replace(submission_preds_lm_f1, submission_preds_lm_f1 < 0, 0)

submission_preds_lm_f2 <- predict.lm(model_lm_f2, kaggle_test_data)
submission_preds_lm_f2 <- replace(submission_preds_lm_f2, submission_preds_lm_f2 < 0, 0)

#Colocando as previsões na escala original
submission_preds_lm_f1 <- expm1(submission_preds_lm_f1)
submission_preds_lm_f2 <- expm1(submission_preds_lm_f2)

#Carregando sample submission e submetendo resultados
submission = fread(paste(input_dir, files[3], sep = '/'))

submission[, Demanda_uni_equil := submission_preds_lm_f1]
fwrite(submission, 'output/sample_submission_lm_f1.csv')
#Score público: 0.80168
#Score privado: 0.80942

submission[, Demanda_uni_equil := submission_preds_lm_f2]
fwrite(submission, 'output/sample_submission_lm_f2.csv')
#Score público: 0.80025
#Score privado: 0.80804

kaggle_test_data[, preds_lm_f1 := submission_preds_lm_f1]
kaggle_test_data[, preds_lm_f2 := submission_preds_lm_f2]
kaggle_test_data
fwrite(kaggle_test_data, 'output/kaggle_test_data_predictions.csv')