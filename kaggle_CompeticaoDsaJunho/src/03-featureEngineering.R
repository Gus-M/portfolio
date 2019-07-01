library(data.table)
library(lubridate)

#Configurando diretórios
cur_dir = rstudioapi::getActiveProject()
input_dir = paste(cur_dir, 'input/', sep = '/')
output_dir = paste(cur_dir, 'output/', sep = '/')

#Selecionando arquivos
files <- list.files(input_dir, '*.csv')

#Carregando datasets
dt_train <- fread(paste(input_dir, files[3], sep = ''))
dt_hist <- fread(paste(output_dir, 'dt_hist_modified.csv', sep = ''))

#Convertendo variáveis temprais de string para date
dt_train[, first_active_month := ymd(paste(first_active_month, '01', sep = '-'))]
dt_hist[, purchase_date := as_date(purchase_date)]

#### Elaborando funções para criação de features ####
## Proporção de transações não autorizadas
prop_unauthorized <- function(C_ID){
  qtd_unauth <- nrow(dt_hist[authorized_flag == 'N' & card_id == C_ID])
  tot <- nrow(dt_hist[card_id == C_ID])
  return(qtd_unauth / tot)
}

## Custo normalizado médio - transações autorizadas
avg_norm_purchase_authorized <- function(C_ID){
  return(mean(dt_hist[card_id == C_ID & authorized_flag == 'Y', norm_purchase_amount]))
}

## Período de utilização do cartão em meses - transações autorizadas
last_usage_authorized <- function(C_ID){
  return(dt_hist[card_id == C_ID & authorized_flag == 'Y', max(purchase_date)])
}

## Total de transações autorizadas
tot_purchases_authorized <- function(C_ID){
  return(nrow(dt_hist[card_id == C_ID & authorized_flag == 'Y']))
}

## Total de vendedores distintos - transações autorizadas
tot_unique_merchant_authorized <- function(C_ID){
  return(length(dt_hist[card_id == C_ID & authorized_flag == 'Y', unique(merchant_id)]))
}

#### Extraindo features ####
dt_train[, feature_unauthorized := sapply(card_id, prop_unauthorized)]
dt_train[, feature_avg_purchase := sapply(card_id, avg_norm_purchase_authorized)]
dt_train[, feature_tot_purchase := sapply(card_id, tot_purchases_authorized)]
dt_train[, feature_un_merchants := sapply(card_id, tot_unique_merchant_authorized)]
dt_train[, last_card_usage := sapply(card_id, last_usage_authorized)]
dt_train[, first_active_month := as_date(first_active_month)]
dt_train[, last_card_usage := as_date(last_card_usage)]
dt_train[, feature_months_active := interval(first_active_month, last_card_usage) %/% months(1)]
# # O processamento das features acima levou cerca de 50 minutos

#### Observando a correlação das features extraídas ####
corrplot::corrplot(cor(dt_train[, .(feature_1, feature_2, feature_3, 
                 feature_unauthorized, feature_avg_purchase, 
                 feature_tot_purchase, feature_un_merchants,
                 feature_months_active, target)]))

# Há uma correlação maior entre as features 1 + 3 e unique_merchants e total_purchase
# As features possuem pouca correlação com a variável target. É provável que não seja perceptível muita diferença no RMSE
# do modelo base..
# Criaremos alguns modelos utilizando essas features! Precisamos observar o resultado 
# de nossa análise até agora para saber onde podemos melhorar
fwrite(dt_train, paste(output_dir, 'dt_train_modified.csv', sep = ''))
