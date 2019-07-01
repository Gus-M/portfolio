library(data.table)
library(lubridate)
library(ggbiplot)
library(corrplot)

#Configurando diretórios
cur_dir = rstudioapi::getActiveProject()
input_dir = paste(cur_dir, 'input/', sep = '/')
output_dir = paste(cur_dir, 'output/', sep = '/')

#Selecionando arquivos
files <- list.files(input_dir, '*.csv')

#Carregando datasets
dt_train <- fread(paste(input_dir, files[3], sep = ''))
dt_hist <- fread(paste(input_dir, files[6], sep = ''),
                 select = c('card_id','authorized_flag',
                            'category_1','category_2','category_3',
                            'installments','month_lag','purchase_date'))

dt_hist[authorized_flag == 'Y', authorized_flag := '1']
dt_hist[authorized_flag == 'N', authorized_flag := '0']
dt_hist[, authorized_flag := as.numeric(authorized_flag)]

dt_hist[category_1 == 'Y', category_1 := '1']
dt_hist[category_1 == 'N', category_1 := '0']
dt_hist[, category_1 := as.numeric(category_1)]

dt_hist[is.na(category_2), category_2 := 0]

dt_hist[category_3 == 'A', category_3 := '3']
dt_hist[category_3 == 'B', category_3 := '2']
dt_hist[category_3 == 'C', category_3 := '1']
dt_hist[, category_3 := as.numeric(category_3)]

ref_date <- as_date(dt_hist[month_lag >= 0, max(purchase_date)])

dt_new_hist <- fread(paste(input_dir, files[4], sep = ''),
                 select = c('card_id','authorized_flag',
                            'category_1','category_2','category_3',
                            'installments','month_lag','purchase_date'))

dt_new_hist[authorized_flag == 'Y', authorized_flag := '1']
dt_new_hist[authorized_flag == 'N', authorized_flag := '0']
dt_new_hist[, authorized_flag := as.numeric(authorized_flag)]

dt_new_hist[category_1 == 'Y', category_1 := '1']
dt_new_hist[category_1 == 'N', category_1 := '0']
dt_new_hist[, category_1 := as.numeric(category_1)]

dt_new_hist[is.na(category_2), category_2 := 0]

dt_new_hist[category_3 == 'A', category_3 := '3']
dt_new_hist[category_3 == 'B', category_3 := '2']
dt_new_hist[category_3 == 'C', category_3 := '1']
dt_new_hist[, category_3 := as.numeric(category_3)]

ref_new_date <- as_date(dt_new_hist[month_lag >= 0, max(purchase_date)])

#### Elaborando funções para criação de features ####
## Proporção de transações autorizadas
prop_auth <- function(C_ID){
  return(mean(rbind(dt_hist[card_id == C_ID]$authorized_flag,
                    dt_new_hist[card_id == C_ID]$authorized_flag), na.rm = T))
}

## Conferindo se houve utilização do cartão recentemente
used_recently <- function(C_ID){
  last_usage <- dt_new_hist[card_id == C_ID, max(purchase_date)]
  if(is.na(last_usage)){
    return(-1)
  }
  return(1)
}

## Dias decorridos desde a última utilização
last_usage_days <- function(C_ID){
  last_usage <- dt_new_hist[card_id == C_ID, max(purchase_date)]
  ref <- ref_new_date
  
  if(is.na(last_usage)){
    last_usage <- dt_hist[card_id == C_ID, max(purchase_date)]
    ref <- ref_date
  }
  
  return(interval(last_usage, ref) %/% days(1))
}

## Compras parceladas potencialmente fraudulentas
potential_fraud_by_installments <- function(C_ID){
  sd_installment <- sd(rbind(dt_hist[card_id == C_ID]$installments,
                             dt_new_hist[card_id == C_ID]$installments))
  if(sd_installment > 12){
    return(1)
  }
  
  return(0)
}

## Moda das categorias mais comuns
stat_mode <- function(arr){
  occur <- unique(arr)
  return(occur[which.max(tabulate(match(arr, occur)))])
}

mode_category_1 <- function(C_ID){
  return(stat_mode(rbind(dt_hist[card_id == C_ID]$category_1,
                         dt_new_hist[card_id == C_ID]$category_1)))
}

mode_category_2 <- function(C_ID){
  return(stat_mode(rbind(dt_hist[card_id == C_ID]$category_2,
                         dt_new_hist[card_id == C_ID]$category_2)))
}

mode_category_3 <- function(C_ID){
  return(stat_mode(rbind(dt_hist[card_id == C_ID]$category_3,
                         dt_new_hist[card_id == C_ID]$category_3)))
}


#### Extraindo features ####
dt_train[, ft_auth := sapply(card_id, prop_auth)]
dt_train[, ft_used_recently := sapply(card_id, used_recently)]
dt_train[, ft_last_usage_days := sapply(card_id, last_usage_days)]
dt_train[, ft_potential_fraud := sapply(card_id, potential_fraud_by_installments)]
dt_train[, ft_category_1 := sapply(card_id, mode_category_1)]
dt_train[, ft_category_2 := sapply(card_id, mode_category_2)]
dt_train[, ft_category_3 := sapply(card_id, mode_category_3)]

#Colocando a data da última utilização em escala
dt_train[, ft_last_usage_days := scale(ft_last_usage_days)]

#Calculando a idade do cartão
dt_train[, first_active_month := ymd(paste(first_active_month, '01', sep = '-'))]
dt_train[, card_age_days := interval(first_active_month, ref_new_date) %/% days(1)]
dt_train[is.na(card_age_days), card_age_days := interval(first_active_month, ref_date) %/% days(1)]
dt_train[, card_age_days := scale(card_age_days)]

#### Observando a correlação das features extraídas ####
summary(dt_train)


corrplot(cor(dt_train[, .(feature_1, feature_2, feature_3, card_age_days,
                 ft_auth, ft_used_recently,
                 ft_last_usage_days, ft_potential_fraud,
                 ft_category_1, ft_category_2, ft_category_3, target)]))

dt_train_pca <- prcomp(dt_train[, .(feature_1, feature_2, feature_3, card_age_days,
                                      ft_auth, ft_used_recently, 
                                      ft_last_usage_days, ft_potential_fraud,
                                      ft_category_1, ft_category_2, ft_category_3)])

sapply(dt_train[, .(feature_1, feature_2, feature_3, card_age_days,
                    ft_auth, ft_used_recently, 
                    ft_last_usage_days, ft_potential_fraud,
                    ft_category_1, ft_category_2, ft_category_3)], sd)

summary(dt_train_pca)
str(dt_train_pca)
ggbiplot(dt_train_pca)
# ft_category_2 e feature_1 são as variáveis mais significativas do modelo. 
# Faremos diversos testes nessas features durante a criação do modelo

# Salvando dataset de treino
fwrite(dt_train, paste(output_dir, 'dt_train_modified_2.csv', sep = ''))

# Criando features para o dataset de teste
dt_test <- fread(paste(input_dir, files[2], sep = ''))
dt_test[, ft_auth := sapply(card_id, prop_auth)]
dt_test[, ft_used_recently := sapply(card_id, used_recently)]
dt_test[, ft_last_usage_days := sapply(card_id, last_usage_days)]
dt_test[, ft_potential_fraud := sapply(card_id, potential_fraud_by_installments)]
dt_test[, ft_category_1 := sapply(card_id, mode_category_1)]
dt_test[, ft_category_2 := sapply(card_id, mode_category_2)]
dt_test[, ft_category_3 := sapply(card_id, mode_category_3)]
dt_test[, ft_last_usage_days := scale(ft_last_usage_days)]
dt_test[, first_active_month := ymd(paste(first_active_month, '01', sep = '-'))]
dt_test[, card_age_days := interval(first_active_month, ref_new_date) %/% days(1)]
dt_test[is.na(card_age_days), card_age_days := interval(first_active_month, ref_date) %/% days(1)]
dt_test[, card_age_days := scale(card_age_days)]
fwrite(dt_test, paste(output_dir, 'dt_test_modified_2.csv', sep = ''))
