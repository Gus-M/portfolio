library(tidyverse)
library(data.table)

#Configurando diretórios
cur_dir = rstudioapi::getActiveProject()
input_dir = paste(cur_dir, 'input/', sep = '/')
output_dir = paste(cur_dir, 'output/', sep = '/')

#Selecionando arquivos
files <- list.files(input_dir, '*.csv')

#Carregando datasets
dt_train <- fread(paste(input_dir, files[3], sep = ''))
dt_hist <- fread(paste(input_dir, files[6], sep = ''), select = c('authorized_flag', 'card_id', 'merchant_id', 'purchase_amount', 'purchase_date'))
dt_hist_novas <- fread(paste(input_dir, files[4], sep = ''), select = c('authorized_flag', 'card_id', 'merchant_id', 'purchase_amount', 'purchase_date'))
#Agrupando os odis datasets de histórico
dt_hist <- rbindlist(list(dt_hist, dt_hist_novas))
rm(dt_hist_novas)

#Colhendo amostras dos dois extremos no dataset de treino: menor fidelidade e maior fidelidade
dt_min_target <- dt_train[target == min(target)]
dt_max_target <- dt_train[target == max(target)]

#### Análise do dataset de treino e features disponíveis ####
#Observando as features 1, 2 e 3
dt_min_target
dt_max_target

# Há diversos registros com menor grau de fidelidade e diversas combinações entre as features
# De fato, não parece haver qualquer relação entre as features já apresentadas e o valor da variavel target
dt_min_target[, sum_features := feature_1 + feature_2 + feature_3]
sapply(dt_min_target[,.(feature_1, feature_2, feature_3, sum_features, target)], table)
# Ainda que hajam números maiores ou menores nas features, a variável target apresentou o pior resultado

# Selecionando alguns cartões para análise de histórico 
best_card <- dt_max_target$card_id
worst_cards <- dt_min_target[sample(.N, 5)]$card_id

#### Análise do histórico dos cartões selecionados ####
range(dt_hist$purchase_amount)
# O range de valores de compra está muito discrepante para valores colocados na mesma escala...
summary(dt_hist$purchase_amount)
dt_hist[purchase_amount > 1]
# As transações não autorizadas parecem não ter sido normalizadas
dt_hist[authorized_flag == 'Y' & purchase_amount > 1]

#Observando o range de valores novamente, por tipo de transação
dt_hist[authorized_flag == 'Y', range(purchase_amount)]
dt_hist[authorized_flag == 'N', range(purchase_amount)]

#Observando a média e desvio padrão por volume de compra
dt_hist[, list(mean(purchase_amount), sd(purchase_amount))]
dt_hist[purchase_amount < 1, list(mean(purchase_amount), sd(purchase_amount), min(purchase_amount), max(purchase_amount))]
dt_hist[purchase_amount > 1, list(mean(purchase_amount), sd(purchase_amount), min(purchase_amount), max(purchase_amount))]
dt_hist[purchase_amount == 1]
#Os valores de compra estão normalizados em escalas diferentes. Alguns valores extremamente discrepantes da média
#parecem não ter sofrido qualquer tipo de normalização.
#Pela presença de valores negativos, acredito que os dados foram padronizados numa escala entre -1 e 1.

#Farei o seguinte para padronizar estes valores:
# * Separar cada dataset em que o valor da compra é maior ou menor que 1
# * Aplicar normalização dos valores de compra
# * Agrupar todos os dados e colocá-los na mesma escala
dt_hist[purchase_amount < 1, norm_purchase_amount := (purchase_amount - min(purchase_amount))/(max(purchase_amount) - min(purchase_amount))]
dt_hist[purchase_amount > 1, norm_purchase_amount := (purchase_amount - min(purchase_amount))/(max(purchase_amount) - min(purchase_amount))]
dt_hist[, norm_purchase_amount := scale(norm_purchase_amount)]
dt_hist[, list(mean(norm_purchase_amount), sd(norm_purchase_amount))]

#Observando as transações do melhor cartão
dt_best_card_hist <- dt_hist[card_id == best_card]
dt_best_card_hist
#Existem apenas 8 transações, todas elas autorizadas

#Observando as transações dos piores cartões que selecionei
dt_worst_card_hist <- dt_hist[dt_hist[, card_id %in% rep(worst_cards)]]
lapply(worst_cards, function(x){
  dt_worst_card_hist[card_id == x]
})
# As cinco amostras que selecionei possuem algo em comum: pelo menos uma transação não foi autorizada
# Em contrapartida, todas as transações do melhor cartão foram autorizadas.
# Podemos calcular duas features: Se o cartão já teve uma transação não autorizada e a proporção de transações não autorizadas em relação ao total
# Acredito que exista uma correlação negativa entre o número de transações não autorizadas e a variável target

#### Conclusões e próximas etapas ####
# Existem muitas variáveis à nossa disposição, sendo possível conduzir diversos tipos de trabalhos diferentes
# Como o objetivo desta competição é "prever o índice de lealdade para cada card_id", acredito que as featuresmais importantes sejam:
# * Volume de compras (Quantidade de compras realizadas, valor médio das compras)
# * Situação das compras (transações autorizadas e não autorizadas)
# * Periodicidade das compras (Frequência mensal de compras)
# No próximo documento, criarei algumas funções para elaborar features e testá-las com a variável target
# Não tenho um plano específico, apenas algumas idéias que gostaria de colocar em prática e observar o resultado

## Salvando o histórico transformado
fwrite(dt_hist, paste(output_dir, 'dt_hist_modified.csv', sep = ''))