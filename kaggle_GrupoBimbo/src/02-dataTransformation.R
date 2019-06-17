library(tidyverse)
library(data.table)

#Configurando diretórios
cur_dir = rstudioapi::getActiveProject()
input_dir = paste(cur_dir, 'input/', sep = '/')
output_data_dir = paste(cur_dir, 'output/data/', sep = '/')

#Selecionando arquivos
files <- list.files(output_data_dir)
tbl_clients <- read_csv2(paste(output_data_dir, files[1], sep = ''))
tbl_products <- read_csv2(paste(output_data_dir, files[2], sep = ''))
tbl_towns <- read_csv2(paste(output_data_dir, files[3], sep = ''))
dt_train <- fread(paste(input_dir, 'train.csv', sep = ''),
                  select = c('Semana','Agencia_ID','Cliente_ID','Producto_ID','Venta_uni_hoy','Dev_uni_proxima','Demanda_uni_equil'))
#Retirando o outlier observado no arquivo 01
dt_train <- dt_train[!Producto_ID == 3509]

#### Criação de variáveis ####
## Proporção de itens vendidos
dt_train[, Prop_Venta := Venta_uni_hoy/(Venta_uni_hoy + Dev_uni_proxima)]
summary(dt_train$Prop_Venta)
# Prop_Venta é qualquer range de valores entre 0 e 1, representando o aproveitamento do estoque solicitado:
# * Se 4 produtos são vendidos e 0 devolvidos, esta variável será 1
# * Se 0 produtos são vendidos e 4 devolvidos, esta variável será 0
# * Se 2 produtos são vendidos e 2 devolvidos, esta variável será 0.5
# Entretanto, a Prop_Venta falha ao representar a quantidade de produtos envolvidos

Product_Mean_Demand = function(P_ID){
  return(mean(dt_train[Producto_ID == P_ID]$Demanda_uni_equil))
}

Prop_Venta_Mean = function(P_ID){
  return(mean(dt_train[Producto_ID == P_ID]$Prop_Venta))
}

dt_products <- as.data.table(tbl_products)
dt_products[, c('PMD','PVM') := list(sapply(Producto_ID, Product_Mean_Demand),
                                 sapply(Producto_ID, Prop_Venta_Mean))]
dt_products
#TODO: Continuar daqui: Para cada produto, calculei a demanda média (PMD) e a proporção média de itens vendidos (PVM).
#A intenção destas variáveis é buscar um valor médio global de quantidades vendidas