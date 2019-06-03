library(tidyverse)
library(data.table)

#Configurando diretórios
setwd("/home/gustavo/Projects/kaggle_GrupoBimbo/")
input_dir = "/home/gustavo/Projects/kaggle_GrupoBimbo/input/"
output_dir = "/home/gustavo/Projects/kaggle_GrupoBimbo/output/"

#Selecionando arquivos
files <- list.files(input_dir, '[^zip_files]')
dimensions_dir = paste(input_dir, c(files[[1]], files[[2]], files[[5]]), sep = '')
train_dir = paste(input_dir, files[[6]], sep = '')
test_dir = paste(input_dir, files[[4]], sep = '')

#Carregando dados
dimensions <- lapply(dimensions_dir, read_csv)
train_sample <- read_csv(train_dir, n_max = 100000)
#test_sample <- read_csv(test_dir, n_max = 100000)

#### Distribuidoras ####
tbl_cities <- dimensions[[3]]
glimpse(tbl_cities)
## Cidades
#Todas as cidades possuem um código antes do nome. 
#Talvez seja parte do endereço, ou alguma informação extra a respeito do bairro?
#Observando alguns exemplos
tbl_cities %>%
  filter(grepl('tijuana', Town, ignore.case = T)) %>%
  arrange(Town)

tbl_cities %>%
  filter(grepl('ag.', Town, ignore.case = T)) %>%
  arrange(Town)

tbl_cities %>% 
  arrange(Town) %>% 
  select(Town) %>% 
  unique
#O número é sempre sequencial, iniciando em 2001. Acredito que possa representar a ordem na qual
#o grupo Bimbo instalou agências em cada cidade (primeiro em Atizapan, depois em Azcapotzalco, etc).
#Como não há variações no endereço da cidade, não farei nenhuma alteração neste campo
unique_cities <- tibble(Town = unique(tbl_cities$Town)) %>%
  rowid_to_column('Town_ID')

## Estados
tbl_cities %>% 
  arrange(State) %>% 
  select(State) %>% 
  unique

unique_states <- tibble(State = unique(tbl_cities$State)) %>%
  rowid_to_column('State_ID')

# Unindo as tabelas e mantendo apenas os identificadores
tbl_cities <- tbl_cities %>%
  inner_join(unique_cities, by = 'Town') %>%
  inner_join(unique_states, by = 'State') %>%
  select(Agencia_ID, Town_ID, State_ID)

#### Produtos ####
tbl_products <- dimensions[[2]]
glimpse(tbl_products)
n_distinct(tolower(tbl_products$NombreProducto))
#Nenhum produto parece estar cadastrado em duplicidade. Nenhuma ação será tomada nessa tabela

#### Análise exploratória ####
#Resumo estatístico
summary(train_sample)

#Unindo IDs de Estados e Cidades ao dataset
train_sample <- train_sample %>%
  inner_join(tbl_cities, by = 'Agencia_ID')

## Produtos mais vendidos
top_sales <- train_sample %>%
  filter(Venta_uni_hoy > 0 & Dev_uni_proxima < Venta_uni_hoy) %>%
  select(Producto_ID, Town_ID, State_ID, Demanda_uni_equil) %>%
  group_by(Producto_ID, Town_ID, State_ID) %>%
  summarize(Sales = sum(Demanda_uni_equil)) %>%
  ungroup

top_sales %>%
  group_by(Producto_ID) %>%
  summarize(Sales = sum(Sales)) %>%
  arrange(-Sales)
#Dentro do dataset de 100000 linhas, Os três produtos mais vendidos são: 1278, 1250 e então 35651
#Observando esse comportamento no nível estadual e regional
top_sales %>%
  filter(Producto_ID %in% c(1278, 1250, 35651)) %>%
  group_by(Producto_ID, Town_ID) %>%
  summarize(Sales = sum(Sales)) %>%
  arrange(-Sales)

top_sales %>%
  filter(Producto_ID %in% c(1278, 1250, 35651)) %>%
  group_by(Producto_ID, State_ID) %>%
  summarize(Avg = mean(Sales), Sales = sum(Sales)) %>%
  arrange(-Sales)
#Alguns produtos são mais populares em certos estados e cidades do México. Cada cliente possui sua clientela,
#entretando olhar os padrões de consumo regionais podem apresentar novas oportunidades de mercado. Farei alguns
#testes durante a criação do modelo preditivo adicionando estes IDs à fórmula.

## Carregando o dataset completo
#Para carregar todos os dados do dataset, precisarei usar data.table por ser mais veloz e mais eficiente no
#gerenciamento de memória de grandes datasets
dt_train = fread(train_dir, select = c('Semana','Agencia_ID','Cliente_ID','Producto_ID','Venta_uni_hoy','Dev_uni_proxima','Demanda_uni_equil'))

#Com todo o dataset carregado, vamos visualizar novamente os 100 produtos mais vendidos
products_top100 <- dt_train[Venta_uni_hoy > 0 & Dev_uni_proxima < Venta_uni_hoy, sum(Demanda_uni_equil), Producto_ID][order(-V1)][1:100]

#Curiosamente, alguns dos produtos que aparecem no top 100 foram observados como os mais vendidos no nosso dataset reduzido de 100.000 linhas.
#Aproveitando que o dataset "top_sales" possui IDs regionais, vamos observar a variabilidade regional destas vendas
inner_join(products_top100, top_sales, by = 'Producto_ID') %>%
  group_by(Producto_ID, V1) %>%
  summarize(Spread = IQR(Sales))
#Quanto maior o "Spread", maior a procura deste produto em alguns locais e menor em outros.
#Um "Spread" menor indica que a procura por aquele produto é semelhante em todas as regiões observadas
#Entretanto, um Spread igual a zero significa que aquele produto apareceu em apenas uma região, não havendo valores adicionais para comparação
#
#Por exemplo, o produto 1250 apresentou um IQR de 7226 após registrar registrou 20453 vendas na cidade 2 do estado 1,
#enquanto este mesmo produto registrou 6000 e 9006 vendas nas cidades 1 e 3 do estado 2, respectivamente.
#Isso demonstra que alguns produtos são mais procurados em certas regiões do México e menos procurados em outras

#Total de vendas por semana
dt_train[,sum(Venta_uni_hoy), by = Semana]
#Não parece existir um padrão de vendas por semanas pares ou ímpares. Há pouca variância na quantidade de produtos
#vendidos entre as semanas observadas, portanto, não criarei nenhuma variável a partir da semana para o modelo preditivo.

## Análise de produtos vendidos
#Resumo estatístico por semana
dt_train[,summary(Venta_uni_hoy), by = Semana]
#A maioria dos produtos é vendida em poucas unidades. Entretanto, para alguns produtos, parece existir uma demanda excessiva!
quantile(dt_train[,Venta_uni_hoy], seq(0, 100) * 0.01)

#Baseado nos quantis e resumo estatístico por semana, selecionei o valor de 4000 unidades vendidas para fazer a observação abaixo
dt_train[Venta_uni_hoy > 4000, .(Producto_ID, Venta_uni_hoy, Dev_uni_proxima, Demanda_uni_equil)]
#Esses valores extremos não se tratam de outliers ocasionais. De fato, há uma grande demanda por certos produtos.
#A solicitação de um cliente pelo produto 30916 é uma exceção. Foram solicitadas 7200 unidades, mas apenas 432 foram vendidas e 6768 foram devolvidas

products_venda4k <- dt_train[Venta_uni_hoy > 4000, Producto_ID]
#TODO: Continuar daqui
## Análise de produtos devolvidos

## Análise de produtos com grandes demandas