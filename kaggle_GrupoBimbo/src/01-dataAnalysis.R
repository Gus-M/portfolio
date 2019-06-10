library(tidyverse)
library(data.table)

#Configurando diretórios
cur_dir = rstudioapi::getActiveProject()
input_dir = paste(cur_dir, 'input/', sep = '/')
output_dir = paste(cur_dir, 'output/', sep = '/')

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
  summarize(Sales_stdev = sd(Sales))

#Total de vendas por semana
dt_train[,sum(Venta_uni_hoy), by = Semana]
#Não parece existir um padrão de vendas por semanas pares ou ímpares. Há pouca variância na quantidade de produtos
#vendidos entre as semanas observadas, portanto, não criarei nenhuma variável a partir da semana para o modelo preditivo.

## Análise de produtos vendidos
#Resumo estatístico por semana
dt_train[,summary(Venta_uni_hoy), by = Semana]
#Interpretação do output
#1- min
#2- 1o Quartil
#3- mediana
#4- média
#5- 3o Quartil
#6- max
#A maioria dos produtos é vendida em poucas unidades. Entretanto, para alguns produtos, parece existir uma demanda excessiva!
quantile(dt_train[,Venta_uni_hoy], seq(0, 100) * 0.01)

#Baseado nos quantis e resumo estatístico por semana, selecionei o valor de 4000 unidades vendidas para fazer a observação abaixo
dt_train[Venta_uni_hoy > 4000, .(Producto_ID, Venta_uni_hoy, Dev_uni_proxima, Demanda_uni_equil)]
#Esses valores extremos não se tratam de outliers ocasionais. De fato, há uma grande demanda por certos produtos.
#A solicitação de um cliente pelo produto 30916 é uma exceção. Foram solicitadas 7200 unidades, mas apenas 432 foram vendidas e 6768 foram devolvidas
products_venda4k <- dt_train[Venta_uni_hoy > 4000, Producto_ID]

## Análise de produtos devolvidos
#Resumo estatístico por semana
dt_train[, summary(Dev_uni_proxima), by = Semana]
#Interpretação do output
#1- min
#2- 1o Quartil
#3- mediana
#4- média
#5- 3o Quartil
#6- max
quantile(dt_train[,Dev_uni_proxima], seq(0, 100) * 0.01)

#Observando as 100 maiores ocorrências de devolução e demanda
dt_devol100 <- dt_train[, .(Venta = sum(Venta_uni_hoy),
             Devol = sum(Dev_uni_proxima),
             Demanda = sum(Demanda_uni_equil)), by = Producto_ID][,Perc_Vendido := (1-(Devol/Venta)) * 100][order(-Devol)][1:100]
dt_devol100
#Parece existir algumas inconsistências não observadas anteriormente. Alguns produtos possuem mais devoluções do que solicitações,
#como observado no produto 4053 onde foram solicitadas 18748 unidades, devolvidas 35959, resultando numa demanda de 18745 (?)
#Não deveria ser possível existir mais devoluções do que solicitões. Este produto parece ser um bom candidato para uma análise mais específica
#Algumas possibilidades que justificam este valor de demanda
# * Há devolução de produtos solicitados num período anterior à Semana 3
# * Os clientes fazem devolução de produtos oriundos de outras fontes não registradas neste dataset
# * Nem todas as solicitações foram corretamente cadastradas nesse dataset
dt_devol100[Devol >= Venta]

#Vamos observar estes produtos detalhadamentes
dt_train[Producto_ID == 3509]
tbl_products %>% filter(Producto_ID == 3509)
# O produto 3509 efetuou a devolução de 250.000 itens apesar de não haver qualquer registro de solicitação deste produto no dataset

dt_train[Producto_ID == 4053]
tbl_products %>% filter(Producto_ID == 4053)
# Não parece haver nada de diferente nestes registros. Vamos observar apenas os casos em que houve devolução
dt_train[Producto_ID == 4053 & Dev_uni_proxima > 0]
# Em todas as semanas, o cliente 4102610 efetuou devoluções deste produto sem ter solicitado ou vendido este produto diretamente
# Este mesmo cliente foi responsável por quase todos os registros de devolução deste produto
tbl_cities %>% filter(Agencia_ID == 1345)
dimensions[[3]] %>% filter(Agencia_ID == 1345)
dimensions[[1]] %>% filter(Cliente_ID == 4102610)
# Sem identificação deste cliente

dt_train[Producto_ID == 30946]
tbl_products %>% filter(Producto_ID == 30946)
dt_train[Producto_ID == 30946 & Dev_uni_proxima > 0]
# TODO: Continuar daqui. Semelhante ao produto 3509, este também possui registros de devolução sem antes haver pedidos de solicitação

dt_train[Producto_ID == 36441]
tbl_products %>% filter(Producto_ID == 36441)

#dt_train[Cliente_ID == 653378 & Producto_ID == 45296, sapply(.(Venta_uni_hoy, Dev_uni_proxima, Demanda_uni_equil), sum)]
#dt_train[Cliente_ID == 653378 & Producto_ID == 3509, sapply(.(Venta_uni_hoy, Dev_uni_proxima, Demanda_uni_equil), sum)]

## Análise de produtos com grandes demandas