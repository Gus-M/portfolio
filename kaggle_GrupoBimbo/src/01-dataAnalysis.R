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

#### Clientes ####
tbl_clients <- dimensions[[1]]
glimpse(tbl_clients)
# Apenas olhando os primeiros registros, podemos identificar dois clientes com nomes iguais: SIN NOMBRE (que significa Sem Nome)
# É possível que existam nomes iguais, mas que não necessariamente remetam a um mesmo cliente. Da mesma forma, creio ser possível
#que haja um mesmo cliente cadastrado em duplicidade.

n_distinct(tolower(tbl_clients$NombreCliente))
# Dos 935.362 registros de clientes, apenas 311.030 (33%) possuem nomes exclusivos

#### Produtos ####
tbl_products <- dimensions[[2]]
glimpse(tbl_products)
#Infelizmente, a única informação que temos a respeito dos produtos é seu ID e nome
n_distinct(tolower(tbl_products$NombreProducto))
#Nenhum produto parece estar cadastrado em duplicidade. Nenhuma ação será tomada nessa tabela

#### Distribuidoras ####
tbl_towns <- dimensions[[3]]
glimpse(tbl_towns)
## Cidades
#Todas as cidades possuem um código antes do nome. 
#Talvez seja parte do endereço, ou alguma informação extra a respeito do bairro?
#Observando alguns exemplos
tbl_towns %>%
  filter(grepl('tijuana', Town, ignore.case = T)) %>%
  arrange(Town)

tbl_towns %>%
  filter(grepl('ag.', Town, ignore.case = T)) %>%
  arrange(Town)

tbl_towns %>% 
  arrange(Town) %>% 
  select(Town) %>% 
  unique
#O número é sempre sequencial, iniciando em 2001. Acredito que possa representar a ordem na qual
#o grupo Bimbo instalou agências em cada cidade (primeiro em Atizapan, depois em Azcapotzalco, etc).
#Como não há variações no endereço da cidade, não farei nenhuma alteração neste campo
unique_cities <- tibble(Town = unique(tbl_towns$Town)) %>%
  rowid_to_column('Town_ID')

## Estados
tbl_towns %>% 
  arrange(State) %>% 
  select(State) %>% 
  unique

unique_states <- tibble(State = unique(tbl_towns$State)) %>%
  rowid_to_column('State_ID')

# Unindo as tabelas e mantendo apenas os identificadores
tbl_towns <- tbl_towns %>%
  inner_join(unique_cities, by = 'Town') %>%
  inner_join(unique_states, by = 'State') %>%
  select(Agencia_ID, Town_ID, State_ID)

#### Análise exploratória ####
#Resumo estatístico
summary(train_sample)

#Unindo IDs de Estados e Cidades ao dataset
train_sample <- train_sample %>%
  inner_join(tbl_towns, by = 'Agencia_ID')

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

# ------------------ #

dt_train[Producto_ID == 4053]
tbl_products %>% filter(Producto_ID == 4053)
# Não parece haver nada de diferente nestes registros. Vamos observar apenas os casos em que houve devolução
dt_train[Producto_ID == 4053 & Dev_uni_proxima > 0]
# Em todas as semanas, o cliente 4102610 efetuou devoluções deste produto sem ter solicitado ou vendido este produto diretamente
# Este mesmo cliente foi responsável por quase todos os registros de devolução deste produto
tbl_towns %>% 
  filter(Agencia_ID == 1345) %>%
  inner_join(dimensions[[3]], by = 'Agencia_ID')

tbl_clients %>% filter(Cliente_ID == 4102610)
# Sem identificação deste cliente

# ------------------ #

dt_train[Producto_ID == 30946]
tbl_products %>% filter(Producto_ID == 30946)
dt_train[Producto_ID == 30946 & Dev_uni_proxima > 0]
# Náo parece haver inconsistências nos registros deste produto. Precisamos observar mais detalhadamente o total de vendas e devoluções

dt_prod30946 <- dt_train[Producto_ID == 30946 & Dev_uni_proxima > 0,
         .(sumVenta = sum(Venta_uni_hoy), sumDevol = sum(Dev_uni_proxima)),
         by = Cliente_ID]
# Observando registros em que houveram pelo menos uma venda do produto 30946
dt_prod30946[sumVenta > 0, sapply(.(sumVenta, sumDevol), sum)]
# Quando observamos as devoluções dos registros em que houveram vendas, a discrepância diminui consideravelmente
dt_prod30946[sumVenta > 0][order(-sumVenta)]

# Observando registros em que ocorreram devoluções e nenhuma venda
dt_prod30946[sumVenta == 0, sapply(.(sumVenta, sumDevol), sum)]
dt_prod30946[sumVenta == 0][order(-sumDevol)]
# A partir das análises de produtos devolvidos, é possível concluir que existem clientes apenas para devolução de produtos?
# Farei algumas observações nos cinco primeiros clientes apresentados nesta lista: 923622, 132637 e 60629

dt_train[Cliente_ID == 923622]
# As devoluções do produto 30946 ocorreram na semana 3, o que fortalece a hipótese de que os dados disponibilizados 
#neste dataset estão incompletos, pois não podemos apurar a demanda por este (e outros) produto nas semanas anteriores
# Além do produto 30946, este cliente não teve dificuldade em vender os demais produtos solicitados.

dt_train[Cliente_ID == 132637]
# Este cliente teve dificuldades em vender diversos produtos. 
# Houveram devoluções do item 30946 nas semanas 3, 4, 5 e 6. Nenhuma unidade deste produto foi vendida neste período.

dt_train[Cliente_ID == 60629]
# Semelhante ao cliente 923622, foram observadas devoluções do produto 30946 e nenhuma venda realizada ao longo das semanas 3, 4 e 5.

# ------------------ #

dt_train[Producto_ID == 36441]
tbl_products %>% filter(Producto_ID == 36441)
# Foram identificados 51 registros deste produto, sendo que apenas 5 houveram devoluções e nenhuma venda.

dt_train[Producto_ID == 36441 & Dev_uni_proxima > 0]
# A grande maioria das devoluções deste produto estão concentradas no cliente 1050905, ao longo das semanas 3, 4 e 5
tbl_clients %>% filter(Cliente_ID == 1050905)

# Observando as outras demandas deste mesmo cliente
dt_train[Cliente_ID == 1050905]
# O Cliente 1050905 realiza vendas em larga escala. O produto 36441 foi devolvido ao longo das semanas 3, 4 e 5, sem nenhuma venda registrada

#### Conclusões e próximas etapas ####
# * Com exceção do produto 3509 em que houve um único registro de 250.000 devoluções e 0 vendas, os dados fornecidos parecem consistentes.
#Por se tratar de um outlier extremo, este registro será retirado do modelo preditivo.
# * As variáveis que elaborei (Percentual de vendas em relação a devoluções, Sales_Stdev, Town ID e State ID) auxiliaram na compreensão dos dados.
#Pretendo expandir estas variáveis no próximo documento, aplicando-as na prática.
# * Não esclareci minhas hipóteses mencionadas nas linhas 172 a 174. Assumirei que estes registros estão consistentes

# No próximo arquivo, farei transformações nos dados para criar novas variáveis e observar na prática algumas idéias adquiridas nos estudos deste arquivo
# Logo após, iniciarei a construção do modelo preditivo.

## Exportando as dimensões transformadas neste arquivo:
write_csv2(tbl_towns, paste(output_dir, 'tbl_towns.csv', sep = ''))
write_csv2(tbl_clients, paste(output_dir, 'tbl_clients.csv', sep = ''))
write_csv2(tbl_products, paste(output_dir, 'tbl_products.csv', sep = ''))
#Limpando o ambiente de trabalho
rm(list = ls())