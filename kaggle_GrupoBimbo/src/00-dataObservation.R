library(tidyverse)

#Configurando diretórios
setwd("/home/gustavo/Projects/kaggle_GrupoBimbo/")
input_dir = "/home/gustavo/Projects/kaggle_GrupoBimbo/input/"

#Selecionando arquivos
files <- list.files(input_dir, '[^zip_files]')
# cliente_tabla - Tabela de Clientes
# producto_tabla - Tabela de produtos
# sample_submission - Template a ser enviado ao Kaggle
# test - Dataset de teste
# town_state - Tabela de fornecedores e suas respectivas cidades e estados de atuação no México
# train - Dataset de treino

#Carregando dataset
datasets <- lapply(paste(input_dir, files, sep = ''), read_csv, n_max = 100000)
View(datasets)

#Procurando valores NA
lapply(datasets, function(x){sum(is.na(x))})
#Todos os inputs estão devidamente preenchidos

#Observando os datasets carregados
mapply(glimpse, datasets)
# * Clientes - Todos os clientes cadastrados estão nessa tabela
# * Produtos - Todos os produtos fornecidos também estáo nessa tabela. Entretanto, percebi que o
#primeiro registro (Producto_ID 0, "NO IDENTIFICADO") parece ser um item genérico. Isso pode sugerir
#que o ID zero na verdade representa possíveis valores NA
# * Cidades - Todos os fornecedores e suas respectivas cidades e estados estão cadastrados aqui
# * Test/Train - Os dados a serem processados. 
#Dados que existem à disposição para o modelo preditivo:
# Semana: número da semana do ano, sugerindo que as entregas de produtos são semanais e não diárias
# Agencia_ID: ID do fornecedor que atende aquele cliente
# Canal_ID: ?
# Ruta_SAK: ?
# Cliente_ID: ID do cliente solicitante
# Producto_ID: ID do produto solicitado
#
#Dados adicionais disponíveis no dataset de treino para o algoritmo aprender as correlações entre os campos acima
# Venta_uni_hoy: Quantidade de itens vendidos
# Venta_hoy: Valor monetário obtido pelas vendas
# Dev_uni_proxima: Quantidade de itens a serem devolvidos na semana seguinte (não vendidos)
# Dev_proxima: Valor monetário das devoluções
# **Demanda_uni_equil: Demanda ajustada pelo produto: variável target a ser prevista**
#
#Meu objetivo neste projeto é prever as demandas por produtos de cada cliente,
#buscando maximizar as vendas (Venta_uni) e minimizar as devoluções (Dev_uni).

#### Observando as ocorrências em que os IDs de clientes e produtos são zero ####
##Premissa: Novos clientes e novos produtos ainda não existentes nos datasets serão
#Visualizar possíveis ocorrências de ID zero nas colunas de cliente e produto nos datasets de treino e teste
library(data.table)
lapply(list(files[[4]], files[[6]]), function(x){
  zeroId <- fread(input = paste(input_dir, x, sep = ''), select = c('Semana','Cliente_ID','Producto_ID'))
  print(zeroId[Cliente_ID == 0])
  print(zeroId[Producto_ID == 0])
})

#Não foram encontradas ocorrências dos IDs zero nos datasets de treino e teste, sugerindo que:
# * Todos os produtos solicitados e fornecidos podem ser localizados
# * Todos os clientes atendidos estão registrados
rm(zeroId)

#### Compreendendo os campos Canal_ID e Ruta_SAK ####
canal_rutaSak <- lapply(list(files[[4]], files[[6]]), function(x){
  fread(input = paste(input_dir, x, sep = ''), select = c('Semana','Canal_ID', 'Ruta_SAK'))
})
mapply(summary, canal_rutaSak)
#Os campos Canal_ID e Ruta_SAK representam identificadores que eu não pude associar intuitivamente
#A descrição destes campos na competição do Kaggle diz o seguinte:
# * Canal_ID: Sales Channel ID
# * Ruta_SAK: Route ID (Several routes = Sales Depot)
# A maioria das ocorrências de Canal_ID são '1', sugerindo um meio de comunicação abrangente entre os clientes e a empresa
# As ocorrências de Ruta_SAK são bastante variadas
rm(canal_rutaSak)

#### Conclusões e próximas etapas ####
#Esse projeto possui um grande volume de dados que estão muito bem normalizados, sendo possível realizar
#diversos cruzamentos e extrair insights interessantes.
#
#A próxima etapa será realizar uma análise mais profunda nestes dados e suas correlações. Algumas idéias que quero tentar implementar
# * Demanda por produtos em "múltiplos níveis": estadual (estado), regional (cidade), local (cliente)
#Insights que quero extrair
# * Demanda por produtos por "tipo" de semana: pares/ímpares e múltiplas de 4 (assumindo quatro semanas por mês).
# * Demanda por produtos por rota percorrida