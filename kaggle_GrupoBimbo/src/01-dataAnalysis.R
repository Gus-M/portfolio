library(tidyverse)
library(data.table)
library(reshape2)
library(scales)
library(corrplot)

#Configurando diretórios
cur_dir = rstudioapi::getActiveProject()
input_dir = paste(cur_dir, 'input', sep = '/')

#Selecionando arquivos
files <- list.files(input_dir, '.csv')

#### Produtos ####
tbl_products <- read_csv(paste(input_dir, files[2], sep = '/'))
glimpse(tbl_products)
#Cada produto possui algumas informações adicionais no nome
# * Peso do produto por gramas, kilogramas, litros ou mililitros
# * Quantidade de peças por unidade

#As informações do peso e unidades de cada produto serão extraídas com Regex
extract_from_product_name <- function(tib){
  pattern_metric_weight <- '\\d+\\s*g|\\d+\\s*kg|\\d+\\s*ml|\\d+\\s*l'
  pattern_numeric_amount <- '\\d+p'
  pattern_product_first_name <- '^\\w+'
  return(tib %>%
           mutate(product_weight = tolower(str_extract(NombreProducto, regex(pattern_metric_weight, ignore_case = T))),
                  product_units = str_extract(NombreProducto, regex(pattern_numeric_amount, ignore_case = T)),
                  product_units = as.numeric(str_extract(product_units, '\\d*')),
                  product_first_name = str_extract(NombreProducto, pattern_product_first_name)))
}

#Função para padronizar o peso do produto em gramas e mililitros
standardize_weight = function(tib){
  metrics = data.frame(measure = c('g', 'kg', 'ml', 'l'),
                       value = c(1, 1000, 1, 1000))
  
  return(tib %>%
           mutate(measure = str_extract(product_weight, '[:alpha:]+'),
                  product_weight = as.numeric(str_extract(product_weight, '\\d*'))) %>%
           left_join(metrics, by = 'measure') %>%
           mutate(product_weight = product_weight * value) %>%
           select(-measure, -value))
}

#Valores NA na coluna 'product_units' serão substituídos por 1, indicando que o produto é unitário
impute_constant_unit = function(tib){
  return(tib %>% mutate(product_units = replace_na(product_units, 1)))
}

#Valores NA na coluna 'product_weight' serão substituídos pela média de seus grupos
impute_group_median_weight = function(tib){
  
  #Criamos um array com os produtos de valor missing
  na_weight_types <- tib %>%
    filter(is.na(product_weight)) %>%
    group_by(product_first_name) %>%
    summarize %>%
    flatten_chr
  
  #Para cada grupo, calculamos a mediana de peso de itens semelhantes
  df_median <- tib %>%
    filter(product_first_name %in% na_weight_types) %>%
    group_by(product_first_name) %>%
    summarize(median_weight = median(product_weight, na.rm = T)) %>%
    as.data.frame
  
  #E então, substituímos os valores NA a partir das medianas calculadas por grupo
  tib_list <- bind_rows(apply(df_median, 1, function(x){
    tib %>%
      filter(product_first_name == x[1]) %>%
      mutate(product_weight = replace_na(product_weight, as.numeric(x[2])))
  }))
  
  #Colocamos os valores imputados de volta no tibble inicial
  imputed_tbl <- left_join(tib, tib_list, by = 'Producto_ID') %>%
    mutate(product_weight.x = ifelse(is.na(product_weight.x), product_weight.y, product_weight.x)) %>%
    transmute(Producto_ID = Producto_ID,
              NombreProducto = NombreProducto.x,
              product_weight = product_weight.x,
              product_units = product_units.x)
  return(imputed_tbl)
}

#É provável que não seja possível imputar a mediana do peso do grupo em casos específicos, onde não há
#amostras suficientes para calcular a mediana. 
#Nessas situações, será imputado o peso médio
impute_mean_weight = function(tib){
  overall_mean_weight = round(mean(tib$product_weight, na.rm = T))
  return(tib %>% mutate(product_weight = replace_na(product_weight, overall_mean_weight)))
}

#Colocamos as etapas em um pipeline...
product_pipeline = function(tib){
  steps = extract_from_product_name(tib) %>%
    standardize_weight %>%
    impute_constant_unit %>%
    impute_group_median_weight %>%
    impute_mean_weight
  return(steps)
}

#...que será utilizado para extrair essas features da tabela de produtos
tbl_product_info <- product_pipeline(tbl_products)

#### Clientes ####
tbl_clients <- read_csv(paste(input_dir, files[1], sep = '/'))
glimpse(tbl_clients)
#Observando os primeiros registros de clientes, podemos perceber algumas inconsistências:
# * Existem registros duplicados, como o ID 4
# * Existem clientes não identificados, cujos nomes foram preenchidos com 'SIN NOMBRE' (sem nome)

duplicated_clients <- tbl_clients %>%
  count(Cliente_ID) %>%
  filter(n > 1) %>%
  select(Cliente_ID) %>%
  flatten_dbl
#Dos 935.362 clientes, 4.862 aparecem duplicados. Felizmente é uma pequena parcela dos registros

#Precisamos observar se os registros duplicados referem-se ao mesmo cliente cadastrado mais de uma vez
#ou a clientes distintos compartilhando o mesmo ID
tbl_duplicated_clients <- tbl_clients %>% filter(Cliente_ID %in% duplicated_clients)

#As primeiras linhas sugerem a primeira hipótese: os registros duplicados referem-se ao mesmo cliente
pattern_trim_whitespace = '\\s+'
pattern_replace_punct = '[:punct:]+'
tbl_duplicated_clients <- tbl_duplicated_clients %>%
  mutate(eval_client_name = str_replace_all(NombreCliente, pattern_trim_whitespace, '_'),
         eval_client_name = str_replace_all(eval_client_name, pattern_replace_punct, '_')) %>%
  group_by(Cliente_ID, eval_client_name) %>% summarize %>% ungroup

#Retirando espaços e caracteres especiais, foram encontrados 4863 registros. Isso sugere que,
#dos 4862 registros, apenas 1 foi duplicado representando clientes distintos, enquanto os
#demais 4861 registros representam o mesmo cliente cadastrado repetidamente
duplicated_distinct_clients <- tbl_duplicated_clients %>%
  count(Cliente_ID) %>%
  filter(n > 1) %>%
  select(Cliente_ID)

#Verificando os clientes distintos:
tbl_clients %>%
  filter(Cliente_ID %in% duplicated_distinct_clients$Cliente_ID)
#O cliente 1646352 possui dois nomes: 'SIN NOMBRE' e 'LA CHIQUITA'. Sabendo que 'SIN NOMBRE' é a ocorrência
#genérica para quando o nome do cliente não pode ser apurado, podemos deduzir a possibilidade deste registro
#tratar-se do mesmo cliente.

#Visto que os registros duplicados referem-se ao mesmo cliente, nenhuma ação será tomada nestes dados.
#Se necessário, podemos remover uma das linhas duplicadas sem perder informação.

#### Cidades ####
tbl_towns <- read_csv(paste(input_dir, files[5], sep = '/'))
glimpse(tbl_towns)

#Cidades e Estados serão agrupados numericamente
encode_towns = function(tib){
  return(tib %>%
           mutate(Town_ID = as.numeric(factor(Town)),
                  State_ID = as.numeric(factor(State))) %>%
           select(Agencia_ID, Town_ID, State_ID))
}
encode_towns(tbl_towns)

#### Dados de treino ####
system('wc -l input/train.csv')
#Existem aproximadamente 74 milhões de registros históricos, entretanto, não precisamos de todos estes dados para análise.

#Vamos carregar o dataset completo usando Data Table. A partir deste, separamos a quantidade de semanas disponíveis
#e extraímos algumas amostras a partir de cada semana:
tbl_train <- fread(paste(input_dir, files[6], sep = '/'))
week_range <- range(tbl_train$Semana)
week_sequence <- seq(week_range[1], week_range[2], 1)

set.seed(3)
train_sample <- bind_rows(lapply(week_sequence, function(x){
  tbl_train[Semana == x][sample(.N, 500000)]
})) %>% as_tibble
rm(tbl_train)

#Análise estatística
summary(train_sample)
#A variável que estamos tentando prever é "Demanda_uni_equil", que representa 
#"Venta_uni_hoy" (produtos vendidos) - "Dev_uni_proxima" (produtos que serão devolvidos na próxima semana).
#Há uma variação significativa na demanda. Sob uma visão mais abrangente, é esperado vender 7.2 produtos por requisição.

#Porém, não podemos generalizar essa média para todos os casos:
train_sample %>%
  filter(Demanda_uni_equil == 3920)
#Neste exemplo, foram vendidos 3920 unidades do produto 2604. É a ocorrência mais extrema deste conjunto de dados.
#Será necessário observar, a partir de cada ID, quais variáveis melhor representam a disparidade na demanda por produtos.

hist(train_sample$Demanda_uni_equil)
#Grande parte das requisições se concentram entre os registros próximos da média de 7.2 produtos.
#Dando um zoom neste intervalo:
ggplot(data = train_sample %>% filter(Demanda_uni_equil <= 10)) +
  geom_histogram(mapping = aes(x = Demanda_uni_equil),
                 bins = 10, fill = '#04691d', color = 'black',) +
  scale_x_continuous(breaks = 1:10) +
  ylab('Ocorrências') + xlab('Demanda') + 
  ggtitle('Distribuição de registros (Demanda <= 10)')
#A maior parte das demandas estão concentradas entre 1 e 5 produtos.

#### Feature Engineering ####
## Semana
plotdata_semana <- train_sample %>%
  group_by(Semana) %>%
  summarize(sum_venta = sum(Venta_uni_hoy, na.rm = T),
            sum_devol = sum(Dev_uni_proxima, na.rm = T),
            sum_demanda = sum(Demanda_uni_equil, na.rm = T))

#Vendas por semana
ggplot(data = plotdata_semana) +
  geom_col(mapping = aes(x = as.factor(Semana),
                         y = sum_venta),
           fill = '#328cad', width = 0.65) +
  scale_y_continuous(labels = comma) +
  xlab('Semana') + ylab('Vendas') + theme_light() +
  ggtitle('Vendas por semana')

#Devoluções por semana
ggplot(data = plotdata_semana) +
  geom_col(mapping = aes(x = as.factor(Semana),
                         y = sum_devol),
           fill = '#ad3232', width = 0.65) +
  scale_y_continuous(labels = comma) +
  xlab('Semana') + ylab('Devoluções') + theme_light() +
  ggtitle('Devoluções por semana')

#Demanda por semana
ggplot(data = plotdata_semana) +
  geom_col(mapping = aes(x = as.factor(Semana),
                         y = sum_demanda),
           fill = '#32ad32', width = 0.65) +
  scale_y_continuous(labels = comma) +
  xlab('Semana') + ylab('Demanda') + theme_light() +
  ggtitle('Demanda por semana')
#No período observado, não há variação suficiente na demanda semanal para justificar o uso dessa variável.
#É possível observar um aumento no volume de devoluções a cada 4 semanas, sem variação suficiente para
#justificar a criação de variáveis a partir deste valor.
#A variação percebida nas devoluções das semanas 5, 6, 7 e 8 sugerem um padrão comportamental dos clientes.
#Durante o treinamento do modelo preditivo, serão coletadas amostras referentes a estas semanas

## Canal_ID
#Registros por canal
plotdata_channel <- train_sample %>%
  count(Canal_ID, name = 'count_channel') %>%
  mutate(prop = (count_channel / sum(count_channel)) * 100)

ggplot(data = plotdata_channel,
       mapping = aes(x = reorder(as.factor(Canal_ID), count_channel),
                     y = count_channel,
                     fill = as.factor(Canal_ID))) +
  geom_col(color = '#777777', show.legend = F) +
  geom_text(mapping = aes(label = sprintf('%.2f%%', prop))) +
  scale_y_continuous(labels = comma) +
  coord_flip() + theme_light() +
  xlab('Canal') + ylab('Registros') +
  ggtitle('Registros por canal de comunicação')
#90.9% das requisições por produtos são realizadas pelo canal 1, seguido pelo canal 4 com 5%. 
#Isso sugere que o canal 1 é a opção mais acessível para os clientes registrarem suas solicitações.
#Os demais canais representam menos de 2% das solicitações, sendo os canais 8 e 9 os menos utilizados.

#Observada a quantidade de registros por canal, precisamos observar o volume de demandas
ggplot(data = train_sample %>% filter(Demanda_uni_equil > 0)) +
  geom_boxplot(mapping = aes(x = as.factor(Canal_ID), 
                             y = log(Demanda_uni_equil),
                             fill = as.factor(Canal_ID)),
               show.legend = F) +
  coord_flip() + theme_light() +
  xlab('Canal') + ylab('Demanda (log)') +
  ggtitle('Demandas por canal de comunicação')
#Este gráfico sugere que, em média, as demandas mais volumosas são atendidas pelos canais 5, 9 e 2.
#Ao mesmo tempo, é possível observar outliers em todos canais de comunicação, 
#sugerindo que não há um canal de comunicação exclusivo para grandes demandas de produto.
#Será calculada a média de demandas por canal. Posteriormente, será analisada a correlação com a variável target
tbl_channel_demanda <- train_sample %>%
  group_by(Canal_ID) %>%
  summarize(mean_channel_demanda = mean(Demanda_uni_equil, na.rm = T)) %>%
  ungroup

## Agencia_ID
#É possível extrair diversas informações a partir do ID da agência:
plotdata_depot <- train_sample %>%
  group_by(Agencia_ID) %>%
  summarize(mean_depot_demanda = mean(Demanda_uni_equil),
            sum_depot_demanda = sum(Demanda_uni_equil)) %>%
  left_join(train_sample %>% count(Agencia_ID, name = 'count_agencia'), by = 'Agencia_ID') %>%
  left_join(encode_towns(tbl_towns), by = 'Agencia_ID')


ggplot(data = plotdata_depot,
       mapping = aes(x = count_agencia,
                     y = sum_depot_demanda)) + geom_point() +
  xlab('Quantidade de registros') + ylab('Total de demanda') + theme_light() +
  ggtitle('Proporção de demandas por registros')
#É esperada uma correlação positiva entre "quantidade de registros" e "total de demandas". Este gráfico 
#nos auxilia a perceber alguns outliers: depósitos com baixo volume de registros e alto volume de demandas
large_scale_depot_mean_threshold = quantile(plotdata_depot$mean_depot_demanda, 0.75)
large_scale_depot_count_threshold = quantile(plotdata_depot$count_agencia, 0.5)
plotdata_depot <- plotdata_depot %>% 
  mutate(large_scale_depot = as.numeric(mean_depot_demanda >= large_scale_depot_mean_threshold &
                                          count_agencia >= large_scale_depot_count_threshold))

ggplot(data = plotdata_depot,
       mapping = aes(x = count_agencia,
                     y = sum_depot_demanda)) + 
  geom_point(mapping = aes(color = as.factor(large_scale_depot)),
             show.legend = F) +
  scale_color_manual(values = c('#c5dbc5', '#32ad32')) + 
  xlab('Quantidade de registros') + ylab('Total de demanda') + theme_light() +
  ggtitle('Proporção de demandas por registros')

plotdata_depot %>%
  filter(large_scale_depot == 1) %>%
  arrange(-mean_depot_demanda)
#Em média, é esperado que solicitações nestas distribuidoras sejam maiores que o usual

#A partir da localização das agências, podemos extrair informações de demandas por cidade e estado
plotdata_town <- plotdata_depot %>%
  select(sum_depot_demanda, count_agencia, Town_ID) %>%
  group_by(Town_ID) %>%
  summarize(sum_town_demanda = sum(sum_depot_demanda),
            count_town_demanda = sum(count_agencia),
            mean_town_demanda = sum_town_demanda/count_town_demanda) %>% ungroup

ggplot(data = plotdata_town %>% top_n(50, sum_town_demanda),
       mapping = aes(x = reorder(as.factor(Town_ID), -sum_town_demanda),
                     y = sum_town_demanda)) +
  geom_col(color = '#777777', fill = '#32ad32') +
  scale_y_continuous(labels = comma) + theme_light() +
  xlab('ID Cidade') + ylab('Demanda total') +
  ggtitle('Demanda total por cidade - Top 50')
#A demanda total por cidade auxilia a perceber as cidades com maior demanda por produtos
depots_by_town <- plotdata_depot %>%
  count(Town_ID, name = 'num_depots')

ggplot(data = plotdata_town %>% 
         top_n(50, sum_town_demanda) %>%
         left_join(depots_by_town, by = 'Town_ID') %>%
         select(Town_ID, sum_town_demanda, num_depots) %>% melt(id = 'Town_ID'),
       mapping = aes(x = reorder(as.factor(Town_ID), -value),
                     y = value)) +
  geom_col(color = '#777777', fill = '#32ad32') +
  scale_y_continuous(labels = comma) + theme_light() +
  facet_wrap(facets = ~ variable,
             scales = 'free_y',
             nrow = 2, ncol = 1) +
  xlab('ID Cidade') + ylab('Demanda total') +
  ggtitle('Demanda total e quantidade de distribuidoras por cidade - Top 50')

ggplot(data = plotdata_town %>% 
         top_n(-50, sum_town_demanda) %>%
         left_join(depots_by_town, by = 'Town_ID') %>%
         select(Town_ID, sum_town_demanda, num_depots) %>% melt(id = 'Town_ID'),
       mapping = aes(x = reorder(as.factor(Town_ID), -value),
                     y = value)) +
  geom_col(color = '#777777', fill = '#32ad32') +
  scale_y_continuous(labels = comma) + theme_light() +
  facet_wrap(facets = ~ variable,
             scales = 'free_y',
             nrow = 2, ncol = 1) +
  xlab('ID Cidade') + ylab('Demanda total') +
  ggtitle('Demanda total e quantidade de distribuidoras por cidade - Bottom 50')
#As cidades com maiores demandas possuem mais distribuidoras, ao passo que cidades com menor demanda possuem menos distribuidoras.

## Cliente_ID
#Podemos calcular a demanda total e média de cada cliente para identificar clientes de larga escala
plotdata_client <- train_sample %>%
  group_by(Cliente_ID) %>%
  summarize(mean_demanda_cliente = mean(Demanda_uni_equil),
            sum_demanda_cliente = sum(Demanda_uni_equil)) %>%
  left_join(tbl_clients, by = 'Cliente_ID')
summary(plotdata_client)

ggplot(data = plotdata_client %>%
         top_n(20, sum_demanda_cliente) %>%
         select(-Cliente_ID) %>% melt(id = 'NombreCliente'),
       mapping = aes(x = reorder(as.factor(NombreCliente), value),
                     y = value)) +
  geom_col(color = '#e7e7e7', fill = '#dff0df') + 
  geom_text(mapping = aes(label = sprintf('%.0f', value))) + 
  coord_flip() + theme_light() +
  facet_wrap(facets = ~ variable,
             scales = 'free_x',
             nrow = 1, ncol = 2) +
  xlab('Clientes') + ylab('Demanda média / Demanda total') +
  ggtitle('Demanda total por cliente - Top 20')
#Puebla Remision é o principal cliente, com aproximadamente 860.000 produtos vendidos neste conjunto, enquanto o segundo 
#maior cliente possui um volume de vendas de aproximadamente 39.000.
#Mesmo com este volume significativo, a média de produtos por demanda está alinhada com os demais clientes, entre 140 e 160.

#A discrepância na demanda média observada neste gr áfico sugere que exista uma alta demanda por uma pequena variedade de produtos,
#enquanto os valores mais próximos da média sugerem uma demanda regular por uma variedade maior de produtos.

## Producto_ID
plotdata_product <- train_sample %>%
  group_by(Producto_ID) %>%
  summarize(mean_producto_demanda = mean(Demanda_uni_equil),
            sum_producto_demanda = sum(Demanda_uni_equil)) %>%
  left_join(product_pipeline(tbl_products), by = 'Producto_ID')
summary(plotdata_product)

ggplot(data = plotdata_product,
       mapping = aes(x = product_weight,
                     y = mean_producto_demanda)) +
  geom_point() + theme_light() +
  xlab('Peso do produto') + ylab('Demanda média') +
  ggtitle('Demanda média por peso do produto')
#Não parece haver uma correlação entre a demanda e o peso dos produtos, entretando é possível perceber que
#produtos mais pesados possuem uma demanda menor.
#A maioria dos produtos no catálogo possuem um peso inferior a 5kg, colocando os produtos em um mesmo patamar.

# Produtos e clientes
product_by_client <- train_sample %>%
  group_by(Cliente_ID) %>%
  summarize(unique_product_count = n_distinct(Producto_ID))

ggplot(data = product_by_client %>% right_join(plotdata_client, by = 'Cliente_ID'),
       mapping = aes(x = unique_product_count,
                     y = mean_demanda_cliente)) +
  geom_point() + theme_light() + 
  xlab('Variedade de produtos') + ylab('Demanda média') +
  ggtitle('Demanda média por variedade de produtos de cliente')
#É esperado atender uma demanda menor à medida que o cliente solicita uma variedade maior de produtos.

#Outliers - grande variedade de produtos
product_by_client %>%
  right_join(plotdata_client, by = 'Cliente_ID') %>%
  filter(unique_product_count > 50)

#Outliers - grande demanda por produtos
product_by_client %>%
  right_join(plotdata_client, by = 'Cliente_ID') %>%
  filter(mean_demanda_cliente > 2000)