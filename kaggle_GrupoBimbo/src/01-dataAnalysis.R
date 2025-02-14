library(tidyverse)
library(data.table)
library(reshape2)
library(scales)
library(corrplot)

#Configurando diret�rios
cur_dir = rstudioapi::getActiveProject()
input_dir = paste(cur_dir, 'input', sep = '/')

#Selecionando arquivos
files <- list.files(input_dir, '.csv')

#### Produtos ####
tbl_products <- read_csv(paste(input_dir, files[2], sep = '/'))
glimpse(tbl_products)
#Cada produto possui algumas informa��es adicionais no nome
# * Peso do produto por gramas, kilogramas, litros ou mililitros
# * Quantidade de pe�as por unidade

#As informa��es do peso e unidades de cada produto ser�o extra�das com Regex
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

#Fun��o para padronizar o peso do produto em gramas e mililitros
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

#Valores NA na coluna 'product_units' ser�o substitu�dos por 1, indicando que o produto � unit�rio
impute_constant_unit = function(tib){
  return(tib %>% mutate(product_units = replace_na(product_units, 1)))
}

#Valores NA na coluna 'product_weight' ser�o substitu�dos pela m�dia de seus grupos
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
  
  #E ent�o, substitu�mos os valores NA a partir das medianas calculadas por grupo
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

#� prov�vel que n�o seja poss�vel imputar a mediana do peso do grupo em casos espec�ficos, onde n�o h�
#amostras suficientes para calcular a mediana. 
#Nessas situa��es, ser� imputado o peso m�dio
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

#...que ser� utilizado para extrair essas features da tabela de produtos
tbl_product_info <- product_pipeline(tbl_products)

#### Clientes ####
tbl_clients <- read_csv(paste(input_dir, files[1], sep = '/'))
glimpse(tbl_clients)
#Observando os primeiros registros de clientes, podemos perceber algumas inconsist�ncias:
# * Existem registros duplicados, como o ID 4
# * Existem clientes n�o identificados, cujos nomes foram preenchidos com 'SIN NOMBRE' (sem nome)

duplicated_clients <- tbl_clients %>%
  count(Cliente_ID) %>%
  filter(n > 1) %>%
  select(Cliente_ID) %>%
  flatten_dbl
#Dos 935.362 clientes, 4.862 aparecem duplicados. Felizmente � uma pequena parcela dos registros

#Precisamos observar se os registros duplicados referem-se ao mesmo cliente cadastrado mais de uma vez
#ou a clientes distintos compartilhando o mesmo ID
tbl_duplicated_clients <- tbl_clients %>% filter(Cliente_ID %in% duplicated_clients)

#As primeiras linhas sugerem a primeira hip�tese: os registros duplicados referem-se ao mesmo cliente
pattern_trim_whitespace = '\\s+'
pattern_replace_punct = '[:punct:]+'
tbl_duplicated_clients <- tbl_duplicated_clients %>%
  mutate(eval_client_name = str_replace_all(NombreCliente, pattern_trim_whitespace, '_'),
         eval_client_name = str_replace_all(eval_client_name, pattern_replace_punct, '_')) %>%
  group_by(Cliente_ID, eval_client_name) %>% summarize %>% ungroup

#Retirando espa�os e caracteres especiais, foram encontrados 4863 registros. Isso sugere que,
#dos 4862 registros, apenas 1 foi duplicado representando clientes distintos, enquanto os
#demais 4861 registros representam o mesmo cliente cadastrado repetidamente
duplicated_distinct_clients <- tbl_duplicated_clients %>%
  count(Cliente_ID) %>%
  filter(n > 1) %>%
  select(Cliente_ID)

#Verificando os clientes distintos:
tbl_clients %>%
  filter(Cliente_ID %in% duplicated_distinct_clients$Cliente_ID)
#O cliente 1646352 possui dois nomes: 'SIN NOMBRE' e 'LA CHIQUITA'. Sabendo que 'SIN NOMBRE' � a ocorr�ncia
#gen�rica para quando o nome do cliente n�o pode ser apurado, podemos deduzir a possibilidade deste registro
#tratar-se do mesmo cliente.

#Visto que os registros duplicados referem-se ao mesmo cliente, nenhuma a��o ser� tomada nestes dados.
#Se necess�rio, podemos remover uma das linhas duplicadas sem perder informa��o.

#### Cidades ####
tbl_towns <- read_csv(paste(input_dir, files[5], sep = '/'))
glimpse(tbl_towns)

#Cidades e Estados ser�o agrupados numericamente
encode_towns = function(tib){
  return(tib %>%
           mutate(Town_ID = as.numeric(factor(Town)),
                  State_ID = as.numeric(factor(State))) %>%
           select(Agencia_ID, Town_ID, State_ID))
}
encode_towns(tbl_towns)

#### Dados de treino ####
system('wc -l input/train.csv')
#Existem aproximadamente 74 milh�es de registros hist�ricos, entretanto, n�o precisamos de todos estes dados para an�lise.

#Vamos carregar o dataset completo usando Data Table. A partir deste, separamos a quantidade de semanas dispon�veis
#e extra�mos algumas amostras a partir de cada semana:
tbl_train <- fread(paste(input_dir, files[6], sep = '/'))
week_range <- range(tbl_train$Semana)
week_sequence <- seq(week_range[1], week_range[2], 1)

set.seed(3)
train_sample <- bind_rows(lapply(week_sequence, function(x){
  tbl_train[Semana == x][sample(.N, 500000)]
})) %>% as_tibble
rm(tbl_train)

#An�lise estat�stica
summary(train_sample)
#A vari�vel que estamos tentando prever � "Demanda_uni_equil", que representa 
#"Venta_uni_hoy" (produtos vendidos) - "Dev_uni_proxima" (produtos que ser�o devolvidos na pr�xima semana).
#H� uma varia��o significativa na demanda. Sob uma vis�o mais abrangente, � esperado vender 7.2 produtos por requisi��o.

#Por�m, n�o podemos generalizar essa m�dia para todos os casos:
train_sample %>%
  filter(Demanda_uni_equil == 3920)
#Neste exemplo, foram vendidos 3920 unidades do produto 2604. � a ocorr�ncia mais extrema deste conjunto de dados.
#Ser� necess�rio observar, a partir de cada ID, quais vari�veis melhor representam a disparidade na demanda por produtos.

hist(train_sample$Demanda_uni_equil)
#Grande parte das requisi��es se concentram entre os registros pr�ximos da m�dia de 7.2 produtos.
#Dando um zoom neste intervalo:
ggplot(data = train_sample %>% filter(Demanda_uni_equil <= 10)) +
  geom_histogram(mapping = aes(x = Demanda_uni_equil),
                 bins = 10, fill = '#04691d', color = 'black',) +
  scale_x_continuous(breaks = 1:10) +
  ylab('Ocorr�ncias') + xlab('Demanda') + 
  ggtitle('Distribui��o de registros (Demanda <= 10)')
#A maior parte das demandas est�o concentradas entre 1 e 5 produtos.

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

#Devolu��es por semana
ggplot(data = plotdata_semana) +
  geom_col(mapping = aes(x = as.factor(Semana),
                         y = sum_devol),
           fill = '#ad3232', width = 0.65) +
  scale_y_continuous(labels = comma) +
  xlab('Semana') + ylab('Devolu��es') + theme_light() +
  ggtitle('Devolu��es por semana')

#Demanda por semana
ggplot(data = plotdata_semana) +
  geom_col(mapping = aes(x = as.factor(Semana),
                         y = sum_demanda),
           fill = '#32ad32', width = 0.65) +
  scale_y_continuous(labels = comma) +
  xlab('Semana') + ylab('Demanda') + theme_light() +
  ggtitle('Demanda por semana')
#No per�odo observado, n�o h� varia��o suficiente na demanda semanal para justificar o uso dessa vari�vel.
#� poss�vel observar um aumento no volume de devolu��es a cada 4 semanas, sem varia��o suficiente para
#justificar a cria��o de vari�veis a partir deste valor.
#A varia��o percebida nas devolu��es das semanas 5, 6, 7 e 8 sugerem um padr�o comportamental dos clientes.
#Durante o treinamento do modelo preditivo, ser�o coletadas amostras referentes a estas semanas

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
  ggtitle('Registros por canal de comunica��o')
#90.9% das requisi��es por produtos s�o realizadas pelo canal 1, seguido pelo canal 4 com 5%. 
#Isso sugere que o canal 1 � a op��o mais acess�vel para os clientes registrarem suas solicita��es.
#Os demais canais representam menos de 2% das solicita��es, sendo os canais 8 e 9 os menos utilizados.

#Observada a quantidade de registros por canal, precisamos observar o volume de demandas
ggplot(data = train_sample %>% filter(Demanda_uni_equil > 0)) +
  geom_boxplot(mapping = aes(x = as.factor(Canal_ID), 
                             y = log(Demanda_uni_equil),
                             fill = as.factor(Canal_ID)),
               show.legend = F) +
  coord_flip() + theme_light() +
  xlab('Canal') + ylab('Demanda (log)') +
  ggtitle('Demandas por canal de comunica��o')
#Este gr�fico sugere que, em m�dia, as demandas mais volumosas s�o atendidas pelos canais 5, 9 e 2.
#Ao mesmo tempo, � poss�vel observar outliers em todos canais de comunica��o, 
#sugerindo que n�o h� um canal de comunica��o exclusivo para grandes demandas de produto.
#Ser� calculada a m�dia de demandas por canal. Posteriormente, ser� analisada a correla��o com a vari�vel target
tbl_channel_demanda <- train_sample %>%
  group_by(Canal_ID) %>%
  summarize(mean_channel_demanda = mean(Demanda_uni_equil, na.rm = T)) %>%
  ungroup

## Agencia_ID
#� poss�vel extrair diversas informa��es a partir do ID da ag�ncia:
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
  ggtitle('Propor��o de demandas por registros')
#� esperada uma correla��o positiva entre "quantidade de registros" e "total de demandas". Este gr�fico 
#nos auxilia a perceber alguns outliers: dep�sitos com baixo volume de registros e alto volume de demandas
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
  ggtitle('Propor��o de demandas por registros')

plotdata_depot %>%
  filter(large_scale_depot == 1) %>%
  arrange(-mean_depot_demanda)
#Em m�dia, � esperado que solicita��es nestas distribuidoras sejam maiores que o usual

#A partir da localiza��o das ag�ncias, podemos extrair informa��es de demandas por cidade e estado
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
#Podemos calcular a demanda total e m�dia de cada cliente para identificar clientes de larga escala
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
  xlab('Clientes') + ylab('Demanda m�dia / Demanda total') +
  ggtitle('Demanda total por cliente - Top 20')
#Puebla Remision � o principal cliente, com aproximadamente 860.000 produtos vendidos neste conjunto, enquanto o segundo 
#maior cliente possui um volume de vendas de aproximadamente 39.000.
#Mesmo com este volume significativo, a m�dia de produtos por demanda est� alinhada com os demais clientes, entre 140 e 160.

#A discrep�ncia na demanda m�dia observada neste gr �fico sugere que exista uma alta demanda por uma pequena variedade de produtos,
#enquanto os valores mais pr�ximos da m�dia sugerem uma demanda regular por uma variedade maior de produtos.

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
  xlab('Peso do produto') + ylab('Demanda m�dia') +
  ggtitle('Demanda m�dia por peso do produto')
#N�o parece haver uma correla��o entre a demanda e o peso dos produtos, entretando � poss�vel perceber que
#produtos mais pesados possuem uma demanda menor.
#A maioria dos produtos no cat�logo possuem um peso inferior a 5kg, colocando os produtos em um mesmo patamar.

# Produtos e clientes
product_by_client <- train_sample %>%
  group_by(Cliente_ID) %>%
  summarize(unique_product_count = n_distinct(Producto_ID))

ggplot(data = product_by_client %>% right_join(plotdata_client, by = 'Cliente_ID'),
       mapping = aes(x = unique_product_count,
                     y = mean_demanda_cliente)) +
  geom_point() + theme_light() + 
  xlab('Variedade de produtos') + ylab('Demanda m�dia') +
  ggtitle('Demanda m�dia por variedade de produtos de cliente')
#� esperado atender uma demanda menor � medida que o cliente solicita uma variedade maior de produtos.

#Outliers - grande variedade de produtos
product_by_client %>%
  right_join(plotdata_client, by = 'Cliente_ID') %>%
  filter(unique_product_count > 50)

#Outliers - grande demanda por produtos
product_by_client %>%
  right_join(plotdata_client, by = 'Cliente_ID') %>%
  filter(mean_demanda_cliente > 2000)