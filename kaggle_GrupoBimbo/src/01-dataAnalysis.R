library(tidyverse)

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
#TODO: Continuar daqui. Extrair ID de Towns e criar um State_ID para State