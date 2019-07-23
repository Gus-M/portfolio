library(tidyverse)
library(data.table)

#### Produtos ####
#Extrair métricas a partir do nome do produto
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

#Padronização de peso por produto
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

#Imputar valor constante na quantidade unitária de produtos
impute_constant_unit = function(tib){
  return(tib %>% mutate(product_units = replace_na(product_units, 1)))
}

#Imputar mediana no peso por grupo de produto
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

#Imputar média no peso do produto
impute_mean_weight = function(tib){
  overall_mean_weight = round(mean(tib$product_weight, na.rm = T))
  return(tib %>% mutate(product_weight = replace_na(product_weight, overall_mean_weight)))
}

#Calcular as features de produtos
calc_product_features = function(tib){
  
  #Produtos acima de 5 litros ou 5 kilogramas serão classificados como "produtos pesados"
  weight_threshold = 5000
  
  #Produtos com 6 ou mais unidades serão classificados como "produtos empacotados"
  bundle_threshold = 6
  
  return(tib %>%
           mutate(heavy_product = as.numeric(product_weight >= weight_threshold),
                  bundled_product = replace_na(product_weight, 0)))
}

#Pipeline de produtos
product_pipeline = function(tib){
  steps = extract_from_product_name(tib) %>%
    standardize_weight %>%
    impute_constant_unit %>%
    impute_group_median_weight %>%
    impute_mean_weight %>%
    calc_product_features
  return(steps)
}

#### Cidades ####
#Aplicar label encoding nas cidades e estados
encode_towns = function(tib){
  return(tib %>%
           mutate(Town_ID = as.numeric(factor(Town)),
                  State_ID = as.numeric(factor(State))) %>%
           select(Agencia_ID, Town_ID, State_ID))
}

#### Feature Engineering ####
calc_train_features = function(train_data, product_data, town_data,
                               quantile_depot_mean_threshold = 0.75, 
                               quantile_depot_count_threshold = 0.5){
  
  #Produtos
  product_info = product_pipeline(product_data) %>%
    select(-NombreProducto, -product_weight, -product_units)
  
  #Cidades
  town_info = encode_towns(town_data)
  
  #Demanda por canal
  channels = train_data %>%
    group_by(Canal_ID) %>%
    summarize(mean_channel_demanda = mean(Demanda_uni_equil, na.rm = T)) %>% ungroup
  
  #Demanda por distribuidoras
  depots = train_data %>%
    group_by(Agencia_ID) %>%
    summarize(mean_depot_demanda = mean(Demanda_uni_equil)) %>%
    left_join(train_data %>% count(Agencia_ID, name = 'count_agencia'), by = 'Agencia_ID') %>%
    left_join(town_info, by = 'Agencia_ID')
  
  large_scale_depot_mean_threshold = quantile(depots$mean_depot_demanda, quantile_depot_mean_threshold)
  large_scale_depot_count_threshold = quantile(depots$count_agencia, quantile_depot_count_threshold)
  
  depots <- depots %>%
    mutate(large_scale_depot = as.numeric(mean_depot_demanda >= large_scale_depot_mean_threshold &
                                            count_agencia >= large_scale_depot_count_threshold)) %>%
    select(-count_agencia)
  
  #Demanda por cidade
  town_depots = train_data %>%
    left_join(town_info, by = 'Agencia_ID') %>%
    group_by(Town_ID) %>%
    summarize(mean_town_demanda = mean(Demanda_uni_equil))
  
  #Demanda por cliente
  clients = train_data %>%
    group_by(Cliente_ID) %>%
    summarize(mean_demanda_cliente = mean(Demanda_uni_equil)) %>%
    distinct
  
  #Demanda por produto
  products = train_data %>%
    group_by(Producto_ID) %>%
    summarize(mean_producto_demanda = mean(Demanda_uni_equil)) %>%
    left_join(product_info, by = 'Producto_ID')
  
  # #Variedade de produto por cliente
  # product_stock = train_data %>%
  #   group_by(Cliente_ID) %>%
  #   summarize(unique_product_count = n_distinct(Producto_ID)) %>%
  #   distinct
  
  return(list(channels, depots, town_depots, clients, products))
}