# #system('wc -l input/train.csv')
#Output: 184903891 input/train.csv
#O arquivo de treino possui 184 milhões de observações. Será impossível trabalhar com este volume de dados no meu ambiente.
#Minha idéia é coletar indices aleatórios e extrair pedaços a partir de cada índice.
#Depois agrupar tudo em um único tibble e desenvolver o modelo preditivo a partir daí.

## Agrupar todos inputs em um único tibble
merge_inputs_by_row = function(..., is_attributed = NA){
  tib <- bind_rows(...)
  if(is.na(is_attributed)){
    return(tib)
  }
  return(tib %>% filter(is_attributed == !!is_attributed))
}
