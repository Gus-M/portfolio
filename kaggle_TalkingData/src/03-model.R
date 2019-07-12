source('src/02-toolkit.R')

#Configurando diretórios
cur_dir = rstudioapi::getActiveProject()
input_dir = paste(cur_dir, 'input', sep = '/')

#Carregando arquivos
files = list.files(input_dir, '.csv')

#system('wc -l input/train.csv')
#Output: 184903891 input/train.csv
#O arquivo de treino possui 184 milhões de observações. Será impossível trabalhar com este volume de dados no meu ambiente.
#Minha idéia é coletar indices aleatórios e extrair pedaços a partir de cada índice.
#Depois agrupar tudo em um único tibble e desenvolver o modelo preditivo a partir daí.
