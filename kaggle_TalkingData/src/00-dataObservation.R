library(tidyverse)

#Configurando diretórios
cur_dir = rstudioapi::getActiveProject()
input_dir = paste(cur_dir, 'input', sep = '/')

#Carregando arquivos
files = list.files(input_dir, '.csv')
sapply(paste(input_dir, files, sep = '/'), file.info)
#O arquivo de treino completo possui 7GB. É improvável que meu ambiente de trabalho seja capaz
#de carregar todo esse conjunto em memória.

#As análises serão realizadas no arquivo "train_sample.csv", que representam 100.000 amostras aleatórias extraídas do conjunto de treino
tbl_train_sample = read_csv(paste(input_dir, files[4], sep = '/'))
glimpse(tbl_train_sample)
#Estamos tentando prever a variável "is_attributed", que representa se um clique resultou em um download
table(tbl_train_sample$is_attributed)

#Observando dados do dataset de testes
tbl_test_sample = read_csv(paste(input_dir, files[3], sep = '/'), n_max = 100000)
glimpse(tbl_test_sample)
#Temos as variáveis ip, app, device, os, channel e click_time à nossa disposição para elaborar o modelo preditivo
#Entretanto, com exceção de click_time, todas essas variáveis são categóricas e representam um ID. Se optarmos por utilizar
#essas variáveis na concepção do modelo, precisamos prepará-lo para tratar valores não observados durante o treinamento.