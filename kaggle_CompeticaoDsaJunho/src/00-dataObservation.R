library(tidyverse)
library(data.table)

#Configurando diretórios
cur_dir = rstudioapi::getActiveProject()
input_dir = paste(cur_dir, 'input/', sep = '/')
output_dir = paste(cur_dir, 'output/', sep = '/')

#Selecionando arquivos
files <- list.files(input_dir, '*.csv')
tbl_comerciantes <- read_csv(paste(input_dir, files[1], sep = ''))
tbl_teste <- read_csv(paste(input_dir, files[2], sep = ''))
tbl_treino <- read_csv(paste(input_dir, files[3], sep = ''))

# Os dados relativos a transações sáo muito mais pesados e terão de ser analizados com data.table
# Não tentei carregar o dataset inteiro em memória ainda
dt_transacoes <- fread(paste(input_dir, files[4], sep = ''), nrows = 1000000)
dt_transacoes_hist <- fread(paste(input_dir, files[6], sep = ''), nrows = 1000000)

#### Analisando as primeiras linhas de cada input ####
## Comerciantes
View(head(tbl_comerciantes, 100), 'Comerciantes')

## Transações
View(head(dt_transacoes, 100), 'Transações')
View(head(dt_transacoes_hist, 100), 'Transações - Hist')

# Os dados de Comerciantes e Transações podem ser cruzados com cada cartão de cada cliente. Isso será importante
#para encontrarmos outras features.

## Dados de Treino e Teste
View(head(tbl_treino, 100), 'Treino')
#Observando a variável target
range(tbl_treino$target)
summary(tbl_treino$target)
hist(tbl_treino$target)
sd(tbl_treino$target)
# Os dados da variável target estão em uma distribuição normal. Há alguns outliers que precisamos analisar mais atentamente
# Não está claro ainda o que são as Features 1, 2 e 3. Precisamos cruzar informações destes IDs com dados históricos para compreender

View(head(tbl_teste, 100), 'Teste')
# Para as variáveis de teste, temos apenas as Features.

sapply(select(tbl_treino, feature_1, feature_2, feature_3), range)
sapply(select(tbl_teste, feature_1, feature_2, feature_3), range)
sapply(select(tbl_treino, feature_1, feature_2, feature_3), table)
sapply(select(tbl_teste, feature_1, feature_2, feature_3), table)
# As features parecem ser algum tipo de avaliação de cada cliente.
# A Feature 1 é um valor entre 1 e 5
# A Feature 2 é um valor entre 1 e 3
# A Feature 3 é é um valor binário
cor(tbl_treino %>% select(feature_1, feature_2, feature_3, target))
# Há pouca correlação negativa entre as features disponíveis e a variável target.
# É possível que haja correlação entre as features

#### Conclusões e próximas etapas ####
# A variável target é um índice de "lealdade" de cada cliente de cartão. Acredito que a variável target foi concebida através da análise
#de diversas features, mas apenas três foram disponibilizadas neste dataset.
# A primeira tarefa que realizarei será a elaboração de dois modelos com base nas três features disponíveis. Pensei em utilizar dois
#modelos simples de regressão linear e árvore de decisão. Após o treinamento do modelo, vamos submeter as previsões obtidas e analisar
#o resultado obtido na competição.
# A partir daí, analisaremos os datasets de transações e comerciantes, cruzaremos informações com clientes e criaremos outras features
#que ajudem a melhorar a explicar a variável target, de forma a melhorar o RMSE do modelo.

