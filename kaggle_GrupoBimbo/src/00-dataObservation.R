library(tidyverse)

#Configurando diretórios
cur_dir = rstudioapi::getActiveProject()
input_dir = paste(cur_dir, 'input', sep = '/')

#Selecionando arquivos
files <- list.files(input_dir, '.csv')

#files[1] cliente_tabla.csv: Tabela com ID e nome de clientes
#files[2] producto_tabla.csv: Tabela com ID e nome de produtos
#files[3] sample_submission: Template de output para testar modelo preditivo no Kaggle
#files[4] test.csv: Dados para teste
#files[5] town_state.csv: Tabela com ID e nome de cidades e estados
#files[6] train.csv: Dados para treino
datasets = lapply(files, function(x){
  read_csv(paste(input_dir, x, sep = '/'), n_max = 100)
})

## Tabelas de junção
#Clientes
head(datasets[[1]], 20)
#Observando os primeiros registros, é possível detectar algumas inconsistências:
# * O cliente 4 está duplicado.
# * Os clientes 0 e 2 possuem o mesmo nome. Isso significa que não podemos agrupar clientes pelo nome,
#dada a possibilidade de existir clientes homônimos
#É possível que exista mais de um registro para o mesmo cliente

#Produtos
head(datasets[[2]], 20)
# * Os produtos parecem estar corretamente cadastrados. O ID 0 aparenta ser um registro genérico para produtos não identificados.
#Novos produtos não cadastrados nessa lista serão associado ao ID 0.
# * Cada produto possui informações extras em seu nome, como a quantidade de gramas e/ou a quantidade de peças.
#Extrair essa informação pode ser útil para medir o volume de itens solicitados

#Cidades
head(datasets[[5]], 20)
#Registro de cada distribuidora. Existe um ID primário para cada agência de distribuição e um ID secundário
#para cada cidade, representado pelo número no texto.
#Cada cidade e agência está associada a um estado.

## Dados disponíveis para previsão
#sample_submission
head(datasets[[3]], 20)
# Para cada ID no dataset de teste, é esperado um valor numérico de Demanda

#Teste
head(datasets[[4]], 20)
#Descrição das variáveis disponíveis para teste:
# * Semana: Número da semana no ano
# * Agencia_ID: ID da distribuidora, podendo ser associado ao dataset town_state.csv
# * Canal_ID: Meio de comunicação utilizado entre cliente e distribuidora
# * Ruta_SAK: ID do percurso realizado entre a distribuidora e o cliente
# * Cleinte_ID: ID do cliente, podendo ser associado ao dataset cliente_tabla.csv
# * Producto_ID: ID do produto, podendo ser associado ao dataset producto_tabla.csv

#Treino
head(datasets[[6]], 20)
#Além dos dados de teste, temos disponível os valores necessários para calcular as features do modelo preditivo:
# * Venta_uni_hoy: Número de vendas nesta semana
# * Venta_hoy: Valor financeiro obtido pelas vendas
# * Dev_uni_proxima: Número de devoluções na próxima semana
# * Dev_proxima: Valor financeiro das devoluções
# * Demanda_uni_equil: Variável target, representando a Demanda ajustada (Venta_uni_hoy - Dev_uni_proxima)
#     * A Demanda prevista sempre será maior que zero.