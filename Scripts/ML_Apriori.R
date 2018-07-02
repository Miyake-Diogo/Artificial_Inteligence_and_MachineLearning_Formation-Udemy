# Machine Learning - Apriori

# Instalação e carregamento dos pacotes

install.packages("arules")
library(arules)

install.packages("arulesViz")
library("arulesViz")

# Imprtação das transações, em uma variável e realização de inspeção
transacoes = read.transactions(file.choose(), format="basket",  sep=",")
transacoes 
inspect(transacoes) 
image(transacoes) # cada ponto é um item da respectiva transação


# Criação das regras de associação
# Função apriori(conjunto de dados, parametros(suporte minimo, confiança minima, tamanho minimo))
regras = apriori(transacoes, parameter=list(supp=0.03,conf=0.4,minlen=2))
regras
# Resumo das regras, e inspeção das regras
summary(regras)
inspect(regras) 

# Plotagem das regras, em graficos e matrizes.
plot(regras, method="graph")
plot(regras, method="matrix")
plot(regras, method="matrix",engine="3d", measure="lift", control=list(reorder='none'))
plot(regras, method="grouped")
