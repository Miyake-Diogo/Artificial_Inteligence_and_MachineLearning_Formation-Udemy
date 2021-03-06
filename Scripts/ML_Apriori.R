# Machine Learning - Apriori

# Instala��o e carregamento dos pacotes

install.packages("arules")
library(arules)

install.packages("arulesViz")
library("arulesViz")

# Imprta��o das transa��es, em uma vari�vel e realiza��o de inspe��o
transacoes = read.transactions(file.choose(), format="basket",  sep=",")
transacoes 
inspect(transacoes) 
image(transacoes) # cada ponto � um item da respectiva transa��o


# Cria��o das regras de associa��o
# Fun��o apriori(conjunto de dados, parametros(suporte minimo, confian�a minima, tamanho minimo))
regras = apriori(transacoes, parameter=list(supp=0.03,conf=0.4,minlen=2))
regras
# Resumo das regras, e inspe��o das regras
summary(regras)
inspect(regras) 

# Plotagem das regras, em graficos e matrizes.
plot(regras, method="graph")
plot(regras, method="matrix")
plot(regras, method="matrix",engine="3d", measure="lift", control=list(reorder='none'))
plot(regras, method="grouped")
