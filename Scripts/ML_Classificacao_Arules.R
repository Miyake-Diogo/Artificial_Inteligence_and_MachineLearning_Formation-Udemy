# Uso de ML de classifica��o usando o pacote arules


#instalar e caregar pacote
install.packages("arules")
# Pacote arulesCBA usa o arules para a cria��o de regras
install.packages("arulesCBA")

library(arules)
library(arulesCBA)

#pacote secundario , neste caso j�  tenho instalado
# install.packages("caret", dependencies=T)
library(caret)

# Importa��o do conjunto de dados weather
tempo = read.csv(file.choose(), sep=";", header=T)
# Cria��o do modelo
# CBA(classe e suas dependencas, conjunto de dados, parametros)
modelo = CBA(play ~ . , tempo, supp=0.05, conf=.9)
# Inspe��o do modelo
inspect(modelo$rules)
# Previs�o 
previsao = predict(modelo, tempo)
# verifica��o da previs�o
head(previsao)
#Gera��o da matriz para poder ver a precis�o
confusionMatrix(previsao,tempo$play)
# a precis�o do modelo foi de 76%
