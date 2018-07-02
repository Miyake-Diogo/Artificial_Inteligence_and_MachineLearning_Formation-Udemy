# Uso de ML de classificação usando o pacote arules


#instalar e caregar pacote
install.packages("arules")
# Pacote arulesCBA usa o arules para a criação de regras
install.packages("arulesCBA")

library(arules)
library(arulesCBA)

#pacote secundario , neste caso já  tenho instalado
# install.packages("caret", dependencies=T)
library(caret)

# Importação do conjunto de dados weather
tempo = read.csv(file.choose(), sep=";", header=T)
# Criação do modelo
# CBA(classe e suas dependencas, conjunto de dados, parametros)
modelo = CBA(play ~ . , tempo, supp=0.05, conf=.9)
# Inspeção do modelo
inspect(modelo$rules)
# Previsão 
previsao = predict(modelo, tempo)
# verificação da previsão
head(previsao)
#Geração da matriz para poder ver a precisão
confusionMatrix(previsao,tempo$play)
# a precisão do modelo foi de 76%
