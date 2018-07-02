# Machine Learning Baseado em Instancia - KNN

#instalar e caregar pacote
# install.packages("class")
library(class)

# Uso do conjunto Iris
novairis = iris
# divisao de teste e treino, para teste somente a primeira linha
iristeste = novairis[1,]
# Para treino todas linhas menos a primeira
novairis = novairis[-1,]
# Verificação da Dimensão
dim(iristeste)
dim(novairis)
# Verificação da coluna Species
iristeste$Species
# Geracao e visualizacao da previsao
# knn(conjunto de dados de treino, conjunto de dados de teste, classes , numero de vizinhos mais proximos)
previsao = knn(novairis[,1:4],iristeste[,1:4],novairis[,5],k=3)
previsao
