# Classifica��o baseado em agrupamentos - Random Forest

# Instala��o e carga do pacote
install.packages("randomForest")
library(randomForest)

# Importa��o do credito
credito = read.csv(file.choose(), sep=",", header=T)

# gera��o do Modelo para previs�o
modelo = randomForest(class ~ ., data = credito, ntree=500 ) 
# O RandomForest n�o precisa separar dados de teste e treino, 
# pois automaticamente ele j� separa cerca de 1/3 do modelo para o teste, o "out of bag"

#previsao baseado nos dados out of bag
modelo$predicted

#importancia dos atributos no modelo
modelo$importance

#proporcao de votos na classificacao de cada instancia
modelo$votes

#arvores induzidas
modelo$forest

#matriz de confusao baseado nos dados out of bag
modelo$confusion

# Plot do modelo
plot(modelo)

#fazendo previsoes com um registro (no caso a instancia 154)
predict(modelo, newdata = credito[154,])
