# Forma��o IA e ML - Udemy
## Multilayer Perceptron
# Instala��o dos pacotes
install.packages("neuralnet")
library(neuralnet)
# Cria��o de uma c�pia do conjunto iris

myiris = iris
# Verifica��o do conte�do da dos dados
dim(myiris)
head(myiris, 10)
# Cria��o de mais tr�s colunas para binariza��o, cada coluna ser� preenchida com TRUE e FALSE,
# De acordo com as colunas que j� havia no conjunto de dados

myiris = cbind(myiris,myiris$Species=='setosa')
myiris = cbind(myiris,myiris$Species=='versicolor')
myiris = cbind(myiris,myiris$Species=='virginica')
head(myiris, 5)
# Renomeamento das colunas criadas
names(myiris)[6] <- 'setosa'
names(myiris)[7] <- 'versicolor'
names(myiris)[8] <- 'virginica'
# Verifica��o da distribui��o de T e F para cada coluna
summary(myiris)

#install.packages("caret", dependencies=T)
library(caret)

#particao dos dados em treino (70%) e teste (30%)
particao = createDataPartition(1:dim(myiris)[1],p=.7)
iristreino = myiris[particao$Resample1,]
iristeste = myiris[- particao$Resample1,]
dim(iristreino)
dim(iristeste)
# Gera��o do modelo usando redes neurais 
# neuralnet(formula, conjunto de dados, arquitetura da rede( camadas, neuronios por camada), numero de repeti��es)
modelo = neuralnet( setosa  + versicolor  +  virginica  ~ Sepal.Length + Sepal.Width +  Petal.Length + Petal.Width , iristreino, hidden=c(5,4), rep = 2)
# Impress�o do modelo
print(modelo)
# plotagem do modelo
plot(modelo)

# Verifica��o de teste
teste = compute(modelo,iristeste[,1:4])
# Resultado do teste
teste$net.result
# Cria��o de um dataframe com os resultados
resultado = as.data.frame(teste$net.result)

names(resultado)[1] <- 'setosa'
names(resultado)[2] <- 'versicolor'
names(resultado)[3] <- 'virginica'

# Resultados de acordo com a as colunas criadas
resultado$class = colnames(resultado[,1:3])[max.col(resultado[,1:3], ties.method = 'first')]
# Matriz de confus�o
confusao = table(resultado$class,iristeste$Species)
confusao

#Acertos
sum(diag(confusao) * 100 / sum(confusao))
