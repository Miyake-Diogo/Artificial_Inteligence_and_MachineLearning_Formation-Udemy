## Forma��o IA e ML - Udemy
# Sele��o de atributos e Multi Label

# Instala��o e importa��o dos pacotes
install.packages("FSelector", dependencies=T)
library(FSelector)

install.packages("e1071")
library(e1071)

install.packages("caret", dependencies=T)
library(caret)

# Importa��o do dataset e visualiza��o de dados
anuncios = read.csv(file.choose(), sep=",",header=F)
head(anuncios)
dim(anuncios)
# Gera��o de dados de treino e teste
particao = createDataPartition(1:3279,p=.7)
anunciostreino = anuncios[particao$Resample1,]
dim(anunciostreino)
anunciosteste = anuncios[-particao$Resample1,]
dim(anunciosteste)

# Gera��o do modelo usando naive Bayes do pacote e1070
modelo = naiveBayes(V1559 ~. , data=anunciostreino)
# Gera��o de previs�o e matrix de confus�o
previsao = predict(modelo, newdata=anunciosteste)
confusionMatrix(previsao, anunciosteste$V1559)

# Cria��o de atributos

atributos <- chi.squared(V1559 ~., anuncios)
# Verifica��o de atributos
head(atributos)
# Cria��o de um subgrupo para utilizar a fun��o de corte (atributos mais importantes) para tentar melhorar a performance
subgrupo <- cutoff.k(atributos, 7)

subgrupo
# Verifica��o da precis�o com a fun��o de corte
modelo = naiveBayes(V1559 ~ V3 + V2 + V1 + V1244 + V1400 + V352 + V1484 , data=anunciostreino)
previsao = predict(modelo, newdata=anunciosteste)
confusionMatrix(previsao, anunciosteste$V1559)
# A partir de utilizar os atributos mais importantes a precis�o melhoru muito de 13% para 95%....

##################
## Classifica��o usando multi label
# Instala��o e importa��o dos pacotes 
install.packages("mlr")
library(mlr)

install.packages("caret", dependencies=T)
library(caret)
# Importa��o do arquivo musica
musica = read.csv(file.choose(), sep=",", header = T)
# Verifica��o dos primeiros itens
head(musica)

musica[, 1:6] <- sapply(musica[, 1:6], as.logical)
# Cria��o dos rotulos
rotulos = colnames(musica)[1:6]

#criamos a tarefa (makeMultilabelTask(dados, alvo ou labels))
tarefa = makeMultilabelTask(data = musica, target = rotulos)
#cria um objeto de aprendizado
aprendizado = makeLearner("classif.rpart")

#primeiro binary relevance
tipoclass = makeMultilabelBinaryRelevanceWrapper(aprendizado)

#cria particao de dados
particao = createDataPartition(1:592,p=.7)

#descarrega caret para n�o ter conflito com metodo train
detach("package:caret", unload=TRUE)

#treina o modelo
modelo = train(tipoclass, tarefa, subset = particao$Resample1)
modelo

#fazemos a previsao (predict(modelo, tareda, subconjuntos, seqeuncia ou n�o dentro da parti��o))
predicao = predict(modelo, task = tarefa, subset = subset(seq(1:592),!seq(1:592) %in% particao$Resample1))
predicao


#avaliar a performance e anotar o resultado
performance(predicao, measures = list(multilabel.hamloss))

####################
#testando outro tipo de transforma��o
#classifier chains
tipoclass = makeMultilabelClassifierChainsWrapper(aprendizado)

#treina o modelo
modelo = train(tipoclass, tarefa, subset = particao$Resample1)

#fazemos a previsao
predicao = predict(modelo, task = tarefa, subset = subset(seq(1:592),!seq(1:592) %in% particao$Resample1))

#avaliar a performance e anotar o resultado
performance(predicao, measures = list(multilabel.hamloss))
