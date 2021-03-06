# Arvores de decis�o

# instalar e caregar pacote
install.packages("rpart")
# Importa��o do pacote
library(rpart)

# pacote secundario
install.packages("caret", dependencies=T)
# Importa��o do pacote
library(caret)

# importando dados, buscar o arquivo credit-g.csv
credito = read.csv(file.choose(), sep=",", header=T)

# criando samples de treino e teste
particao = createDataPartition(1:1000,p=.7)
creditotreino = credito[particao$Resample1,]
creditotreino <- creditotreino[complete.cases(creditotreino),]
creditoteste = credito[- particao$Resample1,]
creditoteste <- creditoteste[complete.cases(creditoteste),]


# primeiro modelo sem alteracao - outro metodo � o anova para regress�o
# fun��o rpart(variavel de resposta ~.(indica que todos atributos s�o variaveis dependentes), data=conjunto de dados, metodo a ser utilizado)
modelo1 = rpart(class  ~., data=creditotreino, method="class")

# print no modelo para podermos compara��o
plot(modelo1)
text(modelo1)

# Gera��o da previs�o do modelo
previsao = predict(modelo1, newdata=creditoteste)

# mostrar que previsao preve conforme a probabilidade 
head(previsao) # somente os primeiros, pois tem muitos dados

# transforma previsao em df
previsao = as.data.frame(previsao)

#tranforma a coluna selecionada em uma variavel discreta
# Se for verdadeiro, bad, sen�o good, onde a compara��o � baseada em probabilidade, acima de 50% good e abaixo bad.
previsao$class = ifelse(previsao$bad >= .5, "bad", "good")

#matriz de confusao do pacote caret
# confusionMatrix(previsao$class, creditoteste$class), retonnou em erro:Error: `data` and `reference` should be factors with the same levels.
confusionMatrix(table(previsao$class, creditoteste$class)) 
# A precis�o foi de ~ 75%.

#novo modelo, baseado em valores minimos
# control=rpart.control(minsplit=20), parametro que define os valores minimos para resolu��o da arvore
modelo2 = rpart(class  ~., data=credito, method="class", control=rpart.control(minsplit=20))
modelo2

#print no modelo para comparar depois
plot(modelo2)
text(modelo2)

# Basicamente o mesmo c�digo do modelo anterior
previsao = predict(modelo2, newdata=creditoteste)
previsao = as.data.frame(previsao)
previsao$class = ifelse(previsao$bad >= .5, "bad", "good")
confusionMatrix(table(previsao$class, creditoteste$class))
# A precis�o foi de ~ 82%, melhorou bastante

# Modelo de poda
# Fun��o prune(modelo a ser podado, parametro de complexidade, passar um valor m�dio)
modelo3 = prune(modelo2, cp=0.05)
# novamente as plotagems
plot(modelo3)
text(modelo3)

# Gera��o da previs�o 
previsao = predict(modelo3, newdata=creditoteste)
previsao = as.data.frame(previsao)
previsao$class = ifelse(previsao$bad >= .5, "bad", "good")
# Novamente a gera��o da Matrix de confus�o.
confusionMatrix(table(previsao$class, creditoteste$class))
# A precis�o foi de ~ 77%

