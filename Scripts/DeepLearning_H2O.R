# Formação IA e ML - UDEMY
## Deep Learningo com o H2O

#warnb , desligar os warnings sem relevancia
options(warn=-1)

#importa arquivos de digitos, importar treino
digitos <- read.csv(gzfile(file.choose()), header=F)

# cada linha possui 785 colunas, sÃ£o 784 pixels e mais uma coluna para a classe (digito)
dim(digitos)
head(digitos)

# visualizar 4 digitos compartilhando a tela
split.screen(figs=c(2,2))

#precisamos transformar a imagem que esta na linha em uma matriz de duas dimensões
#784 é 28 x 28, ou seja, uma imagem de 28 x 28 pixels

dig = t(matrix(unlist(digitos[20,-784]), nrow = 28, byrow = F))
dig = t(apply(dig, 2, rev))

#ver imagem em "pixels"
dig

screen(1)
image(dig, col = grey.colors(255))

#conferindo se é o digito 4
digitos[20,785]

# Faz o mesmo para as outras áreas
screen(2)
dig = t(matrix(unlist(digitos[2,-784]), nrow = 28, byrow = F))
dig = t(apply(dig, 2, rev))
image(dig,col=grey.colors(255))

screen(3)
dig = t(matrix(unlist(digitos[4,-784]), nrow = 28, byrow = F))
dig = t(apply(dig, 2, rev))
image(dig,col=grey.colors(255))

screen(4)
dig = t(matrix(unlist(digitos[5,-784]), nrow = 28, byrow = F))
dig = t(apply(dig, 2, rev))
image(dig,col=grey.colors(255))

close.screen(all=TRUE)
# Feito a visualização do reconhecimento dos digitos, hora de por a mão na massa

install.packages("h2o")
library(h2o)

# precisa ser inicializado
h2o.init()

#importa os dados de treino e teste
treino <- h2o.importFile(file.choose())
teste <- h2o.importFile(file.choose())

dim(treino)
head(treino)
colnames(treino)

#transforma a classe em factor - exigencia do funcionamento da rede neural profunda
treino[,785] <- as.factor(treino[,785])
teste[,785] <- as.factor(teste[,785])

# Criação do modelo da Rede neural
# h2o.deeplearning(variaveis independentes,  classe, objeto treino,  objeto de validação, 
                # Tipo de distribuição,  Tipo de ativação,  camadas,  tratamento de dados esparsos, 
                        # quantidade de epochs)
modelo <- h2o.deeplearning(x = colnames(treino[,1:784]),  y = "C785",  training_frame = treino,  validation_frame = teste,  distribution = "AUTO",  activation = "RectifierWithDropout",  hidden = c(64,64,64),  sparse = TRUE, epochs = 20)
# plotagem do modelo
plot(modelo)

h2o.performance(modelo)


#previsao de novos dados
#vimos que na linha 20 tinha o numero 4
#vamos conferir
treino[20,785]

#fazendo previsão
pred <- h2o.predict(modelo, newdata = treino[20,1:784])

#verificando a previsão
pred$predict
