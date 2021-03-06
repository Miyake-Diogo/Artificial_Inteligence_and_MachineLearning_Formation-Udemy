#Sistemas Especialistas - Cooke , Mendel, e pesos
#Instalando o pacote expert
install.packages("expert")
#carregando o pacote
library(expert)

# O primeiro modelo ser� o de Cooke


# Especialistas: EXP1, EXP2, EXP3
# Seeds: SEM1, SEM2
# Variavel de interesse INT
#Lista com os dados de especialistas, semente, e evari�veis de interesse
x <- list(	EXP1 <- list(	SEM1 <- c(75, 80, 85),
                         SEM2 <- c(10, 15, 20),
                         INT <- c(650, 800, 850)),
           EXP2 <- list(	SEM1 <- c(80, 90, 95),
                         SEM2 <- c(25, 30, 35),
                         INT <- c(500, 600, 700)),
           EXP3 <- list(	SEM1 <- c(65, 70, 80),
                         SEM2 <- c(20, 25, 30),
                         INT <- c(450, 650, 800)))

#quantils 10,50,90 na variavel probabilidade
prob <- c(0.1, 0.5, 0.9)

#semente verdadeira
semverd <- c(80, 25)

#inferencia com a fun��o expert
inf <- expert(x, "cooke", prob, semverd)

#decision maker, combina��o das avalia��es dos especialistas
inf
#histograma da inferencia em azul
hist(inf,col = "blue")

#comparando as distribui��es agregado e especialistas

par(bg = "white")
#Divis�o da tela em 2 linhas e duas colunas
split.screen(c(2,2))
#Tela 1 apresentando a vari�vel
screen(1)
#histograma 
hist(inf,col = "gray",main ="Distribui��o agregada")
#tela 2
screen(2)
#Variavel s com a densidade das variaveis de interesse
s = density(c(650, 800, 850))
#Plotagem da densidade com as opni�es dos especialistas

plot(s,main="Especialista 1")
polygon(s,col="blue")
screen(3)
s = density(c(500, 600, 700))
plot(s,main="Especialista 2")
polygon(s,col="blue")
screen(4)
s = density(c(450, 650, 800))
plot(s,main="Especialista 3")
polygon(s,col="blue")

#fechamento das janelas
close.screen(all = TRUE) 

#mais informa��es da inferencia
#quantile zero e 100 s�o calculados automaticamnete
summary(inf)

#quantiles
quantile(inf)

#media dos quatis
mean(inf)

#distribui��o cumulativa
dc = cdf(inf)
plot(dc)

#ogiva, usado para mostrar a frequencia acumulada
og = ogive(inf)
plot(og)

##########################################

# Metodo de Mendel

y <- list(	EXP1 <- list(	SEM1 <- c(75, 80, 85),
                         SEM2 <- c(10, 15, 20),
                         INT <- c(650, 800, 850)),
           EXP2 <- list(	SEM1 <- c(80, 90, 95),
                         SEM2 <- c(25, 30, 35),
                         INT <- c(500, 600, 700)),
           EXP3 <- list(	SEM1 <- c(65, 70, 80),
                         SEM2 <- c(20, 25, 30),
                         INT <- c(450, 650, 800)))

#quantils 10,50,90
prob <- c(0.1, 0.5, 0.9)

#semente verdadeira
semverd <- c(80, 25)


#ms para o modelo de mendel-sheridan, 
inf <- expert(y, "ms", prob, semverd)

#infer�ncia e histograma

inf

hist(inf,col = "blue")
#Plotagem do grafico de ogiva para mendel
og = ogive(inf)
plot(og)

##################

#Inser��o de pesos manuais

#Note que n�o s�o necess�rios as sementes e nem os especialistas, somente a variu�vel de interesse
z <- list(	EXP1 <- list(	           INT <- c(650, 800, 850)),
           EXP2 <- list(	   INT <- c(500, 600, 700)),
           EXP3 <- list(	INT <- c(450, 650, 800)))

# Probabilidade em quantis
prob <- c(0.1, 0.5, 0.9)

# weights para o pesos "manuais" com a fun��o expert, com o metodo peso(weights),e com o peso de cada especialista
inf <- expert(z, "weights", prob,  w=c(.1,.7,.2))
#Verifica��o da inferencia
inf
# Histograma da inferencia
hist(inf, col="blue")
# Exibi��o da plotagem de ogiva
og = ogive(inf)
plot(og)




