# FORMAÇÂO IA E ML -UDEMY
# Projeto 3 - Oito Rainhas

## resolução do problema oito rainhas
# posicionar 8 rainhas em um tabuleiro 8x8 de forma que uma não ataque a outra.

########################################

# inicia com a função de oit inteiros recebendo a posição de cada rainha
oitorainhas <- function(solucao)
{
        #teste
        #solucao = c(1,5,8,4,2,6,7,3)
        
        
        #inicializa um vetor com 64 posicoes preenchido com zeros
        vetor = rep.int(0,64)
        
        #variavel auxiliar
        posicao = 1
        
        #transforma a solucao em vetor com 64 posicoes
        for (i in 1:8) {
                vetor[ posicao + solucao[i] -1       ] = 1;
                posicao = posicao + 8
        }
        
        #tranforma o vetor em uma matriz 8 x 8 
        queens = matrix(vetor, nrow=8,ncol=8,byrow =F)
        
        #variavel para contar os ataques
        total = 0 
        
        #verifica linhas e colunas
        for (i in 1:8) {
                #verifica colunas
                total = total + ifelse(sum(queens[,i])>1,1,0)
                #verifica linhas
                total = total + ifelse(sum(queens[i,])>1,1,0)
        }
        
        #faz a transposicao das diagonais
        tmp <- row(queens) - col(queens)
        z = split(queens,tmp)
        
        #inverte a matriz para ler as outras diagonais
        queens2 = queens[,8:1]
        tmp <- row(queens2) - col(queens2)
        y = split(queens2,tmp)
        
        for (i in 1:15) {
                #verifica diagonais
                total = total + ifelse(sum( z[[i]])>1,1,0)
                #verifica diagonais inversa
                total = total + ifelse(sum( y[[i]])>1,1,0)
        }
        return(-total)
        
}

# Impressão do tabuleiro
install.packages("imager")

impressao <- function(d)

        {
        
        library(imager)
        
        
        image <- load.image(file.choose())
        plot(image)
        
        
        #coordenadas
        x = c(18,53,88,122,158,192,227,262)
        
        
        for (i in 1:8) {
                
                text(x[i],x[d[i]],"Q",cex=4,col = "red")
                
                
        }
 }
# Execução da Solução 
install.packages("GA")
library(GA)
#algoritmo genetico
resultado <- ga(type="permutation", fitness=oitorainhas,min=c(1,1,1,1,1,1,1,1), max=c(8,8,8,8,8,8,8,8),popSize = 10, maxiter = 1000)

#solucao
summary(resultado)$solution

#evolucao
plot(resultado)

impressao(summary(resultado)$solution[1,])

# Se a solução chegar a zero então será o ótimo global
