#Resolução da Função de Rosenbrock's
#f(x,y) = (1-x)^2 + 100(y-x^2)^2, onde o mínimo global f(x,y)=0 com x e y =1
#Uso do pacote GenSA, busca minimo global em função objetiva não linear.

#Criação da função objeto
Rosenbrock <- function(z) {
  x = z[1]
  y = z[2]
  
  return((1-x)^2 + 100*(y-x^2)^2)
  
}

#Instalação do pacote caso não possua
install.packages("GenSA")
#chamada do pacote
library(GenSA)
#Criação da função resultado onde os parâmetros são:
#GenSA(minimo , máximo, função objeto, parametos que podem passados ,(logs para serem apresentados))
resultado <- GenSA(lower = c(0,0), upper = c(9,9), fn = Rosenbrock, control=list(verbose=TRUE))
#chamada do resultado com todos os valores encontrados 
resultado
#Chamada do resultado final
resultado$par


