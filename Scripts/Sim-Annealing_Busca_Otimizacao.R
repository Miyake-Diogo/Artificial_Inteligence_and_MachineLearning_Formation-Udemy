#Resolu��o da Fun��o de Rosenbrock's
#f(x,y) = (1-x)^2 + 100(y-x^2)^2, onde o m�nimo global f(x,y)=0 com x e y =1
#Uso do pacote GenSA, busca minimo global em fun��o objetiva n�o linear.

#Cria��o da fun��o objeto
Rosenbrock <- function(z) {
  x = z[1]
  y = z[2]
  
  return((1-x)^2 + 100*(y-x^2)^2)
  
}

#Instala��o do pacote caso n�o possua
install.packages("GenSA")
#chamada do pacote
library(GenSA)
#Cria��o da fun��o resultado onde os par�metros s�o:
#GenSA(minimo , m�ximo, fun��o objeto, parametos que podem passados ,(logs para serem apresentados))
resultado <- GenSA(lower = c(0,0), upper = c(9,9), fn = Rosenbrock, control=list(verbose=TRUE))
#chamada do resultado com todos os valores encontrados 
resultado
#Chamada do resultado final
resultado$par


