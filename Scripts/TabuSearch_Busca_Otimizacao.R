# Algoritmos de busca e otimiza��o
#Problema da loja 711: cliente compra roupas para a sua familia e as roupas
#n�o servem, ele vai devolver e e a atendente multiplica a devolu��o e da 7.11,
# o cliente reclama e diz que ela deveria ter somado , 
# a caixa se desculpa e ela soma a devolu��o e d� 7.11, tendo a lista qual os produtos
#que somados e multiplicados equivalem a 7.11?

#valores = 1.10,1.20,1.25,1.41,1.5,1.6,2.05,2.22,2.65,2.9,3.04,3.16
# A melhor forma � usar o tabuSearch com a fun��o bin�ria.
# Objetivo do tabu Seaarch � maximizar o valor de retorno da fun��o de avalia��o.
#Basicamente TabuSearch > Fun��o de avalia��o > devolve valor real ao TabuSearch

#Fun��o solu��o
compras <- function(solucao)
{
  
  # a vari�vel solucao ser� o conjunto vetorial dos pre�os que foram transformados em bits 
  #de forma aleat�ria e iterativa pelo tabuSearch, exemplo: solucao= c(1,0,1,1,1,0,1,0,1,1,1,0)
  
  #pre�os das roupas
  valores = c(1.1,1.2,1.25,1.41,1.5,1.63,2.05,2.22,2.65,2.9,3.04,3.16)
  # Vari�veis soma e produto
  soma = 0;
  produto = 1;
#Loop para que a solu��o corra sobra cada item 
#onde o produto sera a multiplica��o de cada um com os valores  
  for (i in 1:12) {
    #Se a solu��o referente a cada valor for 1 ocorrer� a soma e a multiplica��o
    if (solucao[i]==1)
    {
      soma = soma + valores[i];
      produto = produto * valores[i];
    }
  }
  #Verifica��o se a soma e o produto s�o 7.11, caso o valor d� 7.11 retorna no vaor, 
  #caso n�o, ele retorna em 0
  if (soma==7.11 & produto==7.11)
  {
    return(7.11)
    
  }
  else
  {
    return(0)
    
  }  
  
}

#Instalar o tabuSearch caso n�o tenha
install.packages("tabuSearch")
#Chamada do pacote
library(tabuSearch)
#Vari�vel Z que captura a fun��o compras
#tabuSearch(numero de bits, num de itera��es, fun��o objeto, 
#tamanho da lista, num de reinicio, repeti��o, logs)
z = tabuSearch(size = 12, iters = 1000, objFunc = compras, 
               listSize = 9, nRestarts = 10, repeatAll = 1, verbose = T)
#Plotagem da fun��o de acordo com as buscas.
plot(z)
#resumo do trabalho feito pelo TabuSearch
summary(z)
#resumo da melhor solu��o
summary(z, verbose = TRUE)

