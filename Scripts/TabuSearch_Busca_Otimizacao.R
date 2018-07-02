# Algoritmos de busca e otimização
#Problema da loja 711: cliente compra roupas para a sua familia e as roupas
#não servem, ele vai devolver e e a atendente multiplica a devolução e da 7.11,
# o cliente reclama e diz que ela deveria ter somado , 
# a caixa se desculpa e ela soma a devolução e dá 7.11, tendo a lista qual os produtos
#que somados e multiplicados equivalem a 7.11?

#valores = 1.10,1.20,1.25,1.41,1.5,1.6,2.05,2.22,2.65,2.9,3.04,3.16
# A melhor forma é usar o tabuSearch com a função binária.
# Objetivo do tabu Seaarch é maximizar o valor de retorno da função de avaliação.
#Basicamente TabuSearch > Função de avaliação > devolve valor real ao TabuSearch

#Função solução
compras <- function(solucao)
{
  
  # a variável solucao será o conjunto vetorial dos preços que foram transformados em bits 
  #de forma aleatória e iterativa pelo tabuSearch, exemplo: solucao= c(1,0,1,1,1,0,1,0,1,1,1,0)
  
  #preços das roupas
  valores = c(1.1,1.2,1.25,1.41,1.5,1.63,2.05,2.22,2.65,2.9,3.04,3.16)
  # Variáveis soma e produto
  soma = 0;
  produto = 1;
#Loop para que a solução corra sobra cada item 
#onde o produto sera a multiplicação de cada um com os valores  
  for (i in 1:12) {
    #Se a solução referente a cada valor for 1 ocorrerá a soma e a multiplicação
    if (solucao[i]==1)
    {
      soma = soma + valores[i];
      produto = produto * valores[i];
    }
  }
  #Verificação se a soma e o produto são 7.11, caso o valor dê 7.11 retorna no vaor, 
  #caso não, ele retorna em 0
  if (soma==7.11 & produto==7.11)
  {
    return(7.11)
    
  }
  else
  {
    return(0)
    
  }  
  
}

#Instalar o tabuSearch caso não tenha
install.packages("tabuSearch")
#Chamada do pacote
library(tabuSearch)
#Variável Z que captura a função compras
#tabuSearch(numero de bits, num de iterações, função objeto, 
#tamanho da lista, num de reinicio, repetição, logs)
z = tabuSearch(size = 12, iters = 1000, objFunc = compras, 
               listSize = 9, nRestarts = 10, repeatAll = 1, verbose = T)
#Plotagem da função de acordo com as buscas.
plot(z)
#resumo do trabalho feito pelo TabuSearch
summary(z)
#resumo da melhor solução
summary(z, verbose = TRUE)

