# Naive Bayes e redes Bayesianas
#instala��o do Pacote para Naive Bayes e
install.packages("bnlearn")
#carregamento do pacote
library(bnlearn)
#Pacote Caret
install.packages("caret", dependencies=T)
library(caret)

#� necess�rio criar a parti��o dos dados em treino e teste, para posteriormente gerar a predi��o
# Vari�vel parti��o chamando a fun��o createDataPartition(quantidade de registros, probabilidade para treino)
# Obs.: As parti��es s�o geradas com amostragem aleat�ria sem reposi��o
particao = createDataPartition(1:20000,p=.7)
# Cria��od da vari�vel com os dados de treino e teste
segurotreino = insurance[particao$Resample1,]
seguroteste = insurance[- particao$Resample1,]

# Verifica��o da diments�o dos dados
dim(segurotreino)
dim(seguroteste)
# Gera��o do Modelo usando a fun��o naive.bayes(conjunto de dados, classe especial)
modelo = naive.bayes(x=segurotreino,training = "Accident")
modelo
# Plotagem da vari�vel, para ver sua �rvore de depend�ncia
plot(modelo)

#Gera��o da previs�o usando o predict(modelo criado, dados de teste)
previsao  = predict(modelo, seguroteste)
previsao
# Gera��o da matriz de confus�o 
confusionMatrix(previsao, seguroteste$Accident)

######################################################


# Redes Bayesianas
# Fazendo a indu��o da estrutura de rede com a fun��o hc(conjunto de dados)
res <- hc(insurance)
plot(res)

#Gera��o do modelo e as tabelas de distribui��o de probabilidade
# Fun��o bn.fit(estrutura, dados)
modelo <- bn.fit(res, data = insurance)
#Verifica��o de um dos cabe�alhos da tabela
modelo$GoodStudent

# cpquery(modelo, evento a inferir e as evidencias que s�o os atributos com seus respectivos dados)
cpquery(modelo, event =( Accident=="Moderate" | Accident=="Severe" ), 
        evidence=(Age=="Senior" & RiskAversion=="Adventurous" & MakeModel == "SportsCar"))
#estes acima seriam os dados a se averiguar no caso de um cliente especifico.

