# Naive Bayes e redes Bayesianas
#instalação do Pacote para Naive Bayes e
install.packages("bnlearn")
#carregamento do pacote
library(bnlearn)
#Pacote Caret
install.packages("caret", dependencies=T)
library(caret)

#É necessário criar a partição dos dados em treino e teste, para posteriormente gerar a predição
# Variável partição chamando a função createDataPartition(quantidade de registros, probabilidade para treino)
# Obs.: As partições são geradas com amostragem aleatória sem reposição
particao = createDataPartition(1:20000,p=.7)
# Criaçãod da variável com os dados de treino e teste
segurotreino = insurance[particao$Resample1,]
seguroteste = insurance[- particao$Resample1,]

# Verificação da dimentsão dos dados
dim(segurotreino)
dim(seguroteste)
# Geração do Modelo usando a função naive.bayes(conjunto de dados, classe especial)
modelo = naive.bayes(x=segurotreino,training = "Accident")
modelo
# Plotagem da variável, para ver sua árvore de dependência
plot(modelo)

#Geração da previsão usando o predict(modelo criado, dados de teste)
previsao  = predict(modelo, seguroteste)
previsao
# Geração da matriz de confusão 
confusionMatrix(previsao, seguroteste$Accident)

######################################################


# Redes Bayesianas
# Fazendo a indução da estrutura de rede com a função hc(conjunto de dados)
res <- hc(insurance)
plot(res)

#Geração do modelo e as tabelas de distribuição de probabilidade
# Função bn.fit(estrutura, dados)
modelo <- bn.fit(res, data = insurance)
#Verificação de um dos cabeçalhos da tabela
modelo$GoodStudent

# cpquery(modelo, evento a inferir e as evidencias que são os atributos com seus respectivos dados)
cpquery(modelo, event =( Accident=="Moderate" | Accident=="Severe" ), 
        evidence=(Age=="Senior" & RiskAversion=="Adventurous" & MakeModel == "SportsCar"))
#estes acima seriam os dados a se averiguar no caso de um cliente especifico.

