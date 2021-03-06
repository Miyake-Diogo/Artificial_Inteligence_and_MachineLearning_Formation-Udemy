# Forma��o IA e ML - Udemy
# Processamento de Linguagem Natural - NLP

#####################################

# Exemplo B�sico 
# Instalar e carregar os pacotes
install.packages("udpipe", dependencies = T)
library(udpipe)

FILE = "Nossa vida � controlada por algoritmos,"
# ID como um inteiro
ID = 1L
# Transforma File e ID em um data frame para organizar
arquivo = data.frame(FILE,ID)
#Transforma file me caractere
arquivo$FILE = as.character(arquivo$FILE)


#download do modelo
modelo = udpipe_download_model(language = "portuguese-br")

#carregar o modelo do disco
modelo = udpipe_load_model(file.choose())

#anotacoes no fromato conll-u
# udpipe_annotate(modelo, arquivo, ID)
anotar = udpipe_annotate(modelo, x = arquivo$FILE, doc_id = arquivo$ID)
anotar
anotar = as.data.frame(anotar) # Estrutura os dados em data frame
colnames(anotar)

fix(anotar)
###########################################

# Exemplo com documento

# library(udpipe)
# Carregamento do arquivo
arquivo <- read.csv(file.choose(), sep=";", encoding = "UTF-8")
# transformar o identificador em inteiro e o arquivo em caractere
arquivo$ID = as.integer(arquivo$ID)
arquivo$FILE = as.character(arquivo$FILE)
# Verifica��o dos dados
dim(arquivo)
colnames(arquivo)
# Abertura dos modelos
modelo = udpipe_load_model(file.choose())


#anotacoes no fromato conll-u
anotar = udpipe_annotate(modelo, x = arquivo$FILE, doc_id = arquivo$ID)
anotar = as.data.frame(anotar)
fix(anotar)
dim(anotar)

# Mostrar as anota��es de p�s tagging
anotar$upos

#sumariza frequencia upos
frequencia = txt_freq(anotar$upos)
# Gera��o de um grafico de barplot
barplot(frequencia$freq, col = gray.colors(14),xlab="Tipo",ylab="Frequ�ncia", names.arg = frequencia$key)


#palavras chave - combina��o
# padroniza��o dos tokens
anotar$palavra = tolower(anotar$token)
# Elementos em combina��o
estatisticas = keywords_collocation(x = anotar, term="token", group = "doc_id")
# Estatistica chava para Produ��o dos resultados
estatisticas$chave = factor(estatisticas$keyword, levels= rev(estatisticas$keyword))
# Frequencia da estatistica chave
estatisticas2 = head(subset(estatisticas,freq>2))

# gera��o do gr�fico
barplot(estatisticas2$pmi, col = gray.colors(14),xlab="Tipo",ylab="Frequência", names.arg =estatisticas2$chave )

################################################
# Exemplo de classifica��o - NLP
# Carregar o pacote udpipe
library(udpipe)
# Carregar o arquivo reuterscorn-train
arquivo <- read.csv(file.choose(), sep=",", encoding = "UTF-8", header = F, quote="\"")

colnames(arquivo) = c("FILE","CLASS")
#1450 linhas
dim(arquivo)[1]
head(arquivo)
# Transforma��o em inteiros e caracteres
arquivo$FILE = as.character(arquivo$FILE)
arquivo$CLASS = as.character(arquivo$CLASS)

# Gera��o de uma coluna com os identificadores
arquivo$ID = seq(1:dim(arquivo)[1])


modelo = udpipe_download_model(language = "english")

modelo = udpipe_load_model(file.choose())

# Processo de anota��o
anotar = udpipe_annotate(modelo, x = arquivo$FILE, doc_id = arquivo$ID)

# estruturar as anota��es em um Data Frame
anotar = as.data.frame(anotar)
head(anotar)
dim(anotar)


#filtra apenas verbos, substantetivos e adjetivos
anotar2 = anotar[anotar$upos %in% c("VERB", "NOUN", "ADJ","ADV"), ]
dim(anotar2)


#Transformar o identificar em um com apenas duas colunas  doc_id, lemma, assim fica mais f�cil para classificar
anotar2 = anotar2[,c(1,7)]

# Cria��o da matriz de termos frequentes
x <- document_term_frequencies(anotar2[, c("doc_id", "lemma")])
dtm <- document_term_matrix(x)
dim(dtm)
summary(dtm)
colnames(dtm)
rownames(dtm)
head(dtm)

# transformando a matriz de termos frequentes em um dataframe
df = as.data.frame(as.matrix(dtm))
#numero do doc esta no nome da linha, vamos passar para um coluna para fazer um merge com o documento original e pegar a classe
df$doc_id =   rownames(df)
df$doc_id = as.integer(df$doc_id)

# Mesclar os identificadores com o orifinal, para poder ter acesso a classe ao dataframe
df2 = merge(df,arquivo, by.x =c("doc_id") ,by.y = c("ID")  )
#REMOVEMOS A COLUNA COM ARQUIVO QUE FOI CRIADA NO MERGE
df2$FILE =  NULL
head(df2$CLASS,n=100)

dim(df2)

#install.packages("caret", dependencies=T)
library(caret)

#install.packages("FSelector", dependencies=T)
library(FSelector)

#selecao de atributos, usar as mais interessantes para classifica��o
atributos <- information.gain(CLASS ~., df2)
# Sele��o dos 20 atributos maus importantes
atributos2 = cutoff.k(atributos,20)
# cria a formula para o processo de classifica��o de forma autom�tica
formula =  as.simple.formula(atributos2, class="CLASS")

#classe em fator
df2$CLASS = as.factor(df2$CLASS)

#particao em treino e teste
particao = createDataPartition(1:dim(df2)[1],p=.7)
dftreino = df2[particao$Resample1,]
dfteste = df2[- particao$Resample1,]
dim(dftreino)
dim(dfteste)


#executando random forest
library(randomForest)
# Cria��o do modelo e dos conjuntos de dados 
modelo = randomForest(formula, data = dftreino, ntree=500 ) 
previsao  = predict(modelo, dfteste)
# Verifica��o da previs�o e da matriz de confus�o
previsao
confusionMatrix(previsao, dfteste$CLASS)





