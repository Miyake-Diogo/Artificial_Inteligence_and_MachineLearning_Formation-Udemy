# Machine Learning - Cluster - KMeans

# Instalacao do pacote e carregamento
install.packages("fpc")
library(fpc)

install.packages("cluster")
library(cluster)

# Uso da função kmeans(conjunto de dados iris com 4 colunas, e 3 centros )
cls = kmeans(iris[,1:4],centers=3)
# Verificação dos clusters, cada instancia pertencendo a uma classe
cls$cluster
# Verificação dos centros para agrupamento de cada classe
cls$centers

# Geração de uma tabela com as especies e os agrupamentos
table(iris$Species, cls$cluster)

# Plotagem do agrupamento pelas colunas
plotcluster(iris[1:4],cls$cluster)
# Plotagem dos agrupamentos de acordo com os parametos
clusplot(iris[1:4],cls$cluster)
# Plotagem de duas colunas de iris, cor= especies, pontos=agrupamento
# pela cor se vê a classe real, e com o formato do ponto se ve a que ponto o ite pertence
plot(iris[,3:4], col=iris$Species, pch=cls$cluster)
# Preto = Setosa, Vermelha = Versicolor, Verde = Virginica
