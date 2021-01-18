#Deber 01 - Caso de Estudio Clustering 

#Nombre: Wilson Alfredo Quelal Godoy

#########Cargar Datos########

#Importar Data set

#install.packages("readr")

library(readr)
segmentation <- read_csv("C:/Users/hp/Downloads/segmentation_data.csv")
View(segmentation_data)

segmentation = data.frame(segmentation)
segmentation

#####Escalamiento Multidimensional######

d = dist(segmentation[,2:4], method = "euclidean")
d

#### Matriz de correlaciones####

#install.packages("corrplot")

library(corrplot)
corrplot(c)
c=cor(segmentation[,2:4])
c

fit = cmdscale(d,eig=TRUE, k=2)
fit

x = fit$points[,1]
x
y = fit$points[,2]
y
plot(x,y)
text(x, y, labels = row.names(segmentation), cex=1) 

#####Identificacion de las Clases#####
clase = segmentation[,1] 
clase
plot(x,y,col=c(1:4)[clase], main = "segmentation Dataset Original")

#####Crear Grupos K-Means#####

grupos = kmeans(segmentation[,2:4],4) 
grupos
g1 = grupos$cluster
g1
g2 = grupos$size
g2
plot(x,y,col=c("green","orange","blue","red")[g1], main = "Segmentation Dataset K-Means")


#########Crear Grupos: Jerarquico#########

library("dendextend")
hc = hclust(d, method = "complete" ) 
hc

clus3 = cutree(hc, 4)
clus3

dend = as.dendrogram(hc)
dend
dend = color_branches(dend, 4) 
dend
colors = c("green", "orange", "blue","red") 
plot(dend, fill = colors[clus3], cex = 0.1 , main = "Clustering Jerarquico")


######### Elbow #########
# Crea diferentes valores de k
wi = c()
for (i in 1:10) #Variacion de "k-grupos" desde 1 hasta 10
{
  g = kmeans(segmentation[,2:4],i) 
  wi[i] = g$tot.withinss 
}
plot((1:length(wi)),wi, xlab="Numero de Clusters", ylab="SSE: Suma Cuadrados Internos", pch=19, col="red", type = "b")

######### Validacion Interna #########
#Indice de Dunn

install.packages("cluster")
install.packages("clValid")
library(cluster)
library(clValid)

du1 = dunn(d,g1) 
du1
du2 = dunn(d,clus3)
du2

#Coeficiente de Silueta

sil1 = silhouette(g1,d) # Silueta N
sil1
plot(sil1,col=c("green", "orange", "blue","red"), border=NA)

sil2 = silhouette(clus3,d)
sil2
plot(sil2,col=c("green", "orange", "blue","red"), border=NA)

######### Validacion Externa #########
# ARI, AMI, NMI
install.packages("aricode")
library(aricode)
library(plyr)

#ARI, Adjusted Rand Index

ground = segmentation[,4]
ground

ground = revalue(ground, c('1'="VIP",'2'="VIPPOTENCIAL",'3'="NUevos",'4'="Bajafrecuancia"))
ground

ARI1= ARI(ground,g1)
ARI1

ARI2= ARI(ground,clus3)
ARI2

#AMI, Adjusted Mutual Information

AMI1= AMI(ground,g1)
AMI1

#NMI, Normalized Muatual Information

NMI1= NMI(ground,g1,variant = c("joint"))
NMI1
