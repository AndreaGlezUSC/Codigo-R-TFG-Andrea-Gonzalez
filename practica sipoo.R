######SIPOO######

##PRESENTACION DE LOS DATOS

library(ade4)
library(vegan)
data(sipoo)
head(sipoo)
summary(sipoo)
dim(sipoo)
data(sipoo.map)
head(sipoo.map)
summary(sipoo.map)

##ANALISIS EXPLORATORIO

windows()
coords_inter<-sipoo.map[,-3]
coords<-coords_inter[c("E","N")]
coords
rownames(coords)<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)

windows()
lug<-rowSums(sipoo)
plot(coords,asp=1,type="n",xlab="Coordenada este (E)",ylab="Coordenada norte (N)", main= "Riqueza de especies por isla")
points(coords,cex=7*lug/max(lug),pch=19,col="grey")
text(coords,row.names(coords),pos=3)

windows()
esp<-colSums(sipoo)
esp
hist(esp,xlim=c(0,20),ylim=c(0,30),xlab="nº de lugares",ylab="nº de especies",col="navajowhite",main="Frecuencia absoluta de especies")

#2.4-APLICACIONES DE LOS COEFICIENTES DE SIMILITUD

######### 1.LUGARES #########

#DISTANCIA DE JACCARD
rowsipoo<-rownames(sipoo)
rownames(sipoo)<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)
disjac1<-vegdist(sipoo,"jac",binary=TRUE)
round(disjac1,3)
disjac2<-dist(sipoo,'binary')
disjac2<-round(disjac2,3)
dis.jac<-as.matrix(disjac2)
dis.jac
disjac3<-dist.binary(sipoo,method=1)
round(disjac3,3)

library(xtable)
xtable(dis.jac, type = "latex", file = "tablas.tex")

#INDICE DE JACCARD
A<-matrix(1,nrow=18,ncol=18)
sim.jac<-A-dis.jac

#DISIMILITUD DE SORENSEN

dissor1<-vegdist(sipoo,binary=TRUE)
dissor1<-round(dissor1,3)
dis.sor<-as.matrix(dissor1)
dis.sor
max(dis.sor)
dissor2<-dist.binary(sipoo,method=5)
round(dissor2,3)

######### 2.VARIABLES #########
colsipoo<-colnames(sipoo)
colnames(sipoo)<-c(1:50)
depochi<-dist.binary(t(sipoo),method=7)
depochi<-round(depochi,3)

#REPRESENTACIÓN GRÁFICA DE LAS DISIMILITUDES ANTERIORES

library(gclus)
source("coldiss.R")

windows()
coldiss(disjac2,byrank=FALSE,diag=TRUE)
coldiss(dissor1,byrank=FALSE,diag=TRUE)
coldiss(depochi,byrank=FALSE,diag=TRUE)

#ILUSTRACION DEL CLUSTERING JERARQUICO DE MANERA MANUAL

dis.ej<-dist(sipoo[1:5,],'binary')

clust.ej.simple<-hclust(dis.ej,method='single')
plot(clust.ej.simple,lwd=4, main='',)

#EJEMPLO CLUSTER JERARQUICO

windows()

clust.jac.simple<-hclust(disjac1,method='single')
plot(clust.jac.simple,lwd=3, main='Método de enlace por distancia mínima',xlab='Islas de Sipoo')

clust.jac.complete<-hclust(disjac1,method='complete')
plot(clust.jac.complete,lwd=3,main='Método de enlace por distancia máxima',xlab='Islas de Sipoo')
rect.hclust(clust.jac.complete, k = 4)

clust.jac.average<-hclust(disjac1,method='average')
plot(clust.jac.average,lwd=3,main='Método de enlace por distancia media',xlab='Islas de Sipoo')
rect.hclust(clust.jac.average, k = 4,border=c(2:5))  

#CORRELACIÓN COFENÉTICA
library(stats)
clust.jac.simple.coph<-cophenetic(clust.jac.simple)
cor(disjac1,clust.jac.simple.coph)

clust.jac.complete.coph<-cophenetic(clust.jac.complete)
cor(disjac1,clust.jac.complete.coph)

clust.jac.average.coph<-cophenetic(clust.jac.average)
cor(disjac1,clust.jac.average.coph)

#AGRUPAMIENTO DE LAS ISLAS CON EL MÉTODO DE DISTANCIA MEDIA SOBRE EL MAPA DE LAS ISLAS

windows()
coords_inter<-sipoo.map[,-3]
coords<-coords_inter[c("E","N")]
coords
plot(coords,asp=1,type="n",xlab="Coordenada este (E)",ylab="Coordenada norte (N)", main= "Islas de Sipoo")
k=4
divgrupos<-cutree(clust.jac.average,k=k)
for (i in 1:k){
  points(coords[divgrupos==i,1],coords[divgrupos==i,2],pch=i+20,col=i+1,cex=3,bg=i+1)
  
}

text(coords,row.names(coords),cex=0.7,col=1,font=2)
legend(417000,6670500,paste('Grupo',1:k),pch=(1:k)+20,col=2:(k+1),pt.bg=2:(k+1),pt.cex=2,bty='n',cex=1.2)
