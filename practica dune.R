######DUNE######

##PRESENTACI0N DE LOS DATOS

library(ade4)
library(vegan)
data(dune)
head(dune)
summary(dune)
dim(dune)
data(dune.env)
head(dune.env)
summary(dune.env)
dim(dune.env)

#A1

med=mean(dune.env$A1);med #media de las observaciones de A1
desv.tip=sd(dune.env$A1);desv.tip #desviacion tipica de las observaciones de A1
windows()
layout(mat=matrix(c(1,2),2,1,byrow=TRUE),height = c(3,7))
par(mar=c(0,3.1, 1.1, 2.1))
boxplot(dune.env$A1, horizontal=TRUE , ylim=c(2,12), xaxt="n" , col=rgb(0.8,0.8,0,0.5) , frame=F)
par(mar=c(4, 4.1, 1.1, 2.1))
hist(dune.env$A1 , breaks=10 , col=4, main="" , xlab="grosor de A1", ylab="nº de lugares con el suelo de tal grosor" ,xlim=c(2,12))
abline(v=med,col=1, lty=2, lwd=3)
abline(v=med+desv.tip,col=2,lwd=1.5)
abline(v=med-desv.tip,col=2,lwd=1.5)
legend(7.5,5.5, c("Media del grosor de A1","Media +- Desviacion tipica"),col=c(1,2),lty=c(2,1),lwd=c(3,1.5),cex=0.95)

windows()
plot(dune.env$A1)
boxplot(dune.env$A1)
hist(dune.env$A1,breaks=10)

#Variables categoricas

windows()
par(mfrow=c(2,2))
barplot(table(dune.env$Moisture),col="lightcoral",main="Moisture",xlab="grupo",ylab=" nº de observaciones")
barplot(table(dune.env$Manure),col="palegreen",main="Manure",xlab="grupo",ylab="nº de observaciones")
barplot(table(dune.env$Use),col="lightcyan2",main="Use",xlab="grupo",ylab="nº de observaciones")
barplot(table(dune.env$Management),col="plum2",main="Management",xlab="grupo",ylab="nº de observaciones")

##ANALISIS EXPLORATORIO

windows()
r<-apply(dune>0,1,sum)
barplot(r,ylim=c(0,15),main="Riqueza de especies",ylab="nº especies", xlab="Lugares",col='darkolivegreen1')

windows()
fr1<-apply(dune>0,2,sum)
fr<-100*fr1/nrow(dune)
hist(fr,breaks=10,xlim=c(0,100),xlab="porcentaje de aparicion (%)",ylab="nº de especies",col="navajowhite",main="Frecuencia relativa de especies")
grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)
hist(fr,breaks=10,add=TRUE,xlim=c(0,100),xlab="porcentaje de aparicion (%)",ylab="nº de especies",col="navajowhite",main="Frecuencia relativa de especies")

windows()
barplot(dune.env$A1,ylim=c(0,13),xlab="Lugares",ylab="Grosor de la capa A1", main="A1",names.arg = c(1:20),col='lightyellow2')

par(mfrow=c(2,2))
plot(x=c(1:20),y=dune.env$Moisture,ylim=c(0,5.5),type="h",main="Moisture",xlab="Lugares",ylab="Niveles",col="coral")
plot(x=c(1:20),y=dune.env$Manure,ylim=c(0,5.5),type="h",main="Manure",xlab="Lugares",ylab="Niveles",col='green4')
plot(x=c(1:20),y=dune.env$Management,ylim=c(0,5.5),type="h",main="Use",xlab="Lugares",ylab="Niveles",col="blue")
plot(x=c(1:20),y=dune.env$Use,ylim=c(0,5),type="h",main="Management",xlab="Lugares",ylab="Niveles",col="mediumorchid")

##EJEMPLO VARIABLES CUALITATIVAS (MODO Q)

env.ej<-dune.env[1:7,c(3,4)]
attach(env.ej)

#Creación de variables binarias
env.ej$UseHayf<-(Use == "Hayfield")*1
env.ej$UseHayp<-(Use == "Haypastu")*1
env.ej$UsePast<-(Use == "Pasture")*1
env.ej$ManagemBF<-(Management == "BF")*1
env.ej$ManagemHF<-(Management == "HF")*1
env.ej$ManagemNM<-(Management == "NM")*1
env.ej$ManagemSF<-(Management == "SF")*1
bin.ej<-env.ej[,3:9]

#EJEMPLO RELACIONES DIRECTAS INVERSAS Y SIN CORRELACION


windows()
par(mfrow=c(1,3))
x <- 1:100

#relación lineal directa
set.seed(1)
y <- x + rnorm(100, mean = 0, sd = 10)
plot(x, y, pch = 19)
abline(lm(y ~ x), col = "red", lwd = 3)
r1<-round(cor(x, y), 2)
text(paste("Correlación:",r1), x = 15, y = 100,col='red')

#sin relación lineal
set.seed(1)
y <- x + rnorm(100, mean = 0, sd = 400)
plot(x, y, pch = 19)
abline(lm(y ~ x), col = "blue", lwd = 3)
r2<-round(cor(x, y), 2)
text(paste("Correlación:", round(cor(x, y), 2)), x = 15, y = 900,col="blue")

#relación lineal inversa
set.seed(1)
y <- -x + rnorm(100, mean = 0, sd = 10)
plot(x, y, pch = 19)
abline(lm(y ~ x), col = "green", lwd = 3)
r3<-round(cor(x, y), 2)
text(paste("Correlación:",r3), x = 80, y = 5,col='green')

#2.4-APLICACIONES DE LOS COEFICIENTES DE SIMILITUD

#######1.LUGARES DE DUNE#######

#DISIMILITUD DE BRAY-CURTIS
disBC<-vegdist(dune)

#DISTANCIA DE HELLINGER
dune.hel<-decostand(dune,'hel')  #se transforman los datos por la transformación Hellinger
disHel<-dist(dune.hel)
disHel2<-vegdist(dune,method='hellinger') 

#######2.VARIABLES DE DUNE#######
coldune<-colnames(dune)
colnames(dune)<-c(1:30)

#DISTANCIA CHI-CUADRADO
dunev.chi<-decostand(t(dune),'chi.square')
disChi<-dist(dunev.chi)  
disChi2<-vegdist(t(dune),method='chisq')  

#CORRELACION DE PEARSON
#Busquemos las variables con más ceros para eliminarlas (nos quedaremos con 20)
matdune<-as.matrix(dune)

for (i in 1:20){
  for (j in 1:30){
    if (matdune[i,j]==0){
      matdune[i,j]<-1
      }else matdune[i,j]<-0
    
  }
  
}
vecdune<-apply(matdune,2,sum)

espcorr<-vecdune[vecdune<16] #nos quedamos con las variables que tengan menos de 16 ceros
dd<-dune[,c(rownames(data.frame(espcorr)))]

deppear<-round(cor(dd),3)   #correlación de Pearson sobre las variables con menos ceros

#######3.LUGARES DE DUNE.ENV#######

#Gower

library(cluster)

disGow<-daisy(dune.env,metric='gower')

library(FD)

disGow2<-gowdis(dune.env,ord='classic')

#######4.VARIABLES DE DUNE.ENV#######

#Chi-cuadrado

tabla1<-table(Moisture,Management)
chisq.test(tabla1,correct = FALSE)

tabla2<-table(Moisture,Use)
chisq.test(tabla2,correct = FALSE)

tabla3<-table(Moisture,Manure)
chisq.test(tabla3,correct = FALSE)

tabla4<-table(Management,Use)
chisq.test(tabla4,correct = FALSE)

tabla5<-table(Use,Manure)
chisq.test(tabla5,correct = FALSE)

tabla6<-table(Management,Manure)
chisq.test(tabla6,correct = FALSE)

#ANOVA
windows()
boxplot(A1 ~ Moisture,col=c(6,2,3,4))

anova<-aov(lm(A1 ~ Moisture))
shapiro.test(anova$residuals) 
bartlett.test(anova$residuals ~ Moisture)

kruskal.test(A1,Moisture)

#REPRESENTACIÓN GRÁFICA DE LAS DISIMILITUDES ANTERIORES

library(gclus)
source("coldiss.R")

windows()
coldiss(disBC,byrank=FALSE,diag=TRUE)
coldiss(disHel,byrank=FALSE,diag=TRUE)

coldiss(disChi,byrank=FALSE,diag=TRUE)

library(psych)

corPlot(deppear, cex = 0.65,main='')

coldiss(disGow,byrank=FALSE,diag=TRUE)

#EJEMPLO CLUSTER JERARQUICO Y NO JERARQUICO

windows()

clust.hel.ward<-hclust(disHel,method='ward.D')
plot(clust.hel.ward,main='Método de Ward',xlab='Lugares de dune')
rect.hclust(clust.hel.ward, k = 4,border=c('red','green','blue','purple'))
legend(15,3.2, c("Grupo 1","Grupo 2","Grupo 3","Grupo 4"),col=c('red','green','blue','purple'),lty=c(1,1,1,1))


clust.hel.kmeans<-kmeans(dune.hel,centers=4)
