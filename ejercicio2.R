#####Ejercicio 2, tarea 3 de ciencia de datos     ##
#Autor:Jorge Luis Ramos Zavaleta                  ##
#jorge.ramos@cimat.mx                             ##
####################################################

#Usando lm para clasificar un conjunto haciendo uso de la relacion con
#analisis discriminante de Fisher
#Primer ejemplo
#Generamos los datos
c1<-matrix(c(seq(1,5),2,3,3,5,5),ncol=2)
c2<-matrix(c(1,2,3,3,5,6,0,1,1,2,3,5),ncol=2)
#Escalamos los datos
cm1<-rbind(c1,c2)
cm1<-scale(cm1)

#Preparamos nuestro vector de resultado
y<-c(rep(1/5,5),rep(1/6,6))
b<-lm(y~cm1) #Aplicamos lm

#Graficamos los datos originales
plot(c2,col="blue",pch=18,main="Datos sinteticos 1", xlab="x", ylab="y")
points(c1,col="red",pch=18)

#Graficamos los datos escalados con la recta de "Fisher"
plot(cm1[1:5,],col="blue",pch=18,xlim=c(min(cm1[,1]),max(cm1[,1])), 
     ylim=c(min(cm1[,2]),max(cm1[,2])),main="Datos sinteticos 1", xlab="x", ylab="y")
points(cm1[6:11,],col="red",pch=18)
abline(b$coefficients[1:2], col="green")

#Segundo ejemplo
#Generamos los datos
set.seed(2)
c11<-matrix(c(rnorm(50,2,sd=1),rnorm(50,2,sd=1)),ncol =2)
c21<-matrix(c(rnorm(52,5,sd=0.5),rnorm(52,5,sd=0.5)),ncol=2)
cm2<-rbind(c11,c21)
cm2<-scale(cm2)

#Preparamos nuestro vector de resultado
y1<-c(rep(1/50,50),rep(1/52,52))
#Aplicamos lm
b<-lm(y1~cm2)

#Graficamos los datos originales
plot(c21,col="blue",pch=18,main="Datos sinteticos 1", xlab="x", ylab="y",xlim=c(0,6),ylim=c(0,6))
points(c11,col="red",pch=18)

#Graficamos la recta de separacion
plot(cm2[1:50,],col="red",pch=18,xlim=c(min(cm2[,1]),max(cm2[,1])), 
     ylim=c(min(cm2[,2]),max(cm2[,2])),main="Datos sinteticos 2", xlab="x", ylab="y")
points(cm2[51:102,],col="blue",pch=18)
abline(b$coefficients[1:2], col="green")

#Inciso f ejercicio 2
#Se usara el mismo ejemplo 2 agregando un dato atipico

set.seed(2)
c12<-matrix(c(rnorm(50,2,sd=1),-5,rnorm(50,2,sd=1),-5),ncol =2)
c22<-matrix(c(rnorm(52,5,sd=0.5),rnorm(52,5,sd=0.5)),ncol=2)
#Calculamos los centroides
c12mean<-apply(c12,2,mean)      #Centroide con el dato atitipo 
c11mean<-apply(c11,2,mean)      #Centroide sin el dato atipico
pesoatipico<-dist(rbind(c(-5,5),c12mean))/dist(rbind(c(-5,5),c11mean))
cm3<-rbind(c12,c22)

cm3<-scale(cm3)

#Preparamos nuestro vector de resultado
y2<-c(rep(1/51,51),rep(1/52,52))
#Aplicamos lm
b<-lm(y2~cm3,weights = c(rep(1,50),pesoatipico,rep(1,52)))

#Graficamos los datos originales
plot(c22,col="blue",pch=18,main="Datos sinteticos 1", xlab="x", ylab="y",xlim=c(-5,6),ylim=c(-5,6))
points(c12,pch=18,col=c(rep("red",50),"green"))

#Graficamos la recta de separacion
plot(cm3[1:51,],col=c(rep("red",50),"green"),pch=18,xlim=c(min(cm3[,1]),max(cm3[,1])), 
     ylim=c(min(cm3[,2]),max(cm3[,2])),main="Datos sinteticos 2", xlab="x", ylab="y")
points(cm3[52:103,],col="blue",pch=18)
abline(b$coefficients[1:2], col="green")

