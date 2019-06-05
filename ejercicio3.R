#####Ejercicio 5, tarea 3 de ciencia de datos     ##
#Autor:Jorge Luis Ramos Zavaleta                  ##
#jorge.ramos@cimat.mx                             ##
####################################################

rm(list=ls())
setwd("~/ciencia de datos/Tarea3")
library(readr)

#Funcion que genera los pesos adecuados acorde con el algoritmo perceptron en linea
perceptron<-function(x,y,stepSize=0.1,pesos,tolerancia=1e-7,iter=10){
  n<- nrow(x) 
  m <- ncol(x)
  pesos <- rep(0,m)
  pesos2 <- rep(0,m)
  i<- 1
  while (i <=iter){
    for (j in 1:n){
      h = sign(t(as.double(pesos))%*%as.double(x[j,]))
      if(h != y[j]){ #Condición del perceptron en linea (se actualiza cuando encuentra un error)
        pesos = pesos  + stepSize*y[j]*x[j,] 
      }
    }
    if ( (1/n)*abs(sum(pesos2 - pesos)) < tolerancia){ #Caso de no convergencia
      print(paste0("Converge en ",i," iteraciones"))
      break 
    }
    pesos2 <- pesos # Actualización para la condición de paro
    
    if(i==iter){
      print("No converge")
    }
    i<-i+1
  }
  
  pesos # retornar los pesos modificados
}

#Funcion que genera la clasificación usando los pesos generados por el perceptron
percPred<-function(x,w){
    output <- sign(t(as.double(w))%*%as.double(x))
  output
}


#Generamos un conjuntos separable usando 2 distribuciones gaussianas distintas para el primer grupo
#y solo una gaussiana para el segundo
set.seed(215)
g1<-matrix(rnorm(100,mean = 1),nrow = 50,ncol = 2,byrow = T)
g2<- matrix(rnorm(100,mean = 3,sd=0.5),nrow = 50,ncol = 2,byrow = T)
g3<-matrix(rnorm(100,mean = 6),nrow = 50,ncol = 2,byrow = T)
etiquetas<-c(rep(-1,100),rep(1,50)) # Las etiquetas de clasificacion correcta usando -1,1
#Grafica de la clasificación original
plot(rbind(g1,g2,g3),col=c(rep(4,100),rep(2,50)),main="Distribucion original",xlab="x",ylab="y")

#Agregamos el intercepto
datos<-cbind(rep(1,150),rbind(g1,g2,g3))
resultado<-perceptron(datos,etiquetas,stepSize =0.5,1,tolerancia =1e-8,200)

#Predecir las clases usando los pesos previamente calculados "salida"
clasific<-apply(datos,1,percPred,resultado)
table(clasific,etiquetas)
clasPlot<-ifelse(clasific>0,1,2)
plot(rbind(g1,g2,g3),col=as.factor(clasPlot), main="Clasificación con perceptron",xlab="x",ylab="y")


#Perceptron en los datos pima
pimaTrain<-read_table2("pima.tr")
pimaTest<-read_table2("pima.te")

train<-as.matrix(pimaTrain)

#Debido a que los datos de prueba tienen la ultima columna como "si" o "no" se realiza un
#encoding para 
parser <- list(Yes=1, No = -1)
foo <- unlist(parser[pimaTest$type])
names(foo) <- NULL
pimaTest$type <- foo

test<-as.matrix(pimaTest)
train<-cbind(rep(1,nrow(train)),train) #Les ponemos el intercepto
test<-cbind(rep(1,nrow(test)),test)
etiquetas2<-train[,9]
etiquetas3<-test[,9]
etiquetas2[etiquetas2==0] = -1


train<-train[,1:8]
test<-test[,1:8]

#Generando el vector de perceptron
resultado3<-perceptron(train,etiquetas2,stepSize=0.5,pesos=1,tolerancia =1e-9,220)



#Clasificacion con perceptron para datos de entrenamiento
clasific2<-apply(train,1,percPred,resultado3)
#Tabla de clasificacion para datos de entrenamiento
table(clasific2,etiquetas2)


#Clasificacion con perceptron para datos de prueba

clasific3<-apply(test,1,percPred,resultado3)
#Tabla de clasificacion para datos de prueba
table(clasific3,etiquetas3)


