rm(list=ls())
setwd("~/ciencia de datos/Tarea3")
library('Rcpp')
library('inline')
library('RcppArmadillo')


#Funcion en Rcpp que genera la matriz de Gram para 2 posibles kernels: Gaussiano que se obtiene
#poniendo en metodo=1 y polinomial poniendo metodo=2
cppFunction("arma::mat GramMat(arma::mat A, double parametro, int parametro2, int metodo){
            arma::mat resultado(A.n_rows,A.n_rows);
            double temp;
              for (int i=0; i<A.n_rows; i++){
              for (int j=i; j<A.n_rows; j++){ //calculamos la parte triangular inferior de la matriz
                if (metodo==1){
                  temp=  (double) -1*parametro*dot(A.row(i)-A.row(j),A.row(i)-A.row(j));
                  resultado(i,j)=  exp(temp);
                }
                else if (metodo==2){
                      temp=  (double) parametro + dot(A.row(i)-A.row(j),A.row(i)-A.row(j));
                  resultado(i,j)=  pow(temp,parametro2);
                }
              }
              }
for (int i=0; i<A.n_rows; i++){   //Aprovechando la simetría de la matriz copiamos las demás entradas
  for (int j=0; j<i; j++){
    resultado(i,j)=resultado(j,i);
  }
}
          return resultado;}",depends="RcppArmadillo") 

source("kfda2.R")

###Generamos los datos para probar que la implementación funciona

set.seed(1000)   #Sinusoidal
N <- 200
sinusoidal <- function(m,noise=0.2) 
{
  x1 <- c(1:2*m)
  x2 <- c(1:2*m)
  for (i in 1:m) {
    x1[i] <- (i/m) * pi
    x2[i] <- sin(x1[i]) + rnorm(1,0,noise)
  }
  for (j in 1:m) {
    x1[m+j] <- (j/m + 1/2) * pi
    x2[m+j] <- cos(x1[m+j]) + rnorm(1,0,noise)
  }
  target <- c(rep(+1,m),rep(-1,m))
  return(data.frame(x1,x2,target))
}

dataset <- sinusoidal(N)      #Datos de la sinusoidal
dataset[,3]<-ifelse (dataset[,3]>0,1,0)
#Cluster originales
plot(dataset[,-3],col=as.factor(dataset$target), main="Sinusoide original", xlab="x", ylab="y")
#Aplicando kfda con kernel gaussiano 0.074
#for (i in seq(0.05,.1,.001)){
resultado<-kfda(as.matrix(dataset[,-3]),as.matrix(dataset[,-3]),200,200,parametro=0.074,shift=0.01)

print(paste("eficiencia",(1-sum(abs(resultado==dataset[,3]))/length((dataset[,3])))*100))
#}
#Dibujando la clasificación con kfda 
plot(dataset[,-3],col=as.factor(resultado), main=paste("Sinusoide KFDA"), xlab="x", ylab="y")

#Probando con los datos pima
library(readr)
pima_training<-read_table2("pima.tr")
pima_test<-read_table2("pima.te")


Z<-as.matrix(pima_training)
parser <- list(Yes=1, No = 0)
foo <- unlist(parser[pima_test$type])
names(foo) <- NULL
pima_test$type <- foo
Z2<-as.matrix(pima_test)

#Aplicando kfda a los datos pima
resultado1<-kfda(Z2,Z,109,223,parametro=0.002,shift=0.01)
print(paste("La precisión es de",(sum(abs(resultado1==Z2[,8]))/length((Z2[,8])))*100,"%"))

#Aplicando LDA a los datos pima
library(MASS)
resultado2<-lda(type~.,data=pima_training)
predmodel.test.lda = predict(resultado2, newdata=pima_test)
a<-as.data.frame(table(Predicho=predmodel.test.lda$class, Actual=Z2[,8]))
print(paste('Precision',1-(a[2,3]+a[3,3])/sum(a[,3]))) 
