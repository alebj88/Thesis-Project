#Publicado en:
#https://alebj88.shinyapps.io/ShinyExploratory/

library(DT)
library(ggmap)
library(ggplot2)
library(tidyr)
library(devtools)
library(ggbiplot)
library(dplyr)
library(caret)
#library(nnet)
#library(psych)
#library(matlib)
#library(blockmatrix)
#library(ggord)
#library(klaR)

#Funciones necesarias.//////////////////////////////////////////////////////////

#Funcion myplclust.
myplclust <- function(hclust,lab = hclust$labels,
                      lab.col = rep(1,length(hclust$labels)),hang =0.1,...){
    y <- rep(hclust$height,2)
    x <- as.numeric(hclust$merge)
    y <- y[which(x<0)]
    x <- x[which(x<0)]
    x <- abs(x)
    y <- y[order(x)]
    x <- x[order(x)]
    plot(hclust,labels=FALSE,hang = hang,...)
    text(x = x,y = y[hclust$order]-(max(hclust$height)*hang)
         ,labels = lab[hclust$order],col = lab.col[hclust$order],srt = 90,
         adj = c(1,0.5),xpd = NA,...)
}

#Funcion para calcular la matriz de autovectores conociendo los scores y la
#matriz de datos.
autoVecMat <- function(data,scores){
    #Numero de variables den data
    m <- dim(data)[2]
    n <- dim(scores)[2]
    #Resolvemos cada sistema de ecuaciones y 
    #almacenamos los resultados en una lista.
    k<-apply(scores,2,function(b){
        res <- MASS::ginv(data) %*% b
        dim(res) <- c(m,1)
        return(res)
    })
    #Extraemos los numeros
    k <- unlist(k)
    #Les damos forma
    dim(k) <- c(m,n)
    #Exportamos.
    return(k)
}

#Construccion de datasets //////////////////////////////////////////////////////

#Datasets principales
DataSetAmb <-  read.csv("WorldOceanographicAtlas.csv")

#Matriz Biologica
DataSet <- read.csv("DataCoralesProcMan.csv")

#Intercambiamos Especie por Tipo de Arrecife.
DataSet <- DataSet[,c(1,4,2,3,5:ncol(DataSet))] 

#Extraemos las posibles variables dependientes.
FactorVars <- names(DataSet)[1:6]
NumericVars    <- names(DataSet)[13:(ncol(DataSet)-4)]
NumericVars <- c(NumericVars,"Tiempo.en.Corriente.dias","Distancia.Euclidea.kms")

#Strings como factores.
s_prod <- sapply(FactorVars,function(var){
    DataSet[var][,1] <<- as.factor(DataSet[var][,1])
})
rm(s_prod)
gc()

#CLASIFICACION -----------------------------------

#Creamos dataset biologico sin NA's.
DataSet2 <- read.csv("DataCoralesProcMan2.csv")

#Intercambiamos Especie por Tipo de Arrecife.
DataSet2 <- DataSet2[,c(4,2,3,1,5:ncol(DataSet2))] 

#Chequeo de porcentaje de NA's por variable.
#apply(DataSet2,2,function(x)sum(is.na(x))/length(x)*100)

#Extraemos las posibles variables dependientes.
FactorVars2 <- names(DataSet2)[1:6]
NumericVars2    <- names(DataSet2)[7:ncol(DataSet2)]

#Strings como factores.
s_prod <- sapply(FactorVars2 ,function(var){
    DataSet2[var][,1] <<- as.factor(DataSet2[var][,1])
})
rm(s_prod)
gc()

#Log Vars
logYvar <- c("Tabla Datos")
logXvar <- c("Tabla Datos")