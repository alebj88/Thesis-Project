#Publicado en:
#https://alebj88.shinyapps.io/ShinyExploratory/

library(DT)
library(dplyr)
library(ggmap)
library(ggplot2)
library(tidyr)

#Funciones necesarias.

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

#Datasets principales
MatrizBiologica <- read.csv("DataCoralesProcMan.csv")

#Eliminamos variables innecesarias.
MatrizBiologica$latitude <- NULL
MatrizBiologica$longitude <- NULL
MatrizBiologica$depth.m <- NULL

#Strings como factores.
s_prod <- sapply(names(MatrizBiologica)[1:6],function(var){
    MatrizBiologica[var][,1] <<- as.factor(MatrizBiologica[var][,1])
})
rm(s_prod)
gc()

#Extraemos las posibles variables dependientes.
VarDepFactor <- names(MatrizBiologica)[1:6]
varDepNum    <- names(MatrizBiologica)[7:(ncol(MatrizBiologica)-4)]
varDepNum <- c("Tiempo.en.Corriente.dias","Distancia.Euclidea.kms",varDepNum)


#CLASIFICACION /////////////////////////////////////////////////////////////////

#Creamos dataset biologico sin NA's.
MatrizBiologica2 <- read.csv("DataCoralesProcMan2.csv")

#Chequeo de porcentaje de NA's por variable.
#apply(MatrizBiologica2,2,function(x)sum(is.na(x))/length(x)*100)

#Extraemos las posibles variables dependientes.
VarDepFactor2 <- names(MatrizBiologica2)[1:6]
varDepNum2    <- names(MatrizBiologica2)[7:ncol(MatrizBiologica2)]

