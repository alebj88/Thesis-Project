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

#Creamos la matriz biologica 2 para clasificaciones.
#Cada objeto de Google Earth sera tomado como una variable de la matriz.
#Objetos con valores de distancia y objetos con valores de tiempo en corriente.
#Esto se hace con el objetivo de ver si esos datos soy buenos clasificadores.

#Chequeo de porcentaje de NA's por variable.
#apply(MatrizBiologica,2,function(x)sum(is.na(x))/length(x)*100)

#Creamos dataset biologico sin NA's.
MatrizBiologica2 <- MatrizBiologica[complete.cases(MatrizBiologica),]

MatrizBiologica2$objeto.t <- sapply(MatrizBiologica2$objeto.t,function(x){
    y <- paste0("Dias.",x)
    return(y)
})
MatrizBiologica2$objeto.d <- sapply(MatrizBiologica2$objeto.d,function(x){
    y <- paste0("Kms.",x)
    return(y)
})

#Procedimiento spread para datos no unicos de objeto.t.
MatrizBiologica2$index <- 1:nrow(MatrizBiologica2)					
MatrizBiologica2$index <- sapply(MatrizBiologica2$index,function(i){
    ind <- sum(MatrizBiologica2$objeto.t[1:i] == MatrizBiologica2$objeto.t[i])
    return(ind)
})
MatrizBiologica2 <- spread(MatrizBiologica2,
                           key = objeto.t, value = Tiempo.en.Corriente.dias)
MatrizBiologica2$index <- NULL

#Procedimiento spread para datos no unicos de objeto.d.
MatrizBiologica2$index <- 1:nrow(MatrizBiologica2)					
MatrizBiologica2$index <- sapply(MatrizBiologica2$index,function(i){
    ind <- sum(MatrizBiologica2$objeto.d[1:i] == MatrizBiologica2$objeto.d[i])
    return(ind)
})
MatrizBiologica2 <- spread(MatrizBiologica2,
                           key = objeto.d, value = Distancia.Euclidea.kms)
MatrizBiologica2$index <- NULL
MatrizBiologica2$Dias.NA <- NULL
MatrizBiologica2$Kms.NA <- NULL

#Eliminamos filas repetidas.
MatrizBiologica2 <- MatrizBiologica2 %>% unique()

#Extraemos las posibles variables dependientes.
VarDepFactor2 <- names(MatrizBiologica2)[1:6]
varDepNum2    <- names(MatrizBiologica2)[7:ncol(MatrizBiologica2)]

