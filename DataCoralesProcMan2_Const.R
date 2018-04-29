#Creamos la matriz biologica 2 para clasificaciones.
#Cada objeto de Google Earth sera tomado como una variable de la matriz.
#Objetos con valores de distancia y objetos con valores de tiempo en corriente.
#Esto se hace con el objetivo de ver si esos datos soy buenos clasificadores.

library(dplyr)
library(tidyr)

setwd("~/Thesis Project AB/Shiny Apps/ShinyExploratory")
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

#Exportamos tabla.
write.csv(MatrizBiologica2,file="DataCoralesProcMan2.csv",row.names=F)
