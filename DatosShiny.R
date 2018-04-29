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

#//////////////////
#Chequeo de porcentaje de NA's por variable.
#apply(MatrizBiologica,2,function(x)sum(is.na(x))/length(x)*100)
#//////////////////

#Creamos dataset biologico sin NA's.
MatrizBiologica2 <- MatrizBiologica[complete.cases(MatrizBiologica),]

#//////////////////
#Chequeo de porcentaje de NA's por variable.
#apply(MatrizBiologica2,2,function(x)sum(is.na(x))/length(x)*100)
#//////////////////

MatrizBiologica2$objeto.t <- sapply(MatrizBiologica2$objeto.t,function(x){
    y <- paste0("Dias.",x)
    return(y)
})
MatrizBiologica2$objeto.d <- sapply(MatrizBiologica2$objeto.d,function(x){
    y <- paste0("Kms.",x)
    return(y)
})

#Unimos las variables de tiempo y dystancia en una sola.  
pos1 <- which(names(MatrizBiologica2) %in% 
                  c("objeto.d","Distancia.Euclidea.kms"))
pos2 <- which(names(MatrizBiologica2) %in% 
                  c("objeto.t","Tiempo.en.Corriente.dias"))

matAux1 <- MatrizBiologica2[,-pos1]
matAux2 <- MatrizBiologica2[,-pos2]

#Renombramos
pos1 <- which(names(matAux1) %in% c("objeto.t","Tiempo.en.Corriente.dias"))
pos2 <- which(names(matAux2) %in% c("objeto.d","Distancia.Euclidea.kms"))

names(matAux1)[pos1] <- c("objeto","medida")
names(matAux2)[pos2] <- c("objeto","medida")

#Unimos por rbind.
MatrizBiologica2 <- rbind(matAux1,matAux2)

#Procedimiento spread para datos no unicos de objeto.
#Construccion del indice.
MatrizBiologica2$index <- rep(NA,times = nrow(MatrizBiologica2))	
tab <- table(MatrizBiologica2$objeto)

proc_app <- sapply(names(tab),function(obj){
    MatrizBiologica2$index[which(MatrizBiologica2$objeto == obj)] <<- 1:tab[obj]
})
rm(proc_app)
gc()

MatrizBiologica2 <- spread(MatrizBiologica2, key = objeto, value = medida)
MatrizBiologica2$index <- NULL

#//////////////////
#Chequeo de porcentaje de NA's por variable.
#apply(MatrizBiologica2,2,function(x)sum(is.na(x))/length(x)*100)
#//////////////////

#Imputamos NA's colocando el valor 999 para indicar que el objeto no influye 
#sobre el punto de interes.
for(i in 1 : ncol(MatrizBiologica2)){
    posNas <- which(is.na(MatrizBiologica2[,i]))
    MatrizBiologica2[,i][posNas] <- rep(999,times = length(posNas))
}

#Eliminamos las variables que tengan una desviacion estandar igual a 0.
varSinDes <- which(sapply(MatrizBiologica2[,-c(1:6)],function(x){sd(x) == 0}))
varSinDes <- varSinDes + 6

MatrizBiologica2 <- MatrizBiologica2[,-varSinDes]
            
#Eliminamos filas repetidas.
MatrizBiologica2 <- MatrizBiologica2 %>% unique()

#//////////////////
#Chequeo de porcentaje de NA's por variable.
#sapply(MatrizBiologica2[,-c(1:6)],function(x){sd(x) == 0})
#//////////////////

#Exportamos tabla.
write.csv(MatrizBiologica2,file="DataCoralesProcMan2.csv",row.names=F)
