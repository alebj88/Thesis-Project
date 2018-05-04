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
MatrizBiologica$depth.m <- NULL

#Reordenamos. 
#Localidad, var categoricas, puntos de geolocalizacion, var numericas.
categoricas <- 1:6
geoloc <- which(names(MatrizBiologica) %in% 
                              c("latitude","longitude","lat.objeto.d",
                                "lon.objeto.d","lat.objeto.t","lon.objeto.t"))
vect <- 1:ncol(MatrizBiologica)
numericas <- vect[!(vect %in% c(categoricas,geoloc))]

MatrizBiologica <- MatrizBiologica[,c(categoricas,geoloc,numericas)]


#Strings como factores.
s_prod <- sapply(names(MatrizBiologica)[categoricas],function(var){
    MatrizBiologica[var][,1] <<- as.factor(MatrizBiologica[var][,1])
})
rm(s_prod)
gc()

#Actualizamos tabla.
write.csv(MatrizBiologica,file="DataCoralesProcMan.csv",row.names=F)

#//////////////////
#Chequeo de porcentaje de NA's por variable.
#NO DEBEN HABER NA's EN LOS DATOS QUE NO PROVIENEN  DE GOOGLE EARTH.
#apply(MatrizBiologica,2,function(x)sum(is.na(x))/length(x)*100)
#ELIMINAR MANUALMENTE EN DADO CASO.
#//////////////////

#Removemos variables inecesarias.
MatrizBiologica$latitude <- NULL
MatrizBiologica$longitude <- NULL
MatrizBiologica$lat.objeto.d <- NULL
MatrizBiologica$lon.objeto.d <- NULL
MatrizBiologica$lat.objeto.t <- NULL
MatrizBiologica$lon.objeto.t <- NULL

#Renombramos las variables de google earth para su reordenacion.
MatrizBiologica$objeto.t <- sapply(MatrizBiologica$objeto.t,function(x){
    y <- paste0("Dias.",x)
    return(y)
})
MatrizBiologica$objeto.d <- sapply(MatrizBiologica$objeto.d,function(x){
    y <- paste0("Kms.",x)
    return(y)
})

#Unimos las variables de tiempo y dystancia en una sola.  
pos1 <- which(names(MatrizBiologica) %in% 
                  c("objeto.d","Distancia.Euclidea.kms"))
pos2 <- which(names(MatrizBiologica) %in% 
                  c("objeto.t","Tiempo.en.Corriente.dias"))

matAux1 <- MatrizBiologica[,-pos1]
matAux2 <- MatrizBiologica[,-pos2]

#Renombramos
pos1 <- which(names(matAux1) %in% c("objeto.t","Tiempo.en.Corriente.dias"))
pos2 <- which(names(matAux2) %in% c("objeto.d","Distancia.Euclidea.kms"))

names(matAux1)[pos1] <- c("objeto","medida")
names(matAux2)[pos2] <- c("objeto","medida")

#Unimos por rbind.
MatrizBiologica <- rbind(matAux1,matAux2)

#Procedimiento spread para datos no unicos de objeto.
#Construccion del indice.
MatrizBiologica$index <- rep(NA,times = nrow(MatrizBiologica))	
tab <- table(MatrizBiologica$objeto)

proc_app <- sapply(names(tab),function(obj){
    MatrizBiologica$index[which(MatrizBiologica$objeto == obj)] <<- 1:tab[obj]
})
rm(proc_app)
gc()

MatrizBiologica <- spread(MatrizBiologica, key = objeto, value = medida)
MatrizBiologica$index <- NULL

#Imputamos NA's colocando el valor 999 para indicar que el objeto no influye 
#sobre el punto de interes.
for(i in 1 : ncol(MatrizBiologica)){
    posNas <- which(is.na(MatrizBiologica[,i]))
    MatrizBiologica[,i][posNas] <- rep(999,times = length(posNas))
}

#Eliminamos las variables que tengan una desviacion estandar igual a 0.
varSinDes <- which(sapply(MatrizBiologica[,-c(1:6)],function(x){sd(x) == 0}))
varSinDes <- varSinDes + 6

if(length(varSinDes) != 0){
    MatrizBiologica <- MatrizBiologica[,-varSinDes]
}
            
#Eliminamos filas repetidas.
MatrizBiologica <- MatrizBiologica %>% unique()

#//////////////////
#Chequeo de variabilidad por variable.
#sapply(MatrizBiologica[,-c(1:6)],function(x){sd(x) == 0})
#//////////////////

#//////////////////
#Chequeo de porcentaje de NA's por variable.
#apply(MatrizBiologica,2,function(x)sum(is.na(x))/length(x)*100)
#//////////////////

#Exportamos tabla.
write.csv(MatrizBiologica,file="DataCoralesProcMan2.csv",row.names=F)
