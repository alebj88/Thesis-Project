#Este codigo recibe las tablas biologicas precesadas.
#Primero se copian los archivos generados por el script ProcesadorMB.R en la 
#carpeta Processed Data y se renombran como aparece abajo.
#(se les ha retirado el _proc1)
# FactorsTransect_SCLESpecies.csv
# FactorsTransect_BenthicSubstrate.csv
# FactorsTransect_GreatGroups.csv
# FactorsTransect_SCLEGrowth.csv

#Luego se ejecuta el script Proyecto.R y se le agregan las variables de Google
#Earth usando 100km de distancia y 15 dias de viaje en corriente.
#Las tablas generadas por Proyecto.R son preocesadas por este script.
# FactorsTransect_SCLESpecies_proc1.csv
# FactorsTransect_BenthicSubstrate_proc1.csv
# FactorsTransect_GreatGroups_proc1.csv
# FactorsTransect_SCLEGrowth_proc1.csv
library(dplyr)
library(tidyr)

################################################################################
################################ SET DE PRUEBA #################################
################################################################################
# 
# #Datasets principales
# MatBio <- read.csv(
#     "~/Thesis Project AB/Data/Processed Data/DataCorales_proc1.csv")
# 
# #//////////////////
# #Chequeo de porcentaje de NA's por variable.
# #NO DEBEN HABER NA's EN LOS DATOS QUE NO PROVIENEN  DE GOOGLE EARTH.
# #apply(MatBio,2,function(x)sum(is.na(x))/length(x)*100)
# #ELIMINAR MANUALMENTE EN DADO CASO.
# #//////////////////
# 
# #Renombramos
# MatBio$objeto.t <- paste0(MatBio$objeto.t,".t")
# MatBio$objeto.d <- paste0(MatBio$objeto.d,".d")
# 
# #Eliminamos los lat=lon de los objetos
# MatBio$lon.objeto.d <- NULL
# MatBio$lat.objeto.d <- NULL
# MatBio$lon.objeto.t <- NULL
# MatBio$lat.objeto.t <- NULL
# 
# #Unimos las variables de tiempo y dystancia en una sola.  
# pos1 <- which(names(MatBio) %in% 
#                     c("objeto.d","Distancia.Euclidea.kms"))
# pos2 <- which(names(MatBio) %in% 
#                     c("objeto.t","Tiempo.en.Corriente.dias"))
# 
# matAux1 <- MatBio[,-pos1]
# matAux2 <- MatBio[,-pos2]
# 
# #Renombramos
# pos1 <- which(names(matAux1) %in% c("objeto.t","Tiempo.en.Corriente.dias"))
# pos2 <- which(names(matAux2) %in% c("objeto.d","Distancia.Euclidea.kms"))
# 
# names(matAux1)[pos1] <- c("objeto","medida")
# names(matAux2)[pos2] <- c("objeto","medida")
# 
# #Unimos por rbind.
# MatBio <- rbind(matAux1,matAux2)
# 
# #Redondeamos
# MatBio$medida <- round(MatBio$medida,3)
# 
# #Reemplazamos los NA's de medida por 999
# MatBio$medida[is.na(MatBio$medida)] <- rep(999,sum(is.na(MatBio$medida)))
# 
#  #nos quedamos solo con las unicaciones de menor distancia
# MatBio$fila <- 1:nrow(MatBio)
# posiciones <- 1:(ncol(MatBio)-3)
# 
# MatBio$clave <-apply(MatBio[-(ncol(MatBio):(ncol(MatBio)-2))],1,function(vect){
#     vect <- paste(vect,collapse = "")
#     vect
# })
# MatBio$clave <- as.factor(MatBio$clave)
# 
# proc1 <- tapply(MatBio$fila,MatBio$clave,function(filas){
#     dfAux <- MatBio[filas,]
#     proc2 <- tapply(dfAux$fila,dfAux$objeto,function(filas){
#         if(length(filas) != 1){
#             filas <- filas[-which.min(MatBio$medida[filas])]
#             MatBio$medida[filas] <<- rep("Elim",length(MatBio$medida[filas]))
#         }
#         return(NULL)
#     })
#     return(NULL)
# })
# #liberamos memoria
# rm(proc1)
# gc()
# 
# MatBio$fila <- NULL
# MatBio$clave <- NULL
# 
# #Eliminamos los que seleccionamos previamente
# MatBio <- MatBio[MatBio$medida != "Elim",]
# 
# #Procedimiento spread para datos no unicos de objeto.
# #Construccion del indice.
# MatBio$index <- rep(NA,times = nrow(MatBio))	
# tab <- table(MatBio$objeto)
# 
# proc_app <- sapply(names(tab),function(obj){
#     MatBio$index[which(MatBio$objeto == obj)] <<- 1:tab[obj]
# })
# rm(proc_app)
# gc()
# 
# MatBio <- spread(MatBio, key = objeto, value = medida)
# 
# #Eliminamos variables inecesarias
# MatBio$index <- NULL
# MatBio$NA.d <- NULL
# MatBio$NA.t <- NULL
# 
# #Juntamos similares
# MatBio$Grupos <-apply(MatBio[posiciones],1,function(vect){
#     vect <- paste(vect,collapse = "")
#     vect
# })
# MatBio$Grupos <- as.factor(MatBio$Grupos)
# 
# #Calculamos promedios
# for(i in (posiciones[length(posiciones)]+1):(ncol(MatBio) - 1)){
#     t <- tapply(MatBio[i][,1],MatBio$Grupos,median,na.rm = T)	
#     MatBio[i][,1] <-  sapply(MatBio$Grupos,function(x){
#         unname(t[as.character(x)])
#     })
# }
# 
# #Hacemos unique	
# MatBio$Grupos <- NULL
# 
# MatBio <- MatBio %>% unique()
# 
# 
# #Imputamos NA's colocando el valor 999 para indicar que el objeto no influye 
# #sobre el punto de interes.
# for(i in 1 : ncol(MatBio)){
#     posNas <- which(is.na(MatBio[,i]))
#     MatBio[,i][posNas] <- rep(999,times = length(posNas))
# }
# 
# setwd("~/Thesis Project AB/Data/Processed Data")
# 
# #Exportamos tabla.
# write.csv(MatBio,file="DataCorales_ProcMan2.csv",row.names=F)

################################################################################
#################################### Tabla 1 ###################################
################################################################################

#Datasets principales
MatBio <- read.csv(
"~/Thesis Project AB/Data/Processed Data/FactorsTransect_BenthicSubstrate_proc1.csv")

#//////////////////
#Chequeo de porcentaje de NA's por variable.
#NO DEBEN HABER NA's EN LOS DATOS QUE NO PROVIENEN  DE GOOGLE EARTH.
#apply(MatBio,2,function(x)sum(is.na(x))/length(x)*100)
#ELIMINAR MANUALMENTE EN DADO CASO.
#//////////////////

#Renombramos
MatBio$objeto.t <- paste0(MatBio$objeto.t,".t")
MatBio$objeto.d <- paste0(MatBio$objeto.d,".d")

#Eliminamos los lat=lon de los objetos
MatBio$lon.objeto.d <- NULL
MatBio$lat.objeto.d <- NULL
MatBio$lon.objeto.t <- NULL
MatBio$lat.objeto.t <- NULL

#Unimos las variables de tiempo y dystancia en una sola.  
pos1 <- which(names(MatBio) %in% 
                  c("objeto.d","Distancia.Euclidea.kms"))
pos2 <- which(names(MatBio) %in% 
                  c("objeto.t","Tiempo.en.Corriente.dias"))

matAux1 <- MatBio[,-pos1]
matAux2 <- MatBio[,-pos2]

#Renombramos
pos1 <- which(names(matAux1) %in% c("objeto.t","Tiempo.en.Corriente.dias"))
pos2 <- which(names(matAux2) %in% c("objeto.d","Distancia.Euclidea.kms"))

names(matAux1)[pos1] <- c("objeto","medida")
names(matAux2)[pos2] <- c("objeto","medida")

#Unimos por rbind.
MatBio <- rbind(matAux1,matAux2)

#Redondeamos
MatBio$medida <- round(MatBio$medida,3)

#Reemplazamos los NA's de medida por 999
MatBio$medida[is.na(MatBio$medida)] <- rep(999,sum(is.na(MatBio$medida)))

#nos quedamos solo con las unicaciones de menor distancia
MatBio$fila <- 1:nrow(MatBio)
posiciones <- 1:(ncol(MatBio)-3)

MatBio$clave <-apply(MatBio[-(ncol(MatBio):(ncol(MatBio)-2))],1,function(vect){
    vect <- paste(vect,collapse = "")
    vect
})
MatBio$clave <- as.factor(MatBio$clave)

proc1 <- tapply(MatBio$fila,MatBio$clave,function(filas){
    dfAux <- MatBio[filas,]
    proc2 <- tapply(dfAux$fila,dfAux$objeto,function(filas){
        if(length(filas) != 1){
            filas <- filas[-which.min(MatBio$medida[filas])]
            MatBio$medida[filas] <<- rep("Elim",length(MatBio$medida[filas]))
        }
        return(NULL)
    })
    return(NULL)
})
#liberamos memoria
rm(proc1)
gc()

MatBio$fila <- NULL
MatBio$clave <- NULL

#Eliminamos los que seleccionamos previamente
MatBio <- MatBio[MatBio$medida != "Elim",]

#Procedimiento spread para datos no unicos de objeto.
#Construccion del indice.
MatBio$index <- rep(NA,times = nrow(MatBio))	
tab <- table(MatBio$objeto)

proc_app <- sapply(names(tab),function(obj){
    MatBio$index[which(MatBio$objeto == obj)] <<- 1:tab[obj]
})
rm(proc_app)
gc()

MatBio <- spread(MatBio, key = objeto, value = medida)

#Eliminamos variables inecesarias
MatBio$index <- NULL
MatBio$NA.d <- NULL
MatBio$NA.t <- NULL

#Juntamos similares
MatBio$Grupos <-apply(MatBio[posiciones],1,function(vect){
    vect <- paste(vect,collapse = "")
    vect
})
MatBio$Grupos <- as.factor(MatBio$Grupos)

#Calculamos promedios
for(i in (posiciones[length(posiciones)]+1):(ncol(MatBio) - 1)){
    t <- tapply(MatBio[i][,1],MatBio$Grupos,median,na.rm = T)	
    MatBio[i][,1] <-  sapply(MatBio$Grupos,function(x){
        unname(t[as.character(x)])
    })
}

#Hacemos unique	
MatBio$Grupos <- NULL

MatBio <- MatBio %>% unique()


#Imputamos NA's colocando el valor 999 para indicar que el objeto no influye 
#sobre el punto de interes.
for(i in 1 : ncol(MatBio)){
    posNas <- which(is.na(MatBio[,i]))
    MatBio[,i][posNas] <- rep(999,times = length(posNas))
}

setwd("~/Thesis Project AB/Data/Processed Data")

#Exportamos tabla.
write.csv(MatBio,file="FactorsTransect_BenthicSubstrate_ProcMan2.csv",
          row.names=F)

################################################################################
#################################### Tabla 2 ###################################
################################################################################

#Datasets principales
MatBio <- read.csv(
"~/Thesis Project AB/Data/Processed Data/FactorsTransect_GreatGroups_proc1.csv")

#//////////////////
#Chequeo de porcentaje de NA's por variable.
#NO DEBEN HABER NA's EN LOS DATOS QUE NO PROVIENEN  DE GOOGLE EARTH.
#apply(MatBio,2,function(x)sum(is.na(x))/length(x)*100)
#ELIMINAR MANUALMENTE EN DADO CASO.
#//////////////////

#Renombramos
MatBio$objeto.t <- paste0(MatBio$objeto.t,".t")
MatBio$objeto.d <- paste0(MatBio$objeto.d,".d")

#Eliminamos los lat=lon de los objetos
MatBio$lon.objeto.d <- NULL
MatBio$lat.objeto.d <- NULL
MatBio$lon.objeto.t <- NULL
MatBio$lat.objeto.t <- NULL

#Unimos las variables de tiempo y dystancia en una sola.  
pos1 <- which(names(MatBio) %in% 
                  c("objeto.d","Distancia.Euclidea.kms"))
pos2 <- which(names(MatBio) %in% 
                  c("objeto.t","Tiempo.en.Corriente.dias"))

matAux1 <- MatBio[,-pos1]
matAux2 <- MatBio[,-pos2]

#Renombramos
pos1 <- which(names(matAux1) %in% c("objeto.t","Tiempo.en.Corriente.dias"))
pos2 <- which(names(matAux2) %in% c("objeto.d","Distancia.Euclidea.kms"))

names(matAux1)[pos1] <- c("objeto","medida")
names(matAux2)[pos2] <- c("objeto","medida")

#Unimos por rbind.
MatBio <- rbind(matAux1,matAux2)

#Redondeamos
MatBio$medida <- round(MatBio$medida,3)

#Reemplazamos los NA's de medida por 999
MatBio$medida[is.na(MatBio$medida)] <- rep(999,sum(is.na(MatBio$medida)))

#nos quedamos solo con las unicaciones de menor distancia
MatBio$fila <- 1:nrow(MatBio)
posiciones <- 1:(ncol(MatBio)-3)

MatBio$clave <-apply(MatBio[-(ncol(MatBio):(ncol(MatBio)-2))],1,function(vect){
    vect <- paste(vect,collapse = "")
    vect
})
MatBio$clave <- as.factor(MatBio$clave)

proc1 <- tapply(MatBio$fila,MatBio$clave,function(filas){
    dfAux <- MatBio[filas,]
    proc2 <- tapply(dfAux$fila,dfAux$objeto,function(filas){
        if(length(filas) != 1){
            filas <- filas[-which.min(MatBio$medida[filas])]
            MatBio$medida[filas] <<- rep("Elim",length(MatBio$medida[filas]))
        }
        return(NULL)
    })
    return(NULL)
})
#liberamos memoria
rm(proc1)
gc()

MatBio$fila <- NULL
MatBio$clave <- NULL

#Eliminamos los que seleccionamos previamente
MatBio <- MatBio[MatBio$medida != "Elim",]

#Procedimiento spread para datos no unicos de objeto.
#Construccion del indice.
MatBio$index <- rep(NA,times = nrow(MatBio))	
tab <- table(MatBio$objeto)

proc_app <- sapply(names(tab),function(obj){
    MatBio$index[which(MatBio$objeto == obj)] <<- 1:tab[obj]
})
rm(proc_app)
gc()

MatBio <- spread(MatBio, key = objeto, value = medida)

#Eliminamos variables inecesarias
MatBio$index <- NULL
MatBio$NA.d <- NULL
MatBio$NA.t <- NULL

#Juntamos similares
MatBio$Grupos <-apply(MatBio[posiciones],1,function(vect){
    vect <- paste(vect,collapse = "")
    vect
})
MatBio$Grupos <- as.factor(MatBio$Grupos)

#Calculamos promedios
for(i in (posiciones[length(posiciones)]+1):(ncol(MatBio) - 1)){
    t <- tapply(MatBio[i][,1],MatBio$Grupos,median,na.rm = T)	
    MatBio[i][,1] <-  sapply(MatBio$Grupos,function(x){
        unname(t[as.character(x)])
    })
}

#Hacemos unique	
MatBio$Grupos <- NULL

MatBio <- MatBio %>% unique()


#Imputamos NA's colocando el valor 999 para indicar que el objeto no influye 
#sobre el punto de interes.
for(i in 1 : ncol(MatBio)){
    posNas <- which(is.na(MatBio[,i]))
    MatBio[,i][posNas] <- rep(999,times = length(posNas))
}

setwd("~/Thesis Project AB/Data/Processed Data")

#Exportamos tabla.
write.csv(MatBio,file="FactorsTransect_GreatGroups_ProcMan2.csv",row.names=F)

################################################################################
#################################### Tabla 3 ###################################
################################################################################

#Datasets principales
MatBio <- read.csv(
"~/Thesis Project AB/Data/Processed Data/FactorsTransect_SCLEGrowth_proc1.csv")

#//////////////////
#Chequeo de porcentaje de NA's por variable.
#NO DEBEN HABER NA's EN LOS DATOS QUE NO PROVIENEN  DE GOOGLE EARTH.
#apply(MatBio,2,function(x)sum(is.na(x))/length(x)*100)
#ELIMINAR MANUALMENTE EN DADO CASO.
#//////////////////

#Renombramos
MatBio$objeto.t <- paste0(MatBio$objeto.t,".t")
MatBio$objeto.d <- paste0(MatBio$objeto.d,".d")

#Eliminamos los lat=lon de los objetos
MatBio$lon.objeto.d <- NULL
MatBio$lat.objeto.d <- NULL
MatBio$lon.objeto.t <- NULL
MatBio$lat.objeto.t <- NULL

#Unimos las variables de tiempo y dystancia en una sola.  
pos1 <- which(names(MatBio) %in% 
                  c("objeto.d","Distancia.Euclidea.kms"))
pos2 <- which(names(MatBio) %in% 
                  c("objeto.t","Tiempo.en.Corriente.dias"))

matAux1 <- MatBio[,-pos1]
matAux2 <- MatBio[,-pos2]

#Renombramos
pos1 <- which(names(matAux1) %in% c("objeto.t","Tiempo.en.Corriente.dias"))
pos2 <- which(names(matAux2) %in% c("objeto.d","Distancia.Euclidea.kms"))

names(matAux1)[pos1] <- c("objeto","medida")
names(matAux2)[pos2] <- c("objeto","medida")

#Unimos por rbind.
MatBio <- rbind(matAux1,matAux2)

#Redondeamos
MatBio$medida <- round(MatBio$medida,3)

#Reemplazamos los NA's de medida por 999
MatBio$medida[is.na(MatBio$medida)] <- rep(999,sum(is.na(MatBio$medida)))

#nos quedamos solo con las unicaciones de menor distancia
MatBio$fila <- 1:nrow(MatBio)
posiciones <- 1:(ncol(MatBio)-3)

MatBio$clave <-apply(MatBio[-(ncol(MatBio):(ncol(MatBio)-2))],1,function(vect){
    vect <- paste(vect,collapse = "")
    vect
})
MatBio$clave <- as.factor(MatBio$clave)

proc1 <- tapply(MatBio$fila,MatBio$clave,function(filas){
    dfAux <- MatBio[filas,]
    proc2 <- tapply(dfAux$fila,dfAux$objeto,function(filas){
        if(length(filas) != 1){
            filas <- filas[-which.min(MatBio$medida[filas])]
            MatBio$medida[filas] <<- rep("Elim",length(MatBio$medida[filas]))
        }
        return(NULL)
    })
    return(NULL)
})
#liberamos memoria
rm(proc1)
gc()

MatBio$fila <- NULL
MatBio$clave <- NULL

#Eliminamos los que seleccionamos previamente
MatBio <- MatBio[MatBio$medida != "Elim",]

#Procedimiento spread para datos no unicos de objeto.
#Construccion del indice.
MatBio$index <- rep(NA,times = nrow(MatBio))	
tab <- table(MatBio$objeto)

proc_app <- sapply(names(tab),function(obj){
    MatBio$index[which(MatBio$objeto == obj)] <<- 1:tab[obj]
})
rm(proc_app)
gc()

MatBio <- spread(MatBio, key = objeto, value = medida)

#Eliminamos variables inecesarias
MatBio$index <- NULL
MatBio$NA.d <- NULL
MatBio$NA.t <- NULL

#Juntamos similares
MatBio$Grupos <-apply(MatBio[posiciones],1,function(vect){
    vect <- paste(vect,collapse = "")
    vect
})
MatBio$Grupos <- as.factor(MatBio$Grupos)

#Calculamos promedios
for(i in (posiciones[length(posiciones)]+1):(ncol(MatBio) - 1)){
    t <- tapply(MatBio[i][,1],MatBio$Grupos,median,na.rm = T)	
    MatBio[i][,1] <-  sapply(MatBio$Grupos,function(x){
        unname(t[as.character(x)])
    })
}

#Hacemos unique	
MatBio$Grupos <- NULL

MatBio <- MatBio %>% unique()


#Imputamos NA's colocando el valor 999 para indicar que el objeto no influye 
#sobre el punto de interes.
for(i in 1 : ncol(MatBio)){
    posNas <- which(is.na(MatBio[,i]))
    MatBio[,i][posNas] <- rep(999,times = length(posNas))
}

setwd("~/Thesis Project AB/Data/Processed Data")

#Exportamos tabla.
write.csv(MatBio,file="FactorsTransect_SCLEGrowth_ProcMan2.csv",row.names=F)

################################################################################
#################################### Tabla 4 ###################################
################################################################################

#Datasets principales
MatBio <- read.csv(
"~/Thesis Project AB/Data/Processed Data/FactorsTransect_SCLESpecies_proc1.csv")

#//////////////////
#Chequeo de porcentaje de NA's por variable.
#NO DEBEN HABER NA's EN LOS DATOS QUE NO PROVIENEN  DE GOOGLE EARTH.
#apply(MatBio,2,function(x)sum(is.na(x))/length(x)*100)
#ELIMINAR MANUALMENTE EN DADO CASO.
#//////////////////

#Renombramos
MatBio$objeto.t <- paste0(MatBio$objeto.t,".t")
MatBio$objeto.d <- paste0(MatBio$objeto.d,".d")

#Eliminamos los lat=lon de los objetos
MatBio$lon.objeto.d <- NULL
MatBio$lat.objeto.d <- NULL
MatBio$lon.objeto.t <- NULL
MatBio$lat.objeto.t <- NULL

#Unimos las variables de tiempo y dystancia en una sola.  
pos1 <- which(names(MatBio) %in% 
                  c("objeto.d","Distancia.Euclidea.kms"))
pos2 <- which(names(MatBio) %in% 
                  c("objeto.t","Tiempo.en.Corriente.dias"))

matAux1 <- MatBio[,-pos1]
matAux2 <- MatBio[,-pos2]

#Renombramos
pos1 <- which(names(matAux1) %in% c("objeto.t","Tiempo.en.Corriente.dias"))
pos2 <- which(names(matAux2) %in% c("objeto.d","Distancia.Euclidea.kms"))

names(matAux1)[pos1] <- c("objeto","medida")
names(matAux2)[pos2] <- c("objeto","medida")

#Unimos por rbind.
MatBio <- rbind(matAux1,matAux2)

#Redondeamos
MatBio$medida <- round(MatBio$medida,3)

#Reemplazamos los NA's de medida por 999
MatBio$medida[is.na(MatBio$medida)] <- rep(999,sum(is.na(MatBio$medida)))

#nos quedamos solo con las unicaciones de menor distancia
MatBio$fila <- 1:nrow(MatBio)
posiciones <- 1:(ncol(MatBio)-3)

MatBio$clave <-apply(MatBio[-(ncol(MatBio):(ncol(MatBio)-2))],1,function(vect){
    vect <- paste(vect,collapse = "")
    vect
})
MatBio$clave <- as.factor(MatBio$clave)

proc1 <- tapply(MatBio$fila,MatBio$clave,function(filas){
    dfAux <- MatBio[filas,]
    proc2 <- tapply(dfAux$fila,dfAux$objeto,function(filas){
        if(length(filas) != 1){
            filas <- filas[-which.min(MatBio$medida[filas])]
            MatBio$medida[filas] <<- rep("Elim",length(MatBio$medida[filas]))
        }
        return(NULL)
    })
    return(NULL)
})
#liberamos memoria
rm(proc1)
gc()

MatBio$fila <- NULL
MatBio$clave <- NULL

#Eliminamos los que seleccionamos previamente
MatBio <- MatBio[MatBio$medida != "Elim",]

#Procedimiento spread para datos no unicos de objeto.
#Construccion del indice.
MatBio$index <- rep(NA,times = nrow(MatBio))	
tab <- table(MatBio$objeto)

proc_app <- sapply(names(tab),function(obj){
    MatBio$index[which(MatBio$objeto == obj)] <<- 1:tab[obj]
})
rm(proc_app)
gc()

MatBio <- spread(MatBio, key = objeto, value = medida)

#Eliminamos variables inecesarias
MatBio$index <- NULL
MatBio$NA.d <- NULL
MatBio$NA.t <- NULL

#Juntamos similares
MatBio$Grupos <-apply(MatBio[posiciones],1,function(vect){
    vect <- paste(vect,collapse = "")
    vect
})
MatBio$Grupos <- as.factor(MatBio$Grupos)

#Calculamos promedios
for(i in (posiciones[length(posiciones)]+1):(ncol(MatBio) - 1)){
    t <- tapply(MatBio[i][,1],MatBio$Grupos,median,na.rm = T)	
    MatBio[i][,1] <-  sapply(MatBio$Grupos,function(x){
        unname(t[as.character(x)])
    })
}

#Hacemos unique	
MatBio$Grupos <- NULL

MatBio <- MatBio %>% unique()


#Imputamos NA's colocando el valor 999 para indicar que el objeto no influye 
#sobre el punto de interes.
for(i in 1 : ncol(MatBio)){
    posNas <- which(is.na(MatBio[,i]))
    MatBio[,i][posNas] <- rep(999,times = length(posNas))
}

setwd("~/Thesis Project AB/Data/Processed Data")

#Exportamos tabla.
write.csv(MatBio,file="FactorsTransect_SCLESpecies_ProcMan2.csv",row.names=F)
