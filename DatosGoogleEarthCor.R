
library(dplyr)
library(tidyr)


#Datasets principales
MatBio <- read.csv(
    "~/Thesis Project AB/Data/Processed Data/DataCorales_proc1.csv")

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
MatBio$clave<- as.factor(paste0(MatBio[1][,1],MatBio[2][,1],MatBio[3][,1],
                                MatBio[4][,1],MatBio[5][,1],MatBio[6][,1],
                                MatBio[7][,1],MatBio[8][,1]))

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
MatBio$Grupos<- as.factor(paste0(MatBio[1][,1],MatBio[2][,1],MatBio[3][,1],
                                MatBio[4][,1],MatBio[5][,1],MatBio[6][,1],
                                MatBio[7][,1],MatBio[8][,1]))

#Calculamos promedios
for(i in 9:24){
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
write.csv(MatBio,file="DataCoralesProcMan2.csv",row.names=F)