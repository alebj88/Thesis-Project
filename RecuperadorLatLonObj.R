library(dplyr)
library(tidyr)

#Importamos datos
Datos <- read.csv(
    "~/Thesis Project AB/Data/Processed Data/FactorsTransect_BenthicSubstrate_proc1.csv")

#Eliminamos variables inecesarias
Datos <- Datos[119:128]
DatosDist <- Datos[c(1,2,7:10)]
DatosTemp <- Datos[1:6]

#Eliminamos NAS
DatosDist <- DatosDist[!is.na(DatosDist$Distancia.Euclidea.kms),]
DatosTemp <- DatosTemp[!is.na(DatosTemp$Tiempo.en.Corriente.dias),]

#Definimos funcion necesaria
posMinF <- function(vect){
    vect <- rep(0,length(vect))
    vect[1] <- 1
    return(vect)
}

#Procesamos datos Dist
DatosDist <- DatosDist %>% mutate(clave = paste0(latitude,longitude)) %>% 
    arrange(clave,objeto.d,Distancia.Euclidea.kms) %>% 
    group_by(clave,objeto.d) %>% 
    mutate(pmf=posMinF(Distancia.Euclidea.kms)) %>%
    filter(pmf == 1)

DatosDist$clave <- NULL
DatosDist$pmf <- NULL

#Procesamos datos Temp
DatosTemp <- DatosTemp %>% mutate(clave = paste0(latitude,longitude)) %>% 
    arrange(clave,objeto.t,Tiempo.en.Corriente.dias) %>% 
    group_by(clave,objeto.t) %>% 
    mutate(pmf=posMinF(Tiempo.en.Corriente.dias)) %>%
    filter(pmf == 1)

DatosTemp$clave <- NULL
DatosTemp$pmf <- NULL

#Redondeamos mediadas
DatosDist$Distancia.Euclidea.kms <- round(DatosDist$Distancia.Euclidea.kms,3)
DatosTemp$Tiempo.en.Corriente.dias <-round(DatosTemp$Tiempo.en.Corriente.dias,3)

#Exportamos datos
setwd("~/Thesis Project AB/Data/Final Data")

write.csv(DatosDist,file = "LatLonObjDist.csv",row.names = FALSE)
write.csv(DatosTemp,file = "LatLonObjTemp.csv",row.names = FALSE)