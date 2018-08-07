#Procesador de matrices biologicas.
#
#Almacenar los archivos:
# FactorsTransect_SCLESpecies.csv
# FactorsTransect_BenthicSubstrate.csv
# FactorsTransect_GreatGroups.csv
# FactorsTransect_SCLEGrowth.csv

#En la carpeta "Raw Data" de "Thesis Project AB".

#Este codigo le agrega los puntos de geolocalizacion a las tablas biologicas.
#Las tablas procesadas las almacena en Pre-processed Data. Estas seben ser
#copiadas y renombradas en la carpeta Processed Data removiendoles el "_proc1"

library(dplyr)
library(tidyr)

################################################################################
######################### TABLA DE GEOLOCALIZACION #############################
################################################################################

metadataGeo <- read.csv(
    "~/Thesis Project AB/Data/Raw Data/Progress_ Venezuela - metadata.csv")

#Corregimos variables
metadataGeo$Locality <- unname(sapply(metadataGeo$Locality,function(text){
    text <- gsub("_"," ",text)
    text
}))

metadataGeo$Site <- unname(sapply(metadataGeo$Site,function(text){
    text <- gsub("_"," ",text)
    text
}))

metadataGeo$Longitude <- unname(sapply(metadataGeo$Longitude,function(text){
    text <- gsub(",","\\.",text)
    text
}))

metadataGeo$Latitude <- unname(sapply(metadataGeo$Latitude,function(text){
    text <- gsub(",","\\.",text)
    text
}))

#Renombramos
names(metadataGeo)[5] <- "latitude"
names(metadataGeo)[6] <- "longitude"

#Formateamos
metadataGeo$latitude <- as.numeric(as.character(metadataGeo$latitude))
metadataGeo$longitude <- as.numeric(as.character(metadataGeo$longitude))
    
#redondeamos lat lon
metadataGeo$latitude <- round(metadataGeo$latitude,3) 
metadataGeo$longitude <- round(metadataGeo$longitude,3) 

#Creamos clave para el merge
metadataGeo$Clave <- paste(metadataGeo$Locality,metadataGeo$Site,sep = "_") 

#Subsetting
metadataGeo <- metadataGeo[c("Clave","latitude","longitude")]


################################################################################
################################ TABLA NRO 1 ###################################
################################################################################

setwd("~/Thesis Project AB/Data/Raw Data")

#Importamos y procesamos las matrices biologicas

Matriz1 <- read.csv("FactorsTransect_BenthicSubstrate.csv")

Matriz1$File <- NULL
Matriz1$Loc <- NULL

#Establecemos los grupos de transectas a agrupar

Matriz1$GrupoTrans <- as.factor(paste0(Matriz1$Locality,Matriz1$Site))

#Calculamos promedios
for(i in 6:119){
    t <- tapply(Matriz1[i][,1],Matriz1$GrupoTrans,mean,na.rm = T)	
    Matriz1[i][,1] <-  sapply(Matriz1$GrupoTrans,function(x){
        unname(t[as.character(x)])
    })
}

#Hacemos unique	
Matriz1$Transect <- NULL
Matriz1$GrupoTrans <- NULL

Matriz1 <- Matriz1 %>% unique()

#Corregimos nombres
names(Matriz1) <- unname(sapply(names(Matriz1),function(text){
    text <- gsub("\\.\\.","\\.",text)
    text <- gsub("\\.$","",text)
    text <- gsub("Other.Other.","Other.",text)
    text <- gsub(".other$","",text)
}))

#Editamos variables
Matriz1$Site <- unname(sapply(Matriz1$Site,function(text){
    text <- gsub("Playa Tiburón","Playa Tiburon",text)
    text <- gsub("Cayo Agua","Cayo de Agua",text)
    text <- gsub("Dos Mosquises Sur","Dos Mosquises",text)
    text <- gsub("Madrisqui","Madriski",text)
    text <- gsub("Pelona de Rabusqui","Rabuski",text)
    text <- gsub("Boca de Cote","Cote",text)
    text <- gsub("Salinas","Las Salinas",text)
    text
}))

#Tratamiento final/////////////////////////////////////

#Creamos clave para el merge
Matriz1$Clave <- paste(Matriz1$Locality,Matriz1$Site,sep = "_") 

#Merge
Matriz1 <- merge(Matriz1,metadataGeo,by="Clave",all.x = TRUE)
Matriz1$Clave <- NULL

setwd("~/Thesis Project AB/Data/Pre-processed Data")

write.csv(Matriz1,file = "FactorsTransect_BenthicSubstrate_Proc1.csv", 
          row.names = F)


################################################################################
################################ TABLA NRO 2 ###################################
################################################################################

setwd("~/Thesis Project AB/Data/Raw Data")

#Importamos y procesamos las matrices biologicas

Matriz1 <- read.csv("FactorsTransect_GreatGroups.csv")

Matriz1$File <- NULL
Matriz1$Loc <- NULL

#Establecemos los grupos de transectas a agrupar

Matriz1$GrupoTrans <- as.factor(paste0(Matriz1$Locality,Matriz1$Site))

#Calculamos promedios
for(i in 6:18){
    t <- tapply(Matriz1[i][,1],Matriz1$GrupoTrans,mean,na.rm = T)	
    Matriz1[i][,1] <-  sapply(Matriz1$GrupoTrans,function(x){
        unname(t[as.character(x)])
    })
}

#Hacemos unique	
Matriz1$Transect <- NULL
Matriz1$GrupoTrans <- NULL

Matriz1 <- Matriz1 %>% unique()

#Editamos variables
Matriz1$Site <- unname(sapply(Matriz1$Site,function(text){
    text <- gsub("Playa Tiburón","Playa Tiburon",text)
    text <- gsub("Cayo Agua","Cayo de Agua",text)
    text <- gsub("Dos Mosquises Sur","Dos Mosquises",text)
    text <- gsub("Madrisqui","Madriski",text)
    text <- gsub("Pelona de Rabusqui","Rabuski",text)
    text <- gsub("Boca de Cote","Cote",text)
    text <- gsub("Salinas","Las Salinas",text)
    text
}))

#Exportamos tabla /////////////////////////////////////

#Creamos clave para el merge
Matriz1$Clave <- paste(Matriz1$Locality,Matriz1$Site,sep = "_") 

#Merge
Matriz1 <- merge(Matriz1,metadataGeo,by="Clave",all.x = TRUE)
Matriz1$Clave <- NULL

setwd("~/Thesis Project AB/Data/Pre-processed Data")

write.csv(Matriz1,file = "FactorsTransect_GreatGroups_Proc1.csv", row.names = F)


################################################################################
################################ TABLA NRO 3 ###################################
################################################################################

setwd("~/Thesis Project AB/Data/Raw Data")

#Importamos y procesamos las matrices biologicas

Matriz1 <- read.csv("FactorsTransect_SCLEGrowth.csv")

Matriz1$File <- NULL
Matriz1$Loc <- NULL

#Establecemos los grupos de transectas a agrupar

Matriz1$GrupoTrans <- as.factor(paste0(Matriz1$Locality,Matriz1$Site))

#Calculamos promedios
for(i in 6:14){
    t <- tapply(Matriz1[i][,1],Matriz1$GrupoTrans,mean,na.rm = T)	
    Matriz1[i][,1] <-  sapply(Matriz1$GrupoTrans,function(x){
        unname(t[as.character(x)])
    })
}

#Hacemos unique	
Matriz1$Transect <- NULL
Matriz1$GrupoTrans <- NULL

Matriz1 <- Matriz1 %>% unique()

#Corregimos nombres
names(Matriz1) <- unname(sapply(names(Matriz1),function(text){
    text <- gsub("\\.\\.","\\.",text)
}))

#Editamos variables
Matriz1$Site <- unname(sapply(Matriz1$Site,function(text){
    text <- gsub("Playa Tiburón","Playa Tiburon",text)
    text <- gsub("Cayo Agua","Cayo de Agua",text)
    text <- gsub("Dos Mosquises Sur","Dos Mosquises",text)
    text <- gsub("Madrisqui","Madriski",text)
    text <- gsub("Pelona de Rabusqui","Rabuski",text)
    text <- gsub("Boca de Cote","Cote",text)
    text <- gsub("Salinas","Las Salinas",text)
    text
}))

#Exportamos tabla /////////////////////////////////////

#Creamos clave para el merge
Matriz1$Clave <- paste(Matriz1$Locality,Matriz1$Site,sep = "_") 

#Merge
Matriz1 <- merge(Matriz1,metadataGeo,by="Clave",all.x = TRUE)
Matriz1$Clave <- NULL

setwd("~/Thesis Project AB/Data/Pre-processed Data")

write.csv(Matriz1,file = "FactorsTransect_SCLEGrowth_Proc1.csv", row.names = F)


################################################################################
################################ TABLA NRO 4 ###################################
################################################################################

setwd("~/Thesis Project AB/Data/Raw Data")

#Importamos y procesamos las matrices biologicas

Matriz1 <- read.csv("FactorsTransect_SCLESpecies.csv")

Matriz1$File <- NULL
Matriz1$Loc <- NULL

#Establecemos los grupos de transectas a agrupar

Matriz1$GrupoTrans <- as.factor(paste0(Matriz1$Locality,Matriz1$Site))

#Calculamos promedios
for(i in 6:48){
    t <- tapply(Matriz1[i][,1],Matriz1$GrupoTrans,mean,na.rm = T)	
    Matriz1[i][,1] <-  sapply(Matriz1$GrupoTrans,function(x){
        unname(t[as.character(x)])
    })
}

#Hacemos unique	
Matriz1$Transect <- NULL
Matriz1$GrupoTrans <- NULL

Matriz1 <- Matriz1 %>% unique()

#Corregimos nombres
names(Matriz1) <- unname(sapply(names(Matriz1),function(text){
    text <- gsub("\\.\\.","\\.",text)
    text <- gsub("\\.$","",text)   
    text <- gsub("Other.Other.","Other.",text)
    text <- gsub(".other$","",text)
}))

#Editamos variables
Matriz1$Site <- unname(sapply(Matriz1$Site,function(text){
    text <- gsub("Playa Tiburón","Playa Tiburon",text)
    text <- gsub("Cayo Agua","Cayo de Agua",text)
    text <- gsub("Dos Mosquises Sur","Dos Mosquises",text)
    text <- gsub("Madrisqui","Madriski",text)
    text <- gsub("Pelona de Rabusqui","Rabuski",text)
    text <- gsub("Boca de Cote","Cote",text)
    text <- gsub("Salinas","Las Salinas",text)
    text
}))

#Exportamos tabla /////////////////////////////////////

#Creamos clave para el merge
Matriz1$Clave <- paste(Matriz1$Locality,Matriz1$Site,sep = "_") 

#Merge
Matriz1 <- merge(Matriz1,metadataGeo,by="Clave",all.x = TRUE)
Matriz1$Clave <- NULL

setwd("~/Thesis Project AB/Data/Pre-processed Data")

write.csv(Matriz1,file="FactorsTransect_SCLESpecies_Proc1.csv",row.names = F)

#Limpiamos memoria.
rm(list = ls())
gc()

cat("Finalizado")
cat("\n")


