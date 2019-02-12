#Procesamiento de datos biologicos.
#Recoge los datos biologicos generados por el LEEUSB presentes en la carpeta Raw
#Data y los promedia por transecta. Las tablas resultantes son almacenadas en
#el directorio Biologicas del proyecto.

library(dplyr)

#Importamos datos
DataSet1 <- read.csv(
    "~/Thesis Project AB/Data/Raw Data/FactorsTransect_BenthicSubstrate.csv")
DataSet2 <- read.csv(
    "~/Thesis Project AB/Data/Raw Data/FactorsTransect_GreatGroups.csv")
DataSet3 <- read.csv(
    "~/Thesis Project AB/Data/Raw Data/FactorsTransect_SCLEGrowth.csv")
DataSet4 <- read.csv(
    "~/Thesis Project AB/Data/Raw Data/FactorsTransect_SCLESpecies.csv")


#Eliminamos variables inecesarias
DataSet1 <- DataSet1[,-c(1,4,5)]
DataSet2 <- DataSet2[,-c(1,4,5)]
DataSet3 <- DataSet3[,-c(1,4,5)]
DataSet4 <- DataSet4[,-c(1,4,5)]

#Promediamos transectas
for(i in 5:ncol(DataSet1)){
    t <- tapply(DataSet1[,i],DataSet1$Site,mean)	
    DataSet1[,i] <- sapply(DataSet1$Site,function(x){unname(t[as.character(x)])})
}

for(i in 5:ncol(DataSet2)){
    t <- tapply(DataSet2[,i],DataSet2$Site,mean)	
    DataSet2[,i] <- sapply(DataSet2$Site,function(x){unname(t[as.character(x)])})
}

for(i in 5:ncol(DataSet3)){
    t <- tapply(DataSet3[,i],DataSet3$Site,mean)	
    DataSet3[,i] <- sapply(DataSet3$Site,function(x){unname(t[as.character(x)])})
}

for(i in 5:ncol(DataSet4)){
    t <- tapply(DataSet4[,i],DataSet4$Site,mean)	
    DataSet4[,i] <- sapply(DataSet4$Site,function(x){unname(t[as.character(x)])})
}

#Elimiamos repetidas
DataSet1 <- DataSet1 %>% unique()
DataSet2 <- DataSet2 %>% unique()
DataSet3 <- DataSet3 %>% unique()
DataSet4 <- DataSet4 %>% unique()

#Colcemos Site un character
DataSet1$Site <- as.character(DataSet1$Site)
DataSet2$Site <- as.character(DataSet2$Site)
DataSet3$Site <- as.character(DataSet3$Site)
DataSet4$Site <- as.character(DataSet4$Site)

#Renombramos sitios
DataSet1$Site[DataSet1$Site == "Cayo Agua"] <- "Cayo de Agua"
DataSet2$Site[DataSet2$Site == "Cayo Agua"] <- "Cayo de Agua"
DataSet3$Site[DataSet3$Site == "Cayo Agua"] <- "Cayo de Agua"
DataSet4$Site[DataSet4$Site == "Cayo Agua"] <- "Cayo de Agua"

DataSet1$Site[DataSet1$Site == "Dos Mosquises Sur"] <- "Dos Mosquises"
DataSet2$Site[DataSet2$Site == "Dos Mosquises Sur"] <- "Dos Mosquises"
DataSet3$Site[DataSet3$Site == "Dos Mosquises Sur"] <- "Dos Mosquises"
DataSet4$Site[DataSet4$Site == "Dos Mosquises Sur"] <- "Dos Mosquises"

DataSet1$Site[DataSet1$Site == "Boca de Cote"] <- "Cote"
DataSet2$Site[DataSet2$Site == "Boca de Cote"] <- "Cote"
DataSet3$Site[DataSet3$Site == "Boca de Cote"] <- "Cote"
DataSet4$Site[DataSet4$Site == "Boca de Cote"] <- "Cote"

DataSet1$Site[DataSet1$Site == "Madrisqui"] <- "Madriski"
DataSet2$Site[DataSet2$Site == "Madrisqui"] <- "Madriski"
DataSet3$Site[DataSet3$Site == "Madrisqui"] <- "Madriski"
DataSet4$Site[DataSet4$Site == "Madrisqui"] <- "Madriski"

DataSet1$Site[DataSet1$Site == "Playa Tiburón"] <- "Playa Tiburon"
DataSet2$Site[DataSet2$Site == "Playa Tiburón"] <- "Playa Tiburon"
DataSet3$Site[DataSet3$Site == "Playa Tiburón"] <- "Playa Tiburon"
DataSet4$Site[DataSet4$Site == "Playa Tiburón"] <- "Playa Tiburon"

DataSet1$Site[DataSet1$Site == "Pelona de Rabusqui"] <- "Rabuski"
DataSet2$Site[DataSet2$Site == "Pelona de Rabusqui"] <- "Rabuski"
DataSet3$Site[DataSet3$Site == "Pelona de Rabusqui"] <- "Rabuski"
DataSet4$Site[DataSet4$Site == "Pelona de Rabusqui"] <- "Rabuski"

DataSet1$Site[DataSet1$Site == "Salinas"] <- "Las Salinas"
DataSet2$Site[DataSet2$Site == "Salinas"] <- "Las Salinas"
DataSet3$Site[DataSet3$Site == "Salinas"] <- "Las Salinas"
DataSet4$Site[DataSet4$Site == "Salinas"] <- "Las Salinas"

#ordenamos los datasets en funcion del sitio
orden <- c(
    "La Pared",             "Media Legua",         
    "Petaquire",            "Playa Tiburon",       
    "Punta de Media Legua", "Punta Mono",          
    "Charagato",            "La Muerta",           
    "Punta Conejo",         "Cominoto" ,           
    "La Pecha",             "Puerto Real" ,        
    "Cayo de Agua",         "Cote" ,               
    "Dos Mosquises",        "La Venada" ,          
    "Las Salinas",          "Madriski",            
    "Rabuski",              "Blanca" ,             
    "Carabela",             "Gabarra",             
    "Garrapata",            "Punta Cruz" ,         
    "San Agustin",          "Bajo Caiman",         
    "Boca Seca",            "Medio",               
    "Mero",                 "Norte",               
    "Sombrero",             "Sur",                 
    "Cienaga este",         "Cienaga interno",     
    "Cienaga oeste",        "Guabinitas"
)   

orden1 <- sapply(orden,function(tx){
    p <- which(tx == DataSet1$Site)
    if(length(p) == 0){
        return(0)
    }
    return(p)
})

orden2 <- sapply(orden,function(tx){
    p <- which(tx == DataSet2$Site)
    if(length(p) == 0){
        return(0)
    }
    return(p)
})

orden3 <- sapply(orden,function(tx){
    p <- which(tx == DataSet3$Site)
    if(length(p) == 0){
        return(0)
    }
    return(p)
})

orden4 <- sapply(orden,function(tx){
    p <- which(tx == DataSet4$Site)
    if(length(p) == 0){
        return(0)
    }
    return(p)
})

DataSet1 <- DataSet1[orden1,]
DataSet2 <- DataSet2[orden2,]
DataSet3 <- DataSet3[orden3,]
DataSet4 <- DataSet4[orden4,]

#Agregamos latitud - longitud

latitude <- c(10.554, 10.555, 10.548, 10.547, 10.557, 10.551, 10.816,
              10.800, 10.830, 11.215, 11.202, 11.188, 11.817, 11.773,
              11.793, 11.879, 11.740, 11.940, 11.882, 10.394, 10.387,
              10.382, 10.381, 10.396, 10.382, 10.851, 10.834, 10.742,
              10.820, 10.780, 10.889, 10.729, 10.487, 10.475, 10.478,
              10.484)

longitude <- c(-67.239, -67.210, -67.277, -67.267, -67.203, -67.252,
               -64.227, -64.217, -64.165, -63.768, -63.753, -63.730,
               -66.940, -66.721, -66.896, -66.724, -66.807, -66.661,
               -66.689, -64.335, -64.375, -64.338, -64.338, -64.368,
               -64.347, -68.069, -68.237, -68.236, -68.248, -68.201,
               -68.215, -68.244, -67.804, -67.809, -67.809, -67.825)

DataSet1$latitude <- latitude
DataSet2$latitude <- latitude
DataSet3$latitude <- latitude
DataSet4$latitude <- latitude

DataSet1$longitude <- longitude
DataSet2$longitude <- longitude
DataSet3$longitude <- longitude
DataSet4$longitude <- longitude

#Corregimos nombres de fila
rownames(DataSet1) <- 1:36
rownames(DataSet2) <- 1:36
rownames(DataSet3) <- 1:36
rownames(DataSet4) <- 1:36

#Correcciones adicionales
names(DataSet3)[names(DataSet3) == "Solitary..Other"] <- "Solitary.Other"

names(DataSet4) <- sapply(names(DataSet4),function(txt){
    txt <- gsub("\\.\\.","\\.",txt)
    txt <- gsub("\\.$","",txt)
    return(txt)
})

names(DataSet1) <- sapply(names(DataSet1),function(txt){
    txt <- gsub("\\.\\.","\\.",txt)
    txt <- gsub("\\.$","",txt)
    return(txt)
})


#Exportamos en Biologicas
setwd("~/Thesis Project AB/Data/Biologicas")

write.csv(DataSet1,file = "BenthicSubstrate.csv",
          row.names = FALSE)
write.csv(DataSet2,file = "GreatGroups.csv",
          row.names = FALSE)
write.csv(DataSet3,file = "SCLEGrowth.csv",
          row.names = FALSE)
write.csv(DataSet4,file = "SCLESpecies.csv",
          row.names = FALSE)

cat("Las tablas: 
    SCLESpecies.csv,
    SCLEGrowth.csv,
    GreatGroups.csv y 
    BenthicSubstrate.csv
    se crearon exitosamente en el directorio Biologicas de la carpeta 
    Datos del proyecto.\n")
cat("\n")

#Liberamos memoria
rm(list=ls())
gc()

cat("Procedimiento Biologicas.R finalizado.\n")
cat("\n")
cat("/////////////////////////////////////////////////\n")
cat("\n")
