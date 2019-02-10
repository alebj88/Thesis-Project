# Este script recibe la tabla generada por el programa 
# "union por geolocalizacion.R" al ejecutarlos 4 veces sobre la tabla biologica 
# FactorsTransect_BenthicSubstrate.csv para agregarle la informacion ambiental
# contenida en las tablas:

# GoogleEarthRiosLigares.csv        (Datos de distancia minima)
# MatrizAmbientalGIO1.csv  
# MatrizAmbientalGIO2.csv 
# corrientesAux.csv.

# Dicha tabla se llama 
# TablaMin_FactorsTransect_BenthicSubstrate_proc1_proc1_proc1_proc1.csv
# Y es la que utiliza este codigo para agregarle los datos oceanograficmo y 
# demograficos al resto de las tablas biologicas.

# Las tablas reesultantes (Biologicas con informacion ambiental) son guardadas
# en la carpeta "Datos Finales" del directorio del proyecto.

#Dataset principal
MatBio1 <- read.csv(
"~/Thesis Project AB/Union por Geolocalizacion/Datos Finales/TablaMin_FactorsTransect_BenthicSubstrate_proc1_proc1_proc1_proc1.csv")

#Tabla para merge 1
MatBio2 <- read.csv(
"~/Thesis Project AB/Union por Geolocalizacion/Datos Finales/FactorsTransect_GreatGroups.csv")

#Tabla para merge 2
MatBio3 <- read.csv(
"~/Thesis Project AB/Union por Geolocalizacion/Datos Finales/FactorsTransect_SCLEGrowth.csv")

#Tabla para merge 3
MatBio4 <- read.csv(
"~/Thesis Project AB/Union por Geolocalizacion/Datos Finales/FactorsTransect_SCLESpecies.csv")

#Claves para merge
MatBio1$clave <- paste(MatBio1$latitude,MatBio1$longitude)

MatBio2$clave <- paste(MatBio2$latitude,MatBio2$longitude)
MatBio3$clave <- paste(MatBio3$latitude,MatBio3$longitude)
MatBio4$clave <- paste(MatBio4$latitude,MatBio4$longitude)

#Eliminamos las variables inecesarias de MatBio1 
MatBio1 <- MatBio1[154:ncol(MatBio1)]

#Check de unique
length(unique(MatBio1$clave)) == nrow(MatBio1)

length(unique(MatBio2$clave)) == nrow(MatBio2)
length(unique(MatBio3$clave)) == nrow(MatBio3)
length(unique(MatBio4$clave)) == nrow(MatBio4)

#Merge
MatBio2 <- merge(MatBio2,MatBio1,by = "clave",all.x = T)
MatBio3 <- merge(MatBio3,MatBio1,by = "clave",all.x = T)
MatBio4 <- merge(MatBio4,MatBio1,by = "clave",all.x = T)

#Eliminamos claves
MatBio2$clave <- NULL
MatBio3$clave <- NULL
MatBio4$clave <- NULL

#Exportamos a final data
setwd("~/Thesis Project AB/Data/Final Data")

MatBio1 <- read.csv(
"~/Thesis Project AB/Union por Geolocalizacion/Datos Finales/TablaMin_FactorsTransect_BenthicSubstrate_proc1_proc1_proc1_proc1.csv")

write.csv(MatBio1,file = "FactorsTransect_BenthicSubstrate_Matrix.csv",
          row.names = FALSE)
write.csv(MatBio2,file = "FactorsTransect_GreatGroups_Matrix.csv",
          row.names = FALSE)
write.csv(MatBio3,file = "FactorsTransect_SCLEGrowth_Matrix.csv",
          row.names = FALSE)
write.csv(MatBio4,file = "FactorsTransect_SCLESpecies_Matrix.csv",
          row.names = FALSE)

cat("Las tablas: 
    FactorsTransect_SCLESpecies_Matrix.csv,
    FactorsTransect_SCLEGrowth_Matrix.csv,
    FactorsTransect_GreatGroups_Matrix.csv y 
    FactorsTransect_BenthicSubstrate_Matrix.csv
    se crearon exitosamente en el directorio Datos Finales de la carpeta 
    Datos del proyecto.\n")
cat("\n")

#Liberamos memoria
rm(list=ls())
gc()

cat("Procedimiento EasyProyecto.R finalizado.\n")
cat("\n")
cat("/////////////////////////////////////////////////\n")
cat("\n")
