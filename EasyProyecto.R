# Este script recibe las tablas generadas por DatosGoogleEarthCor.R y les
# agrega las variables ambientales sin la necesidad de usar Proyecto.R multiples
# veces
#

#Importamos la tabla procesada por Proyecto.R junto con las demas. Note que esa 
#tabla debio pasar dos veces por Proyecto.R para recibir la informacion 
#ambiental contenida en las dos matrices climatologicas MatrizAmbientalGIO1.csv 
#y MatrizAmbientalGIO2.csv. 

#Esa tabla generada es FactorsTransect_BenthicSubstrate_ProcMan2_proc1_proc1.csv

#Dicha tabla sera incluida en MatBio1 para agregarle la informacio climatologica
#al resto. Asegurese de que MatBio1 contenga TODAS las variables metereologicas
#Deben estar tanto las de MatrizAmbientalGIO1 como las de MatrizAmbientalGIO2

#Dataset principal
MatBio1 <- read.csv(
"~/Thesis Project AB/Data/Processed Data/FactorsTransect_BenthicSubstrate_ProcMan2_proc1_proc1.csv")

#Tabla para merge 1
MatBio2 <- read.csv(
"~/Thesis Project AB/Data/Processed Data/FactorsTransect_GreatGroups_ProcMan2.csv")

#Tabla para merge 2
MatBio3 <- read.csv(
"~/Thesis Project AB/Data/Processed Data/FactorsTransect_SCLEGrowth_ProcMan2.csv")

#Tabla para merge 3
MatBio4 <- read.csv(
"~/Thesis Project AB/Data/Processed Data/FactorsTransect_SCLESpecies_ProcMan2.csv")

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

write.csv(MatBio1,file = "FactorsTransect_BenthicSubstrate_Matrix.csv",
          row.names = FALSE)
write.csv(MatBio2,file = "FactorsTransect_GreatGroups_Matrix.csv",
          row.names = FALSE)
write.csv(MatBio3,file = "FactorsTransect_SCLEGrowth_Matrix.csv",
          row.names = FALSE)
write.csv(MatBio4,file = "FactorsTransect_SCLESpecies_Matrix.csv",
          row.names = FALSE)