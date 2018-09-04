#Codigo que permite reducir las matrices ambientales para conservar solo 
#aquellas variables importantes para el estudio. Cabe destacar que para
#caracterizar la costa venezolana, es necesario tener los puntos de interes bien
#definidos y agregarles la informacion climatologica usando Proyecto.R. Esto 
#se debe hacer por GEOLOCALIZACION y no por MERGE. Se debem emplear las dos 
#tablas generadas aca: 
#"MatrizAmbientalGIO1_Filtrada.csv" y "MatrizAmbientalGIO2_Filtrada.csv"

#FILTRANDO TABLA 1//////////////////////////////////////////////////////////////

MatrizAmbientalGIO1 <- read.csv("~/Thesis Project AB/Data/Pre-processed Data 2/MatrizAmbientalGIO1.csv")

#Eliminamos variables colineales.
posNoElim <- which(names(MatrizAmbientalGIO1) %in% c(
    "Laguna.d",                        
    "Pueblo.d",                         
    "Puerto.d",                        
    "Afluente.Seco.d",                  
    "Quebrada.d",                       
    "Rio.d",                            
    "Petroleo.d",
    "AerosolOpticalThickness.Q1",       
    "ChlorophyllaConcent.Mean",        
    "ParticInorganicCarbon.Max",        
    "PhotosyntheticRadiation.Q3",      
    "SSTemperatureDay.Mean" , 
    "SSTemperatureNight.Mean",         
    "ParticOrganicCarbon.Kurtosis",     
    "SeaSaltColumnDensity.Sd",          
    "SeaSaltColumnDensity.Skewness",
    "SO2SurfaceConcentration.Mean",  
    "CarbonDioxide.Skewness",  
    "IncommingShortwave.Mean", 
    "UpwellingLongwave.Mean"
))


#Las eliminamos
MatrizAmbientalGIO1 <- MatrizAmbientalGIO1[c(190,191,posNoElim)]

#Exportamos tabla
setwd("~/Thesis Project AB/Data/Pre-processed Data 2")

write.csv(MatrizAmbientalGIO1,file = "MatrizAmbientalGIO1_Filtrada.csv",
          row.names = FALSE)

#FILTRANDO TABLA 2//////////////////////////////////////////////////////////////

MatrizAmbientalGIO2 <- read.csv("~/Thesis Project AB/Data/Pre-processed Data 2/MatrizAmbientalGIO2.csv")

#Eliminamos variables colineales.
posNoElim <- which(names(MatrizAmbientalGIO2) %in% c(
    "Laguna.d",                        
    "Pueblo.d",                         
    "Puerto.d",                        
    "Afluente.Seco.d",                  
    "Quebrada.d",                       
    "Rio.d",                            
    "Petroleo.d",
    "AerosolOpticalThickness.Q1",       
    "ChlorophyllaConcent.Mean",        
    "ParticInorganicCarbon.Max",        
    "PhotosyntheticRadiation.Q3",      
    "SSTemperatureDay.Mean" , 
    "SSTemperatureNight.Mean",         
    "ParticOrganicCarbon.Kurtosis",     
    "SeaSaltColumnDensity.Sd",          
    "SeaSaltColumnDensity.Skewness",
    "SO2SurfaceConcentration.Mean",  
    "CarbonDioxide.Skewness",  
    "IncommingShortwave.Mean", 
    "UpwellingLongwave.Mean"
))


#Las eliminamos
MatrizAmbientalGIO2 <- MatrizAmbientalGIO2[c(82,83,posNoElim)]

#Exportamos tabla
setwd("~/Thesis Project AB/Data/Pre-processed Data 2")

write.csv(MatrizAmbientalGIO2,file = "MatrizAmbientalGIO2_Filtrada.csv",
          row.names = FALSE)
