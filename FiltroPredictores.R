# Este codigo recibe las matrices ambientales MatrizAmbientalGIO1.csv y
# MatrizAmbientalGIO2.csv, filtra el conjunto de predictores y agrega las tablas
# resultantes directamente en la carpeta del modelo predictivo Shinypredictor.

# Las tablas generadas con este codigo son :
# MatrizAmbientalGIO1_Filtrada.csv y 
# MatrizAmbientalGIO2_Filtrada.csv

#FILTRANDO TABLA 1//////////////////////////////////////////////////////////////

MatrizAmbientalGIO1 <- read.csv("~/Thesis Project AB/Data/Ambientales/MatrizAmbientalGIO1.csv")

#Eliminamos variables colineales.
posNoElim <- which(names(MatrizAmbientalGIO1) %in% c(
    "AbsorCoeffPhytoplank.Q3",     
    "AbsorCoeffNonAlgalMat.Median" ,"ChlorophyllaConcent.Mean",    
    "PhotosyntheticRadiation.Q3"   ,"RemoteReflectance.Mean",      
    "SSTemperatureDay.Mean"        ,"SSTemperatureNight.Mean",     
    "SSTemperatureNight.Kurtosis"  ,"SeaLevelPreassure.Skewness",  
    "SeaSaltColumnDensity.Mean"    ,"SeaSaltColumnDensity.Sd",     
    "TwoMAirTemperature.Mean"      ,"SeaSaltConcentration.Mean",   
    "CarbonDioxide.Q1"             ,"CarbonDioxide.Skewness" ,     
    "MethaneNight.Median"          ,"MethaneNight.Q3",             
    "MethaneDay.Kurtosis"          ,"IncidentShortwave.Max",       
    "IncommingShortwave.Max"       ,"speed.cm.s.min"
))


#Las eliminamos
MatrizAmbientalGIO1 <- MatrizAmbientalGIO1[c(190,191,posNoElim)]

#Exportamos tabla
setwd("~/Thesis Project AB/ShinyPredictor")

write.csv(MatrizAmbientalGIO1,file = "MatrizAmbientalGIO1_Filtrada.csv",
          row.names = FALSE)

#FILTRANDO TABLA 2//////////////////////////////////////////////////////////////

MatrizAmbientalGIO2 <- read.csv("~/Thesis Project AB/Data/Ambientales/MatrizAmbientalGIO2.csv")

#Eliminamos variables colineales.
posNoElim <- which(names(MatrizAmbientalGIO2) %in% c(
    "AbsorCoeffPhytoplank.Q3",     
    "AbsorCoeffNonAlgalMat.Median" ,"ChlorophyllaConcent.Mean",    
    "PhotosyntheticRadiation.Q3"   ,"RemoteReflectance.Mean",      
    "SSTemperatureDay.Mean"        ,"SSTemperatureNight.Mean",     
    "SSTemperatureNight.Kurtosis"  ,"SeaLevelPreassure.Skewness",  
    "SeaSaltColumnDensity.Mean"    ,"SeaSaltColumnDensity.Sd",     
    "TwoMAirTemperature.Mean"      ,"SeaSaltConcentration.Mean",   
    "CarbonDioxide.Q1"             ,"CarbonDioxide.Skewness" ,     
    "MethaneNight.Median"          ,"MethaneNight.Q3",             
    "MethaneDay.Kurtosis"          ,"IncidentShortwave.Max",       
    "IncommingShortwave.Max"       ,"speed.cm.s.min"
))


#Las eliminamos
MatrizAmbientalGIO2 <- MatrizAmbientalGIO2[c(82,83,posNoElim)]

#Exportamos tabla
setwd("~/Thesis Project AB/ShinyPredictor")

write.csv(MatrizAmbientalGIO2,file = "MatrizAmbientalGIO2_Filtrada.csv",
          row.names = FALSE)

