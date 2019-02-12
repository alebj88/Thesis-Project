#Este codigo recibe la tabla de latitud-longitud de todos los puntos de 
#geolocalizacion del mar venezolano que podria ser habitado por corales luego
#de recibir datos ambientales con el codigo "union por geolocalizacion.R".

#El mismo debio ser ejecutado 4 veces pata agregar la informacion demografica
#de distancia minima, los datos de corrientes y los oceanograficos.

#Los datos son creados directamente en la carpeta ShinyPredictor

library(dplyr)
 
MarVzla <- read.csv(
    "~/Thesis Project AB/Union por Geolocalizacion/Datos Finales/TablaMin_Mar_Vzla_proc1_proc1_proc1_proc1.csv")

MarVzla$Aeropuerto.d <-NULL
MarVzla$Atracadero.d <-NULL
MarVzla$Salina.d <-NULL
MarVzla$Industria.Minera.d <-NULL
MarVzla$Zona.Hipoxica.d <-NULL

DataSet <- MarVzla

#Unimos objetos costeros
DataSet$Afluente.Seco.d <- unname(apply(DataSet[c(
    "Quebrada.Seca.d","Rio.Seco.d")],1,function(x){
        vect <- min(x,na.rm = T)
        return(vect)
    }))

DataSet$Quebrada.Seca.d <- NULL
DataSet$Rio.Seco.d <- NULL

#Unimos objetos costeros
DataSet$Petroleo.d <- unname(apply(DataSet[c(
    "Termoelectrica.d","Refineria.d","Petroquimica.d")],1,function(x){
        vect <- min(x,na.rm = T)
        return(vect)
    }))

DataSet$Termoelectrica.d <- NULL
DataSet$Refineria.d <- NULL
DataSet$Petroquimica.d <- NULL

#Unimos objetos costeros
DataSet$Poblado.d <- unname(apply(DataSet[c(
    "Pueblo.d","Ciudad.d")],1,function(x){
        vect <- min(x,na.rm = T)
        return(vect)
    }))

DataSet$Pueblo.d <- NULL
DataSet$Ciudad.d <- NULL
DataSet$Canal.d <- NULL

posNoElim <- c(
    "latitude",
    "longitude",
    'InsularCoast',
    "Laguna.d"                     ,"Puerto.d",                   
    "Rio.d"                        ,"AbsorCoeffPhytoplank.Q3",     
    "AbsorCoeffNonAlgalMat.Median" ,"ChlorophyllaConcent.Mean",    
    "PhotosyntheticRadiation.Q3"   ,"RemoteReflectance.Mean",      
    "SSTemperatureDay.Mean"        ,"SSTemperatureNight.Mean",     
    "SSTemperatureNight.Kurtosis"  ,"SeaLevelPreassure.Skewness",  
    "SeaSaltColumnDensity.Mean"    ,"SeaSaltColumnDensity.Sd",     
    "TwoMAirTemperature.Mean"      ,"SeaSaltConcentration.Mean",   
    "CarbonDioxide.Q1"             ,"CarbonDioxide.Skewness" ,     
    "MethaneNight.Median"          ,"MethaneNight.Q3",             
    "MethaneDay.Kurtosis"          ,"IncidentShortwave.Max",       
    "IncommingShortwave.Max"       ,"speed.cm.s.min",              
    "Afluente.Seco.d"              ,"Petroleo.d" ,                 
    "Poblado.d"
)

#Eliminamos predictores y datos faltantes
DataSet <- DataSet[posNoElim]
DataSet <- DataSet[complete.cases(DataSet),]

#Eliminamos valores repetidos
DataSet <- DataSet %>% unique()

#Reordenamos
DataSet <- DataSet[c(1:6,28:30,7:27)]

#Transformamos 999's en 120's en las variables de Google Earth
for(k in 4:9){
    DataSet[,k][DataSet[k][,1] == 999] <- rep(120,sum(DataSet[k][,1] == 999))
}

#Exportamos
setwd("~/Thesis Project AB/ShinyPredictor")

write.csv(DataSet,file = "PredictoresMarVzla.csv",row.names=F)
