#Este codigo recibe la tabla de latitud-longitud de todos los puntos de 
#geolocalizacion del mar "venezolano" que "podria" ser habitado por corales. 
#La misma ya debe estar cargada con toda la informacion ambiental y este script
#solo le da el formato adecuado. Se crean todos los predictores necesarios.
#La tabla generada por este codigo sera utilizada para generar la prediccion 
#completa con el Random Forest.

PredAmb_proc1_proc1 <- 
    read.csv("C:/Users/Alejandro/Desktop/Union por Geolocalizacion/Datos Finales/PredAmb_proc1_proc1.csv")

PredAmb_proc1_proc1$Contorno <- NULL
PredAmb_proc1_proc1$Aeropuerto.d <-NULL
PredAmb_proc1_proc1$Atracadero.d <-NULL
PredAmb_proc1_proc1$Salina.d <-NULL
PredAmb_proc1_proc1$Industria.Minera.d <-NULL
PredAmb_proc1_proc1$Zona.Hipoxica.d <-NULL

DataSet <- PredAmb_proc1_proc1

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
    "Laguna.d",                        
    "Poblado.d",                         
    "Puerto.d",                        
    "Afluente.Seco.d",                  
    "Rio.d",                            
    "Petroleo.d" ,
    "ChlorophyllaConcent.Mean",        
    "PhotosyntheticRadiation.Q3",      
    "SSTemperatureDay.Mean" , 
    "SSTemperatureNight.Mean",         
    "ParticOrganicCarbon.Kurtosis",     
    "SeaSaltColumnDensity.Sd",          
    "SeaSaltColumnDensity.Skewness",
    "CarbonDioxide.Skewness",  
    "IncommingShortwave.Mean", 
    "UpwellingLongwave.Mean",
    "AerosolOpticalThickness.Skewness",   ###    
    "ParticInorganicCarbon.Skewness",   ## 
    "PartiBackscatteringCoef.Max",     # ##
    "SurfaceWindSpeed.Sd" , ##
    "speed.cm.s.mean"
)

DataSet <- DataSet[posNoElim]

DataSet <- DataSet[complete.cases(DataSet),]

#Creamos los predictores adicionales. Lat Lon
DataSet$Lat <- DataSet$latitude
DataSet$Lon <- DataSet$longitude

#Transformamos 999's en 120's en las variables de Google Earth
for(k in 3:8){
    DataSet[,k][DataSet[k][,1] == 999] <- rep(120,sum(DataSet[k][,1] == 999))
}

#Exportamos
setwd("~/Thesis Project AB/Data/Final Data")

write.csv(DataSet,file = "PredictoresMarVzla.csv",row.names=F)
