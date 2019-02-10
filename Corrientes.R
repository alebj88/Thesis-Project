library(rnoaa)
library(ggmap) 
library(ncdf4)
library(grDevices)
library(dplyr)
library(tidyr)


setwd("~/Thesis Project AB/R-Scripts")
source('Functions.R')	

#Autenticacion
if(NOAAkey == FALSE){
    stop('Se requiere autenticacion en la NOAA. 
         Comuniquese con el administrador del codigo.')
}

cat("Conectando con la base de datos de la NOAA.\n")
cat("\n")


#///////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////
#-------------------------------------------------------------------------------
#Fuente temporal:https://www.google.co.ve/search?q=u.cm.s+v.cm.s+direction.deg.t+marine+currents+NOAA&nirf=u.cm.s+to.cm.s+direction.deg.t+marine+currents+NOAA&sa=X&ved=0ahUKEwj4m7qR_-zYAhUK71MKHUCaB_0Q8BYIJCgB&biw=1270&bih=649
#El rango de fechas esta fuera de rango. 1900-1985.
#Data de corrientes marinas
setwd("~/Thesis Project AB/Data/Raw Data")

cat("Descargando y procesando datos sobre las corrientes marinas...\n")
cat("\n")
u1 <- "https://www.nodc.noaa.gov/gocd/data/a0098060/gocd_scuds_all.csv.gz"
download.file(url = u1,destfile ="gocd_scuds_all.csv.gz")

con <- gzfile("gocd_scuds_all.csv.gz")
dat <- read.csv(con,header=TRUE)			

#Filtrando datos del caribe y procesando datos
dat <- dat[(-73 <= dat$Longitude) & (dat$Longitude <= -60) &
             (8 <= dat$Latitude) & (dat$Latitude <= 17),]
names(dat) <- sapply(names(dat),tolower)

#Escribiendo tabla
setwd("~/Thesis Project AB/Data/Raw Data")

#Grafico de geolocalizacion del atlas
lat <- dat$latitude
lon <- dat$longitude
df <- data.frame(lat=lat,lon=lon)

arch <- "CorrientesMarinas.png"
titulo <- "Corrientes Marinas"
#Grafico de corrientes carpeta Ambientales.
setwd("~/Thesis Project AB/Data/Ambientales")
MapaRawData(df = df,file = arch,title = titulo)
setwd("~/Thesis Project AB/Data/Raw Data")

corrientes <- dat

rm(df);rm(lat);rm(lon);rm(arch);rm(titulo)
rm(dat);rm(con);rm(u1)
rm(list=ls())
gc()

cat("Procesando tabla de corrientes marinas...\n")
cat("\n")

#Eliminamos las filas con fechas incorrectas.
corrientes$date[grep("00$",corrientes$date)]<-NA
corrientes$date[corrientes$date=="1908-6.35e+00-22"]<-NA
corrientes<-corrientes[!is.na(corrientes$date),]

corrientes$aux<-rep("00:01:00",times=nrow(corrientes))
corrientes$date<-paste(as.character(corrientes$date),corrientes$aux,sep=" ")
corrientes$aux<-NULL
corrientes$date<-strptime(corrientes$date,"%Y-%m-%d %H:%M:%S")
corrientes<-corrientes[!is.na(corrientes$date),]

#BUscamos los puntos de geolocalizaccion del dataFrame "Corrientes" que mejor 
#aproximen los de Cariaco.  

corrientes$latitude<-as.numeric(as.character(corrientes$latitude))
corrientes$longitude<-as.numeric(as.character(corrientes$longitude))

#Creamos data frame de corrientes que resume la informacion por medio del
#promedio y la desviacion estandar.
corrientes$clave2 <-paste(corrientes$latitude,corrientes$longitude,sep=" ")

corrientesAux <-as.data.frame(
    tapply(corrientes$u.cm.s.,corrientes$clave2,mean))
names(corrientesAux) <-"u.cm.s.mean"

corrientesAux$u.cm.s.sd <-as.numeric(
    unname(tapply(corrientes$u.cm.s.,corrientes$clave2,sd)))
corrientesAux$u.cm.s.sd[is.na(corrientesAux$u.cm.s.sd)] <-0

corrientesAux$v.cm.s.mean <-as.numeric(
    unname(tapply(corrientes$v.cm.s.,corrientes$clave2,mean)))
corrientesAux$v.cm.s.sd <-as.numeric(
    unname(tapply(corrientes$v.cm.s.,corrientes$clave2,sd)))
corrientesAux$v.cm.s.sd[is.na(corrientesAux$v.cm.s.sd)] <-0

corrientesAux$speed.cm.s.mean <-as.numeric(     #Velocidad promedio.
    unname(tapply(corrientes$speed.cm.s.,corrientes$clave2,mean)))
corrientesAux$speed.cm.s.sd <-as.numeric(       #Dispersion de la velocidad.
    unname(tapply(corrientes$speed.cm.s.,corrientes$clave2,sd)))
corrientesAux$speed.cm.s.sd[is.na(corrientesAux$speed.cm.s.sd)] <-0

corrientesAux$speed.cm.s.max <-as.numeric(      #Velocidad maxima.
    unname(tapply(corrientes$speed.cm.s.,corrientes$clave2,max)))
corrientesAux$speed.cm.s.min <-as.numeric(      #Velocidad minima.
    unname(tapply(corrientes$speed.cm.s.,corrientes$clave2,min)))

corrientesAux$direction.deg.t.mean <-as.numeric(
    unname(tapply(corrientes$direction.deg.t.,corrientes$clave2,mean)))
corrientesAux$direction.deg.t.sd <-as.numeric(
    unname(tapply(corrientes$direction.deg.t.,corrientes$clave2,sd)))
corrientesAux$direction.deg.t.sd[is.na(corrientesAux$direction.deg.t.sd)] <-0

corrientesAux$clave2 <- row.names(corrientesAux)

#Le damos formato.\\\\\\\\\\\\\\\\\\\\\\\\\
corrientesAux <- separate(corrientesAux,
                          col = clave2,into=c("latitude","longitude"),sep=" ")

corrientesAux$latitude  <- as.numeric(corrientesAux$latitude)
corrientesAux$longitude <- as.numeric(corrientesAux$longitude)

#Exportamos
setwd("~/Thesis Project AB/Data/Ambientales")
write.csv(corrientesAux,file="CorrientesAux.csv",row.names=FALSE)

cat("La tabla CorrientesAux.csv se creo exitosamente en el directorio 
    Ambientales.\n")
cat("\n")

#Liberamos memoria
rm(list=ls())
gc()

cat("Procedimiento Corrientes.R finalizado.\n")
cat("\n")
cat("/////////////////////////////////////////////////\n")
cat("\n")

