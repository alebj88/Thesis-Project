#Segundo procesamiento de la data.
#
#
setwd("~/Thesis Project/Data/Pre-processed Data")
cat("Importando tablas de la carpeta Pre-processed Data\n")
cat("\n")

#Importando todos los archivos CSV de la carpeta Pre-processed Data.

cariacoOc<-read_csv("OceanographicCariaco(Ci_i_NODC).csv",na=c("-9999.000000",
                   "-9999.00000","-9999.0000","-9999.000","-9999.00","-9999.0"))
cariacoTS <-read_csv("CariacoTimeSeries.csv")
corrientes <-read_csv("CorrientesMarinas.csv")
boyasNBDC <-read_csv("BoyasNBDC.csv")
boyasArgo <-read_csv("BoyasArgo.csv")
world <-read_csv("WorldOceanicAtlas2013.csv")

#Procesando tabla cariacoOc-----------------------------------------------------
#Eliminamos columna de rownames y damos formato a cariacoOc.
cariacoOc<-cariacoOc[,-1]
names(cariacoOc)<-paste0(names(cariacoOc),".",cariacoOc[1,])
cariacoOc<-cariacoOc[-1,]

#Nombres sin parentesis.
names(cariacoOc)<-sapply(names(cariacoOc),function(x){gsub("\\(",".",x)})
names(cariacoOc)<-sapply(names(cariacoOc),function(x){gsub(")","",x)})

#formato de Fecha y Hora.
aux<-paste0("0",cariacoOc$date.mddyy)
cariacoOc$date.mddyy[sapply(cariacoOc$date.mddyy,nchar)==5]<-aux[
    sapply(cariacoOc$date.mddyy,nchar)==5]

aux<-paste0("0",cariacoOc$time.local.hhmm)
cariacoOc$time.local.hhmm[sapply(cariacoOc$time.local.hhmm,nchar)==3]<-aux[
    sapply(cariacoOc$time.local.hhmm,nchar)==3]
cariacoOc$time.local.hhmm<-paste0(cariacoOc$time.local.hhmm,"00")

#Creamos variable date.
cariacoOc$date<-paste(cariacoOc$date.mddyy,cariacoOc$time.local.hhmm,sep=" ")
cariacoOc$date<-strptime(cariacoOc$date,"%m%d%y %H%M%S")

#Corregimos ambiguedad de formato en la variable "date.mddyy".
cariacoOc$date.mddyy[is.na(cariacoOc$date)]<-rep("051512",30)
cariacoOc$date<-paste(cariacoOc$date.mddyy,cariacoOc$time.local.hhmm,sep=" ")
cariacoOc$date<-strptime(cariacoOc$date,"%m%d%y %H%M%S")

cariacoOc$time.local.hhmm<-NULL
cariacoOc$date.mddyy<-NULL

#Renombrando variables
names(cariacoOc)[6]<-"latitude"
names(cariacoOc)[7]<-"longitude"
names(cariacoOc)[8]<-"depth.m"  #Original: target_depth.m

#Creando clave para Merge.
cariacoOc$clave<-paste(as.character(cariacoOc$date),cariacoOc$latitude,
                         cariacoOc$longitude,cariacoOc$depth.m,sep=" ")

#Eliminamos filas donde tenemos claves repetidas. Se observo que en estos
#casos los valores de las variables eran identicos salvo el numero de la 
#"botella". El numero maximo de veces que una clave se repitio fue dos.
cariacoOc<-cariacoOc[match(unique(cariacoOc$clave),cariacoOc$clave),]

#Procesando tabla cariacoTS---------------------------------------------------

#Correcion rapida de la variable date.
cariacoTS$date[is.na(cariacoTS$date) & 
                   cariacoTS$notes=="6Dec2010"]<-"2010-12-06"
cariacoTS$date[is.na(cariacoTS$date) & 
                   cariacoTS$notes=="7Dec2010"]<-"2010-12-07"

#Eliminamos columna 1 pero guardamos su informacion.
infoTS<-cariacoTS[,1]
cariacoTS<-cariacoTS[,-1]
names(cariacoTS)<-paste0(names(cariacoTS),".",cariacoTS[1,],cariacoTS[2,])
names(cariacoTS)<-sapply(names(cariacoTS),function(x){gsub("NA","",x)})
cariacoTS<-cariacoTS[-c(1,2),]

#Renombrando variables
names(cariacoTS)[1]<-"date"
names(cariacoTS)[2]<-"latitude"
names(cariacoTS)[3]<-"longitude"
names(cariacoTS)[4]<-"depth.m"  #Original: depth.meters

#Damos formato a la variable date.
cariacoTS$date<-paste(as.character(cariacoTS$date),
                      rep("00:01:00",nrow(cariacoTS),sep=" "))
cariacoTS$date<-strptime(cariacoTS$date,"%Y-%m-%d %H:%M:%S")

#DEBEMOS IMPUTAR LOS VALORES FALTANTES DE LA VARIABLE depth.m.
#//////////??///?///???????

#Creando clave para Merge.
cariacoTS$clave<-paste(as.character(cariacoTS$date),cariacoTS$latitude,
                       cariacoTS$longitude,cariacoTS$depth.m,sep=" ")


#Procesando tabla corrientes---------------------------------------------------






#Creamos clave para los merges.-------------------------

#Hacemos coincidir nombres de variables.
names(boyasArgo)[1]<-"date"
names(boyasArgo)[2]<-"latitude"
names(boyasArgo)[3]<-"longitude"


setwd("~/Thesis Project/Data/Pre-processed Data 2")
write.table(file="HereWeAre.txt",data.frame(prue=c(1,2,3),ba=c(4,5,6)))

cat("Procedimiento Getting-Preprocessing2.R finalizado.\n")
cat("\n")
