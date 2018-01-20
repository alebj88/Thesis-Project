#Segundo procesamiento de la data.
#
library(tidyr)

setwd("~/Thesis Project/Data/Pre-processed Data")
cat("Importando tablas de la carpeta Pre-processed Data\n")
cat("\n")

#Importando todos los archivos CSV de la carpeta Pre-processed Data.

cariacoOc<-read.csv("OceanographicCariaco(Ci_i_NODC).csv",na=c("-9999.000000",
                   "-9999.00000","-9999.0000","-9999.000","-9999.00","-9999.0"),
                   stringsAsFactors=FALSE)
cariacoTS <-read.csv("CariacoTimeSeries.csv",na=c("n.d.",NA),
                     stringsAsFactors=FALSE)
corrientes <-read.csv("CorrientesMarinas.csv")
boyasNBDC <-read.csv("BoyasNBDC.csv")
boyasArgo <-read.csv("BoyasArgo.csv")
world <-read.csv("WorldOceanicAtlas2013.csv")

#desgargando metadata.
cat("Descargando informacion sobre la metadata.\n")
cat("\n")
setwd("~/Thesis Project/Data/Metadata")

url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0090/0147704/1.1/data/1-data/NODC_addendum_May_2016.pdf"
download.file(url=url,destfile="metadataCariacoTS6.pdf",mode="wb")
url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0109/0164194/2.2/data/0-data/CariacoTimeSeries_Biogeochem_Bacteria_Full_Masterfile_METADATA_NCEI.rtf"
download.file(url=url,destfile="metadataCariacoTS5.rtf")
url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0090/0147704/1.1/data/0-data/NODC%20addendum_May%202016.rtf"
download.file(url=url,destfile="metadataCariacoTS4.rtf")
url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0028/0066503/2.2/data/0-data/NODC%20addendum_April%202012.rtf"
download.file(url=url,destfile="metadataCariacoTS3.rtf")
url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0090/0119359/1.1/data/0-data/NODC%20addendum_April%202014.pdf"
download.file(url=url,destfile="metadataCariacoTS1.pdf",mode="wb")
url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0061/0112926/1.1/data/0-data/CARIACO_Meta.html"
download.file(url=url,destfile="metadataCariacoTS0.html")

url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0046/0092235/1.1/data/0-data/NODC_metadata_Jun2011.pdf"
download.file(url=url,destfile="metadataOceanographicC3.pdf",mode="wb")
url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0083/0121284/1.1/about/0121284_EDDF.pdf"
download.file(url=url,destfile="metadataOceanographicC2.pdf",mode="wb")
url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0082/0139312/1.1/data/0-data/58H31G-ISO-19115-2.xml"
download.file(url=url,destfile="metadataOceanographicC1.xml")
url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0056/0107209/1.1/data/0-data/NODC_metadata_May2012.pdf"
download.file(url=url,destfile="metadataOceanographicC0.pdf",mode="wb")

setwd("~/Thesis Project/Data/Pre-processed Data")


#Procesando tabla cariacoOc-----------------------------------------------------
#Eliminamos columna de rownames y damos formato a cariacoOc.
cariacoOc<-cariacoOc[,-1]
names(cariacoOc)<-paste0(names(cariacoOc),".",cariacoOc[1,])
cariacoOc<-cariacoOc[-1,]

#Renombramos variables
names(cariacoOc)<-sapply(names(cariacoOc),function(x){gsub(" ","",x)})
names(cariacoOc)[9]<-"time.local.hhmm"

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

#Eliminamos filas repetidas. 1688 filas originalmente.
cariacoOc<- cariacoOc %>% unique()    #No hay filas repetidas.

#Eliminamos filas donde tenemos claves repetidas. Se observo que en estos
#casos los valores de las variables eran identicos salvo el numero de la 
#"botella". El numero maximo de veces que una clave se repitio fue dos.
#209 casos
cariacoOc<-cariacoOc[match(unique(cariacoOc$clave),cariacoOc$clave),]

#Eliminamos variables poco utiles.
cariacoOc<-cariacoOc[,-c(1,2,3,4,5)]


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

#Eliminamos las filas donde la variable "depth.m" tiene valores perdidos.
#En total son 162 filas de 9719.
cariacoTS<-cariacoTS[!is.na(cariacoTS$depth.m),]

#Creando clave para Merge.
cariacoTS$clave<-paste(as.character(cariacoTS$date),cariacoTS$latitude,
                       cariacoTS$longitude,cariacoTS$depth.m,sep=" ")

#Eliminamos filas repetidas. 9557 filas originalmente.
cariacoTS<- cariacoTS %>% unique()


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
