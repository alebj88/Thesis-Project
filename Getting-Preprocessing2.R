#Segundo procesamiento de la data.
#
library(tidyr)
library(caret)

setwd("~/Thesis Project/Data/Pre-processed Data")
cat("Importando tablas de la carpeta Pre-processed Data\n")
cat("\n")

#Importando todos los archivos CSV de la carpeta Pre-processed Data.

cariacoOc<-read.csv("OceanographicCariaco(Ci_i_NODC).csv",na=c("-9999.000000",
    "-9999.00000","-9999.0000","-9999.000","-9999.00","-9999.0","-9999",NA),
                   stringsAsFactors=FALSE)
cariacoTS <-read.csv("CariacoTimeSeries.csv",na=c("n.d.",NA,"-9999.000000",
    "-9999.00000","-9999.0000","-9999.000","-9999.00","-9999.0","-9999"),
                     stringsAsFactors=FALSE)
corrientes <-read.csv("CorrientesMarinas.csv")
world <-read.csv("WorldOceanicAtlas2013.csv")
boyasNBDC <-read.csv("BoyasNBDC.csv")
boyasArgo <-read.csv("BoyasArgo.csv")

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
cat("Uniendo tablas de Cariaco...\n")
cat("\n")

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

#Eliminamos fila con decha perdida.
cariacoTS<-cariacoTS[!is.na(cariacoTS$date),]

#Uniendo tablas de cariaco y procesando el dataset resultante-------------------
#Fuente:http://cdiac.ess-dive.lbl.gov/ftp/oceans/NDP_094/
#Fuente:http:http://cdiac.ess-dive.lbl.gov/ftp/oceans/2nd_QC_Tool_V2/

#Uniendo tablas de cariaco.
cariaco<-merge(cariacoTS,cariacoOc,by="clave",all=TRUE)
rm(cariacoOc);rm(cariacoTS)
gc()

#Eliminamos las variables "depth.m.y","latitude.y","longitude.y","date.y".
cariaco$date.x[is.na(cariaco$date.x)] <-cariaco$date.y[is.na(cariaco$date.x)]
cariaco$latitude.x[is.na(cariaco$latitude.x)] <-cariaco$latitude.y[
                                                is.na(cariaco$latitude.x)]
cariaco$longitude.x[is.na(cariaco$longitude.x)]<-cariaco$longitude.y[
                                                is.na(cariaco$longitude.x)]
cariaco$depth.m.x[is.na(cariaco$depth.m.x)]<-cariaco$depth.m.y[
                                                is.na(cariaco$depth.m.x)]
cariaco<-cariaco[,-c(104,84,83,82)]

#Renombramos variables en funcion de las que son equivalentes.

#Reordenamos un data frame auxiliar en funcion de las que son mas parecidas por
#clusterizacion. Usamos estos datos como guia.
options(scipen=20)
auxdf<-cariaco[,-(1:5)]
#auxdf<-cariaco[,-c(1:5,91)]
#auxdf<-cariaco[,-c(1:9,17,20,24,26,83:89)]
auxdf<-as.data.frame(apply(auxdf,2,function(x){
    round(as.numeric(unname(summary(as.numeric(as.character(x))))),4)
}))
auxdf<-auxdf[-7,]
nombres<-names(summary(as.numeric(cariaco$`nitrat.UMOL/KG`)))[1:6]
auxdf<-as.data.frame(t(auxdf))
names(auxdf)<-nombres

d<-dist(auxdf)
cluster<-hclust(d)
auxdf<-auxdf[cluster$order,]
#View(auxdf)

options(scipen=NULL)
rm(auxdf);rm(d);rm(cluster);rm(nombres)
gc()
	
#Tabla de conversiones 
#Fuente: http://www.endmemo.com/sconvert/mmol_lumol_l.php
#1 milliliter/liter (ml/l)  =  1000000 microgram/kilogram (ug/kg)
#1 ml/l = 44.661 uMol/l
#uM = Micromolar (uMOl/L)
#uMOl = Micromole
#mL = Mililiter
#ug = Microgram

names(cariaco)[5]<-"depth.m"
names(cariaco)[3]<-"latitude"
names(cariaco)[4]<-"longitude"
names(cariaco)[2]<-"date"

#Variables de temperatura equivalentes
names(cariaco)[6]<-"temperature1.Celsius"   #Originalmente "ctdtmp.DEG_C"
names(cariaco)[83]<-"temperature2.Celsius"  #Originalmente  "temperature.CTD..degrees_Celsius"

#Variables de salinidad equivalentes
names(cariaco)[7]<-"salinity1.psu"         #Originalmente "ctdsal." 
names(cariaco)[8]<-"salinity2.psu"         #Originalmente "salnty." 
names(cariaco)[84]<-"salinity3.psu"        #Originalmente "salinity.CTD..psu"
names(cariaco)[86]<-"salinity4.psu"        #"descrete_salinity.psu"

#Variables de oxigeno disuelto equivalentes
names(cariaco)[9]<-"dissolved.oxygen1.UMOLperKG"    #Originalmente "oxygen.UMOL/KG" 
names(cariaco)[88]<-"dissolved.oxygen2.UMOLperKG"   #Originalmente "dissolved_oxygen.1.umol/kg_of_seawater" 
names(cariaco)[87]<-"dissolved.oxygen3.mLperLiter"  #Originalmente "dissolved_oxygen.ml/l"   

#Variables de PON equivalentes (Nitrogeno organico Particulado)
names(cariaco)[20]<-"particulate.organic.nitrogen1.UGperKG" #Originalmente "pon.UG/KG" 
names(cariaco)[26]<-"particulate.organic.nitrogen2.UGperKG" #Originalmente "pn.UG/KG" 

#Variables de pH on the total hydrogen ion scale equivalentes.
names(cariaco)[89]<-"pHTotal.Hydrogen.ion.Scale1"   #Originalmente "pHT.at25oC..total_hydrogen_ion_scale"
names(cariaco)[17]<-"pHTotal.Hydrogen.ion.Scale2"   #Originalmente "ph_tot."  

#'Sigma-t' is density minus 1000 (without the effect of pressure)
names(cariaco)[85]<-"density.sigmat1.KGperM3"      #Originalmente "density.CTD..sigma-t" 
names(cariaco)[24]<-"density.sigmat2.KGperM3"      #Originalmente "sigma_theta.KG/M3"

#El data frame cariaco NO TIENE FACTORES.

#Convirtiendo unidades. mLperLiter a UMOLperKG.
#Masa = Volumen*densidad   (densidad =1.0293KG/L)
cariaco[,87]<-as.numeric(cariaco[,87])
cariaco$dissolved.oxygen3.UMOLperKG<-cariaco[,87]*(44.661/1.0293)

#Combinando variables equivalentes de Cariaco.
cariaco[,6] <-as.numeric(cariaco[,6])
cariaco[,83] <-as.numeric(cariaco[,83])
cariaco$temperature.Celsius <- apply(cariaco[,c(6,83)],1,mean,na.rm=TRUE)
cariaco$temperature.Celsius[is.nan(cariaco$temperature.Celsius)]<-NA
    
cariaco[,7] <-as.numeric(cariaco[,7])
cariaco[,8] <-as.numeric(cariaco[,8])
cariaco[,84] <-as.numeric(cariaco[,84])
cariaco[,86] <-as.numeric(cariaco[,86])
cariaco$salinity.psu <-apply(cariaco[,c(7,8,84,86)],1,mean,na.rm=TRUE)
cariaco$salinity.psu[is.nan(cariaco$salinity.psu)] <-NA

cariaco[,9] <-as.numeric(cariaco[,9])
cariaco[,88] <-as.numeric(cariaco[,88])
#which(names(cariaco)=="dissolved.oxygen3.UMOLperKG") = 101
cariaco$dissolved.oxygen.UMOLperKG <-apply(cariaco[,c(9,88,101)],1,mean,na.rm=T)
cariaco$dissolved.oxygen.UMOLperKG[is.nan(
                            cariaco$dissolved.oxygen.UMOLperKG)] <-NA
    
cariaco[,20] <-as.numeric(cariaco[,20])
cariaco[,26] <-as.numeric(cariaco[,26])
cariaco$particulate.organic.nitrogen.UGperKG <- apply(
                            cariaco[,c(20,26)],1,mean,na.rm=TRUE)
cariaco$particulate.organic.nitrogen.UGperKG[is.nan(
                            cariaco$particulate.organic.nitrogen.UGperKG)] <-NA
    
cariaco[,89] <-as.numeric(cariaco[,89])
cariaco[,17] <-as.numeric(cariaco[,17])
cariaco$pHTotal.Hydrogen.ion.Scale <-apply(cariaco[,c(89,17)],1,mean,na.rm=TRUE)
cariaco$pHTotal.Hydrogen.ion.Scale[is.nan(
                            cariaco$pHTotal.Hydrogen.ion.Scale)] <-NA

cariaco[,85] <-as.numeric(cariaco[,85])
cariaco[,24] <-as.numeric(cariaco[,24])
cariaco$density.sigmat.KGperM3 <-apply(cariaco[,c(85,24)],1,mean,na.rm=TRUE)
cariaco$density.sigmat.KGperM3[is.nan(cariaco$density.sigmat.KGperM3)] <-NA

#Eliminando variables que no aportan informacion util
#nearZeroVar(cariaco,saveMetrics=TRUE)	
cariaco$ph_tmp.DEG_C<-NULL
cariaco$temperature1.Celsius <-NULL
cariaco$temperature2.Celsius <-NULL
cariaco$salinity1.psu <-NULL
cariaco$salinity2.psu <-NULL
cariaco$salinity3.psu <-NULL
cariaco$salinity4.psu <-NULL
cariaco$dissolved.oxygen1.UMOLperKG <-NULL
cariaco$dissolved.oxygen2.UMOLperKG <-NULL
cariaco$dissolved.oxygen3.mLperLiter <-NULL
cariaco$dissolved.oxygen3.UMOLperKG <-NULL
cariaco$particulate.organic.nitrogen1.UGperKG <-NULL
cariaco$particulate.organic.nitrogen2.UGperKG <-NULL
cariaco$pHTotal.Hydrogen.ion.Scale1 <-NULL
cariaco$pHTotal.Hydrogen.ion.Scale2 <-NULL
cariaco$density.sigmat1.KGperM3 <-NULL
cariaco$density.sigmat2.KGperM3 <-NULL

#Renombrando variables
names(cariaco)<-make.names(names(cariaco))

names(cariaco)<-sapply(names(cariaco),function(x){
    gsub("/","per",x)
})
names(cariaco)<-sapply(names(cariaco),function(x){
    gsub("micromoles","UMOL",x)
})
names(cariaco)<-sapply(names(cariaco),function(x){
    gsub(".liter","Liter",x)
})
names(cariaco)<-sapply(names(cariaco),function(x){
    gsub("nMOL.KG","nMOLperKG",x)
})
names(cariaco)<-sapply(names(cariaco),function(x){
    gsub("UMOL.KG","UMOLperKG",x)
})
names(cariaco)<-sapply(names(cariaco),function(x){
    gsub("mgC.m.3.hr","mgCperM3perHour",x)
})
names(cariaco)<-sapply(names(cariaco),function(x){
    gsub("mg.m.3","mgperM3",x)
})
names(cariaco)<-sapply(names(cariaco),function(x){
    gsub("nmol.L","nMOLperLiter",x)
})
names(cariaco)<-sapply(names(cariaco),function(x){
    gsub("cells.perLiter","cellsperLiter",x)
})
names(cariaco)<-sapply(names(cariaco),function(x){
    gsub("micrograms.C.per.Liter.per.day","mgCperLiterperDay",x)
})
names(cariaco)<-sapply(names(cariaco),function(x){
    gsub("UMOL.perLiter","UMOLperLiter",x)
})
names(cariaco)[38]<-"total.bacteria.cells.sd" 
names(cariaco)[74]<-"total_alkalinity.molperKG"


#Procesando tabla de corrientes-------------------------------------------------
#Fuente temporal:https://www.google.co.ve/search?q=u.cm.s+v.cm.s+direction.deg.t+marine+currents+NOAA&nirf=u.cm.s+to.cm.s+direction.deg.t+marine+currents+NOAA&sa=X&ved=0ahUKEwj4m7qR_-zYAhUK71MKHUCaB_0Q8BYIJCgB&biw=1270&bih=649
#El rango de fechas esta fuera de rango. 1900-1985.
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
dataF<-cariaco[c("latitude","longitude")]
dataF$latitude<-as.numeric(as.character(dataF$latitude))
dataF$longitude<-as.numeric(as.character(dataF$longitude))
corrientes$latitude<-as.numeric(as.character(corrientes$latitude))
corrientes$longitude<-as.numeric(as.character(corrientes$longitude))
dataF2<-corrientes[c("latitude","longitude")]

cat("Agregando informacion de corrientes a Cariaco...\n")
cat("\n")

aux<-apply(dataF,1,function(x){aproxGeoloc(dataF2,x[1],x[2])})

cariaco$latAprox <-sapply(aux,function(x){x$latAprox})
cariaco$lonAprox <-sapply(aux,function(x){x$lonAprox})
cariaco$ErrorLatLon.Corrientes <-sapply(aux,function(x){x$Error}) 
                        #Usamos distancia euclidea. Por simplicidad.
rm(aux);rm(dataF);rm(dataF2)
gc()

#summary(cariaco$Error)  #Verificamos la distribucion de los errores.

#Creamos claves para el merge de Cariaco con Corrientes.
cariaco$clave2 <-paste(cariaco$latAprox,cariaco$lonAprox,sep=" ")
cariaco$latAprox <-NULL
cariaco$lonAprox <-NULL

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

corrientesAux$clave2<-row.names(corrientesAux)

#Agregamos data de corrientes a cariaco.

cariaco<-merge(cariaco,corrientesAux,by="clave2",all.x=TRUE)

#Procesando tabla world---------------------------------------------------------
#PROMEDIO DE TODAS LAS DECADAS.
cat("Procesando tabla World...\n")
cat("\n")

names(world)[4]<-"X1"
names(world)[-c(1,2,3)]<-unname(sapply(names(world)[-c(1,2,3)],
                                       function(x){gsub("X","",x)}))
#Ordenamos data frame. Tiene valores como variables.
world<-gather(world,key=depth.m,value=value,-c(1,2,3))

#Reescribiendo variable "medida".
world$medida<-as.character(world$medida)
world$medida<-unname(sapply(world$medida,function(x){gsub("-"," ",x)}))
world$medida<-unname(sapply(world$medida,function(x){gsub("eA"," ",x)}))

splits<-strsplit(world$medida,split=' ')
world$medida<-sapply(splits,function(x){paste(x[-c(1,2,3,4)],collapse=" ")})
aux<-sapply(splits,function(x){paste(x[c(1,2,3,4)],collapse=" ")})
aux[aux=="#WOA13 1/4 degree ANNUAL"]<-"1/4ºAnnual"
aux[aux!="#WOA13 1/4 degree ANNUAL"]<-"1ºAnnual"

world$medida<-paste(world$medida,aux,sep=" ")

rm(aux);rm(splits)
gc()

#Eliminamos filas repetidas 168069 filas originalmente. 160215 restantes.
world<- world %>% unique()

#Eliminamos filas con valores perdidos en la variable "value"
world<-world[!is.na(world$value),]

#plot de medidas
lat<-as.numeric(world$latitude)
lon<-as.numeric(world$longitude)
color<-as.factor(world$medida)
df1<-data.frame(lat=lat,lon=lon,color=color)
df1<-df1[sample(1:149472,1000),]

png(file="WOAcolorVar2013.png")
map<-get_map(location= c(lon=-66.715,lat=13.115),zoom=5,maptype="terrain") 
plotMapa<-ggmap(map) +
    geom_point(data=df1,aes(x=lon,y=lat,col=color),show.legend=F) +
    labs(title="WOA2013 Coloreando por variable.(muestra de 1000)")	
plot(plotMapa)
dev.off()

rm(lat);rm(lon);rm(df1);rm(color);rm(map);rm(plotMapa)
gc()

#Ordenamos data frame. Data Frame con variables almacenadas en filas y columnas.
world$medida<-make.names(world$medida)
world<-spread(world,key=medida,value=value)

#Preparamos datos para el merge con corrientes.
dataF<-world[c("latitude","longitude")]
dataF$latitude<-as.numeric(as.character(dataF$latitude))
dataF$longitude<-as.numeric(as.character(dataF$longitude))
dataF2<-corrientes[c("latitude","longitude")]

cat("Agregando informacion de corrientes a World...\n")
cat("\n")
   
aux<-apply(dataF,1,function(x){aproxGeoloc(dataF2,x[1],x[2])})

world$latAprox <-sapply(aux,function(x){x$latAprox})
world$lonAprox <-sapply(aux,function(x){x$lonAprox})
world$ErrorLatLon.Corrientes <-sapply(aux,function(x){x$Error}) 
                                    #Usamos distancia euclidea. Por simplicidad.                     
rm(aux);rm(dataF);rm(dataF2);rm(corrientes)
gc()

#summary(world$Error)   #Verificamos la distribucion de los errores.

#Creamos claves para el merge de world con Corrientes.
world$clave2 <-paste(world$latAprox,world$lonAprox,sep=" ")
world$latAprox <-NULL
world$lonAprox <-NULL

#Agregamos data de corrientes a World.

world<-merge(world,corrientesAux,by="clave2",all.x=TRUE)







#Creamos clave para los merges.-------------------------
cat("Procesando tabla boyasArgo...\n")
cat("\n")

#Hacemos coincidir nombres de variables.
names(boyasArgo)[1]<-"date"
names(boyasArgo)[2]<-"latitude"
names(boyasArgo)[3]<-"longitude"

cat("Escribiendo rablas procesadas en Pre-processed Data 2.\n")
cat("\n")

setwd("~/Thesis Project/Data/Pre-processed Data 2")
write.table(file="HereWeAre.txt",data.frame(prue=c(1,2,3),ba=c(4,5,6)))
write.csv(cariaco,file="cariaco.csv",row.names = FALSE)
write.csv(world,file="WOA.csv",row.names = FALSE)


cat("Procedimiento Getting-Preprocessing2.R finalizado.\n")
cat("\n")
