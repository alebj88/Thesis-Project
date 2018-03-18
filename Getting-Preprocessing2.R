#Segundo procesamiento de la data.
#
library(dplyr)
library(tidyr)
library(caret)
library(ggmap)

setwd("~/Thesis Project AB/R-Scripts")
source('Functions.R')

setwd("~/Thesis Project AB/Data/Pre-processed Data")
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
setwd("~/Thesis Project AB/Data/Metadata")

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

url<-"ftp://ftp.aoml.noaa.gov/pub/od/library/oxygenation-yarincik.pdf"
download.file(url=url,destfile="metadataOceanographicOxygenation.pdf",mode="wb")
#url<-"http://onlinelibrary.wiley.com/doi/10.1002/jgrg.20075/pdf"
#download.file(url=url,destfile="metadataOceanographicDissolvedOM.pdf",mode="wb")
url<-"http://www.ndbc.noaa.gov/docs/ndbc_web_data_guide.pdf"
download.file(url=url,destfile="NBDCwebDataGuide.pdf",mode="wb")

url<-"http://data.nodc.noaa.gov/woa/WOA13/DOC/woa13_vol1.pdf"
download.file(url=url,destfile="metadataOceanographicTemperature.pdf",mode="wb")
url<-"http://data.nodc.noaa.gov/woa/WOA13/DOC/woa13_vol2.pdf"
download.file(url=url,destfile="metadataOceanographicSalinity.pdf",mode="wb")
url<-"http://data.nodc.noaa.gov/woa/WOA13/DOC/woa13_vol3.pdf"
download.file(url=url,destfile="metadataOceanographicOxigen.pdf",mode="wb")
url<-"http://data.nodc.noaa.gov/woa/WOA13/DOC/woa13_vol4.pdf"
download.file(url=url,destfile="metadataOceanographicNutrients.pdf",mode="wb")

url<-"http://data.nodc.noaa.gov/woa/WOA13/DOC/woa13documentation.pdf"
download.file(url=url,destfile="metadataOceanographicC4.pdf",mode="wb")
url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0046/0092235/1.1/data/0-data/NODC_metadata_Jun2011.pdf"
download.file(url=url,destfile="metadataOceanographicC3.pdf",mode="wb")
url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0083/0121284/1.1/about/0121284_EDDF.pdf"
download.file(url=url,destfile="metadataOceanographicC2.pdf",mode="wb")
url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0082/0139312/1.1/data/0-data/58H31G-ISO-19115-2.xml"
download.file(url=url,destfile="metadataOceanographicC1.xml")
url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0056/0107209/1.1/data/0-data/NODC_metadata_May2012.pdf"
download.file(url=url,destfile="metadataOceanographicC0.pdf",mode="wb")

rm(url)
gc()

setwd("~/Thesis Project AB/Data/Pre-processed Data")

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
cariaco$depth.m.x[is.na(cariaco$depth.m.x)] <-cariaco$depth.m.y[
                                                is.na(cariaco$depth.m.x)]
cariaco<-cariaco[,-c(104,84,83,82)]

#Renombramos variables en funcion de las que son equivalentes.
	
#Tabla de conversiones 
#Fuente: http://www.endmemo.com/sconvert/mmol_lumol_l.php
#1 milliliter/liter (ml/l)  =  1000000 microgram/kilogram (ug/kg)
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
#Masa = Volumen*densidad   (densidad =1.027KG/L) (UMOL Oxygen = 44.661)
cariaco[,87]<-as.numeric(cariaco[,87])            
cariaco$dissolved.oxygen3.UMOLperKG<-cariaco[,87]*(44.661/1.027)

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
#Fuente: https://www.ncdc.noaa.gov/paleo-search/reports/all?dataTypeId=7&search=true
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

names(cariaco) <-unname(sapply(names(cariaco),function(x){
    gsub("nitrat","nitrate",x)
}))
names(cariaco) <-unname(sapply(names(cariaco),function(x){
    gsub("nitrit","nitrite",x)
}))
names(cariaco) <-unname(sapply(names(cariaco),function(x){
    gsub("phspht","phosphate",x)
}))
names(cariaco) <-unname(sapply(names(cariaco),function(x){
    gsub("silcat","silicate",x)
}))
names(cariaco) <-unname(sapply(names(cariaco),function(x){
    gsub("nh4","ammonium",x)
}))
names(cariaco) <-unname(sapply(names(cariaco),function(x){
    gsub("h2s","hydrogen.sulfide",x)
}))
names(cariaco) <-unname(sapply(names(cariaco),function(x){
    gsub("ch4","methane",x)
}))
names(cariaco) <-unname(sapply(names(cariaco),function(x){
    gsub("no2","nitrite2",x)     #Esto es NO2-
}))
names(cariaco) <-unname(sapply(names(cariaco),function(x){
    gsub("no3","nitrate2",x)     
}))
names(cariaco) <-unname(sapply(names(cariaco),function(x){
    gsub("fco2","CO2.fugacity",x)
}))
# names(cariaco) <-unname(sapply(names(cariaco),function(x){
#     gsub("tcarbn","total.CO2.concentrations",x)
# }))
names(cariaco) <-unname(sapply(names(cariaco),function(x){
    gsub("doc","dissolved.organic.carbon",x)
}))
names(cariaco) <-unname(sapply(names(cariaco),function(x){
    gsub("toc","total.organic.carbon",x)
}))
names(cariaco) <-unname(sapply(names(cariaco),function(x){
    gsub("poc","particulate.organic.carbon",x)
}))
names(cariaco) <-unname(sapply(names(cariaco),function(x){
    gsub("tpp","total.particulate.phosphorus",x)
}))
names(cariaco) <-unname(sapply(names(cariaco),function(x){
    gsub("pip","particulate.inorganic.phosphorus",x)
}))
names(cariaco) <-unname(sapply(names(cariaco),function(x){
    gsub("don","dissolved.organic.nitrogen",x)
}))
names(cariaco) <-unname(sapply(names(cariaco),function(x){
    gsub("dop","dissolved.organic.phosphorus",x)
}))
names(cariaco) <-unname(sapply(names(cariaco),function(x){
    gsub("po4.3","phosphate2",x)
}))
names(cariaco) <-unname(sapply(names(cariaco),function(x){
    gsub(".mn.",".manganese.",x)
}))
names(cariaco) <-unname(sapply(names(cariaco),function(x){
    gsub("_",".",x)
}))
names(cariaco) <-unname(sapply(names(cariaco),function(x){
    gsub("\\.\\.","\\.",x)
}))

names(cariaco)[35]<-"cruise.corrected.depth"
names(cariaco)[36]<-"cruise.hydrogen.sulfide"
names(cariaco)[37]<-"total.bacteria.cells.x108.L.1"
names(cariaco)[39]<-"bacterial.production.mgCperLiterperDay"
names(cariaco)[40]<-"bacterial.production.mgc.sd"
names(cariaco)[80]<-"nitrite.uM"
names(cariaco)[81]<-"nitrate.uM"

names(cariaco)[58]<-"cyanobacteria.cellsperLiter.sd"
names(cariaco)[60]<-"methanogens.cellsperLite.sd"
names(cariaco)[62]<-"flagellated.protists.cellsperLiter.sd"
names(cariaco)[64]<-"ciliated.protists.cellsperLiter.sd"
names(cariaco)[66]<-"viral.like.particles.vlp.cellsperLiter.sd"
names(cariaco)[68]<-"dark.carbon.fixation.rate.mgCperLiterperDay.sd"
names(cariaco)[70]<-"dissolved.manganese.nMOLperLiter.sd"
names(cariaco)[72]<-"dissolved.fe.nMOLperLiter.sd"

names(cariaco)[39]<-"heterotrophic.bacterial.production.mgCperLiterperDay"
names(cariaco)[40]<-"heterotrophic.bacterial.production.mgCperLiterperDay.sd"
names(cariaco)[44]<-"total.flagellate.cells.sd"
names(cariaco)[56]<-"total.prokaryotes.cellsperLiter.sd"
names(cariaco)[33]<-"total.zero.valent.sulfur.UMOLperLiter"
names(cariaco)[34]<-"total.zero.valent.sulfur.UMOLperLiter.sd"

#Convertimos las unidades de las variables.
#https://www.calculatoratoz.com/es/volumen-molar-calculadora/node-353
#Masa = Volumen*densidad   (densidad =1.027KG/L)
#uM = (uMOl/L)
#uMol/Kg * 1.027Kg/L = 1.027 uMol/L

uMolperL<-as.numeric(cariaco$silica.uM)
cariaco$silicate2.UMOLperKG<-uMolperL/(1.027)             #uMolperL a UMOLperKG.
cariaco$silica.uM<-NULL

uMolperL<-as.numeric(cariaco$nitrate.uM)
cariaco$nitrate2.UMOLperKG<-uMolperL/(1.027)              #uMolperL a UMOLperKG.
cariaco$nitrate.uM<-NULL

uMolperL<-as.numeric(cariaco$nitrite.uM)
cariaco$nitrite2.UMOLperKG<-uMolperL/(1.027)              #uMolperL a UMOLperKG.
cariaco$nitrite.uM<-NULL

uMolperL<-as.numeric(cariaco$ammonia.uM)
cariaco$ammonia.UMOLperKG<-uMolperL/(1.027)               #uMolperL a UMOLperKG.
cariaco$ammonia.uM<-NULL

uMolperL<-as.numeric(cariaco$phosphate2.UMOLperLiter)  
cariaco$phosphate2.UMOLperKG<-uMolperL/(1.027)            #uMolperL a UMOLperKG.
cariaco$phosphate2.UMOLperLiter<-NULL

uMolperL<-as.numeric(cariaco$ammonium.UMOLperLiter)
cariaco$ammonium2.UMOLperKG<-uMolperL/(1.027)             #uMolperL a UMOLperKG.
cariaco$ammonium.UMOLperLiter<-NULL

uMolperL<-as.numeric(cariaco$nitrate2.UMOLperLiter)
cariaco$nitrate2.2.UMOLperKG<-uMolperL/(1.027)            #uMolperL a UMOLperKG.
cariaco$nitrate2.UMOLperLiter<-NULL

uMolperL<-as.numeric(cariaco$nitrite2.UMOLperLiter)
cariaco$nitrite2.2.UMOLperKG<-uMolperL/(1.027)            #uMolperL a UMOLperKG.
cariaco$nitrite2.UMOLperLiter<-NULL

rm(uMolperL)
gc()

#Reordenamos un data frame auxiliar en funcion de las que son mas parecidas por
#clusterizacion. Usamos estos datos como guia.
# options(scipen=20)
# auxdf<-cariaco[,-(1:5)]
# 
# auxdf<-as.data.frame(apply(auxdf,2,function(x){
#     round(as.numeric(unname(summary(as.numeric(as.character(x))))),4)
# }))
# auxdf<-auxdf[-7,]
# nombres<-names(summary(as.numeric(cariaco$nitrat.UMOLperKG)))[1:6]
# auxdf<-as.data.frame(t(auxdf))
# names(auxdf)<-nombres
# 
# d<-dist(auxdf)
# cluster<-hclust(d)
# auxdf<-auxdf[cluster$order,]
# #View(auxdf)
# 
# options(scipen=NULL)
# rm(auxdf);rm(d);rm(cluster);rm(nombres)
# gc()

#Combinando variables equivalentes de Cariaco.
cariaco$nitrate.UMOLperKG <-as.numeric(cariaco$nitrate.UMOLperKG)
cariaco$nitrate2.2.UMOLperKG <-as.numeric(cariaco$nitrate2.2.UMOLperKG)
cariaco$nitrate2.UMOLperKG <-as.numeric(cariaco$nitrate2.UMOLperKG)
cariaco$nitrate.UMOLperKG <- apply(cariaco[c("nitrate2.UMOLperKG",
                 "nitrate2.2.UMOLperKG","nitrate.UMOLperKG")],1,mean,na.rm=TRUE)
cariaco$nitrate.UMOLperKG[is.nan(cariaco$nitrate.UMOLperKG)]<-NA
cariaco$nitrate2.UMOLperKG <- NULL
cariaco$nitrate2.2.UMOLperKG <- NULL

cariaco$silicate.UMOLperKG <-as.numeric(cariaco$silicate.UMOLperKG)
cariaco$silicate2.UMOLperKG <-as.numeric(cariaco$silicate2.UMOLperKG)
cariaco$silicate.UMOLperKG  <- apply(cariaco[c("silicate.UMOLperKG",
                 "silicate2.UMOLperKG")],1,mean,na.rm=TRUE)
cariaco$silicate.UMOLperKG [is.nan(cariaco$silicate.UMOLperKG )]<-NA
cariaco$silicate2.UMOLperKG <-NULL

cariaco$phosphate.UMOLperKG <-as.numeric(cariaco$phosphate.UMOLperKG)
cariaco$phosphate2.UMOLperKG <-as.numeric(cariaco$phosphate2.UMOLperKG)
cariaco$phosphate.UMOLperKG  <- apply(cariaco[c("phosphate.UMOLperKG",
                 "phosphate2.UMOLperKG")],1,mean,na.rm=TRUE)
cariaco$phosphate.UMOLperKG [is.nan(cariaco$phosphate.UMOLperKG )]<-NA
cariaco$phosphate2.UMOLperKG <-NULL

cariaco$nitrite.UMOLperKG <-as.numeric(cariaco$nitrite.UMOLperKG)
cariaco$nitrite2.2.UMOLperKG <-as.numeric(cariaco$nitrite2.2.UMOLperKG)
cariaco$nitrite2.UMOLperKG <-as.numeric(cariaco$nitrite2.UMOLperKG)
cariaco$nitrite.UMOLperKG <- apply(cariaco[c("nitrite2.UMOLperKG",
                "nitrite2.2.UMOLperKG","nitrite.UMOLperKG")],1,mean,na.rm=TRUE)
cariaco$nitrite.UMOLperKG[is.nan(cariaco$nitrite.UMOLperKG)]<-NA
cariaco$nitrite2.UMOLperKG <- NULL
cariaco$nitrite2.2.UMOLperKG <- NULL

cariaco$ammonium.UMOLperKG <-as.numeric(cariaco$ammonium.UMOLperKG)
cariaco$ammonium2.UMOLperKG <-as.numeric(cariaco$ammonium2.UMOLperKG)
cariaco$ammonium.UMOLperKG  <- apply(cariaco[c("ammonium.UMOLperKG",
                "ammonium2.UMOLperKG")],1,mean,na.rm=TRUE)
cariaco$ammonium.UMOLperKG [is.nan(cariaco$ammonium.UMOLperKG )]<-NA
cariaco$ammonium2.UMOLperKG <-NULL


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

#summary(cariaco$Error)      #Verificamos la distribucion de los errores.

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

cariaco <- merge(cariaco,corrientesAux,by="clave2",all.x=TRUE)
cariaco$clave2 <- NULL

#Eliminamos repetidas.
cariaco <- cariaco %>% unique()

#Posiciones donde hay puntos con varias mediciones simultaneas (maximo 2).
#posRep <- unname(unclass(which((table(cariaco$clave) == 2))))
claveUniq <- unique(cariaco$clave)
posRep <- sapply(claveUniq,function(x){
    p <- which(cariaco$clave == x)[1]
    return(p)
})

#Eliminamos los datos extra.
cariaco <- cariaco[posRep,]

#Eliminamos variables auxiliares.
cariaco$clave <- NULL

#Procesando tabla world---------------------------------------------------------
#PROMEDIO DE TODAS LAS DECADAS.
cat("Procesando tabla World...\n")
cat("\n")

#Renombramos variables.
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
aux[aux=="#WOA13 1/4 degree ANNUAL"]<-"1/4ÂºAnnual"
aux[aux!="#WOA13 1/4 degree ANNUAL"]<-"1ÂºAnnual"

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
df1<-df1[sample(1:nrow(df1),1000),]

png(file="WOAcolorVar2013.png")
map<-get_map(location= c(lon=-66.715,lat=13.115),zoom=6,maptype="terrain") 
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

#Renombramos variables.
names(world) <-unname(sapply(names(world),function(x){
     gsub("1ÂºAnnual","",x)
}))
names(world)[11]<-"oxygen"
names(world)[21]<-"phosphate"
names(world)[27]<-"salinity"
names(world)[38]<-"temperature"
names(world)[5]<-"nitrate"
names(world)[32]<-"silicate"

#Creamos clave para los merges.-------------------------
cat("Procesando tablas de boyas...\n")
cat("\n")

#Hacemos coincidir nombres de variables.
names(boyasArgo)[1]<-"date"
names(boyasArgo)[2]<-"latitude"
names(boyasArgo)[3]<-"longitude"
names(boyasArgo)[4]<-"pression.decibar"
names(boyasArgo)[5]<-"temperture.celsius"
names(boyasArgo)[6]<-"salinity.psu"
names(boyasArgo)[7]<-"pression.adjusted.decibar"
names(boyasArgo)[8]<-"temperture.adjusted.celsius"
names(boyasArgo)[9]<-"salinity.adjusted.psu"

#Hacemos coincidir nombres de variables. BOYAS NBDC
names(boyasNBDC)[1]<-"spectral.wave.density.metrosCuadradoSobreHz"
names(boyasNBDC)[2]<-"mean.wave.direction.degrees"
names(boyasNBDC)[3]<-"principal.wave.direction.degrees"
names(boyasNBDC)[4]<-"wave.spectrum.FirstNormalizedPolarCoordinateDerivedfromtheFourierCoefficients"
names(boyasNBDC)[5]<-"wave.spectrum.SecondNormalizedPolarCoordinateDerivedfromtheFourierCoefficients"
names(boyasNBDC)[6]<-"event.mode"
names(boyasNBDC)[7]<-"water.column.height.meters"
names(boyasNBDC)[8]<-"date"
names(boyasNBDC)[9]<-"latitude"
names(boyasNBDC)[10]<-"longitude"
names(boyasNBDC)[11]<-"gust.direction.degrees"
names(boyasNBDC)[12]<-"gust.speed.metersPersecond"
names(boyasNBDC)[13]<-"gust.time.seconds.since.19700101"
names(boyasNBDC)[14]<-"wind.gust.speed.metersPersecond"
names(boyasNBDC)[15]<-"significant.wave.height.meters"
names(boyasNBDC)[16]<-"dominant.wave.period.seconds"
names(boyasNBDC)[17]<-"average.wave.period.seconds"
names(boyasNBDC)[18]<-"air.sea.level.pressure.hPa"
names(boyasNBDC)[19]<-"air.temperature.celsius"
names(boyasNBDC)[20]<-"sea.surface.temperature.celsius"
names(boyasNBDC)[21]<-"dew.point.temperature.celsius"
names(boyasNBDC)[22]<-"wind.direction.degrees"
names(boyasNBDC)[23]<-"wind.speed.metersPersecond"

cat("Escribiendo tablas procesadas en Pre-processed Data 2.\n")
cat("\n")

setwd("~/Thesis Project AB/Data/Pre-processed Data 2")
write.csv(cariaco,file="cariaco.csv",row.names = FALSE)
write.csv(world,file="WOA.csv",row.names = FALSE)
write.csv(corrientesAux,file="corrientesAux.csv",row.names = FALSE)
write.csv(boyasArgo,file="boyasArgo.csv",row.names = FALSE)
write.csv(boyasNBDC,file="boyasNBDC.csv",row.names = FALSE)

rm(list=ls())
gc()

#Cargamos datos de corrientes y les damos formato.\\\\\\\\\\\\\\\\\\\\\\\\\
setwd("~/Thesis Project AB/Data/Pre-processed Data 2")
corrientesAux <- read.csv("corrientesAux.csv")
corrientesAux <- separate(corrientesAux,
                          col=clave2,into=c("latitude","longitude"),sep=" ")

corrientesAux$latitude  <- as.numeric(corrientesAux$latitude)
corrientesAux$longitude <- as.numeric(corrientesAux$longitude)

write.csv(corrientesAux,file="corrientesAux.csv",row.names=FALSE)

#Creamos tabla linea costera.--------------------------------------------
cat("Generando tabla de linea costera...\n")
cat("\n")

setwd("~/Thesis Project AB/R-Scripts")
source('Functions.R')

setwd("~/Thesis Project AB/Data/Raw Data")
LineaCostera <- read.csv("LineaCosteraRaw.csv")

png(file="LineaCosteraRaw.png")
map <- get_map(location= c(lon=-66.715,lat=13.115),zoom=6,maptype="terrain") 
plotMapa <-ggmap(map) +
    geom_point(data=LineaCostera,aes(x=longitude,y=latitude),color="red",
               size = 0,show.legend=F) +
    labs(title="Linea Costera Raw")	
plot(plotMapa)
dev.off()

LineaCostera$posicion <- rep(NA,times=nrow(LineaCostera))
ini <- LineaCostera[LineaCostera$latitude >= 12.35 & 
                                         LineaCostera$longitude <= -71.28,]
LineaCostera$posicion[as.numeric(rownames(ini))] <- 1
LineaCostera <- LineaCostera[order(LineaCostera$posicion),]

for(i in 2:(nrow(LineaCostera)-1)){
    num <- unname(apply(LineaCostera[i:nrow(LineaCostera),-3],1,function(x){
            distGL(as.numeric(LineaCostera[(i-1),][-3]),c(x[1],x[2]))
        }
    ))
    LineaCostera$posicion[which.min(num)+(i-1)] <- i
    LineaCostera <- LineaCostera[order(LineaCostera$posicion),]
}
LineaCostera$posicion[nrow(LineaCostera)] <- nrow(LineaCostera)  #Pos del ultimo

setwd("~/Thesis Project AB/Data/Pre-processed Data")
write.csv(LineaCostera,file="LineaCostera.csv",row.names=FALSE)

#Evaluacion
#plot(LineaCostera$longitude,LineaCostera$latitude,type = "l")

#Utilizaremos los datos de corrientes obtenidos del mapa de la LUZ.
#Este mapa contiene informacion sobre las corrientes marinas en la costa.

#??????????????/////////////////////////////////////////////////////
#Construyendo el archivo corrientesCostaRaw
#Esto se borrara
# corrientesCosta <- read.csv("~/corrientesCostaRaw.csv")
# corrientesCosta <-corrientesCosta[!is.na(corrientesCosta$medida),]
# 
# corrientesCosta$latitude <- round(corrientesCosta$latitude,2)
# corrientesCosta$longitude <- round(corrientesCosta$longitude,2)
# 
# #recortamos
# corrientesCosta <- corrientesCosta[corrientesCosta$latitude < 13,]
# row.names(corrientesCosta) <- 1:nrow(corrientesCosta)
# 
# names(corrientesCosta)[3]<-"speed.m.s.mean"
# 
# corrientesCosta$posicion <- rep(NA,times=nrow(corrientesCosta))
# ini <- corrientesCosta[corrientesCosta$latitude >= 12.46 &
#                            corrientesCosta$longitude <= -71.61,]
# corrientesCosta$posicion[as.numeric(rownames(ini))] <- 1
# corrientesCosta <- corrientesCosta[order(corrientesCosta$posicion),]
# 
# for(i in 2:(nrow(corrientesCosta)-1)){
#     num <- unname(apply(corrientesCosta[i:nrow(corrientesCosta),-3],1,function(x){
#         distGL(as.numeric(corrientesCosta[(i-1),][-3]),c(x[1],x[2]))
#     }
#     ))
#     corrientesCosta$posicion[which.min(num)+(i-1)] <- i
#     corrientesCosta <- corrientesCosta[order(corrientesCosta$posicion),]
# }
# corrientesCosta$posicion[nrow(corrientesCosta)] <- nrow(corrientesCosta)  #Pos del ultimo
# row.names(corrientesCosta) <- corrientesCosta$posicion

#Debemos agregar la direccion del agua a la tabla "corrientesCostaRaw.csv" que 
#esta actualmente en Documentos pero va a ser colocada en Raw Data.
#Van a pares
#Hay que ordenar corrientes costa usando Linea costera
# corrientesCosta <- read.csv("~/corrientesCostaRaw.csv")
# LineaCostera$posicion <- LineaCostera$posicion*100
# LineaCostera$posicion[1] <- 1
# 
# for (i in 1:nrow(corrientesCosta)){
#     distancias <- as.numeric(unname(apply(LineaCostera[,c(1,2)],1,function(x){
#         d<- distGL(as.numeric(x),as.numeric(corrientesCosta[i,c(1,2)]))
#         return(d)
#     })))
#     posminimo <- which.min(distancias)
#     corrientesCosta$posicion[i] <- LineaCostera$posicion[posminimo] + i
# }
# corrientesCosta <- corrientesCosta[order(corrientesCosta$posicion),]
# corrientesCosta$posicion <- 1:nrow(corrientesCosta)
# row.names(corrientesCosta) <- 1:nrow(corrientesCosta)
#setwd("~/")
#write.csv(corrientesCosta,file="corrientesCosta1234.csv",row.names=FALSE)

# g<-c(200,240,240,270,270,200,200,190,190,250,250,210,210,190,190,160) 
# p<-c(1,140,141,350,351,610,611,850,851,1000,1001,1400,1401,1500,1501,1680)
# g<-c(g,160,180,180,240,240,210,210,220,220,270,270,270,270,310)
# p<-c(p,1681,1890,1910,2050,2051,2150,2151,2250,2251,2650,2651,3230,3231,3370)
# g<-c(g,310,360,360,320,320,290,290,270,270,330,330,270,270,250)
# p<-c(p,3371,3480,3481,3600,3601,3850,3851,4150,4151,4540,4541,4940,4941,4970)
# g<-c(g,250,230,230,270,270,310,310,280,280,260,260,240,240,260)
# p<-c(p,5020,5150,5151,5490,5491,5570,5571,5770,5771,6200,6201,6400,6401,6600)
# g<-c(g,260,270,270,260,260,300,300,260,260,260,260,270,270,360)
# p<-c(p,6601,6800,6801,6900,6901,6950,6951,7450,7451,7500,7585,7700,7701,7950) 
# g<-c(g,0,180,160,230,230,220,220,180,180,260,260,270,270,290)
# p<-c(p,7951,8200,8201,8350,8351,8730,8731,8830,8831,8870,8871,9040,9041,9160)
# g<-c(g,290,280,280,270,270,190,190,250)
# p<-c(p,9161,9460,9461,9760,9761,9960,9961,10075)
# 
# corrientesCosta$direction.deg.t.mean <-rep(NA,times=nrow(corrientesCosta))
# i<-1
# while(i <= 93){
#     corrientesCosta$direction.deg.t.mean[p[i]:p[i+1]] <- seq(g[i],g[i+1],length= (p[i+1]-p[i]+1))
#     i<-i+2
# }
# corrientesCosta<-corrientesCosta[!is.na(corrientesCosta$direction.deg.t.mean),]
# corrientesCosta$posicion<-NULL

# i<-100
# f<-200
# lon <-mean(corrientesCosta$longitude[i:f])
# lat <-mean(corrientesCosta$latitude[i:f])
# map <- get_map(location= c(lon=lon,lat=lat),zoom=8,maptype="terrain")
# plotMapa <-ggmap(map) +
#     geom_point(data=corrientesCosta[i:f,],aes(x=longitude,y=latitude,
#                colour=direction.deg.t.mean),show.legend=T)
# plotMapa
# 
# i<-100
# f<-120
# lon <-mean(LineaCostera$longitude[i:f])
# lat <-mean(LineaCostera$latitude[i:f])
# map <- get_map(location= c(lon=lon,lat=lat),zoom=8,maptype="terrain")
# plotMapa <-ggmap(map) +
#     geom_point(data=LineaCostera[i:f,],aes(x=longitude,y=latitude,
#                                               colour=direction.deg.t.mean),show.legend=T)
# plotMapa
#write.csv(corrientesCosta,file = "corrientesCostaRaw.csv",row.names=FALSE)
####MONTAR LA TABLA corrientesCostaRaw.csv EN GITHUB con su JPG
#Esto se borrara
#??????????????/////////////////////////////////////////////////////



#Recuperamos tablas en caso de ser necesario...
setwd("~/Thesis Project AB/Data/Pre-processed Data")
LineaCostera <- read.csv("LineaCostera.csv")
LineaCostera <- LineaCostera[c(1,seq(10,2740,by=10)),]         #Reducimos tamano

setwd("~/Thesis Project AB/Data/Pre-processed Data 2")
corrientesAux <- read.csv("corrientesAux.csv")

setwd("~/Thesis Project AB/Data/Raw Data") 
corrientesCosta <- read.csv("corrientesCostaRaw.csv")  
corrientesCosta <- corrientesCosta[c(1,seq(10,9920,by=10)),]   #Reducimos tamano

setwd("~/Thesis Project AB/Data/Pre-processed Data 2")
cat("Agregando informacion de corrientes a la linea costera...\n")
cat("\n")

#Preparamos tablas para unirlas por rbind.
LineaCostera$speed.m.s.mean <- rep(NA,times=nrow(LineaCostera))
LineaCostera$direction.deg.t.mean <- rep(NA,times=nrow(LineaCostera))

corrientesCosta$posicion <- rep(-1,times=nrow(corrientesCosta))
corrientesCosta <- corrientesCosta[,names(LineaCostera)]

#Unimos tablas y completamos datos faltantes con valores promedio.
#Los datos faltantes son aquellos puntos de la linea costera como tal.
LineaCostera <-rbind(LineaCostera,corrientesCosta)

LineaCostera1 <-impute.per.regionAux(LineaCostera,radio = 1 ,medida="mean")
LineaCostera2 <-impute.per.regionAux(LineaCostera,radio = 2 ,medida="mean")
LineaCostera3 <-impute.per.regionAux(LineaCostera,radio = 5 ,medida="mean")
LineaCostera4 <-impute.per.regionAux(LineaCostera,radio = 10,medida="mean")
LineaCostera5 <-impute.per.regionAux(LineaCostera,radio = 25,medida="mean")

LineaCostera$speed.m.s.mean[is.na(LineaCostera$speed.m.s.mean)] <- 
                LineaCostera1$speed.m.s.mean[is.na(LineaCostera$speed.m.s.mean)]

LineaCostera$speed.m.s.mean[is.na(LineaCostera$speed.m.s.mean)] <- 
                LineaCostera2$speed.m.s.mean[is.na(LineaCostera$speed.m.s.mean)]

LineaCostera$speed.m.s.mean[is.na(LineaCostera$speed.m.s.mean)] <- 
                LineaCostera3$speed.m.s.mean[is.na(LineaCostera$speed.m.s.mean)]

LineaCostera$speed.m.s.mean[is.na(LineaCostera$speed.m.s.mean)] <- 
                LineaCostera4$speed.m.s.mean[is.na(LineaCostera$speed.m.s.mean)]

LineaCostera$speed.m.s.mean[is.na(LineaCostera$speed.m.s.mean)] <- 
                LineaCostera5$speed.m.s.mean[is.na(LineaCostera$speed.m.s.mean)]

LineaCostera$direction.deg.t.mean[is.na(LineaCostera$direction.deg.t.mean)] <- 
    LineaCostera1$direction.deg.t.mean[is.na(LineaCostera$direction.deg.t.mean)]

LineaCostera$direction.deg.t.mean[is.na(LineaCostera$direction.deg.t.mean)] <- 
    LineaCostera2$direction.deg.t.mean[is.na(LineaCostera$direction.deg.t.mean)]

LineaCostera$direction.deg.t.mean[is.na(LineaCostera$direction.deg.t.mean)] <- 
    LineaCostera3$direction.deg.t.mean[is.na(LineaCostera$direction.deg.t.mean)]

LineaCostera$direction.deg.t.mean[is.na(LineaCostera$direction.deg.t.mean)] <- 
    LineaCostera4$direction.deg.t.mean[is.na(LineaCostera$direction.deg.t.mean)]

LineaCostera$direction.deg.t.mean[is.na(LineaCostera$direction.deg.t.mean)] <- 
    LineaCostera5$direction.deg.t.mean[is.na(LineaCostera$direction.deg.t.mean)]

rm(LineaCostera1);rm(LineaCostera2);rm(LineaCostera3);rm(LineaCostera4)
rm(LineaCostera5)
gc()

#Formateamos la tabla de linea costera.
LineaCostera <- LineaCostera[LineaCostera$posicion > 0,]
LineaCostera <- LineaCostera[order(LineaCostera$posicion),]
LineaCostera$posicion <- 1:nrow(LineaCostera)
LineaCostera <- LineaCostera[1:(nrow(LineaCostera)-1),]
LineaCostera <- LineaCostera[,c(2,1,3,4,5)]                    #Formato lon-lat
LineaCostera$speed <- rep(NA,times = nrow(LineaCostera))
LineaCostera$ruta  <- rep(NA,times = nrow(LineaCostera))
LineaCostera <- LineaCostera[-c(1,2,3),]
row.names(LineaCostera) <- seq(1,nrow(LineaCostera))

#Calculando la velocidad de la corriente entre puntos consecutivos.
#0 grados corresponde al Norte, 90º al Este, 180º al Sur, 270º al Oeste y 360 
#grados nuevamente al Norte.

#Suavizado
LineaCostera$speed.m.s.mean <- mm(LineaCostera$speed.m.s.mean)
LineaCostera$direction.deg.t.mean <- mm(LineaCostera$direction.deg.t.mean)

#Colocamos los Na's de Margarita y evitamos su procesado.
LineaCostera$speed.m.s.mean[257:271] <- rep(NA,times=(271-257+1))
LineaCostera$direction.deg.t.mean[257:271] <- rep(NA,times=(271-257+1))
margarita <- LineaCostera[257:271,]
margarita$posicion <- seq(1:nrow(margarita))*(-1)
LineaCostera <- LineaCostera[-(257:271),]
    
#Ciclo
for(i in 1:(nrow(LineaCostera)-1)){ 
    v <- as.numeric(LineaCostera[i,c(1,2)] - LineaCostera[(i+1),c(1,2)])
    u <- v/sqrt(v[1]^2 + v[2]^2)
    ang <- as.numeric(LineaCostera$direction.deg.t.mean[i+1])
    if(ang <= 90 & ang >= 0){
        angulo <- 90 - ang
    }else if( ang <= 180 & ang > 90){
        ang <- ang - 90
        angulo <- 360 - ang
    }else if(ang <= 270 & ang > 180){
        ang <- ang - 180
        angulo <- 270 - ang
    }else if(ang <= 360 & ang > 270){
        ang <- ang - 270
        angulo <- 180 - ang
    }
    
    angulo <- angulo/180          #En radianes
    norma <- as.numeric(LineaCostera$speed.m.s.mean[i+1])
    
    w <- c(cospi(angulo),sinpi(angulo))*norma 
    
    prod <-(w[1]*u[1] + w[2]*u[2])
    velocidad <- prod*u
    velocidad <- sqrt(velocidad[1]^2 + velocidad[2]^2)
        
    LineaCostera$speed[i+1] <- velocidad 
    
    if(prod > 0 ){
        direccion <- "Ruta peninsula"
    }else{
        direccion <- "Ruta esequivo"
    }
    LineaCostera$ruta[i+1] <- direccion
}
LineaCostera$ruta[1] <- "Ruta esequivo"
LineaCostera$speed[1] <- LineaCostera$speed.m.s.mean[1]

#Formato a LineaCostera
LineaCostera$posicion <- seq(1,nrow(LineaCostera))

#Recuperamos la data de Margarita y agregamos informacion de corrientes
margarita$speed <- NULL
margarita$ruta  <- NULL
corrientesAux <- corrientesAux[corrientesAux$latitude  <= 11.5 &
                               corrientesAux$latitude  >= 10.5 &
                               corrientesAux$longitude <= -63  &
                               corrientesAux$longitude >= -65 ,]

corrientesAux <- corrientesAux[,c(5,9,11,12)]
corrientesAux$speed.cm.s.mean <- corrientesAux$speed.cm.s.mean/100
names(corrientesAux)[1] <-"speed.m.s.mean"
corrientesAux$posicion <- rep(0,times=nrow(corrientesAux))

corrientesAux <- corrientesAux[,names(margarita)]

margarita <- rbind(margarita,corrientesAux)
margarita1 <-impute.per.regionAux(margarita,radio = 5 ,medida="mean")
margarita2 <-impute.per.regionAux(margarita,radio = 10 ,medida="mean")
margarita3 <-impute.per.regionAux(margarita,radio = 20 ,medida="mean")

#Agregamos informacion de corrientes
margarita$speed.m.s.mean[is.na(margarita$speed.m.s.mean)] <- 
    margarita1$speed.m.s.mean[is.na(margarita$speed.m.s.mean)]

margarita$speed.m.s.mean[is.na(margarita$speed.m.s.mean)] <- 
    margarita2$speed.m.s.mean[is.na(margarita$speed.m.s.mean)]

margarita$speed.m.s.mean[is.na(margarita$speed.m.s.mean)] <- 
    margarita3$speed.m.s.mean[is.na(margarita$speed.m.s.mean)]

margarita$direction.deg.t.mean[is.na(margarita$direction.deg.t.mean)] <- 
    margarita1$direction.deg.t.mean[is.na(margarita$direction.deg.t.mean)]

margarita$direction.deg.t.mean[is.na(margarita$direction.deg.t.mean)] <- 
    margarita2$direction.deg.t.mean[is.na(margarita$direction.deg.t.mean)]

margarita$direction.deg.t.mean[is.na(margarita$direction.deg.t.mean)] <- 
    margarita3$direction.deg.t.mean[is.na(margarita$direction.deg.t.mean)]

#Juntamos tablas
margarita<- margarita[margarita$posicion != 0,]
margarita$speed <- rep(NA,times = nrow(margarita))
margarita$ruta  <- rep(NA,times = nrow(margarita))

LineaCostera <- rbind(LineaCostera,margarita)

#Limpiamos memoria
rm(margarita);rm(corrientesAux)
rm(margarita1);rm(margarita2);rm(margarita3)
gc()

#Guardamos la tabla en la carpeta Pre-processed Data.
setwd("~/Thesis Project AB/Data/Pre-processed Data")

#Corregimos manualmente la linea costera.

LineaCostera$ruta[16] <- "Ruta esequivo"
LineaCostera$ruta[39] <- "Ruta esequivo"
LineaCostera$ruta[85] <- "Ruta peninsula"
LineaCostera$ruta[146] <-"Ruta peninsula"
LineaCostera$ruta[172] <-"Ruta esequivo"
LineaCostera$ruta[178] <-"Ruta peninsula"
LineaCostera$ruta[213] <-"Ruta peninsula"
LineaCostera$ruta[228] <-"Ruta peninsula"
LineaCostera$ruta[240] <-"Ruta peninsula"
LineaCostera$ruta[244] <-"Ruta peninsula"
LineaCostera$ruta[245] <-"Ruta peninsula"
LineaCostera$ruta[248] <-"Ruta peninsula"
LineaCostera$ruta[256] <-"Ruta peninsula"

#Mapa en carpeta de procesados.
png(file="LineaCosteraConCorrientes.png")
map <- get_map(location= c(lon=-66.715,lat=13.115),zoom=6,maptype="terrain")
plotMapa <-ggmap(map) +
    geom_point(data=LineaCostera[1:nrow(LineaCostera),],
               aes(x=longitude,y=latitude,color=ruta),show.legend=T) +
    labs(title="Linea Costera con Corrientes Marinas")
plot(plotMapa)
dev.off()

#Exportamos la tabla
write.csv(LineaCostera,file="LineaCosteraConCorrientes.csv",row.names=FALSE)
setwd("~/Thesis Project AB/Data/Pre-processed Data 2")
write.csv(LineaCostera,file="LineaCosteraConCorrientes.csv",row.names=FALSE)

message("Se agrego a las carpetas 'Pre-processed Data' 1 y 2 el archivo 'LineaCosteraConCorrientes.csv' exitosamente.")

#Creamos las tablas rios y lugares creadas con google earth.
setwd("~/Thesis Project AB/R-Scripts")
source('RiosLugares.R')
message("Se agregaron a la carpeta 'Pre-processed Data 2' las tablas:
        'GoogleEarthRios.csv' y 'GoogleEarthLugares.csv' de Google Earth.")

#Dibujamos mapa de parques en Pre-processed Data.
setwd("~/Thesis Project AB/Data/Pre-processed Data")
parques <- read.csv("parques.csv")
parques$latitude  <- round(parques$latitude,2)
parques$longitude <- round(parques$longitude,2)

png(file="parques.png")
map <- get_map(location= c(lon=-66.715,lat=13.115),zoom=6,maptype="terrain")
plotMapa <-ggmap(map) +
    geom_point(data=parques[1:nrow(parques),],
               aes(x=longitude,y=latitude,color=localidad),show.legend=T) +
    labs(title="Parques")
plot(plotMapa)
dev.off()

write.csv(parques,file="parques.csv",row.names=F)

#Editamos la tabla DataCorales agregandole los puntos promedio de lat-lon.
setwd("~/Thesis Project AB/Data/Processed Data")
DataCorales <- read.csv("DataCorales.csv")
DataCorales$Localidad <- as.character(DataCorales$Localidad) 

Aux <- as.data.frame(tapply(parques$longitude,parques$localidad,function(x){
    pos <- ceiling(length(x)/2)
    return(x[pos])
}))
Aux$Localidad <- row.names(Aux)
names(Aux)[1] <- "longitude"
Aux$latitude <- parques$latitude[unname(unlist(apply(Aux,1,function(x){
    x[1] <- as.numeric(x[1])
    vect <- which(parques$longitude == x[1] & parques$localidad == x[2])
    pos  <- ceiling(length(vect)/2)
    return(vect[pos])
})))]

DataCorales <- merge(DataCorales,Aux,by="Localidad",all.x=T)

#Reordenamos sus columnas
DataCorales <- DataCorales[,c(1,2,3,4,5,6,8,7)]

#Exportamos
write.csv(DataCorales,file="DataCorales.csv",row.names=F)
message("Se agrego a la carpeta 'Processed Data' la tabla: 'DataCorales.csv'")

rm(list=ls())
gc()

#Agregamos datos necesarios a la linea costera con corrientes.
setwd("~/Thesis Project AB/R-Scripts")
source('Functions.R')

setwd("~/Thesis Project AB/Data/Pre-processed Data")
cat("Actualizando data de corrientes...\n")

LineaCostera <- read.csv("LineaCosteraConCorrientes.csv")
LineaCostera <- LineaCostera[-256,]
row.names(LineaCostera) <- 1:nrow(LineaCostera)

#La variable speed indica la velocidad del agua de un punto a su predecesor.
#Pasamos speed de metros por segundo a kilometros por hora.
LineaCostera$speed.km.h <- LineaCostera$speed*(3600/1000)

#Incluimos la distancia en kilometros con su predecesor.
LineaCostera$dist.km <- rep(0,times=nrow(LineaCostera))
for(i in 2:nrow(LineaCostera)){
    LineaCostera$dist.km[i] <- 
        distGL(c(LineaCostera$latitude[i],LineaCostera$longitude[i]),
               c(LineaCostera$latitude[i-1],LineaCostera$longitude[i-1]))
}
#Agregamos el tiempo de viaje en horas de un punto a su predecesor.
LineaCostera$tiempoViaje.h <- LineaCostera$dist.km / LineaCostera$speed.km.h

#Eliminamos datos de margarita.
LineaCostera$dist.km[256:270] <- rep(NA,times=15)

#Agregamos variable con tiempo ajustado
pos <- which(LineaCostera$tiempoViaje.h > 140)
long <- length(pos)

LineaCostera$tiempoViaje.h.Adj <- LineaCostera$tiempoViaje.h
LineaCostera$tiempoViaje.h.Adj[pos] <- rep(140,times=long)

LineaCostera$tiempoViaje.h.Adj <- mm(LineaCostera$tiempoViaje.h.Adj,ventana = 3)

#Tiempo de viaje ajustado en dias.
LineaCostera$tiempoViaje.Adj.dias <- LineaCostera$tiempoViaje.h.Adj /24
    
LineaCostera$tiempoViaje.h.Adj[256:270]    <- rep(NA,times=15)
LineaCostera$tiempoViaje.Adj.dias[256:270] <- rep(NA,times=15)

#Exportamos la tabla
write.csv(LineaCostera,file="LineaCosteraConCorrientes.csv",row.names=FALSE)
setwd("~/Thesis Project AB/Data/Pre-processed Data 2")
write.csv(LineaCostera,file="LineaCosteraConCorrientes.csv",row.names=FALSE)

cat("Data actualizada.\n")
cat("\n")

rm(list=ls())
gc()


cat("Procedimiento Getting-Preprocessing2.R finalizado.\n")
cat("\n")
cat("/////////////////////////////////////////////////\n")
cat("\n")
