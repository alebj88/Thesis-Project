library(rnoaa)
library(ggmap)
library(ncdf4)
library(grDevices)
library(readr)
library(ff)
library(ffbase)
#library(bigmemory)
options(noaakey = "YDPCMrBIaxjsPZzSPrBlGeziseeHcOWn")

setwd("~/Thesis Project/R-Scripts")
source('Functions.R')	

#//////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////
#Argo buoy data----------------------------------------------------------
#NA-ARC web API Doc_ The database.html
cat("Conectando con la base de datos de la NOAA.\n")
cat("Descargando la data de las boyas Argo...\n")
cat("\n")

boyas<-argo_search("fullcoord","wmo", box = c(-66.715, 13.115, 5.195, 4.225))
getGeo<-function(v){
    lat<-as.numeric(v[[2]])
    lon<-as.numeric(v[[3]])
    return(c(lat,lon))
}
lat<-unname(sapply(boyas,getGeo))[1,]
lon<-unname(sapply(boyas,getGeo))[2,]
df<-data.frame(lat=lat,lon=lon)

arch<-"BoyasArgo_GeoL.png"
titulo<-"Boyas en el caribe (Argo buoy data)"
#Grafico de las boyas en el caribe en la carepeta Pre-processed Data.
setwd("~/Thesis Project/Data/Pre-processed Data")
MapaRawData(df=df,file=arch,title=titulo)
setwd("~/Thesis Project/Data/Raw Data")

#Pagina fuente del enlace de descarga           
#argo_files(wmo=as.numeric(boyas[[1]]$wmo),cyc=as.numeric(boyas[[1]]$cyc))$csv
url<-"ftp://ftp.ifremer.fr/ifremer/coriolis/tmp/co0501/3900743_133_20170925140240639.csv"
download.file(url=url,destfile = "BoyasArgo1.csv" )
d1<- read_csv("BoyasArgo1.csv")
#Pagina fuente del enlace de descarga           
#argo_files(wmo=as.numeric(boyas[[2]]$wmo),cyc=as.numeric(boyas[[2]]$cyc))$csv
url<-"ftp://ftp.ifremer.fr/ifremer/coriolis/tmp/co0501/3900743_134_20170925140439633.csv"
download.file(url=url,destfile = "BoyasArgo2.csv" )
d2<- read_csv("BoyasArgo2.csv")
#Pagina fuente del enlace de descarga           
#argo_files(wmo=as.numeric(boyas[[3]]$wmo),cyc=as.numeric(boyas[[3]]$cyc))$csv
url<-"ftp://ftp.ifremer.fr/ifremer/coriolis/tmp/co0501/3900743_135_20170925140611268.csv"
download.file(url=url,destfile = "BoyasArgo3.csv" )
d3<- read_csv("BoyasArgo3.csv")
#Pagina fuente del enlace de descarga           
#argo_files(wmo=as.numeric(boyas[[4]]$wmo),cyc=as.numeric(boyas[[4]]$cyc))$csv
url<-"ftp://ftp.ifremer.fr/ifremer/coriolis/tmp/co0501/3900743_136_20170925140646000.csv"
download.file(url=url,destfile = "BoyasArgo4.csv" )
d4<- read_csv("BoyasArgo4.csv")
#Pagina fuente del enlace de descarga           
#argo_files(wmo=as.numeric(boyas[[5]]$wmo),cyc=as.numeric(boyas[[5]]$cyc))$csv
url<-"ftp://ftp.ifremer.fr/ifremer/coriolis/tmp/co0501/4900178_047_20170925140721399.csv"
download.file(url=url,destfile = "BoyasArgo5.csv" )
d5<- read_csv("BoyasArgo5.csv")
#Pagina fuente del enlace de descarga           
#argo_files(wmo=as.numeric(boyas[[6]]$wmo),cyc=as.numeric(boyas[[6]]$cyc))$csv
url<-"ftp://ftp.ifremer.fr/ifremer/coriolis/tmp/co0501/4900178_048_20170925140754738.csv"
download.file(url=url,destfile = "BoyasArgo6.csv" )
d6<- read_csv("BoyasArgo6.csv")
#Pagina fuente del enlace de descarga           
#argo_files(wmo=as.numeric(boyas[[7]]$wmo),cyc=as.numeric(boyas[[7]]$cyc))$csv
url<-"ftp://ftp.ifremer.fr/ifremer/coriolis/tmp/co0501/4900178_049_20170925140831466.csv"
download.file(url=url,destfile = "BoyasArgo7.csv" )
d7<- read_csv("BoyasArgo7.csv")
#Pagina fuente del enlace de descarga           
#argo_files(wmo=as.numeric(boyas[[8]]$wmo),cyc=as.numeric(boyas[[8]]$cyc))$csv
url<-"ftp://ftp.ifremer.fr/ifremer/coriolis/tmp/co0501/4900178_050_20170925140908975.csv"
download.file(url=url,destfile = "BoyasArgo8.csv" )
d8<- read_csv("BoyasArgo8.csv")
#Pagina fuente del enlace de descarga           
#argo_files(wmo=as.numeric(boyas[[9]]$wmo),cyc=as.numeric(boyas[[9]]$cyc))$csv
url<-"ftp://ftp.ifremer.fr/ifremer/coriolis/tmp/co0501/4900178_051_20170925140937500.csv"
download.file(url=url,destfile = "BoyasArgo9.csv" )
d9<- read_csv("BoyasArgo9.csv")
#Pagina fuente del enlace de descarga           
#argo_files(wmo=as.numeric(boyas[[10]]$wmo),cyc=as.numeric(boyas[[10]]$cyc))$csv
url<-"ftp://ftp.ifremer.fr/ifremer/coriolis/tmp/co0501/4900178_052_20170925141012797.csv"
download.file(url=url,destfile = "BoyasArgo10.csv" )
d10<- read_csv("BoyasArgo10.csv")

#Unimos por filas de forma separada porque tenemos variables distintas en los 
#dataset.
data1<-rbind(d1,d2,d3,d4)
data2<-rbind(d5,d6,d7,d8,d9,d10)

#Eliminamos las variables vacias y con datos inecesarios.
data1<-data1[,-c(12,(15:21))]
data2<-data2[,-c(10,12,14,15,(18:21))]
data<-rbind(data1,data2)

#La tabla preprocesada se almacena en la carpeta Pre-processed Data.
setwd("~/Thesis Project/Data/Pre-processed Data")
write.csv(data,file="BoyasArgo.csv")
setwd("~/Thesis Project/Data/Raw Data")

cat("Archivo BoyasArgo.csv creado en el directorio Pre-processed Data.\n")
cat("\n")

#Limpiamos memoria
rm(boyas);rm(arch);rm(titulo)
rm(lat);rm(lon);rm(df);rm(url)
rm(d1);rm(d2);rm(d3);rm(d4);rm(d5);rm(d6);rm(d7);rm(d8);rm(d9);rm(d10)
rm(data);rm(data1);rm(data2)
gc()


#//////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////
#National Buoy Data Center------------------------------------------------
#DODS Access.html
cat("Descargando la data de las boyas de la NBDC...\n")
cat("\n")

data<-as.data.frame(buoy_stations())
data<-data[!is.na(data$lat) & !is.na(data$lon),]
data<-data[(-71.91<=data$lon) & (data$lon<=-61.52) &
                     (8.89<=data$lat) & (data$lat<=17.37) ,]

arch<-"BoyasNBDC_GeoL.png"
titulo<-"Boyas en el caribe (National Buoy Data Center)"
#Grafico de las boyas en el caribe 
setwd("~/Thesis Project/Data/Pre-processed Data")
MapaRawData(df=data,file=arch,title=titulo)
setwd("~/Thesis Project/Data/Raw Data")

#Viendo que datasets estan disponibles en las boyas que tenemos (data[[1]]). 
boyasAcoustic<-buoys("adcp")$id
boyasAcousticMMS<-buoys("adcp2")$id
boyasWind<-buoys("cwind")$id
boyasTsu<-buoys("dart")$id
boyasCurrent<-buoys("mmbcur")$id
boyasOcean<-buoys("ocean")$id
boyasPwind<-buoys("pwind")$id
boyasStdmet<-buoys("stdmet")$id
boyasWave<-buoys("swden")$id
boyasLevel<-buoys("wlevel")$id

as.character(data[[1]]) %in% boyasAcoustic    #[1] FALSE FALSE FALSE FALSE
as.character(data[[1]]) %in% boyasAcousticMMS #[1] FALSE FALSE FALSE FALSE
as.character(data[[1]]) %in% boyasWind        #[1] TRUE TRUE FALSE FALSE
as.character(data[[1]]) %in% boyasTsu         #[1] FALSE FALSE TRUE FALSE
as.character(data[[1]]) %in% boyasCurrent     #[1] FALSE FALSE FALSE FALSE
as.character(data[[1]]) %in% boyasOcean       #[1] FALSE FALSE FALSE FALSE
as.character(data[[1]]) %in% boyasPwind       #[1] TRUE TRUE FALSE FALSE
as.character(data[[1]]) %in% boyasStdmet      #[1] TRUE TRUE FALSE FALSE
as.character(data[[1]]) %in% boyasWave        #[1] TRUE TRUE FALSE FALSE
as.character(data[[1]]) %in% boyasLevel       #[1] FALSE FALSE FALSE FALSE

rm(arch);rm(titulo)
rm(boyasAcoustic);rm(boyasAcousticMMS);rm(boyasWind);rm(boyasTsu);
rm(boyasCurrent);rm(boyasOcean);rm(boyasPwind);rm(boyasStdmet);
rm(boyasWave);rm(boyasLevel)
gc()

#Velocidad del viendo en metros por segundo.---------------------------- 
#INFORMACION SOBRE LA DATA DISPONIBLE EN "$meta".
#wind_dir$units "degrees_true"
wind1<-buoy(dataset='cwind',buoyid=as.numeric(data[1,1]),year=2015)$data
wind1.1<-buoy(dataset='cwind',buoyid=as.numeric(data[1,1]),year=2016)$data
wind2<-buoy(dataset='cwind',buoyid=as.numeric(data[2,1]),year=2015)$data
wind2.1<-buoy(dataset='cwind',buoyid=as.numeric(data[2,1]),year=2016)$data

wind<-rbind(wind1,wind1.1,wind2,wind2.1) 

#Clave para el merge
wind$aux<-seq(1:nrow(wind))
wind$timeAux<- strsplit(wind$time,split=":")[[1]][1]
wind$clave<-with(wind,paste(timeAux,round(lat,4),round(lon,4),aux,sep="-"))
wind$aux<-NULL
wind$timeAux<-NULL
#Limpiamos memoria
rm(wind1);rm(wind1.1);rm(wind2);rm(wind2.1) 
gc()

#Deep-ocean Assessment and Reporting of Tsunamis data.------------------
#INFORMACION SOBRE LA DATA DISPONIBLE EN "$meta".
#$height$units "meters"
tsu1<-buoy(dataset='dart',buoyid=as.numeric(data[3,1]),year=2015)$data
tsu1.1<-buoy(dataset='dart',buoyid=as.numeric(data[3,1]),year=2016)$data

tsu<-rbind(tsu1,tsu1.1)

#Clave para el merge
tsu$aux<-seq(1:nrow(tsu))
tsu$timeAux<- strsplit(tsu$time,split=":")[[1]][1]
tsu$clave<-with(tsu,paste(timeAux,round(lat,4),round(lon,4),aux,sep="-"))
tsu$aux<-NULL
tsu$timeAux<-NULL
#Limpiamos memoria
rm(tsu1);rm(tsu1.1) 
gc()

#Peak Winds data ------------------------------------------------------
#INFORMACION SOBRE LA DATA DISPONIBLE EN "$meta".
#$gust_spd$units "meters/second" 
#$gust_time$units "seconds since 1970-01-01 00:00:00 UTC"
pwind1<-buoy(dataset='pwind',buoyid=as.numeric(data[1,1]),year=2015)$data
pwind1.1<-buoy(dataset='pwind',buoyid=as.numeric(data[1,1]),year=2016)$data
pwind2<-buoy(dataset='pwind',buoyid=as.numeric(data[2,1]),year=2015)$data
pwind2.1<-buoy(dataset='pwind',buoyid=as.numeric(data[2,1]),year=2016)$data

pwind<-rbind(pwind1,pwind1.1,pwind2,pwind2.1) 

#Clave para el merge
pwind$aux<-seq(1:nrow(pwind))
pwind$timeAux<- strsplit(pwind$time,split=":")[[1]][1]
pwind$clave<-with(pwind,paste(timeAux,round(lat,4),round(lon,4),aux,sep="-"))
pwind$aux<-NULL
pwind$timeAux<-NULL
#Limpiamos memoria
rm(pwind1);rm(pwind1.1);rm(pwind2);rm(pwind2.1) 
gc()

#Standard Meteorological data ------------------------------------------
#INFORMACION SOBRE LA DATA DISPONIBLE EN "$meta".
#"$visibility$units US_statute_miles" 
#$water_level$units "feet" dewpt_temperature$units "degree_Celsius"  
#$sea_surface_temperature$units "degree_Celsius" 
#$air_temperature$units "degree_Celsius" $air_pressure$units "hPa"
#$mean_wave_dir$units  "degrees_true" $average_wpd$units "seconds"
#$dominant_wpd$units "seconds" $wave_height$units "meters"
#$gust$units "meters/second" $wind_spd$units "meters/second" $wind_dir$units
#"degrees_true"
stdmet1<-buoy(dataset='stdmet',buoyid=as.numeric(data[1,1]),year=2015)$data
stdmet1.1<-buoy(dataset='stdmet',buoyid=as.numeric(data[1,1]),year=2016)$data
stdmet2<-buoy(dataset='stdmet',buoyid=as.numeric(data[2,1]),year=2015)$data
stdmet2.1<-buoy(dataset='stdmet',buoyid=as.numeric(data[2,1]),year=2016)$data

stdmet<-rbind(stdmet1,stdmet1.1,stdmet2,stdmet2.1)

#Clave para el merge
stdmet$aux<-seq(1:nrow(stdmet))
stdmet$timeAux<- strsplit(stdmet$time,split=":")[[1]][1]
stdmet$clave<-with(stdmet,paste(timeAux,round(lat,4),round(lon,4),aux,sep="-"))
stdmet$aux<-NULL
stdmet$timeAux<-NULL
#Limpiamos memoria
rm(stdmet1);rm(stdmet1.1);rm(stdmet2);rm(stdmet2.1) 
gc()

#Spectral Wave Density data with Spectral Wave Direction data-------------
#INFORMACION SOBRE LA DATA DISPONIBLE EN "$meta".
#$spectral_wave_density$units "(meter * meter)/Hz" 
#$mean_wave_dir$units "degrees_true" $principal_wave_dir$units "degrees_true
#wave_spectrum_r1$units  ""
swden1<-buoy(dataset='swden',buoyid=as.numeric(data[1,1]),year=2015)$data
swden1.1<-buoy(dataset='swden',buoyid=as.numeric(data[1,1]),year=2016)$data
swden2<-buoy(dataset='swden',buoyid=as.numeric(data[2,1]),year=2015)$data
swden2.1<-buoy(dataset='swden',buoyid=as.numeric(data[2,1]),year=2016)$data

swden<-rbind(swden1,swden1.1,swden2,swden2.1)

#Clave para el merge
swden$aux<-seq(1:nrow(swden))
swden$timeAux<- strsplit(swden$time,split=":")[[1]][1]
swden$clave<-with(swden,paste(timeAux,round(lat,4),round(lon,4),aux,sep="-"))
swden$aux<-NULL
swden$timeAux<-NULL
#Limpiamos memoria
rm(swden1);rm(swden1.1);rm(swden2);rm(swden2.1) 
gc()

cat("Preprocesando datos...\n")
cat("\n")

#Merging datasets pwind y stdmet
m1<-merge(pwind,stdmet,by="clave",all=TRUE)
m1$time<-m1$time.x
m1$time[which(is.na(m1$time))]<-m1$time.y[which(is.na(m1$time))]
m1$lat<-m1$lat.x
m1$lat[which(is.na(m1$lat))]<-m1$lat.y[which(is.na(m1$lat))]
m1$lon<-m1$lon.x
m1$lon[which(is.na(m1$lon))]<-m1$lon.y[which(is.na(m1$lon))]
m1$time.x<-NULL
m1$time.y<-NULL
m1$lat.x<-NULL
m1$lat.y<-NULL
m1$lon.x<-NULL
m1$lon.y<-NULL
#Limpiamos la memoria
rm(pwind);rm(stdmet)
gc()

#Merging datasets swden y tsu
m2<-merge(swden,tsu,by="clave",all=TRUE)
#Manipulando m2 como Big Data.
#Strings en factores
m2<-df.as.factor(m2)
m2<-as.ffdf(m2)

m2$time<-m2$time.x
#Eliminando Bug
m2$aux<-is.na(m2$time)
m2$time[ffwhich(m2,aux==TRUE)]<-m2$time.y[ffwhich(m2,aux==TRUE)]
m2$lat<-m2$lat.x
m2$lat[ffwhich(m2,is.na(lat))]<-m2$lat.y[ffwhich(m2,is.na(lat))]
m2$lon<-m2$lon.x
m2$lon[ffwhich(m2,is.na(lon))]<-m2$lon.y[ffwhich(m2,is.na(lon))]
m2$time.x<-NULL
m2$time.y<-NULL
m2$lat.x<-NULL
m2$lat.y<-NULL
m2$lon.x<-NULL
m2$lon.y<-NULL
m2$aux<-NULL
#Limpiamos la memoria
rm(swden);rm(tsu)
gc()

#Merging datasets wind y m1. 
#A veces nos salen dos velocidades de viento por observacion producto del merge,
#esto porque La unidad de medida mas pequena tomada para la clasificacion fue 
#la hora.
m3<-merge(wind,m1,by="clave",all=TRUE)
#Manipulando m3 como Big Data.
#Strings en factores
m3<-df.as.factor(m3)
m3<-as.ffdf(m3)

m3$time<-m3$time.x
#Eliminando Bug
m3$aux<-is.na(m3$time)
m3$time[ffwhich(m3,aux==TRUE)]<-m3$time.y[ffwhich(m3,aux==TRUE)]
m3$lat<-m3$lat.x
m3$lat[ffwhich(m3,is.na(lat))]<-m3$lat.y[ffwhich(m3,is.na(lat))]
m3$lon<-m3$lon.x
m3$lon[ffwhich(m3,is.na(lon))]<-m3$lon.y[ffwhich(m3,is.na(lon))]
m3$time.x<-NULL
m3$time.y<-NULL
m3$lat.x<-NULL
m3$lat.y<-NULL
m3$lon.x<-NULL
m3$lon.y<-NULL
m3$aux<-NULL
#Limpiamos la memoria
rm(wind);rm(m1)
gc()

#Merging datasets m2 y m3. El merge.ffdf con all=TRUE no sirve.
m4<-merge.ffdf.all(m2,m3,by="clave")

#Limpiamos memoria para habilitar espacio.
rm(m2);rm(m3);rm(data)
gc()

#Manipulando m4 como Big Data.
#Strings en factores
m4<-df.as.factor(m4)
m4<-as.ffdf(m4)

#Manipulamos m4 como BigData.
if(sum(is.na(m4$time))!=0){
    stop("La variable time de m4 tiene NA's.")
}
m4$time.y<-NULL
m4$lat.y<-NULL
m4$lon.y<-NULL
m4$clave<-NULL

#Exportamos la tabla a la carpeta Pre-processed Data.
setwd("~/Thesis Project/Data/Pre-processed Data")
write.csv.ffdf(m4,file="BoyasNBDC.csv")
setwd("~/Thesis Project/Data/Raw Data")

cat("Archivo BoyasNBDC.csv creado en el directorio Pre-processed Data.\n")
cat("\n")

#Limpiamos la memoria
rm(m4)
gc()

#//////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////
#Estaciones NOAA co-ops data  (No disponible en la zona de estudio)---------
#https://tidesandcurrents.noaa.gov/api/

#Precipitation data from NOAA Climate Prediction Center (CPC)---------------
#http://www.cpc.ncep.noaa.gov/
#(No disponible en la zona de estudio)
#rain<-as.data.frame(cpc_prcp(date = "2017-08-15"))
#rain<-rain[!is.na(rain$lat) & !is.na(rain$lon),]
#rain<-rain[(-71.91<=rain$lon) & (rain$lon<=-61.52) &
#             (8.89<=rain$lat) & (rain$lat<=17.37) ,]
#print(rain)

#NOAA Extended Reconstructed Sea Surface Temperature (ERSST) data
#https://www.ncdc.noaa.gov/data-access/marineocean-data/extended-reconstructed-sea-surface-temperature-ersst-v4
#Nada Util se puede estraer de aqui aun
#res<-ersst(year = 2017, month = 8)
#sea<-ncdf4::ncvar_get(res, "sst")
#ssta "Extended reconstructed SST anomalies"
#sst  "Extended reconstructed sea surface temperature" "degree_C"
#pal<-colorRampPalette(c("Purple","Blue","aquamarine3","green","yellow","red"))(n = 299)
#image(sea,col=pal)
#image(sea[140:170,48:63],col=pal)
#res$var$sst$dim[[1]]$vals  longitudes en grados este (filas de sea)
#res$var$sst$dim[[2]]$vals  latitudes en grados norte (columnas de sea)
#image(sea[140:170,48:63],col=pal)
#nc <- nc_open("salinity.nc")
#nc <- nc_open( "writevals.nc", readunlim=FALSE )
#print(paste("The file has",nc$nvars,"variables"))



#-------------------------------------------------------------------------------
#///////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////
#Physical, chemical and biological data collected from CTD and bottle casts 
#from the R/V HERMANO GINES from the Caribbean Sea and continental shelf of
#Venezuela in support of the Carbon Retention in a Colored Ocean (CARIACO) 
#project from February 10, 2010 to July 07, 2011 (NODC Accession 0092235)
#ftp://ftp.nodc.noaa.gov/nodc/archive/arc0046/0092235/1.1/
#Descarga [4/9/2017]
cat("Descargando data oceanografica del ftp://ftp.nodc.noaa.gov/...\n")
cat("\n")

setwd("~/Thesis Project/Data/Raw Data")
url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0046/0092235/1.1/data/0-data/C166_1_NODC.txt"
aux1<-strsplit(url,split="")[[1]][1:71]
aux2<-strsplit(url,split="")[[1]][74:84]

#Se descargan las tablas a la carpeta Raw Data
for(i in 66:82){
    if (i==69){
        url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0046/0092235/1.1/data/0-data/C169_2_NODC.txt"
        download.file(url=url,destfile ="C169_2_NODC.txt")
    }else if (i==75){
        url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0046/0092235/1.1/data/0-data/C175_3_NODC.txt"
        download.file(url=url,destfile ="C175_3_NODC.txt")    
    }else if (i==80) {
        url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0046/0092235/1.1/data/0-data/C180_2_NODC.txt"
        download.file(url=url,destfile ="C180_2_NODC.txt")
    }else{
        url<-paste0(c(aux1,i,aux2),collapse="") 
        download.file(url=url,destfile = paste0(c("C1",i,"_1_NODC.txt"),collapse=""))
    }
}

#Se unifican las tablas y se les da el formato adecuado
d1<-importar("C182_1_NODC.txt")
d2<-importar("C181_1_NODC.txt")[-1,]
d3<-importar("C180_2_NODC.txt")[-1,]
d4<-importar("C179_1_NODC.txt")[-1,]
d5<-importar("C178_1_NODC.txt")[-1,]
d6<-importar("C177_1_NODC.txt")[-1,]
d7<-importar("C176_1_NODC.txt")[-1,]
d8<-importar("C175_3_NODC.txt")[-1,]
d9<-importar("C174_1_NODC.txt")[-1,]
d10<-importar("C173_1_NODC.txt")[-1,]
d11<-importar("C172_1_NODC.txt")[-1,]
d12<-importar("C171_1_NODC.txt")[-1,]
d13<-importar("C170_1_NODC.txt")[-1,]
d14<-importar("C169_2_NODC.txt")[-1,]
d15<-importar("C168_1_NODC.txt")[-1,]
d16<-importar("C167_1_NODC.txt")[-1,]
d17<-importar("C166_1_NODC.txt")[-1,]

data<-rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17)

#La tabla formateada y compactada se almacena en la carpeta Pre-processed Data.
setwd("~/Thesis Project/Data/Pre-processed Data")
write.table(data,file="PhysicalChemicalandBiologicalData(Ci_i_NODC).txt",row.names = FALSE)

lat<-as.numeric(data$latitude[2:nrow(data)])
lon<-as.numeric(data$longitude[2:nrow(data)])
df<-data.frame(lat=lat,lon=lon)

arch<-"PhysicalChemicalandBiologicalData(Ci_i_NODC).png"
titulo<-"Physical Chemical and Biological Data"
#Grafico de geolocalizacion del estudio
MapaRawData(df=df,file=arch,title=titulo)

rm(aux1);rm(aux2)
rm(lat);rm(lon);rm(df)
rm(d1);rm(d2);rm(d3);rm(d4);rm(d5);rm(d6);rm(d7);rm(d8);rm(d9)
rm(d10);rm(d11);rm(d12);rm(d13);rm(d14);rm(d15);rm(d16);rm(d17)
rm(data)
gc()

#///////////////////////////////////////////////////////////////////////////////
#Oceanographic physical, chemical, and biological data collected from CTD and 
#bottle casts aboard the R/V HERMANO GINES in the Caribbean Sea and continental
#shelf of Venezuela in support of the Carbon Retention in a Colored Ocean 
#(CARIACO) project from 2012-11-08 to 2014-02-04 (NCEI Accession 0121284)
#ftp://ftp.nodc.noaa.gov/nodc/archive/arc0083/0121284/
#Descarga [5/9/2017]
setwd("~/Thesis Project/Data/Raw Data")
url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0083/0121284/1.1/data/0-data/C197-210/C197_1_NODC.txt"
aux1<-strsplit(url,split="")[[1]][1:79]
aux2<-strsplit(url,split="")[[1]][83:93]

#Se descargan las tablas a la carpeta Raw Data
for(i in 197:210){
    if (i==201){
        url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0083/0121284/1.1/data/0-data/C197-210/C201_2_NODC.txt"
        download.file(url=url,destfile ="C201_2_NODC.txt")
    }else if (i==207){
        url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0083/0121284/1.1/data/0-data/C197-210/C207_3_NODC.txt"
        download.file(url=url,destfile ="C207_3_NODC.txt")    
    }else{
        url<-paste0(c(aux1,i,aux2),collapse="") 
        download.file(url=url,destfile = paste0(c("C",i,"_1_NODC.txt"),collapse=""))
    }
}

d1<-importar("C197_1_NODC.txt")
d2<-importar("C198_1_NODC.txt")[-1,]
d3<-importar("C199_1_NODC.txt")[-1,]
d4<-importar("C200_1_NODC.txt")[-1,]
d5<-importar("C201_2_NODC.txt")[-1,]
d6<-importar("C202_1_NODC.txt")[-1,]
d7<-importar("C203_1_NODC.txt")[-1,]
d8<-importar("C204_1_NODC.txt")[-1,]
d9<-importar("C205_1_NODC.txt")[-1,]
d10<-importar("C206_1_NODC.txt")[-1,]
d11<-importar("C207_3_NODC.txt")[-1,]
d12<-importar("C208_1_NODC.txt")[-1,]
d13<-importar("C209_1_NODC.txt")[-1,]
d14<-importar("C210_1_NODC.txt")[-1,]

data<-rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14)

#La tabla formateada y compactada se almacena en la carpeta Pre-processed Data.
setwd("~/Thesis Project/Data/Pre-processed Data")
write.table(data,file="OceanographicPhysicalChemicalandBiologicalData(Ci_i_NODC).txt",row.names = FALSE)

lat<-as.numeric(data$latitude[2:nrow(data)])
lon<-as.numeric(data$longitude[2:nrow(data)])
df<-data.frame(lat=lat,lon=lon)

arch<-"OceanographicPhysicalChemicalandBiologicalData(Ci_i_NODC).png"
titulo<-"Oceanographic, physical, chemical and biological data"
#Grafico de geolocalizacion del estudio
MapaRawData(df=df,file=arch,title=titulo)

rm(aux1);rm(aux2)
rm(lat);rm(lon);rm(df)
rm(d1);rm(d2);rm(d3);rm(d4);rm(d5);rm(d6);rm(d7);rm(d8);rm(d9)
rm(d10);rm(d11);rm(d12);rm(d13);rm(d14)
rm(data)
gc()

#///////////////////////////////////////////////////////////////////////////////
#Oceanographic physical and chemical profile data from bottle and CTD casts 
#collected aboard the R/V Hermano Ginés in Southeastern Caribbean Sea from
#2014-04-11 to 2015-07-29 (NCEI Accession 0139312)
#ftp://ftp.nodc.noaa.gov/nodc/archive/arc0082/0139312/1.1/
setwd("~/Thesis Project/Data/Raw Data")
url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0082/0139312/1.1/data/0-data/C211_1_NODC.txt"
aux1<-strsplit(url,split="")[[1]][1:71]
aux2<-strsplit(url,split="")[[1]][74:84]

#Se descargan las tablas a la carpeta Raw Data
for(i in 11:21){
    if (i==16){
        url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0082/0139312/1.1/data/0-data/C216_4_NODC.txt"
        download.file(url=url,destfile ="C216_4_NODC.txt")
    }else{
        url<-paste0(c(aux1,i,aux2),collapse="") 
        download.file(url=url,destfile = paste0(c("C2",i,"_1_NODC.txt"),collapse=""))
    }
}

d1<-importar("C211_1_NODC.txt")
d2<-importar("C212_1_NODC.txt")[-1,]
d3<-importar("C213_1_NODC.txt")[-1,]
d4<-importar("C214_1_NODC.txt")[-1,]
d5<-importar("C215_1_NODC.txt")[-1,]
d6<-read_delim("~/Thesis Project/Data/Raw Data/C216_4_NODC.txt","\t", escape_double = FALSE, trim_ws = TRUE)[-1,]
names(d6)<-names(d5)
d7<-importar("C217_1_NODC.txt")[-1,]
d8<-importar("C218_1_NODC.txt")[-1,]
d9<-importar("C219_1_NODC.txt")[-1,]
d10<-importar("C220_1_NODC.txt")[-1,]
d11<-importar("C221_1_NODC.txt")[-1,]

data<-rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11)

#La tabla formateada y compactada se almacena en la carpeta Pre-processed Data.
setwd("~/Thesis Project/Data/Pre-processed Data")
write.table(data,file="OceanographicPhysicalChemicalprofileData(Ci_i_NODC).txt",row.names = FALSE)

lat<-as.numeric(data$latitude[2:nrow(data)])
lon<-as.numeric(data$longitude[2:nrow(data)])
df<-data.frame(lat=lat,lon=lon)

arch<-"OceanographicPhysicalChemicalprofileData(Ci_i_NODC).png"
titulo<-"Oceanographic, physical, chemical profile data"
#Grafico de geolocalizacion del estudio
MapaRawData(df=df,file=arch,title=titulo)

rm(aux1);rm(aux2)
rm(lat);rm(lon);rm(df)
rm(d1);rm(d2);rm(d3);rm(d4);rm(d5);rm(d6);rm(d7);rm(d8);rm(d9)
rm(d10);rm(d11)
rm(data)
gc()

#///////////////////////////////////////////////////////////////////////////////
#Physical, chemical and biological data collected from CTD and bottle casts 
#aboard the R/V HERMANO GINES from the Caribbean Sea and continental shelf of 
#Venezuela in support of the Carbon Retention in a Colored Ocean (CARIACO) 
#project from August 09, 2011 to October 10, 2012 (NODC Accession 0107209)
#ftp://ftp.nodc.noaa.gov/nodc/archive/arc0056/0107209/1.1/
setwd("~/Thesis Project/Data/Raw Data")
url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0056/0107209/1.1/data/0-data/C183_1_NODC.txt"
aux1<-strsplit(url,split="")[[1]][1:71]
aux2<-strsplit(url,split="")[[1]][74:84]

#Se descargan las tablas a la carpeta Raw Data
for(i in 83:96){
    if (i==86){
        url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0056/0107209/1.1/data/0-data/C186_3_NODC.txt"
        download.file(url=url,destfile ="C186_3_NODC.txt")
    }else if (i==91){
        url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0056/0107209/1.1/data/0-data/C191_2_NODC.txt"
        download.file(url=url,destfile ="C191_2_NODC.txt")    
    }else if (i==92){
    }else{
        url<-paste0(c(aux1,i,aux2),collapse="") 
        download.file(url=url,destfile = paste0(c("C1",i,"_1_NODC.txt"),collapse=""))
    }
}

d1<-importar("C183_1_NODC.txt")
d2<-importar("C184_1_NODC.txt")[-1,]
d3<-importar("C185_1_NODC.txt")[-1,]
d4<-importar("C186_3_NODC.txt")[-1,]
d5<-importar("C187_1_NODC.txt")[-1,]
d6<-importar("C188_1_NODC.txt")[-1,]
d7<-importar("C189_1_NODC.txt")[-1,]
d8<-importar("C190_1_NODC.txt")[-1,]
d9<-importar("C191_2_NODC.txt")[-1,]
d10<-importar("C193_1_NODC.txt")[-1,]
d11<-importar("C194_1_NODC.txt")[-1,]
d12<-importar("C195_1_NODC.txt")[-1,]
d13<-importar("C196_1_NODC.txt")[-1,]

data<-rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13)

#La tabla formateada y compactada se almacena en la carpeta Pre-processed Data.
setwd("~/Thesis Project/Data/Pre-processed Data")
write.table(data,file="PhysicalChemicalandBiologicalDataAboard(Ci_i_NODC).txt",row.names = FALSE)

lat<-as.numeric(data$latitude[2:nrow(data)])
lon<-as.numeric(data$longitude[2:nrow(data)])
df<-data.frame(lat=lat,lon=lon)

arch<-"PhysicalChemicalandBiologicalDataAboard(Ci_i_NODC).png"
titulo<-"Data collected from CTD and bottle casts aboard the R/V"
#Grafico de geolocalizacion del estudio
MapaRawData(df=df,file=arch,title=titulo)

rm(aux1);rm(aux2)
rm(lat);rm(lon);rm(df);rm(arch);rm(titulo)
rm(d1);rm(d2);rm(d3);rm(d4);rm(d5);rm(d6);rm(d7);rm(d8);rm(d9)
rm(d10);rm(d11);rm(d12);rm(d13)
rm(data)
gc()

#///////////////////////////////////////////////////////////////////////////////
#Uniendo las tablas de informacion oceanografica preprocesadas.
cat("Preprocesando las tablas descargadas...\n")
cat("\n")

setwd("~/Thesis Project/Data/Pre-processed Data")
d1<-read_delim("PhysicalChemicalandBiologicalData(Ci_i_NODC).txt", 
                  " ", escape_double = FALSE, trim_ws = TRUE)
d2<-read_delim("OceanographicPhysicalChemicalandBiologicalData(Ci_i_NODC).txt", 
                  " ", escape_double = FALSE, trim_ws = TRUE)
d3<-read_delim("OceanographicPhysicalChemicalprofileData(Ci_i_NODC).txt", 
                  " ", escape_double = FALSE, trim_ws = TRUE)
d4<-read_delim("PhysicalChemicalandBiologicalDataAboard(Ci_i_NODC).txt", 
                  " ", escape_double = FALSE, trim_ws = TRUE)
d2<-d2[-1,]
d3<-d3[-1,]
d4<-d4[-1,]
data<-rbind(d1,d2,d3,d4)

write.csv(data,file="OceanographicCariaco(Ci_i_NODC).csv")

cat("Archivo OceanographicCariaco(Ci_i_NODC).csv creado en el directorio Pre-processed Data.\n")
cat("\n")

rm(d1);rm(d2);rm(d3);rm(d4)
rm(data)
gc()

#///////////////////////////////////////////////////////////////////////////////
#Biogeochemical and microbiological variables measured by CTD and Niskin 
#bottles from the Hermano Gines in the Caribbean Sea for the CARIACO Ocean 
#Time-Series Program from 1995-11-13 to 2015-11-14 (NCEI Accession 0164194)
#ftp://ftp.nodc.noaa.gov/nodc/archive/arc0109/0164194/2.2/data/1-data/
cat("Descargando los datos de Cariaco del ftp://ftp.nodc.noaa.gov/ ...\n")
cat("\n")

setwd("~/Thesis Project/Data/Raw Data")
#-----------------
url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0109/0164194/2.2/data/1-data/CariacoTimeSeries_Biogeochem_Bacteria_Full_Masterfile_DATA_NCEI.csv"
download.file(url=url,destfile = "CariacoTimeSeries_Biogeochem_Bacteria_Full_Masterfile_DATA_NCEI.csv" )
CariacoTimeSeries<- read_csv("CariacoTimeSeries_Biogeochem_Bacteria_Full_Masterfile_DATA_NCEI.csv")
lat<-as.numeric(CariacoTimeSeries$X3[17:975])
lon<-as.numeric(CariacoTimeSeries$X4[17:975])
df1<-data.frame(lat=lat,lon=lon)

arch<-"CariacoTimeSeries_Biogeochem_Bacteria_Full_Masterfile_DATA_NCEI.png"
titulo<-"Biogeochemical and microbiological Biogeochem_Bacteria_Full"
#Grafico de geolocalizacion del estudio
MapaRawData(df=df1,file=arch,title=titulo)

rm(lat);rm(lon);rm(df1);rm(url)
rm(arch);rm(titulo)
gc()

#-----------------
url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0109/0164194/1.1/data/1-data/CariacoTimeSeries_Biogeochem_Full_Masterfile_DATA_NCEI.csv"
download.file(url=url,destfile ="CariacoTimeSeries_Biogeochem_Full_Masterfile_DATA_NCEI.csv")
CariacoTimeSeries1<- read_csv("CariacoTimeSeries_Biogeochem_Full_Masterfile_DATA_NCEI.csv")
lat<-as.numeric(CariacoTimeSeries1$X3[17:974])
lon<-as.numeric(CariacoTimeSeries1$X4[17:974])
df1<-data.frame(lat=lat,lon=lon)

arch<-"CariacoTimeSeries_Biogeochem_Full_Masterfile_DATA_NCEI.png"
titulo<-"Biogeochemical and microbiological Biogeochem_Full"
#Grafico de geolocalizacion del estudio
MapaRawData(df=df1,file=arch,title=titulo)

rm(lat);rm(lon);rm(df1);rm(url)
rm(arch);rm(titulo)
gc()

#///////////////////////////////////////////////////////////////////////////////
#Microbiological and geochemical data collected from Niskin bottle casts in the 
#continental shelf of Venezuela in support of the Carbon Retention in a Colored 
#Ocean (CARIACO) project from 2013-05-12 to 2014-11-11 (NCEI Accession 0147704)
#ftp://ftp.nodc.noaa.gov/nodc/archive/arc0090/0147704/1.1/
#docpdf<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0090/0147704/1.1/data/1-data/NODC_addendum_May_2016.pdf"
setwd("~/Thesis Project/Data/Raw Data")
#-----------------
url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0090/0147704/1.1/data/0-data/NODC-CAR207.csv"
download.file(url=url,destfile = "CariacoNODC-CAR207.csv" )
CariacoTimeSeries2<- read_csv("CariacoNODC-CAR207.csv")
CariacoTimeSeries2[5,9]<-"10o29.89'N 64o40.04'W"
lista<-sapply(CariacoTimeSeries2$X9[c(5,6,7)],strsplit,split = " ")
df1<-dfGeo(lista)

arch<-"CariacoNODC-CAR207.png"
titulo<-"Microbiological and geochemical data NODC-CAR207"
#Grafico de geolocalizacion del estudio
MapaRawData(df=df1,file=arch,title=titulo)

rm(lista);rm(df1);rm(url)
rm(arch);rm(titulo)
gc()

#-----------------
url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0090/0147704/1.1/data/0-data/NODC-CAR212.csv"
download.file(url=url,destfile ="CariacoNODC-CAR212.csv")
CariacoTimeSeries3<- read_csv("CariacoNODC-CAR212.csv")
CariacoTimeSeries3[5,9]<-"10o30.061'N 64o39.913'W"
lista<-sapply(CariacoTimeSeries3$X9[c(5,6,7)],strsplit,split = " ")
df1<-dfGeo(lista)

arch<-"CariacoNODC-CAR212.png"
titulo<-"Microbiological and geochemical data NODC-CAR212"
#Grafico de geolocalizacion del estudio
MapaRawData(df=df1,file=arch,title=titulo)

rm(lista);rm(df1);rm(url)
rm(arch);rm(titulo)
gc()

#-----------------
url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0090/0147704/1.1/data/0-data/NODC-CAR216.csv"
download.file(url=url,destfile ="CariacoNODC-CAR216.csv")
CariacoTimeSeries4<- read_csv("CariacoNODC-CAR216.csv")
CariacoTimeSeries4[5,8]<-"10o29.97'N 64o40.08'W"
CariacoTimeSeries4[6,8]<-"10o30.15'N 64o40.15'W"
CariacoTimeSeries4[7,8]<-"10o30.66'N 64o39.4'W"
lista<-sapply(CariacoTimeSeries4$X8[c(5,6,7)],strsplit,split = " ")
df1<-dfGeo(lista)

arch<-"CariacoNODC-CAR216.png"
titulo<-"Microbiological and geochemical data NODC-CAR216"
#Grafico de geolocalizacion del estudio
MapaRawData(df=df1,file=arch,title=titulo)

rm(lista);rm(df1);rm(url)
rm(arch);rm(titulo)
gc()

#///////////////////////////////////////////////////////////////////////////////
#Geochemistry and microbiology data collected from bottle casts from the R/V 
#HERMANO GINES from the continental shelf of Venezuela in support of the Carbon 
#Retention in a Colored Ocean (CARIACO) project from 19 January to 7 December 
#2010 (NODC Accession 0066503)
#ftp://ftp.nodc.noaa.gov/nodc/archive/arc0028/0066503/2.2/
#docupdf<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0028/0066503/1.1/data/1-data/NODC_addendum_Sept2010.pdf"
setwd("~/Thesis Project/Data/Raw Data")
#-----------------
url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0028/0066503/2.2/data/0-data/NODC-CAR153_revised.csv"
download.file(url=url,destfile = "CariacoNODC-CAR153_revised.csv" )
CariacoTimeSeries5<- read_csv("CariacoNODC-CAR153_revised.csv")
lista<-sapply(CariacoTimeSeries5$X9[c(6,7,8)],strsplit,split = " ")
df1<-dfGeo(lista)

arch<-"CariacoNODC-CAR153_revised.png"
titulo<-"Geochemistry and microbiology data NODC-CAR153_revised"
#Grafico de geolocalizacion del estudio
MapaRawData(df=df1,file=arch,title=titulo)

rm(lista);rm(df1);rm(url)
rm(arch);rm(titulo)
gc()

#-----------------
url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0028/0066503/2.2/data/0-data/NODC-CAR157_revised.csv"
download.file(url=url,destfile ="CariacoNODC-CAR157_revised.csv")
CariacoTimeSeries6<- read_csv("CariacoNODC-CAR157_revised.csv")
lista<-sapply(CariacoTimeSeries6$X9[c(6,7,8)],strsplit,split = " ")
df1<-dfGeo(lista)

arch<-"CariacoNODC-CAR157_revised.png"
titulo<-"Geochemistry and microbiology data NODC-CAR157_revised"
#Grafico de geolocalizacion del estudio
MapaRawData(df=df1,file=arch,title=titulo)

rm(lista);rm(df1);rm(url)
rm(arch);rm(titulo)
gc()

#-----------------
url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0028/0066503/2.2/data/0-data/NODC-CAR163.csv"
download.file(url=url,destfile ="CariacoNODC-CAR163.csv")
CariacoTimeSeries7<- read_csv("CariacoNODC-CAR163.csv")
CariacoTimeSeries7[5,9]<-"10o29.754'N 64o40.182'W"
lista<-sapply(CariacoTimeSeries7$X9[c(5,6,7)],strsplit,split = " ")
df1<-dfGeo(lista)

arch<-"CariacoNODC-CAR163.png"
titulo<-"Geochemistry and microbiology data NODC-CAR163"
#Grafico de geolocalizacion del estudio
MapaRawData(df=df1,file=arch,title=titulo)

rm(lista);rm(df1);rm(url)
rm(arch);rm(titulo)
gc()

#-----------------
url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0028/0066503/2.2/data/0-data/NODC-CAR169.csv"
download.file(url=url,destfile ="CariacoNODC-CAR169.csv")
CariacoTimeSeries8<- read_csv("CariacoNODC-CAR169.csv")
lista<-sapply(CariacoTimeSeries8$X9[c(6,7,8)],strsplit,split = " ")
df1<-dfGeo(lista)

arch<-"CariacoNODC-CAR169.png"
titulo<-"Geochemistry and microbiology data NODC-CAR169"
#Grafico de geolocalizacion del estudio
MapaRawData(df=df1,file=arch,title=titulo)

rm(lista);rm(df1);rm(url)
rm(arch);rm(titulo)
gc()

#-----------------
url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0028/0066503/2.2/data/0-data/NODC-CAR175.csv"
download.file(url=url,destfile ="CariacoNODC-CAR175.csv")
CariacoTimeSeries9<- read_csv("CariacoNODC-CAR175.csv")
CariacoTimeSeries9[6,9]<-"10o29.729'N 64o40.249'W"
lista<-sapply(CariacoTimeSeries9$X9[c(6,7,8)],strsplit,split = " ")
df1<-dfGeo(lista)

arch<-"CariacoNODC-CAR175.png"
titulo<-"Geochemistry and microbiology data NODC-CAR175"
#Grafico de geolocalizacion del estudio
MapaRawData(df=df1,file=arch,title=titulo)

rm(lista);rm(df1);rm(url)
rm(arch);rm(titulo)
gc()

#///////////////////////////////////////////////////////////////////////////////
#Microbiological and geochemical measurements collected using niskin bottle from 
#the Hermano Gines in the Caribbean Sea, in support of the Carbon Retention in 
#a Colored Ocean (CARIACO) project from 2011-11-10 to 2014-05-14 
#(NCEI Accession 0119359)
#ftp://ftp.nodc.noaa.gov/nodc/archive/arc0090/0119359/1.1/
setwd("~/Thesis Project/Data/Raw Data")

#-----------------
url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0090/0119359/1.1/data/0-data/NODC-CAR180.csv"
download.file(url=url,destfile = "CariacoNODC-CAR180.csv" )
CariacoTimeSeries10<- read_csv("CariacoNODC-CAR180.csv")
CariacoTimeSeries10[6,9]<-"10o30.067'N 64o40.00'W"
lista<-sapply(CariacoTimeSeries10$X9[c(6,7,8)],strsplit,split = " ")
df1<-dfGeo(lista)

arch<-"CariacoNODC-CAR180.png"
titulo<-"Microbiological and geochemical measurements NODC-CAR180"
#Grafico de geolocalizacion del estudio
MapaRawData(df=df1,file=arch,title=titulo)

rm(lista);rm(df1);rm(url)
rm(arch);rm(titulo)
gc()

#-----------------
url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0090/0119359/1.1/data/0-data/NODC-CAR186.csv"
download.file(url=url,destfile ="CariacoNODC-CAR186.csv")
CariacoTimeSeries11<- read_csv("CariacoNODC-CAR186.csv")
CariacoTimeSeries11[6,9]<-"10o29.914'N 64o40.108'W"
lista<-sapply(CariacoTimeSeries11$X9[c(6,7,8)],strsplit,split = " ")
df1<-dfGeo(lista)

arch<-"CariacoNODC-CAR186.png"
titulo<-"icrobiological and geochemical measurements NODC-CAR186"
#Grafico de geolocalizacion del estudio
MapaRawData(df=df1,file=arch,title=titulo)

rm(lista);rm(df1);rm(url)
rm(arch);rm(titulo)
gc()

#-----------------
url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0090/0119359/1.1/data/0-data/NODC-CAR191.csv"
download.file(url=url,destfile ="CariacoNODC-CAR191.csv")
CariacoTimeSeries12<- read_csv("CariacoNODC-CAR191.csv")
CariacoTimeSeries12[5,9]<-"10o30.032'N 64o40.302'W"
lista<-sapply(CariacoTimeSeries12$X9[c(5,6,7)],strsplit,split = " ")
df1<-dfGeo(lista)

arch<-"CariacoNODC-CAR191.png"
titulo<-"Microbiological and geochemical measurements NODC-CAR191"
#Grafico de geolocalizacion del estudio
MapaRawData(df=df1,file=arch,title=titulo)

rm(lista);rm(df1);rm(url)
rm(arch);rm(titulo)
gc()

#-----------------
url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0090/0119359/1.1/data/0-data/NODC-CAR201.csv"
download.file(url=url,destfile ="CariacoNODC-CAR201.csv")
CariacoTimeSeries13<- read_csv("CariacoNODC-CAR201.csv")
CariacoTimeSeries13[5,9]<-"10o30.032'N 64o40.302'W"
lista<-sapply(CariacoTimeSeries13$X9[c(5,6,7)],strsplit,split = " ")
df1<-dfGeo(lista)

arch<-"CariacoNODC-CAR201.png"
titulo<-"icrobiological and geochemical measurements NODC-CAR201"
#Grafico de geolocalizacion del estudio
MapaRawData(df=df1,file=arch,title=titulo)

rm(lista);rm(df1);rm(url)
rm(arch);rm(titulo)
gc()

#///////////////////////////////////////////////////////////////////////////////
#Partial pressure (or fugacity) of carbon dioxide, dissolved inorganic carbon, 
#pH, alkalinity, temperature, salinity and other variables collected from 
#discrete sample, profile and time series profile observations using Alkalinity 
#titrator, CTD and other instruments from HERMANO GINES in the Caribbean Sea 
#from 1995-11-08 to 2015-07-29 (NODC Accession 0112926)
#ftp://ftp.nodc.noaa.gov/nodc/archive/arc0061/0112926/2.2/

url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0061/0112926/2.2/data/0-data/CARIACO_20151209.csv"
download.file(url=url,destfile = "CARIACO_20151209.csv" )
CariacoTimeSeries14<- read_csv("CARIACO_20151209.csv")


lat<-CariacoTimeSeries14$LATITUDE[2:(nrow(CariacoTimeSeries14)-4)]
lon<-CariacoTimeSeries14$LONGITUDE[2:(nrow(CariacoTimeSeries14)-4)]
df1<-data.frame(lat=lat,lon=lon)

arch<-"CARIACO_20151209.png"
titulo<-"PartialPressureofCarbonDioxideCARIACO_20151209"
#Grafico de geolocalizacion del estudio
MapaRawData(df=df1,file=arch,title=titulo)

rm(lat);rm(lon);rm(df1);rm(url)
rm(arch);rm(titulo)
gc()

#Documentacion
#url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0061/0112926/2.2/data/0-data/oceanbt/mercury-ops2.ornl.gov_OceanOME_admin_OceanMetadata_discrete_CARIACO_TS.xml"
#download.file(url=url,destfile = "DocCARIACO_20151209.xml" )

#///////////////////////////////////////////////////////////////////////////////
#Partial pressure (or fugacity) of carbon dioxide, dissolved inorganic carbon, 
#pH, alkalinity, temperature, salinity and other variables collected from 
#discrete sample, profile and time series profile observations using Alkalinity 
#titrator, CTD and other instruments from HERMANO GINES in the Caribbean Sea 
#from 1995-11-08 to 2015-07-29 (NODC Accession 0112926)
#ftp://ftp.nodc.noaa.gov/nodc/archive/arc0061/0112926/1.1/data/0-data/

url<-"ftp://ftp.nodc.noaa.gov/nodc/archive/arc0061/0112926/1.1/data/0-data/CARIACO_Data.CSV"
download.file(url=url,destfile = "CARIACO_Data.csv" )
CariacoTimeSeries15<- read_csv("CARIACO_Data.csv")


lat<-CariacoTimeSeries15$LATITUDE[2:(nrow(CariacoTimeSeries15)-4)]
lon<-CariacoTimeSeries15$LONGITUDE[2:(nrow(CariacoTimeSeries15)-4)]
df1<-data.frame(lat=lat,lon=lon)

arch<-"CARIACO_Data.png"
titulo<-"PartialPressureofCarbonDioxideCARIACO_Data"
#Grafico de geolocalizacion del estudio
MapaRawData(df=df1,file=arch,title=titulo)

rm(lat);rm(lon);rm(df1);rm(url)
rm(arch);rm(titulo)
gc()


#///////////////////////////////////////////////////////////////////////////////
##################### Formateando tablas de Cariaco.############################
#///////////////////////////////////////////////////////////////////////////////
cat("Formateando datasets de Cariaco...\n")
cat("\n")

#Formateando tabla CariacoTimeSeries---------------------------
CariacoTimeSeries[1,1]<-names(CariacoTimeSeries)[1]
names(CariacoTimeSeries)[1]<-"X1"
#Removiendo filas de NA's
CariacoTimeSeries<-CariacoTimeSeries[-fullNA(CariacoTimeSeries),]

#Formateando tabla CariacoTimeSeries1--------------------------
CariacoTimeSeries1[1,1]<-names(CariacoTimeSeries1)[1]
names(CariacoTimeSeries1)[1]<-"X1"
#Removiendo filas de NA's
CariacoTimeSeries1<-CariacoTimeSeries1[-fullNA(CariacoTimeSeries1),]

#Formateando tabla CariacoTimeSeries2--------------------------
CariacoTimeSeries2<-cbind(CariacoTimeSeries2[1:31,],CariacoTimeSeries2[33:63,])
names(CariacoTimeSeries2)<-c(sapply(1:22,function(i){paste("x",i,sep="")}))
df1<-dfGeo(sapply(CariacoTimeSeries2$x9[c(5,6,7)],strsplit,split = " "))

CariacoTimeSeries2<-CariacoFormat(CariacoTimeSeries2,c(5,6,7),";",";",";")
#Removiendo filas de NA's
CariacoTimeSeries2<-CariacoTimeSeries2[-fullNA(CariacoTimeSeries2),]
rm(df1)
gc()

#Formateando tabla CariacoTimeSeries3----------------------------
CariacoTimeSeries3[34:39,1]<-CariacoTimeSeries3[33:38,1]
CariacoTimeSeries3<-cbind(CariacoTimeSeries3[1:31,],CariacoTimeSeries3[34:64,])
names(CariacoTimeSeries3)<-c(sapply(1:22,function(i){paste("x",i,sep="")}))
df1<-dfGeo(sapply(CariacoTimeSeries3$x9[c(5,6,7)],strsplit,split = " "))

CariacoTimeSeries3<-CariacoFormat(CariacoTimeSeries3,c(5,6,7))
#Removiendo filas de NA's
CariacoTimeSeries3<-CariacoTimeSeries3[-fullNA(CariacoTimeSeries3),]
rm(df1)
gc()

#Formateando tabla CariacoTimeSeries4-----------------------------
CariacoTimeSeries4<-cbind(CariacoTimeSeries4[1:31,],CariacoTimeSeries4[33:63,])
names(CariacoTimeSeries4)<-c(sapply(1:22,function(i){paste("x",i,sep="")}))
df1<-dfGeo(sapply(CariacoTimeSeries4$x8[c(5,6,7)],strsplit,split = " "))

CariacoTimeSeries4<-CariacoFormat(CariacoTimeSeries4,c(5,6,7),p3=";")
#Removiendo filas de NA's
CariacoTimeSeries4<-CariacoTimeSeries4[-fullNA(CariacoTimeSeries4),]
rm(df1)
gc()

#Formateando tabla CariacoTimeSeries5-------------------------------
CariacoTimeSeries5<-cbind(CariacoTimeSeries5[1:31,],CariacoTimeSeries5[32:62,])
names(CariacoTimeSeries5)<-c(sapply(1:30,function(i){paste("x",i,sep="")}))
df1<-dfGeo(sapply(CariacoTimeSeries5$x9[c(6,7,8)],strsplit,split = " "))

CariacoTimeSeries5<-CariacoFormat(CariacoTimeSeries5,c(6,7,8),p1=";",p2=";",
                    p3=";",v1=23,x2=FALSE,r1=-c(1,2,6,7,8),r2=-c(1,2,6,7,8),
                    r3=-c(1,2,6,7,8,9,10))
#Eliminando columnas vacias
CariacoTimeSeries5<-CariacoTimeSeries5[,-(25:30)]
rm(df1)
gc()

#Formateando tabla CariacoTimeSeries6--------------------------------
CariacoTimeSeries6<-cbind(CariacoTimeSeries6[1:32,],CariacoTimeSeries6[33:64,])
names(CariacoTimeSeries6)<-c(sapply(1:30,function(i){paste("x",i,sep="")}))
df1<-dfGeo(sapply(CariacoTimeSeries6$x9[c(6,7,8)],strsplit,split = " "))

CariacoTimeSeries6<-CariacoFormat(CariacoTimeSeries6,c(6,7,8),p1=";",p2=";",
                    p3=";",v1=23,x2=FALSE,r1=-c(1,2,6,7,8),r2=-c(1,2,6,7,8),
                    r3=-c(1,2,6,7,8,9,10))
#Eliminando columnas vacias
CariacoTimeSeries6<-CariacoTimeSeries6[,-(25:30)]
rm(df1)
gc()

#Formateando tabla CariacoTimeSeries7--------------------------------
CariacoTimeSeries7[36:44,1]<-CariacoTimeSeries7[32:40,1]
CariacoTimeSeries7<-cbind(CariacoTimeSeries7[1:30,],CariacoTimeSeries7[36:65,])
names(CariacoTimeSeries7)<-c(sapply(1:18,function(i){paste("x",i,sep="")}))
df1<-dfGeo(sapply(CariacoTimeSeries7$x9[c(5,6,7)],strsplit,split = " "))

CariacoTimeSeries7<-CariacoFormat(CariacoTimeSeries7,c(5,6,7),p=10
                    ,p1=";",p2=";",p3=";",v1=17,x2=FALSE,r1=3:5,r2=3:5,r3=3:5)
rm(df1)
gc()

#Formateando tabla CariacoTimeSeries8--------------------------------
CariacoTimeSeries8[51:68,1]<-CariacoTimeSeries8[50:67,1]
CariacoTimeSeries8<-CariacoTimeSeries8[-50,]
CariacoTimeSeries8[37:47,1]<-CariacoTimeSeries8[33:43,1]
CariacoTimeSeries8<-cbind(CariacoTimeSeries8[1:31,],CariacoTimeSeries8[37:67,])
names(CariacoTimeSeries8)<-c(sapply(1:26,function(i){paste("x",i,sep="")}))
df1<-dfGeo(sapply(CariacoTimeSeries8$x9[c(6,7,8)],strsplit,split = " "))


CariacoTimeSeries8<-
    CariacoFormat(CariacoTimeSeries8,c(6,7,8),p1=";",p2=";",p3=";")
#Eliminando columnas vacias
CariacoTimeSeries8<-CariacoTimeSeries8[,-(23:26)]
rm(df1)
gc()

#Formateando tabla CariacoTimeSeries9--------------------------------
CariacoTimeSeries9<-CariacoTimeSeries9[,c(1:11,14,15,12,13)]
#Removiendo filas de NA's
CariacoTimeSeries9<-CariacoTimeSeries9[-fullNA(CariacoTimeSeries9),]
names(CariacoTimeSeries9)[1]<-"x1"
CariacoTimeSeries9$x1[34:43]<-CariacoTimeSeries9$x1[30:39]

CariacoTimeSeries9<-cbind(CariacoTimeSeries9[1:29,],CariacoTimeSeries9[34:62,])
names(CariacoTimeSeries9)<-c(sapply(1:30,function(i){paste("x",i,sep="")}))
df1<-dfGeo(sapply(CariacoTimeSeries9$x9[c(4,5,6)],strsplit,split = " "))

CariacoTimeSeries9<-CariacoFormat(CariacoTimeSeries9,c(4,5,6),p1=";",p2=";",
                    p3=";",v1=23,p=9)
#Eliminando columnas vacias
CariacoTimeSeries9<-CariacoTimeSeries9[,-(25:30)]
rm(df1)
gc()

#Formateando tabla CariacoTimeSeries10-----------------------------
CariacoTimeSeries10[42:53,1]<-CariacoTimeSeries10[33:44,1]
CariacoTimeSeries10<-
    cbind(CariacoTimeSeries10[1:31,],CariacoTimeSeries10[42:72,])
names(CariacoTimeSeries10)<-c(sapply(1:36,function(i){paste("x",i,sep="")}))
df1<-dfGeo(sapply(CariacoTimeSeries10$x9[c(6,7,8)],strsplit,split = " "))

CariacoTimeSeries10<-
    CariacoFormat(CariacoTimeSeries10,c(6,7,8),p1=";",p2=";",p3=";",v1=26)
#Eliminando columnas vacias
CariacoTimeSeries10<-CariacoTimeSeries10[,-(28:36)]
rm(df1)
gc()

#Formateando tabla CariacoTimeSeries11-----------------------------
CariacoTimeSeries11[37:46,1]<-CariacoTimeSeries11[34:43,1]
CariacoTimeSeries11<-
    cbind(CariacoTimeSeries11[1:32,],CariacoTimeSeries11[37:68,])
names(CariacoTimeSeries11)<-c(sapply(1:32,function(i){paste("x",i,sep="")}))
df1<-dfGeo(sapply(CariacoTimeSeries11$x9[c(6,7,8)],strsplit,split = " "))

CariacoTimeSeries11<-
    CariacoFormat(CariacoTimeSeries11,c(6,7,8),p1=";",p2=";",p3=";",v1=24)
#Eliminando columnas vacias
CariacoTimeSeries11<-CariacoTimeSeries11[,-(26:32)]
rm(df1)
gc()

#Formateando tabla CariacoTimeSeries12-----------------------------
CariacoTimeSeries12<-CariacoTimeSeries12[,c(1:11,14,15,12,13)]
CariacoTimeSeries12<-
    cbind(CariacoTimeSeries12[1:32,],CariacoTimeSeries12[34:65,])
names(CariacoTimeSeries12)<-c(sapply(1:30,function(i){paste("x",i,sep="")}))
df1<-dfGeo(sapply(CariacoTimeSeries12$x9[c(5,6,7)],strsplit,split = " "))

CariacoTimeSeries12<-
    CariacoFormat(CariacoTimeSeries12,c(5,6,7),p1=";",p2=";",p3=";",v1=23)
#Removiendo filas de NA's
CariacoTimeSeries12<-CariacoTimeSeries12[-fullNA(CariacoTimeSeries12),]
#Eliminando columnas vacias
CariacoTimeSeries12<-CariacoTimeSeries12[,-(25:30)]
rm(df1)
gc()

#Formateando tabla CariacoTimeSeries13-----------------------------
CariacoTimeSeries13<-cbind(CariacoTimeSeries13[1:31,],CariacoTimeSeries13[33:63,])
names(CariacoTimeSeries13)<-c(sapply(1:22,function(i){paste("x",i,sep="")}))
df1<-dfGeo(sapply(CariacoTimeSeries13$x9[c(5,6,7)],strsplit,split = " "))

CariacoTimeSeries13<-CariacoFormat(CariacoTimeSeries13,c(5,6,7),complete=FALSE)
#Removiendo filas de NA's
CariacoTimeSeries13<-CariacoTimeSeries13[-fullNA(CariacoTimeSeries13),]
rm(df1)
gc()

#Formateando tabla CariacoTimeSeries14-----------------------------
CariacoTimeSeries14<-CariacoTimeSeries14[,-(51:69)]
#Removiendo filas de NA's
CariacoTimeSeries14<-CariacoTimeSeries14[-fullNA(CariacoTimeSeries14),]

#///////////////////////////////////////////////////////////////////////////////
##################### Uniendo las tablas de Cariaco.############################
#///////////////////////////////////////////////////////////////////////////////
cat("Uniendo tablas de CariacoTimeSeries...\n")
cat("\n")

#Union de tablas de Cariaco. 0 y 1.---------------------------- 
CariacoAux1<-rbind(CariacoTimeSeries,CariacoTimeSeries1[-(1:14),])
rm(CariacoTimeSeries);rm(CariacoTimeSeries1)
gc()
#Renombrando Variables
row.names(CariacoAux1)<-as.character(seq(1,nrow(CariacoAux1)))
names(CariacoAux1)<-make.names(stringr::str_trim(CariacoAux1[13,]))
CariacoAux1<-CariacoAux1[-13,]
CariacoAux1$Comments<-NULL

#Union de tablas de Cariaco. 2,3,4,y 13.-------------------- 
CariacoAux2<-rbind(CariacoTimeSeries2,CariacoTimeSeries3[-(1:13),],
                   CariacoTimeSeries4[-(1:13),],CariacoTimeSeries13[-(1:13),])
rm(CariacoTimeSeries2);rm(CariacoTimeSeries3);rm(CariacoTimeSeries4)
rm(CariacoTimeSeries13)
gc()
#Renombrando Variables
names(CariacoAux2)[1]<-names(CariacoAux1)[1]
row.names(CariacoAux2)<-as.character(seq(1,nrow(CariacoAux2)))
nombres<-unname(apply(CariacoAux2[8:10,],2,function(i){paste(i,collapse=" ")}))
nombres<-gsub("NA","",nombres)
nombres<-gsub("  "," ",nombres)
nombres<-stringr::str_trim(nombres)
nombres<-make.names(nombres)

names(CariacoAux2)[-1]<-nombres[-1]
CariacoAux2<-CariacoAux2[-(8:10),]
names(CariacoAux2)[12]<-"Notes2"
names(CariacoAux2)[16]<-"Total.Bacteria.cells.sd" 
names(CariacoAux2)[18]<-"Bacterial.Production.micrograms.C.sd"
names(CariacoAux2)[20]<-"Total.Flagellate.cells.sd"  

#Eliminando columnas repetidas
CariacoAux2[,13]<-NULL
CariacoAux2[,13]<-NULL

#Union de tablas de Cariaco. 5,6,9 y 12.--------------------------
CariacoAux3<-rbind(CariacoTimeSeries5,CariacoTimeSeries6[-(1:14),],
                   CariacoTimeSeries9[-(1:11),],CariacoTimeSeries12[-(1:13),])
rm(CariacoTimeSeries5);rm(CariacoTimeSeries6);rm(CariacoTimeSeries9)
rm(CariacoTimeSeries12)
gc()

#Renombrando Variables
names(CariacoAux3)[1]<-names(CariacoAux1)[1]
row.names(CariacoAux3)<-as.character(seq(1,nrow(CariacoAux3)))
nombres<-unname(apply(CariacoAux3[9:11,],2,function(i){paste(i,collapse=" ")}))
nombres<-gsub("NA","",nombres)
nombres<-gsub("  "," ",nombres)
nombres<-stringr::str_trim(nombres)
nombres<-make.names(nombres)

names(CariacoAux3)[-1]<-nombres[-1]
CariacoAux3<-CariacoAux3[-(9:11),]
names(CariacoAux3)[16]<-"Notes2"
names(CariacoAux3)[20]<-"Total.Bacteria.sd" 
names(CariacoAux3)[22]<-"Bacterial.Production.micrograms.C.sd" 

#Eliminando columnas repetidas
CariacoAux3[,17]<-NULL
CariacoAux3[,17]<-NULL

#Union de tablas de Cariaco. 7,8-----------------------------------
CariacoTimeSeries7$x19<-rep(NA,times=nrow(CariacoTimeSeries7))
CariacoTimeSeries7$x20<-rep(NA,times=nrow(CariacoTimeSeries7))
CariacoTimeSeries7$x21<-rep(NA,times=nrow(CariacoTimeSeries7))
CariacoTimeSeries7$x22<-rep(NA,times=nrow(CariacoTimeSeries7))
CariacoTimeSeries7<-CariacoTimeSeries7[,c(1:9,19:22,10:18)]
#Eliminando columnas repetidas
CariacoTimeSeries7$x11<-NULL
CariacoTimeSeries7$x12<-NULL
CariacoTimeSeries8$x15<-NULL
CariacoTimeSeries8$x16<-NULL

#Renombrando CariacoTimesSeries8
nombres<-unname(
    apply(CariacoTimeSeries8[9:11,1:13],2,function(i){paste(i,collapse=" ")}))
nombres<-gsub("NA","",nombres)
nombres<-gsub("  "," ",nombres)
nombres<-stringr::str_trim(nombres)
nombres<-make.names(nombres)
nombres[1]<-names(CariacoAux1)[1]

nombres2<-unname(
    apply(CariacoTimeSeries8[10:12,15:20],2,function(i){paste(i,collapse=" ")}))
nombres2<-gsub("NA","",nombres2)
nombres2<-gsub("  "," ",nombres2)
nombres2<-stringr::str_trim(nombres2)
nombres2<-make.names(nombres2)
nombres2[2]<-"Total.Bacteria.sd" 
nombres2[4]<-"Bacterial.Production.micrograms.C.sd"
nombres<-c(nombres,"Notes2",nombres2)

names(CariacoTimeSeries8)<-nombres

#Renombrando CariacoTimesSeries7
names(CariacoTimeSeries7)<-nombres
names(CariacoTimeSeries7)[7]<-"CH4.sd"
rm(nombres);rm(nombres2)
gc()

CariacoTimeSeries7<-CariacoTimeSeries7[-(8:10),]
CariacoTimeSeries7<-CariacoTimeSeries7[-(1:12),]
CariacoTimeSeries8<-CariacoTimeSeries8[-(9:11),]

#Agregando columans faltantes
CariacoTimeSeries7$x21<-rep(NA,times=nrow(CariacoTimeSeries7))
CariacoTimeSeries8$x21<-rep(NA,times=nrow(CariacoTimeSeries8))
CariacoTimeSeries7<-CariacoTimeSeries7[,c(1:7,21,8:20)]
CariacoTimeSeries8<-CariacoTimeSeries8[,c(1:6,21,7:20)]
names(CariacoTimeSeries7)[8]<-names(CariacoTimeSeries8)[8]
names(CariacoTimeSeries8)[7]<-names(CariacoTimeSeries7)[7]

#Uniendo Tablas
CariacoAux4<-rbind(CariacoTimeSeries8,CariacoTimeSeries7)
row.names(CariacoAux4)<-as.character(seq(1,nrow(CariacoAux4)))
CariacoAux4$CH4.sd[9]<-"micromoles"
CariacoAux4$CH4.sd[10]<-"per liter"
rm(CariacoTimeSeries7);rm(CariacoTimeSeries8)
gc()

#Union de tablas de Cariaco. 10,11-----------------------------------
CariacoTimeSeries11$x26<-rep(NA,times=nrow(CariacoTimeSeries11))
CariacoTimeSeries11$x27<-rep(NA,times=nrow(CariacoTimeSeries11))
CariacoTimeSeries11<-CariacoTimeSeries11[,c(1:2,19,26:27,4:18,20:25)]

#Eliminando columnas repetidas
CariacoTimeSeries11$x18<-NULL
CariacoTimeSeries11$x14<-NULL
CariacoTimeSeries10$x16<-NULL
CariacoTimeSeries10$x20<-NULL
CariacoTimeSeries10$x21<-NULL

#Renombrando CariacoTimesSeries11
nombres<-unname(
    apply(CariacoTimeSeries11[10:12,1:15],2,function(i){paste(i,collapse=" ")}))
nombres<-gsub("NA","",nombres)
nombres<-gsub("  "," ",nombres)
nombres<-stringr::str_trim(nombres)
nombres<-make.names(nombres)
nombres[1]<-names(CariacoAux1)[1]

nombres2<-unname(
    apply(CariacoTimeSeries11[9:11,16:24],2,function(i){paste(i,collapse=" ")}))
nombres2<-gsub("NA","",nombres2)
nombres2<-gsub("  "," ",nombres2)
nombres2<-stringr::str_trim(nombres2)
nombres2<-make.names(nombres2)
nombres<-c(nombres,nombres2)

nombres[4]<-"H2S"
nombres[5]<-"H2S.sd"
nombres[16]<-"Cruise Corrected.Depth"
nombres[18]<-"Notes2"  
nombres[20]<-"Total.Bacteria.sd" 
nombres[22]<-"Bacterial.Production.sd" 

names(CariacoTimeSeries11)<-nombres
#Renombrando CariacoTimesSeries10
names(CariacoTimeSeries10)<-nombres
rm(nombres);rm(nombres2)
gc()

CariacoTimeSeries11<-CariacoTimeSeries11[-(1:14),]

#Uniendo Tablas
CariacoAux5<-rbind(CariacoTimeSeries10,CariacoTimeSeries11)
row.names(CariacoAux5)<-as.character(seq(1,nrow(CariacoAux5)))
rm(CariacoTimeSeries10);rm(CariacoTimeSeries11)
gc()

#Union de tablas Auxiliares 2,3,4,5. -----------------------------------
#Corregimos los nombres de fila de todas las tablas aux.
row.names(CariacoAux1)<-as.character(seq(1,nrow(CariacoAux1)))
row.names(CariacoAux2)<-as.character(seq(1,nrow(CariacoAux2)))
row.names(CariacoAux3)<-as.character(seq(1,nrow(CariacoAux3)))
row.names(CariacoAux4)<-as.character(seq(1,nrow(CariacoAux4)))
row.names(CariacoAux5)<-as.character(seq(1,nrow(CariacoAux5)))

#Haciendo coincidir nombres
names(CariacoAux2)[9] <-"Elemental.sulfur.sd" 
names(CariacoAux2)[10]<-"Total.zero.valent.sulfur"
names(CariacoAux2)[11]<-"Total.zero.valent.sulfur.sd"
names(CariacoAux4)[16]<-"Total.Bacteria.cells" 
names(CariacoAux4)[17]<-"Total.Bacteria.cells.sd" 
names(CariacoAux3)[12]<-"Total.zero.valent.sulfur"
names(CariacoAux3)[13]<-"Total.zero.valent.sulfur.sd"
names(CariacoAux3)[17]<-"Total.Bacteria.cells" 
names(CariacoAux3)[18]<-"Total.Bacteria.cells.sd" 
names(CariacoAux5)[14]<-"Total.zero.valent.sulfur"
names(CariacoAux5)[15]<-"Total.zero.valent.sulfur.sd"
names(CariacoAux5)[19]<-"Total.Bacteria.cells" 
names(CariacoAux5)[20]<-"Total.Bacteria.cells.sd" 
names(CariacoAux5)[21]<-"Bacterial.Production.micrograms.C" 
names(CariacoAux5)[22]<-"Bacterial.Production.micrograms.C.sd" 
names(CariacoAux5)[17]<-"Cruise H2S"

#Unimos CariacoAux3 y CariacoAux5-------------------------------------------
#Formateando variables para unir CariacoAux3 y CariacoAux5 con rbind
CariacoAux5<-formatUnion(CariacoAux5,CariacoAux3)[[1]]
CariacoAux3<-formatUnion(CariacoAux5,CariacoAux3)[[2]]

#Unimos CariacoAux3 y CariacoAux5
CariacoAux3<-CariacoAux3[-(1:10),]
CariacoAux6<-rbind(CariacoAux5,CariacoAux3)
row.names(CariacoAux6)<-as.character(seq(1,nrow(CariacoAux6)))
rm(CariacoAux5);rm(CariacoAux3)
gc()

#Unimos CariacoAux4 y CariacoAux6--------------------------------------------
#Formateando variables para unir CariacoAux4 y CariacoAux6 con rbind
CariacoAux6<-formatUnion(CariacoAux6,CariacoAux4)[[1]]
CariacoAux4<-formatUnion(CariacoAux6,CariacoAux4)[[2]]

#Unimos CariacoAux4 y CariacoAux6
CariacoAux4<-CariacoAux4[-(1:10),]
CariacoAux7<-rbind(CariacoAux6,CariacoAux4)
row.names(CariacoAux7)<-as.character(seq(1,nrow(CariacoAux7)))
rm(CariacoAux6);rm(CariacoAux4)
gc()

#Unimos CariacoAux2 y CariacoAux7---------------------------------------------
#Formateando variables para unir CariacoAux2 y CariacoAux7 con rbind
CariacoAux7<-formatUnion(CariacoAux7,CariacoAux2)[[1]]
CariacoAux2<-formatUnion(CariacoAux7,CariacoAux2)[[2]]

#Unimos CariacoAux2 y CariacoAux7
CariacoAux2<-CariacoAux2[-(1:9),]
CariacoAux8<-rbind(CariacoAux7,CariacoAux2)
row.names(CariacoAux8)<-as.character(seq(1,nrow(CariacoAux8)))
names(CariacoAux8)[23]<-"Latitude"
CariacoAux8$Total.Flagellate.cells[12]<-"(x105 L-1)"
CariacoAux8$Total.Flagellate.cells.sd[12]<-"sd"
CariacoAux8$CH4.rel..error[12]<-"micromoles"
CariacoAux8$CH4.rel..error[13]<-"per liter"
CariacoAux8$Elemental.sulfur.rel..error[12]<-"micromoles"
CariacoAux8$Elemental.sulfur.rel..error[13]<-"per liter"
rm(CariacoAux2);rm(CariacoAux7)
gc()

#Unimos Aux8 y Aux1---------------------------------------------------------
#Eliminamos variables inecesarias de CariacoAux1
CariacoAux1$Cruise<-NULL
CariacoAux1$Cruise.number<-NULL
CariacoAux1$Station<-NULL
CariacoAux1$Cast.number<-NULL
CariacoAux1$decimal.year<-NULL
CariacoAux1$year<-NULL
CariacoAux1$month<-NULL
CariacoAux1$day<-NULL

#Renombramos variables de CariacoAux1 para unir las tablas.
names(CariacoAux1)[20]<-"Sulfite.sd"
names(CariacoAux1)[22]<-"Thiosulfate.sd"
names(CariacoAux1)[23]<-"Elemental.sulfur"
names(CariacoAux1)[24]<-"Elemental.sulfur.sd" 
names(CariacoAux1)[26]<-"Total.zero.valent.sulfur.sd"
names(CariacoAux1)[39]<-"Bacterial.Production.micrograms.C" 
names(CariacoAux1)[40]<-"Bacterial.Production.micrograms.C.sd"

#Creando columna de fechas en Aux8
CariacoAux8$Date<-rep(NA,times=nrow(CariacoAux8))
CariacoAux8$Date[14:nrow(CariacoAux8)]<-CariacoAux8$Notes[14:nrow(CariacoAux8)]

#Formateando variables para unir CariacoAux1 y CariacoAux8 con rbind
CariacoAux8<-formatUnion(CariacoAux8,CariacoAux1)[[1]]
CariacoAux1<-formatUnion(CariacoAux8,CariacoAux1)[[2]]

#Unimos CariacoAux1 y CariacoAux8
CariacoAux8[12,30:57]<-CariacoAux1[13,30:57]
CariacoAux1[9:13,-1]<-CariacoAux8[9:13,-1]
CariacoAux9<-rbind(CariacoAux1,CariacoAux8[-(1:13),])
row.names(CariacoAux9)<-as.character(seq(1,nrow(CariacoAux9)))
names(CariacoAux9)<-sapply(names(CariacoAux9),tolower)  #Nombres en minuscula
rm(CariacoAux1);rm(CariacoAux8)
gc()

#Unimos CariacoTimeSeries15 y CariacoTimeSeries14------------------------------
#Nombres en minuscula
names(CariacoTimeSeries14)<-sapply(names(CariacoTimeSeries14),tolower)
names(CariacoTimeSeries15)<-sapply(names(CariacoTimeSeries15),tolower)
#Formateando variables para unir CariacoTimeSeries15 y CariacoTimeSeries14
#con rbind
CariacoTimeSeries15<-formatUnion(CariacoTimeSeries15,CariacoTimeSeries14)[[1]]
CariacoTimeSeries14<-formatUnion(CariacoTimeSeries15,CariacoTimeSeries14)[[2]]

#Unimos CariacoTimeSeries15 y CariacoTimeSeries14
NAvec<-is.na(CariacoTimeSeries14[1,])
CariacoTimeSeries14[1,NAvec]<-CariacoTimeSeries15[1,NAvec]
NAvec<-is.na(CariacoTimeSeries15[1,])
CariacoTimeSeries15[1,NAvec]<-CariacoTimeSeries14[1,NAvec]

CariacoAux10<-rbind(CariacoTimeSeries14,CariacoTimeSeries15[-1,]) #PEOO
row.names(CariacoAux10)<-as.character(seq(1,nrow(CariacoAux10)))
rm(CariacoTimeSeries14);rm(CariacoTimeSeries15);rm(NAvec)
gc()

#Unimos CariacoAux10 y CariacoAux9----------------------------------------------
CariacoAux10$section<-NULL
CariacoAux10$cruise<-NULL
CariacoAux10$leg<-NULL
flags<-grep("[Ff]lag|[Ff]lags",names(CariacoAux10),value=T)
CariacoAux10[,flags]<-NULL
rm(flags)
gc()

#Formateando variables para unir CariacoAux10 y CariacoAux9 con rbind
CariacoAux10<-formatUnion(CariacoAux10,CariacoAux9)[[1]]
CariacoAux9 <-formatUnion(CariacoAux10,CariacoAux9)[[2]]
CariacoAux9[12,5:29]<-CariacoAux10[1,5:29]

#Unimos CariacoAux10 y CariacoAux9
CariacoAux11<-rbind(CariacoAux9,CariacoAux10[-1,])
row.names(CariacoAux11)<-as.character(seq(1,nrow(CariacoAux11)))
rm(CariacoAux9);rm(CariacoAux10)
gc()

#Reordenamos dataFrame CariacoAux11--------------------------------------------
CariacoAux11<-CariacoAux11[,c(30,46,1:29,31:45,47:82)]
CariacoAux11$notes[9:24]<-CariacoAux11$notes[1:16]
CariacoAux11<-CariacoAux11[-c(1:8),]
row.names(CariacoAux11)<-as.character(seq(1,nrow(CariacoAux11)))
CariacoAux11$notes[18:20]<-CariacoAux11$notes2[2:4]
CariacoAux11$notes2<-NULL
CariacoAux11$notes[4:23]<-CariacoAux11$notes[1:20]
CariacoAux11<-CariacoAux11[-c(1:3),]
row.names(CariacoAux11)<-as.character(seq(1,nrow(CariacoAux11)))

#Dandole formato estandar a las fechas
vec<-CariacoAux11$date
vec[vec=="19January2009"] <-"01/19/2009"
vec[vec=="10November2014"]<-"11/10/2014"
vec[vec=="11November2014"]<-"11/11/2014"
vec[vec=="11-12May2012"]  <-"05/12/2012"
vec[vec=="14-May-13"] <-"05/14/2013"
vec[vec=="15-May-13" ]<-"05/15/2013"
vec[grep("/",vec)]<-as.character(as.Date(vec[grep("/",vec)],"%m/%d/%Y"))	
vec[grep("[a-zA-Z]",vec)]<-as.character(
    as.Date(vec[grep("[a-zA-Z]",vec)],"%d%b%Y"))	
vec[!is.na(as.numeric(vec))]<-as.character(
    as.Date(vec[!is.na(as.numeric(vec))],"%Y%m%d"))

CariacoAux11$date<-vec
rm(vec)
gc()

#Dando formato a los missing values.
for(i in 1:ncol(CariacoAux11)){
    CariacoAux11[,i]<-formatNA(CariacoAux11[,i])
}

#Escribiendo tabla con los datos de Cariaco.
setwd("~/Thesis Project/Data/Pre-processed Data")
write.csv(CariacoAux11,file="CariacoTimeSeries.csv",row.names=FALSE)
setwd("~/Thesis Project/Data/Raw Data")

cat("Archivo CariacoTimeSeries.csv creado en el directorio Pre-processed Data.\n")
cat("\n")

rm(CariacoAux11)
gc()    




#///////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////
#-------------------------------------------------------------------------------
#Datasets de la NOAA's National Climatic Data Center (NCDC)
#ncdc_datasets()        #Listado de todos los datasets
#out<-ncdc(datasetid="NEXRAD2",startdate = "2015-01-01",enddate = "2016-01-01")

cat("Procedimiento Getting-Preprocessing.R finalizado.\n")
cat("\n")