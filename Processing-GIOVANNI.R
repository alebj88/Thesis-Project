#Progesando imagenes de GIOVANNI
#La solicitud de variables se debe realizar para la zona comprendida entre las
#latitudes 8 y 17 grados norte y las longitudes 60 y 73 grados oeste.
#
library(png)
library(caret)
library(rpart)
library(tidyr)

setwd("~/Thesis Project/R-Scripts")
source('Functions.R')

#Creamos el modelo de reconocimiento de imagenes y lo almacenamos en cache.
setwd("~/Thesis Project/Data/R-Objects")
if("interpretador.RData" %in% dir() == TRUE){
    load("interpretador.RData")
}else{
    setwd("~/Thesis Project/Data/R-Objects/Train Inter")
    entrenador<-read.table("entrenador.txt",header = TRUE)
    modFit<-interBuilder(entrenador)    #ENTRENARLOO
    setwd("~/Thesis Project/Data/R-Objects")
    save(modFit,file = "interpretador.RData")
}

#Creamos el arreglo de eliminacion de marcas de mapa.



#Vemos cuales imagenes de la carpeta ya fueron procesadas.
setwd("~/Thesis Project/Data/Pre-processed Data")
processed <-grep("^GIO.",dir(),value=TRUE)

if(length(processed)!=0){
    processed <-paste0(sapply(strsplit(processed,split="\\."),
                               function(x){x[2]}),".png")
}

#Buscamos las imagenes de la carpeta Giovanni que no han sido procesadas. Estas 
#imagenes fueron colocadas en la carpeta por el usuario luego de la ejecucion 
#del programa.
setwd("~/Thesis Project/Data/Raw Data/Giovanni")
arch <-grep("png$",dir(),value=TRUE)

if(length(processed)==0){
    procesar <-arch
}else{
    procesar <-arch[!(arch %in% processed)]
}

#Procesamos solo las imegenes nuevas. Se crearan csv's para cada una de ellas en
#Pre-processed Data.
if(length(procesar)!=0){
    for (i in 1:length(procesar)){
        cat("Procesando la imagen ",procesar[i],"\n")   
        cat("\n")
        procesamientoDeImagenes(procesar[i]) #PROGRAMARLOOOO
    }
}

#Juntamos todas las imagenes procesadas y las escribimos en una tabla en 
#Pre-processed Data 2.
setwd("~/Thesis Project/Data/Pre-processed Data")
procesar <-grep("^GIO.",dir(),value=TRUE)

if(length(procesar)!=0){
    #juntarImagenes(procesar) #Creamos tabla en Pre-processed Data 2.
}   #PROGRAMARLOOOOOO

rm(list=ls())
gc()

cat("Procedimiento Processing-GIOVANNI.R finalizado.\n")
cat("\n")
cat("/////////////////////////////////////////////////\n")
cat("\n")
