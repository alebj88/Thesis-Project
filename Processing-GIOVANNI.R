#Progesando imagenes de GIOVANNI
#La solicitud de variables se debe realizar para la zona comprendida entre las
#latitudes 9 y 17 grados norte y las longitudes 62 y 72 grados oeste.
#
library(png)

setwd("~/Thesis Project/R-Scripts")
source('Functions.R')

#Vemos cuales imagenes de la carpeta ya fueron procesadas.
setwd("~/Thesis Project/Pre-processed Data")
processed <-grep("^GIO.",dir(),value=TRUE)
processed <-paste0(sapply(strsplit(arch,split="\\."),function(x){x[2]}),".png")

#Buscamos las imagenes de la carpeta Giovanni que no han sido procesadas. Estas 
#imagenes fueron colocadas en la carpeta por el usuario luego de la ejecucion 
#del programa.
setwd("~/Thesis Project/Data/Raw Data/Giovanni")
arch <-grep("png$",dir(),value=TRUE)
procesar <-arch[!(arch %in% processed)]

#Procesamos solo las imegenes nuevas.
if(length(procesar)!=0){
    for (i in 1:length(procesar)){
        procesamientoDeImagenes(procesar[i])
    }
}

#Juntamos todas las imagenes procesadas y las escribimos en una tabla en 
#Pre-processed Data 2.
setwd("~/Thesis Project/Pre-processed Data")
procesar <-grep("^GIO.",dir(),value=TRUE)

if(length(procesar)!=0){
    juntarImagenes(procesar) #Creamos tabla en Pre-processed Data 2.
}

rm(list=ls())
gc()

cat("Procedimiento Processing-GIOVANNI.R finalizado.\n")
cat("\n")
cat("/////////////////////////////////////////////////\n")
cat("\n")
