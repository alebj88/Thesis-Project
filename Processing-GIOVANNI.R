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
    modFit<-interBuilder(entrenador)    
    setwd("~/Thesis Project/Data/R-Objects")
    save(modFit,file = "interpretador.RData")
}

#Vemos cuales imagenes de la carpeta GIovanni ya fueron procesadas.
setwd("~/Thesis Project/Data/Pre-processed Data")
processed <-grep("^GIO.",dir(),value=TRUE)

if(length(processed)!=0){
    processed <-paste0(sapply(strsplit(processed,split="\\."),
                             function(x){x[2]}),".png")
}
#Vemos cuales directorios de Giovanni ya fueron creados en Pre-processed Data.
setwd("~/Thesis Project/Data/Pre-processed Data")
DirectoriosEnPPD <-grep("^GIODIR.",dir(),value=TRUE)

if(length(DirectoriosEnPPD)!=0){
    DirectoriosEnPPD <-paste0(sapply(strsplit(DirectoriosEnPPD,split="\\."),
                             function(x){x[2]}))
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

#A cada directorio incluido por el usuario en la carpeta Giovanni le creamos
#un directorio en Pre-processed Data.
setwd("~/Thesis Project/Data/Raw Data/Giovanni")
DirectoriosEnGIO <-dir()[!(dir() %in% c(arch,"Ctrl","README_GIOVANNI.txt"))]
DirectoriosFalt <- DirectoriosEnGIO[!(DirectoriosEnGIO %in% DirectoriosEnPPD)]

if(length(DirectoriosFalt)!=0){
    for(i in 1:length(DirectoriosFalt)){
        dir.create(file.path("~/Thesis Project","Data","Pre-processed Data",
                             paste0("GIODIR.",DirectoriosFalt[i])),recursive=TRUE)
    }
}
setwd("~/Thesis Project/Data/Pre-processed Data")
DirectoriosEnPPD <-grep("^GIODIR.",dir(),value=TRUE) #Actualizamos

#Procesamos solo las imegenes nuevas. Se crearan csv's para cada una de ellas en
#Pre-processed Data.
setwd("~/Thesis Project/Data/Raw Data/Giovanni")
if(length(procesar)!=0){
    #Preguntamos si se desea evaluar el interpretadoe de imagenes o no.
    userInput <-FALSE
    while(userInput == FALSE){
        message("Indique si desea evaluar los numeros indentificados por el interpretador de imagenes.")
        humanT<- readline(prompt="Introduzca Si o No: ")
        cat("\n")
        if(humanT %in% c("NO","No","no","SI","Si","si")){
            userInput <-TRUE
        }else{
            message("Input invalido!")
        }
        if(humanT %in% c("NO","No","no")){
            humanT <-FALSE
        }else{
            humanT <-TRUE
        }
    }
    for (i in 1:length(procesar)){
        message("Procesando la imagen '",procesar[i],"'...")   
        procesamientoDeImagenes(procesar[i],HT=humanT) #VALIDAR ESTOOOOOOOOOOO
    }
}else{
    cat("No hay imagenes individuales nuevas en la carpeta 'Giovanni' que procesar.\n")   
    cat("\n")
}

#Recorremos los directorios de Giovanni escritos en Pre-processed Data y vemos
#Que imagenes faltan por porcesar en cada uno de ellos.
if(length(DirectoriosEnPPD) != 0){
    Directorios <-paste0(
        sapply(strsplit(DirectoriosEnPPD,split="\\."),function(x){x[2]}))
    for (i in 1:length(Directorios)){
        message("Procesando el directorio '",Directorios[i],"'...\n")
        
        pathGIO <-paste0("~/Thesis Project/Data/Raw Data/Giovanni/",Directorios[i])
        pathPPD <-paste0("~/Thesis Project/Data/Pre-processed Data/",
                         paste0("GIODIR.",Directorios[i]))
        #Vemos cuales imagenes estan en el directorio alojado en Giovanni 
        #pero no en el que esta en Pre-Processed Data.
        setwd(pathGIO)
        imagenesGIO <-sapply(strsplit(dir(),split="\\."),function(x){x[1]})
        setwd(pathPPD)
        imagenesPPD <-sapply(strsplit(dir(),split="\\."),function(x){x[1]})
        
        procesarImagDIR <-imagenesGIO[!(imagenesGIO %in% imagenesPPD)]
        
        if(length(procesarImagDIR) != 0){
            #Preguntamos si se desea evaluar el interpretadoe de imagenes o no.
            userInput <-FALSE
            while(userInput == FALSE){
                message("Indique si desea evaluar los numeros indentificados por el interpretador de imagenes.")
                humanT<- readline(prompt="Introduzca Si o No: ")
                cat("\n")
                if(humanT %in% c("NO","No","no","SI","Si","si")){
                    userInput <-TRUE
                }else{
                    message("Input invalido!")
                }
                if(humanT %in% c("NO","No","no")){
                    humanT <-FALSE
                }else{
                    humanT <-TRUE
                }
            }
            procesarImagDIR <-unname(
                sapply(procesarImagDIR,function(x){paste0(x,".png")}))
            for (j in 1:length(procesarImagDIR)){
                setwd(pathGIO)
                message("Procesando la imagen '",procesarImagDIR[j],"'...")
                procesamientoDeImagenes(procesarImagDIR[j],newDir=TRUE,pathPPD,HT=humanT) 
            }
        }
    }
}else{
    cat("No hay directorios nuevos en la carpeta 'Giovanni' que procesar.\n")   
    cat("\n")
}







#Juntamos todas las imagenes procesadas y las escribimos en una tabla en 
#Pre-processed Data 2.
# setwd("~/Thesis Project/Data/Pre-processed Data")
# procesar <-grep("^GIO.",dir(),value=TRUE)
# 
# if(length(procesar)!=0){
#     #juntarImagenes(procesar) #Creamos tabla en Pre-processed Data 2.
# }   #PROGRAMARLOOOOOO

rm(list=ls())
gc()

cat("Procedimiento Processing-GIOVANNI.R finalizado.\n")
cat("\n")
cat("/////////////////////////////////////////////////\n")
cat("\n")
