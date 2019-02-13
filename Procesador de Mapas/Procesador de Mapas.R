#//////////////////////////////////////////////////////////////////////////////
#Autor: Alejandro Jose Bravo Jimenez 
#Carnet: 14-89834
#Proyecto de Tesis 2017-2018
#Universidad Simon Bolivar.
#
#
#OBSERVACION: Se Recomienda ejecutar este codigo en modo Source
#OBSERVACION: Si hay un error borrar la carpeta Thesis Project AB con todo su 
#contenido.

rm(list = ls())
gc()

#///////////////////////////////////////////////////////////////////////////////
#///////////////////PANEL PARA PROCESAR IMAGENES DE GIOVANNI////////////////////
#///////////////////////////////////////////////////////////////////////////////
#Leer el archivo Readme_Giovanni.txt antes de ejecutar el procesador d imagenes.

#REGION PREDEFINIDA ////////////////////////
#Usar -73,8,-60,17 en https://giovanni.gsfc.nasa.gov/giovanni/ 
#Colocar en FALSE solo cuando las imagenes provengan de la region -73,8,-60,17.

alternativo <- FALSE    

#REGION ALTERNATIVA ////////////////////////
#Haga alternativo == TRUE e ingrese los margenes d la nueva region en Margen_Map
#Margen_Map <- c(lonIzq,latInf,lonDer,latSup)
#Debe tener 13 grados de longitud y 9 de latitud.  

# alternativo <- TRUE
# 
# Margen_Map <- c(-73,10,-60,19)           #mapCenter == c(19.5,-76.5) 
#Margen_Map <-c(-73,25,-60,34)          #mapCenter == c(29.5,-66.5)  

#///////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////

#Check del Input
if(alternativo == TRUE){
    if(Margen_Map[1] >= Margen_Map[3]){
        stop("Valores de longitud incorrectos")
    }
    if(Margen_Map[2] >= Margen_Map[4]){
        stop("Valores de latitud incorrectos")
    }
    if(Margen_Map[3] - Margen_Map[1] != 13){
        stop("El mapa debe abarcar 13 grados de longitud")
    }
    if(Margen_Map[4] - Margen_Map[2] != 9){
        stop("El mapa debe abarcar 13 grados de latitud")
    }
    #Centro del mapa
    mapCenter <- c((Margen_Map[4] + Margen_Map[2])/2,
                   (Margen_Map[3] + Margen_Map[1])/2)
}

#Instalando los paquetes necesarios
cat("Chequeando la instalacion de los paquetes necesarios.\n")
cat("\n")
paquetes<-.packages(all = TRUE)
if(("rnoaa" %in% paquetes) == FALSE){
    install.packages("rnoaa")
}
if(("ggmap" %in% paquetes) == FALSE){
    install.packages("ggmap")
}
if(("ncdf4" %in% paquetes) == FALSE){
    install.packages("ncdf4")
}
if(("grDevices" %in% paquetes) == FALSE){
    install.packages("grDevices")
}
if(("ff" %in% paquetes) == FALSE){
    install.packages("ff")
}
if(("ffbase" %in% paquetes) == FALSE){
    install.packages("ffbase")
}
if(("stringr" %in% paquetes) == FALSE){
    install.packages("stringr")
}
if(("tidyr" %in% paquetes) == FALSE){
    install.packages("tidyr")
}
if(("caret" %in% paquetes) == FALSE){
    install.packages("caret")
}
if(("png" %in% paquetes) == FALSE){
    install.packages("png")
}
if(("rpart" %in% paquetes) == FALSE){
    install.packages("rpart")
}
if(("e1071" %in% paquetes) == FALSE){
    install.packages("e1071")
}
if(("dplyr" %in% paquetes) == FALSE){
    install.packages("dplyr")
}
#Control de instalacion de paquetes
paquetes<-.packages(all = TRUE)
inst<-c("rnoaa","ggmap","ncdf4","grDevices","tidyr","ff","ffbase","stringr",
        "caret","png","rpart","dplyr","e1071")
if(sum(!(inst %in% paquetes))!=0){
    stop("Las librerias necesarias no pudieron ser instaladas correctamente.")
}
rm(paquetes)
rm(inst)
gc()

#Creando directorios en Documentos
#Si no existe directorio descargamos datos y preprocesamos. Caso contrario
#iniciamos directamente el analisis exploratorio.

#Reestablecemos el working directory
if("d" %in% ls() == FALSE){
    d <- getwd()    
    d <- strsplit(d,split = "/")[[1]]
    while (d[length(d)] != "Procesador de Mapas"){
        d <- d[1:(length(d) - 1)]
    }
    d <- paste(d, collapse = "/")
}
setwd(d)

if (!dir.exists("Data Preprocesada")){
    dir.create("Data Preprocesada",recursive=TRUE)
}

if (!dir.exists("Mapas")){
    dir.create("Mapas",recursive=TRUE)
}

if (!dir.exists("Datos Finales")){
    dir.create("Datos Finales",recursive=TRUE)
}
if (!dir.exists(file.path("Mapas","Ctrl"))){
    dir.create(file.path("Mapas","Ctrl"),recursive=TRUE)
}


#Procesando data de GIOVANNI.

cat("Ejecutando el 'Processing-GIOVANNI.R' para el analisis de imagenes.\n")
cat("\n")
setwd(paste0(d,"/R-Scripts"))
source("Processing-GIOVANNI.R")

cat("Codigo finalizado.\n")
cat("\n")

