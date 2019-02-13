#//////////////////////////////////////////////////////////////////////////////
#Autor: Alejandro Jose Bravo Jimenez 
#Carnet: 14-89834
#Proyecto de Tesis 2017-2018 04167248967
#Universidad Simon Bolivar.
#
#
#OBSERVACION: Se Recomienda ejecutar este codigo en modo Source
#OBSERVACION: Si hay un error borrar la carpeta Thesis Project AB con todo su 
#contenido.

#///////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////

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
    while (d[length(d)] != "Union por Geolocalizacion"){
        d <- d[1:(length(d) - 1)]
    }
    d <- paste(d, collapse = "/")
}
setwd(d)

if (!dir.exists("Ambientales")){
    dir.create("Ambientales",recursive=TRUE)
}

if (!dir.exists("Biologicas")){
    dir.create("Biologicas",recursive=TRUE)
}

if (!dir.exists("Datos Finales")){
    dir.create("Datos Finales",recursive=TRUE)
}

#Procesando las matrices ambientales de Processed Data.
cat("Ejecutando el 'ProcesadorBA.R' para la union de datos por Geolocalizacion.\n")
cat("\n")
setwd(paste0(d,"/R-Scripts"))
source("ProcesadorBA.R")

cat("Codigo finalizado.\n")
cat("\n")
