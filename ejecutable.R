#//////////////////////////////////////////////////////////////////////////////
#Autor: Alejandro Jose Bravo Jimenez 
#Carnet: 14-89834
#Proyecto de Tesis 2017-2018
#Universidad Simon Bolivar.
#
#
#OBSERVACION: Se Recomienda ejecutar este codigo en modo Source
#OBSERVACION: Si hay un error borrar la carpeta Thesis Project con todo su 
#contenido
#//////////////////////////////////////////////////////////////////////////////
#Opciones Globales
#options(warn=-1)
#rm(list = ls())
#gc()

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
#Control de instalacion de paquetes
paquetes<-.packages(all = TRUE)
inst<-c("rnoaa","ggmap","ncdf4","grDevices","tidyr","ff","ffbase","stringr",
        "caret","png","rpart")
if(sum(!(inst %in% paquetes))!=0){
    stop("Las librerias necesarias no pudieron ser instaladas correctamente.")
}
rm(paquetes)
rm(inst)
gc()

#Creando directorios en Documentos
#Si no existe directorio descargamos datos y preprocesamos. Caso contrario
#iniciamos directamente el analisis exploratorio.
cat("Creando directorios y descargando codigos de Github...\n")
cat("\n")

setwd("~/")
if (!dir.exists("Thesis Project")){
    d<-"Thesis Project"
    dir.create(d)
    dir.create(file.path(d,"Data"),recursive=TRUE)
    dir.create(file.path(d,"Data","Raw Data"),recursive=TRUE)
    dir.create(file.path(d,"Data","Pre-processed Data"),recursive=TRUE)
    dir.create(file.path(d,"Data","Pre-processed Data 2"),recursive=TRUE)
    dir.create(file.path(d,"Data","Metadata"),recursive=TRUE)
    dir.create(file.path(d,"Data","R-Objects"),recursive=TRUE)
    dir.create(file.path(d,"Data","R-Objects","Train Inter"),recursive=TRUE)
    dir.create(file.path(d,"Data","Raw Data","Giovanni"),recursive=TRUE)
    dir.create(file.path(d,"Data","Raw Data","Giovanni","Ctrl"),recursive=TRUE)
    
    cat("Directorio de trabajo creado en Documentos.\n")
    cat("\n")
    setwd("~/")
    
    #Descargando codigos
    setwd("~/Thesis Project")
    url<-"https://github.com/alebj88/Thesis-Project/archive/master.zip"
    download.file(url,destfile="master.zip") 
    unzip("master.zip")
    file.rename("Thesis-Project-master","R-Scripts")
    file.remove("master.zip")
    
    #Reubicamos el archivo del entrenamiento del interpretador.
    library(ff)
    from<-"~/Thesis Project/R-Scripts/"
    to<-"~/Thesis Project/Data/R-Objects/Train Inter/"
    path1<-paste0(from,"entrenador.txt")
    path2<-paste0(to,"entrenador.txt")
    file.move(path1,path2)
    to<-"~/Thesis Project/Data/Raw Data/Giovanni/"
    path1<-paste0(from,"README_GIOVANNI.txt")
    path2<-paste0(to,"README_GIOVANNI.txt")
    file.move(path1,path2)
    
    #Limpiando memoria
    rm(d);rm(from);rm(to);rm(fotos);rm(path1);rm(path2)
    rm(url)
    gc()
    
    #Descargando datos y ejecutando el preprocesamiento de los mismos.
    setwd("~/Thesis Project/R-Scripts")
    cat("Ejecutando el 'Getting-Preprocessing.R' para la descarga y manipulacion\n")
    cat("de los datos originales.\n")
    cat("\n")
    OriginalArgo<-FALSE     #Si hay problemas en la descarga colocar en FALSE.
    source("Getting-Preprocessing.R")
    setwd("~/Thesis Project/R-Scripts")
    cat("Ejecutando el 'Getting-Preprocessing2.R' para el procesamiento de datos.\n")
    cat("\n")
    source("Getting-Preprocessing2.R")
    
    #Creando archivo de Especificaciones De La Ejecucion.
    setwd("~/Thesis Project")
    tabla<-list(c("Fecha.de.Descarga.de.Datos-----------",
                  as.character(Sys.time()),
                  "Info.Plataforma----------------------",sessionInfo()[[2]],
                  "Info.Windows-------------------------",sessionInfo()[[4]],
                  "Version de R-------------------------",
                  sessionInfo()[[1]]$version.string,
                  "Info.Compilador----------------------",
                  as.character(sessionInfo()[[7]]$compiler),
                  "Paquetes Cargados-------------------",
                  paste(names(sessionInfo()[[6]]),collapse=", ")))
    names(tabla)<-"INFORMACION SOBRE LA SESION DE R- FECHA DE DESCARGA"
    write.table(tabla,file="Especificaciones_Ejecucion.txt")
    rm(tabla)
    gc()
    setwd("~/Thesis Project/R-Scripts")
}
#Procesando data de GIOVANNI.
cat("Ejecutando el 'Processing-GIOVANNI.R' para el analisis de imagenes.\n")
cat("\n")
setwd("~/Thesis Project/R-Scripts")
source("Processing-GIOVANNI.R")

cat("Procedimiento finalizado.\n")
cat("\n")