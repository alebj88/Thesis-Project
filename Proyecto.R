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

#alternativo <- TRUE

#Margen_Map <-c(-85,5,-72,14)           #mapCenter == c(19.5,-76.5) 
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

setwd("~/")
if (!dir.exists("Thesis Project AB")){
    cat("Creando directorios y descargando codigos de Github...\n")
    cat("\n")
    
    d<-"Thesis Project AB"
    dir.create(d)
    dir.create(file.path(d,"Data"),recursive=TRUE)
    dir.create(file.path(d,"Data","Raw Data"),recursive=TRUE)
    dir.create(file.path(d,"Data","Pre-processed Data"),recursive=TRUE)
    dir.create(file.path(d,"Data","Pre-processed Data 2"),recursive=TRUE)
    dir.create(file.path(d,"Data","Processed Data"),recursive=TRUE)
    dir.create(file.path(d,"Data","Final Data"),recursive=TRUE)
    dir.create(file.path(d,"Data","Metadata"),recursive=TRUE)
    dir.create(file.path(d,"R-Objects"),recursive=TRUE)
    dir.create(file.path(d,"R-Objects","Train Inter"),recursive=TRUE)
    dir.create(file.path(d,"Data","Raw Data","Giovanni"),recursive=TRUE)
    dir.create(file.path(d,"Data","Raw Data","Giovanni","Ctrl"),recursive=TRUE)
    dir.create(file.path(d,"Shiny Apps"),recursive=TRUE)
    dir.create(file.path(d,"Shiny Apps","ShinyExploratory"),recursive=TRUE)
    dir.create(file.path(d,"Rmd Adicionales"),recursive=TRUE)
    dir.create(file.path(d,"Rmd Exploracion"),recursive=TRUE)
    dir.create(file.path(d,"Rmd Resultados"),recursive=TRUE)
    
    cat("Directorio de trabajo creado en Documentos.\n")
    cat("\n")
    setwd("~/")
    
    #Descargando codigos
    setwd("~/Thesis Project AB")
    url<-"https://github.com/alebj88/Thesis-Project/archive/master.zip"
    download.file(url,destfile="master.zip") 
    unzip("master.zip")
    file.rename("Thesis-Project-master","R-Scripts")
    file.remove("master.zip")
    
    #Reubicamos archivos.
    library(ff)
    #--------
    from <- "~/Thesis Project AB/R-Scripts/"
    to   <- "~/Thesis Project AB/R-Objects/Train Inter/"
    path1 <- paste0(from,"entrenador.txt")
    path2 <- paste0(to,"entrenador.txt")
    file.move(path1,path2)
    to   <- "~/Thesis Project AB/"
    path1 <- paste0(from,"Readme_Giovanni.txt")
    path2 <- paste0(to,"Readme_Giovanni.txt")
    file.move(path1,path2)
    #--------
    from <- "~/Thesis Project AB/R-Scripts/"
    to   <- "~/Thesis Project AB/Data/Raw Data/"
    path1 <- paste0(from,"LineaCosteraRaw.csv")
    path2 <- paste0(to,"LineaCosteraRaw.csv")
    file.move(path1,path2)
    path1 <- paste0(from,"corrientesCostaRaw.csv")
    path2 <- paste0(to,"corrientesCostaRaw.csv")
    file.move(path1,path2)
    path1 <- paste0(from,"corrientesCostaRaw.jpg")
    path2 <- paste0(to,"corrientesCostaRaw.jpg")
    file.move(path1,path2)
    #--------
    from <- "~/Thesis Project AB/R-Scripts/"
    to   <- "~/Thesis Project AB/Data/Pre-processed Data/"
    path1 <- paste0(from,"GiovanniCleaner.csv")
    path2 <- paste0(to,"GiovanniCleaner.csv")
    file.move(path1,path2)
    path1 <- paste0(from,"parques.csv")
    path2 <- paste0(to,"parques.csv")
    file.move(path1,path2)
    #--------
    from <- "~/Thesis Project AB/R-Scripts/"
    to   <- "~/Thesis Project AB/Shiny Apps/ShinyExploratory/"
    path1 <- paste0(from,"global.R")
    path2 <- paste0(to,"global.R")
    file.move(path1,path2)
    path1 <- paste0(from,"server.R")
    path2 <- paste0(to,"server.R")
    file.move(path1,path2)
    path1 <- paste0(from,"ui.R")
    path2 <- paste0(to,"ui.R")
    file.move(path1,path2)
    path1 <- paste0(from,"DatosShiny.R")
    path2 <- paste0(to,"DatosShiny.R")
    file.move(path1,path2)
    path1 <- paste0(from,"Readme_DatosShiny.txt")
    path2 <- paste0(to,"Readme_DatosShiny.txt")
    file.move(path1,path2)
    
    #Limpiando memoria
    rm(d);rm(from);rm(to);rm(path1);rm(path2)
    rm(url)
    gc()
    
    #Eliminando archivo innecesario.
    setwd("~/Thesis Project AB/R-Scripts")
    file.remove("Acceso a la Informacion")
    
    #Descargando datos y ejecutando el preprocesamiento de los mismos.
    setwd("~/Thesis Project AB/R-Scripts")
    cat("Ejecutando el 'Getting-Preprocessing.R' para la descarga y manipulacion\n")
    cat("de los datos originales.\n")
    cat("\n")
    OriginalArgo <- FALSE     #Si hay problemas en la descarga colocar en FALSE.
    NOAAkey <- FALSE     
    source("Getting-Preprocessing.R")
    setwd("~/Thesis Project AB/R-Scripts")
    cat("Ejecutando el 'Getting-Preprocessing2.R' para el procesamiento de datos.\n")
    cat("\n")
    source("Getting-Preprocessing2.R")
    
    #Creando archivo de Especificaciones De La Ejecucion.
    setwd("~/Thesis Project AB")
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
    setwd("~/Thesis Project AB/R-Scripts")
}
#Procesando data de GIOVANNI.
cat("Ejecutando el 'Processing-GIOVANNI.R' para el analisis de imagenes.\n")
cat("\n")
setwd("~/Thesis Project AB/R-Scripts")
source("Processing-GIOVANNI.R")
#Procesando las matrices ambientales de Processed Data.
cat("Ejecutando el 'ProcesadorBA.R' para el procesamiento de matrices biologicas.\n")
cat("\n")
setwd("~/Thesis Project AB/R-Scripts")
source("ProcesadorBA.R")

cat("Codigo finalizado.\n")
cat("\n")