#//////////////////////////////////////////////////////////////////////////////
#Autor: Alejandro Jose Bravo Jimenez 
#Carnet: 14-89834
#Proyecto de Tesis 2017-2019
#Universidad Simon Bolivar.
#
#
#OBSERVACION: Se Recomienda ejecutar este codigo en modo Source
#OBSERVACION: Si hay un error borrar la carpeta Thesis Project AB con todo su 
#contenido.

#Instalando los paquetes necesarios
cat("Chequeando la instalacion de los paquetes necesarios.\n")
cat("\n")
paquetes <- .packages(all = TRUE)
packnec <- c("rnoaa","ggmap","ncdf4","grDevices","ff","ffbase","stringr",
             "tidyr","caret","png","rpart","e1071","dplyr","ggplot2","maps",
             "mapdata","tidyverse","ggrepel","lubridate","vegan","rgl",
             "shinythemes","nortest","igraph","psych","Hmisc")

#Ciclo de busqueda
for(i in 1:length(packnec)){
    if((packnec[i] %in% paquetes) == FALSE){
        install.packages(packnec[i])
    }
}


#Control de instalacion de paquetes
paquetes<-.packages(all = TRUE)

if(sum(!(packnec %in% paquetes))!=0){
    stop("Las librerias necesarias no pudieron ser instaladas correctamente.")
}
rm(paquetes)
rm(packnec)
gc()

#Creando directorios en Documentos
#Si no existe directorio descargamos datos y preprocesamos. Caso contrario
#iniciamos directamente el analisis exploratorio.

setwd("~/")
if (!dir.exists("Thesis Project AB")){
    cat("Creando directorios y descargando codigos de Github...\n")
    cat("\n")
    
    d <- "Thesis Project AB"
    dir.create(d)
    dir.create(file.path(d,"Data"),recursive = TRUE)
    dir.create(file.path(d,"Data","Raw Data"),recursive = TRUE)
    dir.create(file.path(d,"Data","Ambientales"),recursive = TRUE)
    dir.create(file.path(d,"Data","Biologicas"),recursive = TRUE)
    dir.create(file.path(d,"Data","Final Data"),recursive = TRUE)
    dir.create(file.path(d,"ShinyPredictor"),recursive = TRUE)
    dir.create(file.path(d,"Rmd Resultados"),recursive = TRUE)
    
    cat("Directorio de trabajo creado en Documentos.\n")
    cat("\n")
    setwd("~/")
    
    #Descargando codigos
    setwd("~/Thesis Project AB")
    url <- "https://github.com/alebj88/Thesis-Project/archive/master.zip"
    download.file(url,destfile = "master.zip") 
    unzip("master.zip")
    file.rename("Thesis-Project-master","R-Scripts")
    file.remove("master.zip")
    
    #Reubicamos archivos.
    library(ff)
    #--------
    from <- "~/Thesis Project AB/R-Scripts/"
    to   <- "~/Thesis Project AB/"
    path1 <- paste0(from,"Readme_FinalData.txt")
    path2 <- paste0(to,"Readme_FinalData.txt")
    file.move(path1,path2)
    #--------
    from <- "~/Thesis Project AB/R-Scripts/"
    to   <- "~/Thesis Project AB/Data/Raw Data/"
    path1 <- paste0(from,"corrientesCostaRaw.jpg")
    path2 <- paste0(to,"corrientesCostaRaw.jpg")
    file.move(path1,path2)
    #--------
    
    #Limpiando memoria
    rm(d);rm(from);rm(to);rm(path1);rm(path2)
    rm(url)
    gc()
    
    #Eliminando archivo innecesario.
    setwd("~/Thesis Project AB/R-Scripts")
    file.remove("Acceso a la Informacion")
    
    #Descargando datos y ejecutando el preprocesamiento de los mismos.
    # setwd("~/Thesis Project AB/R-Scripts")
    # cat("Ejecutando el 'Getting-Preprocessing.R' para la descarga y manipulacion\n")
    # cat("de los datos originales.\n")
    # cat("\n")
    # OriginalArgo <- FALSE     #Si hay problemas en la descarga colocar en FALSE.
    # NOAAkey <- FALSE     
    # source("Getting-Preprocessing.R")
    # setwd("~/Thesis Project AB/R-Scripts")
    # cat("Ejecutando el 'Getting-Preprocessing2.R' para el procesamiento de datos.\n")
    # cat("\n")
    # source("Getting-Preprocessing2.R")
    
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

cat("Codigo finalizado.\n")
cat("\n")