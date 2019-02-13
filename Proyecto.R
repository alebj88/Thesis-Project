#//////////////////////////////////////////////////////////////////////////////
#Autor: Alejandro Jose Bravo Jimenez 
#Carnet: 14-89834
#Proyecto de Tesis 2017-2019
#Universidad Simon Bolivar.
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
             "shinythemes","nortest","igraph","psych","Hmisc","MASS")

#Ciclo de busqueda
for(i in 1:length(packnec)){
    if((packnec[i] %in% paquetes) == FALSE){
        install.packages(packnec[i])
    }
}


#Control de instalacion de paquetes
paquetes <- .packages(all = TRUE)

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
    
    cat("Directorio de trabajo creado en Documentos.\n")
    cat("\n")
    setwd("~/")
    
    #Descargando codigos
    cat("Descargando codigos de Github.\n")
    cat("\n")
    setwd("~/Thesis Project AB")
    url <- "https://github.com/alebj88/Thesis-Project/archive/master.zip"
    download.file(url,destfile = "master.zip") 
    unzip("master.zip")
    file.rename("Thesis-Project-master","R-Scripts")
    file.remove("master.zip")
    
    #Reubicando archivos.
    cat("Reubicando archivos.\n")
    cat("\n")
    library(ff)
    #-------- THESIS PROJECT AB
    from <- "~/Thesis Project AB/R-Scripts/"
    to   <- "~/Thesis Project AB/"
    path1 <- paste0(from,"Readme_FinalData.txt")
    path2 <- paste0(to,"Readme_FinalData.txt")
    file.move(path1,path2)
    path1 <- paste0(from,"Procesador de Mapas")
    path2 <- paste0(to,"Procesador de Mapas")
    file.move(path1,path2)
    path1 <- paste0(from,"Union por Geolocalizacion")
    path2 <- paste0(to,"Union por Geolocalizacion")
    file.move(path1,path2)
    #-------- RAW DATA
    from <- "~/Thesis Project AB/R-Scripts/"
    to   <- "~/Thesis Project AB/Data/"
    path1 <- paste0(from,"Raw Data")
    path2 <- paste0(to,"Raw Data")
    file.move(path1,path2)
    #-------- BIOLOGICAS
    from <- "~/Thesis Project AB/R-Scripts/"
    to   <- "~/Thesis Project AB/Data/"
    path1 <- paste0(from,"Biologicas")
    path2 <- paste0(to,"Biologicas")
    file.move(path1,path2)
    #-------- FINAL DATA
    from <- "~/Thesis Project AB/R-Scripts/"
    to   <- "~/Thesis Project AB/Data/"
    path1 <- paste0(from,"Final Data")
    path2 <- paste0(to,"Final Data")
    file.move(path1,path2)
    #-------- AMBIENTALES
    from <- "~/Thesis Project AB/R-Scripts/"
    to   <- "~/Thesis Project AB/Data/"
    path1 <- paste0(from,"Ambientales")
    path2 <- paste0(to,"Ambientales")
    file.move(path1,path2)
    #-------- RMD RESULTADOS
    from <- "~/Thesis Project AB/R-Scripts/"
    to   <- "~/Thesis Project AB/"
    path1 <- paste0(from,"Rmd Resultados")
    path2 <- paste0(to,"Rmd Resultados")
    file.move(path1,path2)
    #-------- RMD EXPLORACION
    from <- "~/Thesis Project AB/R-Scripts/"
    to   <- "~/Thesis Project AB/"
    path1 <- paste0(from,"Rmd Exploracion")
    path2 <- paste0(to,"Rmd Exploracion")
    file.move(path1,path2)
    #-------- SHINYPREDICTOR
    from <- "~/Thesis Project AB/R-Scripts/"
    to   <- "~/Thesis Project AB/"
    path1 <- paste0(from,"ShinyPredictor")
    path2 <- paste0(to,"ShinyPredictor")
    file.move(path1,path2)
    #--------
    
    #Eliminando archivos innecesario.
    setwd("~/Thesis Project AB/R-Scripts")
    file.remove("Acceso a la Informacion")
    
    fold <- "~/Thesis Project AB/Union por Geolocalizacion/"
    file.remove(paste0(fold,"Datos Finales/BorrarEsto.txt"))
    file.remove(paste0(fold,"Biologicas/BorrarEsto.txt"))
    file.remove(paste0(fold,"Ambientales/BorrarEsto.txt"))
	
    fold <- "~/Thesis Project AB/Procesador de Mapas/"
    file.remove(paste0(fold,"Mapas/Ctrl/BorrarEsto.txt"))
    file.remove(paste0(fold,"Mapas/BorrarEsto.txt"))
    file.remove(paste0(fold,"Datos Finales/BorrarEsto.txt"))

    #Creando archivo de Especificaciones De La Ejecucion.
    setwd("~/Thesis Project AB")
    tabla <- list(c("Fecha.de.Descarga.de.Datos-----------",
                  as.character(Sys.time()),
                  "Info.Plataforma----------------------",sessionInfo()[[2]],
                  "Info.Windows-------------------------",sessionInfo()[[4]],
                  "Version de R-------------------------",
                  sessionInfo()[[1]]$version.string,
                  "Info.Compilador----------------------",
                  as.character(sessionInfo()[[7]]$compiler),
                  "Paquetes Cargados-------------------",
                  paste(names(sessionInfo()[[6]]),collapse=", ")))
    names(tabla) <- "INFORMACION SOBRE LA SESION DE R"
    write.table(tabla,file="Especificaciones_Ejecucion.txt")
    
    #Descomprimiendo ZIP'S
    cat("Descomprimiendo ZIP'S...\n")
    cat("\n")
    
    setwd("~/Thesis Project AB/Data/Ambientales")
    unzip("MatrizAmbientalGIO2.zip")
    file.remove("MatrizAmbientalGIO2.zip")
    unzip("BenthicSubstrate_proc1.zip")
    file.remove("BenthicSubstrate_proc1.zip")
    
    setwd("~/Thesis Project AB/ShinyPredictor")
    unzip("MatrizAmbientalGIO1_Filtrada.zip")
    file.remove("MatrizAmbientalGIO1_Filtrada.zip")
    unzip("MatrizAmbientalGIO2_Filtrada.zip")
    file.remove("MatrizAmbientalGIO2_Filtrada.zip")
}

rm(list = ls())
gc()

cat("Codigo finalizado.\n")
cat("\n")