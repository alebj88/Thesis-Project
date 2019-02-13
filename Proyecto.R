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
    dir.create(file.path(d,"Data","Raw Data"),recursive = TRUE)
    dir.create(file.path(d,"Data","Ambientales"),recursive = TRUE)
    dir.create(file.path(d,"Data","Biologicas"),recursive = TRUE)
    dir.create(file.path(d,"Data","Final Data"),recursive = TRUE)
    dir.create(file.path(d,"Rmd Resultados"),recursive = TRUE)
    dir.create(file.path(d,"Rmd Exploracion"),recursive = TRUE)
    
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
    path1 <- paste0(from,"Readme_Giovanni.txt")
    path2 <- paste0(to,"Readme_Giovanni.txt")
    file.move(path1,path2)
    #--------
    from <- "~/Thesis Project AB/R-Scripts/"
    to   <- "~/Thesis Project AB/Data/Raw Data/"
    path1 <- paste0(from,"gocd_scuds_all.csv.gz")
    path2 <- paste0(to,"gocd_scuds_all.csv.gz")
    file.move(path1,path2)
    path1 <- paste0(from,"CorrientesMarinas.png")
    path2 <- paste0(to,"CorrientesMarinas.png")
    file.move(path1,path2)
    path1 <- paste0(from,"FactorsTransect_SCLESpecies.csv")
    path2 <- paste0(to,"FactorsTransect_SCLESpecies.csv")
    file.move(path1,path2)
    path1 <- paste0(from,"FactorsTransect_SCLEGrowth.csv")
    path2 <- paste0(to,"FactorsTransect_SCLEGrowth.csv")
    file.move(path1,path2)
    path1 <- paste0(from,"FactorsTransect_GreatGroups.csv")
    path2 <- paste0(to,"FactorsTransect_GreatGroups.csv")
    file.move(path1,path2)
    path1 <- paste0(from,"FactorsTransect_BenthicSubstrate.csv")
    path2 <- paste0(to,"FactorsTransect_BenthicSubstrate.csv")
    file.move(path1,path2)
    path1 <- paste0(from,"Mar_Vzla.csv")
    path2 <- paste0(to,"Mar_Vzla.csv")
    file.move(path1,path2)
    #--------
    from <- "~/Thesis Project AB/R-Scripts/"
    to   <- "~/Thesis Project AB/Rmd Resultados/"
    path1 <- paste0(from,"ValidacionEsferas.Rmd")
    path2 <- paste0(to,"ValidacionEsferas.Rmd")
    file.move(path1,path2)
    path1 <- paste0(from,"ResultadosNMDS3D.Rmd")
    path2 <- paste0(to,"ResultadosNMDS3D.Rmd")
    file.move(path1,path2)
    path1 <- paste0(from,"ResultadosNMDS2D.Rmd")
    path2 <- paste0(to,"ResultadosNMDS2D.Rmd")
    file.move(path1,path2)
    #--------
    from <- "~/Thesis Project AB/R-Scripts/"
    to   <- "~/Thesis Project AB/Rmd Exploracion/"
    path1 <- paste0(from,"ExploracionBiologica.Rmd")
    path2 <- paste0(to,"ExploracionBiologica.Rmd")
    file.move(path1,path2)
    path1 <- paste0(from,"RelacionOceanograficas1.Rmd")
    path2 <- paste0(to,"RelacionOceanograficas1.Rmd")
    file.move(path1,path2)
    path1 <- paste0(from,"DispersionAmbientales2.Rmd")
    path2 <- paste0(to,"DispersionAmbientales2.Rmd")
    file.move(path1,path2)
    path1 <- paste0(from,"DispersionAmbientales.Rmd")
    path2 <- paste0(to,"DispersionAmbientales.Rmd")
    file.move(path1,path2)
    #--------
    from <- "~/Thesis Project AB/R-Scripts/"
    to   <- "~/Thesis Project AB/Data/Biologicas/"
    path1 <- paste0(from,"SCLESpecies.csv")
    path2 <- paste0(to,"SCLESpecies.csv")
    file.move(path1,path2)
    path1 <- paste0(from,"SCLEGrowth.csv")
    path2 <- paste0(to,"SCLEGrowth.csv")
    file.move(path1,path2)
    path1 <- paste0(from,"GreatGroups.csv")
    path2 <- paste0(to,"GreatGroups.csv")
    file.move(path1,path2)
    path1 <- paste0(from,"BenthicSubstrate.csv")
    path2 <- paste0(to,"BenthicSubstrate.csv")
    #--------
    from <- "~/Thesis Project AB/R-Scripts/"
    to   <- "~/Thesis Project AB/Data/Final Data/"
    path1 <- paste0(from,"SCLESpecies_Matrix.csv")
    path2 <- paste0(to,"SCLESpecies_Matrix.csv")
    file.move(path1,path2)
    path1 <- paste0(from,"SCLEGrowth_Matrix.csv")
    path2 <- paste0(to,"SCLEGrowth_Matrix.csv")
    file.move(path1,path2)
    path1 <- paste0(from,"GreatGroups_Matrix.csv")
    path2 <- paste0(to,"GreatGroups_Matrix.csv")
    file.move(path1,path2)
    path1 <- paste0(from,"BenthicSubstrate_Matrix.csv")
    path2 <- paste0(to,"BenthicSubstrate_Matrix.csv")
    #--------
    from <- "~/Thesis Project AB/R-Scripts/"
    to   <- "~/Thesis Project AB/Data/Ambientales/"
    path1 <- paste0(from,"MatrizAmbientalGIO2.csv")
    path2 <- paste0(to,"MatrizAmbientalGIO2.csv")
    file.move(path1,path2)
    path1 <- paste0(from,"MatrizAmbientalGIO1.csv")
    path2 <- paste0(to,"MatrizAmbientalGIO1.csv")
    file.move(path1,path2)
    path1 <- paste0(from,"GoogleEarthRiosLugares.csv")
    path2 <- paste0(to,"GoogleEarthRiosLugares.csv")
    file.move(path1,path2)
    path1 <- paste0(from,"BenthicSubstrate_proc1.csv")
    path2 <- paste0(to,"BenthicSubstrate_proc1.csv")
    file.move(path1,path2)
    path1 <- paste0(from,"CorrientesAux.csv")
    path2 <- paste0(to,"CorrientesAux.csv")
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
    rm(tabla)
    gc()
    setwd("~/Thesis Project AB/R-Scripts")
}

cat("Codigo finalizado.\n")
cat("\n")