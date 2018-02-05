#Progesando imagenes de GIOVANNI
#La solicitud de variables se debe realizar para la zona comprendida entre las
#latitudes 8 y 17 grados norte y las longitudes 60 y 73 grados oeste.
#
#El codigo procesara las imagenes colocadas en la carpeta Giovanni y creara 
#una tabla para cada una de ellas en la carpeta Pre-preocessed Data.Las imagenes
#provenientes del estilo "Animation" deben ser incluidas en Giovanni dentro de 
#su carpeta original procurando colocarle a la misma el nombre correcto. En este
#caso se construiran directorios en Pre-processed Data para cada una de esas
#carpetas.
#
#El programa buscara en Pro-processed Data si hay archivos CSV cuyos nombres
#empiecen con "GIO.". Con eso evitara procesar las imagenes que permitieron su
#creacion nuevamente.
#
#Una vez finalizado el procesamiento de todas las images el programa las juntara 
#en una sola generando la matriz ambiental. Una vez la matriz es creada, los
#archivos que permitieron su creacion seran registrados para evitar que el 
#programa en una nueva ejecucion, la construya de nuevo. Ella solo recibira
#la informacion nieva que se le suministre al programa.
#
#Si se agrega una imagen nueva a Giovanni o una carpeta con variables sin 
#procesar, el programa detectara que las mismas no tienen un CSV asociado y 
#procedera a su tratamiento. Una vez finalizado las unira a la matriz ambiental
#y registrara esas nuevas tablas como procesadas.
#
#Si la matria ambiental existe el programa ignorara los archivos cuyos nombres
#uso para su construccion alojados en Pre-processed Data. esto implica que 
#si se desean incluir cambios realizados a los mismos la matriz ambiental debe 
#removida primero para que la union de tablas se reinicie.

library(png)
library(caret)
library(rpart)
library(tidyr)
library(dplyr)
library(e1071)

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

#Vemos cuales imagenes de la carpeta Giovanni ya fueron procesadas.
setwd("~/Thesis Project/Data/Pre-processed Data")
processed <-grep("^GIO\\.",dir(),value=TRUE)

if(length(processed)!=0){
    processed <-paste0(sapply(strsplit(processed,split="\\."),
                             function(x){x[2]}),".png")
}
#Vemos cuales directorios de Giovanni ya fueron creados en Pre-processed Data.
setwd("~/Thesis Project/Data/Pre-processed Data")
DirectoriosEnPPD <-grep("^GIODIR\\.",dir(),value=TRUE)

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
DirectoriosEnPPD <-grep("^GIODIR\\.",dir(),value=TRUE) #Actualizamos

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

#Limpiamos memoria
rm(list=ls())
gc()

#Juntamos todas las imagenes procesadas y las escribimos en una tabla en 
#Pre-processed Data 2.

setwd("~/Thesis Project/Data/Pre-processed Data 2")
existe <-FALSE
if(file.exists("MatrizAmbientalGIO.csv") == TRUE){
    existe <-TRUE
    procesadas <-read.table("procesadas.txt",header=TRUE)
    procesadas <-apply(procesadas,1,function(x){x})
}

setwd("~/Thesis Project/Data/Pre-processed Data")

#Buscamos las tablas GIO individuales y creamos la matriz ambiental con ellas.
procesar1 <-grep("^GIO\\.",dir(),value=TRUE)

if(existe == TRUE){
    procesar1 <- procesar1[!(procesar1 %in% procesadas)]
}

if(length(procesar1)!=0){
    cat("Uniendo las tablas GIO individuales generadas a partir de las imagenes...\n")
    cat("\n")
    matAmb <-read.csv(procesar1[1])
    names(matAmb)[3]<-strsplit(procesar1[1],"\\.")[[1]][2]
    matAmb$clave <-paste(matAmb$latitude,matAmb$longitude,sep="_")
    matAmb <-matAmb[,-c(1,2)]
    if(length(procesar1)>1){
        for (i in 2 :length(procesar1)){
            df <-read.csv(procesar1[i])
            names(df)[3] <-strsplit(procesar1[i],"\\.")[[1]][2]
            df$clave <-paste(df$latitude,df$longitude,sep="_")
            df <-df[,-c(1,2)]
            matAmb <-merge(matAmb,df,by="clave",all=TRUE)
        }  
    }
    matAmb <-separate(matAmb,col=clave,into=c("latitude","longitude"),sep="_")
}
#BUscamos las series de tiempo que vienen en animaciones y las compactamos en 
#una sola tabla.
procesar2 <-grep("^GIODIR\\.",dir(),value=TRUE)

if(existe == TRUE){
    procesar2 <- procesar2[!(procesar2 %in% procesadas)]
}

if(length(procesar2) != 0){
    cat("Compactando las series temporales que se generaron con las imagenes ZIP...\n")
    cat("\n")
    for(i in 1:length(procesar2)){   #Procesamos cada carpeta individualmente
        setwd(paste0("~/Thesis Project/Data/Pre-processed Data/",procesar2[i]))
        if(length(dir()) != 0){
            df <-read.csv(dir()[1])
        }
        if(length(dir()) > 1){      #Juntamos serie temporal.
            for(j in 2:length(dir())){
               aux <- read.csv(dir()[j])
               df <-rbind(df,aux)
            }
            rm(aux)
            gc()
        }
        #Unimos las variables por latitud-longitud.Extraemos resumen estadistico
        #del conjunto.
        df$clave <-as.factor(paste(df$latitude,df$longitude,sep="_"))
        df$medida <-as.numeric(as.character(df$medida))
        df <-df[,-c(1,2)]
        df <-df %>% group_by(clave) %>% 
            summarize(Min = min(medida, na.rm=TRUE),
                      Q1 = quantile(medida,probs=c(0.25), na.rm=TRUE),
                      Median = median(medida, na.rm=TRUE),
                      Mean = mean(medida, na.rm=TRUE),
                      Q3 = quantile(medida,probs=c(0.75), na.rm=TRUE),
                      Max = max(medida, na.rm=TRUE),
                      Sd = sd(medida, na.rm=TRUE),
                      Kurtosis = kurtosis(medida, na.rm=TRUE),
                      Skewness = skewness(medida, na.rm=TRUE))
        #Renombramos
        nombres <-strsplit(procesar2[i],"\\.")[[1]][2]
        nombres <-paste(strsplit(nombres,"_")[[1]][-1],collapse=".")
        names(df)[-1] <-paste(nombres,names(df)[-1],sep=".")
        #xportamos a una tabla   
        setwd("~/Thesis Project/Data/Pre-processed Data")
        write.csv(df,file = paste0("GIODIRproc.",
                    paste(strsplit(procesar2[i],"\\.")[[1]][-1],collapse=".")
                    ,".csv"),row.names = FALSE)
    }
}
#Buscamos las tablas procesadas de las series de tiempo y las unimos.
setwd("~/Thesis Project/Data/Pre-processed Data")
procesar3 <-grep("^GIODIRproc\\.",dir(),value=TRUE)

if(existe == TRUE){
    procesar3 <- procesar3[!(procesar3 %in% procesadas)]
}

if(length(procesar3) != 0){
    cat("Uniendo el conjunto completo de tablas del Pre-processed Data...\n")
    cat("\n")
    matAmb$clave <-paste(matAmb$latitude,matAmb$longitude,sep="_")
    matAmb <-matAmb[,-c(1,2)]
    for(i in 1:length(procesar3)){
        df <-read.csv(procesar3[i])
        matAmb <-merge(matAmb,df,by="clave",all=TRUE)
    }
    matAmb <-separate(matAmb,col=clave,
                      into=c("latitude","longitude"),sep="_")
}
#Escribimos la matriz ambiental terminada en Pre-processed Data2
setwd("~/Thesis Project/Data/Pre-processed Data 2")

if(existe == TRUE){
    #Vemos si se proceso alguna tabla.
    nuevasProcesadas <-c(procesar1,procesar2,procesar3)
    if(length(nuevasProcesadas) != 0){
        procesadas <-c(procesadas,nuevasProcesadas)
        write.table(procesadas,file="procesadas.txt",row.names = FALSE)
        
        #Importamos la matriz ambiental original y la preparamos para el merge.
        matAmbOriginal <-read.csv("MatrizAmbientalGIO.csv")
        matAmbOriginal$clave <-
            paste(matAmbOriginal$latitude,matAmbOriginal$longitude,sep="_")
        matAmbOriginal <-matAmbOriginal[,-c(1,2)]
        
        #Preparamos para el merge la matriz recien procesada.
        matAmb$clave <-paste(matAmb$latitude,matAmb$longitude,sep="_")
        matAmb <-matAmb[,-c(1,2)]
        
        #Hacemos el merge.
        matAmbOriginal <-merge(matAmbOriginal,matAmb,by="clave",all=TRUE)
        matAmbOriginal <-separate(matAmbOriginal,col=clave,
                          into=c("latitude","longitude"),sep="_")
        
        #Reescribimos la matriz ambiental.
        write.csv(matAmbOriginal,file="MatrizAmbientalGIO.csv",row.names = FALSE)
    }
}else{
    #Exportamos la tabla de procesadas y la matriz ambiental si hubo 
    #procesamiento de datos.
    procesadas<-c(procesar1,procesar2,procesar3)
    if(length(procesadas) != 0){
        write.table(procesadas,file="procesadas.txt",row.names = FALSE)
        write.csv(matAmb,file="MatrizAmbientalGIO.csv",row.names = FALSE)    
    }
}

cat("Procedimiento Processing-GIOVANNI.R finalizado.\n")
cat("\n")
cat("/////////////////////////////////////////////////\n")
cat("\n")
