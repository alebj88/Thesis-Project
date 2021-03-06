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
library(ff)
library(ffbase)

setwd(paste0(d,"/R-Scripts"))
source('Functions.R')

#Creamos el modelo de reconocimiento de imagenes y lo almacenamos en cache.
setwd(paste0(d,"/R-Objects"))
if("interpretador.RData" %in% dir() == TRUE){
    load("interpretador.RData")           
}else{
    setwd(paste0(d,"/R-Objects/Train Inter"))
    entrenador<-read.table("entrenador.txt",header = TRUE)
    modFit<-interBuilder(entrenador)    
    setwd(paste0(d,"/R-Objects"))
    save(modFit,file = "interpretador.RData")
}

#Vemos cuales imagenes de la carpeta Giovanni ya fueron procesadas.
setwd(paste0(d,"/Data Preprocesada"))
processed <-grep("^GIO\\.",dir(),value=TRUE)

if(length(processed)!=0){
    processed <-paste0(sapply(strsplit(processed,split="\\."),
                             function(x){x[2]}),".png")
}
#Vemos cuales directorios de Giovanni ya fueron creados en Pre-processed Data.
setwd(paste0(d,"/Data Preprocesada"))
DirectoriosEnPPD <-grep("^GIODIR\\.",dir(),value=TRUE)

if(length(DirectoriosEnPPD)!=0){
    DirectoriosEnPPD <-paste0(sapply(strsplit(DirectoriosEnPPD,split="\\."),
                             function(x){x[2]}))
}
#Buscamos las imagenes de la carpeta Giovanni que no han sido procesadas. Estas 
#imagenes fueron colocadas en la carpeta por el usuario luego de la ejecucion 
#del programa.
setwd(paste0(d,"/Mapas"))
arch <-grep("png$",dir(),value=TRUE)

if(length(processed)==0){
    procesar <-arch
}else{
    procesar <-arch[!(arch %in% processed)]
}

#A cada directorio incluido por el usuario en la carpeta Giovanni le creamos
#un directorio en Pre-processed Data.
setwd(paste0(d,"/Mapas"))
DirectoriosEnGIO <-dir()[!(dir() %in% c(arch,"Ctrl"))]
DirectoriosFalt <- DirectoriosEnGIO[!(DirectoriosEnGIO %in% DirectoriosEnPPD)]

if(length(DirectoriosFalt)!=0){
    for(i in 1:length(DirectoriosFalt)){
        dir.create(file.path(paste0(d,"/Data Preprocesada"),
                           paste0("GIODIR.",DirectoriosFalt[i])),recursive=TRUE)
    }
}
setwd(paste0(d,"/Data Preprocesada"))
DirectoriosEnPPD <-grep("^GIODIR\\.",dir(),value=TRUE) #Actualizamos

#Procesamos solo las imegenes nuevas. Se crearan csv's para cada una de ellas en
#Pre-processed Data.
setwd(paste0(d,"/Mapas"))
if(length(procesar) != 0){
    #Preguntamos si se desea evaluar el interpretador de imagenes o no.
    userInput <-FALSE
    while(userInput == FALSE){
        message("Indique si desea evaluar los numeros indentificados por el")
        message("interpretador de imagenes.")
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
        #Mapa Alternativo.
        if(alternativo == FALSE){
            procesamientoDeImagenes(procesar[i],HT=humanT) 
        }else{
            procesamientoDeImagenes(procesar[i],HT=humanT,mapCenter=mapCenter)  
        }
    }
}else{
    message("No hay imagenes individuales que procesar.")   
    cat("\n")
}

#Recorremos los directorios de Giovanni escritos en Pre-processed Data y vemos
#Que imagenes faltan por procesar en cada uno de ellos.
if(length(DirectoriosEnPPD) != 0){
    Directorios <-paste0(
        sapply(strsplit(DirectoriosEnPPD,split="\\."),function(x){x[2]}))
    
    for (i in 1:length(Directorios)){
        pathGIO <-paste0(
            paste0(d,"/Mapas/"),Directorios[i])
        pathPPD <-paste0(paste0(d,"/Data Preprocesada/"),
                         paste0("GIODIR.",Directorios[i]))
        #Vemos cuales imagenes estan en el directorio alojado en Giovanni 
        #pero no en el que esta en Pre-Processed Data.
        setwd(pathGIO)
        
        #Eliminamos archivos basura
        if("Thumbs.db" %in% dir()){
            file.remove("Thumbs.db")
        }
        
        imagenesGIO <-sapply(strsplit(dir(),split="\\."),function(x){x[1]})
        setwd(pathPPD)
        imagenesPPD <-sapply(strsplit(dir(),split="\\."),function(x){x[1]})
        
        procesarImagDIR <-imagenesGIO[!(imagenesGIO %in% imagenesPPD)]
        
        if(length(procesarImagDIR) != 0){
            message("Procesando el directorio '",Directorios[i],"'...\n")
            
            #Preguntamos si se desea evaluar el interpretadoe de imagenes o no.
            userInput <-FALSE
            while(userInput == FALSE){
                message("Indique si desea evaluar los numeros indentificados")
                message("por el interpretador de imagenes.")
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
            nSet <-FALSE        #Solo ejecutamos el interpretador una vez.
            for (j in 1:length(procesarImagDIR)){
                setwd(pathGIO)
                message("Procesando la imagen '",procesarImagDIR[j],"'...")
                #Mapa Alternativo.
                if(alternativo == FALSE){
                    procesamientoDeImagenes(
                        procesarImagDIR[j],newDir=TRUE,pathPPD,HT=humanT,nSet) 
                }else{
                    procesamientoDeImagenes(
                        procesarImagDIR[j],newDir=TRUE,pathPPD,HT=humanT,nSet,
                        mapCenter=mapCenter)  
                }
                humanT <-FALSE  #No volvemos a evaluar los numeros.
                nSet <-TRUE     #Usamos los numeros interpretados para el resto.
            }
        }
    }
}else{
    message("No hay directorios nuevos que procesar.")   
    cat("\n")
}

#/////////////////////////////////////////////////////////////////////////
#Juntamos todas las imagenes procesadas y las escribimos en una tabla en 
#Datos Finales.

setwd(paste0(d,"/Datos Finales"))
existe <-FALSE

if(file.exists("MatrizAmbientalGIO.csv") == TRUE){
    existe <-TRUE
    procesadas <-read.table("procesadas.txt",header=FALSE)
    procesadas <-apply(procesadas,1,function(x){x})
}

setwd(paste0(d,"/Data Preprocesada"))

#Buscamos las tablas GIO individuales y creamos la matriz ambiental con ellas.
procesar1 <-grep("^GIO\\.",dir(),value=TRUE)

if(existe == TRUE){
    procesar1 <- procesar1[!(procesar1 %in% procesadas)]
}

if(length(procesar1) != 0){
    cat("Juntando las tablas GIO que se generaron a partir de imagenes \n")
    cat("individuales...\n")
    cat("\n")
    matAmb <-read.csv(procesar1[1])

    #Reemplazamos "medida" por el nombre real de la variable
    names(matAmb)[3]<-strsplit(procesar1[1],"\\.")[[1]][2]
    #Preparamos para el merge...
    matAmb$clave <-paste(matAmb$latitude,matAmb$longitude,sep="_")
    matAmb <-matAmb[,-which(names(matAmb) %in% c("latitude","longitude"))]
    
    #Unimos las claves repetidas. #En teoria no entra mas.
    if(length(unique(matAmb$clave)) != nrow(matAmb)){
        stop("Se generaron claves repetidas en matAmb")
    }
    
    if(length(procesar1) > 1){
        for (i in 2 :length(procesar1)){
            df <-read.csv(procesar1[i])

            #Reemplazamos "medida" por el nombre real de la variable
            names(df)[3] <-strsplit(procesar1[i],"\\.")[[1]][2]
            #Preparamos para el merge...
            df$clave <-paste(df$latitude,df$longitude,sep="_")
            df <-df[,-which(names(df) %in% c("latitude","longitude"))]
            
            #Unimos las claves repetidas.
            if(length(unique(df$clave)) != nrow(df)){
                stop("Se generaron claves repetidas en un df de matAmb")
            }
            #Merge
            matAmb <-merge(matAmb,df,by="clave",all=TRUE)
        }  
    }
    matAmb <-separate(matAmb,col=clave,into=c("latitude","longitude"),sep="_")
    #Formateamos
    matAmb$latitude <- as.numeric(as.character(matAmb$latitude))
    matAmb$longitude <- as.numeric(as.character(matAmb$longitude))
}
#BUscamos las series de tiempo que vienen en animaciones y las compactamos en 
#una sola tabla.
procesar2 <-grep("^GIODIR\\.",dir(),value=TRUE)

if(existe == TRUE){
    procesar2 <- procesar2[!(procesar2 %in% procesadas)]
}

if(length(procesar2) != 0){
    cat("Generando tabla de resumen para cada serie temporal (ZIP's).\n")
    cat("\n")
    #Importamos el limpiador
    setwd(paste0(d,"/Data Preprocesada"))
    cleaner <- read.csv("GiovanniCleaner.csv")
    for(i in 1:length(procesar2)){     #Procesamos cada carpeta individualmente
        
        #Check para saltar iteracion.
        check <- paste0("GIODIRproc.",paste(
            strsplit(procesar2[i],"\\.")[[1]][-1],collapse="."),".csv")
        if(check %in% dir() == TRUE){
            next    
        }
        
        setwd(paste0(
            paste0(d,"/Data Preprocesada/"),procesar2[i]))
        
        message("Procesando al directorio ",procesar2[i])
        cat(length(dir())," archivos en total...\n")
        cat("\n")
        #Check Rapido
        if(length(dir()) == 0){
            stop("El directorio esta vacio. Remuevalo.")
        }
        #Leemos el primer archivo del directorio.
        df <- read.csv(dir()[1])
        df <- as.ffdf(df)
        
        if(length(dir()) > 1){                         #Juntamos serie temporal.
            for(j in 2:length(dir())){
               aux <- read.csv(dir()[j])
               aux <- as.ffdf(aux)
               df <- ffdfappend(df,aux,adjustvmode=TRUE)
            }
            rm(aux)
            gc()
        }
        #Unimos las variables por latitud-longitud.Extraemos resumen estadistico
        #del conjunto.
        
        #////////////////////////////////////////////////////////
        if(nrow(df) > 3000000){ 
            #Creamos df$clave para df's bigdata
            message("Alto volumen de datos. Manipulando ",nrow(df)," filas...")
            NroSecc <- nrow(df) %/% 100000
            extra <- nrow(df) %% 100000
            
            #Creamos el df "clv" que contiene el primer subconjunto de claves.
            #Guardamos ese df como ff.df en aux1.
            posInf <- 1
            posSup <- 100000
            clv <- data.frame(clave = as.factor(paste(
                        df$latitude[posInf:posSup],
                        df$longitude[posInf:posSup],sep="_")))
            aux1 <-as.ffdf(clv)
            
            #Anexamos el conjunto de claves a aux1 por medio del ciclo.
            for(p in 2:NroSecc){
                posInf <- (p-1)*100000 +1
                posSup <- p*100000
                clv <- as.ffdf(data.frame(clave = as.factor(paste(
                            df$latitude[posInf:posSup],
                            df$longitude[posInf:posSup],sep="_"))))
                aux1 <-ffdfappend(aux1,clv,adjustvmode=TRUE)
            }  
            
            if(extra != 0){
                posInf <- (NroSecc)*100000 +1
                posSup <- (NroSecc)*100000 +extra
                clv <- as.ffdf(data.frame(clave = as.factor(paste(
                            df$latitude[posInf:posSup],
                            df$longitude[posInf:posSup],sep="_"))))
                aux1 <-ffdfappend(aux1,clv,adjustvmode=TRUE)
            }
            df <- cbind.ffdf2(df,aux1)
            
            rm(aux1);rm(clv);rm(posInf);rm(posSup);rm(extra);rm(NroSecc)
            gc()
        }else{
            #Creamos df$clave para df's NO bigdata
            df <- as.data.frame(df)
            df$clave <-as.factor(paste(df$latitude,df$longitude,sep="_"))  
        }
        #/////////////////////////////////////////////////////////////
        #RESUMEN ESTADISTICO PARA EL DF QUE CONTIENE TODOS LOS CSV'S DEL
        #DIRECTORIO UNIDOS POR "RBIND".
        
        if(nrow(df) > 3000000){ 
          df <- as.ffdf(df)
          df$latitude <- NULL
          df$longitude <- NULL
          #df <- as.ffdf(df[,-which(names(df) %in% c("latitude","longitude"))])
          df <- as.ffdf(transform(df,medida = as.numeric(as.character(medida))))
        }else{
          df <- df[,-which(names(df) %in% c("latitude","longitude"))]
          df$medida <- as.numeric(as.character(df$medida))
        }
        


        #Check rapido
        if(identical(names(df),c("medida","clave")) == FALSE){
            stop("El data frame df llego desordenado a summarize")
        }
        
        #Eliminamos NA's para evitar inconsistencias en summarize
        if(nrow(df) >= 10000000){
            message("Volumen excesivo. Procesando en disco...")

            #Big data.
            df <- as.ffdf(df)
            idx <- ffwhich(df,!is.na(medida))
            
            #Check
            if(!is.null(idx)){
                df <- df[idx, ]
            }
            
            #Limpiamos memoria
            rm(idx)
            gc()
            
        }else{
            df <- df[!is.na(df$medida),]
        }
        #Eliminamos nombrtes de fila.
        rownames(df) <- NULL
        
        #Precondicion
        if(class(df) == "ffdf"){
            #///////////////////////////////////////////////////////////////////
            #///////////////////////////////////////////////////////////////////
            # if(nrow(df) >= 100000000){     #Comentar esto#Comentar esto#Comentar 
            #     elim <- read.csv( ##############################################
            #     paste0(d,"/Data Preprocesada/clavesExtra1.csv")  
            #     idx <- ffwhich(df,!(clave %in% elim[,1]))        #Comentar esto
            #     rm(elim)
            #     gc()
            #     df <- df[idx, ]  #Comentar esto#Comentar esto#Comentar esto#Come
            #     rm(idx)
            #     gc()
            #     elim <- read.csv( ##############################################
            #     paste0(d,"/Data Preprocesada/clavesExtra2.csv")  
            #     idx <- ffwhich(df,!(clave %in% elim[,1]))        #Comentar esto
            #     rm(elim)
            #     gc()
            #     df <- df[idx, ]  #Comentar esto#Comentar esto#Comentar esto#Come 
            #     rm(idx)
            #     gc()
            #     elim <- read.csv( ##############################################
            #     paste0(d,"/Data Preprocesada/clavesExtra3.csv")  
            #     idx <- ffwhich(df,!(clave %in% elim[,1]))        #Comentar esto 
            #     rm(elim)
            #     gc()
            #     df <- df[idx, ]  #Comentar esto#Comentar esto#Comentar esto#Come
            #     rm(idx)
            #     gc()
            #     elim <- read.csv( ##############################################
            #     paste0(d,"/Data Preprocesada/clavesExtra4.csv")  
            #     idx <- ffwhich(df,!(clave %in% elim[,1]))        #Comentar esto 
            #     rm(elim)
            #     gc()
            #     df <- df[idx, ]  #Comentar esto#Comentar esto#Comentar esto#Come
            #     rm(idx)
            #     gc()
            # }   #Comentar esto#Comentar esto#Comentar esto#Comentar esto#Comenta
            #///////////////////////////////////////////////////////////////////
            #///////////////////////////////////////////////////////////////////
            df <- as.data.frame(df)
        }
        
        #Unimos por geolocalizacion usando resumen estadistico.
        cat("Uniendo datos por geolocalizacion.\n")
        cat("Calculando resumen estadistico de la variable medida...\n")
        cat("\n")
        df <- df %>% group_by(clave) %>% 
            summarize(Min = min(medida, na.rm=TRUE),
                      Q1 = quantile(medida,probs=c(0.25), na.rm=TRUE),
                      Median = median(medida, na.rm=TRUE),
                      Mean = mean(medida, na.rm=TRUE),
                      Q3 = quantile(medida,probs=c(0.75), na.rm=TRUE),
                      Max = max(medida, na.rm=TRUE),
                      Sd = sd(medida, na.rm=TRUE),
                      Kurtosis = kurtosis(medida, na.rm=TRUE),
                      Skewness = skewness(medida, na.rm=TRUE))
        
        #Check rapido. Medida debio desaparecer.
        if(identical(names(df)[1:3],c("clave","Min","Q1")) == FALSE){
            stop("El data frame df salio desordenado de summarize")
        }
        #Renombramos
        nombres <-strsplit(procesar2[i],"\\.")[[1]][2]       #Quitamos la clave.
        names(df)[-1] <-paste(nombres,names(df)[-1],sep=".")
        
        #Exportamos   
        #YA VIENEN PREPARADOS PARA EL MERGE.
        setwd(paste0(d,"/Data Preprocesada"))
        #Borramos datos manchados solo cuando son de la region establecida por 
        #defecto.
        if(alternativo == FALSE){
            df <- df[!(df$clave %in% cleaner$clave),]  
        }
        write.csv(df,file = paste0("GIODIRproc.",
                    paste(strsplit(procesar2[i],"\\.")[[1]][-1],collapse=".")
                    ,".csv"),row.names = FALSE)
        message("Directorio ",procesar2[i]," procesado exitosamente.")
        rm(df)
        gc()
    }
}
#Buscamos las tablas procesadas de las series de tiempo y las unimos.
setwd(paste0(d,"/Data Preprocesada"))
procesar3 <-grep("^GIODIRproc\\.",dir(),value=TRUE)

if(existe == TRUE){
    procesar3 <- procesar3[!(procesar3 %in% procesadas)]
}

if(length(procesar3) != 0){
    #si no hay datos procesados previamente creamos el archivo matAmb con 
    #estos datos.
    cat("Uniendo las tablas de las series temporales ingresadas...\n")
    
    if(("matAmb" %in% ls()) == FALSE){
        cat("Solo se detectaron datos provenientes de animaciones.\n")
        cat("\n")
        matAmb <-read.csv(procesar3[1])     #VIENEN PREPARADOS PARA EL MERGE.
        if(length(procesar3) > 1){
            for(i in 2:length(procesar3)){
                df <-read.csv(procesar3[i]) #VIENEN PREPARADOS PARA EL MERGE.
                matAmb <-merge(matAmb,df,by="clave",all=TRUE)
            }
        }
    #Si hay datos previamente los unimos.
    }else{
        cat("Se detectaron datos de animaciones y de otros tipos de mapa.\n")
        cat("\n")
        matAmb$clave <-paste(matAmb$latitude,matAmb$longitude,sep="_")
        matAmb <-matAmb[,-which(names(matAmb) %in% c("latitude","longitude"))]
        #matAmb <- as.ffdf(matAmb)
        for(i in 1:length(procesar3)){
            df <-read.csv(procesar3[i])     #VIENEN PREPARADOS PARA EL MERGE.
            #df <- as.ffdf(df)
            #matAmb <-merge.ffdf.all(matAmb,df,by="clave")
            matAmb <-merge(matAmb,df,by="clave",all=TRUE)
        }
    }

    #Check
    if(length(unique(matAmb$clave)) != nrow(matAmb)){
        stop("La matriz ambiental no tiene claves unicas.")
    }
    matAmb <-separate(matAmb,col=clave,
                      into=c("latitude","longitude"),sep="_") 
    #Formateamos
    matAmb$latitude <- as.numeric(as.character(matAmb$latitude))
    matAmb$longitude <- as.numeric(as.character(matAmb$longitude))
}
#//////////////////////////////////////////////////////////////
#Escribimos la matriz ambiental terminada en Pre-processed Data2
setwd(paste0(d,"/Datos Finales"))

if(existe == TRUE){
    #Vemos si se proceso alguna tabla.
    nuevasProcesadas <-c(procesar1,procesar2,procesar3)
    if(length(nuevasProcesadas) != 0){
        cat("Agregando datos nuevos a la matriz ambiental ya creada...\n") 
        
        #Importamos la matriz ambiental original y la preparamos para el merge.
        matAmbOriginal <-read.csv("MatrizAmbientalGIO.csv")
        matAmbOriginal$clave <-
            paste(matAmbOriginal$latitude,matAmbOriginal$longitude,sep="_")
        matAmbOriginal <-matAmbOriginal[,
                   -which(names(matAmbOriginal) %in% c("latitude","longitude"))]
        matAmbOriginal <-df.as.factor(matAmbOriginal)   #Pasamos a ff
        matAmbOriginal <-as.ffdf(matAmbOriginal)        #Pasamos a ff
        
        #Preparamos para el merge la tabla recien procesada.
        matAmb$clave <-paste(matAmb$latitude,matAmb$longitude,sep="_")
        matAmb <-matAmb[,-which(names(matAmb) %in% c("latitude","longitude"))]
        matAmb <-df.as.factor(matAmb)                   #Pasamos a ff
        matAmb <-as.ffdf(matAmb)                        #Pasamos a ff
        
        #Hacemos el merge.
        #@matAmbOriginal <-merge(matAmbOriginal,matAmb,by="clave",all=TRUE)
        matAmbOriginal <-merge.ffdf.all(matAmbOriginal,matAmb,by="clave")   #ff
        matAmbOriginal <-as.data.frame(matAmbOriginal)
        
        #Check
        if(length(unique(matAmbOriginal$clave)) != nrow(matAmbOriginal)){
            stop("La matriz ambiental no tiene claves unicas.")
        }
        #Eliminamos la clave.
        matAmbOriginal <-separate(matAmbOriginal,col=clave,
                          into=c("latitude","longitude"),sep="_")
        
        #Reescribimos la matriz ambiental.
        write.csv(matAmbOriginal,file="MatrizAmbientalGIO.csv",row.names =FALSE)
        message("Matriz ambiental actualizada exitosamente.\n")
        
        #Actualizamos tabla de procesadas.
        procesadas <-c(procesadas,nuevasProcesadas)
        write.table(procesadas,file="procesadas.txt",
                    row.names = FALSE,col.names = FALSE)
    }
}else{
    #Exportamos la tabla de procesadas y la matriz ambiental si hubo 
    #procesamiento de datos.
    procesadas<-c(procesar1,procesar2,procesar3)
    
    if(length(procesadas) != 0){
        cat("Creando matriz ambiental en el directorio Datos Finales...\n")
        write.csv(matAmb,file="MatrizAmbientalGIO.csv",row.names = FALSE) 
        message("Matriz ambiental creada exitosamente.\n")
        
        #Creamos tabla de procesadas.
        write.table(procesadas,file="procesadas.txt",row.names = FALSE,
                    col.names = FALSE)
    }
}

borrarList <- ls()[!("d" %in% ls())]
rm(list=borrarList)
gc()

cat("Procedimiento Processing-GIOVANNI.R finalizado.\n")
cat("\n")
cat("/////////////////////////////////////////////////\n")
cat("\n")
