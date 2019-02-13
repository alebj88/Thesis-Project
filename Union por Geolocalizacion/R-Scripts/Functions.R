#FUNCIONES EMPLEADAS EN EL PROCESAMIENTO DE DATOS.

#/////////////////////////////////////////////////
#Cbind para data frames ff.
cbind.ffdf2 <- function(d1, d2){
    D1names <- colnames(d1)
    D2names <- colnames(d2)
    mergeCall <- do.call("ffdf", c(physical(d1), physical(d2)))
    colnames(mergeCall) <- c(D1names, D2names)
    mergeCall
}


#/////////////////////////////////////////////////
#Data frames con columnas de caracteres as.factor
df.as.factor<-function(df){
    pos<-which(unname(sapply(df,class))=="character")
    if(length(pos)!=0){
        for (i in 1:length(pos)){
            df[,pos[i]]<-as.factor(df[,pos[i]])
        }
    }
    return(df)
}

#/////////////////////////////////////////////////
#Merge ffdf all=TRUE
merge.ffdf.all<-function(x,y,by=intersect(names(x),names(y)),by.x=by,by.y=by){
    #Hacemos merges ff's por parte.
    aux1<-merge.ffdf(x,y,by.x,by.y,all.x=TRUE)
    aux2<-merge.ffdf(y,x,by.x,by.y,all.x=TRUE)
    #Transformamos en data frames estandar y eliminamos filas repetidas
    aux2<-aux2[,names(aux1)]
    aux1<-aux1[,names(aux1)]
    aux2<-aux2[!(aux2$clave %in% aux1$clave),]
    #Transformamos en data frames ff nuevamente y hacemos 'rbind'.
    aux1<-as.ffdf(aux1)
    #Check
    if(nrow(aux2) != 0){
        aux2 <- as.ffdf(aux2)
        df3 <- ffdfappend(aux1,aux2)
    }else{
        df3 <- aux1
    }
    #Exportamos
    return(df3)
}

#/////////////////////////////////////////////////
#Calculo de distancia entre puntos de geolocalizacion en kilometros.   
#p1=(lat,lon) p2=(lat,lon)
distGL <-function(p1,p2,r=6371){
    lat1 <-abs(p1[1])/180
    lat2 <-abs(p2[1])/180
    lon1 <-abs(p1[2])/180
    lon2 <-abs(p2[2])/180
    u <-c(r*cospi(lat1)*sinpi(lon1),r*cospi(lat1)*cospi(lon1),r*sinpi(lat1))
    v <-c(r*cospi(lat2)*sinpi(lon2),r*cospi(lat2)*cospi(lon2),r*sinpi(lat2))
    value <-as.numeric((u %*% v)/(sqrt(u %*% u)*sqrt(v %*% v)))
    if(abs(value - 1) < 0.00000000001){ #Esto es un Bug letal, un super bug.
        alp <-0
    }else{
        alp <-acos(value)
    }
    d <- r*alp
    return(d)
}   

#/////////////////////////////////////////////////
#Algoritmo para imputar datos por cercania geografica usando la mediana o la 
#media ponderada. Para datos catgoricos se usa la moda. 
#Recibe un data frame con lat, lon y la variable a imputar.
#Retorna un data frame con lat, lon y la imputada.
#Este data frame contendra solo las filas indicadas en "rango".
#El par lat-lon debe ser unico.
#Le pasamos latitude,longitude e impVar.
#Retorna un vector con impVar corregida.

impute.per.region <- function(RegionB,RegionC,vecinos,medida="median",
                              CLS="inter"){
    #Precondicion
    if(ncol(RegionB) != 3){
        stop("RegionB no tiene exactamente 3 variables.")
    }
    if(ncol(RegionC) != 3){
        stop("RegionC no tiene exactamente 3 variables.")
    }
    if(length(unique(
        paste(RegionB$latitude,RegionB$longitude))) != nrow(RegionB)){
        stop("RegionB tiene puntos de geolocalizacion duplicados.")
    }
    if(length(unique(
        paste(RegionC$latitude,RegionC$longitude))) != nrow(RegionC)){
        stop("RegionC tiene puntos de geolocalizacion duplicados.")
    }
    if(sum(is.na(RegionB$latitude)) != 0){
        stop("La variable 'latitude' tiene valores perdidos en RegionB")
    }
    if(sum(is.na(RegionB$longitude)) != 0){
        stop("La variable 'longitude' tiene valores perdidos en RegionB")
    }
    if(sum(is.na(RegionC$latitude)) != 0){
        stop("La variable 'latitude' tiene valores perdidos en RegionC")
    }
    if(sum(is.na(RegionC$longitude)) != 0){
        stop("La variable 'longitude' tiene valores perdidos en RegionC")
    }
    if(length(vecinos) != nrow(RegionC)){
        stop("length(vecinos) != nrow(RegionC)")
    }
    
    #Extraemos la dimension original del dataset.
    originalDim <-dim(RegionB)
    
    #Reordenamos los data frames ingresados y les damos formato.
    varName <- names(RegionB)[!(names(RegionB) %in% c("latitude","longitude"))]
    
    #Check
    if(identical(RegionB,c("latitude","longitude",varName)) != FALSE){
        stop("Los nombres de RegionB no son correctos")
    }
    if(identical(RegionC,c("latitude","longitude",varName)) != FALSE){
        stop("Los nombres de RegionC no son correctos")
    }
    if(sum(is.na(RegionB[varName][,1])) == nrow(RegionB)){
        print(c("La variable ",varName, " esta vacia"))
        stop("RegionB tiene al menos una variable sin datos.")
    }
    
    #Formateamos
    RegionC$latitude <- as.numeric(as.character(RegionC$latitude)) 
    RegionC$longitude<-as.numeric(as.character(RegionC$longitude)) 
    
    RegionB$latitude <- as.numeric(as.character(RegionB$latitude)) 
    RegionB$longitude<-as.numeric(as.character(RegionB$longitude)) 
    
    if(CLS !="inter"){
        if(CLS == "numeric" | CLS == "integer"){
            RegionB[,3] <- as.numeric(as.character(RegionB[,3]))
        }else{
            RegionB[,3] <- as.character(RegionB[,3])
        }
    }
    
    #Agregamos clave a RegionB
    RegionB$clave <-paste(RegionB$latitude,RegionB$longitude,sep="_")
    
    #Una vez localizados los puntos vecinos dentro del radio esperado imputamos.
    RegionC$sec <- seq(1:nrow(RegionC))

    #Procesamos
    correctedVar <-unname(apply(RegionC,1,function(y){
            y <- unname(y)
           
            if(is.na(y[3])){      #Intentamos imputar si es NA.
                #Tipo correcto de variable.
                y4 <- as.numeric(as.character(y[4]))
                #Si no tiene vecinos retornamos NA.
                if(any(is.na(vecinos[[y4]]))){
                    if(length(vecinos[[y4]]) != 1){
                        stop("Hay mas de un vecino nulo en la misma ubicacion en lista.")
                    }
                    return(y[3])            
                }
                #Buscamos los valores de correccion.
                ubic <- RegionB$clave %in% vecinos[[y4]]
                valores <- RegionB[ubic,3]

                #print(valores)
                #Si la variable a imputar es catgorica...........
                if(class(RegionB[,3]) !="numeric" & 
                                                class(RegionB[,3]) !="integer"){ 
                    #Eliminamos NA's
                    valores <- valores[!is.na(valores)]
                    
                    #Calculamos moda
                    if(length(valores) != 0){
                        t <- table(valores)
                        moda <- names(t[which(t == max(t))])
                        if(length(moda) > 1){
                            warning("Ambiguedad al imputar la variable categorica.
                                    La moda no es unica.",call.=FALSE)
                        }
                    }else{
                        moda <- NA
                    }
                    return(moda[1]) 
                }
                #Si la variable a imputar es numerica............
                if(medida == "median"){                      #Mediana.
                    #Eliminamos NA's
                    valores <- valores[!is.na(valores)]
                    
                    #Calculamos mediana
                    if(length(valores) != 0){
                        y[3] <- median(valores)
                        
                        #Check
                        if(is.na(y[3])){
                            stop("Se generaron NA's para la mediana")
                        }
                        
                    }else{
                        y[3] <- NA
                    }
                    
                }else if(medida == "mean"){                  #Media ponderada.
                    #Geolocalizacion del NA.
                    latNA <- as.numeric(as.character(RegionC$latitude[y4]))
                    lonNA <- as.numeric(as.character(RegionC$longitude[y4]))
                    
                    #Geolocalizacion de los vecinos del NA en RegionB.
                    geoVeci <- RegionB[ubic,c(1,2,3)]
                    geoVeci <- geoVeci[!is.na(geoVeci[3][,1]),]
                    geoVeci <- geoVeci[c(1,2)]
                    
                    #Check
                    if(identical(names(geoVeci),c("latitude","longitude"))== F){
                        stop("geoVeci tiene los nombres incorrectos")
                    }
                    geoVeci[,1] <- as.numeric(as.character(geoVeci[,1]))
                    geoVeci[,2] <- as.numeric(as.character(geoVeci[,2]))
                    
                    #Calculamos conjunto de distancias.
                    dis <-apply(geoVeci,1,function(w){
                        d <- distGL(c(w[1],w[2]),c(latNA,lonNA))
                        return(d)
                    })
                    #Check
                    if(class(dis) != "numeric"){
                        stop("La clase de 'dis' no es numerica.")
                    }
                    if(any(is.na(dis))){
                        stop("Se generaron NA's en el calculo de dis.")
                    }
                    
                    #Eliminamos NA's
                    valores <- valores[!is.na(valores)]
                   
                    #Check
                    if(length(valores) != length(dis)){
                        stop("length(valores) != length(dis)")
                    }
                    
                    #Calculamos mediana
                    if(length(valores) != 0){
                        #Reordenamos de mayor distancia a menor
                        valores <-valores[order(dis,decreasing = T)]
                        dis <-sort(dis,decreasing = T)
                        
                        #Generamos pesos
                        maxi <- max(dis) + 1
                        total <- sum(abs(maxi-dis))
                        pesos <- abs(maxi-dis)/total
                        if(abs(sum(pesos)-1) > 0.000001){
                            stop("La suma de los pesos no es 1.")
                        }
    
                        #Creamos media ponderada.
                        y[3] <-round(sum(pesos*valores),4)
                        
                        #Check
                        if(is.na(y[3])){
                            stop("Se generaron NA's para la media")
                        }
                    }else{
                        y[3] <- NA
                    }
                    
                }else{
                    stop("La medida que se solicito es invalida.")
                }
                return(y[3]) 
            }else{
                return(y[3])     #Si no es NA no imputamos.
            }
    }))
    #Check
    if(length(correctedVar) != nrow(RegionC)){
        stop("La variable correctedVar no tiene la dimension esperada.")
    }
    
    #Damos formato de salida a la variable creada.
    if(class(RegionB[,3]) %in% c("factor","character","logical")){
        correctedVar <- as.factor(correctedVar)

    }else if(class(RegionB[,3]) %in% c("integer","numeric")){
        correctedVar <- as.numeric(as.character(correctedVar))

    }else{
        stop("Clase de variable no definida.")
    }

    #Exportamos vector corregido
    return(correctedVar)
}

#/////////////////////////////////////////////////
#Recibe un dataframe al q se le quieren agregar variables con valores imputados
#"dat" y otro donde estan las variables que se desean anexar "imputador". Estos
#datos estan ausentes porque los puntos de geolocalizacion no coinciden.
#LAS VARIABLES DEL IMPUTADOR NO EXISTEN EN DAT SALVO LAT Y LON.
#EL IMPUTADOR DEBE TENER UN UNICO VALOR PARA CADA TRIO LAT-LON-DEPTH.
###CORREGIR SECCION DE PROFUNDIDADES
impute.per.region2 <- function(dat,imputador,radio=c(1,2,5,10),medida="median",
                                                        Google=FALSE){
    #Data original
    datos <- dat
    dat <- dat[c("latitude","longitude")]

    #Check.Precondicion sobre el dataset imputador.
    if("depth.m" %in% names(imputador) == TRUE){
        claves <- unique(paste0(
            imputador$latitude,imputador$longitude,imputador$depth.m))
        if(length(claves) != nrow(imputador)){
            stop("El imputador tiene multiples datos por lat-lon-depth.")
        }
        rm(claves)
        gc()
    }else{
        #Las tablas de Google Earth se procesan por merge. Esta condicion no es 
        #necesaria
        if(Google == FALSE){
            claves <- unique(paste0(imputador$latitude,imputador$longitude))
            if(length(claves) != nrow(imputador)){
                stop("El imputador tiene multiples datos por lat-lon.")
            }
            rm(claves)
            gc()
        }
    }
    
    #Vemos si el imputador tiene valores de profundidad y completamos su data.
    if("depth.m" %in% names(imputador) == TRUE){
        loc <- c("latitude","longitude","depth.m")
        #variables de chequeo
        lenIni <- length(unique(paste0(imputador$latitude,imputador$longitude)))
        nVarIni <- length(names(imputador))
        
        #Claves
        profundidades <- sort(unique(imputador$depth.m))
        claveLL <- unique(paste(imputador$latitude,imputador$longitude,sep="_"))
        
        impAux <- data.frame(clave= rep(claveLL,each=length(profundidades)),
                             depth.m = rep(profundidades,times=length(claveLL)))
        impAux <- separate(
                        impAux,col=clave,into=c("latitude","longitude"),sep="_")
        #Claves
        impAux$clavePr <- paste(
            impAux$latitude,impAux$longitude,impAux$depth.m,sep="_")
        
        imputador$clavePr <- paste(
            imputador$latitude,imputador$longitude,imputador$depth.m,sep="_")
        #Merge
        imputador <- merge(impAux,imputador[
                !(names(imputador) %in% c("latitude","longitude","depth.m"))]
                                                       ,by="clavePr",all.x=T)
        imputador$clavePr <- NULL
        
        #Check
        if(nrow(imputador) != length(profundidades)*lenIni){
            stop("El imputador no recibio el numero de filas adecuado.")
        }
        if(length(names(imputador)) != nVarIni){
            stop("El imputador no recibio el numero de columnas adecuado.")
        }
        
        #Limpiamos
        rm(impAux);rm(claveLL);rm(lenIni);rm(nVarIni)
        gc()
        
    }else{
        loc <- c("latitude","longitude")
    }
    
    #Seleccionamoss la variables.
    imputedVars <- names(imputador)[!(names(imputador) %in% c(loc))]

    #La tabla 'dat' puede tener valores de geolocalizacion repetidos.
    #Procedemos a eliminarlos.
    dat <- dat %>% unique()
    
    if("depth.m" %in% loc == TRUE){
        #Agregamos la secuencia de profundidades a dat y creamos la clave.
        dat <-data.frame(latitude=rep(dat[,1],each=length(profundidades)),
                         longitude=rep(dat[,2],each=length(profundidades)),
                         depth.m=rep(profundidades,times=nrow(dat)))
        #Claves.
        dat$clave <- paste(dat$latitude,dat$longitude,dat$depth.m,sep="_")
        imputador$clave <- paste(
            imputador$latitude,imputador$longitude,imputador$depth.m,sep="_")
        #Merge.
        dat <- merge(
            dat,imputador[c(imputedVars,"clave")],by="clave",all.x=T)
        
        #Check.
        claveDatos <- unique(paste0(datos$latitude,datos$longitude))
        if(nrow(dat) != length(claveDatos)*length(profundidades)){
            stop("La tabla dat no recibio el numero de filas adecuado.")
        }
        
        #Limpiamos memoria.
        rm(claveDatos)
        gc()
        
    }else{
        #Claves
        dat$clave <- paste(dat$latitude,dat$longitude,sep="_")
        imputador$clave <- paste(imputador$latitude,imputador$longitude,sep="_")
        #Merge
        dat <- merge(dat,imputador[c(imputedVars,"clave")],by="clave",all.x=T)
    }
    
    #Eliminamos las filas del imputador que fueron agregadas a 'dat' con el 
    #merge solo para evaluar si el mismo quedo vacio. SOn datos utiles que no 
    #podemos eliminar.
    imputEval <- imputador[!(imputador$clave %in% dat$clave),]
    dat$clave <- NULL
    
    #Check sobre tablas de Google Earth.
    if(Google == TRUE & nrow(imputEval) != 0){
        stop("El imputador para datos de Google Earth tiene filas que no se procesan con merge.")
    }
    
    #Si el imputador quedo vacio, merge final y exportamos.
    if(nrow(imputEval) == 0){
        datos$clave <- paste(datos$latitude,datos$longitude)
        dat$clave <- paste(dat$latitude,dat$longitude)
        datos <- merge(datos,dat[!(names(dat) %in% c("latitude","longitude"))],
                       by="clave",all.x=T)
        datos$clave <- NULL
        
        return(datos)
    }

    #Reordenamos data frames y creamos RegionB y RegionC. Ambos dataframes 
    #tienen las variables clave, latitude, longitude, variables a corregir. 
    imputador <- imputador[,names(dat)]
    
    #Establecemos region de busqueda y region de correccion.
    RegionB <- imputador
    RegionC <- dat
    RegionC$clave <- NULL
    RegionB$clave <- NULL
    
    #Check
    if(names(RegionB)[1] != "latitude"){
        stop("RegionB tiene la latitud en la posicion incorrecta")
    }
    if(names(RegionB)[2] != "longitude"){
        stop("RegionB tiene la longitud en la posicion incorrecta")
    }
    if(names(RegionC)[1] != "latitude"){
        stop("RegionC tiene la latitud en la posicion incorrecta")
    }
    if(names(RegionC)[2] != "longitude"){
        stop("RegionC tiene la longitud en la posicion incorrecta")
    }
    
    #Check
    if(identical(names(RegionB),names(RegionC)) == FALSE){
        stop("RegionB y RegionC no son identicos")
    }
    
    #liberamos memoria
    rm(imputador);rm(dat)
    gc()
    
    #Tenemos la tabla preparada 'dat'
    if("depth.m" %in% loc == TRUE){
        #Procesamos por niveles de profundidad.
        #Procedemos a alimentar impute.per.region con una variable a la vez.
        #Se toma una variable, se imputa usando los radios y luego se almacena.
        #Tenemos la tabla preparada 'dat'. Eliminamos columnas y luego filas.
        dataProfList <- list()
        for (np in 1:length(profundidades)){
            dataVarList <- list()
            f_prodd <- sapply(imputedVars,function(impVar){
                datAux <-dat[c("clave","latitude","longitude","depth.m",impVar)]
                datAux <-datAux[is.na(datAux[,1]) | 
                                     (!is.na(datAux[,1]) & !is.na(datAux[,5])),]
                ##Extraemos profundidad deseada
                datAux <- datAux[datAux$depth.m == profundidades[np],]
                datAux$depth.m <- NULL
                
                #Check
                if(sum(is.na(datAux[,1]))*length(profundidades) != 
                                                           sum(is.na(dat[,1]))){
                    stop("Dimension de datAux inadecuada.")
                }
                #Si hay datos disponibles para hacer la imputacion.
                ejecutar <- FALSE
                if(nrow(datAux) != sum(is.na(dat$clave))){
                    ##Establecemos rango.
                    rango <- c(nrow(datAux)-sum(is.na(datAux$clave))+1,
                                                                   nrow(datAux))
                    
                    #Imputamos cada variable usando el set de radios
                    dataList <- list()
                    CLS <- class(datAux[,4])
                    for(i in 1:length(radio)){
                        #Le pasamos latitude,longitude e impVar.
                        df <- impute.per.region(datAux[,-1],
                               radio=radio[i],medida=medida,rango=rango,CLS=CLS)
                        #Recuperamos la seccion de la variable imputada
                        if(nrow(df) != nrow(datAux)){
                            stop("impute.per.region esta retornando un data set de con un numero diferente filas.")
                        }
                        df$clave <- datAux[,1]
                        df <- df[impVar][is.na(df$clave),]
                        df <- as.data.frame(df)
                        names(df) <- impVar
                        
                        #Lo agregamos a la lista.
                        dataList[[length(dataList)+1]] <- df
                    }
                    ejecutar <- TRUE
                    
                    #Limpiamos memoria
                    rm(df)
                    gc()
                }   

                #Extraemos la variable a corregir en datAux.
                datAux <- datAux[impVar][is.na(datAux$clave),]
                datAux <- as.data.frame(datAux)
                names(datAux) <- impVar
                
                #Si hay datos disponibles para hacer la imputacion.
                if(ejecutar == TRUE){
                    #Corregimos los NA's de impVar usando los valores hallados.
                    #lapply funcionara como un procedimiento no como una funcion
                    f_prod <- lapply(dataList,function(df){
                        datAux[,1][is.na(datAux[,1])] <<- 
                                                       df[,1][is.na(datAux[,1])]
                    })
                    rm(f_prod)
                    gc()
                }
                #Lo volvemos data frame en caso que se haya modificado.
                datAux <- as.data.frame(datAux)
                names(datAux) <- impVar
                
                #Agregamos a la lista la variable corregida impVar.
                #Superasignacion.
                dataVarList[[length(dataVarList)+1]] <<- datAux
            })
            rm(f_prodd)
            gc()
            
            #Preparamos la salida de la tabla por profundidad en datAux2.
            datAux2 <- dat[is.na(dat$clave),]
            datAux2 <- datAux2[datAux2$depth.m == profundidades[np],]
            datAux2$clave <- NULL
            
            #Reemplazamos la variables de dat por las nuevas conseguidas.
            #lapply funcionara como un procedimiento no como una funcion.
            f_prod4 <- lapply(dataVarList,function(datAux){
                var <- names(datAux)
                #Check
                if(length(var) != 1){
                    stop("Los data frames de dataVarList tienen mas de una variable.")
                }
                #Check
                if(nrow(datAux) != nrow(datAux2)){
                    stop("datAux y datAux2 no tienen el mismo numero de filas.")
                }
                datAux2[var][,1] <<- datAux[var][,1]
            })
            rm(f_prod4)
            gc()
        
            dataProfList[[length(dataProfList)+1]] <- datAux2
        }
        #Limpiamos memoria
        rm(datAux2);rm(dataVarList)
        gc()
        
        #Check
        if(length(dataProfList) != length(profundidades)){
            stop("length(dataProfList) != length(profundidades)")
        }
        
        #Preparamos data frame de salida dat.
        dat <- dat[is.na(dat$clave),]
        dat$clave <- NULL
        
        #Agregamos a dat los valores obtenidos por profundidad.
        for(np in 1:length(profundidades)){
            dat[dat$depth.m == profundidades[np],] <- dataProfList[[np]][,]
        }

    }else{
        #Procedemos a alimentar impute.per.region con una variable a la vez.
        #Se toma una variable, se imputa usando los radios y luego se almacena.
        #Tenemos la tabla preparada 'dat'. Eliminamos columnas y luego filas.
        
        #Primero hallamos los vecinos de cada punto de geolocalizacion de la 
        #region de correccion para los cuatro radios ingresados.
        
        #Vecinos para cada radio. RegionB y Region C solo contienen puntos de 
        #geolocalizacion.
        vecinos1 <- list()
        vecinos2 <- list()
        vecinos3 <- list()
        vecinos4 <- list()
        
        v <- apply(RegionC,1,function(x){

            #Clase correcta para x
            x1 <- as.numeric(as.character(x[1]))
            x2 <- as.numeric(as.character(x[2]))
            #Distancias del punto de RegionC al resto
            distancias <-unname(apply(RegionB,1,function(y){
                #Clase correcta para y
                y1 <- as.numeric(as.character(y[1]))
                y2 <- as.numeric(as.character(y[2]))
                d <-distGL(c(y1,y2),c(x1,x2))
                return(d)
            }))
            #Check
            if(class(distancias) != "numeric"){
                stop("Las distancias no son numericas.")
            }
            if(any(is.na(distancias))){
                stop("Se generaron NA's en el calculo de distancias.")
            }
            #Posiciones de RegionB donde estan los vecinos. Radio Menor
            posiciones <- which(distancias <= radio[1] & distancias != 0)
            if(length(posiciones) == 0){     #Reemplazamos integer(0) por NA
                posiciones <-NA
            }
            #Escribimos en lista
            vecinos1[[length(vecinos1)+1]] <<- posiciones  
            
            #Posiciones de RegionB donde estan los vecinos.
            if(length(radio) > 1){
                posiciones <- which(distancias <= radio[2] & distancias != 0)
                if(length(posiciones) == 0){ #Reemplazamos integer(0) por NA
                    posiciones <-NA
                }
                #Escribimos en lista
                vecinos2[[length(vecinos2)+1]] <<- posiciones 
            }
 
            #Posiciones de RegionB donde estan los vecinos.
            if(length(radio) > 2){
                posiciones <- which(distancias <= radio[3] & distancias != 0)
                if(length(posiciones) == 0){ #Reemplazamos integer(0) por NA
                    posiciones <-NA
                }
                #Escribimos en lista
                vecinos3[[length(vecinos3)+1]] <<- posiciones 
            }
            
            #Posiciones de RegionB donde estan los vecinos.
            if(length(radio) > 3){
                posiciones <- which(distancias <= radio[4] & distancias != 0)
                if(length(posiciones) == 0){ #Reemplazamos integer(0) por NA
                    posiciones <-NA
                }
                #Escribimos en lista
                vecinos4[[length(vecinos4)+1]] <<- posiciones 
            }
            return(NULL)
        })
        
        #Transformamos las posiciones de los vecinos en claves cib puntos de 
        #geolocalizacion.
        RegionB$clave <-paste(RegionB$latitude,RegionB$longitude,sep="_")
        
        #Vecinos 1
        if(length(vecinos1) != 0){
            #Check
            if(length(vecinos1) != nrow(RegionC)){
                stop("No todos los puntos de la tabla a imputar recibieron vecinos. R1")
            }
            vecinos1 <-lapply(vecinos1,function(x){  
                if(is.na(x[1])){                    
                    return(NA)
                }else{
                    s <-RegionB$clave[x]
                    return(s)
                }
            })
        }else{
            vecinos1[[1]] <- NA
        }
        #Vecinos 2
        if(length(vecinos2) != 0){
            #Check
            if(length(vecinos2) != nrow(RegionC)){
                stop("No todos los puntos de la tabla a imputar recibieron vecinos. R2")
            }
            vecinos2 <-lapply(vecinos2,function(x){  
                if(is.na(x[1])){                    
                    return(NA)
                }else{
                    s <-RegionB$clave[x]
                    return(s)
                }
            })
        }else{
            vecinos2[[1]] <- NA
        }
        #Vecinos 3
        if(length(vecinos3) != 0){
            #Check
            if(length(vecinos3) != nrow(RegionC)){
                stop("No todos los puntos de la tabla a imputar recibieron vecinos. R3")
            }
            vecinos3 <-lapply(vecinos3,function(x){  
                if(is.na(x[1])){                    
                    return(NA)
                }else{
                    s <-RegionB$clave[x]
                    return(s)
                }
            })
        }else{
            vecinos3[[1]] <- NA
        }
        #Vecinos 4
        if(length(vecinos4) != 0){
            #Check
            if(length(vecinos4) != nrow(RegionC)){
                stop("No todos los puntos de la tabla a imputar recibieron vecinos. R4")
            }
            vecinos4 <-lapply(vecinos4,function(x){  
                if(is.na(x[1])){                    
                    return(NA)
                }else{
                    s <-RegionB$clave[x]
                    return(s)
                }
            })
        }else{
            vecinos4[[1]] <- NA
        }
        
        #Lista de vecinos
        vecinos <- list(vecinos1,vecinos2,vecinos3,vecinos4)

        f_prodd <- sapply(imputedVars,function(impVar){
            #Data frames auxiliares
            datAuxB <- RegionB[c("latitude","longitude",impVar)]
            datAuxC <- RegionC[c("latitude","longitude",impVar)]
        
            CLS <- class(datAuxB[,3])
            #Imputamos cada variable usando el set de radios
            
            for(i in 1:length(radio)){
                #Le pasamos latitude,longitude e impVar.
                #Retorna un vector con impVar corregida.
                vect <- impute.per.region(datAuxB,datAuxC,vecinos=vecinos[[i]],
                                        medida=medida,CLS=CLS)
  
                #Recuperamos la seccion de la variable imputada
                if(length(vect) != nrow(RegionC)){
                    stop("impute.per.region esta retornando un data set de con un numero diferente filas.")
                }
            
                #Actualizamos datAuxC           
                datAuxC[impVar][,1] <- vect
            }
            
            #Corregimos RegionC (antiguamente dat)
            RegionC[impVar][,1] <<- datAuxC[impVar][,1]
            
            #Limpiamos memoria
            rm(vect)
            gc()
            
            #Es un procedimiento. Retornamos NULL para mayor velocidad.
            return(NULL)

        })
        
        rm(f_prodd)
        gc()

    }
    
    #El data frame de salida listo para el merge con datos se llama RegionC y 
    #esta altura ya fue procesado totalmente.
    
    #Claves
    claveDatos <- paste(datos$latitude,datos$longitude)
    claveDat <- paste(RegionC$latitude,RegionC$longitude)
    
    #Check
    if("depth.m" %in% loc == TRUE){
        if(nrow(RegionC) != (length(unique(claveDatos))*length(profundidades))){
            stop("El numero de filas del dataset imputado no es el esperado.")
        }
    }else{
        if(nrow(RegionC) != length(unique(claveDatos))){
            stop("El numero de filas del dataset imputado no es el esperado.")
        }
    }
    if(length(unique(claveDat) ) != length(unique(claveDatos))){
        stop("El numero de puntos distintos de geolocalizacion de RegionC y datos no coinciden.")
    }
    if(any(!(unique(claveDat) %in% unique(claveDatos)))){
        stop("Hay datos de geolocalizacion en las tablas RegionC y datos distintos.")
    }
    
    #Claves para el merge final.
    datos$clave <- claveDatos
    RegionC$clave <- claveDat
    datos <- merge(datos,
                   RegionC[!(names(RegionC) %in% c("latitude","longitude"))],
                   by="clave",all.x=T)
    datos$clave <- NULL
    
    #Salida de datos
    return(datos)
}
	
#/////////////////////////////////////////////////
#Funcion para unir los datos de Google Earth con la matriz biologica.
#En este caso tablaGE es la tabla creada cn Google y LatLonTB el dataset con los
#puntos de geolocalizacion de los datos de la tabla biologica.
#Retorna la tabla de Google Earth con los datos correspondientes al calculo de 
#distancias agregados como columnas. 
distGoogleEarth <- function(LatLonTB,tablaGE,dlMax=150,tiempoMax=14){
    
    cat("Calculando distancias...\n")
    #Reducimos el trabajo computacional.
    LatLonTB <- LatLonTB %>% unique()
    
    #Distancia lineal----------------------------------
    #Construimos un data frame con latitude, longitud, lat.objeto, lon.objeto
    #objeto, tipo y valor para cada fila de la tabla LatLonTB. Estos valores 
    #de latitud y longitud coinciden con los vistos en la tabla ambiental Google
    #Earth (tablaGE).
    #EN LA ULTIMA MODIFICACION (AL FINAL DEL CODIGO) SE SEPARO 'tipo' EN DOS 
    #NUEVAS VARIABLES 'Tiempo.en.Corriente.dias' y 'Distancia.Euclidea.kms'.
    #EL RESTO DE LAS VARIABLES FUERON IGUALMENTE SUBDIVIDIDAS Y UNIDAS POR MERGE
    distLin <- unname(apply(LatLonTB,1,function(x){
        dist2 <- unname(apply(tablaGE[c("latitude","longitude")],1,function(y){
            #Corregimos formato.
            d <- distGL(c(as.numeric(as.character(x[1])),
                          as.numeric(as.character(x[2]))),
                        c(as.numeric(as.character(y[1])),
                          as.numeric(as.character(y[2]))))
            return(d)
        }))
        #Distancia maxima,
        objt <- tablaGE$objeto[dist2 <= dlMax]
        latObj  <- tablaGE$latitude[dist2 <= dlMax]
        lonObj  <- tablaGE$longitude[dist2 <= dlMax]
        dist2 <- dist2[dist2 <= dlMax]
        
        #Check
        if((length(objt) != length(dist2)) == TRUE){
            stop("Las longitudes de objt y de dist2 no coinciden.")
        }
        med <- rep("Distancia Lineal (Kms)",times=length(objt))
        lat <- rep(as.numeric(as.character(x[1])),times = length(objt))
        lon <- rep(as.numeric(as.character(x[2])),times = length(objt))
        datosPtoi <- data.frame(latitude =lat,longitude =lon,lat.objeto= latObj,
                                lon.objeto = lonObj ,objeto = objt,tipo = med,
                                valor = dist2)
        
        #Solo retornamos datasets con datos.
        if(nrow(datosPtoi) != 0){
            #Lo pasamos a ffdf
            datosPtoi$objeto <- as.factor(datosPtoi$objeto)
            datosPtoi$tipo <- as.factor(datosPtoi$tipo)
            datosPtoi <- as.ffdf(datosPtoi)
            
            #Exportamos
            return(datosPtoi)
        }else{
            return(NULL)
        }
    }))
    cat("\n")
    cat("Uniendo datos de distancia.\n")
    cat("\n")
    
    #Check
    if(is.null(distLin)){
        message("Ingrese un radio de busqueda mas amplio.")
        message("No se hallaron objetos costeros.")
        cat("\n")
        stop("Ejecute el codigo nuevamente.",call. = F)
    }
    
    #Unificamos los datos en un solo data frame por rbind.
    dataDistLin <- data.frame(latitude = NA,longitude = NA,lat.objeto = NA,
                              lon.objeto = NA,objeto = NA ,tipo = NA,valor = NA)
    
    dataDistLin$objeto <- as.factor(dataDistLin$objeto)
    dataDistLin$tipo <- as.factor(dataDistLin$tipo)
    dataDistLin <- as.ffdf(dataDistLin)
    #Procedimiento
    s <- lapply(distLin,function(df){  #Agregamos solo data frames con datos.
        if(!is.null(df)){
            dataDistLin <<- ffdfappend(dataDistLin,df) 
        }
        return(NA)
    })
    
    #Eliminamos primera linea
    dataDistLin <<- dataDistLin[2:nrow(dataDistLin),]
    
    #Limpiamos memoria
    rm(s);rm(distLin)
    gc()

    #Removemos los NA's generado sy creamos el dataset de salida.
    if(!is.null(ffwhich(dataDistLin,!is.na(lat.objeto)))){
        dataDistLin <- dataDistLin[ffwhich(dataDistLin,!is.na(lat.objeto)),]  
    }

    #Tiempo de viaje en corrientes.--------------------
    if(tiempoMax == 0){
        cat("No se consideraran datos de corrientes.\n")
    }else{
        cat("Calculando tiempos de viaje en corriente...\n")
        
        LCCC <- read.csv(paste0(d,"/Ambientales/LineaCosteraConCorrientes.csv"))
        LCCC <- LCCC[1:255,]         #Eliminamos datos incoherentes.
        
        #Agregamos variables a tablaGE. 
        tablaGE$lat.objeto <- tablaGE$latitude   #Estas dos son iguales.
        tablaGE$lon.objeto <- tablaGE$longitude
        tablaGE$tipo <- rep(
            "Tiempo de viaje en corriente (Dias)",times = nrow(tablaGE))
        tablaGE$valor <- rep(0,times=nrow(tablaGE))
        tablaGE$elect <- rep(FALSE,times=nrow(tablaGE))
        tablaGE$clave <- paste0(tablaGE$latitude,tablaGE$longitude)
        
        #Reordenamos
        tablaGE <- tablaGE[,c(1,2,4,5,3,6,7,8,9)]
        
        #Creamos la lista de dataframes.
        tiempoRec <- unname(apply(LatLonTB,1,function(x){
            #Buscamos punto de linea costera mas proximo.
            Latsup <- as.numeric(as.character(x[1])) + 0.01
            LatInf <- as.numeric(as.character(x[1])) - 0.01
            LonDer <- as.numeric(as.character(x[2])) + 0.01
            LonIzq <- as.numeric(as.character(x[2])) - 0.01
            puntosCaja <- LCCC[LCCC$latitude  <= Latsup & 
                                   LCCC$latitude  >= LatInf &
                                   LCCC$longitude <= LonDer &
                                   LCCC$longitude >= LonIzq,c(1,2,3)] 
                                                    #lat,lon,posicion.
            #Buscamos los puntos costeros mas cercanos.
            while(nrow(puntosCaja) == 0){
                Latsup <- Latsup + 0.01
                LatInf <- LatInf - 0.01
                LonDer <- LonDer + 0.01
                LonIzq <- LonIzq - 0.01
                puntosCaja <- LCCC[LCCC$latitude  <= Latsup & 
                                       LCCC$latitude  >= LatInf &
                                       LCCC$longitude <= LonDer &
                                       LCCC$longitude >= LonIzq,  c(1,2,3)]
                if(LonDer >= -53){
                    stop("No se encontraron datos cercanos en la linea costera.")
                }
            }
            #Hallamos las distancias entre ellos y el punto central.
            dist <- unname(apply(puntosCaja,1,function(y){
                d <- distGL(c(as.numeric(as.character(x[1])),
                              as.numeric(as.character(x[2]))),
                            c(as.numeric(as.character(y[1])),
                              as.numeric(as.character(y[2]))))
                return(d)
            }))
            #Marcamos la fila de LCCC donde esta el punto costero mas cercano.
            posPtoCosta <-puntosCaja$posicion[which.min(dist)] #Posicion en la costa
            #Corregimos formato
            posPtoCosta <- as.numeric(as.character(posPtoCosta))
            
            if(class(posPtoCosta) == "character" | 
                                                class(posPtoCosta) == "factor"){
                stop("La clase de posPtoCosta es incorrecta.")
            }
            
            #En la posicion 19 esta la boca del lago.
            #En la posicion 69 esta la boca del lago.
            #En la posicion 167 esta la boca del golfo de cariaco.
            #En la posicion 176 esta la boca del golfo de cariaco.  
            #Buscamos de izquiera a derecha por pares.
            #Siempre buscamos contra corriente.
            if(1 <= posPtoCosta & posPtoCosta <= 45){          
                #Del 1 al  19 =  RE
                reco <- posPtoCosta : 1
            }else if(46 <= posPtoCosta  & posPtoCosta <= 87){  
                #Del 46 al  87 =  RP
                reco <- posPtoCosta : 87
            }else if(88 <= posPtoCosta  & posPtoCosta <= 95){  
                #Del 88 al  95 =  RE
                reco <- posPtoCosta : 87
            }else if(96 <= posPtoCosta  & posPtoCosta <= 171){ 
                #Del 96 al 171 =  RP
                reco <- posPtoCosta : 171
            }else if(172 <= posPtoCosta & posPtoCosta <= 177){ 
                #Del 172 al 177 =  RE
                reco <- posPtoCosta : 171
            }else if(178 <= posPtoCosta & posPtoCosta <= 200){ 
                #Del 178 al 200 =  RP
                reco <- posPtoCosta  :200
            }else if(201 <= posPtoCosta & posPtoCosta <= 210){ 
                #Del 201 al 210 =  RE
                reco <- posPtoCosta : 200
            }else if(211 <= posPtoCosta & posPtoCosta <= 215){ 
                #Del 211 al 215 =  RP
                reco <- posPtoCosta : 215
            }else if(216 <= posPtoCosta & posPtoCosta <= 218){ 
                #Del 216 al 218 =  RE
                reco <- posPtoCosta : 215   
            }else if(219 <= posPtoCosta & posPtoCosta <= 255){ 
                #Del 219 al 255 =  RP
                reco <- posPtoCosta : 255 
            }else{
                stop("posMin fuera de rango.")
            }
            #Si solo tenemos una posicion hacemos
            if(length(reco) == 1){
                tAcumulado <- 0
            }else{
                #Vemos en que direccion avanza.
                if(reco[1] >= reco[2]){        #Direccion al predecesor.
                    tAcumulado <-c(0,LCCC$tiempoViaje.Adj.dias[reco]
                                                                [-length(reco)])
                }else{                                   #Direccion al sucesor.
                    tAcumulado <-c(0,LCCC$tiempoViaje.Adj.dias[reco][-1])
                }
                tAcumulado <- cumsum(tAcumulado) #Tiempo acumulado de recorrido.
                
                #check
                if(length(reco) != length(tAcumulado)){
                    stop("Las variables reco y tAcumulado no miden lo mismo.")   
                }
                #Limitamos la busqueda a dos semanas de viaje por default.
                reco <- reco[tAcumulado <= tiempoMax] 
                tAcumulado <- tAcumulado[tAcumulado <= tiempoMax] 
            }
            #Lo volvemos data frame.
            dfReco <- data.frame(reco = reco,tAcumulado = tAcumulado,
                                 pos = seq(1:length(reco)))
            
            #Ciclo de busqueda de objetos costeros a lo largo de la ruta.
            #Procesamos la lista donde estan los datasets de busqueda en funcion 
            #de la amplitud del recorrido. La lista contiene 6 dataframes. En la
            #posicion 1 estan las ubicaciones mas cercanas a los puntos de 
            #recorrido sobre la linea costera y en la ultima posicion estan los 
            #mas lejanos. Este nivel de cercania o lejania esta acotado a maximo 
            #lat +- 0.06 y lon +- 0,06
            listaDF <- list()
            for(i in 1:10){             #Originalmente 1:6.
                #Creamos lista con el conjunto de datasets.
                listaRec <- apply(dfReco,1,function(r){
                    m <- 0.01*i
                    #Limitamos el radio de la primera posicion.
                    if((as.numeric(as.character(r[3])) == 1) & (i > 3)){
                        m <- 0.03
                    }
                    #Recolectamos por zona los puntos no tratados.
                    #Contiene los puntos de geolocalizacion de los objetos.
                    zona <- tablaGE[tablaGE$latitude  <= 
                                        LCCC$latitude[ as.numeric(
                                            as.character(r[1]))] + m &
                                        tablaGE$latitude  >=
                                        LCCC$latitude[ as.numeric(
                                            as.character(r[1]))] - m &
                                        tablaGE$longitude <= 
                                        LCCC$longitude[as.numeric(
                                            as.character(r[1]))] + m &
                                        tablaGE$longitude >= 
                                        LCCC$longitude[as.numeric(
                                            as.character(r[1]))] - m &
                                        tablaGE$elect == FALSE,]
                    
                    #Volvemos la zona circular en caso de tener datos.
                    if(nrow(zona) != 0){
                        D <-apply(zona[c("latitude","longitude")],1,function(f){
                            dc <- distGL(
                                c(as.numeric(as.character(f[1])),
                                  as.numeric(as.character(f[2]))),
                                c(LCCC$latitude[as.numeric(as.character(r[1]))], 
                                  LCCC$longitude[
                                      as.numeric(as.character(r[1]))]))
                            return(dc)
                        })
                        D <- unname(D)
                        rad <- distGL(c(0,0),c(m,0))
                        zona <- zona[D <= rad,]  
                    }
                    
                    #Asignamos ubicaciones e impedimos su reasignacion.
                    #Si la variable "signo" es negativa indica que estamos 
                    #empleando la distancia con el punto que esta en la 
                    #posicion previa porque no hay sucesores en el recorrido.
                    #La variable 'direc' indica la direccion del recorrido 
                    #contracorriente.
                    if(nrow(zona) != 0){
                        #Agregamos variables necesarias.
                        zona$posicion <- 1:nrow(zona)
                        
                        if(identical(names(zona),c(names(tablaGE),"posicion"))
                                                                      == FALSE){
                            stop("El dataset zona esta desordenado.")
                        }
                        #Posicion actual en la tabla LCCC
                        posAct <- as.numeric(as.character(r[1]))       
                        if(nrow(dfReco) == 1){
                            if(posAct == 255){              #usamos la previa.
                                posSig <- 254
                            }else{           #Usamos la siguiente.
                                posSig <- posAct + 1
                            }
                            signo  <- 1      #No existe una direccion correcta.
                        }else{
                            #Si estamos en la ultima
                            if(as.numeric(as.character(r[3])) == nrow(dfReco)){       
                                posSig <-dfReco$reco[       #usamos la previa.
                                    as.numeric(as.character(r[3])) - 1]
                                signo <- -1
                            }else{                      #Caso contrario
                                posSig <-dfReco$reco[   #usamos la siguiente.
                                    as.numeric(as.character(r[3])) + 1]
                                signo <- 1
                            }
                        }
                        #Vector que une los puntos consecutivos del recorrido
                        v1 <- c(LCCC$latitude[posAct],LCCC$longitude[posAct])
                        v2 <- c(LCCC$latitude[posSig],LCCC$longitude[posSig])
                        v <- v2 - v1
                        #Normalizado
                        normaV <- sqrt(v[1]^2 + v[2]^2)
                        v <- v/normaV    
                        #Procesamos todos los puntos de geolocalizacion de la 
                        #zona.
                        df_prod <- apply(zona,1,function(y){
                            #Vector que une elpunto dee zona con el de recorrido
                            w <- c(as.numeric(as.character(y[1])),
                                   as.numeric(as.character(y[2]))) - v1
                            prodt <- signo*(v[1]*w[1] + v[2]*w[2])
                            prodt <- prodt/normaV                #Proporcion.
                            #Si el producto es positivo se incrementa la dist.
                            #Hallamos el tiempo total de recorrido aproximado
                            #r[2] contienen el tiempo acumulado hasta la pos
                            #actual. r= c(reco,tAcumulado,pos)
                            #Si no hay recorrido el acumulado es cero.
                            if(nrow(dfReco) == 1){
                                tps <- 0.5       #Valor promedio
                            }else{
                                tps <- dfReco$tAcumulado[dfReco$reco == posSig]
                            }
                            zona$valor[as.numeric(as.character(y[10]))] <<- 
                                as.numeric(as.character(r[2])) + 
                                prodt*abs(as.numeric(as.character(r[2])) - tps)
                            #El prime paso del recorrido considera todas los 
                            #tiempos Positivos. Suponemos que por cercania 
                            g#eografica el objeto afecta el lugar de estudio.
                            if(r[3] == 1){
                                zona$valor[as.numeric(as.character(y[10]))] <<- 
                                    abs(zona$valor[
                                        as.numeric(as.character(y[10]))
                                    ])
                            }
                        })
                        #liberamos memoria
                        rm(df_prod)
                        gc()
                        
                        #Evitamos reasignacion de datos.
                        clavesElim <- tablaGE$clave %in% zona$clave
                        tablaGE$elect[clavesElim] <<- rep(TRUE,sum(clavesElim))
                        
                        #Colocamos en lat lon la posicion de geolocalizacion del
                        #punto biologico
                        zona$latitude  <- rep(x[1],nrow(zona))
                        zona$longitude <- rep(x[2],nrow(zona))
                        
                        #Eliminamos variables inecesarias.
                        zona$elect <-NULL
                        zona$posicion <- NULL
                        zona$clave <- NULL
                        
                        #Exportamos solo si tiene variables.
                        return(zona)
                    }
                })
                #Unimos las zonas almacenadas de listaRec.
                unionZonas <-  data.frame(latitude = NA,longitude =NA,
                                          lat.objeto =NA,
                                          lon.objeto = NA,objeto = NA , 
                                          tipo = NA,valor = NA)
                #Procedimiento
                s <- lapply(listaRec,function(z){
                    unionZonas <<- rbind(unionZonas,z)
                })
                #limpiamos memoria
                rm(s)
                gc()
                
                #Borrar datos vacios.
                unionZonas <-  unionZonas[!is.na(unionZonas$latitude),]
                
                #Agregamos el data frame a la lista auxiliar.
                listaDF[[length(listaDF) + 1]] <- unionZonas
            }
            #Procesamos la lista donde estan los datasets de busqueda en funcion
            #de la amplitud del recorrido. La lista contiene 6 dataframes. En la
            #posicion 1 estan las ubicaciones mas cercanas a los puntos de 
            #recorrido sobre la linea costera y en la ultima posicion estan los 
            #mas lejanos. Este nivel de cercania o lejania esta acotado a maximo
            #lat +- 0.06 y lon +- 0,06
            datosPtoi <- data.frame(latitude = NA,longitude = NA,
                                    lat.objeto = NA,
                                    lon.objeto = NA,objeto = NA , 
                                    tipo = NA,valor = NA)
            #Procedimiento
            s <- lapply(listaDF,function(l){
                datosPtoi <<- rbind(datosPtoi,l)
            })
            #limpiamos memoria
            rm(s)
            gc()
            
            #Borrar datos vacios.
            datosPtoi <-  datosPtoi[!is.na(datosPtoi$latitude),]
            
            return(datosPtoi)
        }))
        cat("\n")
        cat("Uniendo datos de tiempo.\n")
        cat("\n")
        
        #Unificamos los datos en un solo data frame por rbind.
        dataTiempoRec <- data.frame(latitude = NA,longitude = NA,
                                    lat.objeto = NA,
                                    lon.objeto = NA,objeto = NA , 
                                    tipo = NA,valor = NA)
        
        dataTiempoRec$objeto <- as.factor(dataTiempoRec$objeto)
        dataTiempoRec$tipo <- as.factor(dataTiempoRec$tipo)
        dataTiempoRec <- as.ffdf(dataTiempoRec)
        
        #Procedimiento
        #Agregamos solo data frames con datos
        s <- lapply(tiempoRec,function(df){ 
            if(!is.null(df)){
                df$objeto <- as.factor(df$objeto)
                df$tipo <- as.factor(df$tipo)
                df <- as.ffdf(df)
                dataTiempoRec <<- ffdfappend(dataTiempoRec,df) 
            }
            return(NA)
        })
        
        #Limpiamos memoria
        rm(s);rm(tiempoRec)
        gc()
        
        #Removemos los NA's generadosy creamos el dataset de salida.
        if(!is.null(ffwhich(dataTiempoRec,!is.na(lat.objeto)))){
            dataTiempoRec <- dataTiempoRec[
                ffwhich(dataTiempoRec,!is.na(lat.objeto)),]   
        }

        #Eliminamos los datos con tiempos de recorrido mayor al solicitado
        dataTiempoRec <- dataTiempoRec[
            ffwhich(dataTiempoRec,valor <= tiempoMax),] 
    }
    
    #Unimos tablas para la salida.
    cat("\n")
    cat("Generando salida...\n")
    cat("\n")
    
    if(tiempoMax == 0){
        tablaA <- dataDistLin
        
        if(nrow(tablaA) == 0){
            message("Error:")
            message("Bajo las especificaciones dadas se han excluido todos los datos.")
            stop("Incremente la distancia o el tiempo de viaje en corriente.",
                 call.= F)
        }
        
    }else{
        tablaA <- ffdfappend(dataDistLin,dataTiempoRec)
        
        if(nrow(tablaA) == 0){
            message("Error:")
            message("Bajo las especificaciones dadas se han excluido todos los datos.")
            stop("Incremente la distancia o el tiempo de viaje en corriente.",
                 call.= F)
        }
        
        #MODIFICACION FINAL.------------------- (Seccion borrable)
        tablaA1 <- tablaA[ffwhich(tablaA,
                                tipo == "Tiempo de viaje en corriente (Dias)"),] 
        tablaA1$tipo <- NULL
        names(tablaA1)[-c(1,2)] <- paste0(names(tablaA1)[-c(1,2)],".t")
        tablaA <- transform(tablaA, clave =  paste(latitude,longitude,sep="_"))
        
        tablaA2 <- tablaA[ffwhich(tablaA,
                                tipo == "Distancia Lineal (Kms)"),]
        tablaA2$tipo <- NULL
        names(tablaA2)[-c(1,2)] <- paste0(names(tablaA2)[-c(1,2)],".d")
        tablaA2 <- transform(tablaA2, clave = paste(latitude,longitude,sep="_"))
        
        #Checks finales.
        if(names(tablaA1)[1] != "latitude"){
            stop("Tabla desordenada en la modificacion final.")   
        }
        if(names(tablaA1)[2] != "longitude"){
            stop("Tabla desordenada en la modificacion final.")   
        }
        if(names(tablaA2)[1] != "latitude"){
            stop("Tabla desordenada en la modificacion final.")   
        }
        if(names(tablaA2)[2] != "longitude"){
            stop("Tabla desordenada en la modificacion final.")   
        }
        if(any(is.na(tablaA1$clave))){
            stop("La clave de tablaA1 tiene valores perdidos")   
        }
        if(any(is.na(tablaA2$clave))){
            stop("La clave de tablaA2 tiene valores perdidos")   
        }
        #Merge.
        tablaA <- merge.ffdf.all(tablaA1,tablaA2[,-c(1,2)],by="clave")
        tablaA$clave <- NULL
        
        #Renombramos.
        names(tablaA)[which(names(tablaA) =="valor.t")] <-
            "Tiempo.en.Corriente.dias"
        names(tablaA)[which(names(tablaA) =="valor.d")] <-
            "Distancia.Euclidea.kms"
        
        #Formateamos
        tablaA <- transform(tablaA, latitude = 
                                as.numeric(as.character(latitude)))
        tablaA <- transform(tablaA, longitude = 
                                as.numeric(as.character(longitude)))
        
        #Eliminamos negativos.... Hay algo raro por ahi
        tablaA1 <- tablaA[ffwhich(tablaA,
                                tipo == "Tiempo de viaje en corriente (Dias)"),]
        
        tablaA$Tiempo.en.Corriente.dias[ffwhich(tablaA,
                                        Tiempo.en.Corriente.dias < 0 & 
                                        !is.na(Tiempo.en.Corriente.dias))] <-
            rep(NA,sum((tablaA$Tiempo.en.Corriente.dias < 0) &
                           !is.na(tablaA$Tiempo.en.Corriente.dias)))
    }

    #Exportamos.
    return(tablaA)
}

#Version original
#Recibe la matriz ambiental en df1. Ver su formato en Getting-Preprocessing2.R
impute.per.regionAux <- function(df1,radio=1,medida="median"){    #Radio en kms.
    GLdf <-df1[,which(names(df1) %in% c("latitude","longitude"))]
    GLdf[,1] <-as.numeric(as.character(GLdf[,1]))   #Aseguramos tipo numeric.
    GLdf[,2] <-as.numeric(as.character(GLdf[,2]))
    df <-df1[,-which(names(df1) %in% c("latitude","longitude"))]
    df <-as.data.frame(df)
    names(df) <-names(df1)[names(df1) != "latitude" & names(df1) != "longitude"]
    
    #Buscamos las posiciones de los vecinos a cada punto en el data frame.
    vecinos <-as.list(unname(apply(GLdf,1,function(x){
        posiciones <-unname(apply(GLdf,1,function(y){
            d <-distGL(c(y[1],y[2]),c(x[1],x[2]))
            return(d)
        }))         #Posiciones donde estan los puntos 
        #de geoloc (vecinos a cada uno). 
        v <-which(posiciones <= radio & posiciones != 0)
        if(length(v) == 0){     #Reemplazamos integer(0) por NA
            v <-NA
        }
        return(v)            
    })))
    
    #Asignamos a cada posicion su punto de geolocalizacion correspondiente.
    GLdf$clave <-paste(GLdf$latitude,GLdf$longitude,sep="_")
    vecinos <-lapply(vecinos,function(x){    
        if(is.na(x[1])){                    
            return(NA)
        }else{
            s <-GLdf$clave[x]
            return(s)
        }
    })
    
    #Una vez localizados los puntos vecinos dentro del radio esperado imputamos.
    #Asignamos la clase correcta a las variables del dataset generado por apply.
    if(is.null(dim(df)) == TRUE){
        clases <-class(df)
        df[2:(length(df)+1)] <-df       #Incluimos informacion sobre el tipo de
        df[1] <-clases                  #variable en el dataset.
    }else{                              
        clases <-unname(sapply(df,class))
        df[2:(nrow(df)+1),] <-df
        df[1,] <-clases
    }
    
    salida <-apply(df,2,function(x){
        if(x[1] == "character" | x[1] == "factor"){
            x <-as.character(x[-1])              #Tipo real de variable.
        }else if(x[1] == "numeric"){
            x <-as.numeric(as.character(x[-1]))  #Tipo real de variable.
        }
        x <-data.frame(x=x,sec=(1:length(x)))
        z <-apply(x,1,function(y){
            y <-unname(y)
            if(is.na(y[1])){     #Intentamos imputar si es NA.
                if(sum(is.na(vecinos[[as.numeric(y[2])]])) != 0){
                    return(y[1]) #Si no tiene vecinos retornamos NA.
                }
                ubic <-GLdf$clave %in% vecinos[[as.numeric(y[2])]]
                valores <-x[,1][ubic]
                if(class(valores) != "numeric"){#Si los valores son categoricos.
                    t <-table(valores)
                    moda <-names(t[which(t == max(t))])
                    if(length(moda) > 1){
                        warning("Ambiguedad al imputar la variable categorica. La moda no es unica.")
                    }
                    return(moda[1])
                }
                if(medida == "median"){
                    y[1] <-median(valores,na.rm = T)
                }else if(medida == "mean"){
                    if(all(is.na(valores))){      #Vecinos vacios.
                        y[1] <-NA
                    }else{
                        #Media ponderada.
                        latAct <-GLdf$latitude[as.numeric(y[2])]
                        lonAct <-GLdf$longitude[as.numeric(y[2])]
                        geoVec <-GLdf[ubic,1:2]
                        #Removemos geolocalizaciones sin datos.
                        geoVec <-geoVec[!is.na(valores),]
                        valores <-valores[!is.na(valores)]
                        
                        #Calculamos conjunto de distancias.
                        dis <-apply(geoVec,1,function(z){
                            d<-distGL(c(z[1],z[2]),c(latAct,lonAct))
                            return(d)
                        })
                        valores <-valores[order(dis,decreasing = T)]
                        dis <-sort(dis,decreasing = T)
                        maxi <-max(dis)+1
                        total <-sum(abs(maxi-dis))
                        pesos <-abs(maxi-dis)/total
                        if(abs(sum(pesos)-1) > 0.000001){
                            stop("La suma de los pesos no es 1.")
                        }
                        y[1] <-round(sum(pesos*valores),4)
                        #y[1] <-round(mean(valores,na.rm = T),4)
                        #print(y[1])
                    }
                }
                return(y[1]) 
            }else{
                return(y[1])     #Si no es NA no imputamos.
            }
        })
        z <-unname(z)
        return(z)
    })
    #Transformamos en data frame la salida bajo el mismo formato de entrada.
    salida <-as.data.frame(salida)
    for(i in 1:ncol(salida)){               #Formato adecuado de salida.
        if(clases[i] == "numeric"){
            salida[,i] <-as.numeric(as.character(salida[,i]))
        }else{
            salida[,i] <-as.character(salida[,i])
        }
    }
    salida <-cbind(df1[,which(names(df1) %in% c("latitude","longitude"))],
                   salida)
    return(salida)
}
