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
#Plot de geolocalizacion de la raw data en archivos PNG
MapaRawData<-function(df,file,title,lonIni=-66.715,latIni=13.115,zoom=6,size=2,color="red"){
    png(file=file)
    map<-get_map(location= c(lon=lonIni,lat=latIni),zoom=zoom,maptype="terrain") 
    plotMapa<-ggmap(map) +
        geom_point(data=df,aes(x=lon,y=lat),col=color,show.legend=FALSE,size=size) +
        labs(title=title)	
    plot(plotMapa)
    dev.off()
}

#/////////////////////////////////////////////////
#Transformacion a formato decimal para geolocalizacion.
geo2dec<-function(c){
    z<-sapply(strsplit(c, "[o.\']"),as.character)
    dec<- as.numeric(z[1, ]) + as.numeric(z[2, ])/60 + as.numeric(z[3, ])/3600
    if (z[4, ]=="N"||z[4, ]=="E") {
        dec 
    }else{
        -dec
    } 
}

#/////////////////////////////////////////////////
#Formateo de las filas de los archivos txt descargados de la NOAA
formato<-function(fila,Tab){
    vect<-strsplit(as.character(fila)[1],split="")[[1]]
    if(Tab==TRUE){
        vect<-gsub("\t"," ",vect)
    }
    s<-which(vect==" ")
    pos<-rep(0,length(s))
    for(i in 1:(length(s)-1)){
        if(s[i]+1!=s[i+1]){
            pos[i]<-1
        }
    }
    pos[length(s)]<-1
    if(any(pos==0)){
        fila<-stringr::str_trim(paste0(vect[-s[pos==0]],collapse=""))
    }else{
        fila<-stringr::str_trim(paste0(vect,collapse=""))
    }
    return(strsplit(fila,split=" ")[[1]])
}

#/////////////////////////////////////////////////
#Importe y formateo de los archivos txt descargados de la NOAA
importar<-function(file,Tab=FALSE){
    data <- read.csv(paste0("~/Thesis Project AB/Data/Raw Data/",file),
                     na.strings=c(""),stringsAsFactors=F)
    k<-t(apply(data[-1,],1,formato,Tab))
    data[2:nrow(data),]<-k
    return(data)
}

#/////////////////////////////////////////////////
#Construccion del dataframe con los datos de geolocalizacion
dfGeo<-function(lista){
    lat<-c(lista[[1]][1],lista[[2]][1],lista[[3]][1])
    lon<-c(lista[[1]][2],lista[[2]][2],lista[[3]][2])
    lat<-geo2dec(lat)
    lon<-geo2dec(lon)
    return(data.frame(lat=lat,lon=lon))
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
    aux1<-merge.ffdf(x,y,by.x,by.y,all.x=TRUE)
    aux2<-merge.ffdf(y,x,by.x,by.y,all.x=TRUE)
    aux2<-aux2[,names(aux1)]
    aux2<-aux2[!(aux2$clave %in% aux1$clave),]
    aux2<-as.ffdf(aux2)
    df3<-ffdfappend(aux1,aux2)
    return(df3)
}

#/////////////////////////////////////////////////
#Funcion que indica cuales filas del data frame son solo NA's.
fullNA<-function(df){
    d<-apply(df,1,function(i){sum(is.na(i))})
    pos<-which(d==ncol(df))
    if(length(pos)==0){
        stop("No implementar esta funcion. El data frame no tiene filas de NA's")
    }else{
        return(pos)   
    }
}

#/////////////////////////////////////////////////
#Formateando tablas de Cariaco
CariacoFormat<-function(data,vect,p1=":",p2=":",p3=":",complete=TRUE
                        ,v1=21,p=11,x2=TRUE,r1=1:3,r2=1:3,r3=1:3){
    data[,v1][p]<-"Lattitude"
    data[,v1][data$x1=="Cast 1"]<-df1$lat[1]
    data[,v1][data$x1=="Cast 2"]<-df1$lat[2]
    data[,v1][data$x1=="Cast 3"]<-df1$lat[3]
    data[,(v1+1)][p]<-"Longitude"
    data[,(v1+1)][data$x1=="Cast 1"]<-df1$lon[1]
    data[,(v1+1)][data$x1=="Cast 2"]<-df1$lon[2]
    data[,(v1+1)][data$x1=="Cast 3"]<-df1$lon[3] 
    if(x2==TRUE){
        if(complete==TRUE){
            a<-paste(strsplit(data$x2[vect[1]],split=" ")[[1]][r1]
                     ,collapse="")
            a<-sub(p1,"",a)
            data$x2[vect[1]]<-a
            b<-paste(strsplit(data$x2[vect[2]],split=" ")[[1]][r2]
                     ,collapse="")
            b<-sub(p2,"",b)
            data$x2[vect[2]]<-b
            c<-paste(strsplit(data$x2[vect[3]],split=" ")[[1]][r3]
                     ,collapse="")
            c<-sub(p3,"",c)
            data$x2[vect[3]]<-c
        }
        data$x1[data$x1=="Cast 1"]<-data$x2[vect[1]]
        data$x1[data$x1=="Cast 2"]<-data$x2[vect[2]]
        data$x1[data$x1=="Cast 3"]<-data$x2[vect[3]]
    }else{
        if(complete==TRUE){
            a<-paste(strsplit(data$x1[vect[1]],split=" ")[[1]][r1]
                     ,collapse="")
            a<-sub(p1,"",a)
            data$x1[vect[1]]<-a
            b<-paste(strsplit(data$x1[vect[2]],split=" ")[[1]][r2]
                     ,collapse="")
            b<-sub(p2,"",b)
            data$x1[vect[2]]<-b
            c<-paste(strsplit(data$x1[vect[3]],split=" ")[[1]][r3]
                     ,collapse="")
            c<-sub(p3,"",c)
            data$x1[vect[3]]<-c
        }
        data$x1[data$x1=="Cast 1"]<-data$x1[vect[1]]
        data$x1[data$x1=="Cast 2"]<-data$x1[vect[2]]
        data$x1[data$x1=="Cast 3"]<-data$x1[vect[3]]
    }
    return(data)
}

#/////////////////////////////////////////////////
#formateando tablas para futura union con rbind
formatUnion<-function(df1,df2){   
    faltantes<-names(df1)[!(names(df1) %in% names(df2))]
    nroColNA<-length(faltantes)
    if(nroColNA!=0){
        dataNA<-as.data.frame(
            matrix(rep(NA,times=(nroColNA*nrow(df2))),nrow(df2),nroColNA))
        names(dataNA)<-faltantes
        df2<-cbind(df2,dataNA)
    }
    faltantes<-names(df2)[!(names(df2) %in% names(df1))]
    nroColNA<-length(faltantes)
    if(nroColNA!=0){
        dataNA<-as.data.frame(
            matrix(rep(NA,times=(nroColNA*nrow(df1))),nrow(df1),nroColNA))
        names(dataNA)<-faltantes
        df1<-cbind(df1,dataNA)
    }
    df2<-df2[,names(df1)]
    return(list(df1,df2))
}

#/////////////////////////////////////////////////
#Formateando NAs
formatNA<-function(v){
    v[v=="n.d."]<-NA
    v[grep("-999",v)]<-NA
    if(class(v)=="numeric"){
        v[round(v,0)==-999]<-NA
    }
    return(v)
}

#/////////////////////////////////////////////////
#Formateando tablas del atlas.
atlasformat<-function(data){
    char<-as.character(data[1,1])
    data[,1]<-as.character(data[,1])
    data[1,1]<-char
    data$Medida<-rep(data[1,1], times=nrow(data))
    data<-data[,c(ncol(data),1:(ncol(data)-1))]
    data[2,2]<-"latitude"
    names(data)[2:ncol(data)]<-sapply(data[2,-1],as.character)
    data<-data[-c(1,2),]
    names(data)<-sapply(names(data),tolower)
    names(data)[4]<-"values at depths (m):0"
    return(data)
}

#/////////////////////////////////////////////////
#Formateando tablas del atlas.
aproxGeoloc<-function(df,lat,lon){
    if(nrow(df)==0){
        stop("ERROR: El data frame ingresado no tiene filas.")
    }
    if(sum(c("latitude","longitude") %in% names(df)) < 2){
        stop("ERROR: No se reconoce el data frame de localizacion.")
    }
    aux<-df[(lat-0.5)<=df$latitude & df$latitude<=(lat+0.5) &
            (lon-0.5)<=df$longitude & df$longitude<=(lon+0.5),]
    i<-1
    while(nrow(aux)==0){
        i<-i+0.1
        aux<-df[(lat-i)<=df$latitude & df$latitude<=(lat+i) &
                    (lon-i)<=df$longitude & df$longitude<=(lon+i),]       
    }
    df<-aux
    diff<-unname(apply(df,1,function(x){sqrt((x[1]-lat)^2+(x[2]-lon)^2)}))
    pos<-which.min(diff)
    return(list(latAprox=df$latitude[pos],lonAprox=df$longitude[pos],
                Error=round(diff[pos],4)))
}

#/////////////////////////////////////////////////
#Suprimir warings de ciertas funciones temporalmente.
suppressWarnings <- function(expr) {
    ops <- options(warn = -1)       ## FIXME: temporary hack until R_tryEval
    on.exit(options(ops))           ## calls are removed from methods code
    withCallingHandlers(expr,warning=function(w)invokeRestart("muffleWarning"))
}

#/////////////////////////////////////////////////
#Funcion que transforma imagenes en vectores.
imagenTovector<-function(imagen){    #La imagen que ingresa es un arreglo 2x2.
    imagen<-as.matrix(imagen)
    for (i in 1:ncol(imagen)){       #Aumentamos contraste.
        imagen[,i][imagen[,i]<=0.85]<-0
        imagen[,i][imagen[,i]>0.85]<-1
        imagen[,i][imagen[,i]==1]<-2
        imagen[,i][imagen[,i]==0]<-100
        imagen[,i][imagen[,i]==2]<-0
    }
    x<-apply(imagen,2,sum)
    y<-apply(imagen,1,sum)
    breaksX<-seq(0,length(x),length=5)
    breaksY<-seq(0,length(y),length=11)
    indicesX<-seq_along(x)
    indicesY<-seq_along(y)
    gX<-cut(indicesX,breaks=breaksX)
    gY<-cut(indicesY,breaks=breaksY)
    dfx<-data.frame(x=x,gX=gX)
    dfy<-data.frame(y=y,gY=gY)
    x<-as.numeric(tapply(dfx$x,dfx$gX,max))
    y<-as.numeric(tapply(dfy$y,dfy$gY,max))
    x<-x/sum(x)             #Estandarizamos la salida.
    y<-y/sum(y)
    vect<-c(x,y)
    return(vect)
}

#/////////////////////////////////////////////////
#Constructor del modelo de interpretacion de caracteres numericos.
interBuilder<-function(df){
    
    df <-rbind(df,df,df,df,df,df,df,df,df,df)  #Trampa
    #Montamos arbol de decision.
    modFit<-rpart(num~.,method="class",data=df)
    pred<-predict(modFit,newdata=df[,-15],type="class")
    setwd("~/Thesis Project AB/R-Objects")
    sink("TestDelInterpretador.txt")
    print(confusionMatrix(pred,df$num))
    sink()
    return(modFit)
}

#/////////////////////////////////////////////////
#Funcion que transforma la prediccion del modelo en numeros.
predTonum <-function(pred){
    char <-apply(pred,1,function(x){names(which(x ==1))})
    num <-sapply(char,function(x){
        if(x=="mns"){
            return("-")
        }else if(x=="n0"){
            return("0")
        }else if(x=="n1"){
            return("1")
        }else if(x=="n2"){
            return("2")
        }else if(x=="n3"){
            return("3")
        }else if(x=="n4"){
            return("4")
        }else if(x=="n5"){
            return("5")
        }else if(x=="n6"){
            return("6")
        }else if(x=="n7"){
            return("7")
        }else if(x=="n8"){
            return("8")
        }else if(x=="n9"){
            return("9")
        }else if(x=="pt"){
            return(".")
        }
    })
    num<-as.numeric(paste0(num,collapse=""))
    return(num)
}

#/////////////////////////////////////////////////
#Funcion que transforma los numeros en claves para reentrenamiento.
numTopred <-function(num){
    vectUsuario <-strsplit(num,split="")[[1]]
    char <-sapply(vectUsuario,function(x){
        if(x=="-"){
            return("mns")
        }else if(x=="0"){
            return("n0")
        }else if(x=="1"){
            return("n1")
        }else if(x=="2"){
            return("n2")
        }else if(x=="3"){
            return("n3")
        }else if(x=="4"){
            return("n4")
        }else if(x=="5"){
            return("n5")
        }else if(x=="6"){
            return("n6")
        }else if(x=="7"){
            return("n7")
        }else if(x=="8"){
            return("n8")
        }else if(x=="9"){
            return("n9")
        }else if(x=="."){
            return("pt")
        }
    })
    return(char)
}

#/////////////////////////////////////////////////
#Recibimos un archivo png con el siguiente formato de nombre
#oxigenoDisuelto_promedio_micromolesPorLitro_0101199504042017.png

procesamientoDeImagenes <-function(imagPNG,newDir=FALSE,pathPPD="",HT=TRUE,nSet=FALSE){
    #setwd("~/");imagPNG <-"pruebadem.png";newDir<-TRUE;newDir<-FALSE;imagPNG <-"corrientesCostaListo.png"
    img <-readPNG(imagPNG)
    nombre <-strsplit(imagPNG,split="\\.")[[1]][1]
    
    #Eliminamos las franjas de comentarios y subtitulos del mapa.
    if(newDir==FALSE){
        suma <-apply(img[,1:10,1],1,sum)
        suma[suma<10] <-0
        corteSup <-which.min(suma)
        corteInf <-dim(img)[1]-which.min(rev(suma))+1
        
        img<-img[corteSup:corteInf,,]
    }else{
        #Buscammos el borde izquierdo del mapa.
        suma <-apply(img[101:400,1:100,1],2,sum)
        #bde <- 100-which.max(rev(suma))+1  
        bde <-which(suma != 300)[1] -1    #Ultimo blanco de izquierda a derecha.
        mIzq <-bde -4        #Cinco blancos en fila.
        mDer <-bde +5        #Cinco colores en fila.

        for(i in 150:1){
           if(sum(img[i,mIzq:bde,]) != 20 | sum(img[i,(bde+1):mDer,]) == 20){
               corteSup <-i
               break
           } 
        }
        for(i in 151:nrow(img)){
            if(sum(img[i,mIzq:bde,]) != 20 | sum(img[i,(bde+1):mDer,]) == 20){
                corteInf <-i
                break
            } 
        }
        #Cortamos
        img<-img[corteSup:corteInf,,]
        # plot(c(1,1124),c(1,867),type='n')
        # rasterImage(img, 1, 1, 1124, 867)
        # dim(img)
    }
    
    if(newDir==FALSE){
        prueba1 <-identical(as.numeric(dim(img)),c(708,1124,4))
    }else{
        prueba1 <-identical(as.numeric(dim(img)[-1]),c(1024,4)) 
        if(as.numeric(dim(img)[1]) < 530 | as.numeric(dim(img)[1]) > 535){
            prueba1 <-FALSE
        }
        img<-img[,-(1:(bde +1)),]     #Despues de validar recortamos.
    }
    #prueba1<-TRUE   #Forzar procesamiento. 
    if(prueba1 == FALSE){
        message("El codigo se detendra.\n") 
        message("Verifique que la imagen safisface los siguientes requerimientos:")
        cat("\n")
        cat("La imagen proviene de la pagina 'https://giovanni.gsfc.nasa.gov/giovanni/'.\n")
        cat("Esta acotada en la region 73W, 8N, 60W, 17N.\n")
        cat("Cumple todas las especificaciones descritas en el 'README_GIOVANNI.txt'.\n")
        if(newDir==TRUE){
            cat("No esta intentando procesar una imagen estilo 'Time Averaged Map' o 'Monthly and Seasonal Averages' en la carpeta para Animaciones.\n")
        }
        message("Si se satisfacen estos requerimientos y aun falla, comuniquese con el administrador del codigo y retire la imagen de la carpeta.\n")
        stop("ERROR: El archivo PNG no posee la dimension adecuada.")
    }
    
    #Extraccion del mapa.-------------------
    
    suma <-apply(img[1:10,,],2,sum)
    suma[suma < 40] <-0
    corteDer <-dim(img)[2]-which.min(rev(suma))+1
    
    mapa<-img[,1:corteDer,]
    
    setwd("~/Thesis Project AB/Data/Raw Data/Giovanni/Ctrl")
    png(file=paste0(nombre,"MAPA.png"))
    plot(c(1,1124),c(1,867),type='n',main=paste0("MAPA ",nombre))
    rasterImage(mapa, 1, 1, 1124, 867)
    dev.off()
    setwd("~/Thesis Project AB/Data/Raw Data/Giovanni")
    
    #Extraccion de la banda de colores.-------
    
    img<-img[,(corteDer+1):dim(img)[2],]
    
    suma<-apply(img[,,1],1,sum)
    suma[suma<dim(img)[2]]<-0
    corteSup<-which.min(suma)
    corteInf<-dim(img)[1]-which.min(rev(suma))+1
    
    suma<-apply(img[,,1],2,sum)
    suma[suma<dim(img)[1]]<-0
    corteIzq<-which.min(suma)
    corteDer<-dim(img)[2]-which.min(rev(suma))+1
    
    img<-img[corteSup:corteInf,corteIzq:corteDer,]
    
    suma<-apply(img[,,1],2,sum)
    suma[suma<(dim(img)[1]-5)]<-0
    suma[suma>=(dim(img)[1]-5)]<-dim(img)[1]
    corteDer<-which.max(suma)
    
    bandaColores<-img[,1:corteDer,]
    
    setwd("~/Thesis Project AB/Data/Raw Data/Giovanni/Ctrl")
    png(file=paste0(nombre,"colorBAND.png"))
    plot(c(1,1124),c(1,867),type='n',main=paste0("colorBAND ",nombre))         
    rasterImage(bandaColores, 1, 1, 1124, 867)
    dev.off() 
    setwd("~/Thesis Project AB/Data/Raw Data/Giovanni")
    
    #Banda superior.
    superior<-bandaColores[10:20,12:14,]
    
    setwd("~/Thesis Project AB/Data/Raw Data/Giovanni/Ctrl")
    png(file=paste0(nombre,"colSUP.png"))
    plot(c(1,1124),c(1,867),type='n',main=paste0("colSUP ",nombre))         
    rasterImage(superior, 1, 1, 1124, 867)
    dev.off() 
    setwd("~/Thesis Project AB/Data/Raw Data/Giovanni")
    
    #Banda central.
    central<-bandaColores[28:372,12:14,]
    
    setwd("~/Thesis Project AB/Data/Raw Data/Giovanni/Ctrl")
    png(file=paste0(nombre,"colCEN.png"))
    plot(c(1,1124),c(1,867),type='n',main=paste0("colCEN ",nombre))         
    rasterImage(central, 1, 1, 1124, 867)
    dev.off() 
    setwd("~/Thesis Project AB/Data/Raw Data/Giovanni")
    
    #Banda Inferior.
    inferior<-bandaColores[380:390,12:14,]
    
    setwd("~/Thesis Project AB/Data/Raw Data/Giovanni/Ctrl")
    png(file=paste0(nombre,"colINF.png"))
    plot(c(1,1124),c(1,867),type='n',main=paste0("colINF ",nombre))         
    rasterImage(inferior, 1, 1, 1124, 867)
    dev.off() 
    setwd("~/Thesis Project AB/Data/Raw Data/Giovanni")
    
    #Extraccion de la banda numerica.------------

    bandaNumerica<-img[1:(dim(img)[1]-12),(corteDer+1):dim(img)[2],]
    
    setwd("~/Thesis Project AB/Data/Raw Data/Giovanni/Ctrl")
    png(file=paste0(nombre,"Numeros.png"))
    plot(c(1,1124),c(1,867),type='n',main=paste0("Numeros ",nombre))         
    rasterImage(bandaNumerica, 1, 1, 1124, 867)
    dev.off() 
    setwd("~/Thesis Project AB/Data/Raw Data/Giovanni")
    
    cat("Plots de validacion agregados al directorio 'Raw Data/Giovanni/Ctrl'.\n")
    cat("\n")
    
    ################################################
    #Interpretacion de la banda numerica.------------
    if(nSet == FALSE){  #Si la interpretacion ya se realizo no la repetimos.
        cat("Ejecutando el interpretador de imagenes...\n")
        cat("\n")
        
        #Dividimos la banda numerica en renglones de cifras
        suma<-apply(bandaNumerica[,,1],1,sum)
        suma[suma < dim(bandaNumerica)[2]] <-0
        
        cortesH <-numeric(0)
        if(suma[1] == 0){
            cortesH <-1
        }
        for(i in 2:(length(suma)-1)){
            if(suma[i] == 0){
                if(suma[i-1] != 0){
                    cortesH<-c(cortesH,i) 
                }
                if(suma[i+1] != 0){
                    cortesH<-c(cortesH,i) 
                }     
            }
        }
        if(suma[length(suma)] == 0){
            cortesH<-c(cortesH,length(suma))
        }
        slicesSup <-cortesH[seq_along(cortesH) %% 2 == 1]
        slicesInf <-cortesH[seq_along(cortesH) %% 2 == 0]
        
        renglones<-list()
        for (i in 1:length(slicesSup)){
            renglones[[i]] <-bandaNumerica[slicesSup[i]:slicesInf[i],,1]
        }
        
        #Dividimos la cifra de cada renglon en fotos de tamano similar.
        cortesV<-function(renglon){ 
            calibrado<-9.8
            suma<-apply(renglon[,],2,sum)
            suma[suma > calibrado]<-10
            suma[suma < dim(renglon)[1]]<-0
            corteDer <-dim(renglon)[2]-which.min(rev(suma))+1
            
            renglon<- renglon[,1:corteDer]
            
            suma<-apply(renglon[,],2,sum)
            suma[suma > calibrado]<-10
            suma[suma < dim(renglon)[1]]<-0
            
            #Si hay demasiados ceros consecutivos picamos manualmente.
            cuenta<-0
            for(i in 1:length(suma)){
                if(suma[i]==0){
                    cuenta<-cuenta+1
                    if(cuenta==9){
                        renglon[,i]<-rep(1,nrow(renglon))
                        cuenta<-0
                    }
                }
                if(suma[i]!=0){
                    cuenta<-0
                }
            }
            suma<-apply(renglon[,],2,sum)
            suma[suma > calibrado]<-10
            suma[suma < dim(renglon)[1]]<-0
             
            nroCaracteres <-0
            for(i in (length(suma)-1):1){
                if(suma[i] != 0){
                    if(suma[i+1] == 0){
                        nroCaracteres <- nroCaracteres + 1
                    }
                    
                }
            }
            #Ajustamos el margen izquierdo de la imagen
            tamanoPorFoto <-floor(length(suma)/nroCaracteres)
            NroColExtras <-length(suma) - tamanoPorFoto*nroCaracteres
            
            if(NroColExtras < 0){
                stop("Incongruencia en la funcion cortesV. NroColExtras < 0!.")
            }  
            
            renglon<- renglon[,(1+NroColExtras):dim(renglon)[2]]
            suma<-apply(renglon[,],2,sum)
            suma[suma > calibrado]<-10
            suma[suma < dim(renglon)[1]]<-0
            
            #Cortamos el renglon para generar tantas fotos
            #de igual tamano como numeros hayan.
            cortes<-which(suma != 0)
            cortes<-c(cortes,length(suma))
    
            j<-1
            grupos <-numeric(0)
            for(i in 1:(length(cortes)-1)){
                if(cortes[i+1] == (cortes[i]+1)){
                    grupos[i] <- cortes[j]
                }else{
                    grupos[i] <- cortes[j]
                    j<-i+1
                }
            }
            grupos[length(cortes)] <- cortes[length(cortes)]
            grupos<-as.factor(grupos)
            u<-tapply(cortes,grupos,function(x){max(x)})
            grupos<-sapply(grupos,function(x){unname(u[as.character(x)])})
    
            cortes<-unique(grupos)
            if(suma[1] == 0){
                cortes<-c(1,cortes)
            }
            cortes[1]<-cortes[1]-1      #Damos formato para crear los slices.
            
            if(length(cortes) != nroCaracteres +1){
                stop("Incongruencia en la funcion cortesV.length(cortes) != nroCaracteres +1!")
            }
            slicesIzq<-cortes[-length(cortes)]+1
            slicesDer<-cortes[-1]
            
            cifras <-list()
            for(i in 1:nroCaracteres){
                renglonAux<-renglon[,slicesIzq[i]:slicesDer[i]]
                d <-dim(renglonAux)[2]
                p <-dim(renglonAux)[1]
                suma<-apply(renglonAux,2,sum)
                suma[suma > calibrado]<-10
                suma[suma < p]<-0
                corteIzq <-which.min(suma)
                corteDer <- d- which.min(rev(suma)) +1
                renglonAux <-renglonAux[,corteIzq:corteDer]
                
                if(d < 7){
                    m<-rep(1,times=10*(7-d))
                    dim(m)<-c(10,(7-d))
                    renglonAux<-cbind(m,renglonAux)
                }
                cifras[[i]] <-renglonAux
            }
            return(cifras)
        }
        listaDeNrosDesagrupados <-lapply(renglones,cortesV)
        
        ReTrainDFs<-list()
        
        listaImgAnum <-function(bolsaDeNum){
            prueba <-t(as.data.frame(lapply(bolsaDeNum,imagenTovector)))
            prueba <-as.data.frame(prueba)
            row.names(prueba) <- 1:nrow(prueba)
            ReTrainDFs[[length(ReTrainDFs)+1]] <<-prueba
            pred<-predict(modFit,prueba)
            if(sum(pred!=1 & pred!=0) != 0){
                warning("El interpretador esta recibiendo datos ambiguos.")
                pred <-t(apply(pred,1,function(x){
                    posMax <-which.max(x)
                    x[posMax] <-1; x[-posMax] <-0; return(x)
                    }))
            }
            predTonum(pred)
        }
        #Conservamos la variable 'numeros' usando superasignacion.
        numeros <-lapply(listaDeNrosDesagrupados,listaImgAnum)
        numeros <-sapply(numeros,function(x){x[[1]]}) 
        numeros <<-numeros    #Superasignacion
        
        if(length(ReTrainDFs) != length(numeros)){
            stop("El data set de reentrenamiento no tiene la longitud adecuada.")
        }
        
        setwd("~/Thesis Project AB/Data/Raw Data/Giovanni/Ctrl")
        sink(paste0(nombre,"NumCtrl.txt"))
        print(numeros)
        sink()
        setwd("~/Thesis Project AB/Data/Raw Data/Giovanni")
        
        #Test automatico de numeros generados.
        if(sum(is.na(numeros)) != 0){
            message("El codigo se detendra.\n") 
            message("Verifique que la imagen safisface los siguientes requerimientos:")
            cat("\n")
            cat("La imagen proviene de la pagina 'https://giovanni.gsfc.nasa.gov/giovanni/'.\n")
            cat("Esta acotada en la region 73W, 8N, 60W, 17N.\n")
            cat("Cumple todas las especificaciones descritas en el 'README_GIOVANNI.txt'.\n")
            message("Si se satisfacen estos requerimientos y aun falla, comuniquese con el administrador del codigo.\n")
            stop("La imagen no pudo ser procesada correctamente. NA's detectados.")
        }    
        if(sum(diff(rev(numeros)) <= 0) != 0){
            #Programar un reentrenamiento automatico
            message("La imagen no pudo ser procesada correctamente.")
            message("La escala numerica generada por el interpretador no es monotona.")
            cat("Verifique que la imagen safisface los siguientes requerimientos:\n")
            cat("\n")
            cat("La imagen proviene de la pagina 'https://giovanni.gsfc.nasa.gov/giovanni/'.\n")
            cat("Esta acotada en la region 73W, 8N, 60W, 17N.\n")
            cat("Cumple todas las especificaciones descritas en el 'README_GIOVANNI.txt'.\n")
            cat("\n")
            
            validoIni <-FALSE
            while(validoIni == FALSE){
                iptIni<- readline(prompt="La imagen satisface los requerimientos?. Introduzca Si o No: ")
                if(iptIni %in% c("NO","No","no","SI","Si","si")){
                    validoIni <-TRUE
                }else{
                    message("Input invalido!")
                }
            }
            if(iptIni %in% c("NO","No","no")){
                stop("Se suspende la ejecucion del programa.",call.=FALSE)
            }
            message("Ejecutando el reentrenamiento automatico...")
            sapply(numeros,function(x){print(x)})
            plot(c(1,1124),c(1,867),type='n',main="Validacion de Escala.")
            rasterImage(img, 1, 1, 1124, 867)
            setwd("~/Thesis Project AB/R-Objects/Train Inter")       
            entrenador<-read.table("entrenador.txt",header = TRUE)
            
            for (i in 1:length(ReTrainDFs)){
                validok <-FALSE
                while(validok == FALSE){            #Indique si el numero es correcto
                    cat("\n")
                    message("Indique si el numero ",numeros[i]," es correcto.")
                    iptk<- readline(prompt="Introduzca Si o No: ")
                    if(iptk %in% c("NO","No","no","SI","Si","si")){ 
                        validok <-TRUE
                    }else{                                          
                        message("Input invalido!")
                    }
                }
                if(iptk %in% c("NO","No","no")){     #NO, no es correcto!!
                    validokk <-FALSE
                    while(validokk == FALSE){         #Cual es el numero entonces?
                        iptkk <-readline(prompt="Introduzca el numero correcto : ")
                        if(is.na(as.numeric(iptkk)) == FALSE){
                            validokk <-TRUE
                        }else{
                            message("El Input debe ser numerico.")
                        }
                    }
                    predReal <-numTopred(iptkk)
                    if(length(predReal) != nrow(ReTrainDFs[[i]])){   #El error es de programacion.
                        cat("/n")
                        stop("Este error no puede ser corregido por reentrenamiento. Comuniquelo al administrador del codigo.")
                    }
                    ReTrainDFs[[i]]$num <-predReal                   
                    entrenador<-rbind(entrenador,ReTrainDFs[[i]])
                }else{                                  #Si, si si es correcto.
                    message("Ejecutado.")
                }
            }
            message("Respuestas registradas. Reentrenando el modelo...")
            
            setwd("~/Thesis Project AB/R-Objects/Train Inter") 
            file.remove("entrenador.txt")
            write.table(entrenador,file="entrenador.txt",row.names = FALSE)
            nota <-"Colgar en Github el entrenador de esta carpeta.Fue actualizado!."
            write.table(nota,file="NOTA_ACTUALIZAR_EN_GITHUB.txt",row.names = FALSE)
            
            setwd("~/Thesis Project AB/R-Objects")
            file.remove("interpretador.RData")
            
            setwd("~/Thesis Project AB/R-Objects/Train Inter")
            entrenador<-read.table("entrenador.txt",header = TRUE)
            modFit<-interBuilder(entrenador)    
            setwd("~/Thesis Project AB/R-Objects")
            save(modFit,file = "interpretador.RData")
            
            setwd("~/Thesis Project AB/Data/Raw Data/Giovanni")
            message("Reentrenamiento finalizado.")
            cat("\n")
            stop("Ejecute el programa nuevamente para incluir los cambios.",call.=FALSE)
        }
    }
    #Fin de la interpretacion numerica
    #####################################
    
    #Validando la interpretacion
    if(HT==TRUE){
        #Validacion por parte del usuario de los numeros generados.
        message("Indique si los valores mostrados en pantalla coinciden con la escala numerica de la imagen original.")
        sapply(numeros,function(x){print(x)})
        
        plot(c(1,1124),c(1,867),type='n',main="Validacion de Escala.")
        rasterImage(img, 1, 1, 1124, 867)
        
        valido <-FALSE
        while(valido == FALSE){
            ipt<- readline(prompt="Los numeros son correctos?. Introduzca Si o No: ")
            if(ipt %in% c("NO","No","no","SI","Si","si")){
                valido <-TRUE
            }else{
                message("Input invalido!")
            }
        }
        if(ipt %in% c("NO","No","no") == TRUE){
            cat("\n")
            message("Se ha registrado su reporte de error.") 
            message("Antes de continuar verifique que la imagen safisface los siguientes requerimientos:")
            cat("\n")
            cat("La imagen proviene de la pagina 'https://giovanni.gsfc.nasa.gov/giovanni/'.\n")
            cat("Esta acotada en la region 73W, 8N, 60W, 17N.\n")
            cat("Cumple todas las especificaciones descritas en el 'README_GIOVANNI.txt'.\n")
            message("Si la imagen satisface esos requerimientos el problema lo presenta el interpretador.")
            message("En este caso se recomienda aceptar la solicitud de reentrenamiento del mismo.")
            message("Ejecute esta accion solo si la imagen original cumple las especificaciones solicitadas.\n")
            valido0 <-FALSE
            while(valido0 == FALSE){         #Deseamos reentrenar el interpretador?.
                ipt0<- readline(prompt="Desea reentrenar el algoritmo de reconocimiento?. Introduzca Si o No: ")
                if(ipt0 %in% c("NO","No","no","SI","Si","si")){
                    valido0 <-TRUE
                }else{
                    message("Input invalido!")
                }
            }
            if(ipt0 %in% c("SI","Si","si") == TRUE){      #Positivo a reentrenar!!.
                setwd("~/Thesis Project AB/R-Objects/Train Inter")       
                entrenador<-read.table("entrenador.txt",header = TRUE)
                
                for (i in 1:length(ReTrainDFs)){
                    valido2 <-FALSE
                    while(valido2 == FALSE){            #Indique si el numero es correcto
                        cat("\n")
                        message("Indique si el numero ",numeros[i]," es correcto.")
                        ipt2<- readline(prompt="Introduzca Si o No: ")
                        if(ipt2 %in% c("NO","No","no","SI","Si","si")){ 
                            valido2 <-TRUE
                        }else{                                          
                            message("Input invalido!")
                        }
                    }
                    if(ipt2 %in% c("NO","No","no")){     #NO, no es correcto!!
                        valido3 <-FALSE
                        while(valido3 == FALSE){         #Cual es el numero entonces?
                            ipt3 <-readline(prompt="Introduzca el numero correcto : ")
                            if(is.na(as.numeric(ipt3)) == FALSE){
                                valido3 <-TRUE
                            }else{
                                message("El Input debe ser numerico.")
                            }
                        }
                        predReal <-numTopred(ipt3)
                        if(length(predReal) != nrow(ReTrainDFs[[i]])){   #El error es de programacion.
                            cat("/n")
                            stop("Este error no puede ser corregido por reentrenamiento. Comuniquelo al administrador del codigo.")
                        }
                        ReTrainDFs[[i]]$num <-predReal                   
                        entrenador<-rbind(entrenador,ReTrainDFs[[i]])
                    }else{                                  #Si, si si es correcto.
                        message("Ejecutado.")
                    }
                }
                message("Respuestas registradas. Reentrenando el modelo...")
                
                setwd("~/Thesis Project AB/R-Objects/Train Inter") 
                file.remove("entrenador.txt")
                write.table(entrenador,file="entrenador.txt",row.names = FALSE)
                nota <-"Colgar en Github el entrenador de esta carpeta.Fue actualizado!."
                write.table(nota,file="NOTA_ACTUALIZAR_EN_GITHUB.txt",row.names = FALSE)
                
                setwd("~/Thesis Project AB/R-Objects")
                file.remove("interpretador.RData")
                
                setwd("~/Thesis Project AB/R-Objects/Train Inter")
                entrenador<-read.table("entrenador.txt",header = TRUE)
                modFit<-interBuilder(entrenador)    
                setwd("~/Thesis Project AB/R-Objects")
                save(modFit,file = "interpretador.RData")
                
                setwd("~/Thesis Project AB/Data/Raw Data/Giovanni")
                message("Reentrenamiento finalizado.")
                cat("\n")
                stop("Ejecute el programa nuevamente para incluir los cambios.",call.=FALSE)
                
            }else{                                    #Negativo, no reentrenemos.
                cat("\n")
                message("El programa se detendra.")
                message("Remueva la imagen de la carpeta 'Giovanni' o pongase en contacto con el administrador del codigo.\n")
                stop("Los valores generados no fueron aprobados por el usuario.")
            }
        }else{
            cat("Se continua el procesamiento...\n")
            cat("\n")
        }
    }
    #Continua el procesamiento de la imagen.
    
    #Colapsamos banda de colores
    if(identical(central[,1,],central[,2,]) == FALSE){
        stop("Banda central de colores inconsistente.")
    }
    central <-central[,1,]
    central <-as.data.frame(central)
    
    if(identical(superior[,1,],superior[,2,]) == FALSE){
        stop("Banda superior de colores inconsistente.")
    }
    superior <-superior[,1,] 
    superior <-superior[1,] 
    
    if(identical(inferior[,1,],inferior[,2,]) == FALSE){
        stop("Banda inferior de colores inconsistente.")
    }
    inferior <-inferior[,1,]
    inferior <-inferior[1,]
    
    #Agregamos las medidas numericas a las bandas de colores.
    cat("Agregando las medidas numericas a la bandas de colores.\n")
    cat("\n")
    
    #A la superior
    superior <-c(superior,numeros[1])
    #A la inferior
    inferior <-c(inferior,(numeros[length(numeros)]-1))

    clave <-paste0(central[,1],central[,2],central[,3],central[,4])
    
    #length(unique(clave)) representa el numero real de colores en la escala.
    
    #Escala discreta de colores////////////////////
    if(length(unique(clave)) >= (length(numeros)-1) & 
                            length(unique(clave)) <= (2*(length(numeros)-1)-1)){
        #A la central
        numeros <-numeros[-1]
        clave <-names(table(clave)[table(clave) > 30])
        central <-central %>% unique()
        central$clave <-paste0(central[,1],central[,2],central[,3],central[,4])
        
        central <-central[central$clave %in% clave,]
        central$clave <-NULL
        if(nrow(central) != length(numeros)){
            stop("Incongruencia en la asignacion de medidas a la banda de colores discreta.")
        }
        central$medidas <-numeros
    }else{
    #Escala continua de colores////////////////////
        #A la central
        numeros<-rev(numeros)
        central <-central %>% unique()
        rango <-round(nrow(central)/(length(numeros)-1))
        
        escala <-numeric(0)
        for(i in 1:(length(numeros)-2)){
            escala <-c(escala,seq(numeros[i],numeros[i+1],length = rango+1))	
        }
        escala <-escala %>% unique()
        long <-length(numeros)
        rango <- nrow(central) - length(escala)
        escala <-c(escala,seq(numeros[long-1],numeros[long],length=rango+1))
        escala <-escala %>% unique()
        
        if(length(escala) != nrow(central)){
            stop("El rango de medidas generado no coincide con el rango de colores.")
        }
        if(sum(diff(escala) <= 0) != 0){
            stop("El rango de medidas generado no es monotono creciente.")
        }
        if(escala[1] != numeros[1] | escala[length(escala)] != numeros[long]){
            stop("Los extremos de las escalas no coinciden.")
        }
        central$medidas <-rev(escala)
    }
    
    #Unificando dataset de asignacion
    
    central[2:(nrow(central)+1),] <-central  #EL maximo y el minimo representan
    central[1,] <-superior                   #mayor o igual, menor o igual.
    central[nrow(central)+1,] <-inferior
    
    clave <-paste(central[,1],central[,2],central[,3],central[,4],sep=" ")
    asignacion <-data.frame(clave = clave, medidas = central$medidas)
    asignacion$clave<-as.character(asignacion$clave)
    asignacion[nrow(asignacion)+1,] <-c("0 0 0 0",NA)    #Fondo Blanco
    
    #Creando mapa con medidas.
    cat("Creando tabla con los datos del mapa...\n")
    cat("\n")
    
    nrofMapa<-dim(mapa)[1]
    scanner <-function(v){
        dim(v) <-c(nrofMapa,4)   #Originalmente length(v) = 2832
        #v<-as.data.frame(v)
        cla <-as.character(paste(v[,1],v[,2],v[,3],v[,4],sep=" "))
        for(i in 1:length(cla)){
            if(cla[i] %in% asignacion$clave == FALSE){
                cla[i]<-"0 0 0 0"
            }   
        }
        return(cla)
    }
    listaClaves<-apply(mapa,2,scanner) #Mapa con claves 
    
    mapaNum <-apply(listaClaves,2,function(c){
        unname(sapply(c,function(x){asignacion$medidas[asignacion$clave == x]}))
    })
    
    #Creamos mapa numerico
    mapaNum <-as.data.frame(mapaNum)
    mapaNum <-as.data.frame(apply(mapaNum,2,as.numeric)) 
    
    latitude<-seq(17,8,length=dim(mapaNum)[1])
    latitude<-rep(latitude,ncol(mapaNum))
    latitude<-sapply(latitude,function(x){round(x,3)})
    longitude<-seq(-73,-60,length=dim(mapaNum)[2])
    longitude<-rep(longitude,each=nrow(mapaNum))
    longitude<-sapply(longitude,function(x){round(x,3)})
    
    #Transformamos mapa numerico en dataset para su exportacion.
    mapaNum <-as.matrix(mapaNum)
    dim(mapaNum) <-c(dim(mapaNum)[1]*dim(mapaNum)[2],1)
    archivo <-data.frame(latitude=latitude,longitude=longitude,medida=mapaNum)
    
    #Exportamos tabla
    if(newDir==FALSE){
        setwd("~/Thesis Project AB/Data/Pre-processed Data")
        file <-paste0("GIO.",nombre,".csv")
        write.csv(archivo,file=file,row.names=FALSE)
        setwd("~/Thesis Project AB/Data/Raw Data/Giovanni")
        
        message("Archivo '",paste0(nombre,".png'")," procesado exitosamente.\n")
        cat("Se agrego la tabla '",file,"' al directorio Pre-processed Data.\n")
        cat("\n")
    }else{
        setwd(pathPPD)
        file <-paste0(nombre,".csv")
        write.csv(archivo,file=file,row.names=FALSE)
        
        message("Archivo '",paste0(nombre,".png'")," procesado exitosamente.\n")
        cat("Se agrego la tabla '",file,"' al directorio Pre-processed Data\n")
        cat("carpeta: '",strsplit(pathPPD,"\\/")[[1]][
                                 length(strsplit(pathPPD,"\\/")[[1]])],'.\n')
        cat("\n")
    }
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
impute.per.region <- function(data,radio=1,medida="median",rango=0,CLS="inter"){
    
    #Precondicion
    if(ncol(data) != 3){
        stop("El data frame ingresado no tiene exactamente 3 variables.")
    }
    if(length(rango) != 1){
        if(length(rango) > 2){
            stop("El rango no es un vector de dimension 2.")
        }
        if(any(rango <= 0) | any(rango > nrow(data)) | rango[1] >= rango[2]){
            stop("El rango ingresado es invalido.")
        }
    }
    if(length(unique(paste(data$latitude,data$longitude))) != nrow(data)){
        stop("La data ingresada tiene puntos de geolocalizacion duplicados.")
    }
    if(sum(is.na(data$latitude)) != 0){
        stop("La variable 'latitude' tiene valores perdidos")
    }
    if(sum(is.na(data$longitude)) != 0){
        stop("La variable 'longitude' tiene valores perdidos")
    }
    #Extraemos la dimension original del dataset.
    originalDim <-dim(data)
    
    #Reordenamos el data frame ingresado y damos formato.
    varName <- names(data)[!(names(data) %in% c("latitude","longitude"))]
    data <- data[c("latitude","longitude",varName)]
    data$latitude <- as.numeric(as.character(data$latitude)) 
    data$longitude <- as.numeric(as.character(data$longitude)) 
    if(CLS !="inter"){
        if(CLS == "numeric" | CLS == "integer"){
            data[,3] <- as.numeric(as.character(data[,3]))
        }else{
            data[,3] <- as.character(data[,3])
        }
    }
    #Establecemos region de busqueda y region de correccion.
    RegionB <- data
    RegionC <- data
    if(length(rango) != 1){
        RegionC <- RegionC[rango[1]:rango[2],]
    }

    #Eliminamos de RegionB las filas con NAs.
    RegionB <- RegionB[!is.na(RegionB[,3]),]
    
    #Buscamos las posiciones de los vecinos de cada punto de RegionC.
    #Solo consideraremos aquellos puntos que requieran ser imputados. 
    if(class(RegionB[,3]) != "numeric" & class(RegionB[,3]) != "integer"){
        vecinos <-as.list(unname(apply(RegionC,1,function(x){
            if(!is.na(x[3])){
                return(NA)   #Decimos que no tiene vecinos.
            }
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
            #Posiciones de RegionB donde estan los vecinos.
            posiciones <- which(distancias <= radio & distancias != 0)
            if(length(posiciones) == 0){ #Reemplazamos integer(0) por NA
                posiciones <-NA
            }
            return(posiciones)
        })))
    }else{
        vecinos <-as.list(unname(apply(RegionC,1,function(x){
            if(!is.na(x[3])){
                return(NA)   #Decimos que no tiene vecinos.
            }
            #Distancias del punto de RegionC al resto
            distancias <-unname(apply(RegionB,1,function(y){
                d <-distGL(c(y[1],y[2]),c(x[1],x[2]))
                return(d)
            }))
            #Check
            if(class(distancias) != "numeric"){
                stop("Las distancias no son numericas.")
            }
            if(any(is.na(distancias))){
                stop("Se generaron NA's en el calculo de distancias.")
            }
            #Posiciones de RegionB donde estan los vecinos.
            posiciones <- which(distancias <= radio & distancias != 0)
            if(length(posiciones) == 0){ #Reemplazamos integer(0) por NA
                posiciones <-NA
            }
            return(posiciones)
        })))
    }

    
    #Transformamos las posiciones de los vecinos en puntos de geolocalizacion.
    RegionB$clave <-paste(RegionB$latitude,RegionB$longitude,sep="_")
    vecinos <-lapply(vecinos,function(x){    
        if(is.na(x[1])){                    
            return(NA)
        }else{
            s <-RegionB$clave[x]
            return(s)
        }
    })
    #Check
    if(names(RegionB)[3] %in% c("latitude","longitude","clave")){
        stop("El data set RegionB se desorganizo.")
    }
    
    #Una vez localizados los puntos vecinos dentro del radio esperado imputamos.
    RegionC$sec <- seq(1:nrow(RegionC))
    
    #Check
    if(identical(names(RegionC)[-4],names(data)) == FALSE){
        stop("El dataset RegionC se desorganizo.")
    }
    if(identical(names(RegionB)[-4],names(data)) == FALSE){
        stop("El dataset RegionB se desorganizo.")
    }
    if(length(vecinos) != nrow(RegionC)){
        stop("length(vecinos) != nrow(RegionC)")
    }

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
                
                #Check
                if(any(is.na(valores))){
                    stop("RegionB tras la depuracion tiene NA's.")
                }
                #Si la variable a imputar es catgorica...........
                if(class(data[,3]) !="numeric" & class(data[,3]) !="integer"){ 
                    t <- table(valores)
                    moda <- names(t[which(t == max(t))])
                    if(length(moda) > 1){
                        warning("Ambiguedad al imputar la variable categorica. La moda no es unica.",call.=FALSE)
                    }
                    return(moda[1])
                }
                #Si la variable a imputar es numerica............
                if(medida == "median"){                      #Mediana.
                    y[3] <-median(valores)
                }else if(medida == "mean"){                  #Media ponderada.
                    #Geolocalizacion del NA.
                    latNA <-RegionC$latitude[y4]
                    lonNA <-RegionC$longitude[y4]
                    
                    #Geolocalizacion de los vecinos del NA en RegionB.
                    geoVeci <- RegionB[ubic,c(1,2)]
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
                    #Reordenamos de mayor distancia a menor
                    valores <-valores[order(dis,decreasing = T)]
                    dis <-sort(dis,decreasing = T)
                    
                    #Generamos pesos
                    maxi <-max(dis)+1
                    total <-sum(abs(maxi-dis))
                    pesos <-abs(maxi-dis)/total
                    if(abs(sum(pesos)-1) > 0.000001){
                        stop("La suma de los pesos no es 1.")
                    }
                    
                    #Creamos media ponderada.
                    y[3] <-round(sum(pesos*valores),4)
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
    if(length(rango) != 1 & length(correctedVar) != (rango[2] - rango[1] + 1)){
        stop("El vector 'correctedVar' tiene una cantidad incorrecta de filas.")
    }

    #Reescribimos la variable imputada.
    RegionC[,3] <- correctedVar
    RegionC$sec <- NULL
    
    #Damos formato de salida a la variable creada.
    if(class(data[,3]) %in% c("factor","character","logical")){
        RegionC[,3] <- as.character(RegionC[,3])
        data[,3] <- as.character(data[,3])
    }else if(class(data[,3]) %in% c("integer","numeric")){
        RegionC[,3] <- as.numeric(as.character(RegionC[,3]))
        data[,3] <- as.numeric(as.character(data[,3]))
    }else{
        stop("Clase de variable no definida.")
    }

    #Agregamos los datos nuevos al dataset original
    if(length(rango) == 1){         
        data <- cbind(data[c("latitude","longitude")],RegionC[,3])
    }else{
        data[rango[1]:rango[2],] <- RegionC
    }
    
    #Variables categoricas como factores.
    if(class(data[,3]) == "character"){
        data[,3] <- as.factor(data[,3])
    }
    
    #Postcondicion check
    if(identical(as.numeric(dim(data)),as.numeric(originalDim)) == FALSE){
        stop("El dataset imputado no tiene la misma dimension que el original.")
    }

    #Exportamos tabla corregida.
    return(data)
}

#/////////////////////////////////////////////////
#Recibe un dataframe al q se le quieren agregar variables con valores imputados
#"dat" y otro donde estan las variables que se desean anexar "imputador". Estos
#datos estan ausentes porque los puntos de geolocalizacion no coinciden.
#LAS VARIABLES DEL IMPUTADOR NO EXISTEN EN DAT SALVO LAT Y LON.
#EL IMPUTADOR DEBE TENER UN UNICO VALOR PARA CADA TRIO LAT-LON-DEPTH.
impute.per.region2 <- function(dat,imputador,radio=c(1,5,10),medida="median",
                                                        varName=0,Google=FALSE){
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
        lenIni <- nrow(imputador)
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
    
    #Seleccionamos la variable que se desea anexar en caso de ser una sola.
    if(varName != 0){
        if(varName %in% names(imputador) == TRUE){
            imputador <- imputador[c(loc,varName)] 
            imputedVars <- varName
        }else{
            message("La variable '",varName,"' no pertenece al conjunto de datos. 
            Se procesaran todas las variables.")   
            imputedVars <- names(imputador)[!(names(imputador) %in% c(loc))]
        }
    }else{
        imputedVars <- names(imputador)[!(names(imputador) %in% c(loc))]
    } 

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
    #merge. Estos datos ya recibieron su valor respectivo por geolocalizacion.
    imputador <- imputador[!(imputador$clave %in% dat$clave),]
    
    #Check sobre tablas de Google Earth.
    if(Google == TRUE & nrow(imputador) != 0){
        stop("El imputador para datos de Google Earth tiene filas que no se procesa por merge.")
    }
    
    #Si el imputador quedo vacio, merge final y exportamos.
    if(nrow(imputador) == 0){
        datos$clave <- paste(datos$latitude,datos$longitude)
        dat$clave <- paste(dat$latitude,dat$longitude)
        datos <- merge(datos,dat[!(names(dat) %in% c("latitude","longitude"))],
                       by="clave",all.x=T)
        datos$clave <- NULL
        
        return(datos)
    }

    #Reordenamos preparando para el rbind.
    imputador <- imputador[,names(dat)]
    
    #Check.
    if(length(intersect(dat$clave,imputador$clave)) != 0){
        stop("Los dataset imputador y dat tienen filas con claves repetidas.")
    } 

    #Valor de clave que permite identificar el dataset original.
    dat$clave <- rep(NA,times=nrow(dat))
    
    #Ordenamos y unimos por rbind
    dat <- rbind(imputador,dat)
    
    #Unique(clave-datos).
    claveCheck <- unique(paste(datos$latitude,datos$longitude)) 
    #Unique(clave-dat).
    claveCheck2 <- paste(dat$latitude,dat$longitude)    
    
    #Check
    if("depth.m" %in% loc == TRUE){
        if(sum(is.na(dat$depth.m)) > 0){
            stop("El dataset 'dat' creado por rbind tiene NA's en 'depth.m'.")
        }
        if(sum(is.na(dat$clave)) != (length(claveCheck)*length(profundidades))){
            stop("La region de datos a imputar no coincide con la original.")
        }
    }else{
        if(sum(is.na(dat$clave)) != length(claveCheck)){
            stop("La region de datos a imputar no coincide con la original.")
        }  
    }
    if(any(!is.na(dat$clave[claveCheck2 %in% claveCheck]))){
        stop("El vector dat$clave tiene valores no nulos en lugares no esperados.")
    }
    if(any(is.na(dat$clave[!(claveCheck2 %in% claveCheck)]))){
        stop("El vector dat$clave presenta NA's en posiciones no esperadas.")
    }
    
    #liberamos memoria
    rm(imputador);rm(claveCheck);rm(claveCheck2)
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
                    #lapply funcionara como un procedimiento no como una funcion.
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
            datAux2 <- dat[datAux2$depth.m == profundidades[np],]
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
         dataVarList <- list()
         f_prodd <- sapply(imputedVars,function(impVar){
            datAux <- dat[c("clave","latitude","longitude",impVar)]
            datAux <- datAux[is.na(datAux[,1]) | 
                                 (!is.na(datAux[,1]) & !is.na(datAux[,4])),]
            
            #Si hay datos disponibles para hacer la imputacion.
            ejecutar <- FALSE
            if(nrow(datAux) != sum(is.na(dat$clave))){
                ##Establecemos rango.
                rango <- c(nrow(datAux)-sum(is.na(dat$clave))+1,nrow(datAux))
                
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
                    df$clave <- datAux$clave
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
                #lapply funcionara como un procedimiento no como una funcion.
                f_prod21 <- lapply(dataList,function(df){
                    datAux[,1][is.na(datAux[,1])] <<- df[,1][is.na(datAux[,1])]
                })
                rm(f_prod21)
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
        
        #Preparamos la salida en dat dandole formato.
        dat <- dat[is.na(dat$clave),]
        dat$clave <- NULL

        #Reemplazamos la variables de dat por las nuevas conseguidas.
        #lapply funcionara como un procedimiento no como una funcion.
        f_prod <- lapply(dataVarList,function(datAux){
            var <- names(datAux)
            #Check
            if(length(var) != 1){
                stop("Los data frames de dataVarList tienen mas de una variable.")
            }
            #Check
            if(nrow(datAux) != nrow(dat)){
                stop("datAux y dat no tienen el mismo numero de filas.")
            }
            dat[var][,1] <<- datAux[var][,1]
        })
        rm(f_prod)
        gc()
    }
    
    #El data frame de salida listo para el merge con datos se llama dat y 
    #esta altura ya fue procesado totalmente.
    
    #Claves
    claveDatos <- paste(datos$latitude,datos$longitude)
    claveDat <- paste(dat$latitude,dat$longitude)
    
    #Check
    if("depth.m" %in% loc == TRUE){
        if(nrow(dat) != (length(unique(claveDatos))*length(profundidades))){
            stop("El numero de filas del dataset imputado no es el esperado.")
        }
    }else{
        if(nrow(dat) != length(unique(claveDatos))){
            stop("El numero de filas del dataset imputado no es el esperado.")
        }
    }
    if(length(unique(claveDat) ) != length(unique(claveDatos))){
        stop("El numero de puntos distintos de geolocalizacion de dat y datos no coinciden.")
    }
    if(any(!(unique(claveDat) %in% unique(claveDatos)))){
        stop("Hay datos de geolocalizacion en las tablas dat y datos distintos.")
    }
    
    #Claves para el merge final.
    datos$clave <- claveDatos
    dat$clave <- claveDat
    datos <- merge(datos,dat[!(names(dat) %in% c("latitude","longitude"))],
                   by="clave",all.x=T)
    datos$clave <- NULL
    
    #Salida de datos
    return(datos)
}

#/////////////////////////////////////////////////
#Media movil
mm <- function(vect,ventana=10){
    nro <- ventana %/% 2
    for(i in nro:(length(vect) - nro)){
        vect[i] <- mean(vect[(i-(nro-1)):(i+nro)],na.rm = TRUE)
    }
    return(vect)
}
	
#/////////////////////////////////////////////////
#Funcion para unir los datos de Google Earth con la matriz biologica.
#En este caso tablaGE es la tabla creada con Google y LatLonTB el dataset con los
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
                  lon.objeto = lonObj ,objeto = objt,tipo = med,valor = dist2)

        #Solo retornamos datasets con datos.
        if(nrow(datosPtoi) != 0){
            return(datosPtoi)
        }
    }))
    #Unificamos los datos en un solo data frame por rbind.
    dataDistLin <- data.frame(latitude = NA,longitude = NA,lat.objeto = NA,
                     lon.objeto = NA,objeto = NA , tipo = NA,valor = NA)
    #Procedimiento
    s <- lapply(distLin,function(df){  #Agregamos solo data frames con datos.
        dataDistLin <<- rbind(dataDistLin,df) 
    })
    
    #Limpiamos memoria
    rm(s);rm(distLin)
    gc()
    
    #Removemos los NA's generadosy creamos el dataset de salida.
    dataDistLin <- dataDistLin[!is.na(dataDistLin$lat.objeto),]  
    
    #Tiempo de viaje en corrientes.--------------------
    cat("Calculando tiempos de viaje en corriente...\n")
    
    LCCC <- read.csv("~/Thesis Project AB/Data/Pre-processed Data/LineaCosteraConCorrientes.csv")
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
                           LCCC$longitude >= LonIzq,c(1,2,3)] #lat,lon,posicion.
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
        
        if(class(posPtoCosta) == "character" | class(posPtoCosta) == "factor"){
            stop("La clase de posPtoCosta es incorrecta.")
        }
        
        #En la posicion 19 esta la boca del lago.
        #En la posicion 69 esta la boca del lago.
        #En la posicion 167 esta la boca del golfo de cariaco.
        #En la posicion 176 esta la boca del golfo de cariaco.  
        #Buscamos de izquiera a derecha por pares.
        #Siempre buscamos contra corriente.
        if(1 <= posPtoCosta & posPtoCosta <= 45){          #Del 1 al  19 =  RE
            reco <- posPtoCosta : 1
        }else if(46 <= posPtoCosta  & posPtoCosta <= 87){  #Del 46 al  87 =  RP
            reco <- posPtoCosta : 87
        }else if(88 <= posPtoCosta  & posPtoCosta <= 95){  #Del 88 al  95 =  RE
            reco <- posPtoCosta : 87
        }else if(96 <= posPtoCosta  & posPtoCosta <= 171){ #Del 96 al 171 =  RP
            reco <- posPtoCosta : 171
        }else if(172 <= posPtoCosta & posPtoCosta <= 177){ #Del 172 al 177 =  RE
            reco <- posPtoCosta : 171
        }else if(178 <= posPtoCosta & posPtoCosta <= 200){ #Del 178 al 200 =  RP
            reco <- posPtoCosta  :200
        }else if(201 <= posPtoCosta & posPtoCosta <= 210){ #Del 201 al 210 =  RE
            reco <- posPtoCosta : 200
        }else if(211 <= posPtoCosta & posPtoCosta <= 215){ #Del 211 al 215 =  RP
            reco <- posPtoCosta : 215
        }else if(216 <= posPtoCosta & posPtoCosta <= 218){ #Del 216 al 218 =  RE
            reco <- posPtoCosta : 215   
        }else if(219 <= posPtoCosta & posPtoCosta <= 255){ #Del 219 al 255 =  RP
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
                tAcumulado <-c(0,LCCC$tiempoViaje.Adj.dias[reco][-length(reco)])
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
        #Procesamos la lista donde estan los datasets de busqueda en funcion de 
        #la amplitud del recorrido. La lista contiene 6 dataframes. En la
        #posicion 1 estan las ubicaciones mas cercanas a los puntos de recorrido
        #sobre la linea costera y en la ultima posicion estan los mas lejanos.
        #Este nivel de cercania o lejania esta acotado a maximo lat +- 0.06 y
        #lon +- 0,06
        listaDF <- list()
        for(i in 1:6){
            #Creamos lista con el conjunto de datasets.
            listaRec <- apply(dfReco,1,function(r){
                m <- 0.01*i
                #Recolectamos por zona los puntos no tratados.
                #Contiene los puntos de geolocalizacion de los objetos.
                zona <- tablaGE[tablaGE$latitude  <= LCCC$latitude[
                                    as.numeric(as.character(r[1]))] + m &
                                tablaGE$latitude  >= LCCC$latitude[
                                    as.numeric(as.character(r[1]))] - m &
                                tablaGE$longitude <= LCCC$longitude[
                                    as.numeric(as.character(r[1]))] + m &
                                tablaGE$longitude >= LCCC$longitude[
                                    as.numeric(as.character(r[1]))] - m &
                                tablaGE$elect == FALSE,]

                #Asignamos ubicaciones e impedimos su reasignacion.
                #Si la variable "signo" es negativa indica que estamos 
                #empleando la distancia con el punto que esta en la 
                #posicion previa porque no hay sucesores en el recorrido.
                #La variable 'direc' indica la direccion del recorrido 
                #contracorriente.
                if(nrow(zona) != 0){
                    #Agregamos variables necesarias.
                    zona$posicion <- 1:nrow(zona)
                    
                    if(identical(names(zona),c(names(tablaGE),"posicion"))== FALSE){
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
                        }else{                          #Caso contrario
                            posSig <-dfReco$reco[       #usamos la siguiente.
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
                    #Procesamos todos los puntos de geolocalizacion de la zona.
                    df_prod <- apply(zona,1,function(y){
                        #Vector que une elpunto dee zona con el de recorrido
                        w <- c(as.numeric(as.character(y[1])),
                               as.numeric(as.character(y[2]))) - v1
                        prodt <- signo*(v[1]*w[1] + v[2]*w[2])
                        prodt <- prodt/normaV                       #Proporcion.
                        #Si el producto es positivo se incrementa la distancia.
                        #Hallamos el tiempo total de recorrido aproximado
                        #r[2] contienen el tiempo acumulado hasta la posicion
                        #actual. r= c(reco,tAcumulado,pos)
                        tps <- dfReco$tAcumulado[dfReco$reco == posSig]
                        zona$valor[as.numeric(as.character(y[10]))] <<- 
                            as.numeric(as.character(r[2])) + 
                            prodt*abs(as.numeric(as.character(r[2])) - tps)
                        #El prime paso del recorrido considera todas los tiempos
                        #Positivos. Suponemos que por cercania geografica el
                        #objeto afecta el lugar de estudio.
                        if(r[3] == 1){
                            zona$valor[as.numeric(as.character(y[10]))] <<- 
                                abs(zona$valor[as.numeric(as.character(y[10]))])
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
            unionZonas <-  data.frame(latitude = NA,longitude =NA,lat.objeto =NA,
                           lon.objeto = NA,objeto = NA , tipo = NA,valor = NA)
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
        #Procesamos la lista donde estan los datasets de busqueda en funcion de 
        #la amplitud del recorrido. La lista contiene 6 dataframes. En la
        #posicion 1 estan las ubicaciones mas cercanas a los puntos de recorrido
        #sobre la linea costera y en la ultima posicion estan los mas lejanos.
        #Este nivel de cercania o lejania esta acotado a maximo lat +- 0.06 y
        #lon +- 0,06
        datosPtoi <- data.frame(latitude = NA,longitude = NA,lat.objeto = NA,
                        lon.objeto = NA,objeto = NA , tipo = NA,valor = NA)
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
    
    #Unificamos los datos en un solo data frame por rbind.
    dataTiempoRec <- data.frame(latitude = NA,longitude = NA,lat.objeto = NA,
                           lon.objeto = NA,objeto = NA , tipo = NA,valor = NA)
    #Procedimiento
    s <- lapply(tiempoRec,function(df){
        dataTiempoRec <<- rbind(dataTiempoRec,df)
    })
    
    #Limpiamos memoria
    rm(s);rm(tiempoRec)
    gc()
    
    #Removemos los NA's generadosy creamos el dataset de salida.
    dataTiempoRec <- dataTiempoRec[!is.na(dataTiempoRec$lat.objeto),]  
    
    if(length(unique(paste(dataTiempoRec$lat.objeto,dataTiempoRec$lon.objeto))) != 
                                                           nrow(dataTiempoRec)){
        stop("La tabla final presenta datos repetidos.")
    }
    
    #////////////
    #Unimos tablas para la salida.
    tablaA <- rbind(dataDistLin,dataTiempoRec)
    
    #Exportamos.
    return(tablaA)
}



