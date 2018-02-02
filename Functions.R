#Funciones empleadas

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

#Importe y formateo de los archivos txt descargados de la NOAA
importar<-function(file,Tab=FALSE){
    data <- read.csv(paste0("~/Thesis Project/Data/Raw Data/",file),
                     na.strings=c(""),stringsAsFactors=F)
    k<-t(apply(data[-1,],1,formato,Tab))
    data[2:nrow(data),]<-k
    return(data)
}

#Construccion del dataframe con los datos de geolocalizacion
dfGeo<-function(lista){
    lat<-c(lista[[1]][1],lista[[2]][1],lista[[3]][1])
    lon<-c(lista[[1]][2],lista[[2]][2],lista[[3]][2])
    lat<-geo2dec(lat)
    lon<-geo2dec(lon)
    return(data.frame(lat=lat,lon=lon))
}

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

#Formateando NAs
formatNA<-function(v){
    v[v=="n.d."]<-NA
    v[grep("-999",v)]<-NA
    if(class(v)=="numeric"){
        v[round(v,0)==-999]<-NA
    }
    return(v)
}

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

#Suprimir warings de ciertas funciones temporalmente.
suppressWarnings <- function(expr) {
    ops <- options(warn = -1)       ## FIXME: temporary hack until R_tryEval
    on.exit(options(ops))           ## calls are removed from methods code
    withCallingHandlers(expr,warning=function(w)invokeRestart("muffleWarning"))
}


#Procesamiento de imagenes.---------------------------------------

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

#Constructor del modelo de interpretacion de caracteres numericos.
interBuilder<-function(df){
    
    df <-rbind(df,df,df,df,df,df,df,df,df,df)  #Trampa
    #Montamos arbol de decision.
    modFit<-rpart(num~.,method="class",data=df)
    pred<-predict(modFit,newdata=df[,-15],type="class")
    setwd("~/Thesis Project/Data/R-Objects")
    sink("TestDelInterpretador.txt")
    print(confusionMatrix(pred,df$num))
    sink()
    return(modFit)
}

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

#Procesamiento de imagenes.---------------------------------------
#Recibimos un archivo png con el siguiente formato de nombre
#oxigenoDisuelto_promedio_micromolesPorLitro_0101199504042017.png

procesamientoDeImagenes<-function(imagPNG){
    setwd("~/");imagPNG <-"MonthlyandSeasonalAverages.png"
    img <-readPNG(imagPNG)
    nombre <-strsplit(imagPNG,split="\\.")[[1]][1]
    
    #Eliminamos las franjas de comentarios y subtitulos del mapa.
    suma <-apply(img[,1:10,1],1,sum)
    suma[suma<10] <-0
    corteSup <-which.min(suma)
    corteInf <-dim(img)[1]-which.min(rev(suma))+1
    
    img<-img[corteSup:corteInf,,]
    # plot(c(1,1124),c(1,867),type='n')
    # rasterImage(img, 1, 1, 1124, 867)
    # dim(img)

    if(identical(as.numeric(dim(img)),c(708,1124,4)) == FALSE){
        message("El codigo se detendra.\n") 
        message("Verifique que la imagen safisface los siguientes requerimientos:")
        cat("\n")
        cat("La imagen proviene de la pagina 'https://giovanni.gsfc.nasa.gov/giovanni/'.\n")
        cat("Esta acotada en la region 73W, 8N, 60W, 17N.\n")
        cat("Cumple todas las especificaciones descritas en el 'README_GIOVANNI.txt'.\n")
        message("Si se satisfacen estos requerimientos y aun falla, comuniquese con el administrador del codigo y retire la imagen de la carpeta.\n")
        stop("ERROR: El archivo PNG no posee la dimension adecuada.")
    }
    
    #Extraccion del mapa.-------------------
    
    suma <-apply(img[1:10,,1],2,sum)
    suma[suma<10] <-0
    corteDer <-dim(img)[2]-which.min(rev(suma))+1
    
    mapa<-img[,1:corteDer,]
    
    setwd("~/Thesis Project/Data/Raw Data/Giovanni/Ctrl")
    png(file=paste0(nombre,"MAPA.png"))
    plot(c(1,1124),c(1,867),type='n',main=paste0("MAPA ",nombre))
    rasterImage(mapa, 1, 1, 1124, 867)
    dev.off()
    setwd("~/Thesis Project/Data/Raw Data/Giovanni")
    
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
    
    setwd("~/Thesis Project/Data/Raw Data/Giovanni/Ctrl")
    png(file=paste0(nombre,"colorBAND.png"))
    plot(c(1,1124),c(1,867),type='n',main=paste0("colorBAND ",nombre))         
    rasterImage(bandaColores, 1, 1, 1124, 867)
    dev.off() 
    setwd("~/Thesis Project/Data/Raw Data/Giovanni")
    
    #Banda superior.
    superior<-bandaColores[10:20,12:14,]
    
    setwd("~/Thesis Project/Data/Raw Data/Giovanni/Ctrl")
    png(file=paste0(nombre,"colSUP.png"))
    plot(c(1,1124),c(1,867),type='n',main=paste0("colSUP ",nombre))         
    rasterImage(superior, 1, 1, 1124, 867)
    dev.off() 
    setwd("~/Thesis Project/Data/Raw Data/Giovanni")
    
    #Banda central.
    central<-bandaColores[28:372,12:14,]
    
    setwd("~/Thesis Project/Data/Raw Data/Giovanni/Ctrl")
    png(file=paste0(nombre,"colCEN.png"))
    plot(c(1,1124),c(1,867),type='n',main=paste0("colCEN ",nombre))         
    rasterImage(central, 1, 1, 1124, 867)
    dev.off() 
    setwd("~/Thesis Project/Data/Raw Data/Giovanni")
    
    #Banda Inferior.
    inferior<-bandaColores[380:390,12:14,]
    
    setwd("~/Thesis Project/Data/Raw Data/Giovanni/Ctrl")
    png(file=paste0(nombre,"colINF.png"))
    plot(c(1,1124),c(1,867),type='n',main=paste0("colINF ",nombre))         
    rasterImage(inferior, 1, 1, 1124, 867)
    dev.off() 
    setwd("~/Thesis Project/Data/Raw Data/Giovanni")
    
    #Extraccion de la banda numerica.------------

    bandaNumerica<-img[1:(dim(img)[1]-12),(corteDer+1):dim(img)[2],]
    
    setwd("~/Thesis Project/Data/Raw Data/Giovanni/Ctrl")
    png(file=paste0(nombre,"Numeros.png"))
    plot(c(1,1124),c(1,867),type='n',main=paste0("Numeros ",nombre))         
    rasterImage(bandaNumerica, 1, 1, 1124, 867)
    dev.off() 
    setwd("~/Thesis Project/Data/Raw Data/Giovanni")
    
    cat("Plots de validacion agregados al directorio Raw 'Data/Giovanni/Ctrl'.\n")
    cat("Ejecutando el interpretador de imagenes...\n")
    cat("\n")
    
    #Interpretacion de la banda numerica.------------

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
        # plot(c(1,1124),c(1,867),type='n')         
        # rasterImage(renglones[[i]], 1, 1, 1124, 867)
    }
    
    #Dividimos la cifra de cada renglon en fotos de tamano similar.
    cortesV<-function(renglon){ 
        calibrado<-9.8
        suma<-apply(renglon[,],2,sum)
        suma[suma > calibrado]<-10
        suma[suma < dim(renglon)[1]]<-0
        corteDer <-dim(renglon)[2]-which.min(rev(suma))+1
        
        renglon<- renglon[,1:corteDer]
        # plot(c(1,1124),c(1,867),type='n')         
        # rasterImage(renglon, 1, 1, 1124, 867)
        
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
            # plot(c(1,1124),c(1,867),type='n')         
            # rasterImage(cifras[[i]], 1, 1, 1124, 867)
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
    numeros <-lapply(listaDeNrosDesagrupados,listaImgAnum)
    numeros<-sapply(numeros,function(x){x[[1]]})

    if(length(ReTrainDFs) != length(numeros)){
        stop("El data set de reentrenamiento no tiene la longitud adecuada.")
    }
    
    setwd("~/Thesis Project/Data/Raw Data/Giovanni/Ctrl")
    sink(paste0(nombre,"NumCtrl.txt"))
    print(numeros)
    sink()
    setwd("~/Thesis Project/Data/Raw Data/Giovanni")
    
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
        message("El codigo se detendra.\n") 
        message("Verifique que la imagen safisface los siguientes requerimientos:")
        cat("\n")
        cat("La imagen proviene de la pagina 'https://giovanni.gsfc.nasa.gov/giovanni/'.\n")
        cat("Esta acotada en la region 73W, 8N, 60W, 17N.\n")
        cat("Cumple todas las especificaciones descritas en el 'README_GIOVANNI.txt'.\n")
        message("Si se satisfacen estos requerimientos y aun falla, comuniquese con el administrador del codigo.\n")
        stop("La imagen no pudo ser procesada. La escala numerica generada por el interpretador no es monotona.")
    }    
    
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
            setwd("~/Thesis Project/Data/R-Objects/Train Inter")       
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
                    message("Continuamos...")
                }
            }
            message("Respuestas registradas. Reentrenando el modelo...")
            
            setwd("~/Thesis Project/Data/R-Objects/Train Inter") 
            file.remove("entrenador.txt")
            setwd("~/Thesis Project/Data/R-Objects")
            file.remove("interpretador.RData")
            modFit<-interBuilder(entrenador)    
            save(modFit,file = "interpretador.RData")
            setwd("~/Thesis Project/Data/R-Objects/Train Inter") 
            write.table(entrenador,file="entrenador.txt",row.names = FALSE)
            nota <-"Colgar en Github el entrenador de esta carpeta.Fue actualizado!."
            write.table(nota,file="NOTA_ACTUALIZAR_EN_GITHUB.txt",row.names = FALSE)
            setwd("~/Thesis Project/Data/Raw Data/Giovanni")
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
    superior <-c(superior,numeros[1]+0.001)
    #A la inferior
    inferior <-c(inferior,numeros[length(numeros)]-0.001)

    clave <-paste0(central[,1],central[,2],central[,3],central[,4])
    
    #Escala discreta de colores////////////////////
    if(length(unique(clave))-length(numeros)+2 == (length(numeros)-1)){
        #A la central
        numeros <-numeros[-1]
        central <-central %>% unique()
        central <-central[-seq(2,nrow(central),by=2),]
        if(nrow(central) != length(numeros)){
            stop("Incongruencia en la asignacion de medidas a la banda de colores discreta.")
        }
        central$medidas <-numeros
    }
    
    #Escala continua de colores////////////////////
    if(length(unique(clave))-length(numeros)+2 != (length(numeros)-1)){
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
    asignacion[nrow(asignacion)+1,]<-c("0 0 0 0",NA)
    
    #Creando mapa con medidas.
    cat("Creando tabla con los datos del mapa...\n")
    cat("\n")
    
    nrofMapa<-dim(mapa)[1]
    scanner <-function(v){
        dim(v) <-c(nrofMapa,4)
        #v<-as.data.frame(v)
        cla <-as.character(paste(v[,1],v[,2],v[,3],v[,4],sep=" "))
        for(i in 1:length(cla)){
            if(cla[i] %in% asignacion$clave == FALSE){
                cla[i]<-"0 0 0 0"
            }   
        }
        return(cla)
    }
    listaClaves<-apply(mapa,2,scanner) #Mapa con claves CHEQUEAR
    
    mapaNum<-apply(listaClaves,2,function(c){
        unname(sapply(c,function(x){asignacion$medidas[asignacion$clave == x]}))
    })
    
    #Creamos mapa numerico
    mapaNum <-as.data.frame(mapaNum)
    mapaNum<-as.data.frame(apply(mapaNum,2,as.numeric)) 
    
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
    setwd("~/Thesis Project/Data/Pre-processed Data")
    file <-paste0("GIO.",nombre,".csv")
    write.csv(archivo,file=file,row.names=FALSE)
    setwd("~/Thesis Project/Data/Raw Data/Giovanni")
    
    cat("Archivo '",paste0(nombre,".png'")," procesado exitosamente.\n")
    cat("Se agrego la tabla '",file,"' al directorio Pre-processed Data.\n")
    cat("\n")
    
    #Validando

    # m <- as.matrix((mapaNum))
    # image(t(m)[,ncol(t(m)):1])

    # m<-as.matrix(as.data.frame(apply(m,2,function(x){x[x<800]<-NA})))
    # m<-as.data.frame(apply(m,2,as.numeric)) 
    # 
    # grDevices::colors()
    # pal <- grDevices::colorRampPalette(c("lightblue","red","yellow"))
    # image(t(m)[,ncol(t(m)):1],col=pal(100))

    # plot(c(1,1124),c(1,867),type='n')         
    # rasterImage(mapa, 1, 1, 1124, 867)
    # plot(c(1,1124),c(1,867),type='n')         
    # rasterImage(m, 1, 1, 1124, 867)
    #write.table(entrenador,file="entrenador.txt",row.names = FALSE)
}  
    
    
    
    









	


