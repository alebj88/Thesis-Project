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
    modFit<-rpart(numd.,method="class",data=df)
    pred<-predict(modFit,newdata=df[,-15],type="class")
    setwd(paste0(d,"/R-Objects"))
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

procesamientoDeImagenes <-function(imagPNG,newDir= FALSE,pathPPD = "",HT = TRUE,
                                   nSet = FALSE, mapCenter = c(12.5,-66.5)){
#    imagPNG <-"Contorno.png"
#    setwd("C:/Users/Alejandro/Desktop/Procesador de Mapas/Mapas")
#    newDir<-FALSE;HT <-TRUE;mapCenter <- c(12.5,-66.5);nSet <- FALSE
#    pathPPD <- ""
    #Calculo de bordes.
    lat_Bsup <- mapCenter[1] + 4.5
    lat_Binf <- mapCenter[1] - 4.5
    lon_Bder <- mapCenter[2] + 6.5
    lon_Bizq <- mapCenter[2] - 6.5
    
    #Ingreso de imagenes
    img <-readPNG(imagPNG)
    nombre <-strsplit(imagPNG,split="\\.")[[1]][1]
    
    #Inicializamos
    prueba1 <- TRUE
    
    #Check
    if(as.numeric(dim(img)[1]) < 400){
        prueba1 <-FALSE
    }

    #prueba1<-TRUE   #Forzar procesamiento. 
    if(prueba1 == FALSE){
        message("El codigo se detendra.\n") 
        message("Verifique que la imagen safisface los siguientes requerimientos:")
        cat("\n")
        cat("La imagen proviene de la pagina 'https://giovanni.gsfc.nasa.gov/giovanni/'.\n")
        cat("Esta acotada en la region 73W, 8N, 60W, 17N. o en una de dimension similar.\n")
        cat("Tiene un margen de 13 grados de longitud y 9 de latitud exactamente.\n")
        cat("Cumple todas las especificaciones descritas en el 'README_GIOVANNI.txt'.\n")
        if(newDir==TRUE){
            cat("No esta intentando procesar una imagen estilo 'Time Averaged Map'\n")
            cat("o 'Monthly and Seasonal Averages' en la carpeta para Animaciones.\n")
        }
        message("Si se satisfacen estos requerimientos y aun falla, comuniquese")
        message("con el administrador del codigo y retire la imagen de la carpeta.\n")
        stop("El archivo PNG no posee la dimension adecuada.")
    }
    
    #Eliminamos las franjas de comentarios y subtitulos del mapa----------------
    #Buscammos el borde izquierdo del mapa.
    suma <-apply(img[101:400,1:100,1],2,sum)
    #bde <- 100-which.max(rev(suma))+1  
    bde <-which(suma != 300)[1] -1    #Ultimo blanco de izquierda a derecha.
    #Check
    if(is.na(bde)){
        suma <-apply(img[101:400,1:200,1],2,sum)
        bde <-which(suma != 300)[1] -1#Ultimo blanco de izquierda a derecha.
        
    }
    mIzq <-bde -4        #Cinco blancos en fila.
    mDer <-bde +5        #Cinco colores en fila.
    
    #Cortes superior e inferior
    for(i in 150:1){
        if(sum(img[i,mIzq:bde,]) != 20 | sum(img[i,(bde+1):mDer,]) == 20){
            corteInf <-i
            break
        } 
    }
    for(i in 151:nrow(img)){
        if(sum(img[i,mIzq:bde,]) != 20 | sum(img[i,(bde+1):mDer,]) == 20){
            corteSup <-i
            break
        } 
    }
    #Cortamos
    img <- img[corteInf:corteSup,,]

    suma <-apply(img,2,sum)
    suma[suma < (nrow(img)*4)] <- 0
    
    corteIzq <- which.min(suma)                         #Ultimo color izq->der          
    corteDer <- dim(img)[2] - (which.min(rev(suma))-1)  #Primer color izq->der
    
    #Imagen sin blancos en los laterales y sin comentarios
    img <- img[,corteIzq:corteDer,]
    
    # plot(c(1,1124),c(1,867),type='n')
    # rasterImage(img, 1, 1, 1124, 867)
    # dim(img)
    
    #Imagen para el validador
    suma <-apply(img,2,sum)
    suma[suma < (nrow(img)*4)] <- 0
    
    posLoc <- which.max(suma[which.min(suma):length(suma)]) + which.min(suma) -1
    ValEscala  <- img[,posLoc:ncol(img),]  
    
    # plot(c(1,1124),c(1,867),type='n')
    # rasterImage(ValEscala, 1, 1, 1124, 867)

    #Extraccion de la banda numerica.------------
    
    suma <- apply(img,2,sum)
    suma[suma < (nrow(img)*4)] <- 0
    
    #Eliminamos espacio en blanco diminuto entre los numeros
    for(i in 2:(length(suma)-1)){
        if(suma[i-1] == 0 & suma[i] != 0 & suma[i+1] == 0){
            suma[i] <- 0
        }
    }
        
    #Primer color de la banda numerica izq->der
    corteIzq <- dim(img)[2] - (which.max(rev(suma))-1) + 1 
    
    bandaNumerica <- img[,corteIzq:dim(img)[2],]
    
    #Reducimos
    img <- img[,1:(corteIzq-1),]     
    suma <- apply(img,2,sum)
    suma[suma < (nrow(img)*4)] <- 0
    corteDer <- dim(img)[2] - (which.min(rev(suma))-1) 
    img <- img[,1:corteDer,]  
    
    #Cortes superior e inferior
    suma <- apply(bandaNumerica,1,sum)
    suma[suma < (ncol(bandaNumerica)*4)] <- 0
    
    corteSup <- which.min(suma)                         #Ultimo color superior
    corteInf <- dim(img)[1] - (which.min(rev(suma))-1)  #Primer color inferior
    
    bandaNumerica <- bandaNumerica[corteSup:corteInf,,]
    
    setwd(paste0(d,"/Mapas/Ctrl"))
    png(file=paste0(nombre,"Numeros.png"))
    plot(c(1,dim(bandaNumerica)[2]),c(1,dim(bandaNumerica)[1]),
         type='n',main=paste0("Numeros ",nombre))         
    rasterImage(bandaNumerica, 1, 1,dim(bandaNumerica)[2],dim(bandaNumerica)[1])
    dev.off() 
    setwd(paste0(d,"/Mapas"))
    
    cat("Plots de validacion agregados al directorio 'Raw Data/Giovanni/Ctrl'.\n")
    cat("\n")
    
    #Extraccion del mapa.-------------------
    suma <- apply(img,2,sum)
    suma[suma < (nrow(img)*4)] <- 0
    
    #Ultimo color antes del mapa izq->der
    corteDer <- (which.max(suma)-1)
    
    #Corte
    mapa <- img[,1:corteDer,]
    
    #Reducimos
    img <- img[,(corteDer+1):ncol(img),]   
    suma <- apply(img,2,sum)
    suma[suma < (nrow(img)*4)] <- 0
    corteIzq <- which.min(suma) 
    img <- img[,corteIzq:ncol(img),]  

    setwd(paste0(d,"/Mapas/Ctrl"))
    png(file=paste0(nombre,"MAPA.png"))
    plot(c(1,dim(mapa)[2]),c(1,dim(mapa)[1]),type='n',
         main = paste0("MAPA ",nombre))
    rasterImage(mapa, 1, 1, dim(mapa)[2], dim(mapa)[1])
    dev.off()
    setwd(paste0(d,"/Mapas"))
    
    #Extraccion de la banda de colores.-------
    #Cortes laterales
    suma <- apply(img,2,sum)
    suma[suma < (nrow(img)*4)] <- 0
    
    corteIzq <- which.min(suma)                         #Primer color izq->der          
    corteDer <- dim(img)[2] - (which.min(rev(suma))-1)  #Ultimo color izq->der
    
    #Corte
    img <- img[,corteIzq:corteDer,]
    
    #Cortes superior e inferior
    suma <- apply(img,1,sum)
    suma[suma < (ncol(img)*4)]<-0
    
    corteSup <- which.min(suma)                        #Primer color superior 
    corteInf <- dim(img)[1]-which.min(rev(suma))+1     #Ultimo color inferior
    
    #Corte
    img <- img[corteSup:corteInf,,]

    bandaColores <- img
    
    setwd(paste0(d,"/Mapas/Ctrl"))
    png(file=paste0(nombre,"colorBAND.png"))
    plot(c(1,dim(bandaColores)[2]),c(1,dim(bandaColores)[1]),
         type='n',main=paste0("colorBAND ",nombre))         
    rasterImage(bandaColores, 1, 1,dim(bandaColores)[2],dim(bandaColores)[1])
    dev.off() 
    setwd(paste0(d,"/Mapas"))
    
    #Extraccion de muestra central
    barra1 <- round(dim(img)[2]/2,0) - 1
    barra2 <- round(dim(img)[2]/2,0) + 1
    
    img <- img[,barra1:barra2,]
    
    #Banda superior.//////////////////////////
    suma <- apply(img,1,sum)
    suma[suma < (ncol(img)*4)]<-0
    
    corteSup <- which.max(suma) - 1        #Ultimo color del triangulo superior
    
    superior <- img[1:corteSup,,]
    barra1 <- round(dim(superior)[1]/2,0) - 1
    barra2 <- round(dim(superior)[1]/2,0) + 1
    superior <- superior[barra1:barra2,,]
        
    setwd(paste0(d,"/Mapas/Ctrl"))
    png(file=paste0(nombre,"colSUP.png"))
    plot(c(1,1124),c(1,867),type='n',main=paste0("colSUP ",nombre))         
    rasterImage(superior, 1, 1, 1124, 867)
    dev.off() 
    setwd(paste0(d,"/Mapas"))
    
    #Banda central.//////////////////////////
    
    #Primer color del triangulo inferior
    corteInf <- dim(img)[1]-which.max(rev(suma))+2 
    
    central <- img[(corteSup+2):(corteInf-2),,]
    central <- central[3:(nrow(central)-3),,]
    
    setwd(paste0(d,"/Mapas/Ctrl"))
    png(file=paste0(nombre,"colCEN.png"))
    plot(c(1,1124),c(1,867),type='n',main=paste0("colCEN ",nombre))         
    rasterImage(central, 1, 1, 1124, 867)
    dev.off() 
    setwd(paste0(d,"/Mapas"))
    
    #Banda Inferior.////////////////////////
    inferior <- img[corteInf:nrow(img),,]
    barra1 <- round(dim(inferior)[1]/2,0) - 1
    barra2 <- round(dim(inferior)[1]/2,0) + 1
    inferior <- inferior[barra1:barra2,,]
    
    setwd(paste0(d,"/Mapas/Ctrl"))
    png(file=paste0(nombre,"colINF.png"))
    plot(c(1,1124),c(1,867),type='n',main=paste0("colINF ",nombre))         
    rasterImage(inferior, 1, 1, 1124, 867)
    dev.off() 
    setwd(paste0(d,"/Mapas"))
    
    ################################################
    #Interpretacion de la banda numerica.------------
    if(nSet == FALSE){  #Si la interpretacion ya se realizo no la repetimos.
        cat("Ejecutando el interpretador de imagenes...\n")
        cat("\n")
        
        #Dividimos la banda numerica en renglones de cifras
        suma <- apply(bandaNumerica[,,1],1,sum)
        suma[suma < dim(bandaNumerica)[2]] <-0
        
        cortesH <- numeric(0)
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
            cortesH <- c(cortesH,length(suma))
        }
        slicesSup <-cortesH[seq_along(cortesH) %% 2 == 1]
        slicesInf <-cortesH[seq_along(cortesH) %% 2 == 0]
        
        renglones <- list()
        for (i in 1:length(slicesSup)){
            renglones[[i]] <-bandaNumerica[slicesSup[i]:slicesInf[i],,1]
        }
        
        # plot(c(1,1124),c(1,867),type='n')
        # rasterImage(renglones[[1]], 1, 1, 1124, 867)
        # rasterImage(renglones[[2]], 1, 1, 1124, 867)
        # rasterImage(renglones[[3]], 1, 1, 1124, 867)
        # rasterImage(renglones[[4]], 1, 1, 1124, 867)
        # rasterImage(renglones[[length(renglones)]], 1, 1, 1124, 867)
        # length(renglones)
        
        #Dividimos la cifra de cada renglon en fotos de tamano similar.
        cortesV <- function(renglon){ 
            calibrado <- 9.8
            suma <- apply(renglon[,],2,sum)
            suma[suma > calibrado]<-10
            suma[suma < dim(renglon)[1]]<-0
            
            corteDer <- dim(renglon)[2]-which.min(rev(suma))+1 #Ultimo color
            
            renglon<- renglon[,1:corteDer]
            
            suma<-apply(renglon[,],2,sum)
            suma[suma > calibrado]<-10
            suma[suma < dim(renglon)[1]]<-0
            
            #Si hay demasiados ceros consecutivos picamos manualmente.
            cuenta <- 0
            for(i in 1:length(suma)){
                if(suma[i] == 0){
                    cuenta <- cuenta + 1
                    if(cuenta == 9){
                        renglon[,i] <- rep(1,nrow(renglon))
                        cuenta <- 0
                    }
                }
                if(suma[i] != 0){
                    cuenta <- 0
                }
            }
            suma<-apply(renglon[,],2,sum)
            suma[suma > calibrado]<-10
            suma[suma < dim(renglon)[1]]<-0
            
            nroCaracteres <- 0
            for(i in (length(suma)-1):1){
                if(suma[i] != 0){
                    if(suma[i+1] == 0){
                        nroCaracteres <- nroCaracteres + 1
                    }
                    
                }
            }
            
            if(suma[1] == 0){
                nroCaracteres <- nroCaracteres + 1 #Contamos el ultimo
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
            if(suma[1] == 0){
                cortes<-c(1,which(suma != 0))   #Agregamos el primer corte
                #cortes<-c(1,cortes)
            }else{
                cortes<-which(suma != 0) 
            }
            
            cortes<-c(cortes,length(suma))
            
            j<-1
            grupos <- numeric(0)
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
            u <- tapply(cortes,grupos,function(x){max(x)})
            grupos <- sapply(grupos,function(x){unname(u[as.character(x)])})
            
            cortes<-unique(grupos)

            cortes[1]<-cortes[1]-1      #Damos formato para crear los slices.
            
            if(length(cortes) != nroCaracteres +1){
                stop("Incongruencia en la funcion cortesV.
                     length(cortes) != nroCaracteres +1!")
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
        
        #Forzamos correccion en caso de que la interpretacion no pueda generar
        #un valor numerico 
        Forzar <- FALSE
        
        ReTrainDFs<-list()
        
        listaImgAnum <-function(bolsaDeNum){
            prueba <-t(as.data.frame(lapply(bolsaDeNum,imagenTovector)))
            prueba <-as.data.frame(prueba)
            row.names(prueba) <- 1:nrow(prueba)
            ReTrainDFs[[length(ReTrainDFs)+1]] <<-prueba
            pred<-predict(modFit,prueba)
            #Forzamos correccion en caso de que el primer caracter haya sido
            #interpretado como punto.
            if(pred[1,12] == 1){
                Forzar <- TRUE   
            }
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
        
        setwd(paste0(d,"/Mapas/Ctrl"))
        sink(paste0(nombre,"NumCtrl.txt"))
        print(numeros)
        sink()
        setwd(paste0(d,"/Mapas"))
        
        #Test automatico de numeros generados.
        if(sum(is.na(numeros)) != 0 | Forzar == TRUE){
            message("La interpretacion no pudo generar un valor numerico real.") 
            message("Probablemente un caracter fue identificado como signo")
            message("menos y al estar en la posicion incorrecta se genero el")
            message("NA al hacer as.numeric().")
            cat("\n")
            cat("Se iniciara el algoritmo para corregir el interpretador.\n")
            cat("\n")
            message("Indique si la imagen satisface los siguientes requerimientos")
            cat("La imagen proviene de la pagina 'https://giovanni.gsfc.nasa.gov/giovanni/'.\n")
            cat("Esta acotada en la region 73W, 8N, 60W, 17N. o en una de dimension similar.\n")
            cat("Tiene un margen de 13 grados de longitud y 9 de latitud exactamente.\n")
            cat("Cumple todas las especificaciones descritas en el 'README_GIOVANNI.txt'.\n")
            validoIni <-FALSE
            while(validoIni == FALSE){
                iptIni<- readline(prompt="Introduzca Si o No: ")
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
            setwd(paste0(d,"/R-Objects/Train Inter"))      
            entrenador<-read.table("entrenador.txt",header = TRUE)
            
            for (i in 1:length(ReTrainDFs)){
                validok <-FALSE
                while(validok == FALSE){       #Indique si el numero es correcto
                    cat("\n")
                    message("Indique si el numero ",numeros[i]," es correcto.")
                    iptk<- readline(prompt="Introduzca Si o No: ")
                    if(iptk %in% c("NO","No","no","SI","Si","si")){ 
                        validok <-TRUE
                    }else{                                          
                        message("Input invalido!")
                    }
                }
                if(iptk %in% c("NO","No","no")){           #NO, no es correcto!!
                    validokk <-FALSE
                    while(validokk == FALSE){       #Cual es el numero entonces?
                        iptkk <-readline(
                            prompt="Introduzca el numero correcto: ")
                        if(is.na(as.numeric(iptkk)) == FALSE){
                            validokk <-TRUE
                        }else{
                            message("El Input debe ser numerico.")
                        }
                    }
                    predReal <-numTopred(iptkk)
                    #Si el error es de programacion.
                    if(length(predReal) != nrow(ReTrainDFs[[i]])){   
                        cat("/n")
                        stop("Este error no puede ser corregido por reentrenamiento. 
                             Comuniquelo al administrador del codigo.")
                    }
                    ReTrainDFs[[i]]$num <-predReal                   
                    entrenador<-rbind(entrenador,ReTrainDFs[[i]])
                    }else{                                   #Si, si si es correcto.
                        message("Ejecutado.")
                    }
            }
            message("Respuestas registradas. Reentrenando el modelo...")
            
            setwd(paste0(d,"/R-Objects/Train Inter")) 
            file.remove("entrenador.txt")
            write.table(entrenador,file="entrenador.txt",row.names = FALSE)
            nota <-"Colgar en Github el entrenador de esta carpeta.
            Fue actualizado!."
            write.table(nota,file="NOTA_ACTUALIZAR_EN_GITHUB.txt",
                        row.names = FALSE)
            
            setwd(paste0(d,"/R-Objects"))
            file.remove("interpretador.RData")
            
            setwd(paste0(d,"/R-Objects/Train Inter"))
            entrenador<-read.table("entrenador.txt",header = TRUE)
            modFit<-interBuilder(entrenador)    
            setwd(paste0(d,"/R-Objects"))
            save(modFit,file = "interpretador.RData")
            
            setwd(paste0(d,"/Mapas"))
            message("Reentrenamiento finalizado.")
            cat("\n")
            message("Ejecute el programa nuevamente para incluir los cambios.")
            stop_quietly()
            
        }    
        if(sum(diff(rev(numeros)) <= 0) != 0){
            #Programar un reentrenamiento automatico
            message("La imagen no pudo ser procesada correctamente.")
            message("La escala numerica generada por el interpretador no es monotona.")
            cat("Verifique que la imagen safisface los siguientes requerimientos:\n")
            cat("\n")
            cat("La imagen proviene de la pagina 'https://giovanni.gsfc.nasa.gov/giovanni/'.\n")
            cat("Esta acotada en la region 73W, 8N, 60W, 17N. o en una de dimension similar.\n")
            cat("Tiene un margen de 13 grados de longitud y 9 de latitud exactamente.\n")
            cat("Cumple todas las especificaciones descritas en el 'README_GIOVANNI.txt'.\n")
            cat("\n")
            
            validoIni <-FALSE
            while(validoIni == FALSE){
                cat("La imagen satisface los requerimientos?\n")
                iptIni<- readline(prompt="Introduzca Si o No: ")
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
            setwd(paste0(d,"/R-Objects/Train Inter"))       
            entrenador<-read.table("entrenador.txt",header = TRUE)
            
            for (i in 1:length(ReTrainDFs)){
                validok <-FALSE
                while(validok == FALSE){       #Indique si el numero es correcto
                    cat("\n")
                    message("Indique si el numero ",numeros[i]," es correcto.")
                    iptk<- readline(prompt="Introduzca Si o No: ")
                    if(iptk %in% c("NO","No","no","SI","Si","si")){ 
                        validok <-TRUE
                    }else{                                          
                        message("Input invalido!")
                    }
                }
                if(iptk %in% c("NO","No","no")){           #NO, no es correcto!!
                    validokk <-FALSE
                    while(validokk == FALSE){       #Cual es el numero entonces?
                        iptkk <-readline(
                            prompt="Introduzca el numero correcto: ")
                        if(is.na(as.numeric(iptkk)) == FALSE){
                            validokk <-TRUE
                        }else{
                            message("El Input debe ser numerico.")
                        }
                    }
                    predReal <-numTopred(iptkk)
                    #Si el error es de programacion.
                    if(length(predReal) != nrow(ReTrainDFs[[i]])){   
                        cat("/n")
                        stop("Este error no puede ser corregido por reentrenamiento. 
                             Comuniquelo al administrador del codigo.")
                    }
                    ReTrainDFs[[i]]$num <-predReal                   
                    entrenador<-rbind(entrenador,ReTrainDFs[[i]])
                    }else{                                  #Si, si si es correcto.
                        message("Ejecutado.")
                    }
            }
            message("Respuestas registradas. Reentrenando el modelo...")
            
            setwd(paste0(d,"/R-Objects/Train Inter")) 
            file.remove("entrenador.txt")
            write.table(entrenador,file="entrenador.txt",row.names = FALSE)
            nota <-"Colgar en Github el entrenador de esta carpeta.
            Fue actualizado!."
            write.table(nota,file="NOTA_ACTUALIZAR_EN_GITHUB.txt",
                        row.names = FALSE)
            
            setwd(paste0(d,"/R-Objects"))
            file.remove("interpretador.RData")
            
            setwd(paste0(d,"/R-Objects/Train Inter"))
            entrenador<-read.table("entrenador.txt",header = TRUE)
            modFit<-interBuilder(entrenador)    
            setwd(paste0(d,"/R-Objects"))
            save(modFit,file = "interpretador.RData")
            
            setwd(paste0(d,"/Mapas"))
            message("Reentrenamiento finalizado.")
            cat("\n")
            message("Ejecute el programa nuevamente para incluir los cambios.")
            stop_quietly()
        }
    }
    #Fin de la interpretacion numerica
    #####################################
    
    #Validando la interpretacion
    if(HT==TRUE){
        #Validacion por parte del usuario de los numeros generados.
        message("Indique si los valores mostrados en pantalla coinciden con")
        message("la escala numerica de la imagen original.")
        sapply(numeros,function(x){print(x)})
        
        plot(c(1,1124),c(1,867),type='n',main="Validacion de Escala.")
        rasterImage(ValEscala, 1, 1, 1124, 867)
        
        valido <-FALSE
        while(valido == FALSE){
            cat("Los numeros son correctos?\n")
            ipt<- readline(prompt="Introduzca Si o No: ")
            if(ipt %in% c("NO","No","no","SI","Si","si")){
                valido <-TRUE
            }else{
                message("Input invalido!")
            }
        }
        if(ipt %in% c("NO","No","no") == TRUE){
            cat("\n")
            message("Se ha registrado su reporte de error.") 
            message("Antes de continuar verifique que la imagen safisface los ")
            message("siguientes requerimientos:")
            cat("\n")
            cat("La imagen proviene de la pagina 'https://giovanni.gsfc.nasa.gov/giovanni/'.\n")
            cat("Esta acotada en la region 73W, 8N, 60W, 17N. o en una de dimension similar.\n")
            cat("Tiene un margen de 13 grados de longitud y 9 de latitud exactamente.\n")
            cat("Cumple todas las especificaciones descritas en el 'README_GIOVANNI.txt'.\n")
            message("Si la imagen satisface esos requerimientos el problema lo")
            message("presenta el interpretador.")
            message("En este caso se recomienda aceptar la solicitud de reentrenamiento")
            message("del mismo.")
            message("Ejecute esta accion solo si la imagen original cumple las")
            message(" especificaciones solicitadas.\n")
            valido0 <-FALSE
            while(valido0 == FALSE){      #Deseamos reentrenar el interpretador?
                cat("Desea reentrenar el algoritmo de reconocimiento?\n")
                ipt0<- readline(prompt="Introduzca Si o No: ")
                if(ipt0 %in% c("NO","No","no","SI","Si","si")){
                    valido0 <-TRUE
                }else{
                    message("Input invalido!")
                }
            }
            if(ipt0 %in% c("SI","Si","si") == TRUE){    #Positivo a reentrenar!!
                setwd(paste0(d,"/R-Objects/Train Inter"))       
                entrenador<-read.table("entrenador.txt",header = TRUE)
                
                for (i in 1:length(ReTrainDFs)){
                    valido2 <-FALSE
                    while(valido2 == FALSE){   #Indique si el numero es correcto
                        cat("\n")
                        message("Indique si el numero ",numeros[i]," es correcto.")
                        ipt2<- readline(prompt="Introduzca Si o No: ")
                        if(ipt2 %in% c("NO","No","no","SI","Si","si")){ 
                            valido2 <-TRUE
                        }else{                                          
                            message("Input invalido!")
                        }
                    }
                    if(ipt2 %in% c("NO","No","no")){       #NO, no es correcto!!
                        valido3 <-FALSE
                        while(valido3 == FALSE){    #Cual es el numero entonces?
                            ipt3 <-readline(prompt="Introduzca el numero correcto : ")
                            if(is.na(as.numeric(ipt3)) == FALSE){
                                valido3 <-TRUE
                            }else{
                                message("El Input debe ser numerico.")
                            }
                        }
                        predReal <-numTopred(ipt3)
                        #Si el error es de programacion.
                        if(length(predReal) != nrow(ReTrainDFs[[i]])){   
                            cat("/n")
                            stop("Este error no puede ser corregido por reentrenamiento. 
                                 Comuniquelo al administrador del codigo.")
                        }
                        ReTrainDFs[[i]]$num <-predReal                   
                        entrenador<-rbind(entrenador,ReTrainDFs[[i]])
                        }else{                               #Si, si si es correcto.
                            message("Ejecutado.")
                        }
                }
                message("Respuestas registradas. Reentrenando el modelo...")
                
                setwd(paste0(d,"/R-Objects/Train Inter")) 
                file.remove("entrenador.txt")
                write.table(entrenador,file="entrenador.txt",row.names = FALSE)
                nota <-"Colgar en Github el entrenador de esta carpeta.
                Fue actualizado!."
                write.table(nota,file="NOTA_ACTUALIZAR_EN_GITHUB.txt",
                            row.names = FALSE)
                
                setwd(paste0(d,"/R-Objects"))
                file.remove("interpretador.RData")
                
                setwd(paste0(d,"/R-Objects/Train Inter"))
                entrenador<-read.table("entrenador.txt",header = TRUE)
                modFit<-interBuilder(entrenador)    
                setwd(paste0(d,"/R-Objects"))
                save(modFit,file = "interpretador.RData")
                
                setwd(paste0(d,"/Mapas"))
                message("Reentrenamiento finalizado.")
                cat("\n")
                message("Ejecute el programa nuevamente para incluir los cambios.")
                stop_quietly()
                
            }else{                                   #Negativo, no reentrenemos.
                cat("\n")
                message("El programa se detendra.")
                message("Remueva la imagen de la carpeta 'Giovanni' o pongase")
                message(" en contacto con el administrador del codigo.\n")
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
    cat("Agregando las medidas numericas a las bandas de colores.\n")
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
    
    latitude<-seq(lat_Bsup,lat_Binf,length=dim(mapaNum)[1])
    latitude<-rep(latitude,ncol(mapaNum))
    latitude<-sapply(latitude,function(x){round(x,3)})
    longitude<-seq(lon_Bizq,lon_Bder,length=dim(mapaNum)[2])
    longitude<-rep(longitude,each=nrow(mapaNum))
    longitude<-sapply(longitude,function(x){round(x,3)})
    
    #Transformamos mapa numerico en dataset para su exportacion.
    mapaNum <-as.matrix(mapaNum)
    dim(mapaNum) <-c(dim(mapaNum)[1]*dim(mapaNum)[2],1)
    archivo <-data.frame(latitude=latitude,longitude=longitude,medida=mapaNum)
    
    #Exportamos tabla
    if(newDir==FALSE){
        setwd(paste0(d,"/Data Preprocesada"))
        file <-paste0("GIO.",nombre,".csv")
        write.csv(archivo,file=file,row.names=FALSE)
        setwd(paste0(d,"/Mapas"))
        
        message("Archivo '",paste0(nombre,".png'")," procesado exitosamente.\n")
        cat("Se agrego la tabla '",file,"' al directorio Data Preprocesada.\n")
        cat("\n")
    }else{
        setwd(pathPPD)
        file <-paste0(nombre,".csv")
        write.csv(archivo,file=file,row.names=FALSE)
        
        message("Archivo '",paste0(nombre,".png'")," procesado exitosamente.\n")
        cat("Se agrego la tabla '",file,"' al directorio Data Preprocesada\n")
        cat("carpeta: '",strsplit(pathPPD,"\\/")[[1]][
            length(strsplit(pathPPD,"\\/")[[1]])],'.\n')
        cat("\n")
    }
}  

