library(rgl)
library(ggplot2)
library(vegan) 

#Selector de metodologia\\\\\\\\\\\\\\\\\\\\\\\\\\\\
solapada <- FALSE  #Esferas tangentes sin agrupacion
solapada <- TRUE   #Esferas solapadas con agrupacion

nCent <- 10        #Numero de agrupaciones

TresD <- TRUE      #NMDS Tridimensional
TresD <- FALSE     #NMDS Bidimensional

#Si solapada == TRUE--------------------
RadioPorDefecto <- TRUE   
RadioPorDefecto <- FALSE
Radio <- 0.4      #Radio de las esferas (RadioPorDefecto == FALSE) 

#---------------------------------------
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

#IMportamos datos
DataSet <- read.csv("~/Thesis Project AB/Data/Final Data/SCLESpecies_Matrix.csv")

#Renombramos filas


#Unimos objetos costeros
DataSet$Afluente.Seco.d <- unname(apply(DataSet[c(
    "Quebrada.Seca.d","Rio.Seco.d")],1,function(x){
        vect <- min(x,na.rm = T)
        return(vect)
    }))

DataSet$Quebrada.Seca.d <- NULL
DataSet$Rio.Seco.d <- NULL

#Unimos objetos costeros
DataSet$Petroleo.d <- unname(apply(DataSet[c(
    "Termoelectrica.d","Refineria.d","Petroquimica.d")],1,function(x){
        vect <- min(x,na.rm = T)
        return(vect)
    }))

DataSet$Termoelectrica.d <- NULL
DataSet$Refineria.d <- NULL
DataSet$Petroquimica.d <- NULL

#Unimos objetos costeros
DataSet$Poblado.d <- unname(apply(DataSet[c(
    "Pueblo.d","Ciudad.d")],1,function(x){
        vect <- min(x,na.rm = T)
        return(vect)
    }))

DataSet$Pueblo.d <- NULL
DataSet$Ciudad.d <- NULL

#Agregamos latitud y longitud como predictores 
DataSet$Lat <- DataSet$latitude
DataSet$Lon <- DataSet$longitude

#Eliminamos variables colineales.
posNoElim <- which(names(DataSet) %in% c(
    "Laguna.d"                     ,"Puerto.d",                   
    "Rio.d"                        ,"AbsorCoeffPhytoplank.Q3",     
    "AbsorCoeffNonAlgalMat.Median" ,"ChlorophyllaConcent.Mean",    
    "PhotosyntheticRadiation.Q3"   ,"RemoteReflectance.Mean",      
    "SSTemperatureDay.Mean"        ,"SSTemperatureNight.Mean",     
    "SSTemperatureNight.Kurtosis"  ,"SeaLevelPreassure.Skewness",  
    "SeaSaltColumnDensity.Mean"    ,"SeaSaltColumnDensity.Sd",     
    "TwoMAirTemperature.Mean"      ,"SeaSaltConcentration.Mean",   
    "CarbonDioxide.Q1"             ,"CarbonDioxide.Skewness" ,     
    "MethaneNight.Median"          ,"MethaneNight.Q3",             
    "MethaneDay.Kurtosis"          ,"IncidentShortwave.Max",       
    "IncommingShortwave.Max"       ,"speed.cm.s.min",              
    "Afluente.Seco.d"              ,"Petroleo.d" ,                 
    "Poblado.d"
    
))

#Transformamos 999's en 120's en las variables de Google Earth
for(k in c(50:75,370,371,372)){
    DataSet[,k][DataSet[k][,1] == 999] <- rep(120,sum(DataSet[k][,1] == 999))
}

#Guardamos los puntos de geolocalizacion de las ubicaciones de los corales junto
#con la variable InsularCoast. Esta sera agregada como predictor en el RF.
latlondf <- DataSet[c("Site","latitude","longitude","InsularCoast")]
names(latlondf)[1] <- "Ecosystem"

#Las eliminamos
DataSet <- DataSet[c(1:47,posNoElim)]

#Variables Biologicas
varespec <- DataSet[c(5:47)]
rownames(varespec) <- DataSet$Site

#Variables ambientales
varechem <- DataSet[c(48:ncol(DataSet))]
rownames(varechem) <- DataSet$Site

#Entrenamos modelo NMDS
set.seed(1234)

if(TresD == TRUE){
    #NMDS
    nmds <- metaMDS(varespec,distance = "kulczynski",k = 3,
                    trace = FALSE,trymax = 100,tol = 1e-07)
    
    #Extraemos superficies de gradientes para cada variable ambiental.
    ordi1 <- list()
    for(i in 1:ncol(varechem)){
        k <- ordisurf(nmds ~ get(names(varechem)[i]),data = varechem, 
                      plot = T, bubble = 6,main = names(varechem)[i],
                      isotropic = FALSE,bs = "cs", knots = c(3,4), fx = T, 
                      select = F,
                      choices = c(1, 2),ylim = c(-1.8,1.5),xlim = c(-1,1.2))
        ordi1[[length(ordi1) + 1]] <- k$grid
        
    }
    
    ordi2 <- list()
    for(i in 1:ncol(varechem)){
        k <- ordisurf(nmds ~ get(names(varechem)[i]),data = varechem, 
                      plot = T, bubble = 6,main = names(varechem)[i],
                      isotropic = FALSE,bs = "cs", knots = c(3,4), fx = T, 
                      select = F,
                      choices = c(1, 3),ylim = c(-0.8,1.2),xlim = c(-1.2,1.8))
        ordi2[[length(ordi2) + 1]] <- k$grid
        
    }
    
    ordi3 <- list()
    for(i in 1:ncol(varechem)){
        k <- ordisurf(nmds ~ get(names(varechem)[i]),data = varechem, 
                      plot = T, bubble = 6,main = names(varechem)[i],
                      isotropic = FALSE,bs = "cs", knots = c(3,4), fx = T, 
                      select = F,
                      choices = c(2, 3),ylim = c(-1,1),xlim = c(-2,2))
        ordi3[[length(ordi3) + 1]] <- k$grid
        
    }
    
}else{
    #NMDS
    nmds <- metaMDS(varespec,distance = "kulczynski",k = 2,
                    trace = FALSE,trymax = 100,tol = 1e-07)
    
    #Extraemos superficies de gradientes para cada variable ambiental.
    ordi1 <- list()
    for(i in 1:ncol(varechem)){
        k <- ordisurf(nmds ~ get(names(varechem)[i]),data = varechem, 
                      isotropic = FALSE,bs = "cs", knots = c(3,4), fx = T, 
                      select = F,
                      plot = T, bubble = 6,main = names(varechem)[i],
                      choices = c(1, 2),ylim = c(-1.8,1.5),xlim = c(-1,1.2))
        

        ordi1[[length(ordi1) + 1]] <- k$grid
        
    }
    
}


#Construimos esferas de similitud //////////////////////////////////////////////

#Extraemos las coordenadas de los ecosistemas muestreados tras el NMDS.
sit.sc <- scores(nmds)  

#Creamos dataFrame para las esferas, centro y radio.
set.seed(987500)

if(TresD == TRUE){
    DataEsf <- data.frame(NMDS1 = sit.sc[,1],NMDS2 = sit.sc[,2],
                          NMDS3 = sit.sc[,3])
    #Agrupamos por k-medias y recuperamos los centroides
    if(nCent < nrow(DataEsf)){
        k <- kmeans(DataEsf[,1:3],centers = nCent, nstart=100)
    }
}else{
    DataEsf <- data.frame(NMDS1 = sit.sc[,1],NMDS2 = sit.sc[,2])
    #Agrupamos por k-medias y recuperamos los centroides
    if(nCent < nrow(DataEsf)){
        k <- kmeans(DataEsf[,1:2],centers = nCent, nstart=100) 
    }
}

#Almacenamos los puntos de los ecosistemas en el espacio de ordenacion
DataEcos <- DataEsf

if(nCent < nrow(DataEsf)){
    DataEsf <- as.data.frame(k$centers)
    
    #Almacenamos informacion sobre la agrupacion.
    DataEcos$cluster <- as.factor(k$cluster)					
    DataEsf$cluster <- rownames(DataEsf)
}else{
    DataEcos$cluster <- seq(1:nrow(DataEsf))					
    DataEsf$cluster <- seq(1:nrow(DataEsf))
}

#Renombramos
if(TresD == TRUE){
    names(DataEcos)[c(1,2,3)] <- c("NMDS1_Ec","NMDS2_Ec","NMDS3_Ec")
}else{
    names(DataEcos)[c(1,2)] <- c("NMDS1_Ec","NMDS2_Ec")
}

DataEcos$Ecos <- rownames(DataEcos)
DataEcos <- merge(DataEcos,DataEsf,by = "cluster",all.x = TRUE)
rownames(DataEcos) <- DataEcos$Ecos

#Calculamos las distancias entre el punto y su respectivo centroide
if(TresD == TRUE){
    DataEcos$Dist <- apply(DataEcos[,-c(1,5)],1,function(v){
        d <- sqrt((v[1]-v[4])^2 + (v[2]-v[5])^2 + (v[3]-v[6])^2)
        return(d)
    })
}else{
    DataEcos$Dist <- apply(DataEcos[,-c(1,4)],1,function(v){
        d <- sqrt((v[1]-v[3])^2 + (v[2]-v[4])^2)
        return(d)
    })
}

#Seleccion de la metodologia
if(solapada == FALSE){
 
    #Calculamos la matriz de distancias euclideas entre los puntos de la 
    #ordenacion
    dMat <- as.matrix(dist(DataEsf[-ncol(DataEsf)],method = "euclidean"))
    diag(dMat) <- rep(10000,ncol(dMat))
    
    #Inicializamos radios
    DataEsf$Radio <- rep(NA,nrow(DataEsf))
    
    #Calculamos los radios
    repeat{
        #Hallamos la columna que contiene al minimo
        colMin <- which.min(apply(dMat,2,min))
        
        #Buscamos la fila donde esta ese minimo
        filMin <- which.min(dMat[,colMin])
        
        #Check de salida
        if(dMat[filMin,colMin] >= 5000){
            break
        }
        
        #Evaluamos la presencia de alguna esfera
        if(is.na(DataEsf$Radio[colMin])){
            #Agregamos radios
            if(is.na(DataEsf$Radio[filMin])){
                DataEsf$Radio[filMin] <- dMat[filMin,colMin]/2
                DataEsf$Radio[colMin] <- dMat[filMin,colMin]/2
                #Reducimos las otras distancias
                # NAS <- is.na(DataEsf$Radio)
                # dMat[NAS,filMin] <- dMat[NAS,filMin] - DataEsf$Radio[filMin]
                # dMat[filMin,NAS] <- dMat[filMin,NAS] - DataEsf$Radio[filMin]
                # dMat[NAS,colMin] <- dMat[NAS,colMin] - DataEsf$Radio[colMin]
                # dMat[colMin,NAS] <- dMat[colMin,NAS] - DataEsf$Radio[colMin]
                dMat[,filMin] <- dMat[,filMin] - DataEsf$Radio[filMin]
                dMat[filMin,] <- dMat[filMin,] - DataEsf$Radio[filMin]
                dMat[,colMin] <- dMat[,colMin] - DataEsf$Radio[colMin]
                dMat[colMin,] <- dMat[colMin,] - DataEsf$Radio[colMin]
                
            }else{
                DataEsf$Radio[colMin] <- dMat[filMin,colMin]
                #Reducimos las otras distancias
                # NAS <- is.na(DataEsf$Radio)
                # dMat[NAS,colMin] <- dMat[NAS,colMin] - DataEsf$Radio[colMin]
                # dMat[colMin,NAS] <- dMat[colMin,NAS] - DataEsf$Radio[colMin]
                dMat[,colMin] <- dMat[,colMin] - DataEsf$Radio[colMin]
                dMat[colMin,] <- dMat[colMin,] - DataEsf$Radio[colMin]
            }
            
        }else{
            if(is.na(DataEsf$Radio[filMin])){
                DataEsf$Radio[filMin] <- dMat[filMin,colMin]
                #Reducimos las otras distancias
                # NAS <- is.na(DataEsf$Radio)
                # dMat[NAS,filMin] <- dMat[NAS,filMin] - DataEsf$Radio[filMin]
                # dMat[filMin,NAS] <- dMat[filMin,NAS] - DataEsf$Radio[filMin]
                dMat[,filMin] <- dMat[,filMin] - DataEsf$Radio[filMin]
                dMat[filMin,] <- dMat[filMin,] - DataEsf$Radio[filMin]
            }
            
        }
        #Colocamos 1000 en la posicion dada
        dMat[filMin,colMin] <- 10000
        dMat[colMin,filMin] <- 10000
        
    }
       
}else{
    
    #Hallamos la distancia maxima vista entre los centroides y sus puntos
    if(RadioPorDefecto == TRUE){
        maxima <- max(tapply(DataEcos$Dist,DataEcos$cluster,max)) + 0.01
    }else{
        maxima <- Radio
    }

    #Creamos los radios de las esferas de similitud
    DataEsf$Radio <- rep(maxima,nrow(DataEsf))
}

#Renombramos filas
DataEsf <- merge(DataEsf,DataEcos[c("cluster","Ecos")],
                 by = "cluster",all.x = TRUE)

t <- tapply(DataEsf$Ecos,DataEsf$cluster,paste0,collapse = "-")
t <- sapply(DataEsf$cluster,function(x){unname(t[as.character(x)])})

DataEsf$Ecos <- t
DataEsf <- DataEsf %>% unique()
rownames(DataEsf) <- DataEsf$Ecos

#Eliminamos variables
DataEsf$Ecos <- NULL
DataEcos$Ecos <- NULL
DataEsf$cluster <- NULL

#Visualizacion
if(TresD == TRUE){
    par3d(windowRect = c(20, 30, 800, 800))	#Resize window
    
    rgl.spheres(DataEsf$NMDS1, DataEsf$NMDS2, DataEsf$NMDS3, 
                radius = DataEsf$Radio,
                col=rainbow(nCent))
    
}else{
    par3d(windowRect = c(20, 30, 800, 800))	#Resize window
    
    rgl.spheres(DataEsf$NMDS1, DataEsf$NMDS2, rep(0,nrow(DataEsf)), 
                radius = DataEsf$Radio,
                col=rainbow(nCent))
}

View(DataEsf)
summary(DataEsf)

#Factores de correccion ////////////////////////////////////////////////////////

#Generamos factor de correccion para cada sitio y variable.
corFactor <- rep(0,(ncol(varechem)*nrow(varechem)))
dim(corFactor) <- c(nrow(varechem),ncol(varechem))
corFactor <- as.data.frame(corFactor)
names(corFactor) <- names(varechem)
rownames(corFactor) <- rownames(varechem)

#Agregamos factor de correccion si el NMDS es 3D
if(TresD == TRUE){
    
    #Recorremos dataset de muestra
    for(k in 1:ncol(varechem)){
        #Matrices de gradientes por plano
        XY <- ordi1[[k]]  #PlanoXY
        XZ <- ordi2[[k]]  #PlanoXZ
        YZ <- ordi3[[k]]  #PlanoYZ
        
        #Calculamos el valor de la variable en cada plano
        sapply(1:nrow(sit.sc),function(i){
            p1 <- sit.sc[i,] #Ecosistema i
            valXY <- XY$z[which.min(abs(XY$x - p1[1])),
                          which.min(abs(XY$y - p1[2]))]  #Valor en planoXY
            valXZ <- XZ$z[which.min(abs(XZ$x - p1[1])),
                          which.min(abs(XZ$y - p1[3]))]  #Valor en planoXZ
            valYZ <- YZ$z[which.min(abs(YZ$x - p1[2])),
                          which.min(abs(YZ$y - p1[3]))]  #Valor en planoYZ
            
            #Calculamos valor aproximado
            aproxVal <- mean(c(valXY,valXZ,valYZ),na.rm=T)
            
            #Extraemos valor real
            realVal <- varechem[i,k]
            
            #Generamos factor de correccion aditivo
            corFactor[i,k] <<- realVal - aproxVal #Sumar
        })
    }
}else{
    #Recorremos dataset de muestra
    for(k in 1:ncol(varechem)){
        #Matrices de gradientes por plano
        XY <- ordi1[[k]]  #PlanoXY
        
        #Calculamos el valor de la variable en cada plano
        sapply(1:nrow(sit.sc),function(i){
            p1 <- sit.sc[i,] #Ecosistema i
            valXY <- XY$z[which.min(abs(XY$x - p1[1])),
                          which.min(abs(XY$y - p1[2]))]  #Valor en planoXY
            
            #Calculamos valor aproximado
            aproxVal <- valXY
            
            #Extraemos valor real
            realVal <- varechem[i,k]
            
            #Generamos factor de correccion aditivo
            corFactor[i,k] <<- realVal - aproxVal #Sumar
        })
    }
}

#Correccion del factor de correccion por agrupamiento
# if(solapada == TRUE){
#     corFactor$EcoGroup <- rep(NA,nrow(corFactor))
#     
#     #Ciclo de busqueda
#     for(i in 1:nrow(corFactor)){
#         
#         for(j in 1:nrow(DataEsf)){
#             inside <- rownames(corFactor)[i] %in% 
#                 strsplit(rownames(DataEsf)[j],split = "-")[[1]]
#             
#             if(inside == TRUE){
#                 corFactor$EcoGroup[i] <- rownames(DataEsf)[j]
#                 break
#             }
#         }
#     }
#     
#     #Redondeamos factores de correccion
#     dfAux <- data.frame(tapply(corFactor[,1],corFactor$EcoGroup,mean))
# 
#     proc <- sapply(corFactor[,-c(1,ncol(corFactor))],function(v){
#         t <- as.data.frame(tapply(v,corFactor$EcoGroup,mean))
#         dfAux <<- cbind(dfAux,t)
#         return(NULL)
#     })
#     names(dfAux) <- names(corFactor[,-c(ncol(corFactor))])
#     
#     Actualizamos corFactor
#     corFactor <- dfAux
# 
# }


#Creamos cuadricula de puntos para el espacio de ordenacion///////////////////// 

if(TresD == TRUE){
    #Creamos la nube
    Xv <- ordi1[[1]]$x  #Eje X
    Yv <- ordi1[[1]]$y  #Eje Y
    Zv <- ordi2[[1]]$y  #Eje Z
    
    nube <- data.frame(X = rep(Xv,each=length(Yv)), Y = rep(Yv,length(Xv)))
    nube$Z <- rep(Zv[1],times = nrow(nube))
    
    aux <- nube
    for (i in 2:length(Zv)){
        aux$Z <- rep(Zv[i],times = nrow(aux))
        nube <- rbind(nube,aux)
    }
    
}else{
    #Creamos la nube
    Xv <- ordi1[[1]]$x  #Eje X
    Yv <- ordi1[[1]]$y  #Eje Y
    
    nube <- data.frame(X = rep(Xv,each=length(Yv)), Y = rep(Yv,length(Xv)))
    
}


#Le agregamos ecosistemas a la nube en funcion de las esferas
nube$Eco <- rep(NA,nrow(nube)) 
nube$Nro <- 1:nrow(nube)

#Variables de DataEsf: NMDS1,NMDS2,NMDS3,Radio.
#Variables de la nube: X,Y,Z,Eco,Nro.
cands <- data.frame(Eco = rownames(DataEsf), dist = rep(10000, nrow(DataEsf))) 

proc <- apply(nube,1,function(p1){
    for(j in 1:nrow(DataEsf)){
        p2 <- DataEsf[j,]
        
        #Clculo de distancai euclidea
        if(TresD == TRUE){
            d <- sqrt((p1[1]-p2[1])^2 + (p1[2]-p2[2])^2 + (p1[3]-p2[3])^2)
            #Si el punto esta dentro de la esfera del ecosistema
            if(d < p2[4]){
                cands$dist[j] <<- d 
            }
        }else{
            d <- sqrt((p1[1]-p2[1])^2 + (p1[2]-p2[2])^2)
            #Si el punto esta dentro de la esfera del ecosistema
            if(d < p2[3]){
                cands$dist[j] <<- d 
            }
        }
        

    }
    cands$dist <- as.numeric(cands$dist)
    
    #Check
    if(min(cands$dist) == 10000){
        stop("Hay un ecosistema fuera de esfera")
    }
    #Agregamos ecosistema
    nube$Eco[p1[length(p1)]] <<- as.character(cands$Eco[which.min(cands$dist)])
    return(NULL)
})

#Eliminamos NAS
nube <- nube[complete.cases(nube),]
nube$Nro <- NULL

#Incremenamos valores en la nuve en caso de haber agrupaciones 
valoresNE <- unique(nube$Eco)
nube$EcoNG <- nube$Eco

for (i in 1:length(valoresNE)){
    #Extraemos vector de ecosistemas agrupados
    lec1 <- strsplit(valoresNE[i],split = "-")[[1]]
   
    if(length(lec1) != 1){
        pos <- which(nube$Eco == valoresNE[i])
        dfAux1 <- nube[pos,]
        dfAux1$EcoNG <- rep(lec1[1],nrow(dfAux1))
        for(k in 2:length(lec1)){
            dfAux2 <- nube[pos,]
            dfAux2$EcoNG <- rep(lec1[k],nrow(dfAux2))
            dfAux1 <- rbind(dfAux1,dfAux2)
        }
        #Eliminamos filas originales y reemplazamos por las incremtadas
        nube <- nube[-pos,]
        nube <- rbind(nube,dfAux1)
    }
    
}

#Correhimos nombres de fila
rownames(nube) <- 1:nrow(nube)

#Agregamos informacion climatologica a cada punto de la nube
for(k in 1:ncol(varechem)){
    nube$var <- rep(NA,nrow(nube))
    
    #Matrices de gradientes por plano
    if(TresD == TRUE){
        XY <- ordi1[[k]]  #PlanoXY
        XZ <- ordi2[[k]]  #PlanoXZ
        YZ <- ordi3[[k]]  #PlanoYZ
        
        #Calculamos el valor de la variable en cada plano
        sapply(1:nrow(nube),function(i){
            p1 <- nube[i,]
            valXY <- XY$z[which(abs(XY$x - p1$X) < 0.000005),
                          which(abs(XY$y - p1$Y) < 0.000005)]  #Valor en planoXY
            valXZ <- XZ$z[which(abs(XZ$x - p1$X) < 0.000005),
                          which(abs(XZ$y - p1$Z) < 0.000005)]  #Valor en planoXZ
            valYZ <- YZ$z[which(abs(YZ$x - p1$Y) < 0.000005),
                          which(abs(YZ$y - p1$Z) < 0.000005)]  #Valor en planoYZ
            #Promediamos y registramos
            if(all(is.na(c(valXY,valXZ,valYZ)))){
                nube$var[i] <<- NA
            }else{
                f <- which(rownames(corFactor) == p1$EcoNG)
                nube$var[i] <<- mean(c(valXY,valXZ,valYZ),na.rm=T) + 
                    corFactor[f,k]
                #Check
                if(is.na(nube$var[i])){
                    stop("Se fue un NA")
                }
            }
        })
        
    }else{
        XY <- ordi1[[k]]  #PlanoXY
        
        #Calculamos el valor de la variable en cada plano
        sapply(1:nrow(nube),function(i){
            p1 <- nube[i,]
            valXY <- XY$z[which(abs(XY$x - p1$X) < 0.000005),
                          which(abs(XY$y - p1$Y) < 0.000005)]  #Valor en planoXY
            #Check
            if(length(valXY) != 1){
                stop("Xval tiene mas de n valor")
            }
            #Promediamos y registramos
            if(is.na(valXY)){
                nube$var[i] <<- NA
            }else{
                f <- which(rownames(corFactor) == p1$EcoNG)
                nube$var[i] <<- valXY + corFactor[f,k]
            }
        })
    }
    names(nube)[names(nube) == "var"] <- names(varechem)[k]
}

#Limpiamos y eliminamos
nube$X <- NULL
nube$Y <- NULL
nube$Z <- NULL
nube$EcoNG <- NULL
names(nube)[names(nube) == "Eco"] <- "Ecosystem"

#Limpiamos
nube <- nube[complete.cases(nube),]

#Contamos
table(nube$Ecosystem)

#Reordenamos filas
nube <- nube[c(2:ncol(nube),1)]

#Unimos con los datos originales.
varechem$Ecosystem <- rownames(varechem)

#Obtenemos datos para las agrupaciones hechas con el k-medias
if(solapada == TRUE){
    ecosistemas <-  unique(nube$Ecosystem)
    #Ciclo
    for(i in 1:nrow(varechem)){
        
        for(j in 1:length(ecosistemas)){
            inside <- rownames(varechem)[i] %in% 
                strsplit(ecosistemas[j],split = "-")[[1]]
            
            if(inside == TRUE){
                varechem$Ecosystem[i] <- ecosistemas[j]
                break
            }
        }
    }
    
    #Ciclo 2
    latlondf$Ecosystem <- as.character(latlondf$Ecosystem)
    rownames(latlondf) <- latlondf$Ecosystem
    
    for(i in 1:nrow(latlondf)){
        
        for(j in 1:length(ecosistemas)){
            inside <- rownames(latlondf)[i] %in% 
                strsplit(ecosistemas[j],split = "-")[[1]]
            
            if(inside == TRUE){
                latlondf$Ecosystem[i] <- ecosistemas[j]
                break
            }
        }
    }
}

#Unimos con redundancia para mejorar el entrenamiento del modelo
for(k in 1:2){
    varechem <- rbind(varechem,varechem)
}
print(nrow(varechem)/36) #4

unidos <- rbind(varechem,nube)

#Creamos el entrenador ////////////////////////////////////////
unidos <- merge(latlondf,unidos,by = "Ecosystem",all.y = TRUE) 

#Reordenamos filas
unidos <- unidos[c(2:ncol(unidos),1)]

#Correccion de atipicos
if(solapada == FALSE){
    
    #Eliminamos datos incorrectos. De 4 a 23
    nomb <- unique(unidos$Ecosystem)
    
    #Chequeo
    table(unidos$Ecosystem)
    
    #Buscamos ecosistemas vacios o con un solo dato
    if(any(table(unidos$Ecosystem) < 2)){
        stop("Hay ecosistemas sin datos. Incremente el radio de las esferas o edite el codigo a partir de la linea 618.")
    }
        
    #Correccion manual de ecosistemas faltantes.???????????????????????????
    #Ejemplo: Si playa tiburon tiene un solo recibio un dato lo duplicamos.
    #playat <- unidos[which(unidos$Ecosystem == "Playa Tiburon"),]
    #unidos <- rbind(unidos,playat)
    #??????????????????????????????????????????????????????????????????????
    
    #Eliminacion de atipicos, opcional.
    #Registramos numero de fila
    # unidos$fila <- 1:nrow(unidos)  
    # 
    # elim <- c()
    # for(i in 4:(ncol(unidos)-1)){
    #     for(j in 1:length(nomb)){
    #         fila <- unidos$fila[unidos$Ecosystem == nomb[j]]
    #         vect <- unidos[unidos$Ecosystem == nomb[j],i]
    #         
    #         q1 <- summary(vect)[2]
    #         q3 <- summary(vect)[5]
    #         rang <- q3 - q1
    #         
    #         pos <- which(vect <= (q1 - 1.5*rang - (rang/2)) | 
    #                          vect >= (q3 + 1.5*rang + (rang/2)))
    #         
    #         if(length(pos) > 0){
    #             elim <- c(elim,fila[pos])
    #         }
    #     }
    # }
    # 
    # elim <- unique(elim)
    # 
    # #Evitamos la eliminacion de Playa Tiburon
    # #p <- which(unidos$Ecosystem == "Playa Tiburon")
    # #elim <- elim[!(elim %in% p)]
    # 
    # #Limpiamos
    # unidos <- unidos[-elim,]
    # unidos$fila <- NULL

}

#Check
if(any(table(unidos$Ecosystem) < 2)){
    stop("Hay ecosistemas sin datos suficientes. Cambie la metodologia.")
}

#Eliminamos datos incorrectos
VarsPrueba <- c(
    "SSTemperatureNight.Kurtosis"  ,"SeaLevelPreassure.Skewness",  
    "CarbonDioxide.Skewness" , "MethaneDay.Kurtosis" , "Ecosystem",
    "longitude", "latitude", "RemoteReflectance.Mean","InsularCoast"
)

unidos <- unidos[
     unname(apply(unidos[!(names(unidos) %in% VarsPrueba)],1,function(f){
         bool <- !any(f < 0)
         return(bool)
     }))
,]

#Preparamos el reentrenamiento de la APP
setwd("~/Thesis Project AB/ShinyPredictor")

write.table(unidos,file="entrenador.txt",row.names = TRUE)

if("interpretador.RData" %in% dir()){
    file.remove("interpretador.RData")
    cat("Interpretador removido exitosamente.\n")
}

tabla <- as.data.frame(table(unidos$Ecosystem))
tabla <- tabla[c("Freq","Var1")]
write.table(tabla,file="Frecuencias_del_Entrenador.txt",row.names = FALSE)
