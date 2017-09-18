#Funciones empleadas

#Plot de geolocalizacion de la raw data en archivos PNG
MapaRawData<-function(df,file,title,lonIni=-66.715,latIni=13.115,zoom=6,size=2){
    png(file=file)
    map<-get_map(location= c(lon=lonIni,lat=latIni),zoom=zoom,maptype="terrain") 
    plotMapa<-ggmap(map) +
        geom_point(data=df,aes(x=lon,y=lat),col="red",show.legend=FALSE,size=size) +
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
    data <- read_csv(paste0("~/Thesis Project/Data/Raw Data/",file))
    data <- as.data.frame(data)
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