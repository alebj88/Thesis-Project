#Tipo de variable estudiada
discreta <- TRUE
#discreta <- FALSE
#logaritmo <-TRUE
logaritmo <- FALSE
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# PRUEBA DE MERGE

library(ggmap)
library(RColorBrewer)

arch <- dir()[grep(".csv$",dir())][1]

#Importamos tabla y removemos NA's
matAmb <- read.csv(arch)
names(matAmb)[!(names(matAmb) %in% c("latitude","longitude"))] <- "medida"
matAmb <- matAmb[!is.na(matAmb$medida),]

#Formateamos variables.
#matAmb$latitude <-round(matAmb$latitude,2)
#matAmb$longitude <-round(matAmb$longitude,2)

if(discreta == TRUE){
    matAmb$medida <- as.factor(matAmb$medida)
}else{
    if(logaritmo == TRUE){
        matAmb$medida <- log(matAmb$medida) 
    }
}
str(matAmb)

#Plot Dataset 
lon <- mean(matAmb$longitude)
lat <- mean(matAmb$latitude)

map <- get_map(location=c(lon=lon,lat=lat),zoom=6,maptype="terrain")
plotMapa <-ggmap(map) +
    geom_point(data=matAmb,aes(x=longitude,y=latitude,
                               col=medida),show.legend = T,size = 0.5) +
    labs(title=arch)

if(discreta == TRUE){
    plotMapa <- plotMapa + scale_color_brewer(palette="Paired") +
        guides(colour = guide_legend(override.aes = list(size=10))) 
}else{
    mid <- median(matAmb$medida)
    mid <- 6.5
    plotMapa <- plotMapa + scale_color_gradient2(midpoint = mid,
                               low="blue",mid="red",high="yellow",space ="Lab")
}

print(plotMapa)
