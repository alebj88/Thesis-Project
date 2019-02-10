#Este codigo recibe la tabla generada con el procesador de mapas al aplicarlo
#sobre la imagen "Continental.jpg" y genera la tabla "Mar_Vzla.csv" con la
#muestra de puntos de geolocalizacion del mar.

# La tabla resultante "Mar_Vzla.csv" es almacenada en la carpeta Biologicas de
# programa "union por geolocalizacion.R" para recibir la informacion ambiental.

library(dplyr)

MarVzla <- read.csv("~/Thesis Project AB/Procesador de Mapas/Datos Finales/Continental.csv")

#Eliminamos NAS
MarVzla <- MarVzla[!is.na(MarVzla$Continental),]

#Extraemos colores rojo y verde
MarVzla <- MarVzla[MarVzla$Continental == 86.96 | MarVzla$Continental == 65.22,]

#Redondeamos puntos de geolocalizacion y filtramos
MarVzla$latitude <- round(MarVzla$latitude,1)
MarVzla$longitude <- round(MarVzla$longitude,1)

MarVzla <- MarVzla %>% unique()

#Agregamos variable InsularCoast
MarVzla$InsularCoast <- rep("Insular",nrow(MarVzla))
MarVzla$InsularCoast[which(MarVzla$Continental == 65.22)] <- 
    rep("Continental",length(which(MarVzla$Continental == 65.22)))

#Removemos variable y exportamos
MarVzla$Continental <- NULL

setwd("~/Thesis Project AB/Union por Geolocalizacion/Biologicas")
write.csv(MarVzla,file = "Mar_Vzla.csv", row.names = F)