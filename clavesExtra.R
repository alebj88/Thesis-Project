#Codigo para crear el archivo claves extra

#-73,8,-60,17
setwd("~/Thesis Project AB/Data/Pre-processed Data")

latitude  <- (13000:14000)/1000
longitude <- (-73000:-65000)/1000
latn <- length(latitude)
lonn <- length(longitude)

latitude <- rep(latitude,each = lonn)
longitude <- rep(longitude,times = latn)

clave <- paste(latitude,longitude,sep = "_")

rm(latitude);rm(longitude)
gc()

write.csv(clave,"clavesExtra1.csv")

rm(clave)
gc()

latitude  <- (14000:15000)/1000
longitude <- (-73000:-65000)/1000
latn <- length(latitude)
lonn <- length(longitude)

latitude <- rep(latitude,each = lonn)
longitude <- rep(longitude,times = latn)

clave <- paste(latitude,longitude,sep = "_")

rm(latitude);rm(longitude)
gc()

write.csv(clave,"clavesExtra2.csv")

rm(clave)
gc()

latitude  <- (15000:16000)/1000
longitude <- (-73000:-65000)/1000
latn <- length(latitude)
lonn <- length(longitude)

latitude <- rep(latitude,each = lonn)
longitude <- rep(longitude,times = latn)

clave <- paste(latitude,longitude,sep = "_")

rm(latitude);rm(longitude)
gc()

write.csv(clave,"clavesExtra3.csv")

rm(clave)
gc()

latitude  <- (16000:17000)/1000
longitude <- (-73000:-65000)/1000
latn <- length(latitude)
lonn <- length(longitude)

latitude <- rep(latitude,each = lonn)
longitude <- rep(longitude,times = latn)

clave <- paste(latitude,longitude,sep = "_")

rm(latitude);rm(longitude)
gc()

write.csv(clave,"clavesExtra4.csv")

rm(clave)
gc()