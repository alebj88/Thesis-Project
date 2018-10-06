#Este cofigo recibe los predictores del MarVzla y usando el Random Fores les
#agrega la prediccion de los ecosistemas. Genera la tabla 
#PredictoresMarVzla_Ecosystem.csv

#El modelo debe estar guardado en la carpeta ShinyPredictor de Documentos.

PredictoresMarVzla <- 
    read.csv("~/Thesis Project AB/Data/Final Data/PredictoresMarVzla.csv")

load("~/ShinyPredictor/interpretador.RData")

#Generamos Prediccion

pred <- predict(modFit,PredictoresMarVzla) 

#Agregamos prediccion

PredictoresMarVzla$Ecosystem <- pred

#Exportamos
setwd("~/Thesis Project AB/Data/Final Data")

write.csv(PredictoresMarVzla,file ="PredictoresMarVzla_Ecosystem.csv",row.names = F)
