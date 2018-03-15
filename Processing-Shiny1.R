library(dplyr)
library(tidyr)

#Creando archivos para la aplicacion Shiny-----------------------------------
cat("Creando tablas para la aplicacion Shiny...\n")
cat("\n")

setwd("~/Thesis Project AB/R-Scripts")
source('Functions.R')

#Datasets ambientales.
setwd("~/Thesis Project AB/Data/Pre-processed Data 2")
WOA <- read.csv("WOA.csv")
WOA <- WOA[WOA$depth.m <= 50,]
WOA$clave2 <- NULL

#Dataset biologico.
setwd("~/Thesis Project AB/Data/Processed Data")
DataCorales <- read.csv("DataCorales.csv")

#Toma lat lon de DataCorales y de WOA lat lon y la variable a imputar.
#En dataF retorna DataCorales con la variable imputada.
#Imputar variable por variable para mayor velocidad.
#Si se emplea "median" la velocidad es apenas veces mayor.
variables <- names(WOA)[!(names(WOA) %in% c("latitude","longitude"))]

#No hace falta pasarle variable por variable. La funcion lo hace automaticamente.
DataCorales <- impute.per.region2(DataCorales,WOA,radio=c(5,10,65))
                                   
setwd("~/Thesis Project AB/Data/Pre-processed Data")
write.csv(DataCorales,file="shinyDataFactor-Var.csv",row.names=F)
message("Se agrego a la carpeta 'Pre-processed Data' el archivo: 'shinyDataFactor-Var.csv'")

#Limpiamos memoria
rm(list=ls())
gc()

cat("Procedimiento Procesing-Shiny.R finalizado.\n")
cat("\n")
cat("/////////////////////////////////////////////////\n")
cat("\n")
