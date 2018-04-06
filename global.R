#Publicado en:
#https://alebj88.shinyapps.io/ShinyExploratory/

library(DT)
library(dplyr)
library(ggmap)

#Datasets principales
MatrizBiologica <- read.csv("DataCoralesProcMan.csv")

#Strings como factores.
s_prod <- sapply(names(MatrizBiologica)[1:6],function(var){
    MatrizBiologica[var][,1] <<- as.factor(MatrizBiologica[var][,1])
})
rm(s_prod)
gc()

#Extraemos las posibles variables dependientes.
VarDepFactor <- names(MatrizBiologica)[1:6]
varDepNum    <- names(MatrizBiologica)[17:ncol(MatrizBiologica)]
varDepNum <- c("Tiempo.en.Corriente.dias","Distancia.Euclidea.kms",varDepNum)

#Dataset TABLA \\\\\\\\\\\\\\\\\\\\\\\\\\\\\

#Dataset MAPA \\\\\\\\\\\\\\\\\\\\\\\\\\\\\

#Dataset BOXPLOT \\\\\\\\\\\\\\\\\\\\\\\\\\

#Dataset HISTOGRAMAS \\\\\\\\\\\\\\\\\\\\\\

#Dataset SCATTERPLOT \\\\\\\\\\\\\\\\\\\\\\

#Dataset SVM \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

#Dataset KMEANS \\\\\\\\\\\\\\\\\\\\\\\\\\\

#Dataset SVD \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\