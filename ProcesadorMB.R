#Procesador de matrices biologicas.
#
#Almacenar los archivos:
# "Matriz_Especies_Sustrato.csv"                                       
# "Matriz_GrandesGrupos.csv"                                           
# "Matriz_SCLE_Especies.csv"                                           
# "Matriz_SCLE_FormasCrecimiento.csv" 
#En la carpeta "Raw Data" de "Thesis Project AB".

library(dplyr)
library(tidyr)

################################################################################
################################ TABLA NRO 1 ###################################
################################################################################

setwd("~/Thesis Project AB/Data/Raw Data")

#Importamos y procesamos las matrices biologicas

Matriz1 <- read.csv("Matriz_Especies_Sustrato.csv")

Matriz1$Archivo <- NULL

#Establecemos los grupos de transectas a agrupar

Matriz1$GrupoTrans <- as.factor(rep(1:10,each = 4))

#Calculamos promedios
for(i in 4:201){
    t <- tapply(Matriz1[i][,1],Matriz1$GrupoTrans,mean,na.rm = T)	
    Matriz1[i][,1] <-  sapply(Matriz1$GrupoTrans,function(x){
        unname(t[as.character(x)])
    })
}

#Hacemos unique	
Matriz1$Transecta <- NULL
Matriz1$GrupoTrans <- NULL

Matriz1 <- Matriz1 %>% unique()

#Corregimos nombres
names(Matriz1) <- unname(sapply(names(Matriz1),function(text){
    text <- gsub("\\.\\.","\\.",text)
    text <- gsub("\\.$","",text)
    text <- gsub("Other.Other.","Other.",text)
    text <- gsub(".other$","",text)
}))

#Almacenamos el tipo del coral
Scleractinian <- names(Matriz1)[3:80]
octocoral <- names(Matriz1)[81:93]
Antipatharia <- names(Matriz1)[94:95]
Hidrocoral <- names(Matriz1)[96:101]

#Almacenamos la forma de crecimiento del coral
#Del 94 al 101 no hay informacion sobre la forma de crecimiento.
branching <- names(Matriz1)[3:16]
plate <- names(Matriz1)[17:36]
foliose <- names(Matriz1)[37:40]
massive <- names(Matriz1)[41:66]
pillar <- names(Matriz1)[67:68]
solitary <- names(Matriz1)[69:73]
encrusting <- names(Matriz1)[c(74:76,92:93)]
knobby <- names(Matriz1)[77]
flower.cup <- names(Matriz1)[78:80]
erect <- names(Matriz1)[81:91]
    
#Almacenamos la original
MatOrig <- Matriz1

#Juntamos las variables////////////////////////////////////
pos <- grep("^[Oo]ther",names(Matriz1))	#Variables que comienzan por "Other".
k <- names(Matriz1)[pos]
#k

#Corales ==================================================
#names(Matriz1)
Matriz1 <- gather(Matriz1,key = Especie.Coral,value = Cobertura.Coral,3:101)

#Especies sin cobertura reciben NA
posVac <- Matriz1$Cobertura.Coral == 0
Matriz1$Especie.Coral[posVac] <- rep(NA,sum(posVac))

#Eliminamos Repetidos
Matriz1 <- Matriz1 %>% unique()

#Reordenamos
Matriz1 <- arrange(Matriz1,Sitio)

#Indicador de lugares sin corales
vacio <- tapply(Matriz1$Especie.Coral,Matriz1$Sitio,function(x){
    all(is.na(x))
})
lugar <- names(which(vacio == FALSE)) #Ubicaciones con datos

#Eliminamos las filas con NA en las ubicaciones con datos
if(length(lugar) != 0){
   Matriz1 <- Matriz1[
       !(is.na(Matriz1$Especie.Coral) & (Matriz1$Sitio %in% lugar)),]
}


#Esponjas ==================================================
#names(Matriz1)
Matriz1 <- gather(Matriz1,key = Especie.Esponja,value = Cobertura.Esponja,3:33)

#Especies sin cobertura reciben NA
posVac <- Matriz1$Cobertura.Esponja == 0
Matriz1$Especie.Esponja[posVac] <- rep(NA,sum(posVac))

#Eliminamos Repetidos
Matriz1 <- Matriz1 %>% unique()

#Reordenamos
Matriz1 <- arrange(Matriz1,Sitio)

#Indicador de lugares sin corales
vacio <- tapply(Matriz1$Especie.Esponja,Matriz1$Sitio,function(x){
    all(is.na(x))
})
lugar <- names(which(vacio == FALSE)) #Ubicaciones con datos

#Eliminamos las filas con NA en las ubicaciones con datos
if(length(lugar) != 0){
   Matriz1 <- Matriz1[
       !(is.na(Matriz1$Especie.Esponja) & (Matriz1$Sitio %in% lugar)),]
}


#Algas ==================================================
#names(Matriz1)
Matriz1 <- gather(Matriz1,key = Especie.Alga,value = Cobertura.Alga,3:9)

#Especies sin cobertura reciben NA
posVac <- Matriz1$Cobertura.Alga == 0
Matriz1$Especie.Alga[posVac] <- rep(NA,sum(posVac))

#Eliminamos Repetidos
Matriz1 <- Matriz1 %>% unique()

#Reordenamos
Matriz1 <- arrange(Matriz1,Sitio)

#Indicador de lugares sin corales
vacio <- tapply(Matriz1$Especie.Alga,Matriz1$Sitio,function(x){
    all(is.na(x))
})
lugar <- names(which(vacio == FALSE)) #Ubicaciones con datos

#Eliminamos las filas con NA en las ubicaciones con datos
if(length(lugar) != 0){
   Matriz1 <- Matriz1[
       !(is.na(Matriz1$Especie.Alga) & (Matriz1$Sitio %in% lugar)),]
}


#Micro Algas ==================================================
#names(Matriz1)
Matriz1 <- gather(Matriz1,key=Especie.MicroAlga,value=Cobertura.MicroAlga,3:26)

#Especies sin cobertura reciben NA
posVac <- Matriz1$Cobertura.MicroAlga == 0
Matriz1$Especie.MicroAlga[posVac] <- rep(NA,sum(posVac))

#Eliminamos Repetidos
Matriz1 <- Matriz1 %>% unique()

#Reordenamos
Matriz1 <- arrange(Matriz1,Sitio)

#Indicador de lugares sin corales
vacio <- tapply(Matriz1$Especie.MicroAlga,Matriz1$Sitio,function(x){
    all(is.na(x))
})
lugar <- names(which(vacio == FALSE)) #Ubicaciones con datos

#Eliminamos las filas con NA en las ubicaciones con datos
if(length(lugar) != 0){
   Matriz1 <- Matriz1[
       !(is.na(Matriz1$Especie.MicroAlga) & (Matriz1$Sitio %in% lugar)),]
}


#Seagrass ==================================================
#names(Matriz1)
Matriz1 <- gather(Matriz1,key=Especie.Seagrass,value=Cobertura.Seagrass,3:10)

#Especies sin cobertura reciben NA
posVac <- Matriz1$Cobertura.Seagrass == 0
Matriz1$Especie.Seagrass[posVac] <- rep(NA,sum(posVac))

#Eliminamos Repetidos
Matriz1 <- Matriz1 %>% unique()

#Reordenamos
Matriz1 <- arrange(Matriz1,Sitio)

#Indicador de lugares sin corales
vacio <- tapply(Matriz1$Especie.Seagrass,Matriz1$Sitio,function(x){
    all(is.na(x))
})
lugar <- names(which(vacio == FALSE)) #Ubicaciones con datos

#Eliminamos las filas con NA en las ubicaciones con datos
if(length(lugar) != 0){
   Matriz1 <- Matriz1[
       !(is.na(Matriz1$Especie.Seagrass) & (Matriz1$Sitio %in% lugar)),]
}


#Moluscos ==================================================
#names(Matriz1)
Matriz1 <- gather(Matriz1,key=Especie.Molusco,value=Cobertura.Molusco,3:8)

#Especies sin cobertura reciben NA
posVac <- Matriz1$Cobertura.Molusco == 0
Matriz1$Especie.Molusco[posVac] <- rep(NA,sum(posVac))

#Eliminamos Repetidos
Matriz1 <- Matriz1 %>% unique()

#Reordenamos
Matriz1 <- arrange(Matriz1,Sitio)

#Indicador de lugares sin corales
vacio <- tapply(Matriz1$Especie.Molusco,Matriz1$Sitio,function(x){
    all(is.na(x))
})
lugar <- names(which(vacio == FALSE)) #Ubicaciones con datos

#Eliminamos las filas con NA en las ubicaciones con datos
if(length(lugar) != 0){
   Matriz1 <- Matriz1[
       !(is.na(Matriz1$Especie.Molusco) & (Matriz1$Sitio %in% lugar)),]
}


#Polychaete ==================================================
#names(Matriz1)
Matriz1 <- gather(Matriz1,key=Especie.Polychaete,value=Cobertura.Polychaete,3:5)

#Especies sin cobertura reciben NA
posVac <- Matriz1$Cobertura.Polychaete == 0
Matriz1$Especie.Polychaete[posVac] <- rep(NA,sum(posVac))

#Eliminamos Repetidos
Matriz1 <- Matriz1 %>% unique()

#Reordenamos
Matriz1 <- arrange(Matriz1,Sitio)

#Indicador de lugares sin corales
vacio <- tapply(Matriz1$Especie.Polychaete,Matriz1$Sitio,function(x){
    all(is.na(x))
})
lugar <- names(which(vacio == FALSE)) #Ubicaciones con datos

#Eliminamos las filas con NA en las ubicaciones con datos
if(length(lugar) != 0){
   Matriz1 <- Matriz1[
       !(is.na(Matriz1$Especie.Polychaete) & (Matriz1$Sitio %in% lugar)),]
}


#Invertebrados ==================================================
#names(Matriz1)
Matriz1 <- gather(Matriz1,key=Especie.Invert,value=Cobertura.Invert,3:4)

#Especies sin cobertura reciben NA
posVac <- Matriz1$Cobertura.Invert == 0
Matriz1$Especie.Invert[posVac] <- rep(NA,sum(posVac))

#Eliminamos Repetidos
Matriz1 <- Matriz1 %>% unique()

#Reordenamos
Matriz1 <- arrange(Matriz1,Sitio)

#Indicador de lugares sin corales
vacio <- tapply(Matriz1$Especie.Invert,Matriz1$Sitio,function(x){
    all(is.na(x))
})
lugar <- names(which(vacio == FALSE)) #Ubicaciones con datos

#Eliminamos las filas con NA en las ubicaciones con datos
if(length(lugar) != 0){
   Matriz1 <- Matriz1[
       !(is.na(Matriz1$Especie.Invert) & (Matriz1$Sitio %in% lugar)),]
}

#Agregamos tipo de coral y forma de crecimiento/////////////////
Matriz1$Tipo.Coral <- unname(sapply(Matriz1$Especie.Coral,function(x){
    if(x %in% Scleractinian){
        tipo <- "Scleractinian"
    }else if(x %in% octocoral){
        tipo <- "Octocorals"
    }else if(x %in% Antipatharia){
        tipo <- "Antipatharia"
    }else if(x %in% Hidrocoral){
        tipo <- "Hidrocorals"
    }else{
        tipo <- NA
    }
    return(tipo)
}))

#Agregamos forma de crecimiento//////////////////////////////////
Matriz1$FormaCrec.Coral <- unname(sapply(Matriz1$Especie.Coral,function(x){
    if(x %in% branching){
        Forma <- "Branching"
    }else if(x %in% plate){
        Forma <- "Plate"
    }else if(x %in% foliose){
        Forma <- "Foliose"
    }else if(x %in% massive){
        Forma <- "Massive"
    }else if(x %in% pillar){
        Forma <- "Pillar"
    }else if(x %in% solitary){
        Forma <- "Solitary"
    }else if(x %in% encrusting){
        Forma <- "Encrusting"
    }else if(x %in% knobby){
        Forma <- "Knobby"
    }else if(x %in% flower.cup){
        Forma <- "Flower.cup"
    }else if(x %in% erect){
        Forma <- "Erect"
    }else{
        Forma <- NA
    }
    return(Forma)
}))

#Tratamiento final/////////////////////////////////////

Matriz1 <- Matriz1 %>% unique()
Matriz1 <- Matriz1[c(1,2,21,37,38,22:36,3:20)]


#Exportamos tabla//////////////////////////////////////

setwd("~/Thesis Project AB/Data/Pre-processed Data")

write.csv(Matriz1,file = "Matriz_Especies_Sustrato_Proc1.csv", row.names = F)


################################################################################
################################ TABLA NRO 2 ###################################
################################################################################

setwd("~/Thesis Project AB/Data/Raw Data")

#Importamos y procesamos las matrices biologicas

Matriz1 <- read.csv("Matriz_GrandesGrupos.csv")

Matriz1$Archivo <- NULL

#Establecemos los grupos de transectas a agrupar

Matriz1$GrupoTrans <- as.factor(rep(1:10,each = 4))

#Calculamos promedios
for(i in 4:14){
    t <- tapply(Matriz1[i][,1],Matriz1$GrupoTrans,mean,na.rm = T)	
    Matriz1[i][,1] <-  sapply(Matriz1$GrupoTrans,function(x){
        unname(t[as.character(x)])
    })
}

#Hacemos unique	
Matriz1$Transecta <- NULL
Matriz1$GrupoTrans <- NULL

Matriz1 <- Matriz1 %>% unique()

#Almacenamos la original
MatOrig <- Matriz1

#Tipo de Corales ===========================================
#names(Matriz1)
Matriz1 <- gather(Matriz1,key = Tipo.Coral,value = Cobertura.Coral,3:6)

#Especies sin cobertura reciben NA
posVac <- Matriz1$Cobertura.Coral == 0
Matriz1$Tipo.Coral[posVac] <- rep(NA,sum(posVac))

#Eliminamos Repetidos
Matriz1 <- Matriz1 %>% unique()

#Reordenamos
Matriz1 <- arrange(Matriz1,Sitio)

#Indicador de lugares sin corales
vacio <- tapply(Matriz1$Tipo.Coral,Matriz1$Sitio,function(x){
    all(is.na(x))
})
lugar <- names(which(vacio == FALSE)) #Ubicaciones con datos

#Eliminamos las filas con NA en las ubicaciones con datos
if(length(lugar) != 0){
   Matriz1 <- Matriz1[
       !(is.na(Matriz1$Tipo.Coral) & (Matriz1$Sitio %in% lugar)),]
}

#Tratamiento final/////////////////////////////////////

Matriz1 <- Matriz1 %>% unique()
Matriz1 <- Matriz1[c(1,2,10,11,3:9)]

#Exportamos tabla /////////////////////////////////////

setwd("~/Thesis Project AB/Data/Pre-processed Data")

write.csv(Matriz1,file = "Matriz_GrandesGrupos_Proc1.csv", row.names = F)


################################################################################
################################ TABLA NRO 3 ###################################
################################################################################

setwd("~/Thesis Project AB/Data/Raw Data")

#Importamos y procesamos las matrices biologicas

Matriz1 <- read.csv("Matriz_SCLE_Especies.csv")

Matriz1$Archivo <- NULL

#Establecemos los grupos de transectas a agrupar

Matriz1$GrupoTrans <- as.factor(rep(1:10,each = 4))

#Calculamos promedios
for(i in 4:81){
    t <- tapply(Matriz1[i][,1],Matriz1$GrupoTrans,mean,na.rm = T)	
    Matriz1[i][,1] <-  sapply(Matriz1$GrupoTrans,function(x){
        unname(t[as.character(x)])
    })
}

#Hacemos unique	
Matriz1$Transecta <- NULL
Matriz1$GrupoTrans <- NULL

Matriz1 <- Matriz1 %>% unique()

#Corregimos nombres
names(Matriz1) <- unname(sapply(names(Matriz1),function(text){
    text <- gsub("\\.\\.","\\.",text)
    text <- gsub("\\.$","",text)
    text <- gsub("Other.Other.","Other.",text)
    text <- gsub(".other$","",text)
}))

#Almacenamos la original
MatOrig <- Matriz1


#Corales ==================================================
#names(Matriz1)
Matriz1 <- gather(Matriz1,key = Especie.Coral,value = Cobertura.Coral,3:80)

#Especies sin cobertura reciben NA
posVac <- Matriz1$Cobertura.Coral == 0
Matriz1$Especie.Coral[posVac] <- rep(NA,sum(posVac))

#Eliminamos Repetidos
Matriz1 <- Matriz1 %>% unique()

#Reordenamos
Matriz1 <- arrange(Matriz1,Sitio)

#Indicador de lugares sin corales
vacio <- tapply(Matriz1$Especie.Coral,Matriz1$Sitio,function(x){
    all(is.na(x))
})
lugar <- names(which(vacio == FALSE)) #Ubicaciones con datos

#Eliminamos las filas con NA en las ubicaciones con datos
if(length(lugar) != 0){
   Matriz1 <- Matriz1[
       !(is.na(Matriz1$Especie.Coral) & (Matriz1$Sitio %in% lugar)),]
}


#Tratamiento final/////////////////////////////////////

Matriz1 <- Matriz1 %>% unique()

#Exportamos tabla /////////////////////////////////////

setwd("~/Thesis Project AB/Data/Pre-processed Data")

write.csv(Matriz1,file = "Matriz_SCLE_Especies_Proc1.csv", row.names = F)


################################################################################
################################ TABLA NRO 4 ###################################
################################################################################

setwd("~/Thesis Project AB/Data/Raw Data")

#Importamos y procesamos las matrices biologicas

Matriz1 <- read.csv("Matriz_SCLE_FormasCrecimiento.csv")

Matriz1$Archivo <- NULL

#Establecemos los grupos de transectas a agrupar

Matriz1$GrupoTrans <- as.factor(rep(1:10,each = 4))

#Calculamos promedios
for(i in 4:12){
    t <- tapply(Matriz1[i][,1],Matriz1$GrupoTrans,mean,na.rm = T)	
    Matriz1[i][,1] <-  sapply(Matriz1$GrupoTrans,function(x){
        unname(t[as.character(x)])
    })
}

#Hacemos unique	
Matriz1$Transecta <- NULL
Matriz1$GrupoTrans <- NULL

Matriz1 <- Matriz1 %>% unique()

#Corregimos nombres
names(Matriz1) <- unname(sapply(names(Matriz1),function(text){
    text <- gsub("\\.\\.","\\.",text)
    text <- gsub("\\.$","",text)   
    text <- gsub("Other.Other.","Other.",text)
    text <- gsub(".other$","",text)
}))

#Almacenamos la original
MatOrig <- Matriz1

#Corales ==================================================
#names(Matriz1)
Matriz1 <- gather(Matriz1,key = FormaCrec.Coral,value = Cobertura.Coral,3:11)

#Especies sin cobertura reciben NA
posVac <- Matriz1$Cobertura.Coral == 0
Matriz1$FormaCrec.Coral[posVac] <- rep(NA,sum(posVac))

#Eliminamos Repetidos
Matriz1 <- Matriz1 %>% unique()

#Reordenamos
Matriz1 <- arrange(Matriz1,Sitio)

#Indicador de lugares sin corales
vacio <- tapply(Matriz1$FormaCrec.Coral,Matriz1$Sitio,function(x){
    all(is.na(x))
})
lugar <- names(which(vacio == FALSE)) #Ubicaciones con datos

#Eliminamos las filas con NA en las ubicaciones con datos
if(length(lugar) != 0){
   Matriz1 <- Matriz1[
       !(is.na(Matriz1$FormaCrec.Coral) & (Matriz1$Sitio %in% lugar)),]
}

#Tratamiento final/////////////////////////////////////

Matriz1 <- Matriz1 %>% unique()

#Exportamos tabla /////////////////////////////////////

setwd("~/Thesis Project AB/Data/Pre-processed Data")

write.csv(Matriz1,file="Matriz_SCLE_FormasCrecimiento_Proc1.csv",row.names = F)

#Limpiamos memoria.
rm(list = ls())
gc()

cat("Finalizado")
cat("\n")