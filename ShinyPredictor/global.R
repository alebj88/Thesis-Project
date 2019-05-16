#En este APP todos los procedimientos dependen del entrenador ingresado

#RAMDOM FOREST XGBOOST (Machine Lerning) ///////////////////////////////////////
library(ggplot2)
library(caret)
library(rpart)
library(tidyverse)
library(dplyr)
library(tidyr)
library(lubridate)
library(ff)
require(ffbase)
require(maps)
require(mapdata)
library(ggrepel)

source("Functions.R")
 
# #El entrenador se crea con el script "EsferasPredictivas.R" 

# #_______________________________________________________________________________
#Panel de evaluacion del modelo excluyendo ecosistemas. 
#Cuando se corra con EvalSeg == TRUE, debera remover manualmente el archivo
#'interpretador.RData' para crear al modelo predictivo completo nuevamente.

EvalSeg <- FALSE  # Eliminar ecosistemas para validacion
#EvalSeg <- TRUE

#Ecosistemas a remover para la prueba
EcoRem <- c("Cayo de Agua","Dos Mosquises","Petaquire","San Agustin","Norte")

#_______________________________________________________________________________

#Predictores a usar en el entrenamiento 

#Entrenar con 4 variables.
variables <- c(
    "SeaSaltColumnDensity.Mean",
    "RemoteReflectance.Mean",   
    "ChlorophyllaConcent.Mean",
    "SSTemperatureDay.Mean" 
)

#Entrenar con todas las variables disponibles (Overfitting).
# variables <- c(
#     "Laguna.d"                     ,"Puerto.d",                   
#     "Rio.d"                        ,"AbsorCoeffPhytoplank.Q3",     
#     "AbsorCoeffNonAlgalMat.Median" ,"ChlorophyllaConcent.Mean",    
#     "PhotosyntheticRadiation.Q3"   ,"RemoteReflectance.Mean",      
#     "SSTemperatureDay.Mean"        ,"SSTemperatureNight.Mean",     
#     "SSTemperatureNight.Kurtosis"  ,"SeaLevelPreassure.Skewness",  
#     "SeaSaltColumnDensity.Mean"    ,"SeaSaltColumnDensity.Sd",     
#     "TwoMAirTemperature.Mean"      ,"SeaSaltConcentration.Mean",   
#     "CarbonDioxide.Q1"             ,"CarbonDioxide.Skewness" ,     
#     "MethaneNight.Median"          ,"MethaneNight.Q3",             
#     "MethaneDay.Kurtosis"          ,"IncidentShortwave.Max",       
#     "IncommingShortwave.Max"       ,"speed.cm.s.min",              
#     "Afluente.Seco.d"              ,"Petroleo.d" ,                 
#     "Poblado.d"
# )

################################################################################
################################################################################
################################################################################

#FUNCIONES EMPLEADAS EN EL PROCESAMIENTO DE DATOS.
interBuilder <- function(df,test){
    
    ##Configuramos el train control. 
    tune.grid <- list(cp=c(0.0001,0.001,0.01,0.1,1,10,100),
                      minsplit = c(10,20,30,40),
                      maxdepth = c(10,20,30)) %>% 
                 cross_df() #Convert to data frame grid

    #Funcion de Search Grid
    mod <- function(...) {
        rpart(Ecosystem ~ .,data = df,control = rpart.control(...))
    }
    
    tune.grid <- tune.grid %>% mutate(fit = pmap(tune.grid, mod))

    #Obtener precision
    compute_accuracy <- function(fit, test_features, test_labels) {
        predicted <- predict(fit, test_features, type = "class")
        mean(predicted == test_labels)
    }
    
    #Ajuste de modelos
    test_features <- df %>% select(-Ecosystem)
    test_labels   <- df$Ecosystem
    
    tune.grid <- tune.grid %>%
        mutate(test_accuracy = map_dbl(fit, compute_accuracy,
                                       test_features, test_labels))
    tune.grid <- tune.grid %>% 
        arrange(desc(test_accuracy), desc(minsplit), maxdepth)
    
    #Creamos modelos final 
    modFit <- rpart(Ecosystem ~ .,data = df,
                    control = rpart.control(cp = as.numeric(tune.grid[1,1]),
                                      minsplit = as.numeric(tune.grid[1,2]),
                                      maxdepth = 20))
                                      #maxdepth = as.numeric(tune.grid[1,3])))

    #Filtro de predictores
    #print(modFit$variable.importance)
    
    # df <- df[c("Ecosystem",names(modFit$variable.importance)[c(1:3)])]
    # 
    # modFit <- rpart(Ecosystem ~ .,data = df,
    #                 control = rpart.control(cp = as.numeric(tune.grid[1,1]),
    #                                  minsplit = as.numeric(tune.grid[1,2]),
    #                                  #maxdepth = 20))
    #                                  maxdepth = as.numeric(tune.grid[1,3])))
    
    #Poda automatica del arbol.
    modFit <- prune(modFit,cp = modFit$cptable[
        which.min(modFit$cptable[,"xerror"]),"CP"
    ])	
    
    #Eliminamos datos repetidos para mayor precision en la evaluacion
    test <- test %>% unique()
    df <- df %>% unique()
    
    #Evaluacion de la prediccion
    png(file="ErrorVsCP.png")
    plotcp(modFit)
    dev.off()
    
    png(file="Decision_Tree.png")
    rattle :: fancyRpartPlot(modFit)
    dev.off()
    
    sink("Variable_Importance.txt")
    print(modFit$variable.importance)
    sink()
    
    #Evaluacion en el training set////////////////////////////
    pred <- predict(modFit,df,type="class")
    sink("TestDelInterpretador_TrainingSet.txt")
    print(modFit)
    print(confusionMatrix(pred,as.factor(df$Ecosystem)))
    sink()
 
    #Evaluacion en el testing set/////////////////////////////
    pred <- predict(modFit,test,type="class")
    
    ##################
    DataSet <- read.csv("SCLESpecies_Matrix.csv")
    
    #Unimos objetos costeros
    DataSet$Afluente.Seco.d <- unname(apply(DataSet[c(
        "Quebrada.Seca.d","Rio.Seco.d")],1,function(x){
            vect <- min(x,na.rm = T)
            return(vect)
        }))
    
    #Unimos objetos costeros
    DataSet$Petroleo.d <- unname(apply(DataSet[c(
        "Termoelectrica.d","Refineria.d","Petroquimica.d")],1,function(x){
            vect <- min(x,na.rm = T)
            return(vect)
        }))
    
    #Unimos objetos costeros
    DataSet$Poblado.d <- unname(apply(DataSet[c(
        "Pueblo.d","Ciudad.d")],1,function(x){
            vect <- min(x,na.rm = T)
            return(vect)
        }))
    
    #Removemos las no utiles
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
        "Poblado.d",
        "Site"
        
    ))
    DataSet <- DataSet[posNoElim]
    
    #Transformamos 999's en 120's en las variables de Google Earth
    for(k in c(1,2,3,25,26,27)){
        DataSet[,k][DataSet[k][,1] == 999]<- rep(120,sum(DataSet[k][,1] == 999))
    }

    ################
    
    pred2 <- predict(modFit,DataSet,type="class")
    tabla <- data.frame(Real = DataSet$Site,Prediccion = pred2)
    tabla$Acierto <- apply(tabla,1,function(fila){
        if(fila[1] %in% strsplit(fila[2],split = "-")[[1]]){
            return(TRUE)
        }else{
            return(FALSE)
        }
    })
    sink("TestDelInterpretador_TestingSet.txt")
    print(modFit)
    print(confusionMatrix(pred,as.factor(test$Ecosystem)))
    print("----------------------------------------------------------")
    print("----------------------Evaluacion Final--------------------")
    print("----------------------------------------------------------")
    print(tabla)
    print(paste0("Porcentaje de aciertos: ", 
                 round(100*sum(tabla$Acierto)/nrow(tabla),2),"%"))
    sink()
    
    #Exportamos
    return(modFit)
}

################################################################################
################################################################################
################################################################################

#Lectura del modelo 
if("interpretador.RData" %in% dir() == TRUE & EvalSeg == FALSE){
    load("interpretador.RData") 
    
}else{
    cat("Montando el modelo predictivo...\n")
    entrenador <- read.table("entrenador.txt",header = TRUE)
    
    #Evaluar con segmentacion
    if(EvalSeg == TRUE){
        entrenador$Ecosystem <- as.character(entrenador$Ecosystem)
        entrenador <- entrenador[which(!(entrenador$Ecosystem %in% EcoRem)),] 
        entrenador$Ecosystem <- as.factor(entrenador$Ecosystem)
    }
    entrenador$latitude <- NULL
    entrenador$longitude <- NULL
    
    #Seleccion precisa de variables-----------------------------
    #CON ECOSYSTEM Y SIN LATITUDE LONGITUDE.
    posNoElim <- which(names(entrenador) %in% c("Ecosystem",variables))

    #Eliminamos las variables no usadas
    entrenador <- entrenador[posNoElim]

    #-----------------------------------------------------------
    
    
    #Data Partition
    set.seed(3649856)
    
    inTrain <- createDataPartition(entrenador$Ecosystem, p = 0.7,list = FALSE)     		#Usamos library(caret). Opcional.              
    testing <- entrenador[-inTrain,]
    training <- entrenador[inTrain,]
    
    modFit <- interBuilder(training,testing) 
    save(modFit,file = "interpretador.RData")
    cat("Modelo Generado.\n")
}
   
TestDelInter <- readLines("TestDelInterpretador_TestingSet.txt")

#Importamos el entrenador y eliminamos las variables no usadas
entrenador <- read.table("entrenador.txt",header = TRUE)

posNoElim <- which(names(entrenador) %in% c("Ecosystem",variables))
entrenador <- entrenador[posNoElim]

#Evaluar con segmentacion
if(EvalSeg == TRUE){
    entrenador$Ecosystem <- as.character(entrenador$Ecosystem)
    entrenador <- entrenador[which(!(entrenador$Ecosystem %in% EcoRem)),]
    entrenador$Ecosystem <- as.factor(entrenador$Ecosystem)
}

#Cargamos tados biologicos
bioDat1 <- read.csv("SCLESpecies.csv") 
bioDat2 <- read.csv("SCLEGrowth.csv") 
bioDat3 <- read.csv("GreatGroups.csv") 
bioDat4 <- read.csv("BenthicSubstrate.csv") 
AmbDat1 <- read.csv("SCLESpecies_Matrix.csv") 

#Agregamos ecosistemas
names(bioDat1)[names(bioDat1) == "Site"] <- "Ecosystem"
names(bioDat2)[names(bioDat2) == "Site"] <- "Ecosystem"
names(bioDat3)[names(bioDat3) == "Site"] <- "Ecosystem"
names(bioDat4)[names(bioDat4) == "Site"] <- "Ecosystem"
names(AmbDat1)[names(AmbDat1) == "Site"] <- "Ecosystem"

if(EvalSeg == TRUE){
    #BioDat1
    bioDat1$Ecosystem <- as.character(bioDat1$Ecosystem)
    bioDat1 <- bioDat1[which(!(bioDat1$Ecosystem %in% EcoRem)),]
    bioDat1$Ecosystem <- as.factor(bioDat1$Ecosystem)
    
    #BioDat2
    bioDat2$Ecosystem <- as.character(bioDat2$Ecosystem)
    bioDat2 <- bioDat2[which(!(bioDat2$Ecosystem %in% EcoRem)),]
    bioDat2$Ecosystem <- as.factor(bioDat2$Ecosystem)
    
    #BioDat3
    bioDat3$Ecosystem <- as.character(bioDat3$Ecosystem)
    bioDat3 <- bioDat3[which(!(bioDat3$Ecosystem %in% EcoRem)),]
    bioDat3$Ecosystem <- as.factor(bioDat3$Ecosystem)
    
    #BioDat4
    bioDat4$Ecosystem <- as.character(bioDat4$Ecosystem)
    bioDat4 <- bioDat4[which(!(bioDat4$Ecosystem %in% EcoRem)),]
    bioDat4$Ecosystem <- as.factor(bioDat4$Ecosystem)
    
    #AmbDat1
    AmbDat1$Ecosystem <- as.character(AmbDat1$Ecosystem)
    AmbDat1 <- AmbDat1[which(!(AmbDat1$Ecosystem %in% EcoRem)),]
    AmbDat1$Ecosystem <- as.factor(AmbDat1$Ecosystem)
}

solapado <- TRUE
if(solapado == TRUE){
    ecosistemas <-  as.character(unique(entrenador$Ecosystem))
    
    bioDat4$Ecosystem <- as.character(bioDat4$Ecosystem)
    #Ciclo
    for(i in 1:nrow(bioDat1)){
        
        for(j in 1:length(ecosistemas)){
            inside <- bioDat4$Ecosystem[i] %in% 
                strsplit(ecosistemas[j],split = "-")[[1]]
            
            if(inside == TRUE){
                bioDat4$Ecosystem[i] <- ecosistemas[j]
                break
            }
        }
    }
    bioDat4$Ecosystem <- as.factor(bioDat4$Ecosystem)
    #--------------------------
    
    bioDat3$Ecosystem <- as.character(bioDat3$Ecosystem)
    #Ciclo
    for(i in 1:nrow(bioDat2)){
        
        for(j in 1:length(ecosistemas)){
            inside <- bioDat3$Ecosystem[i] %in% 
                strsplit(ecosistemas[j],split = "-")[[1]]
            
            if(inside == TRUE){
                bioDat3$Ecosystem[i] <- ecosistemas[j]
                break
            }
        }
    }
    bioDat3$Ecosystem <- as.factor(bioDat3$Ecosystem)
    #--------------------------
    
    bioDat2$Ecosystem <- as.character(bioDat2$Ecosystem)
    #Ciclo
    for(i in 1:nrow(bioDat3)){
        
        for(j in 1:length(ecosistemas)){
            inside <- bioDat2$Ecosystem[i] %in% 
                strsplit(ecosistemas[j],split = "-")[[1]]
            
            if(inside == TRUE){
                bioDat2$Ecosystem[i] <- ecosistemas[j]
                break
            }
        }
    }
    bioDat2$Ecosystem <- as.factor(bioDat2$Ecosystem)
    #--------------------------
    
    bioDat1$Ecosystem <- as.character(bioDat1$Ecosystem)
    #Ciclo
    for(i in 1:nrow(bioDat4)){
        
        for(j in 1:length(ecosistemas)){
            inside <- bioDat1$Ecosystem[i] %in% 
                strsplit(ecosistemas[j],split = "-")[[1]]
            
            if(inside == TRUE){
                bioDat1$Ecosystem[i] <- ecosistemas[j]
                break
            }
        }
    }
    bioDat1$Ecosystem <- as.factor(bioDat1$Ecosystem)
    #--------------------------
    
    AmbDat1$Ecosystem <- as.character(AmbDat1$Ecosystem)
    #Ciclo
    for(i in 1:nrow(AmbDat1)){
        
        for(j in 1:length(ecosistemas)){
            inside <- AmbDat1$Ecosystem[i] %in% 
                strsplit(ecosistemas[j],split = "-")[[1]]
            
            if(inside == TRUE){
                AmbDat1$Ecosystem[i] <- ecosistemas[j]
                break
            }
        }
    }
    AmbDat1$Ecosystem <- as.factor(AmbDat1$Ecosystem)
}



#Check
if(all(unique(entrenador$Ecosystem) %in% unique(bioDat4$Ecosystem)) == F){
    stop("Los ecosistemas no coinciden. bioDat4")
}
if(all(unique(entrenador$Ecosystem) %in% unique(bioDat3$Ecosystem)) == F){
    stop("Los ecosistemas no coinciden. bioDat3")
}
if(all(unique(entrenador$Ecosystem) %in% unique(bioDat2$Ecosystem)) == F){
    stop("Los ecosistemas no coinciden. bioDat2")
}
if(all(unique(entrenador$Ecosystem) %in% unique(bioDat1$Ecosystem)) == F){
    stop("Los ecosistemas no coinciden. bioDat1")
}
if(all(unique(entrenador$Ecosystem) %in% unique(AmbDat1$Ecosystem)) == F){
    stop("Los ecosistemas no coinciden. AmbDat1")
}
# if(all(unique(bioDat4$Ecosystem) %in% unique(entrenador$Ecosystem)) == F){
#     stop("Los ecosistemas no coinciden. bioDat4-E")
# }
# if(all(unique(bioDat3$Ecosystem) %in% unique(entrenador$Ecosystem)) == F){
#     stop("Los ecosistemas no coinciden. bioDat3-E")
# }
# if(all(unique(bioDat2$Ecosystem) %in% unique(entrenador$Ecosystem)) == F){
#     stop("Los ecosistemas no coinciden. bioDat2-E")
# }
# if(all(unique(bioDat1$Ecosystem) %in% unique(entrenador$Ecosystem)) == F){
#     stop("Los ecosistemas no coinciden. bioDat1-E")
# }
# if(all(unique(AmbDat1$Ecosystem) %in% unique(entrenador$Ecosystem)) == FALSE){
#     stop("Los ecosistemas no coinciden. AmbDat1-E")
# }

#Unimos objetos costeros
AmbDat1$Afluente.Seco.d <- unname(apply(AmbDat1[c(
    "Quebrada.Seca.d","Rio.Seco.d")],1,function(x){
        vect <- min(x,na.rm = T)
        return(vect)
    }))

AmbDat1$Quebrada.Seca.d <- NULL
AmbDat1$Rio.Seco.d <- NULL

#Unimos objetos costeros
AmbDat1$Petroleo.d <- unname(apply(AmbDat1[c(
    "Termoelectrica.d","Refineria.d","Petroquimica.d")],1,function(x){
        vect <- min(x,na.rm = T)
        return(vect)
    }))

AmbDat1$Termoelectrica.d <- NULL
AmbDat1$Refineria.d <- NULL
AmbDat1$Petroquimica.d <- NULL

#Unimos objetos costeros
AmbDat1$Poblado.d <- unname(apply(AmbDat1[c(
    "Pueblo.d","Ciudad.d")],1,function(x){
        vect <- min(x,na.rm = T)
        return(vect)
    }))

AmbDat1$Pueblo.d <- NULL
AmbDat1$Ciudad.d <- NULL


#Eliminamos variables colineales.
posNoElim <- which(names(AmbDat1) %in% c(
    "Ecosystem",
    "latitude"                     ,"longitude",
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

#Eliminamos variables biologicas
AmbDat1 <- AmbDat1[posNoElim]

#Reordenamos
AmbDat1 <- AmbDat1[c(1:6,28:30,7:27)]

#Transformamos 999's en 120's en las variables de Google Earth
for(k in 7:9){
    AmbDat1[,k][AmbDat1[k][,1] == 999] <- rep(120,sum(AmbDat1[k][,1] == 999))
}

#Seleccion precisa de variables-----------------------------
#CON ECOSYSTEM Y CON LATITUDE LONGITUDE.
posNoElim <- which(names(AmbDat1) %in% 
                       c("Ecosystem","latitude","longitude",variables))

#Eliminamos variables biologicas
AmbDat1 <- AmbDat1[posNoElim]

#-----------------------------------------------------------


#Reordenamos
bioDat1 <- bioDat1[c(1,3:ncol(bioDat1),2)]
bioDat2 <- bioDat2[c(1,3:ncol(bioDat2),2)]
bioDat3 <- bioDat3[c(1,3:ncol(bioDat3),2)]
bioDat4 <- bioDat4[c(1,3:ncol(bioDat4),2)]
AmbDat1 <- AmbDat1[c(2:ncol(AmbDat1),1)]

#Liberamos memoria
rm(solapado);rm(TestDelInter);rm(posNoElim);rm(EvalSeg);rm(i);rm(j);rm(k)
rm(interBuilder);rm(inside);rm(EcoRem)
gc()

#Generamos informacion de confiabilidad---------------------

dfmin  <- data.frame(Ecosystem = unique(entrenador$Ecosystem))
dfmax <- data.frame(Ecosystem = unique(entrenador$Ecosystem))

#Calculo de minimos
posEco <- which(names(entrenador) == "Ecosystem")

proc <- unname(sapply(entrenador[,-posEco],function(v){
    vmin <- as.numeric(tapply(v,entrenador$Ecosystem,min))
    dfmin$aux <<- vmin
    vmax <- as.numeric(tapply(v,entrenador$Ecosystem,max))
    dfmax$aux <<- vmax
    names(dfmax)[ncol(dfmax)] <<- as.character(v[1])
    names(dfmin)[ncol(dfmin)] <<- as.character(v[1])
    return(NULL)
}))

names(dfmin) <- c("Ecosystem",paste0(names(entrenador[,-posEco]),"_min"))
names(dfmax) <-c("Ecosystem",paste0(names(entrenador[,-posEco]),"_max"))

#Datos a evaluar del mar venezolano --------------------------

MarVzla <- read.csv("PredictoresMarVzla.csv")

MarVzla <- MarVzla[,which(names(MarVzla) %in% c(
    "latitude","longitude",variables)
)]

#Reordenamos
MarVzla <- MarVzla[
    c("latitude","longitude",unname(sapply(names(dfmin)[-1],function(text){
        text <- strsplit(text,split = "_")[[1]][1]
        text <- grep(paste0("^",text),variables,value=TRUE)
        return(text)
    })))
]


#Creamos variable de evaluacion
MarVzla$Confiable <- unname(apply(MarVzla[,3:ncol(MarVzla)],1,function(vars){
    conf <- FALSE
    #Ciclo de Busqueda
    for(i in 1 : nrow(dfmin)){
        if(all(vars >= as.numeric(dfmin[i,2:(ncol(dfmin))])) &
           all(vars <= as.numeric(dfmax[i,2:(ncol(dfmax))]))){
            conf <- TRUE
            break
        }
    }
    return(conf)
}))

#Eliminamos puntos no confiables
MarVzla2 <- MarVzla[MarVzla$Confiable == TRUE,]

#Plot del mapa
global <- map_data("world")

#recortamos

global <- global[global$long <= -60 & global$long >= -74 &
                     global$lat <= 17 & global$lat >= 8,]

plotMapa  <- ggplot() +
    geom_polygon(data = global, aes(x=long, y = lat, group = group),
                 fill = "green4", color = "black") +
    coord_fixed(1.3) +
    geom_point(data = MarVzla2,aes(x = longitude,y = latitude),
               col = "blue",size = 1) +
    ggtitle("Mar Confiable")

#Evaluacion del modelo
png(file="MarConfiable.png")

plot(plotMapa)

dev.off()

#Eliminamos puntos no confiables
MarVzla2 <- MarVzla[MarVzla$Confiable == FALSE,]

#Plot del mapa
global <- map_data("world")

#recortamos

global <- global[global$long <= -60 & global$long >= -74 &
                     global$lat <= 17 & global$lat >= 8,]

plotMapa  <- ggplot() +
    geom_polygon(data = global, aes(x=long, y = lat, group = group),
                 fill = "green4", color = "black") +
    coord_fixed(1.3) +
    geom_point(data = MarVzla2,aes(x = longitude,y = latitude),
               col = "red",size = 1) +
    ggtitle("Mar No Confiable")

#Evaluacion del modelo
png(file="MarNoConfiable.png")

plot(plotMapa)

dev.off()

cat("Graficos de confiabilidad generados en la carpeta de la APP.\n")

#Creamos la tabla PredictoresMarVzla_Ecosystem.csv ----------------
pred <- predict(modFit,MarVzla,type="class") 

#Agregamos prediccion
MarVzla$Ecosystem <- pred

#Exportamos
write.csv(MarVzla,file ="PredictoresMarVzla_Ecosystem.csv",
          row.names = F)

cat("Tabla PredictoresMarVzla_Ecosystem.csv creada en la carpeta de la APP.\n")

#Liberamos memoria
rm(proc);rm(plotMapa);rm(posEco);rm(MarVzla2)
rm(pred)
gc()