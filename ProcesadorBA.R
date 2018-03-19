#Programa que a solicitud del usuario busca variables dentro de las tablas de
#Pre-processed Data 2 para unirlas por geolocalizacion a una matriz biologica
#presente en la carpeta Processed Data. Esta ultima tambien es indicada por el 
#usuario.
#El usuario indica los radios de imputacion y se realiza el procedimiento
#impute.per.region2. La tabla unificada se retorna en Processed Data.
library(dplyr)
library(tidyr)
options(scipen=20)

setwd("~/Thesis Project AB/R-Scripts")
source('Functions.R')
#tablaB<-impute.per.region2(tablaB,tablaA,radio=radios,medida=medida)

setwd("~/Thesis Project AB/Data/Processed Data")
if(length(dir()) != 0){
    cat("\n")
    cat("Indique si desea agregarle variables a alguna tabla biologica\n")
    cat("usando los datos existentes en entornos proximos.")
    
    userInput <- FALSE
    while(userInput == FALSE){
        humanT<- readline(prompt="Introduzca Si o No: ")
        cat("\n")
        if(humanT %in% c("NO","No","no","SI","Si","si")){
            userInput <- TRUE
        }else{
            message("Input invalido!")
        }
        if(humanT %in% c("NO","No","no")){
            humanT <- FALSE
        }else{
            humanT <- TRUE
        }
    } 
    #Solicitamos tabla biologica si el usuario acepta procesar datos.
    if(humanT == TRUE){
        cat("Archivos disponibles:\n")
        df <-data.frame(I______Archivo______I = dir())
        print(df)
        message("Seleccione la tabla biologica a la que le desea agregar")
        message("variables.")
        
        valido <-FALSE
        while(valido == FALSE){
            inp2 <- readline(prompt="Introduzca el numero correspondiente: ")
            cat("\n")
            if(suppressWarnings(is.na(as.numeric(inp2))) == FALSE){
                inp2 <- as.numeric(inp2)
                if(inp2 %in% seq(1,length(dir()))){
                    valido <-TRUE  
                }else{
                    message("Input invalido!")  
                }
            }else{
                message("Input invalido!")
            }
        }
        #Nombre de la tabla
        inp2N <- dir()[inp2]
        tablaB <- read.csv(inp2N)
        #Check
        if(any(!(c("latitude","longitude") %in% names(tablaB)))){
            stop("La tabla biologica debe tener las variables 'latitude','longitude'.")
        }
        message("Tabla biologica ",inp2N," cargada.")
        
        #Solicitamos tabla ambiental.
        setwd("~/Thesis Project AB/Data/Pre-processed Data 2")
        cat("Indique que archivo desea usar para extraer variables.\n")
        repeat{
            arch <- dir()[dir() != "procesadas.txt"]
            df <-data.frame(I______Archivo______I = arch)
            print(df)
            
            valido <- FALSE
            while(valido == FALSE){
                inp3 <- readline(prompt="Introduzca su numero: ")
                cat("\n")
                if(suppressWarnings(is.na(as.numeric(inp3))) == FALSE){
                    inp3 <- as.numeric(inp3)
                    if(inp3 %in% seq(1,length(arch))){
                        valido <- TRUE  
                    }else{
                        message("Input invalido!")  
                    }
                }else{
                    message("Input invalido!")
                }
            }
            #Nombre de la tabla
            inp3N <- arch[inp3]
            tablaA <- read.csv(inp3N)
            if(all(names(tablaA) %in% names(tablaB))){
                message("La tabla elegida no tiene variables distintas a las ")
                message("existentes en la matriz biologica.")
            }else{
                message("Tabla ambiental ",inp3N," cargada.")
                break
            }
        }
        
        #Si la tabla elegida fue creada con Google-Earth la reescribimos usando 
        #los calculos de distancia correspondientes.
        #Variables de tablaA: latitude,longitude, lat.objeto,lon.objeto, objeto,
        #medida, valor.
        if(inp3N == "GoogleEarthRiosLugares.csv"){
            message("Se selecciono una tabla con informacion de Google Earth.\n")
            
            #Solicitamos distancia maxima en kilometros.
            val <-FALSE
            while(val == FALSE){
                cat("Indique la distancia maxima en kilometros a la que se \n")
                cat("buscaran puntos de interes en los alrededores de la zona\n")
                cat("de estudio.\n")
                dlMax <- readline(prompt="Introduzca un numero mayor o igual que 1: ")
                cat("\n")
                if(suppressWarnings(is.na(as.numeric(dlMax))) == FALSE){
                    dlMax <- as.numeric(dlMax)
                    if(dlMax >= 1){
                        val <-TRUE  
                    }else{
                        message("Input invalido!")  
                    }
                }else{
                    message("Input invalido!")
                }
            }
            message("Registrado.")
            
            #Solicitamos tiempo maximo de viaje en corriente en dias
            val <-FALSE
            while(val == FALSE){
                cat("Indique el tiempo maximo de viaje en corriente (dias) que\n")
                cat("se utilizara para evaluar la influencia de puntos de interes.\n")
                tiempoMax <- readline(prompt="Introduzca un numero mayor o igual que 1: ")
                cat("\n")
                if(suppressWarnings(is.na(as.numeric(tiempoMax))) == FALSE){
                    tiempoMax <- as.numeric(tiempoMax)
                    if(tiempoMax >= 1){
                        val <-TRUE  
                    }else{
                        message("Input invalido!")  
                    }
                }else{
                    message("Input invalido!")
                }
            }
            message("Registrado.")
            
            cat("Procesando la tabla de Google Earth.\n")
            
            tablaA <- distGoogleEarth(tablaB[c("latitude","longitude")],tablaA,
                                            dlMax = dlMax,tiempoMax = tiempoMax)
            rownames(tablaA) <- 1:nrow(tablaA)
            message("Datos cargados.\n")
        }
        
        #Preguntamos si desea agregar todas las variables o solo una seccion.
        cat("Desea agregar a la tabla '",inp2N,"'\n")
        cat("todas las variables contenidas en")
        cat("'",inp3N,"'\n")
        cat("?")
        userInput <- FALSE
        while(userInput == FALSE){
            humanT2<- readline(prompt="Introduzca Si o No: ")
            cat("\n")
            if(humanT2 %in% c("NO","No","no","SI","Si","si")){
                userInput <- TRUE
            }else{
                message("Input invalido!")
            }
            if(humanT2 %in% c("NO","No","no")){
                humanT2 <- FALSE
            }else{
                humanT2 <- TRUE
            }
        }
        vars <- names(tablaA)[!(names(tablaA) %in% names(tablaB))]
        
        #Si el usuario no desea agregarlas todas
        if(humanT2 == FALSE){
            message("Presentamos el conjunto de variables para su eleccion.")
            df <- data.frame(I______Variables______I = vars)
            df[,1] <- as.character(df[,1])
            
            elecciones <-c()
            repetir <- TRUE
            while(repetir == TRUE){
                if(length(vars[!(vars %in% vars[elecciones])]) == 0){
                    message("Seleccion finalizada. Se procesaran las variables:")
                    df <- data.frame(I______Variables______I = vars[elecciones])
                    print(df)
                    break
                }
                cat("Variables disponibles:\n")
                print(df)
                
                valido <-FALSE
                while(valido == FALSE){
                    inp4 <- readline(prompt="Introduzca el numero de la variable que desea agregar o 0 para finalizar: ")
                    cat("\n")
                    if(suppressWarnings(is.na(as.numeric(inp4))) == FALSE){
                        inp4 <- as.numeric(inp4)
                        if(inp4 %in% seq(0,length(vars))){
                            if(inp4 != 0){
                                if(df[inp4,1] == "SELECCIONADA_____######"){
                                    message("Esa variable ya fue seleccionada!.")  
                                }else{
                                    valido <-TRUE 
                                }
                            }else{
                                valido <-TRUE   
                            }
                        }else{
                            message("Input invalido!")  
                        }
                    }else{
                        message("Input invalido!")
                    }
                }
                if(inp4 != 0){
                    message("Variable ",vars[inp4]," seleccionada.")
                    #Indicamos que fue seleccionada
                    df[inp4,1] <- "SELECCIONADA_____######"
                    elecciones <-c(elecciones,inp4) 
                }
                if(inp4 == 0 & length(elecciones) == 0){
                    repetir <- TRUE
                    message("Debe realizar al menos una eleccion!.")
                }else if(inp4 == 0 & length(elecciones) != 0){
                    repetir <- FALSE
                    message("Seleccion finalizada. Se procesaran las variables:")
                    df <- data.frame(I______Variables______I = vars[elecciones])
                    print(df)
                }
            }
            vars <- vars[elecciones]
        }
        
        #Las tabals de Google Earth se unen a ls=os datos biologicos por merge.
        #No hace falta recolectar informacion extra sobre el proceso de 
        #imputacion.
        if(inp3N == "GoogleEarthRiosLugares.csv"){
            radios <- 1         #Asignamos valores de relleno.
            medida <- "median"  #Asignamos valores de relleno.
        }else{
            #Solicitamos radio de imputacion en kilometros.
            cat("\n")
            cat("Indique los radios de proximidad en kilometros que se usaran para\n")
            cat("unir los datos por geolocalizacion en orden de importancia.\n")
            cat("De mayor a menor importancia (maximo tres).\n")
            cat("\n")
            
            radios <-c()
            for(i in 1:3){
                
                valido <-FALSE
                while(valido == FALSE){
                    cat("Radio numero ",i,",\n")
                    inp5 <- readline(prompt="Introduzca un numero positivo (en kms) o 0 para finalizar: ")
                    cat("\n")
                    if(suppressWarnings(is.na(as.numeric(inp5))) == FALSE){
                        inp5 <- as.numeric(inp5)
                        if(inp5 >= 0){
                            if(inp5 == 0 & i == 1){
                                message("Debe elegir al menos un radio.") 
                            }else{
                                valido <-TRUE   
                            }
                        }else{
                            message("Input invalido!")  
                        }
                    }else{
                        message("Input invalido!")
                    }
                }
                if(inp5 != 0){
                    radios[i] <- inp5
                }else{
                    break
                }
            }
            if(length(radios) == 1){
                message("Seleccion finalizada. Se usara el radio:")
                df <- data.frame(Radio.Kilometros = radios)
                print(df)
            }else{
                message("Seleccion finalizada. Se usaran los radios:")
                df <- data.frame(radio = radios)
                print(df)
                message("En orden de importancia.") 
            }
            
            #Solicitamos la medida a usar.
            cat("\n")
            cat("Indique la medida a emplear para asignar los datos.\n")
            cat("Mediana o media ponderada.\n")
            cat("\n")
            validoMed <-FALSE
            while(validoMed == FALSE){         
                ipMed <- readline(prompt="Introduzca 'Median' o 'Mean': ")
                cat("\n")
                if(ipMed %in% c("Median","median","Mean","mean")){
                    validoMed <-TRUE
                }else{
                    message("Input invalido!")
                }
            }
            if(ipMed %in% c("Median","median")){
                medida <- "median"
                message("Se empleara la mediana.\n")
            }else{
                medida <- "mean"
                message("Se empleara la media.\n")
            }
        }
        
        #Extraemos las variables a agregar.
        if("depth.m" %in% names(tablaA) == TRUE){
            tablaA <- tablaA[c("latitude","longitude","depth.m",vars)]
        }else{
            tablaA <- tablaA[c("latitude","longitude",vars)]
        }
        
        #Asignando formato correcto a las variables de tablaA
        #Verificacion de tipo de variable.
        claseVars <- unname(sapply(tablaA,class))
        cat("Para agregar las variables estan deben recibir la clase correcta.\n") 
        cat("Las cualitativas: 'factor','character' o 'logical'\n")
        cat("y las cuantitativas 'numeric' o 'integer'. \n")
        cat("\n")
        for(id in 1: ncol(tablaA)){
            cat("Indique si la clase fue identificada correctamente. \n")
            message("Variable: ",names(tablaA)[id])
            message("Clase: ",claseVars[id])
            cat("\n")
            validoK <-FALSE
            while(validoK == FALSE){         
                ip <- readline(prompt="Introduzca 'si' o 'no': ")
                cat("\n")
                if(ip %in% c("si","Si","SI","no","No","NO")){
                    validoK <-TRUE
                }else{
                    message("Input invalido!")
                }
            }
            if(ip %in% c("si","Si","SI")){
                message("Registrado.\n")
            }else{
                validoN <-FALSE
                while(validoN == FALSE){
                    if(claseVars[id]=="integer" | claseVars[id]=="numeric"){
                        cat("La variable es categorica? \n")
                    }else{
                        cat("La variable es numerica? \n")
                    }
                    ip2 <- readline(prompt="Introduzca 'si' o 'no': ")
                    cat("\n")
                    if(ip2 %in% c("si","Si","SI","no","No","NO")){
                        validoN <-TRUE
                    }else{
                        message("Input invalido!")
                    }
                }
                if(ip2 %in% c("si","Si","SI")){
                    if(claseVars[id]=="integer" | claseVars[id]=="numeric"){
                        #"La variable es categorica
                        tablaA[,id] <- as.factor(tablaA[,id])
                    }else{
                        #La variable es numerica
                        tablaA[,id] <- as.numeric(as.character(tablaA[,id]))
                    }
                    message("Ajuste realizado.\n")
                }else{
                    message("No se haran cambios.\n")
                }
            }
        }
        cat("Sr procesaran las variables con la siguiente especificacion.\n")
        cat("\n")
        df <- data.frame(
            VARIABLE___ = names(tablaA),CLASE___ = unname(sapply(tablaA,class)))
        print(df)
        cat("\n")
        cat("Agregando variables...\n")
        cat("\n")
        #Imputamos///////////////////////////////////
        if(inp3N == "GoogleEarthRiosLugares.csv"){
            tablaB<-impute.per.region2(tablaB,tablaA,radio = radios,
                                        medida = medida,Google = TRUE)
        }else{
            tablaB<-impute.per.region2(tablaB,tablaA,radio=radios,medida=medida)
        }
        
        #Buscamos nombre disponible
        setwd("~/Thesis Project AB/Data/Processed Data")
        i<-1
        repeat{
            inp2N2 <- strsplit(inp2N,split="\\.")[[1]]
            nombre <- paste0(inp2N2[1],"_proc",i,".",inp2N2[2])
            if(nombre %in% dir()){
                i <- i+1
            }else{
                break
            }
            if(i == 1000){
                stop("Falla en el codigo repeat.")
            }
        }
        #Exportamos tabla
        write.csv(tablaB,file=nombre,row.names = FALSE) 
        message("La tabla '",inp2N,"' fue procesada exitosamente.")
        message("Se creo el archivo '",nombre,"' en 'Processed Data'.")
    }
    cat("Procedimiento finalizado.\n")
    cat("\n")
    cat("/////////////////////////////////////////////////\n")
    cat("\n")
}else{
    message("No hay tablas biologicas que procesar.\n")
    cat("\n")
    cat("/////////////////////////////////////////////////\n")
    cat("\n") 
}
