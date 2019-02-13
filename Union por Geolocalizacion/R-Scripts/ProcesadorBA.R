#Programa que a solicitud del usuario busca variables dentro de las tablas de
#Pre-processed Data 2 para unirlas por geolocalizacion a una matriz biologica
#presente en la carpeta Processed Data. Esta ultima tambien es indicada por el 
#usuario.
#El usuario indica los radios de imputacion y se realiza el procedimiento
#impute.per.region2. La tabla unificada se retorna en Processed Data.
library(dplyr)
library(tidyr)
library(lubridate)
library(ff)
require(ffbase)
options(scipen=20)

setwd(paste0(d,"/R-Scripts"))
source('Functions.R')

setwd(paste0(d,"/Biologicas"))
if(length(dir()) != 0){
    cat("\n")
    cat("Ejecutando el codigo de union por geolocalizacion.\n")
    cat("Las tablas deben estar en formato CSV separado por comas.\n")
    cat("\n")
    
    #Solicitamos tabla biologica si el usuario acepta procesar datos.
    humanT <- TRUE
    message("Seleccione la tabla biologica a la que le desea agregar")
    message("variables.")
    
    if(humanT == TRUE){
        cat("Archivos disponibles:\n")
        df <-data.frame(I______Archivo______I = dir())
        print(df)

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
        #Check
        if(any(c("depth.m","date") %in% names(tablaB))){
            cat("OBSERVACION:\n")
            message("La tabla biologica seleccionada contiene las variables")
            message("'depth.m' / 'date' probablemente consecuencia de un ")
            message("procesamiento previo. No se recomienda agregarle datos")
            message("ambientales debido a que el procedimiento de union por")
            message("geolocalizacion solo considera latitud y longitud.")
            message("Presione Enter para continuar.\n")
            readline(prompt="")
        }
        message("Tabla biologica ",inp2N," cargada.")
        
        #Solicitamos tabla ambiental.
        setwd(paste0(d,"/Ambientales"))
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
                dlMax <- readline(prompt = "Introduzca un numero mayor que 0: ")
                cat("\n")
                if(suppressWarnings(is.na(as.numeric(dlMax))) == FALSE){
                    dlMax <- as.numeric(dlMax)
                    if(dlMax > 0){
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
                cat("Colocando 0 no se calcularan datos de corrientes.\n")
                tiempoMax <- readline(prompt =
                                "Introduzca un numero mayor o igual que 0: ")
                cat("\n")
                if(suppressWarnings(is.na(as.numeric(tiempoMax))) == FALSE){
                    tiempoMax <- as.numeric(tiempoMax)
                    if(tiempoMax >= 0){
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
        vars <- names(tablaA)[
            !(names(tablaA) %in% c(names(tablaB),"depth.m","date"))]
        
        #Si el usuario no desea agregarlas todas
        if(humanT2 == FALSE){
            message("Presentamos el conjunto de variables para su eleccion.")
            df <- data.frame(I______Variables______I = vars)
            df[,1] <- as.character(df[,1])
            
            elec <-c()
            repetir <- TRUE
            while(repetir == TRUE){
                if(length(vars[!(vars %in% vars[elec])]) == 0){
                    message(
                        "Seleccion finalizada. Se procesaran las variables:"
                    )
                    df <- data.frame(I______Variables______I = vars[elec])
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
                    elec <-c(elec,inp4) 
                }
                if(inp4 == 0 & length(elec) == 0){
                    repetir <- TRUE
                    message("Debe realizar al menos una eleccion!.")
                }else if(inp4 == 0 & length(elec) != 0){
                    repetir <- FALSE
                    message("Seleccion finalizada. Se procesaran las variables:")
                    df <- data.frame(I______Variables______I = vars[elec])
                    print(df)
                    cat("\n")
                }
            }
            vars <- vars[elec]
        }
        
        #Solicitamos informacion sobre la profundidad deseada en caso de aplicar.
        if("depth.m" %in% names(tablaA) == TRUE){
            #Preguntamos si desea agregar todas las variables o solo una seccion.
            cat("Desea incluir en las variables seleccionadas los datos \n")
            cat("obtenidos en todos los niveles de medicion a diferentes\n")
            cat("profundidades?\n")
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
            prof <- sort(unique(tablaA$depth.m))
            
            #Si el usuario no desea agregarlas todas
            if(humanT2 == FALSE){
                message("Presentamos el conjunto de profundidades para su eleccion.")
                df <- data.frame(I______Profundidades______I = prof)
                df[,1] <- as.character(df[,1])
                
                elec <-c()
                repetir <- TRUE
                while(repetir == TRUE){
                    if(length(prof[!(prof %in% prof[elec])]) == 0){
                        message("Seleccion finalizada. Se procesaran los siguientes niveles:")
                        df <- data.frame(I______Profundidades______I = prof[elec])
                        print(df)
                        break
                    }
                    cat("Niveles de profundidad disponibles:\n")
                    print(df)
                    
                    valido <-FALSE
                    while(valido == FALSE){
                        inp4 <- readline(prompt="Introduzca el numero del nivel que desea agregar o 0 para finalizar: ")
                        cat("\n")
                        if(suppressWarnings(is.na(as.numeric(inp4))) == FALSE){
                            inp4 <- as.numeric(inp4)
                            if(inp4 %in% seq(0,length(prof))){
                                if(inp4 != 0){
                                    if(df[inp4,1] == "SELECCIONADO_____######"){
                                        message("Ese nivel ya fue seleccionado!.")  
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
                        message("Nivel de profundidad ",prof[inp4]," seleccionado.")
                        #Indicamos que fue seleccionada
                        df[inp4,1] <- "SELECCIONADO_____######"
                        elec <-c(elec,inp4) 
                    }
                    if(inp4 == 0 & length(elec) == 0){
                        repetir <- TRUE
                        message("Debe realizar al menos una eleccion!.")
                    }else if(inp4 == 0 & length(elec) != 0){
                        repetir <- FALSE
                        message("Seleccion finalizada. Se procesaran los siguientes niveles:")
                        df <- data.frame(I______Profundidades______I = prof[elec])
                        print(df)
                        cat("\n")
                    }
                }
                prof <- prof[elec]
            }
        }
        
        #Las tablas de Google Earth se unen a ls=os datos biologicos por merge.
        #No hace falta recolectar informacion extra sobre el proceso de 
        #imputacion.
        if(inp3N == "GoogleEarthRiosLugares.csv"){
            radios <- 1         #Asignamos valores de relleno.
            medida <- "median"  #Asignamos valores de relleno.
        }else{
            #Solicitamos radio de imputacion en kilometros.
            cat("Indique los radios de proximidad en kilometros que se usaran para\n")
            cat("unir los datos por geolocalizacion en orden de importancia.\n")
            cat("De mayor a menor importancia (maximo cuatro).\n")
            cat("\n")
            
            radios <-c()
            for(i in 1:4){
                valido <-FALSE
          
                while(valido == FALSE){
                    cat("Radio numero",i,":\n")
                    inp5 <- readline(prompt="Introduzca un numero positivo (en kms) o 0 para finalizar: ")
                    cat("\n")
                    if(suppressWarnings(is.na(as.numeric(inp5))) == FALSE){
                        inp5 <- as.numeric(inp5)
                        if(inp5 >= 0){
                            if(inp5 == 0 & i == 1){
                                message("Debe elegir al menos un radio.") 
                            }else{
                                if(inp5 == 0){
                                    valido <-TRUE
                                }else{
                                    if(length(radios) >0){
                                        if(radios[length(radios)] >= inp5){
                                            message("Debe ingresar un radio mayor al previamente colocado.")
                                        }else{
                                            valido <-TRUE 
                                        }
                                    }else{
                                        valido <-TRUE  
                                    } 
                                }
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
                ipMed <- readline(prompt="Introduzca 'Mediana' o 'Media': ")
                cat("\n")
                if(ipMed %in% c("Mediana","mediana","Media","media")){
                    validoMed <-TRUE
                }else{
                    message("Input invalido!")
                }
            }
            if(ipMed %in% c("Mediana","mediana")){
                medida <- "median"
                message("Se empleara la mediana.\n")
            }else{
                medida <- "mean"
                message("Se empleara la media.\n")
            }
        }
        
        #Extraemos las variables a agregar.
        if("date" %in% names(tablaA) == TRUE){
            if("depth.m" %in% names(tablaA) == TRUE){
                tablaA <-tablaA[c("date","latitude","longitude","depth.m",vars)]
                tablaA <-tablaA[tablaA$depth.m %in% prof,]
            }else{
                tablaA <-tablaA[c("date","latitude","longitude",vars)]
            }
            tablaDate <- tablaA["date"]
            tablaA <- tablaA[,-1]
        }else{
            if("depth.m" %in% names(tablaA) == TRUE){
                tablaA <- tablaA[c("latitude","longitude","depth.m",vars)]
                tablaA <- tablaA[tablaA$depth.m %in% prof,]
            }else{
                tablaA <- tablaA[c("latitude","longitude",vars)]
            }
            tablaDate <- NULL
        }
        #Check
        if("date" %in% names(tablaA) == TRUE){
            stop("La variable 'date' no se removio correctamente.")
        }
        
        #Asignando formato correcto a las variables de tablaA
        #Verificacion de tipo de variable.
        tablaA <- as.data.frame(tablaA)
        
        cat("Desea evaluar la asignacion de clases\n")
        cat("realizada por el programa?\n")

        userInput <- FALSE
        while(userInput == FALSE){
            humanT2 <- readline(prompt="Introduzca Si o No: ")
            cat("\n")
            if(humanT2 %in% c("NO","No","no","SI","Si","si")){
                userInput <- TRUE
            }else{
                message("Input invalido!")
            }
            if(humanT2 %in% c("NO","No","no")){
                EvaluacionClase <- FALSE
            }else{
                EvaluacionClase <- TRUE
            }
        }

        if(EvaluacionClase == TRUE){
            cat("\n")
            cat("Para agregar las variables estan deben recibir la clase correcta.\n") 
            cat("Las cualitativas: 'factor','character' o 'logical'\n")
            cat("y las cuantitativas 'numeric' o 'integer'. \n")
            cat("\n")
            claseVars <- unname(sapply(tablaA,function(x){
                cl <- class(x[1])
                return(cl)
            }))
            for(id in 1: ncol(tablaA)){
                cat("Indique si la clase fue identificada correctamente. \n")
                message("Variable: ",names(tablaA)[id])
                message("Clase: ",claseVars[id])
                cat("\n")
                validoK <-FALSE
                while(validoK == FALSE){         
                    ip <- readline(prompt = "Introduzca 'si' o 'no': ")
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
        }
        cat("Se procesaran las variables con la siguiente especificacion.\n")
        cat("\n")
        df <- data.frame(
            VARIABLE___ = names(tablaA),CLASE___ = unname(sapply(tablaA,class)))
        print(df)
        cat("\n")
        cat("Agregando variables...\n")
        cat("\n")
        
        #Solicitamos informacion sobre la fecha deseada en caso de aplicar.
        if(is.null(tablaDate) == FALSE){
            
            #Preguntamos si desea agregar todas las variables o solo una seccion.
            message("La tabla seleccionada posee la variable 'date'.")
            cat("Desea aplicarle el resumen estadistico por variable a todo el\n")
            cat("rango de fechas?\n")
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
            #Formato de fecha.
            tablaDate <- as.POSIXct(tablaDate[,1])
            fecha <- tablaDate
            fechaIni <- min(fecha)
            fechaFin <- max(fecha)
            
            #Chequeamos si la fecha tienen horas minutos y segundos.
            if(all(hour(fecha) == 0)){
                horas <- FALSE
            }else{
                horas <- TRUE
            }
                
            #Si el usuario no desea agregarlas todas
            if(humanT2 == FALSE){
                
                #Anio Inicial////////////////////
                message("Seleccione el anio de la fecha inicial.")
                df <- data.frame(I______Anio______I = unique(year(fecha)))
                df[,1] <- as.character(df[,1])
                
                cat("Fechas disponibles:\n")
                print(df)
                
                valido <-FALSE
                while(valido == FALSE){
                    y1 <- readline(prompt="Introduzca la posicion del anio deseado: ")
                    cat("\n")
                    if(suppressWarnings(is.na(as.numeric(y1))) == FALSE){
                        y1 <- as.numeric(y1)
                        if(y1 %in% seq(1,length(unique(year(fecha))))){
                            valido <-TRUE 
                        }else{
                            message("Input invalido!")  
                        }
                    }else{
                        message("Input invalido!")
                    }
                }
                message("Registrado.")
                year1 <- unique(year(fecha))[y1]
                
                #Actualizamos
                fecha <- fecha[year(fecha) == year1]
                
                #Mes Inicial////////////////////
                message("Seleccione el mes de de la fecha inicial.")
                df <- data.frame(I______Mes______I = unique(month(fecha)))
                df[,1] <- as.character(df[,1])
                
                cat("Meses disponibles:\n")
                print(df)
                
                valido <-FALSE
                while(valido == FALSE){
                    m1 <- readline(prompt="Introduzca la posicion del mes deseado: ")
                    cat("\n")
                    if(suppressWarnings(is.na(as.numeric(m1))) == FALSE){
                        m1 <- as.numeric(m1)
                        if(m1 %in% seq(1,length(unique(month(fecha))))){
                            valido <-TRUE 
                        }else{
                            message("Input invalido!")  
                        }
                    }else{
                        message("Input invalido!")
                    }
                }
                message("Registrado.")
                month1 <- unique(month(fecha))[m1]
                
                #Actualizamos
                fecha <- fecha[month(fecha) == month1]
                
                #Dia Inicial////////////////////
                message("Seleccione el dia de de la fecha inicial.")
                df <- data.frame(I______Mes______I = unique(day(fecha)))
                df[,1] <- as.character(df[,1])
                
                cat("Dias disponibles:\n")
                print(df)
                
                valido <-FALSE
                while(valido == FALSE){
                    d1 <- readline(prompt="Introduzca la posicion del dia deseado: ")
                    cat("\n")
                    if(suppressWarnings(is.na(as.numeric(d1))) == FALSE){
                        d1 <- as.numeric(d1)
                        if(d1 %in% seq(1,length(unique(day(fecha))))){
                            valido <-TRUE 
                        }else{
                            message("Input invalido!")  
                        }
                    }else{
                        message("Input invalido!")
                    }
                }
                message("Registrado.")
                day1 <- unique(day(fecha))[d1]
                
                #Horas Minutos Segundos...
                if(horas == TRUE){
                    #Actualizamos
                    fecha <- fecha[day(fecha) == day1]
                    
                    #Hora Inicial////////////////////
                    message("Seleccione la hora de inicio.")
                    df <- data.frame(I______Hora______I = unique(hour(fecha)))
                    df[,1] <- as.character(df[,1])
                    
                    cat("Horas disponibles:\n")
                    print(df)
                    
                    valido <-FALSE
                    while(valido == FALSE){
                        y1 <- readline(prompt="Introduzca la posicion de la hora deseada: ")
                        cat("\n")
                        if(suppressWarnings(is.na(as.numeric(y1))) == FALSE){
                            y1 <- as.numeric(y1)
                            if(y1 %in% seq(1,length(unique(hour(fecha))))){
                                valido <-TRUE 
                            }else{
                                message("Input invalido!")  
                            }
                        }else{
                            message("Input invalido!")
                        }
                    }
                    message("Registrado.")
                    hora1 <- unique(hour(fecha))[y1]
                    
                    #Actualizamos
                    fecha <- fecha[hour(fecha) == hora1]
                    
                    #Minuto Inicial////////////////////
                    message("Seleccione el minuto de la hora de inicio.")
                    df<-data.frame(I______Minuto______I = unique(minute(fecha)))
                    df[,1] <- as.character(df[,1])
                    
                    cat("Minutos disponibles:\n")
                    print(df)
                    
                    valido <-FALSE
                    while(valido == FALSE){
                        m1 <- readline(prompt="Introduzca la posicion del mes deseado: ")
                        cat("\n")
                        if(suppressWarnings(is.na(as.numeric(m1))) == FALSE){
                            m1 <- as.numeric(m1)
                            if(m1 %in% seq(1,length(unique(minute(fecha))))){
                                valido <-TRUE 
                            }else{
                                message("Input invalido!")  
                            }
                        }else{
                            message("Input invalido!")
                        }
                    }
                    message("Registrado.")
                    min1 <- unique(minute(fecha))[m1]
                    
                    #Actualizamos
                    fecha <- fecha[minute(fecha) == min1]
               
                    #Segundo Inicial////////////////////
                    message("Seleccione el segundo de la hora de inicio.")
                    df<-data.frame(I______Segundo______I =unique(second(fecha)))
                    df[,1] <- as.character(df[,1])
                    
                    cat("Segundos disponibles:\n")
                    print(df)
                    
                    valido <-FALSE
                    while(valido == FALSE){
                        d1 <- readline(prompt="Introduzca la posicion del dia deseado: ")
                        cat("\n")
                        if(suppressWarnings(is.na(as.numeric(d1))) == FALSE){
                            d1 <- as.numeric(d1)
                            if(d1 %in% seq(1,length(unique(second(fecha))))){
                                valido <-TRUE 
                            }else{
                                message("Input invalido!")  
                            }
                        }else{
                            message("Input invalido!")
                        }
                    }
                    message("Registrado.")
                    sec1 <- unique(second(fecha))[d1]
                }
                
                #Reiniciamos la variable fecha.
                fecha <- tablaDate
                #Obtenemos fecha inicial.
                if(horas == TRUE){
                    fechaIni <-fecha[ year(fecha) == year1  & 
                                     month(fecha) == month1 & 
                                       day(fecha) == day1   &
                                      hour(fecha) == hora1  & 
                                    minute(fecha) == min1   &
                                    second(fecha) == sec1][1]
                }else{
                    fechaIni <-fecha[ year(fecha) == year1  & 
                                     month(fecha) == month1 & 
                                       day(fecha) == day1][1]
                }

                #Restringimos la eleccion del usuario.
                fecha <- fecha[fecha >= fechaIni] 
                
                #Anio Final////////////////////
                message("Seleccione el anio de la fecha final.")
                df <- data.frame(I______Anio______I = unique(year(fecha)))
                df[,1] <- as.character(df[,1])
                
                cat("Fechas disponibles:\n")
                print(df)
                
                valido <-FALSE
                while(valido == FALSE){
                    y1 <- readline(prompt="Introduzca la posicion del anio deseado: ")
                    cat("\n")
                    if(suppressWarnings(is.na(as.numeric(y1))) == FALSE){
                        y1 <- as.numeric(y1)
                        if(y1 %in% seq(1,length(unique(year(fecha))))){
                            valido <-TRUE 
                        }else{
                            message("Input invalido!")  
                        }
                    }else{
                        message("Input invalido!")
                    }
                }
                message("Registrado.")
                year2 <- unique(year(fecha))[y1]
                
                #Actualizamos
                fecha <- fecha[year(fecha) == year2]
                
                #Mes final////////////////////
                message("Seleccione el mes de de la fecha final.")
                df <- data.frame(I______Mes______I = unique(month(fecha)))
                df[,1] <- as.character(df[,1])
                
                cat("Meses disponibles:\n")
                print(df)
                
                valido <-FALSE
                while(valido == FALSE){
                    m1 <- readline(prompt="Introduzca la posicion del mes deseado: ")
                    cat("\n")
                    if(suppressWarnings(is.na(as.numeric(m1))) == FALSE){
                        m1 <- as.numeric(m1)
                        if(m1 %in% seq(1,length(unique(month(fecha))))){
                            valido <-TRUE 
                        }else{
                            message("Input invalido!")  
                        }
                    }else{
                        message("Input invalido!")
                    }
                }
                message("Registrado.")
                month2 <- unique(month(fecha))[m1]
                
                #Actualizamos
                fecha <- fecha[month(fecha) == month2]
                
                #Dia Final////////////////////
                message("Seleccione el dia de de la fecha final.")
                df <- data.frame(I______Mes______I = unique(day(fecha)))
                df[,1] <- as.character(df[,1])
                
                cat("Dias disponibles:\n")
                print(df)
                
                valido <-FALSE
                while(valido == FALSE){
                    d1 <- readline(prompt="Introduzca la posicion del dia deseado: ")
                    cat("\n")
                    if(suppressWarnings(is.na(as.numeric(d1))) == FALSE){
                        d1 <- as.numeric(d1)
                        if(d1 %in% seq(1,length(unique(day(fecha))))){
                            valido <-TRUE 
                        }else{
                            message("Input invalido!")  
                        }
                    }else{
                        message("Input invalido!")
                    }
                }
                message("Registrado.")
                day2 <- unique(day(fecha))[d1]
                
                if(horas == TRUE){
                    #Actualizamos
                    fecha <- fecha[day(fecha) == day2]
                    
                    #Hora Final////////////////////
                    message("Seleccione la hora final.")
                    df <- data.frame(I______Hora______I = unique(hour(fecha)))
                    df[,1] <- as.character(df[,1])
                    
                    cat("Horas disponibles:\n")
                    print(df)
                    
                    valido <-FALSE
                    while(valido == FALSE){
                        y2 <- readline(prompt="Introduzca la posicion de la hora deseada: ")
                        cat("\n")
                        if(suppressWarnings(is.na(as.numeric(y2))) == FALSE){
                            y2 <- as.numeric(y2)
                            if(y2 %in% seq(1,length(unique(hour(fecha))))){
                                valido <-TRUE 
                            }else{
                                message("Input invalido!")  
                            }
                        }else{
                            message("Input invalido!")
                        }
                    }
                    message("Registrado.")
                    hora2 <- unique(hour(fecha))[y2]
                    
                    #Actualizamos
                    fecha <- fecha[hour(fecha) == hora2]
                    
                    #Minuto Final////////////////////
                    message("Seleccione el minuto de la hora de inicio.")
                    df<-data.frame(I______Minuto______I = unique(minute(fecha)))
                    df[,1] <- as.character(df[,1])
                    
                    cat("Minutos disponibles:\n")
                    print(df)
                    
                    valido <-FALSE
                    while(valido == FALSE){
                        m1 <- readline(prompt="Introduzca la posicion del mes deseado: ")
                        cat("\n")
                        if(suppressWarnings(is.na(as.numeric(m1))) == FALSE){
                            m1 <- as.numeric(m1)
                            if(m1 %in% seq(1,length(unique(minute(fecha))))){
                                valido <-TRUE 
                            }else{
                                message("Input invalido!")  
                            }
                        }else{
                            message("Input invalido!")
                        }
                    }
                    message("Registrado.")
                    min2 <- unique(minute(fecha))[m1]
                    
                    #Actualizamos
                    fecha <- fecha[minute(fecha) == min2]
                    
                    #Segundo Final////////////////////
                    message("Seleccione el segundo de la hora de inicio.")
                    df<-data.frame(I______Segundo______I =unique(second(fecha)))
                    df[,1] <- as.character(df[,1])
                    
                    cat("Segundos disponibles:\n")
                    print(df)
                    
                    valido <-FALSE
                    while(valido == FALSE){
                        d1 <- readline(prompt="Introduzca la posicion del dia deseado: ")
                        cat("\n")
                        if(suppressWarnings(is.na(as.numeric(d1))) == FALSE){
                            d1 <- as.numeric(d1)
                            if(d1 %in% seq(1,length(unique(second(fecha))))){
                                valido <-TRUE 
                            }else{
                                message("Input invalido!")  
                            }
                        }else{
                            message("Input invalido!")
                        }
                    }
                    message("Registrado.")
                    sec2 <- unique(second(fecha))[d1]
                }
                
                #Reiniciamos la variable fecha.
                fecha <- tablaDate
                #Obtenemos fecha inicial.
                if(horas == TRUE){
                    fechaFin <-fecha[ year(fecha) == year2  & 
                                     month(fecha) == month2 & 
                                       day(fecha) == day2   &
                                      hour(fecha) == hora2  & 
                                    minute(fecha) == min2   &
                                    second(fecha) == sec2][1]
                }else{
                    fechaFin <-fecha[ year(fecha) == year2  & 
                                     month(fecha) == month2 & 
                                       day(fecha) == day2][1]
                }
                
                message("Seleccion finalizada. Se procesara el siguiente rango:")
                df <- data.frame(I______Rango______I = c(fechaIni,fechaFin))
                row.names(df) <- c("Fecha inicial","Fecha final")
                print(df)
                cat("\n")
            }
            #Extraemos el rango solicitado.
            tablaA <- cbind(tablaDate,tablaA)
            tablaA <- tablaA[tablaA$date >= fechaIni & tablaA$date <= fechaFin,]
            
            #Generamos clave para resumen estadistico.
            if("depth.m" %in% names(tablaA) == TRUE){
                tablaA$clave <- paste(tablaA$date,tablaA$latitude,
                                      tablaA$longitude,tablaA$depth.m,sep="_")
                depthBool <-TRUE
                #Eliminamos profundidad.
                tablaA$depth.m <- NULL
            }else{
                tablaA$clave <- paste(tablaA$date,tablaA$latitude,
                                      tablaA$longitude,sep="_")
                depthBool <-FALSE
            }
            #Eliminamos variables.
            tablaA$date <- NULL
            tablaA$latitude <- NULL
            tablaA$longitude <- NULL
            
            #Creamos el data frame de salida.
            tablaAux <- data.frame(clave = unique(tablaA$clave)) 
            if(depthBool == TRUE){
                tablaAux <- separate(tablaAux,col = clave, into = c("date",
                                      "latitude","longitude","depth.m"),sep="_")
            }else{
                tablaAux <- separate(tablaAux,col = clave, into = c("date",
                                      "latitude","longitude"),sep="_")
            }
            #Hacemos resumen estadistico de las variables
            for(i in 1 :length(vars)){
                #El resumen de las categoricas sera la moda.
                if(class(tablaA[vars[i]][,1]) == "numeric" | 
                   class(tablaA[vars[i]][,1]) == "integer"){
                    meanName <- paste0(vars[i],".mean")
                    medianName <- paste0(vars[i],".median")
                    sdName <- paste0(vars[i],".sd")
                    #Data frame auxiliar.
                    dfAux <- data.frame(cl =tablaA$clave,va=tablaA[vars[i]][,1])
                    dfAux <- dfAux %>% group_by(cl) %>%
                        summarize(m = mean(va), me = median(va), s = sd(va))
                    dfAux <- dfAux[,-1] 
                    names(dfAux)[which(names(dfAux) == "m")] <- meanName
                    names(dfAux)[which(names(dfAux) =="me")] <- medianName
                    names(dfAux)[which(names(dfAux) == "s")] <- sdName
                }else{
                    modaName <- paste0(vars[i],".mode")
                    #Data frame auxiliar.
                    dfAux <- data.frame(cl =tablaA$clave,va=tablaA[vars[i]][,1])
                    dfAux <- dfAux %>% group_by(cl) %>%
                        summarize(m = moda(va))
                    dfAux <- dfAux[,-1]
                    names(dfAux)[1] <- modaName
                }
                dfAux <- as.data.frame(dfAux)
                #Check
                if(ncol(dfAux) != 3 & ncol(dfAux) != 1){
                    stop("Numero de columnas incorrecto en dfAux.")
                }
                #Check
                if(nrow(dfAux) != nrow(tablaAux)){
                    stop("El numero de filas de dfAux y de tablaAux no coinciden.")
                }
                tablaAux <- cbind(tablaAux,dfAux)
            }
            #Acualizamos tabla
            tablaA <- tablaAux
            tablaA$date <- NULL
            
            #Limpiamos memoria
            rm(tablaAux);rm(dfAux)
            gc()
            
            #Check
            if(any(names(tablaA) %in% c("date","clave"))){
                stop("Hay variable incorrectas en el dataset tablaA.")
            }
            
        }
        
        #Reducimos dimension de la tablaA /////////////////////////////
        #Indicador para reduccion
        cat("Dimension de la tabla antes del procesamiento ",dim(tablaA),"\n")
        tablaA$Ind <- rep(NA,nrow(tablaA))
        dmax <- max(radios)     #Radio maximo ingresado
        dmax <- dmax/106        #De kilometros a grados con ampliacion de medida
        
        #Tabla auxiliar
        tablaAaux <- tablaA[c("Ind","latitude","longitude")]
        tablaAaux$fila <- 1:nrow(tablaA)
        
        #Buscamos puntos de geoloc cercanos a los de la tablaB en tablaAaux
        #y los registramos en tablaA$Ind. Procedimiento.
        invisible(apply(tablaB[c("latitude","longitude")],1,function(vect){
            latB <- vect[1]
            lonB <- vect[2]
            tablaAaux$Ind[which(
                abs(tablaAaux$longitude - lonB) <= dmax &
                abs(tablaAaux$latitude  - latB) <= dmax
            )] <- "a"
            #Registramos
            pos <- tablaAaux$fila[!is.na(tablaAaux$Ind)]
            tablaA$Ind[pos] <<- "a"
            #Liberamos
            rm(pos)
            #Recortamos
            tablaAaux <<- tablaAaux[is.na(tablaAaux$Ind),]
            return(NULL)
        }))
        tablaA <- tablaA[!is.na(tablaA$Ind),]
        tablaA$Ind <- NULL
        
        #Limpiamos memoria
        rm(tablaAaux)
        gc()
        cat("Dimension de la tabla despues del procesamiento ",dim(tablaA),"\n")
        
        #Imputamos//////////////////////////////////////////////////////
        if(inp3N == "GoogleEarthRiosLugares.csv"){
            tablaB<-impute.per.region2(tablaB,tablaA,radio = radios,
                                        medida = medida,Google = TRUE)
        }else{
            tablaB<-impute.per.region2(tablaB,tablaA,radio=radios,medida=medida)
        }
        
        #Buscamos nombre disponible
        setwd(paste0(d,"/Biologicas"))
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
        setwd(paste0(d,"/Datos Finales"))
        write.csv(tablaB,file=nombre,row.names = FALSE) 
        message("La tabla '",inp2N,"' fue procesada exitosamente.")
        message("Se creo el archivo '",nombre,"' en 'Datos Finales'.")
    }
    
    #Formateo de salida de Google Earth
    if(inp3N == "GoogleEarthRiosLugares.csv" & "valor" %in% names(tablaB)){
       
        cat("\n")
        cat("Indique si desea generar la tabla de distancia minima a objetos ")
        cat("costeros.\n")
        
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
        if(humanT == TRUE){
            cat("Creando tabla...\n")
            cat("\n")
            
            MatBio <- tablaB

            #//////////////////
            
            #Renombramos
            if("objeto.t" %in% names(MatBio)){
                MatBio$objeto.t <- paste0(MatBio$objeto.t,".t")
                MatBio$objeto.d <- paste0(MatBio$objeto.d,".d")
                
                #Eliminamos los lat=lon de los objetos
                MatBio$lon.objeto.d <- NULL
                MatBio$lat.objeto.d <- NULL
                MatBio$lon.objeto.t <- NULL
                MatBio$lat.objeto.t <- NULL
                
                #Unimos las variables de tiempo y dystancia en una sola.  
                pos1 <- which(names(MatBio) %in% 
                                  c("objeto.d","Distancia.Euclidea.kms"))
                pos2 <- which(names(MatBio) %in% 
                                  c("objeto.t","Tiempo.en.Corriente.dias"))
                
                matAux1 <- MatBio[,-pos1]
                matAux2 <- MatBio[,-pos2]
                
                #Renombramos
                pos1 <- which(names(matAux1) %in% 
                                  c("objeto.t","Tiempo.en.Corriente.dias"))
                pos2 <- which(names(matAux2) %in% 
                                  c("objeto.d","Distancia.Euclidea.kms"))
                
                names(matAux1)[pos1] <- c("objeto","medida")
                names(matAux2)[pos2] <- c("objeto","medida")
                
                #Unimos por rbind.
                MatBio <- rbind(matAux1,matAux2) 
                
            }else{
                MatBio$objeto <- paste0(MatBio$objeto,".d")
                
                #Eliminamos los lat=lon de los objetos
                MatBio$lon.objeto<- NULL
                MatBio$lat.objeto <- NULL
                MatBio$tipo <- NULL
                
                pos1 <- which(names(MatBio) %in% c("valor"))
                
                names(MatBio)[pos1] <- c("medida")
            }
            
            #Redondeamos
            MatBio$medida <- round(MatBio$medida,3)
            
            #Reemplazamos los NA's de medida por 999
            MatBio$medida[is.na(MatBio$medida)] <-
                rep(999,sum(is.na(MatBio$medida)))
            
            #nos quedamos solo con las unicaciones de menor distancia
            MatBio$fila <- 1:nrow(MatBio)
            posiciones <- 1:(ncol(MatBio)-3)
            
            MatBio$clave <-
                apply(MatBio[-(ncol(MatBio):(ncol(MatBio)-2))],1,function(vect){
                    vect <- paste(vect,collapse = "")
                    vect
                })
            
            MatBio$clave <- as.factor(MatBio$clave)
            
            proc1 <- tapply(MatBio$fila,MatBio$clave,function(filas){
                dfAux <- MatBio[filas,]
                proc2 <- tapply(dfAux$fila,dfAux$objeto,function(filas){
                    if(length(filas) != 1){
                        filas <- filas[-which.min(MatBio$medida[filas])]
                        MatBio$medida[filas] <<- 
                            rep("Elim",length(MatBio$medida[filas]))
                    }
                    return(NULL)
                })
                return(NULL)
            })
            #liberamos memoria
            rm(proc1)
            gc()
            
            MatBio$fila <- NULL
            MatBio$clave <- NULL
            
            #Eliminamos los que seleccionamos previamente
            MatBio <- MatBio[MatBio$medida != "Elim",]
            
            #Procedimiento spread para datos no unicos de objeto.
            #Construccion del indice.
            MatBio$index <- rep(NA,times = nrow(MatBio))	
            tab <- table(MatBio$objeto)
            
            proc_app <- sapply(names(tab),function(obj){
                MatBio$index[which(MatBio$objeto == obj)] <<- 1:tab[obj]
            })
            rm(proc_app)
            gc()
            
            MatBio <- spread(MatBio, key = objeto, value = medida)
            
            #Eliminamos variables inecesarias
            MatBio$index <- NULL
            MatBio$NA.d <- NULL
            MatBio$NA.t <- NULL
            
            #Juntamos similares
            MatBio$Grupos <-apply(MatBio[posiciones],1,function(vect){
                vect <- paste(vect,collapse = "")
                vect
            })
            MatBio$Grupos <- as.factor(MatBio$Grupos)
            
            #Calculamos promedios
            for(i in (posiciones[length(posiciones)]+1):(ncol(MatBio) - 1)){
                t <- tapply(MatBio[i][,1],MatBio$Grupos,median,na.rm = T)	
                MatBio[i][,1] <-  sapply(MatBio$Grupos,function(x){
                    unname(t[as.character(x)])
                })
            }
            
            #Hacemos unique	
            MatBio$Grupos <- NULL
            
            MatBio <- MatBio %>% unique()
            
            #Imputamos NA's colocando el valor 999 para indicar que el objeto no
            #influye sobre el punto de interes.
            for(i in 1 : ncol(MatBio)){
                posNas <- which(is.na(MatBio[,i]))
                MatBio[,i][posNas] <- rep(999,times = length(posNas))
            }
            
            #Exportamos tabla.
            write.csv(MatBio,file = paste0("TablaMin_",nombre),
                      row.names=F)
            message("Tabla de distancia minima creada exitosamente en Datos Finales")
        }
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

#Limpiamos memoria.
rm(list=ls())
gc()



