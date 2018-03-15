#Programa que a solicitud del usuario busca variables dentro de las tablas de
#Pre-processed Data 2 para unirlas por geolocalizacion a una matriz biologica
#presente en la carpeta Processed Data. Esta ultima tambien es indicada por el 
#usuario.
#El usuario indica los radios de imputacion y se realiza el procedimiento
#impute.per.region2. La tabla unificada se retorna en Processed Data.

setwd("~/Thesis Project AB/R-Scripts")
source('Functions.R')

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
        #Solicitamos radio de imputacion en kilometros.
        cat("\n")
        cat("Indique los radios en kilometros que se usaran para unir los datos\n")
        cat("por geolocalizacion en orden de importancia (de mayor a menor).\n")
        cat("Maximo tres.\n")
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
            df <- data.frame(radio = radios)
            print(df)
        }else{
            message("Seleccion finalizada. Se usaran los radios:")
            df <- data.frame(radio = radios)
            print(df)
            message("En orden de importancia.") 
        }
        
        #Extraemos las variables a agregar.
        tablaA <- tablaA[c("latitude","longitude",vars)]
        
        #Imputamos
        tablaB <- impute.per.region2(tablaB,tablaA,radio=radios)
        
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
