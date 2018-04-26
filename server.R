#INPUTS
# checkboxInput("Box_Numerica")  #Boolean
# checkboxInput("Box_Influencia")#Boolean
# sliderInput("Slider_Value")    #Numeric
# selectInput("Select_Zona")     #Character
# selectInput("Select_Yvar")     #Character
# selectInput("Select_Xvar")     #Character
# selectInput("Select_Zvar")     #Character
# selectInput("Select_Cvar")     #Character
#
#OUTPUTS
# RUi_Variables
# RPlot_Map
# RText_TablaFmap
# RPlot_Scatt
# RText_RegresionL
# RText_RegresionP
# RPlot_Box
# RText_summaryB
# RPlot_Hist
# RText_TablaF
# RText_summaryH
# RText_TestN
# RText_Distribucion

#DEBUGGING TOOLS
#shiny::runApp(display.mode="showcase")
#options(shiny.reactlog=TRUE) 
#options(shiny.fullstacktrace = TRUE)
#rsconnect::deployApp()
#rsconnect::showLogs(streaming = TRUE)
#cat(file=stderr(), ..., "\n")
#options(shiny.trace=TRUE)

library(shiny)

#Archive server.R.
shinyServer(function(input,output){
    
    #EVALUADOR SHOWME///////////////////////////////////////////////////////////
    #output$ShowMe <-renderText({          #Print section
    #    input$Select_Cvar
    #})
    
    #PANEL RENDERIZADO//////////////////////////////////////////////////////////
    
    #---------------------------------------------------------------------------
    #INPUT: VARIABLE A EXPLICAR (Zvar). 
    # => (input$Select_Zvar)
    # <= RUi_Select_Zvar
    output$RUi_Select_Zvar <- renderUI({
        #print("RUi_Select_Zvar")
        if(input$Box_Numerica == FALSE){
            variables <-VarDepFactor
            variables <- variables[variables != "Localidad"]
            selected <- variables[1]
        }else{
            variables <- varDepNum
            selected <- variables[4]
        }
        selectInput("Select_Zvar", "Seleccione la variable a explicar:",
            choices  = variables,
            selected = selected
        )
    })

    #---------------------------------------------------------------------------
    #INPUT: VALOR DE LA VARIABLE A EXPLICAR (Zval).
    # => (input$Select_Zval)
    #output$RUi_Select_Zval
    output$RUi_Select_Zval <- renderUI({
        #print("RUi_Select_Zval")
        if(input$Box_Numerica == FALSE){
            if(is.null(input$Select_Zvar)){
                alt <- 0
            }else{
                alt <- as.character(
                    unique(MatrizBiologica[input$Select_Zvar][,1]))  
            }
            selectInput("Select_Zval", "Elija el valor de la variable que desea
                                          analizar:",
                choices  = c("Todas",alt),
                selected = "Todas"
            )
        }
    })
    
    #---------------------------------------------------------------------------
    #INPUT: CORRIENTES VARIABLE EXPLORATORIA 1 (CorrY).
    # => (input$Select_CorrY)
    #output$RUi_Select_CorrY
    output$RUi_Select_CorrY <- renderUI({
        #print("RUi_Select_CorrY")
        if(input$Select_Yvar == "Tiempo.en.Corriente.dias"){
            alt <- as.character(unique(MatrizBiologica["objeto.t"][,1]))  
            alt <- alt[!is.na(alt)]
            selectInput("Select_CorrY", "Seleccione el objeto de estudio:",
                        choices  = c("Todos",alt),
                        selected = "Todos"
            )
        }
    })
    
    #---------------------------------------------------------------------------
    #INPUT: DISTANCIA VARIABLE EXPLORATORIA 1 (DistY).
    # => (input$Select_DistY)
    #output$RUi_Select_DistY
    output$RUi_Select_DistY <- renderUI({
        #print("RUi_Select_DistY")
        if(input$Select_Yvar == "Distancia.Euclidea.kms"){
            alt <- as.character(unique(MatrizBiologica["objeto.d"][,1])) 
            alt <- alt[!is.na(alt)]
            selectInput("Select_DistY", "Seleccione el objeto de estudio:",
                        choices  = c("Todos",alt),
                        selected = "Todos"
            )
        }
    })

    #---------------------------------------------------------------------------
    #INPUT: CORRIENTES VARIABLE EXPLORATORIA 2 (CorrX).
    # => (input$Select_CorrX)
    #output$RUi_Select_CorrX
    output$RUi_Select_CorrX <- renderUI({
        #print("RUi_Select_CorrX")
        if(input$Select_Xvar == "Tiempo.en.Corriente.dias"){
            alt <- as.character(unique(MatrizBiologica["objeto.t"][,1])) 
            alt <- alt[!is.na(alt)]
            selectInput("Select_CorrX", "Seleccione el objeto de estudio:",
                        choices  = c("Todos",alt),
                        selected = "Todos"
            )
        }
    })
    
    #---------------------------------------------------------------------------
    #INPUT: DISTANCIA VARIABLE EXPLORATORIA 2 (DistX).
    # => (input$Select_DistX)
    #output$RUi_Select_DistX
    output$RUi_Select_DistX <- renderUI({
        #print("RUi_Select_DistX")
        if(input$Select_Xvar == "Distancia.Euclidea.kms"){
            alt <- as.character(unique(MatrizBiologica["objeto.d"][,1]))  
            alt <- alt[!is.na(alt)]
            selectInput("Select_DistX", "Seleccione el objeto de estudio:",
                        choices  = c("Todos",alt),
                        selected = "Todos"
            )
        }
    })
    
    #TABLAS REACTIVAS //////////////////////////////////////////////////////////
    #Genera la tabla de datos adecuada en funcion de los inputs de usuario.
    
    dfReact <- reactive({
        zona <- input$Select_Zona
        Xvar <- input$Select_Xvar
        Yvar <- input$Select_Yvar
        Zvar <- input$Select_Zvar 

        #Subsetting de variables.
        df <- MatrizBiologica[c("Localidad",Zvar,Yvar,Xvar,"Abundancia")]
        
        #Si no tiene datos salimos.
        if(nrow(df) == 0){
            return(df)
        }
        
        #Incluimos columnas con informacion de Google Earth en caso de aplicar.
        tyv <- FALSE
        if(Yvar == "Tiempo.en.Corriente.dias"){
            tyv <- TRUE
            df <- cbind(df,MatrizBiologica["objeto.t"])
            names(df)[ncol(df)] <- "objeto.t.Yvar"
        }
        dyv <- FALSE
        if(Yvar == "Distancia.Euclidea.kms"){
            dyv <- TRUE
            df <- cbind(df,MatrizBiologica["objeto.d"])
            names(df)[ncol(df)] <- "objeto.d.Yvar"
        }
        txv <- FALSE
        if(Xvar == "Tiempo.en.Corriente.dias"){
            txv <- TRUE
            df <- cbind(df,MatrizBiologica["objeto.t"])
            names(df)[ncol(df)] <- "objeto.t.Xvar"
        }
        dxv <- FALSE
        if(Xvar == "Distancia.Euclidea.kms"){
            dxv <- TRUE
            df <- cbind(df,MatrizBiologica["objeto.d"])
            names(df)[ncol(df)] <- "objeto.d.Xvar"
        }
        
        #Eliminamos NA's en caso de aplicar.
        if(tyv == TRUE){
            df <- df[!is.na(df$objeto.t.Yvar),]  #Removemos NA's
        }
        if(dyv == TRUE){
            df <- df[!is.na(df$objeto.d.Yvar),]  #Removemos NA's
        }
        if(txv == TRUE){
            df <- df[!is.na(df$objeto.t.Xvar),]  #Removemos NA's
        }
        if(dxv == TRUE){
            df <- df[!is.na(df$objeto.d.Xvar),]  #Removemos NA's
        }
        
        #Subsetting por zona.
        if(zona != "Todas"){
            df <- df[df$Localidad == zona,]
        }
        
        #Eliminamos repetidos
        df <- df %>% unique()
        
        #Subsettings de Zval.
        if(!is.null(input$Select_Zval)){
            if(input$Select_Zval != "Todas"){
                df <- df[df[,2] == input$Select_Zval,]
            }
        }
        
        #Si no tiene datos salimos.
        if(nrow(df) == 0){
            return(df)
        }
        
        #Subsetting para tablas de Google Earth.
        #Primera variable exploratoria.
        if(Yvar == "Tiempo.en.Corriente.dias"){
            obj <- input$Select_CorrY
            if(is.null(input$Select_CorrY)){
                obj <- "Todos"
            }
            if(obj != "Todos"){
                df <- df[df$objeto.t.Yvar == obj,]
            }
        }else if(Yvar == "Distancia.Euclidea.kms"){
            obj <- input$Select_DistY
            if(is.null(input$Select_DistY)){
                obj <- "Todos"
            }
            if(obj != "Todos"){
                df <- df[df$objeto.d.Yvar == obj,]
            }
        }
        #Segunda variable exploratoria.
        if(Xvar == "Tiempo.en.Corriente.dias"){
            obj <- input$Select_CorrX
            if(is.null(input$Select_CorrX)){
                obj <- "Todos"
            }
            if(obj != "Todos"){
                df <- df[df$objeto.t.Xvar == obj,]
            }
        }else if(Xvar == "Distancia.Euclidea.kms"){
            obj <- input$Select_DistX
            if(is.null(input$Select_DistX)){
                obj <- "Todos"
            }
            if(obj != "Todos"){
                df <- df[df$objeto.d.Xvar == obj,]
            }
        }

        #Corregimos nombres. 
        if(nrow(df) != 0){
            row.names(df) <- seq(1,nrow(df))
        }
        
        #Salida
        return(df)
    })
    
    #TAB PANEL: TABLA DE DATOS /////////////////////////////////////////////////
    
    output$RDT_TablaDatos <- DT::renderDataTable(
        DT::datatable({
            dfReact() 
        })
    )
    
    #Resumen de la variable Y.
    output$RText_DTY <- renderText({
        input$Select_Yvar
    })
    
    output$RTable_DTY <-renderTable({ 
        resumen <- summary(dfReact()[input$Select_Yvar][,1])
        nombres <- names(resumen)
        if("NA's" %in% nombres){
            d <- rep(NA,times=14)
            dim(d) <- c(2,7)
        }else{
            d <- rep(NA,times=12)
            dim(d) <- c(2,6)
        }

        d <- as.data.frame(d)
        names(d) <- nombres
        d[1,] <- as.vector(resumen)
        d <- d[1,]
        d
    })
    
    #Resumen de la variable X.
    output$RText_DTX <- renderText({
        input$Select_Xvar
    })
    
    output$RTable_DTX <-renderTable({         
        resumen <- summary(dfReact()[input$Select_Xvar][,1])
        nombres <- names(resumen)
        if("NA's" %in% nombres){
            d <- rep(NA,times=14)
            dim(d) <- c(2,7)
        }else{
            d <- rep(NA,times=12)
            dim(d) <- c(2,6)
        }
        d <- as.data.frame(d)
        names(d) <- nombres
        d[1,] <- as.vector(resumen)
        d <- d[1,]
        d
    })
    
    #TAB PANEL: MAPA ///////////////////////////////////////////////////////////
    
    output$RPlot_Map <-renderPlot({
        df <- MatrizBiologica[c("latitude","longitude","Localidad")]
        df <- df %>% unique() 
        df <- df[df$Localidad %in% unique(dfReact()["Localidad"][,1]),]
        
        #Dibujamos el mapa solo si el conjunto de variables no queda vacio.
        if(nrow(df) != 0){
            #Centro del mapa
            lat <- mean(df["latitude"][,1])
            lon <- mean(df["longitude"][,1])
            
            #Plot del mapa
            map <- get_map(location=c(lon=lon,lat=lat),zoom=6,maptype="terrain")
            plotMapa <-ggmap(map) +
                geom_point(data=df,aes(x=longitude,y=latitude),col="red",
                           show.legend = T,size = 2) +
                labs(title="Zonas de Estudio.") 
            plot(plotMapa)
        }
    })
    
    #Referencias
    output$Rtext_text1 <-renderText({						
        x <- c("D. Kahle and H. Wickham. ggmap: Spatial Visualization with 
               ggplot2. The R Journal, 5(1), 144-161.")
        x
    })
    
    output$Rtext_text2 <-renderText({						
        x <- c("URL http://journal.r-project.org/archive/2013-1/
               kahle-wickham.pdf")
        x
    })
    
    output$Rtext_text3 <-renderText({						
        x <- c("Fuente de Datos: National Oceanographic and Atmospheric 
               Administration (NOAA).")
        x
    })
    
    #TAB PANEL: BOXPLOT ////////////////////////////////////////////////////////
    
    output$RPlot_Box <- renderPlot({
        #print("output$RPlot_Box")
        df <- dfReact()
        Xvar <- input$Select_Xvar
        Yvar <- input$Select_Yvar
        Zvar <- input$Select_Zvar

        #recibimos inputs de los UI's renderizados.
        List_Zval <- input$SelectList_Zval
        List_Zona <- input$SelectList_Zona
        
        #Solo ploteamos cuando esten disponibles los datos.
        if(!is.null(List_Zona) & !is.null(List_Zval)){
            #Variable categorica.
            if(input$Box_Numerica == TRUE){
                #Corregimos Zvar
                if(is.null(Zvar)){
                    Zvar <- "nitrate.Climatological.mean."
                }
                #Factorizamos variable continua.
                valores <- suppressWarnings(
                    as.numeric(as.character(df[Zvar][,1])))
                #Esperamos actualizacion.
                if(all(is.na(valores))){
                    return(NULL)
                }
                #Vemos si todos los valores son iguales o no.
                if(min(valores) == max(valores)){
                    cutpoints <- quantile(c(min(valores) - 0.001,valores),
                                          seq(0,1,length=2),na.rm=TRUE)	
                }else{
                    cutpoints <- quantile(c(min(valores) - 0.001,valores),
                                          seq(0,1,length=5),na.rm=TRUE)	 
                }
                df$ZvarFac <- cut(valores,cutpoints)
                #Subsetting del data frame df.
                df <- df[df$Localidad %in% List_Zona,]
                #Configuramos plot.
                BoxPlot <- ggplot(df,aes(ZvarFac,get(Yvar),fill =ZvarFac)) + 			
                    geom_boxplot()	+ facet_grid(.~Localidad) + xlab(Zvar) +
                    ylab(Yvar) + 
                    ggtitle("Boxplots. Primera variable de exploracion.")									
            }else{
                #Corregimos Zvar
                if(is.null(Zvar)){
                    Zvar <- "Especie"
                }
                #Subsetting del data frame df.
                df <- df[df$Localidad %in% List_Zona,]
                df <- df[df[Zvar][,1] %in% List_Zval,]
                #Configuramos plot.
                BoxPlot <- ggplot(df,aes(get(Zvar),get(Yvar),fill =get(Zvar))) + 			
                    geom_boxplot()	+ facet_grid(.~Localidad) + xlab(Zvar) +
                    ylab(Yvar) + 
                    ggtitle("Boxplots. Primera variable de exploracion.")	
            }
            #Ploteamos solo si hay datos.
            if(nrow(df) != 0){
                plot(BoxPlot) 
            }
        }

    })
    
    #Variables disponibles 
    #INPUT: Selector de valores.
    # => (input$Select_Zvar)
    #output$RUi_checkboxGroup.Zvar
    output$RUi_checkboxGroup.Zvar <- renderUI({
        #Se habilida solo cuando la variable ingresada es factor.
        if(input$Box_Numerica == FALSE){
            df <- dfReact()
            Zvar <- input$Select_Zvar
            #Removemos null.
            if(is.null(Zvar)){
                Zvar <- "Especie"
            }
            valores <- as.character(unique(df[Zvar][,1]))
            #Interfaz.
            checkboxGroupInput("SelectList_Zval",
                               "Selector de valores (Variable de Estudio).",
                               choices  = valores
                               ,
                               selected = valores[1:2]
            ) 
        }
    })
    
    #INPUT: Selector de localidad.
    #output$RUi_checkboxGroup.Zona
    output$RUi_checkboxGroup.Zona <- renderUI({
        df <- dfReact()
        valores <- as.character(unique(df["Localidad"][,1]))
        #Interfaz.
        checkboxGroupInput("SelectList_Zona","Selector de localidad.",
                choices  = valores
                ,
                selected = valores[1:2]
        )
    })
    
    #TAB PANEL: HISTOGRAMAS ////////////////////////////////////////////////////
    
    output$RPlot_Hist <- renderPlot({
        #print("output$RPlot_Box")
        df <- dfReact()
        Xvar <- input$Select_Xvar
        Yvar <- input$Select_Yvar
        Zvar <- input$Select_Zvar
        
        #recibimos inputs de los UI's renderizados.
        List_Zval <- input$SelectList_Zval2
        List_Zona <- input$SelectList_Zona2
        #Solo ploteamos cuando esten disponibles los datos.
        if(!is.null(List_Zona) & !is.null(List_Zval)){
            #Variable categorica.
            if(input$Box_Numerica == TRUE){
                #Corregimos Zvar
                if(is.null(Zvar)){
                    Zvar <- "nitrate.Climatological.mean."
                }
                #Factorizamos variable continua.
                valores <- suppressWarnings(
                    as.numeric(as.character(df[Zvar][,1])))
                #Esperamos actualizacion.
                if(all(is.na(valores))){
                    return(NULL)
                }
                #Vemos si todos los valores son iguales o no.
                if(min(valores) == max(valores)){
                    cutpoints <- quantile(c(min(valores) - 0.001,valores),
                                          seq(0,1,length=2),na.rm=TRUE)	
                }else{
                    cutpoints <- quantile(c(min(valores) - 0.001,valores),
                                          seq(0,1,length=5),na.rm=TRUE)	 
                }
                df$ZvarFac <- cut(valores,cutpoints)
                #Subsetting del data frame df.
                df <- df[df$Localidad %in% List_Zona,]
                #Configuramos plot.
                HistP <- ggplot(df,aes(get(Yvar),fill =ZvarFac)) + 			
                    geom_histogram() + facet_grid(ZvarFac ~ Localidad) + 
                    xlab(Yvar) +
                    ylab("Frecuencia") + 
                    scale_fill_discrete(name = Zvar) +
                    ggtitle("Histogramas. Primera variable de exploracion.")									
            }else{
                #Corregimos Zvar
                if(is.null(Zvar)){
                    Zvar <- "Especie"
                }
                #Subsetting del data frame df.
                df <- df[df$Localidad %in% List_Zona,]
                df <- df[df[Zvar][,1] %in% List_Zval,]
                #Configuramos plot.
                HistP <- ggplot(df,aes(get(Yvar),fill =get(Zvar))) + 			
                    geom_histogram() + 
                    facet_grid(get(Zvar) ~ Localidad) + 
                    xlab(Yvar) +
                    ylab("Frecuencia") + 
                    scale_fill_discrete(name = Zvar) +
                    ggtitle("Histogramas. Primera variable de exploracion.") #+ 
                    #geom_vline(aes(xintercept = mean(get(Yvar)))) 
            }
            #Ploteamos solo si hay datos.
            if(nrow(df) != 0){
                plot(HistP) 
            }
        }
        
    })
    
    #Variables disponibles 
    #INPUT: Selector de valores.
    # => (input$Select_Zvar)
    #output$RUi_checkboxGroup.Zvar
    output$RUi_checkboxGroup2.Zvar <- renderUI({
        #Se habilida solo cuando la variable ingresada es factor.
        if(input$Box_Numerica == FALSE){
            df <- dfReact()
            Zvar <- input$Select_Zvar
            #Removemos null.
            if(is.null(Zvar)){
                Zvar <- "Especie"
            }
            valores <- as.character(unique(df[Zvar][,1]))
            #Interfaz.
            checkboxGroupInput("SelectList_Zval2",
                               "Selector de valores (Variable de Estudio).",
                               choices  = valores
                               ,
                               selected = valores[1:2]
            ) 
        }
    })
    
    #INPUT: Selector de localidad.
    #output$RUi_checkboxGroup.Zona
    output$RUi_checkboxGroup2.Zona <- renderUI({
        df <- dfReact()
        valores <- as.character(unique(df["Localidad"][,1]))
        #Interfaz.
        checkboxGroupInput("SelectList_Zona2","Selector de localidad.",
                           choices  = valores
                           ,
                           selected = valores[1:2]
        )
    })
    
    #TAB PANEL: SCATTERPLOT ////////////////////////////////////////////////////
    
    KReact <- reactive({
        #Inputs
        df <- dfReact()
        Xvar <- input$Select_Xvar
        Yvar <- input$Select_Yvar
        Zvar <- input$Select_Zvar
        
        #recibimos inputs de los UI's renderizados.
        List_Zval <- input$SelectList_Zval3
        List_Zona <- input$SelectList_Zona3
        List_Abun <- input$SelectList_Size3
        
        #Solo ploteamos cuando esten disponibles los datos.
        if(!is.null(List_Zona) & !is.null(List_Zval)){
            
            #Variable categorica.
            if(input$Box_Numerica == TRUE){
                
                #Corregimos Zvar
                if(is.null(Zvar)){
                    Zvar <- "nitrate.Climatological.mean."
                }
                #Eliminamos los valores faltantes de la variables estudiada.
                df <- df[!is.na(df[Zvar][,1]),]
                
                #Factorizamos variable continua.
                valores <- suppressWarnings(
                    as.numeric(as.character(df[Zvar][,1])))
                #Esperamos actualizacion.
                if(all(is.na(valores))){
                    return(NULL)
                }
                #Vemos si todos los valores son iguales o no.
                if(min(valores) == max(valores)){
                    cutpoints <- quantile(c(min(valores) - 0.001,valores),
                                          seq(0,1,length=2),na.rm=TRUE)	
                }else{
                    cutpoints <- quantile(c(min(valores) - 0.001,valores),
                                          seq(0,1,length=5),na.rm=TRUE)	 
                }
                #En caso de que los cortes sean iguales separamos manualmente.
                if(any(diff(cutpoints) == 0)){
                    cutpoints[2] <- cutpoints[2] + 0.0001
                    cutpoints[3] <- cutpoints[3] + 0.0002
                    cutpoints[4] <- cutpoints[4] + 0.0003
                    cutpoints[5] <- cutpoints[5] + 0.0004
                }
                df$ZvarFac <- cut(valores,cutpoints)
            
                #Subsetting del data frame df.
                df <- df[df$Localidad %in% List_Zona,]
                df <- df[df$Abundancia %in% List_Abun,]
                #Subsetting por intervalo.
                vect <- c()
                for (i in 1:length(List_Zval)){
                    cadena <- strsplit(List_Zval[i],split=",")[[1]]
                    mini <- as.numeric(sub("\\(","",cadena[1]))
                    maxi <- as.numeric(sub("]","",cadena[2]))
                    vect <- c(vect,df[Zvar][
                        df[Zvar][,1] > mini & df[Zvar][,1] <= maxi,1])
                }
                df <- df[df[Zvar][,1] %in% vect,]
                
                #Exportamos dataset.
                listaRet <- list(df)
                names(listaRet) <- c("df")
                return(listaRet)
                
            }else{
                #Corregimos Zvar
                if(is.null(Zvar)){
                    Zvar <- "Especie"
                }
                #Eliminamos los valores faltantes de la variables estudiada.
                df <- df[!is.na(df[Zvar][,1]),]
                
                #Subsetting del data frame df.
                df <- df[df$Localidad  %in% List_Zona,]
                df <- df[df[Zvar][,1]  %in% List_Zval,]
                df <- df[df$Abundancia %in% List_Abun,]
                
                #Exportamos dataset.
                listaRet <- list(df)
                names(listaRet) <- c("df")
                return(listaRet)
            }
        }else{
            return(NULL)
        }
    })
    
    #Variables disponibles 
    #INPUT: Selector de valores.
    # => (input$Select_Zvar)
    #output$RUi_checkboxGroup.Zvar
    output$RUi_checkboxGroup3.Zvar <- renderUI({
        #Input
        df <- dfReact()
        Zvar <- input$Select_Zvar

        #Se habilita cuando la variable ingresada es factor.
        if(input$Box_Numerica == FALSE){
            #Removemos null.
            if(is.null(Zvar)){
                Zvar <- "Especie"
            }
            #Eliminamos los valores faltantes de la variables estudiada.
            df <- df[!is.na(df[Zvar][,1]),]
            
            valores <- as.character(unique(df[Zvar][,1]))
        }else{
            #Removemos null.
            if(is.null(Zvar)){
                Zvar <- "nitrate.Climatological.mean."
            }
            #Eliminamos los valores faltantes de la variables estudiada.
            df <- df[!is.na(df[Zvar][,1]),]
            
            #Factorizamos variable continua.
            valores <- suppressWarnings(
                as.numeric(as.character(df[Zvar][,1])))
            #Esperamos actualizacion.
            if(all(is.na(valores))){
                return(NULL)
            }
            #Vemos si todos los valores son iguales o no.
            if(min(valores) == max(valores)){
                cutpoints <- quantile(c(min(valores) - 0.001,valores),
                                      seq(0,1,length=2),na.rm=TRUE)	
            }else{
                cutpoints <- quantile(c(min(valores) - 0.001,valores),
                                      seq(0,1,length=5),na.rm=TRUE)	 
            }
            #En caso de que los cortes sean iguales separamos manualmente.
            if(any(diff(cutpoints) == 0)){
                cutpoints[2] <- cutpoints[2] + 0.0001
                cutpoints[3] <- cutpoints[3] + 0.0002
                cutpoints[4] <- cutpoints[4] + 0.0003
                cutpoints[5] <- cutpoints[5] + 0.0004
            }
            valores <- unique(cut(valores,cutpoints))
        }
        #Render UI.
        checkboxGroupInput("SelectList_Zval3",
                           "Selector de valores.",
                           choices  = valores
                           ,
                           selected = valores[1:2]
        )
    
    })
    
    #INPUT: Selector de localidad.
    #output$RUi_checkboxGroup.Zona
    output$RUi_checkboxGroup3.Zona <- renderUI({
        df <- dfReact()
        valores <- as.character(unique(df["Localidad"][,1]))
        #Interfaz.
        checkboxGroupInput("SelectList_Zona3","Selector de localidad.",
                           choices  = valores
                           ,
                           selected = valores[1:2]
        )
    })
    
    #INPUT: Selector de localidad.
    #output$RUi_checkboxGroup.Zona
    output$RUi_checkboxGroup3.Size<- renderUI({
        df <- dfReact()
        valores <- as.character(unique(df["Abundancia"][,1]))
        #Interfaz.
        checkboxGroupInput("SelectList_Size3","Selector de abundancia.",
                           choices  = valores
                           ,
                           selected = valores[1]
        )
    })
    
    #INPUT: Selector de localidad.
    #output$RUi_checkboxGroup.Zona
    output$RUi_checkboxEtiqueta<- renderUI({
        checkboxInput("Box_Etiqueta","Mostrar Etiquetas.",value=T)
    })
    
    #INPUT: Selector del valor de K.
    #output$RUi_SliderK
    output$RUi_SliderK<- renderUI({
        Xvar <- input$Select_Xvar
        Yvar <- input$Select_Yvar
        #Check
        if(!is.null(KReact())){
            dfaux <- KReact()$df[c(Xvar,Yvar)] %>% unique()
            maxi <- nrow(dfaux)
        }else{
            maxi <- 2
        }
        sliderInput("Slider_K","Valor de K: ",min=1,max=maxi,value= 1,step = 1)
    })
    
    #SCATTERPLOT ////////////////////////////////////////////////////
    
    output$RPlot_Scat <- renderPlot({
        #Hacemos plot solo cuando se reciben los datos adecuados.
        if(!is.null(KReact()) == TRUE){
            #Inputs
            Xvar <- input$Select_Xvar
            Yvar <- input$Select_Yvar
            Zvar <- input$Select_Zvar
            Etiq <- input$Box_Etiqueta
            df <- KReact()$df

            #Corrector de variables renderizadas.
            if(is.null(Etiq)){
                Etiq <- TRUE
            }
            #Ploteamos solo cuando se haya actualizado el df.
            if(!is.null(df)){
                
                if(input$Box_Numerica == TRUE){
                    #Semilla del jitter.
                    set.seed("1376")
                    
                    #Puntos por abundancia.
                    if(length(unique(df$Abundancia)) == 1){
                        ScatterP <- ggplot(df,aes(get(Xvar),get(Yvar),
                                                                 col=ZvarFac)) +
                            geom_point(size = 4) +
                            geom_jitter(width= 0.005, height = 0.005,size = 4) +
                            #Etiquetas.
                            xlab(Xvar) +
                            ylab(Yvar) +
                            scale_color_discrete(name = Zvar) +
                            ggtitle("Diagrama de dispersion.") 
                    }else{
                        ScatterP <- ggplot(df,aes(get(Xvar),get(Yvar),
                                               col=ZvarFac,shape=Abundancia))  +
                            geom_point(size = 4) +
                            geom_jitter(width= 0.005, height = 0.005,size = 4) +
                            #Etiquetas.
                            xlab(Xvar) +
                            ylab(Yvar) +
                            scale_color_discrete(name = Zvar) +
                            ggtitle("Diagrama de dispersion.") 
                    }
                }else{
                    #Semilla del jitter.
                    set.seed("1376")
                    
                    #Puntos por abundancia.
                    if(length(unique(df$Abundancia)) == 1){
                        ScatterP <- ggplot(df,aes(get(Xvar),get(Yvar),
                                                               col=get(Zvar))) +
                            geom_point(size = 4) +
                            geom_jitter(width= 0.005, height = 0.005,size = 4) +
                            #Etiquetas.
                            xlab(Xvar) +
                            ylab(Yvar) +
                            scale_color_discrete(name = Zvar) +
                            ggtitle("Diagrama de dispersion.") 
                    }else{
                        ScatterP <- ggplot(df,aes(get(Xvar),get(Yvar),
                                              col=get(Zvar),shape=Abundancia)) +
                            geom_point(size = 4) +
                            geom_jitter(width= 0.005, height = 0.005,size = 4) +
                            #Etiquetas.
                            xlab(Xvar) +
                            ylab(Yvar) +
                            scale_color_discrete(name = Zvar) +
                            ggtitle("Diagrama de dispersion.") 
                    } 
                }
                
                #Etiqueta
                if(Etiq == TRUE){
                    ScatterP <- ScatterP + geom_text(aes(label=df$Localidad),
                                            vjust=1.6,color="black",size=4)
                }
                
                #Ploteamos solo si hay datos.
                if(nrow(df) != 0){
                    plot(ScatterP) 
                }
            }
        }
    })
    
    
    #K MEDIAS ///////////////////////////////////////////////////////
    
    #Funcion de K-Medias
    KmedReact <- reactive({
        #Hacemos plot solo cuando se reciben los datos adecuados.
        if(!is.null(KReact()) == TRUE){
            df <- KReact()$df
            Xvar <- input$Select_Xvar
            Yvar <- input$Select_Yvar
            Zvar <- input$Select_Zvar
            NroCent <- input$Slider_K
            
            if(!is.null(df)){
                #Corregimos NroCent.
                if(is.null(NroCent)){
                    NroCent <- 1
                }
                if(nrow(df[c(Xvar,Yvar)] %>% unique()) < NroCent){
                    NroCent <- 1 
                }
                
                #Check
                if(class(df) == "data frame"){
                    nfilas <- nrow(df)
                }else{
                    nfilas <- length(df)
                }
                
                if(nfilas != 0){
                    #K-means.
                    k <- kmeans(df[c(Xvar,Yvar)],centers = NroCent, nstart =100)				
                    df$cluster <- as.factor(k$cluster)				
                    table(df$cluster,df[Zvar][,1])						
                    centros <- as.data.frame(k$centers)	
                    
                    #Salida de datos.
                    return(list(df,centros,k))
                    
                }else{
                    return(NULL)
                }
            }else{
                return(NULL)
            }
        }else{
            return(NULL)
        }
     })
    
    #Plot de K-Medias.
    output$RPlot_Kmeans <- renderPlot({
        if(!is.null(KmedReact())){
            #Ingreso de datos
            Xvar <- input$Select_Xvar
            Yvar <- input$Select_Yvar
            Zvar <- input$Select_Zvar
            df <- KmedReact()[[1]]
            centros <- KmedReact()[[2]]
          
            #Semilla del jitter.
            set.seed("1376")
            
            ##Scatterplot con k-means.
            g <- ggplot(data=df,aes(get(Xvar),get(Yvar),color = cluster,
                            shape = get(Zvar))) + geom_point(size = 4) +
                geom_point(data=centros,aes(get(Xvar),get(Yvar)),
                            color = "blue",shape="X",size = 6,alpha=1/3) +
                geom_jitter(width= 0.005, height = 0.005,size = 4) +
                ggtitle("Scatterplot with Clustering by K-means")
            g
        }
    })   
    
    #HEATMAP ///////////////////////////////////////////////////////
    
    output$RPlot_HeatM <- renderPlot({
        
        if(!is.null(KmedReact()) == TRUE){
            #Ingreso de datos
            Xvar <- input$Select_Xvar
            Yvar <- input$Select_Yvar
            Zvar <- input$Select_Zvar
            df <- KmedReact()[[1]]
            centros <- KmedReact()[[2]]
            k <- KmedReact()[[3]]
            
            ##Creando los datos del heatmap.	
            dfMat <- as.matrix(df[c(Xvar,Yvar)])					
            row.names(dfMat) <- 1:nrow(dfMat)				
            
            #Normalizamos datos para apreciar colores.
            dfMat[,1] <- (dfMat[,1] - min(dfMat[,1]))/
                                             (max(dfMat[,1]) - min(dfMat[,1]))
            dfMat[,2] <- (dfMat[,2] - min(dfMat[,2]))/
                                             (max(dfMat[,2]) - min(dfMat[,2]))
                
            grouped  <- t(dfMat)[,order(k$cluster)]			
            coorXg <- seq(0,1,length.out=nrow(grouped))
            coorYg <- seq(0,1,length.out=ncol(grouped))
           
            #Heatmap.
            image(t(dfMat)[,order(k$cluster)],xaxt= "n",yaxt= "n")	
            axis(1, at = coorXg,labels = rownames(grouped),las=2)
            
            text(rep(coorXg[1],length(coorYg)),coorYg,
                 labels = df[Zvar][order(k$cluster),1])				
            text(rep(coorXg[2],length(coorYg)),coorYg,
                 labels = df[Zvar][order(k$cluster),1])
            title("Mapa de Calor Dataset Agrupado")	
        }
    })
    
    #TAB PANEL: CLUSTER JERARQUICO /////////////////////////////////////////////
    
    output$RPlot_Hclust <- renderPlot({
        df <- dfReact()
        Xvar <- input$Select_Xvar
        Yvar <- input$Select_Yvar
        Zvar <- input$Select_Zvar
        legend <- input$Box_Legend
        BoxX <- input$Box_VarX
        BoxY <- input$Box_VarY
        
        #Corrector de inputs renderizados.
        if(is.null(legend)){
            legend <- TRUE
        }
        if(is.null(BoxX)){
            BoxX <- TRUE
        }
        if(is.null(BoxY)){
            BoxY <- TRUE
        }
        
        #Subsetting especial para hclust. Existe un bug en potencia. 
        if(BoxX == T & BoxY == T){
            vecCol <- which(names(df) %in% c(Xvar,Yvar))
            df1 <- df[,vecCol]
            plotear <- TRUE
            
        }else if(BoxX == T){
            vecCol <- which(names(df) %in% c(Xvar))
            df1 <- df[,vecCol]
            plotear <- TRUE
            
        }else if(BoxY == T){
            vecCol <- which(names(df) %in% c(Yvar))
            df1 <- df[,vecCol]
            plotear <- TRUE
            
        }else{
            plotear <- FALSE
        }
        
        #Filtro de df1
        if(class(df1) == "data frame"){
            if(nrow(df1) < 2){
                plotear <- FALSE
            }
        }else{
            if(length(df1) < 2){
                plotear <- FALSE
            }
        }
        
        #Clusterizacion jerarquica.
        if(plotear == TRUE){
            d <- dist(df1)											
            cluster <- hclust(d)
            #Corrector del Bug.
            cluster$labels <- as.character(1:nrow(df))
            myplclust(cluster,lab.col = unclass(df[Zvar][,]))
            
            #Agregamos leyenda.
            if(legend == TRUE){
                legend("topright",legend = unique(df[Zvar][,]),
                       col = unique(df[Zvar][,]),pch=19)
            }
        }
    })
    
    #INPUT: Selector de leyenda.
    #output$RUi_checkboxLegend
    output$RUi_checkboxLegend<- renderUI({
        checkboxInput("Box_Legend","Mostrar Leyenda.",value=T)
    })
    
    #INPUT: Selector de la variable X.
    #output$RUi_checkboxVarX
    output$RUi_checkboxVarX<- renderUI({
        checkboxInput("Box_VarX",paste0("Cluster con la variable ",
                                        input$Select_Xvar),value=T)
    })
    
    #INPUT: Selector de la variable Y.
    #output$RUi_checkboxVarY
    output$RUi_checkboxVarY<- renderUI({
        checkboxInput("Box_VarY",paste0("Cluster con la variable ",
                                        input$Select_Yvar),value=T)
    })
    
    
    
})
