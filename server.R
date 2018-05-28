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
options(encoding = 'UTF-8')

#Archive server.R.
shinyServer(function(input,output){
    
    #EVALUADOR SHOWME///////////////////////////////////////////////////////////
    # output$ShowMe <-renderText({          #Print section
    #     input$panels
    # })
    # AllInputs <- reactive({
    #     x <- reactiveValuesToList(input)
    #     print(names(x))
    #     #print(unlist(x, use.names = FALSE))
    # })
    # output$show_inputs <- renderTable({
    #     AllInputs()
    #     return(NULL)
    # })
    
    ############################################################################
    ###############////////////////////////////////////////////////#############
    #//////////////////////------------------------------///////////////////////
    #______________________      EXPLORACION BASICA      ______________________#
    #//////////////////////------------------------------///////////////////////
    ###############////////////////////////////////////////////////#############
    ############################################################################

    
    #===========================================================================
    # SIDEBAR PANEL RENDERIZADO ////////////////////////////////////////////////
    #===========================================================================
    
    #---------------------------------------------------------------------------
    #INPUT: Variable categórica Z (Zvar). 
    output$RUi_Select_Zvar <- renderUI({
        #Input
        BoxNum <- input$Box_Numerica
        
        #Genera Select_Zvar
        if(BoxNum == FALSE){
            variables <-FactorVars
            selected <- variables[2]
        }else{
            variables <- NumericVars
            selected <- variables[3]
        }
        selectInput("Select_Zvar", "Variable categórica Z:",
            choices  = variables,
            selected = selected
        )
    })

    #---------------------------------------------------------------------------
    #INPUT: Variable categórica W (Wval).
    output$RUi_Select_Wvar <- renderUI({
        #Input
        Zvar <- input$Select_Zvar
        BoxNum <- input$Box_Numerica
        
        #Esperamos actualizacion
        if(is.null(Zvar)){
            return(NULL)
        }

        #Genera Select_Zval
        if(BoxNum == FALSE){
            variables <- FactorVars[which(FactorVars != Zvar)]
            selected <- variables[1]
        }else{
            variables <- FactorVars
            selected <- variables[1] 
        }
        selectInput("Select_Wvar", "Variable categórica W:",
                    choices  = variables,
                    selected = selected
        )
    })
    
    #---------------------------------------------------------------------------
    #INPUT: VALOR DE LA VARIABLE DE EXPLORACION (Xvar).
    observeEvent(input$panels,{
        #Log
        logXvar <<- c(logXvar,input$panels)
        #User interface
        if(input$panels %in%  c("Cross Tables","Mapa","Boxplots","Histograms")){
            recVarX <<- input$Select_Xvar
            output$RUi_Select_Xvar <- renderUI({})
        }else{
            if(is.null(input$Select_Xvar)){
                output$RUi_Select_Xvar <- renderUI({
                    #Genera Select_Yvar
                    selectInput("Select_Xvar","Variable numérica X:",
                                choices  = NumericVars
                                ,
                                selected = NumericVars[1]
                    )
                })
            }else{
                if(logYvar[length(logXvar) -1] %in% c("Cross Tables","Mapa",
                                                      "Boxplots","Histograms")){
                    output$RUi_Select_Xvar <- renderUI({
                        #Genera Select_Yvar
                        selectInput("Select_Xvar","Variable numérica X:",
                                choices  = NumericVars
                                ,
                                selected = NumericVars[NumericVars == recVarX]
                        )
                    }) 
                }
            }
        }
    })
    
    #---------------------------------------------------------------------------
    #INPUT: VALOR DE LA VARIABLE DE EXPLORACION (Yvar).
    observeEvent(input$panels,{
        #Log
        logYvar <<- c(logYvar,input$panels)
        #User interface
        if(input$panels %in% "Cross Tables"){
            recVarY <<- input$Select_Yvar
            output$RUi_Select_Yvar <- renderUI({})
        }else{
            if(is.null(input$Select_Yvar)){
                output$RUi_Select_Yvar <- renderUI({
                    #Genera Select_Yvar
                    selectInput("Select_Yvar","Variable numérica Y:",
                                choices  = NumericVars
                                ,
                                selected = NumericVars[2]
                    )
                })
            }else{
                if(logYvar[length(logYvar) -1] == "Cross Tables"){
                    output$RUi_Select_Yvar <- renderUI({
                        #Genera Select_Yvar
                        selectInput("Select_Yvar","Variable numérica Y:",
                                choices  = NumericVars
                                ,
                                selected = NumericVars[NumericVars == recVarY]
                        )
                    }) 
                }
            }
        }
    })
    
    #---------------------------------------------------------------------------
    #INPUT: CORRIENTES VARIABLE EXPLORATORIA 1 (CorrY).
    output$RUi_Select_CorrY <- renderUI({
        #Input
        Yvar <- input$Select_Yvar
        
        #Esperamos actualizacion
        if(is.null(Yvar)){
            return(NULL)
        }
        #Genera Select_CorrY
        if(Yvar == "Tiempo.en.Corriente.dias"){
            alt <- as.character(unique(DataSet["objeto.t"][,1]))  
            alt <- alt[!is.na(alt)]
            selectInput("Select_CorrY", "Objeto de estudio VarX:",
                        choices  = c("Todos",alt),
                        selected = "Todos"
            )
        }
    })
    
    #---------------------------------------------------------------------------
    #INPUT: DISTANCIA VARIABLE EXPLORATORIA 1 (DistY).
    output$RUi_Select_DistY <- renderUI({
        #Input
        Yvar <- input$Select_Yvar
        
        #Esperamos actualizacion
        if(is.null(Yvar)){
            return(NULL)
        }
        #Genera Select_DistY
        if(Yvar == "Distancia.Euclidea.kms"){
            alt <- as.character(unique(DataSet["objeto.d"][,1])) 
            alt <- alt[!is.na(alt)]
            selectInput("Select_DistY", "Objeto de estudio VarY:",
                        choices  = c("Todos",alt),
                        selected = "Todos"
            )
        }
    })

    #---------------------------------------------------------------------------
    #INPUT: CORRIENTES VARIABLE EXPLORATORIA 2 (CorrX).
    output$RUi_Select_CorrX <- renderUI({
        #Input
        Xvar <- input$Select_Xvar
        
        #Esperamos actualizacion
        if(is.null(Xvar)){
            return(NULL)
        }
        #Genera Select_CorrX
        if(Xvar == "Tiempo.en.Corriente.dias"){
            alt <- as.character(unique(DataSet["objeto.t"][,1])) 
            alt <- alt[!is.na(alt)]
            selectInput("Select_CorrX", "Seleccione el objeto de estudio:",
                        choices  = c("Todos",alt),
                        selected = "Todos"
            )
        }
    })
    
    #---------------------------------------------------------------------------
    #INPUT: DISTANCIA VARIABLE EXPLORATORIA 2 (DistX).
    output$RUi_Select_DistX <- renderUI({
        #Input
        Xvar <- input$Select_Xvar
        
        #Esperamos actualizacion
        if(is.null(Xvar)){
            return(NULL)
        }
        #Genera Select_DistX
        if(Xvar == "Distancia.Euclidea.kms"){
            alt <- as.character(unique(DataSet["objeto.d"][,1]))  
            alt <- alt[!is.na(alt)]
            selectInput("Select_DistX", "Seleccione el objeto de estudio:",
                        choices  = c("Todos",alt),
                        selected = "Todos"
            )
        }
    })
    
    #===========================================================================
    # FUNCIONES REACTIVAS //////////////////////////////////////////////////////
    #===========================================================================
    
    #Genera la tabla de datos adecuada en funcion de los inputs de usuario.
    #Extrae las variables requeridas para la exploracion basica y hace los 
    #subsetting correspondientes.
    dfLatLonReact <- reactive({
        Xvar <- input$Select_Xvar
        Yvar <- input$Select_Yvar
        Zvar <- input$Select_Zvar 
        Wvar <- input$Select_Wvar 
        
        #Esperamos actualizacion.
        if(is.null(Xvar)){
            return(NULL)
        }
        if(is.null(Yvar)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(Wvar)){
            return(NULL)
        }
        
        #Matar procedimiento si faltan UI's rendereizadas.
        if(Yvar == "Tiempo.en.Corriente.dias"){
            obj <- input$Select_CorrY
            if(is.null(obj)){
                return(NULL)
            }
        }else if(Yvar == "Distancia.Euclidea.kms"){
            obj <- input$Select_DistY
            if(is.null(obj)){
                return(NULL)
            }
        }
        if(Xvar == "Tiempo.en.Corriente.dias"){
            obj <- input$Select_CorrX
            if(is.null(obj)){
                return(NULL)
            }
        }else if(Xvar == "Distancia.Euclidea.kms"){
            obj <- input$Select_DistX
            if(is.null(obj)){
                return(NULL)
            }
        }
        
        withProgress(message = 'Cargando Datos', value = 0, {
            
            #==========
            incProgress(1/10)
            #==========
            
            #Subsetting de variables.
            if(("Tiempo.en.Corriente.dias" %in% NumericVars)  == TRUE){
                df <- DataSet[c(Wvar,"latitude","longitude",Zvar,Xvar,Yvar,
                                "lat.objeto.d","lon.objeto.d","lat.objeto.t",
                                "lon.objeto.t")]
            }else{
                df <- DataSet[c(Wvar,Zvar,Xvar,Yvar)] 
            }

            #==========
            incProgress(1/10)
            #==========
            
            #Incluimos columnas con informacion de Google Earth en caso de aplicar.
            tyv <- FALSE
            if(Yvar == "Tiempo.en.Corriente.dias"){
                tyv <- TRUE
                df <- cbind(df,DataSet["objeto.t"])
                names(df)[ncol(df)] <- "objeto.t.Yvar"
            }
            dyv <- FALSE
            if(Yvar == "Distancia.Euclidea.kms"){
                dyv <- TRUE
                df <- cbind(df,DataSet["objeto.d"])
                names(df)[ncol(df)] <- "objeto.d.Yvar"
            }
            txv <- FALSE
            if(Xvar == "Tiempo.en.Corriente.dias"){
                txv <- TRUE
                df <- cbind(df,DataSet["objeto.t"])
                names(df)[ncol(df)] <- "objeto.t.Xvar"
            }
            dxv <- FALSE
            if(Xvar == "Distancia.Euclidea.kms"){
                dxv <- TRUE
                df <- cbind(df,DataSet["objeto.d"])
                names(df)[ncol(df)] <- "objeto.d.Xvar"
            }
            #==========
            incProgress(1/10)
            #==========
            
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
            #==========
            incProgress(1/10)
            #==========
            
            #Eliminamos repetidos
            df <- df %>% unique()
            
            #Si no tiene datos salimos.
            if(nrow(df) == 0){
                return(NULL)
            }
            #==========
            incProgress(1/10)
            #==========
            
            #Subsetting para tablas de Google Earth.
            #Primera variable exploratoria.
            if(Yvar == "Tiempo.en.Corriente.dias"){
                obj <- input$Select_CorrY
                #Subsetting
                if(obj != "Todos"){
                    df <- df[df$objeto.t.Yvar == obj,]
                }
            }else if(Yvar == "Distancia.Euclidea.kms"){
                obj <- input$Select_DistY
                #Subsetting
                if(obj != "Todos"){
                    df <- df[df$objeto.d.Yvar == obj,]
                }
            }
            #==========
            incProgress(1/10)
            #==========
            
            #Segunda variable exploratoria.
            if(Xvar == "Tiempo.en.Corriente.dias"){
                obj <- input$Select_CorrX
                #Subsetting
                if(obj != "Todos"){
                    df <- df[df$objeto.t.Xvar == obj,]
                }
            }else if(Xvar == "Distancia.Euclidea.kms"){
                obj <- input$Select_DistX
                #Subsetting
                if(obj != "Todos"){
                    df <- df[df$objeto.d.Xvar == obj,]
                }
            }
            #==========
            incProgress(1/10)
            #==========
            
            #Eliminamos filas con NA's en las variables de exploracion.
            df <- df[!is.na(df[Xvar][,1]),]
            df <- df[!is.na(df[Yvar][,1]),]
            
            #==========
            incProgress(1/10)
            #==========
            
            #Eliminamos repetidos
            df <- df %>% unique()
            
            #==========
            incProgress(1/10)
            #==========
            
            #Si no tiene datos salimos.
            if(nrow(df) == 0){
                return(NULL)
            }
           
            #Corregimos nombres. 
            rownames(df) <- seq(1,nrow(df))
            
            #==========
            incProgress(1/10)
            #==========
        })
        
        #Salida
        return(df)
    })
    
    #---------------------------------------------------------------------------
    #Recibe el df generado por dfLatLonReact() y le elimina las variables de
    #geolocalizacion. Son inecesarias para los calculos.
    dfReact <- reactive({
        #Input
        df <- dfLatLonReact()
        
        #Esperamos actualizacion.
        if(is.null(df)){
            return(NULL)
        }
        withProgress(message = 'Cargando Tabla Renderizada', value = 0, {
            
            if(("Tiempo.en.Corriente.dias" %in% NumericVars)  == TRUE){
                df$latitude  <- NULL
                df$longitude <- NULL
                df$lat.objeto.d <- NULL
                df$lon.objeto.d <- NULL
                df$lat.objeto.t <- NULL
                df$lon.objeto.t <- NULL
            }
            #==========
            incProgress(1/2)
            #==========
            
            #Eliminamos repetidos
            df <- df %>% unique()
            
            #==========
            incProgress(1/2)
            #==========
            
            #Corregimos nombres. 
            rownames(df) <- seq(1,nrow(df))
            
        })
        #Salida
        return(df)
    })
    
    #---------------------------------------------------------------------------
    #Tabla Renderizada que crea la variable ZvarFac y la agrega al data frame 
    #df que genera dfReact().
    #Esta nueva variable es la version factor de Zvar.
    ZvarFacReact <- reactive({
        #Inputs
        df <- dfReact()
        Zvar <- input$Select_Zvar
        BoxNum <- input$Box_Numerica
        
        #Esperamos actualizacion.    
        if(is.null(df)){
            return(NULL)   
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Variable categorica.
        if(BoxNum == TRUE){
            
            #Eliminamos los valores faltantes de la variables estudiada.
            df <- df[!is.na(df[Zvar][,1]),]
            
            #Factorizamos variable continua.
            valores <- suppressWarnings(
                as.numeric(as.character(df[Zvar][,1])))
            
            #Esperamos actualizacion.
            if(all(is.na(valores))){
                return(NULL)
            }
            
            #Construimos una particion para Zvar.
            if(sd(valores) == 0){
                cutpoints <- quantile(c(min(valores) - 0.1,valores),
                                      seq(0,1,length=2),na.rm=TRUE)	
            }else{
                #Buscamos el tamano adecuado.
                nroIni <- 8
                cutpoints <- quantile(c(min(valores) - 0.1,valores),
                                      seq(0,1,length=nroIni),na.rm=TRUE)	
                #Ciclo de busqueda
                while(any(diff(cutpoints) == 0) & nroIni > 1){
                    nroIni <- nroIni -1
                    cutpoints <- quantile(c(min(valores) - 0.1,valores),
                                          seq(0,1,length=nroIni),na.rm=TRUE)	 
                }
            }
            df$ZvarFac <- cut(valores,cutpoints)
        }
        #Exportamos dataset.
        return(df)
    })
    
    #///////////////////////////////////////////////////////////////////////////
    # TAB PANELS ///////////////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #===========================================================================
    #TAB PANEL: TABLA DE DATOS _________________________________________________
    #===========================================================================
    
    #TABLA DE DATOS ////////////////////////////////////////////////////////////
    
    #Tabla de datos
    output$RDT_E.TablaD1 <- DT::renderDataTable(
        DT::datatable({
            #Chequeo de ejecucion
            if(input$panels != "Tabla de Datos"){
                return(NULL)
            }
            
            #Input
            df <- dfLatLonReact()
            Yvar <-input$Select_Yvar
            Xvar <-input$Select_Xvar


            #Chequeamos Input.
            if(is.null(df)){
                return(NULL)
            }
            if(is.null(Yvar)){
                return(NULL)
            }
            if(is.null(Xvar)){
                return(NULL)
            }
            
            #Seleccion de variables de tabla.
            if(!("Tiempo.en.Corriente.dias" %in% c(Xvar,Yvar))){
                df$lat.objeto.t <- NULL
                df$lon.objeto.t <- NULL
            }
            if(!("Distancia.Euclidea.kms" %in% c(Xvar,Yvar))){
                df$lat.objeto.d <- NULL
                df$lon.objeto.d <- NULL
            }   
            
            #Eliminamos repetidos
            df <- df %>% unique()
            
            #Exportamos
            df
        })
    )
    

    
    #===========================================================================
    #TAB PANEL: TABLA DE FRECUENCIAS ___________________________________________
    #===========================================================================
    
    #TABLA DE FRECUENCIAS //////////////////////////////////////////////////////
    
    #Tabla de la variable Z.
    output$RTable_E.TablaD2 <- renderTable({
        #input
        Zvar <- input$Select_Zvar
        df <- ZvarFacReact()
        
        #Chequeamos Input.
        if(is.null(df)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Check numerico.
        if(input$Box_Numerica == TRUE){
            #Check input
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
            tabla <- sort(table(df["ZvarFac"][,1]), decr = T)
        }else{
            #Check input
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
            tabla <- sort(table(df[Zvar][,1]), decr = T)
        }
        
        #Resumen estadistico.
        tabla <- as.data.frame(tabla)
        names(tabla) <- c(Zvar,"Frecuencia")
        tabla
    })
    
    #--------------------------------------------
    #Tabla de la variable W.
    output$RTable_E.TablaD22 <- renderTable({
        #input
        Wvar <- input$Select_Wvar
        df <- ZvarFacReact()
        
        #Chequeamos Input.
        if(is.null(df)){
            return(NULL)
        }
        if(is.null(Wvar)){
            return(NULL)
        }
        #Resumen estadistico.
        tabla <- sort(table(df[Wvar][,1]), decr = T)
        tabla <- as.data.frame(tabla)
        names(tabla) <- c(Wvar,"Frecuencia")
        tabla
    })
    
    #--------------------------------------------
    #Tabla de la variable W.
    output$RPrint_E.TablaC <- renderTable({
        #input
        Zvar <- input$Select_Zvar
        Wvar <- input$Select_Wvar
        df <- ZvarFacReact()
        
        #Chequeamos Input.
        if(is.null(df)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(Wvar)){
            return(NULL)
        }
        
        #Check numerico.
        if(input$Box_Numerica == TRUE){
            #Check input
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
            tabla <- invisible(gmodels :: CrossTable(df["ZvarFac"][,1],
                    df[Wvar][,1], prop.r=F, prop.c=F,prop.t=F, prop.chisq=F))
        }else{
            #Check input
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
            tabla <- invisible(gmodels :: CrossTable(df[Zvar][,1],
                    df[Wvar][,1], prop.r=F, prop.c=F,prop.t=F, prop.chisq=F))
        }
        #Resumen estadistico.
        tabla <- invisible(as.data.frame(tabla$t))
        tabla <- invisible(spread(tabla,key = y,value = Freq))
        
        return(tabla)
    })
    
    #===========================================================================
    #TAB PANEL: MAPA ___________________________________________________________
    #===========================================================================

    #Mapa con puntos de geolocalizacion.
    output$RPlot_Map <-renderPlot({
        #Action Button
        input$goButtonMap1
        
        #Input
        Yvar <- isolate(input$Select_Yvar)
        df <- isolate(dfLatLonReact())
        
        #Chequeamos Input.
        if(is.null(Yvar)){
            return(NULL)
        }
        if(Yvar %in% c("Tiempo.en.Corriente.dias","Distancia.Euclidea.kms")){
            return(plot(1:2, type='n'))  
        }
        if(input$goButtonMap1 == 0){
            return(NULL)
        }
        #El dataset ingresado es de corales.
        if(("Tiempo.en.Corriente.dias" %in% NumericVars)  == FALSE){
            return(plot(1:2, type='n'))
        }
        withProgress(message = 'Creando Mapa', value = 0, {
            
        #Subsetting para el mapa.
            df <- DataSetAmb[c("latitude","longitude",Yvar)]
            
            #==========
            incProgress(1/4)
            #==========
            
            df <- df[complete.cases(df),]
            
            #==========
            incProgress(1/4)
            #==========
            
            df <- df %>% unique() 
            
            #Dibujamos el mapa solo si el conjunto de variables no queda vacio.
            if(nrow(df) != 0){
                #Centro del mapa
                lat <- mean(df["latitude"][,1])
                lon <- mean(df["longitude"][,1])
                
                #==========
                incProgress(1/4)
                
                #==========
                assign("varMap",df[Yvar][,1],envir=globalenv())

                #Plot del mapa
                map <- get_map(location=c(lon=lon,lat=lat),zoom=6,
                               maptype="terrain")
                plotMapa <-ggmap(map) +
                    geom_point(data = df,aes(x = longitude,y = latitude,
                            col = get("varMap",envir=globalenv())), 
                            show.legend = T,size = 2) +
                    scale_color_continuous(name = Yvar) +
                    labs(title=paste0("Mapa de la variable ",Yvar)) 
                
                #==========
                incProgress(1/4)
                #==========
                
                plot(plotMapa)
            }
        })
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
    
    #===========================================================================
    #TAB PANEL: BOXPLOT ________________________________________________________
    #===========================================================================
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: Boxplot por Wvar /////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #UI's RENDERIZADAS /////////////////////////////////////////////////////////
    
    #Opciones de eleccion para los valores de Zvar.
    #Genera SelectList_Zval
    output$RUi_checkboxGroup.EBP1.Zval <- renderUI({
        #Input
        df <- ZvarFacReact()
        Zvar <- input$Select_Zvar
        
        #Esperamos Actualizacion.
        if(is.null(df)){
            return(NULL)
        }
        #Esperamos Actualizacion.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Se habilita cuando la variable ingresada es factor.
        if(input$Box_Numerica == FALSE){
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
            valores <- as.character(unique(df[Zvar][,1]))
        }else{
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
            valores <- as.character(unique(df["ZvarFac"][,1]))
        }
        
        #Render UI.
        checkboxGroupInput("SelectList_Zval",
                           "Valores variable Z",
                           choices  = valores
                           ,
                           selected = valores[1:2]
        )
    })
    
    #--------------------------------------------
    #Opciones de eleccion para los valores de Wvar
    output$RUi_checkboxGroup.EBP1.Loc <- renderUI({
        df <- dfReact()
        Wvar <- input$Select_Wvar
        
        #Chequeamos Input.
        if(is.null(df)){
            return(NULL)
        }
        if(is.null(Wvar)){
             return(NULL)
        }
       
        valores <- as.character(unique(df[Wvar][,1]))
        #Interfaz.
        checkboxGroupInput("SelectList_Wval","Valores variable W",
                           choices  = valores
                           ,
                           selected = valores[1:2]
        )
    })
    
    #-------------------------------------------
    #Mostrar leyenda.
    #Genera Box_LegendBoxLoc
    output$RUi_checkbox.EBP1.Leg <- renderUI({
        checkboxInput("Box_LegendBoxLoc","Mostrar Leyenda",value=T)
    })
    
    #FUNCIONES REACTIVAS ///////////////////////////////////////////////////////
    
    #Recibe la tabla arrojada por ZvarFacReact y le habe subsetting por 
    #SelectList_Zval y SelectList_Zona.
    EBP1React <- reactive({
        #Inputs
        df <- ZvarFacReact()
        Zvar <- input$Select_Zvar
        Wvar <- input$Select_Wvar
        BoxNum <- input$Box_Numerica
        
        #recibimos inputs de los UI's renderizados.
        List_Zval <- input$SelectList_Zval
        List_Wval <- input$SelectList_Wval
        
        #Esperamos actualizacion.    
        if(is.null(df)){
            return(NULL)   
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(Wvar)){
            return(NULL)
        }
        if(is.null(List_Zval)){
            return(NULL)   
        }
        if(is.null(List_Wval)){
            return(NULL)
        }
        #Eliminamos los valores faltantes de la variables estudiada.
        df <- df[!is.na(df[Zvar][,1]),]
        
        #Variable categorica.
        if(BoxNum == TRUE){
            
            #Subsetting del data frame df.
            df <- df[df[Wvar][,1] %in% List_Wval,]
            
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
            
        }else{
            
            #Subsetting del data frame df.
            df <- df[df[Wvar][,1] %in% List_Wval,]
            df <- df[df[Zvar][,1] %in% List_Zval,]
            
        }
        #Exportamos dataset.
        return(df)
        
    })
    
    #GRAFICO BOXPLOT ///////////////////////////////////////////////////////////
    
    #Genera Grafico Boxplot.
    output$RPlot_E.Box1 <- renderPlot({
        #Action Button
        input$goButtonE.Box1
        
        #Input
        df <- isolate(EBP1React())
        Yvar <- isolate(input$Select_Yvar)
        Zvar <- isolate(input$Select_Zvar)
        Wvar <- isolate(input$Select_Wvar)
        BoxNum <- isolate(input$Box_Numerica)
        leyenda <- isolate(input$Box_LegendBoxLoc)
        
        #Esperamos Actualizacion.
        if(is.null(Yvar)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(Wvar)){
            return(NULL)
        }
        if(is.null(leyenda)){
            return(NULL)
        }
        if(is.null(df)){
            return(plot(1:2, type='n'))
        }
        if(nrow(df) == 0){
            return(plot(1:2, type='n'))
        }
        
        #Realizamos boxplots
        if(BoxNum == TRUE){
            
            #Configuramos plot.
            BoxPlot <- ggplot(df,aes(ZvarFac,get(Yvar),fill = ZvarFac)) + 			
                geom_boxplot()	+ facet_grid(.~get(Wvar)) + 
                ylab(Yvar) + xlab(Zvar) +
                scale_fill_discrete(name = Zvar) +
                ggtitle(
                    paste0("Boxplots de ",Yvar," por ",Wvar,"."))									
        }else{
            #Configuramos plot.
            BoxPlot <- ggplot(df,aes(get(Zvar),get(Yvar),fill =get(Zvar))) + 			
                geom_boxplot()	+ facet_grid(.~get(Wvar)) +
                ylab(Yvar) + xlab(Zvar) +
                scale_fill_discrete(name = Zvar) +
                ggtitle(
                    paste0("Boxplots de ",Yvar," por ",Wvar,"."))			
        }
        
        #Leyenda
        if(leyenda == TRUE){
            BoxPlot 
        }else{
            BoxPlot + theme(legend.position="none")
        }
        
    })
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: Boxplot Var #1 ///////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #UI's RENDERIZADAS /////////////////////////////////////////////////////////
    
    #Opciones de eleccion para los valores de Zvar.
    #Genera SelectList_Zval
    output$RUi_checkboxGroup.EBP2.Zval <- renderUI({
        #Input
        df <- ZvarFacReact()
        Zvar <- input$Select_Zvar
        
        #Esperamos Actualizacion.
        if(is.null(df)){
            return(NULL)
        }
        #Esperamos Actualizacion.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Se habilita cuando la variable ingresada es factor.
        if(input$Box_Numerica == FALSE){
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
            valores <- as.character(unique(df[Zvar][,1]))
        }else{
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
            valores <- as.character(unique(df["ZvarFac"][,1]))
        }
        
        #Render UI.
        checkboxGroupInput("SelectList_ZvalVar1",
                           "Valores variable Z",
                           choices  = valores
                           ,
                           selected = valores[1:2]
        )
    })
    
    #--------------------------------------
    #Mostrar leyenda.
    #Genera Box_LegendBoxLocVar1
    output$RUi_checkbox.EBP2.Leg <- renderUI({
        checkboxInput("Box_LegendBoxLocVar1","Mostrar Leyenda",value=T)
    })
    
    #FUNCIONES REACTIVAS ///////////////////////////////////////////////////////
    
    EBP2React <- reactive({
        #Inputs
        df <- ZvarFacReact()
        Zvar <- input$Select_Zvar
        BoxNum <- input$Box_Numerica
        
        #recibimos inputs de los UI's renderizados.
        List_Zval <- input$SelectList_ZvalVar1
        
        #Esperamos actualizacion.    
        if(is.null(df)){
            return(NULL)   
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(List_Zval)){
            return(NULL)   
        }
        
        #Eliminamos los valores faltantes de la variables estudiada.
        df <- df[!is.na(df[Zvar][,1]),]
        
        #Variable categorica.
        if(BoxNum == TRUE){
            
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
            
        }else{
            #Subsetting del data frame df.
            df <- df[df[Zvar][,1]  %in% List_Zval,]
        }
        #Exportamos dataset.
        return(df)
        
    })
    
    #GRAFICO BOXPLOT ///////////////////////////////////////////////////////////
    
    #Genera Grafico Boxplot.
    output$RPlot_E.Box2 <- renderPlot({
        #Action Button
        input$goButtonE.Box2
        
        #Input
        df <- isolate(EBP2React())
        Yvar <- isolate(input$Select_Yvar)
        Zvar <- isolate(input$Select_Zvar)
        BoxNum <- isolate(input$Box_Numerica)
        leyenda <- isolate(input$Box_LegendBoxLocVar1)

        #Esperamos Actualizacion.
        if(is.null(Yvar)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(leyenda)){
            return(NULL)
        }
        if(is.null(df)){
            return(plot(1:2, type='n'))
        }
        if(nrow(df) == 0){
            return(plot(1:2, type='n'))
        }
        
        #Realizamos boxplots
        if(BoxNum == TRUE){
            #Configuramos plot.
            BoxPlot <- ggplot(df,aes(ZvarFac,get(Yvar),fill = ZvarFac)) + 			
                geom_boxplot()	 + 
                ylab(Yvar) + xlab(Zvar) +
                scale_fill_discrete(name = Zvar) +
                ggtitle(paste0("Boxplots de ",Yvar))									
        }else{
            #Configuramos plot.
            BoxPlot <- ggplot(df,aes(get(Zvar),get(Yvar),fill =get(Zvar))) + 			
                geom_boxplot()	+ 
                ylab(Yvar) + xlab(Zvar) +
                scale_fill_discrete(name = Zvar) +
                ggtitle(
                    paste0("Boxplots de ",Yvar))			
        }
        
        #Leyenda
        if(leyenda == TRUE){
            BoxPlot 
        }else{
            BoxPlot + theme(legend.position="none")
        }
        
    })
    
    #--------------------------------------------
    #Resumen de la variable Y.
    output$RTable_E.Box2 <-renderTable({ 
        #Action Button
        input$goButtonE.Box2
        
        #Input 
        df <- isolate(EBP2React())
        Yvar <- isolate(input$Select_Yvar)
        Zvar <- isolate(input$Select_Zvar)
        BoxNum <- isolate(input$Box_Numerica)
        
        #Chequeamos Input.
        if(is.null(df)){
            return(NULL)
        }
        if(is.null(Yvar)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Check de input.
        if(BoxNum == TRUE){
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
        }else{
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL) 
            }
        }
        
        #Variable numerica
        if(BoxNum == TRUE){
            Zvar <- "ZvarFac"
        }
        
        List_Zval <- as.character(unique(df[Zvar][,1]))
        
        #Resumen estadistico Zval #1.
        resumen <- summary(df[Yvar][df[Zvar][,1] == List_Zval[1],1])
        nombres <- names(resumen)

        if("NA's" %in% nombres){
            tabla <- rep(NA,times=14)
            dim(tabla) <- c(2,7)
        }else{
            tabla <- rep(NA,times=12)
            dim(tabla) <- c(2,6)
        }
        
        tabla <- as.data.frame(tabla)
        names(tabla) <- nombres
        tabla[1,] <- as.vector(resumen)
        tabla <- tabla[1,]

        #Agregamos columnas de resumen de NA's.
        if(dim(tabla)[2] == 6){
            tabla[,7] <- 0
            names(tabla)[7] <- "NA's"
        }
        
        #Agregamos Zval a la tabla resumen.
        tabla[,2:8] <- tabla
        names(tabla)[2:8] <- names(tabla)[1:7]
        names(tabla)[1] <- " "
        tabla[,1] <- List_Zval[1]

        #Exportamos tabla.
        if(length(List_Zval) == 1){
            return(tabla)
        }
        
        for(i in 2:length(List_Zval)){
            #Resumen estadistico Zval #i.
            resumen <- summary(df[Yvar][df[Zvar][,1] == List_Zval[i],1])
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
            
            #Agregamos columnas de resumen de NA's.
            if(dim(d)[2] == 6){
                d[,7] <- 0
                names(d)[7] <- "NA's"
            }
            
            #Agregamos Zval a la tabla resumen.
            d[,2:8] <- d
            names(d)[2:8] <- names(d)[1:7]
            names(d)[1] <- " "
            d[,1] <- List_Zval[i]
            
            #Unimos tabla.
            tabla <- rbind(tabla,d) 
        }
        return(tabla)
    })
    
    #Nombre de variable
    output$RText_E.Box2 <- renderText({
        c("Resumen Estadístico: ",input$Select_Yvar)
    })
    
    #===========================================================================
    #TAB PANEL: HISTOGRAMAS ////////////////////////////////////////////////////
    #===========================================================================
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: Histograma por Wvar //////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #UI's RENDERIZADAS /////////////////////////////////////////////////////////
    
    #Opciones de eleccion para los valores de Zvar.
    #Genera SelectList_Zval2
    output$RUi_checkboxGroup.EHG1.Zval <- renderUI({
        #Input
        df <- ZvarFacReact()
        Zvar <- input$Select_Zvar
        
        #Esperamos Actualizacion.
        if(is.null(df)){
            return(NULL)
        }
        #Esperamos Actualizacion.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Se habilita cuando la variable ingresada es factor.
        if(input$Box_Numerica == FALSE){
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
            valores <- as.character(unique(df[Zvar][,1]))
        }else{
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
            valores <- as.character(unique(df["ZvarFac"][,1]))
        }
        
        #Render UI.
        checkboxGroupInput("SelectList_Zval2",
                           "Valores variable Z",
                           choices  = valores
                           ,
                           selected = valores[1:2]
        )
    })

    #--------------------------------------------
    #Opciones de eleccion para los valores de Wvar
    output$RUi_checkboxGroup.EHG1.Loc <- renderUI({
        df <- dfReact()
        Wvar <- input$Select_Wvar
        
        #Chequeamos Input.
        if(is.null(df)){
            return(NULL)
        }
        if(is.null(Wvar)){
            return(NULL)
        }
        
        valores <- as.character(unique(df[Wvar][,1]))
        #Interfaz.
        checkboxGroupInput("SelectList_Wval2","Valores variable W",
                           choices  = valores
                           ,
                           selected = valores[1:2]
        )
    })
    
    #-------------------------------------
    #Mostrar leyenda..
    #Genera Box_LegendHistLoc
    output$RUi_checkbox.EHG1.Leg <- renderUI({
        checkboxInput("Box_LegendEHG1","Mostrar Leyenda",value=T)
    })

    #FUNCIONES REACTIVAS ///////////////////////////////////////////////////////
    
    #Recibe la tabla arrojada por ZvarFacReact y le habe subsetting por 
    #SelectList_Zval2 y SelectList_Zona2
    EHG1React <- reactive({
        #Inputs
        df <- ZvarFacReact()
        Zvar <- input$Select_Zvar
        Wvar <- input$Select_Wvar
        BoxNum <- input$Box_Numerica
        
        #recibimos inputs de los UI's renderizados.
        List_Zval <- input$SelectList_Zval2
        List_Wval <- input$SelectList_Wval2
        
        #Esperamos actualizacion.    
        if(is.null(df)){
            return(NULL)   
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(Wvar)){
            return(NULL)
        }
        if(is.null(List_Zval)){
            return(NULL)   
        }
        if(is.null(List_Wval)){
            return(NULL)
        }
        
        #Eliminamos los valores faltantes de la variables estudiada.
        df <- df[!is.na(df[Zvar][,1]),]
        
        #Variable categorica.
        if(BoxNum == TRUE){
            
            #Subsetting del data frame df.
            df <- df[df[Wvar][,1] %in% List_Wval,]
            
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
            
        }else{
            #Subsetting del data frame df.
            df <- df[df[Wvar][,1] %in% List_Wval,]
            df <- df[df[Zvar][,1] %in% List_Zval,]
            
        }
        #Exportamos dataset.
        return(df)
        
    })
    
    #GRAFICO HiSTOGRAMA ////////////////////////////////////////////////////////
    
    #Genera Grafico Histograma.
    output$RPlot_E.His1 <- renderPlot({
        #Action Button
        input$goButtonE.His1
        
        #Input
        df <- isolate(EHG1React())
        Yvar <- isolate(input$Select_Yvar)
        Zvar <- isolate(input$Select_Zvar)
        Wvar <- isolate(input$Select_Wvar)
        BoxNum <- isolate(input$Box_Numerica)
        leyenda <- isolate(input$Box_LegendEHG1)
        
        #Esperamos Actualizacion.
        if(is.null(Yvar)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(Wvar)){
            return(NULL)
        }
        if(is.null(leyenda)){
            return(NULL)
        }
        if(is.null(df)){
            return(plot(1:2, type='n'))
        }
        if(nrow(df) == 0){
            return(plot(1:2, type='n'))
        }
        
        #Realizamos boxplots
        if(BoxNum == TRUE){
            #Configuramos plot.
            HistP <- ggplot(df,aes(get(Yvar),fill = ZvarFac)) + 			
                geom_histogram() + facet_grid(ZvarFac ~ get(Wvar)) + 
                xlab(Yvar) + ylab("Frecuencia") + 
                scale_fill_discrete(name = Zvar) +
                ggtitle(paste0("Histogramas de ",Yvar," por ",Wvar,"."))									
        }else{
            #Configuramos plot.
            HistP <- ggplot(df,aes(get(Yvar),fill = get(Zvar))) + 			
                geom_histogram() + 
                facet_grid(get(Zvar) ~ get(Wvar)) + 
                xlab(Yvar) + ylab("Frecuencia") + 
                scale_fill_discrete(name = Zvar) +
                ggtitle(paste0("Histogramas de ",Yvar," por ",Wvar,"."))
        }
        
        #Leyenda
        if(leyenda == TRUE){
            HistP 
        }else{
            HistP + theme(legend.position="none")
        }
  
    })
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: Histograma Var #1 ////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #UI's RENDERIZADAS /////////////////////////////////////////////////////////
    
    #Opciones de eleccion para los valores de Zvar.
    #Genera SelectList_Zval2
    output$RUi_checkboxGroup.EHG2.Zval <- renderUI({
        #Input
        df <- ZvarFacReact()
        Zvar <- input$Select_Zvar
        
        #Esperamos Actualizacion.
        if(is.null(df)){
            return(NULL)
        }
        #Esperamos Actualizacion.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Se habilita cuando la variable ingresada es factor.
        if(input$Box_Numerica == FALSE){
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
            valores <- as.character(unique(df[Zvar][,1]))
        }else{
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
            valores <- as.character(unique(df["ZvarFac"][,1]))
        }
        
        #Render UI.
        checkboxGroupInput("SelectList_EHG2.Zval",
                           "Valores variable Z",
                           choices  = valores
                           ,
                           selected = valores[1:2]
        )
    })
    
    #-------------------------------------
    #Mostrar leyenda..
    #Genera Box_LegendHistLoc
    output$RUi_checkbox.EHG2.Leg <- renderUI({
        checkboxInput("Box_LegendEHG2","Mostrar Leyenda",value=T)
    })
    
    #-------------------------------------
    #Nombre de variable
    output$RText_E.His2.1 <- renderText({
        c("Test de Normalidad: Variable ",input$Select_Yvar)
    })
      
    #--------------------------------------------
    #Test de Normalidad.
    output$RPrnt_E.His2.2 <-renderPrint({ 
        #Input 
        df <- EHG2React()
        Yvar <- input$Select_Yvar
        
        #Chequeamos Input.
        if(is.null(df)){
            return(NULL)
        }
        if(is.null(Yvar)){
            return(NULL)
        }
        #EL df debe contener suficientes datos.
        if(nrow(df) <= 7){
            return(NULL)
        }
        test1 <- shapiro.test(df[Yvar][,1])
        test2 <- ks.test(df[Yvar][,1],"pnorm") 
        test3 <- nortest :: ad.test(df[Yvar][,1])
        
        print(test1)
        print(test2)
        print(test3)
    })   
    
    #FUNCIONES REACTIVAS ///////////////////////////////////////////////////////
    
    #Recibe la tabla arrojada por ZvarFacReact y le habe subsetting por 
    #SelectList_Zval2
    EHG2React <- reactive({
        #Inputs
        df <- ZvarFacReact()
        Zvar <- input$Select_Zvar
        BoxNum <- input$Box_Numerica
        
        #recibimos inputs de los UI's renderizados.
        List_Zval <- input$SelectList_EHG2.Zval
        
        #Esperamos actualizacion.    
        if(is.null(df)){
            return(NULL)   
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(List_Zval)){
            return(NULL)   
        }
        
        #Eliminamos los valores faltantes de la variables estudiada.
        df <- df[!is.na(df[Zvar][,1]),]
        
        #Variable categorica.
        if(BoxNum == TRUE){
            
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
            
        }else{

            #Subsetting del data frame df.
            df <- df[df[Zvar][,1]  %in% List_Zval,]
            
        }
        #Exportamos dataset.
        return(df)
        
    })
    
    #GRAFICO HiSTOGRAMA ////////////////////////////////////////////////////////
    
    #Genera Grafico Histograma.
    output$RPlot_E.His2 <- renderPlot({
        #Action Button
        input$goButtonE.His2
        
        #Input
        df <- isolate(EHG2React())
        Yvar <- isolate(input$Select_Yvar)
        Zvar <- isolate(input$Select_Zvar)
        BoxNum <- isolate(input$Box_Numerica)
        leyenda <- isolate(input$Box_LegendEHG2)
        
        #Esperamos Actualizacion.
        if(is.null(Yvar)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(leyenda)){
            return(NULL)
        }
        if(is.null(df)){
            return(plot(1:2, type='n'))
        }
        if(nrow(df) == 0){
            return(plot(1:2, type='n'))
        }
        
        #Realizamos boxplots
        if(BoxNum == TRUE){
            #Configuramos plot.
            HistP <- ggplot(df,aes(get(Yvar),fill = ZvarFac)) + 			
                geom_histogram() + 
                xlab(Yvar) + ylab("Frecuencia") + 
                scale_fill_discrete(name = Zvar) +
                ggtitle(paste0("Histogramas de ",Yvar))									
        }else{
            #Configuramos plot.
            HistP <- ggplot(df,aes(get(Yvar),fill = get(Zvar))) + 			
                geom_histogram()  + 
                xlab(Yvar) + ylab("Frecuencia") + 
                scale_fill_discrete(name = Zvar) +
                ggtitle(paste0("Histogramas de ",Yvar))
        }
        
        #Leyenda
        if(leyenda == TRUE){
            HistP 
        }else{
            HistP + theme(legend.position="none")
        }
        
    })
    
    #===========================================================================
    #TAB PANEL: SCATTERPLOT ////////////////////////////////////////////////////
    #===========================================================================

    #UI's RENDERIZADAS /////////////////////////////////////////////////////////
    
    #Opciones de eleccion para los valores de Zvar.
    #Genera SelectList_Zval3
    output$RUi_checkboxGroup3.Zvar <- renderUI({
        #Input
        df <- ZvarFacReact()
        Zvar <- input$Select_Zvar
        
        #Esperamos Actualizacion.
        if(is.null(df)){
            return(NULL)
        }
        #Esperamos Actualizacion.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Se habilita cuando la variable ingresada es factor.
        if(input$Box_Numerica == FALSE){
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
            valores <- as.character(unique(df[Zvar][,1]))
        }else{
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
            valores <- as.character(unique(df["ZvarFac"][,1]))
        }
        
        #Render UI.
        checkboxGroupInput("SelectList_Zval3",
                           "Selector de valores.",
                           choices  = valores
                           ,
                           selected = valores[1:2]
        )
    })
    
    #--------------------------------------------
    #Opciones de eleccion para los valores de Wvar
    output$RUi_checkboxGroup3.Zona <- renderUI({
        df <- dfReact()
        Wvar <- input$Select_Wvar
        
        #Chequeamos Input.
        if(is.null(df)){
            return(NULL)
        }
        if(is.null(Wvar)){
            return(NULL)
        }
        
        valores <- as.character(unique(df[Wvar][,1]))
        #Interfaz.
        checkboxGroupInput("SelectList_Wval3","Valores variable W",
                           choices  = valores
                           ,
                           selected = valores[1:2]
        )
    })
    
    #--------------------------------------------
    #Opciones de eleccion para los valores
    output$RUi_checkboxEtiqueta<- renderUI({
        checkboxInput("Box_Etiqueta","Mostrar Etiquetas.",value=T)
    })
    
    #--------------------------------------------
    #Opciones de eleccion para los valores
    output$RUi_checkboxJitter<- renderUI({
        checkboxInput("Box_Jitter","Dispersar puntos.",value=F)
    })
    
    #FUNCIONES REACTIVAS ///////////////////////////////////////////////////////
    
    #Recibe la tabla arrojada por ZvarFacReact y le habe subsetting por 
    #SelectList_Zval3, SelectList_Zona3
    KReact <- reactive({
        #Inputs
        df <- ZvarFacReact()
        Zvar <- input$Select_Zvar
        Wvar <- input$Select_Wvar
        BoxNum <- input$Box_Numerica
        
        #recibimos inputs de los UI's renderizados.
        List_Zval <- input$SelectList_Zval3
        List_Wval <- input$SelectList_Wval3
        
        #Esperamos actualizacion.    
        if(is.null(df)){
            return(NULL)   
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(Wvar)){
            return(NULL)
        }
        if(is.null(List_Zval)){
            return(NULL)   
        }
        if(is.null(List_Wval)){
            return(NULL)
        }
        
        #Variable categorica.
        if(BoxNum == TRUE){
            
            #Subsetting del data frame df.
            df <- df[df[Wvar][,1] %in% List_Wval,]
            
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
            
        }else{
            #Eliminamos los valores faltantes de la variables estudiada.
            df <- df[!is.na(df[Zvar][,1]),]
            
            #Subsetting del data frame df.
            df <- df[df[Wvar][,1] %in% List_Wval,]
            df <- df[df[Zvar][,1] %in% List_Zval,]
            
        }
        #Exportamos dataset.
        return(df)
        
    })
    
    #GRAFICO SCATTERPLOT ///////////////////////////////////////////////////////
    
    #Genera Grafico Scatterplot.
    output$RPlot_Scat <- renderPlot({
        #Action Button
        input$goButtonSca
        input$goButtonKme
        input$goButtonHea
        
        #Evitamos el plot automatico por la asignacion inicial del numero de
        #centroides.
        if(input$goButtonSca + input$goButtonKme + input$goButtonHea == 0){
            return(NULL)
        }
        
        #Input reactive
        #df <- isolate(KReact()$df)
        df <- isolate(KReact())
        
        #Inputs
        Xvar <- isolate(input$Select_Xvar)
        Yvar <- isolate(input$Select_Yvar)
        Zvar <- isolate(input$Select_Zvar)
        Wvar <- isolate(input$Select_Wvar)
        Etiq <- isolate(input$Box_Etiqueta)
        BoxNum <- isolate(input$Box_Numerica)
        Jitter <- isolate(input$Box_Jitter)
        
        #Esperamos Actualizacion.
        if(is.null(Xvar)){
            return(NULL)
        }
        if(is.null(Yvar)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(Wvar)){
            return(NULL)
        }
        if(is.null(df)){
            return(plot(1:2, type='n'))   
        }
        if(nrow(df) == 0){
            return(plot(1:2, type='n')) 
        }
        
        #Corrector de variables renderizadas.
        if(is.null(Etiq)){
            Etiq <- TRUE
        }
        if(is.null(Jitter)){
            Jitter <- TRUE
        }
 
        if(BoxNum == TRUE){
            #Check de input
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
            ScatterP <- ggplot(df,aes(get(Xvar),get(Yvar),
                                      col=ZvarFac,shape = get(Wvar)))  +
                geom_point(size = 4) +
                #Etiquetas.
                xlab(Xvar) +
                ylab(Yvar) +
                scale_shape_discrete(name = Wvar) +
                scale_color_discrete(name = Zvar) +
                ggtitle("Diagrama de dispersión.") 

        }else{
            #Check de input
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
            ScatterP <- ggplot(df,aes(get(Xvar),get(Yvar),
                                      col=get(Zvar),shape = get(Wvar))) +
                geom_point(size = 4) +
                #Etiquetas.
                xlab(Xvar) +
                ylab(Yvar) +
                scale_shape_discrete(name = Wvar) +
                scale_color_discrete(name = Zvar) +
                ggtitle("Diagrama de dispersión.") 
        }
        #Jitter
        if(Jitter == TRUE){
            set.seed("1376")         #Semilla del jitter
            ScatterP <- ScatterP +
                geom_jitter(width= 0.005, height = 0.005,size = 4)
        }
        
        #Etiqueta
        if(Etiq == TRUE){
            ScatterP <- ScatterP + geom_text(aes(label = df[Wvar][,1]),
                                             vjust=1.6,color="black",size=4)
        }
        ScatterP
        
    })

    #===========================================================================
    #TAB PANEL: K-MEDIAS ///////////////////////////////////////////////////////
    #===========================================================================    
    
    #UI's RENDERIZADAS /////////////////////////////////////////////////////////
    
    #Selector del valor de K.
    #Genera Slider_K
    output$RUi_SliderK<- renderUI({
        Xvar <- input$Select_Xvar
        Yvar <- input$Select_Yvar
        df <- KReact()
        
        #Check de Input.
        if(is.null(Xvar)){
            return(NULL)    
        }
        
        #Check
        if(!is.null(df)){
            TotalU <- nrow(df[c(Xvar,Yvar)] %>% unique())
            #TotalU siempre debe ser mayor que maxi. 
            #Solo pueden coincidir cuando ambos son 1.
            if(TotalU > 1){
                maxi <- (TotalU - 1) 
            }else{
                maxi <- 1
            }
        }else{
            maxi <- 1
        }
        sliderInput("Slider_K","Número de centroides (k): ",
                    min=1,max=maxi,value= 1,step = 1)
    })
    
    #FUNCIONES REACTIVAS ///////////////////////////////////////////////////////
    
    #Genera agrupacion por K-medias.
    KmedReact <- reactive({
        #Input
        df <- KReact()
        Xvar <- input$Select_Xvar
        Yvar <- input$Select_Yvar
        NroCent <- input$Slider_K
        
        #Esperamos actualizacion.
        if(is.null(df)){
            return(NULL) 
        }
        if(is.null(Xvar)){
            return(NULL)    
        }
        if(is.null(NroCent)){
            return(NULL)    
        }
        if(is.null(NroCent)){
            NroCent <- 1
        }
        
        #Numero de valores unicos.
        nroUniques <- nrow(df[c(Xvar,Yvar)] %>% unique())
        
        #Corregimos valores.
        if(nroUniques < NroCent){
            NroCent <- 1 
        }
        
        #Si se generan NA's no ploteamos.
        if(is.na(nroUniques)){
            return(NULL)
        }
        if(nroUniques == 0){
            return(NULL)
        }
        
        #Algoritmo K-means.
        kmed <- kmeans(df[c(Xvar,Yvar)],centers = NroCent, nstart =100)				
        df$cluster <- as.factor(kmed$cluster)
        centros <- as.data.frame(kmed$centers)	
        
        #Salida de datos.
        return(list(df,centros,kmed))
    })
    
    #GRAFICO DE DISPERSION CON K-MEDIAS ////////////////////////////////////////
    
    #Genera diagrama de dispersion con agrupaciones por K-medias. 
    output$RPlot_Kmeans <- renderPlot({
        #Action Button
        input$goButtonSca
        input$goButtonKme
        input$goButtonHea

        #Evitamos el plot automatico por la asignacion inicial del numero de
        #centroides.
        if(input$goButtonSca + input$goButtonKme + input$goButtonHea == 0){
            return(NULL)
        }
        
        #Input reactiva
        df <- isolate(KmedReact()[[1]])
        centros <- isolate(KmedReact()[[2]])
        
        #Ingreso de datos
        Xvar <- isolate(input$Select_Xvar)
        Yvar <- isolate(input$Select_Yvar)
        Zvar <- isolate(input$Select_Zvar)
        BoxNum <- isolate(input$Box_Numerica)
        Jitter <- isolate(input$Box_Jitter)
        
        #Esperamos actualizacion.
        if(is.null(Xvar)){
            return(NULL) 
        }  
        if(is.null(Yvar)){
            return(NULL) 
        } 
        if(is.null(Zvar)){
            return(NULL) 
        } 
        if(is.null(df)){
            return(plot(1:2, type='n')) 
        }  
        if(is.null(centros)){
            return(plot(1:2, type='n'))
        } 
        
        #Corrector de variables renderizadas.
        if(is.null(Jitter)){
            Jitter <- TRUE
        }
        
        #Input Numerico.
        if(BoxNum == TRUE){
            ##Scatterplot con k-means.
            kplot <- ggplot(data=df,aes(get(Xvar),get(Yvar),color = cluster,
                                        shape = as.factor(ZvarFac))) + 
                geom_point(size=4) +
                geom_point(data=centros,aes(get(Xvar),get(Yvar)),
                           color = "blue",shape="X",size = 6,alpha=1/3) +
                #Etiquetas.
                xlab(Xvar) +
                ylab(Yvar) +
                scale_shape_discrete(name = Zvar) +
                ggtitle("Gráfico con agrupación por K-medias")
        }else{
            ##Scatterplot con k-means.
            kplot <- ggplot(data=df,aes(get(Xvar),get(Yvar),color = cluster,
                                        shape = get(Zvar))) +
                geom_point(size = 4) +
                geom_point(data=centros,aes(get(Xvar),get(Yvar)),
                           color = "blue",shape="X",size = 6,alpha=1/3) +
                #Etiquetas.
                xlab(Xvar) +
                ylab(Yvar) +
                scale_shape_discrete(name = Zvar) +
                ggtitle("Gráfico con agrupación por K-medias")
        }
        
        #Jitter
        if(Jitter == TRUE){
            set.seed("1376")         #Semilla del jitter
            kplot <- kplot + geom_jitter(width= 0.005, height = 0.005,size = 4)
        }
        kplot
        
    }) 
    
    #-------------------------------------
    #Tabla de frecuencias por agrupaciones.
    output$RTable_FrecK <- renderTable({
        #Inputs
        df   <- KmedReact()[[1]]
        Zvar <- input$Select_Zvar
        List_Zval <- input$SelectList_Zval3
        
        #Esperamos actualizacion.
        if(is.null(df)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(List_Zval)){
            return(NULL)
        }
        
        #Creamos tabla de clusters
        if(input$Box_Numerica == TRUE){
            tabla <- as.data.frame(table(df$cluster,df["ZvarFac"][,1]))
        }else{
            tabla <- as.data.frame(table(df$cluster,df[Zvar][,1]))
        }
        
        #subsetting por Zval.
        tabla <- tabla[tabla[,2] %in% List_Zval,]
        tabla <- spread(tabla,key = Var2,value = Freq)
        names(tabla)[1] <- "#Clúster"
        tabla
        
    })

    #===========================================================================
    #TAB PANEL: MAPA DE CALOR //////////////////////////////////////////////////
    #===========================================================================   
  
    #Genera mapa de calor para la agreupacion por k-medias realizada.
    output$RPlot_HeatM <- renderPlot({
        #Action Button
        input$goButtonSca
        input$goButtonKme
        input$goButtonHea
        
        #Evitamos el plot automatico por la asignacion inicial del numero de
        #centroides.
        if(input$goButtonSca + input$goButtonKme + input$goButtonHea == 0){
            return(NULL)
        }
        
        #Input reactive
        kmed  <- isolate(KmedReact()[[3]])
        df <- isolate(KmedReact()[[1]])
        centros <- isolate(KmedReact()[[2]])
        
        
        #Ingreso de datos
        Xvar <- isolate(input$Select_Xvar)
        Yvar <- isolate(input$Select_Yvar)
        Zvar <- isolate(input$Select_Zvar)
        BoxNum <- isolate(input$Box_Numerica)
        
        #Esperamos actualizacion.
        if(is.null(Xvar)){
            return(NULL)   
        }
        if(is.null(Yvar)){
            return(NULL)   
        }
        if(is.null(Zvar)){
            return(NULL)   
        }
        if(is.null(kmed)){
            return(plot(1:2, type='n'))   
        }
        if(is.null(df)){
            return(plot(1:2, type='n'))   
        }
        if(is.null(centros)){
            return(plot(1:2, type='n'))   
        }
        
        ##Creando los datos del heatmap.	
        dfMat <- as.matrix(df[c(Xvar,Yvar)])					
        row.names(dfMat) <- 1:nrow(dfMat)				
        
        #Si no hay datos no ploteamos.
        if(nrow(dfMat) == 0){
            return(plot(1:2, type='n'))
        }
        
        #Normalizamos datos para apreciar colores.
        if(nrow(dfMat) > 1){
            dfMat[,1] <- (dfMat[,1] - min(dfMat[,1]))/
                            (max(dfMat[,1]) - min(dfMat[,1]))
            dfMat[,2] <- (dfMat[,2] - min(dfMat[,2]))/
                            (max(dfMat[,2]) - min(dfMat[,2]))
        }else{
            dfMat[,1] <- (dfMat[,1] - min(dfMat[,1]))
            dfMat[,2] <- (dfMat[,2] - min(dfMat[,2]))
        }
        
        #Agrupacion.
        grouped <- t(dfMat)[,order(kmed$cluster)]
        
        #Solo ploteamos matrices.
        if(class(grouped) != "matrix"){
            return(plot(1:2, type='n'))
        }
        
        #Construimos variables auxiliares
        coorXg <- seq(0,1,length.out=nrow(grouped))
        coorYg <- seq(0,1,length.out=ncol(grouped))

        #Heatmap.
        image(t(dfMat)[,order(kmed$cluster)],xaxt= "n",yaxt= "n")	
        axis(1, at = coorXg,labels = rownames(grouped),las=2)
        
        #Etiquetamos en funcion del tipo de variable explicada.
        if(BoxNum == TRUE){
            #Check de input
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
            text(rep(coorXg[1],length(coorYg)),coorYg,
                             labels = df["ZvarFac"][order(kmed$cluster),1])				
            text(rep(coorXg[2],length(coorYg)),coorYg,
                             labels = df["ZvarFac"][order(kmed$cluster),1])
        }else{
            #Check de input
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
            text(rep(coorXg[1],length(coorYg)),coorYg,
                             labels = df[Zvar][order(kmed$cluster),1])				
            text(rep(coorXg[2],length(coorYg)),coorYg,
                             labels = df[Zvar][order(kmed$cluster),1])
            
        }
        #Titulo del heatmap.
        title("Agrupación Generada en Mapa de Calor")	
        
    })
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: Dispersion V1 vs Idx /////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #UI's RENDERIZADAS /////////////////////////////////////////////////////////
    
    #Selector de leyenda1.
    #Genera Box_EGD.V1vsIdx.Leg
    output$RUi_EGD.V1vsIdx.Leg <- renderUI({
        checkboxInput("Box_EGD.V1vsIdx.Leg","Mostrar Leyenda.",value=F)
    })
    
    
    #GRAFICO DE DISPERSION /////////////////////////////////////////////////////
    
    #Diagrama de dispersion de la variable 1 vs Index. Coloreado por Zvar.
    output$RPlot_EGD.V1vsIdx <- renderPlot({
        #Action Button
        input$goButtonV1vsIdx
        
        #Detener ploteo automatico
        if(input$goButtonV1vsIdx == 0){
            return(NULL)
        }
        
        #Inputs
        leyenda <- isolate(input$Box_EGD.V1vsIdx.Leg)
        df <- isolate(ZvarFacReact())
        Yvar <- isolate(input$Select_Yvar)
        Zvar <- isolate(input$Select_Zvar)
        BoxNum <- isolate(input$Box_Numerica)
        
        #Esperamos Actualizacion.
        if(is.null(df)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(Yvar)){
            return(NULL)
        }
        
        #Corregimos leyenda
        if(is.null(leyenda)){
            leyenda <- FALSE
        }
        
        #Coloreamos en funcion de numeric
        if(BoxNum == TRUE){
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
            #Subsetting de df para el plot.
            df <- df[c("ZvarFac",Yvar)]
            df$Index <- 1:nrow(df)
   
            #Configuramos plot.
            ScatPlot <- ggplot(df,aes(Index,get(Yvar),col=ZvarFac)) +	 			
                geom_point(size = 4) +
                ylab(Yvar) + 
                xlab("Index") +
                scale_color_discrete(name = Zvar) +
                ggtitle(paste0("Variable ",Yvar," vs Index. Coloreando por ",
                               Zvar,"."))
            
        }else{
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
            #Subsetting de df para el plot.
            df <- df[c(Zvar,Yvar)]
            df$Index <- 1:nrow(df)
            
            #Configuramos plot.
            ScatPlot <- ggplot(df,aes(Index,y=get(Yvar),col=get(Zvar))) +	 			
                geom_point(size = 4) +
                ylab(Yvar) + 
                xlab("Index") +
                scale_color_discrete(name = Zvar) +
                ggtitle(paste0("Variable ",Yvar," vs Index. Coloreando por ",
                               Zvar,"."))
        }
        
        #Leyenda
        if(leyenda == TRUE){
            ScatPlot 
        }else{
            ScatPlot + theme(legend.position="none")
        }	
        
    })
   
    #///////////////////////////////////////////////////////////////////////////
    #TAB: Dispersion V2 vs Idx /////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #UI's RENDERIZADAS /////////////////////////////////////////////////////////
    
    #Selector de leyenda1.
    #Genera Box_EGD.V2vsIdx.Leg
    output$RUi_EGD.V2vsIdx.Leg <- renderUI({
        checkboxInput("Box_EGD.V2vsIdx.Leg","Mostrar Leyenda.",value=F)
    })
    
    #GRAFICO DE DISPERSION /////////////////////////////////////////////////////
    
    #Diagrama de dispersion de la variable 2 vs Index. Coloreado por Zvar.
    output$RPlot_EGD.V2vsIdx <- renderPlot({
        #Action Button
        input$goButtonV2vsIdx
        
        #Detener ploteo automatico
        if(input$goButtonV2vsIdx == 0){
            return(NULL)
        }
        
        #Inputs
        leyenda <- isolate(input$Box_EGD.V2vsIdx.Leg)
        df <- isolate(ZvarFacReact())
        Yvar <- isolate(input$Select_Xvar)
        Zvar <- isolate(input$Select_Zvar)
        BoxNum <- isolate(input$Box_Numerica)
        
        #Esperamos Actualizacion.
        if(is.null(df)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(Yvar)){
            return(NULL)
        }
        
        #Corregimos leyenda
        if(is.null(leyenda)){
            leyenda <- FALSE
        }
        
        #Coloreamos en funcion de numeric
        if(BoxNum == TRUE){
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
            #Subsetting de df para el plot.
            df <- df[c("ZvarFac",Yvar)]
            df$Index <- 1:nrow(df)
            
            #Configuramos plot.
            ScatPlot <- ggplot(df,aes(Index,get(Yvar),col=ZvarFac)) +	 			
                geom_point(size = 4) +
                ylab(Yvar) + 
                xlab("Index") +
                scale_color_discrete(name = Zvar) +
                ggtitle(paste0("Variable ",Yvar," vs Index. Coloreando por ",
                               Zvar,"."))
            
        }else{
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
            #Subsetting de df para el plot.
            df <- df[c(Zvar,Yvar)]
            df$Index <- 1:nrow(df)
            
            #Configuramos plot.
            ScatPlot <- ggplot(df,aes(Index,y=get(Yvar),col=get(Zvar))) +	 			
                geom_point(size = 4) +
                ylab(Yvar) + 
                xlab("Index") +
                scale_color_discrete(name = Zvar) +
                ggtitle(paste0("Variable ",Yvar," vs Index. Coloreando por ",
                               Zvar,"."))
        }
        
        #Leyenda
        if(leyenda == TRUE){
            ScatPlot 
        }else{
            ScatPlot + theme(legend.position="none")
        }	
        
    })
    
    #===========================================================================
    #TAB PANEL: DENDROGRAMA ////////////////////////////////////////////////////
    #===========================================================================
    
    #UI's RENDERIZADAS /////////////////////////////////////////////////////////
    
    #Selector de visualizacion de leyenda.
    #Genera Box_Legend
    output$RUi_checkboxLegend<- renderUI({
        checkboxInput("Box_Legend","Mostrar Leyenda.",value=F)
    })
    
    #--------------------------------------------
    #Selector de la variable X.
    #Genera Box_VarX
    output$RUi_checkboxVarX<- renderUI({
        #Check de Input.
        if(is.null(input$Select_Xvar)){
            return(NULL)    
        }
        checkboxInput("Box_VarX",paste0("Clusterizar incluyendo la variable ",
                                        input$Select_Xvar),value=T)
    })
    
    #--------------------------------------------
    #Selector de la variable Y.
    #Genera Box_VarY
    output$RUi_checkboxVarY<- renderUI({
        checkboxInput("Box_VarY",paste0("Clusterizar incluyendo la variable ",
                                        input$Select_Yvar),value=T)
    })
    
    #GRAFICO DENDROGRAMA ///////////////////////////////////////////////////////
    
    #Genera dendrograma de clusterizacion jerarquica. 
    output$RPlot_Hclust <- renderPlot({
        
        #Ingreso de inputs
        df <- ZvarFacReact()
        Xvar <- input$Select_Xvar
        Yvar <- input$Select_Yvar
        Zvar <- input$Select_Zvar
        BoxX <- input$Box_VarX
        BoxY <- input$Box_VarY
        legend <- input$Box_Legend
        BoxNum <- input$Box_Numerica

        #Esperamos Actualizacion.
        if(is.null(Xvar)){
            return(NULL)
        }
        if(is.null(Yvar)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(df)){
            return(NULL)
        }

        #Chequeamos input y extraemos variable dependiente.
        if(BoxNum == TRUE){
            #Esperamos actualizacion
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
            varDep <- df$ZvarFac
        }else{
            #Esperamos actualizacion
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
            varDep <- df[Zvar][,1]
        }
        
        #Corrector de Inputs.
        if(is.null(BoxX)){
            BoxX <- TRUE
        }
        if(is.null(BoxY)){
            BoxY <- TRUE
        }
        if(is.null(legend)){
            legend <- TRUE
        }
        
        #Subsetting especial para hclust. Existe un bug en potencia. 
        if(BoxX == T & BoxY == T){
            vecCol <- which(names(df) %in% c(Xvar,Yvar))
            nombres <- names(df)[vecCol]
            df <- df[,vecCol]
            plotear <- TRUE
            
        }else if(BoxX == T){
            vecCol <- which(names(df) %in% c(Xvar))
            nombres <- names(df)[vecCol]
            df <- df[,vecCol]
            plotear <- TRUE
            
        }else if(BoxY == T){
            vecCol <- which(names(df) %in% c(Yvar))
            nombres <- names(df)[vecCol]
            df <- df[,vecCol]
            plotear <- TRUE
            
        }else{
            return(NULL)
        }
        
        #Filtro de df1
        if(class(df) != "data frame"){
            df <- as.data.frame(df)
            names(df) <- nombres
        }
        
        #Agregamos variable explicada
        df$varDep <- varDep
        
        #Eliminamos repetidos y faltantes
        df <- df %>% unique()
        df <- df[complete.cases(df),]
        
        #Chequeo del df
        if(nrow(df) < 2){
            plotear <- FALSE
        }
        
        #Chequeo de condiciones para plotear.
        if(plotear == FALSE){
            return(plot(1:2, type='n'))
        }
        
        #Clusterizacion jerarquica.
        d <- dist(df[!(names(df)  %in% "varDep")])											
        cluster <- hclust(d)
        
        #Corrector del Bug.
        cluster$labels <- as.character(1:nrow(df))
        
        #Dendrograma
        myplclust(cluster,lab.col = unclass(df$varDep), main = 
                      paste0("Clusterización de la Variable ",Zvar))
        #Agregamos leyenda.
        if(legend == TRUE){
            legend("topright",legend = unique(df$varDep),
                   col = unique(df$varDep),pch=19,title = Zvar)
        }

    })
    
    ############################################################################
    ###############////////////////////////////////////////////////#############
    #//////////////////////------------------------------///////////////////////
    #______________________         CLASIFICACION        ______________________#
    #//////////////////////------------------------------///////////////////////
    ###############////////////////////////////////////////////////#############
    ############################################################################
    
    
    #===========================================================================
    # SIDEBAR PANEL RENDERIZADO ////////////////////////////////////////////////
    #===========================================================================
    
    #---------------------------------------------------------------------------
    #INPUT: VARIABLE A CLASIFICAR (Zvar). 
    output$RUi_Select_ZvarClas <- renderUI({
        #Input
        BoxNum <- input$Box_Numerica2
        
        #Genera Select_ZvarClas
        if(BoxNum == FALSE){
            variables <-FactorVars2
            selected <- variables[1]
        }else{
            variables <- NumericVars2
            selected <- variables[1]
        }
        selectInput("Select_ZvarClas", "Variable a clasificar:",
                    choices  = variables,
                    selected = selected
        )
    })
    
    #INPUT: VARIABLEs CUALITATIVAS CLASIFICADORAS. 
    output$RUi_Select_ClasCV <- renderUI({
        #Input
        ZvarClas <- input$Select_ZvarClas
        BoxNum <- input$Box_Numerica2
        factores <- FactorVars2
        
        #Genera Select_ZvarClas
        if(BoxNum == FALSE){
            #Check Input
            if(is.null(ZvarClas)){
                return(NULL)
            }
            factores <- factores[which(factores != ZvarClas)]
            checkboxGroupInput("Select_ClasCV",
                               "Variables categóricas explicativas:",
                               choices  = factores
                               ,
                               selected = factores
            )
        }else{
            checkboxGroupInput("Select_ClasCV",
                               "Variables categóricas explicativas:",
                               choices  = factores
                               ,
                               selected = factores
            )
        }
    })
    
    #===========================================================================
    # FUNCIONES REACTIVAS //////////////////////////////////////////////////////
    #===========================================================================
    
    #Genera la tabla de datos adecuada en funcion de los inputs de usuario.
    #Extrae las variables requeridas para la exploracion basica y hace los 
    #subsetting correspondientes.
    dfReactClas <- reactive({
        
        withProgress(message = 'Creando Tabla Renderizada', value = 0, {
            
            #==========
            incProgress(1/3)
            #==========
            
            #Subsetting de variables.
            df <- DataSet2
            
            #==========
            incProgress(1/3)
            #==========
            
            #Eliminamos repetidos
            df <- df %>% unique()
            
            #==========
            incProgress(1/3)
            #==========
            
            #Si no tiene datos salimos.
            if(nrow(df) == 0){
                return(NULL)
            }
          
            #Salida
            return(df)
            
        })
    })
    
    #---------------------------------------------------------------------------
    #Tabla Renderizada que crea la variable ZvarFac y la agrega al data frame 
    #df que genera dfReact().
    #Esta nueva variable es la version factor de Zvar.
    ZvarFacReactClas <- reactive({
        #Inputs
        df <- dfReactClas()
        Zvar <- input$Select_ZvarClas
        BoxNum <- input$Box_Numerica2
        
        #Esperamos actualizacion.    
        if(is.null(df)){
            return(NULL)   
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Variable categorica.
        if(BoxNum == TRUE){
            
            #Factorizamos variable continua.
            valores <- suppressWarnings(
                as.numeric(as.character(df[Zvar][,1])))
            
            #Esperamos actualizacion.
            if(all(is.na(valores))){
                return(NULL)
            }
            
            #Check de clase.
            if(!(class(valores) %in% c("numeric","integer"))){
                stop("Los valores de la variable de respuesta no son numericos")
            }
            
            #Revisamos si la variable a clasificar es de Google Earth
            #Las de Google Earth tienen valores faltantes escritos como 999.
            check <- strsplit(Zvar,split="\\.")[[1]][1]
            
            if(check %in% c("Kms","Dias")){
                posFalt <- which(valores == 999)
                posVal  <- which(valores != 999)
                #Retiramos los 999.
                valores <- valores[-posFalt]
                recorte <- TRUE
            }else{
                recorte <- FALSE
            }
            
            ##Retornamos NULL si nos quedamos sin valores.
            if(length(valores) == 0){
                return(NULL)
            }
            
            #Construimos una particion para Zvar.
            if(sd(valores) == 0){
                cutpoints <- quantile(c(min(valores) - 0.1,valores),
                                      seq(0,1,length=2),na.rm=TRUE)	
            }else{
                #Buscamos el tamano adecuado.
                nroIni <- 8
                cutpoints <- quantile(c(min(valores) - 0.1,valores),
                                      seq(0,1,length=nroIni),na.rm=TRUE)	
                #Ciclo de busqueda
                while(any(diff(cutpoints) == 0) & nroIni > 1){
                    nroIni <- nroIni -1
                    cutpoints <- quantile(c(min(valores) - 0.1,valores),
                                          seq(0,1,length=nroIni),na.rm=TRUE)	 
                }
            }
            
            #Creamos factor.
            if(recorte == FALSE){
                df$ZvarFac <- cut(valores,cutpoints)
            }else{
                df$ZvarFac <- rep("Sin Datos",times = nrow(df))
                df$ZvarFac[posVal]  <- as.character(cut(valores,cutpoints))
                df$ZvarFac <- as.factor(df$ZvarFac)
            }
        }
        #Exportamos dataset.
        return(df)
    })
    
    #===========================================================================
    #TAB PANEL: TABLA DE DATOS _________________________________________________
    #===========================================================================
    
    #Tabla de datos
    output$RDT_TablaDatosClas <- DT::renderDataTable(
        DT::datatable({
            #Chequeamos Input.
            if(is.null(dfReactClas())){
                return(NULL)
            }
            dfReactClas() 
        })
    )
    
    #===========================================================================
    #TAB PANEL: SINGULAR VALUE DESCOMPOSITION //////////////////////////////////
    #===========================================================================
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: SVD Izq //////////////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #UI's RENDERIZADAS /////////////////////////////////////////////////////////
    
    #Valores de la variable dependiente a usar en el SVD.
    #Genera SelectList_ZvalSVD
    output$RUi_checkboxGroupSVD.Zvar <- renderUI({
        #Input
        df <- ZvarFacReactClas()
        Zvar <- input$Select_ZvarClas
        
        #Esperamos Actualizacion.
        if(is.null(df)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Se habilita cuando la variable ingresada es factor.
        if(input$Box_Numerica2 == FALSE){
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
            valores <- as.character(unique(df[Zvar][,1]))
        }else{
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
            valores <- as.character(unique(df["ZvarFac"][,1]))
        }
        
        #Render UI.
        checkboxGroupInput("SelectList_ZvalSVD",
                           "Selector de valores.",
                           choices  = valores
                           ,
                           selected = valores
        )
    })
    
    
    #--------------------------------------------
    #Selector de leyenda.
    #Genera Box_LegendClas
    output$RUi_checkboxLegendClas<- renderUI({
        checkboxInput("Box_LegendClas","Leyenda.",value=F)
    })
    
    
    #--------------------------------------------
    #Selector del autovector de izquierda 1.
    #Genera Select_autoVec1
    output$RUi_autoVec1 <- renderUI({
        #Action Button
        #input$goButtonCla
        
        #Input
        #svd1 <- isolate(SReact()[[2]])
        svd1 <- SReact()[[2]]
        #Check de entrada.
        if(is.null(svd1)){
            return(NULL)
        }
        #Numero maximo de autovewctores
        maximo <- dim(svd1$u)[2]
        
        numericInput("Select_autoVec1","Autovector #1",
                     min = 1,max = maximo,value = 1,step = 1)
    })
    
    
    #--------------------------------------------
    #Selector del autovector de izquierda 2.
    #Genera Select_autoVec2
    output$RUi_autoVec2 <- renderUI({
        #Action Button
        
        #Input
        svd1 <- SReact()[[2]]
        #Check de entrada.
        if(is.null(svd1)){
            return(NULL)
        }
        #Numero maximo de autovewctores
        maximo <- dim(svd1$u)[2]
        
        numericInput("Select_autoVec2","Autovector #2",
                     min = 1,max = maximo,value = 2,step = 1)
    })
    
    
    #--------------------------------------------
    #Contador del numero de variables explicativas en uso.
    output$RText_varEnUso <- renderText({
        #Action Button
        input$goButtonCla
        
        varEnUso <- isolate(SReact()[[3]])
        paste0(length(varEnUso)," variables explicativas en uso.")
    })    
    
    #FUNCIONES REACTIVAS ///////////////////////////////////////////////////////
    
    #Recibe la tabla arrojada por ZvarFacReactClas y le hace subsetting por 
    #SelectList_ZvalSVD. Elimina las variables sin desviacion, prepara los datos
    #para el SVD y lo realiza. Retornamos la variable dependiente.
    #Retorna el conjunto de datos utilizados, el sdv y el numero de variables
    #explicativas empleadas en el procedimiento.
    SReact <- reactive({
        #Inputs
        df <- ZvarFacReactClas()
        Zvar <- input$Select_ZvarClas
        BoxNum <- input$Box_Numerica2
        
        #recibimos inputs de los UI's renderizados.
        List_Zval <- input$SelectList_ZvalSVD

        #Esperamos actualizacion.    
        if(is.null(df)){
            return(NULL)   
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(List_Zval)){
            return(NULL)   
        }

        #Variable categorica.
        if(BoxNum == TRUE){
            
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
            
            #Subsetting por List_Zval.
            #Si la variable contiene datos de Google Earth, reemplazamos
            #el valor "sin Datos" por "(998,1000]"
            if("Sin Datos" %in% List_Zval){
                List_Zval <- as.character(List_Zval)
                List_Zval[which(List_Zval == "Sin Datos")] <- "(998,1000]"
            }
            vect <- c()
            for (i in 1:length(List_Zval)){
                cadena <- strsplit(List_Zval[i],split=",")[[1]]
                mini <- as.numeric(as.character(sub("\\(","",cadena[1])))
                maxi <- as.numeric(as.character(sub("]","",cadena[2])))
                vect <- c(vect,df[Zvar][
                    df[Zvar][,1] > mini & df[Zvar][,1] <= maxi,1])
            }
            df <- df[df[Zvar][,1] %in% vect,]
            
            #Retornamos NULL si nos quedamos sin observaciones.
            if(nrow(df) == 0){
                return(NULL)
            }
            #Recuperamos la variable dependiente.
            varDep <- as.factor(df["ZvarFac"][,1])
            
            #Posiciones de la variable explicada (Zvar y ZvarFac).
            factorVars <- which(names(df) %in% FactorVars2)
            pos <- which(names(df) == Zvar)
            pos2 <- which(names(df) == "ZvarFac")
            
            #Cortamos el dataFrame. Nos quedamos con el conjunto de predictores.
            df <- df[,-c(factorVars,pos,pos2)]
            
            #Eliminamos las variables que tengan una sd() igual a 0.
            varConDes <- which(sapply(df,function(x){
                sd(x) != 0
            }))
            df <- df[,varConDes]
            varEnUso <- names(df)
            
            #Check del SVD.
            if(is.null(dim(df))){
                return(NULL)
            }else{
                if(0 %in% dim(df)){
                    return(NULL)
                }
            }
   
            #SVD
            svd1 <- svd(scale(df))
            
        }else{
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
            #Subsetting por List_Zval.
            df <- df[df[Zvar][,1]  %in% List_Zval,]
            
            #Retornamos NULL si nos quedamos sin observaciones.
            if(nrow(df) == 0){
                return(NULL)
            }
            #Recuperamos la variable dependiente.
            varDep <- as.factor(df[Zvar][,1])
            
            #Cortamos el dataFrame. Nos quedamos con el conjunto de predictores.
            factorVars <- which(names(df) %in% FactorVars2)
            df <- df[,-c(factorVars)]
            
            #Eliminamos las variables que tengan una sd() igual a 0.
            varConDes <- which(sapply(df,function(x){
                sd(x) != 0
            }))
            df <- df[,varConDes]
            varEnUso <- names(df)
            
            #Check del SVD.
            if(is.null(dim(df))){
                return(NULL)
            }else{
                if(0 %in% dim(df)){
                    return(NULL)
                }
            }
            
            #SVD
            svd1 <- svd(scale(df))
        }
        return(list(varDep,svd1,varEnUso))
        
    })

    #GRAFICO DE SINGULAR VALUE DESCOMPOSITION (IZQ)/////////////////////////////
    
    #Realizamos el grafico del SVD con las opciones solicitadas por el usuario.
    #Autovectores de Izquierda.
    output$RPlot_SVDizq <- renderPlot({
        #Action Button
        input$goButtonCla
        
        #Inputs
        aV1  <- isolate(input$Select_autoVec1)
        aV2  <- isolate(input$Select_autoVec2)
        Zvar <- isolate(input$Select_ZvarClas)
        leyenda <- isolate(input$Box_LegendClas)
        varDep   <- isolate(SReact()[[1]])            #Variable dependiente.
        svd1 <- isolate(SReact()[[2]])
        
        #Esperamos actualizacion.
        if(is.null(Zvar)){
            return(NULL)	
        }
        if(is.null(varDep)){
            return(plot(1:2, type='n'))	
        }
        if(is.null(svd1)){
            return(plot(1:2, type='n'))	
        }
        
        #Corregimos input.
        if(is.null(aV1)){
            aV1 <- 1
        }
        if(is.null(aV2)){
            aV2 <- 2
        }
        if(is.null(leyenda)){
            leyenda <- FALSE
        }
        
        #Headers
        autovect1 <- paste0("1.Autovector de Izquierda #",aV1)
        autovect2 <- paste0("2.Autovector de Izquierda #",aV2)
        if(aV1 == aV2){
            autovect2 <- autovect1
        }
        #Dataframe para el plot.
        dataPlot1 <- data.frame(Yvar=svd1$u[,aV1],Zvar = varDep)
        dataPlot1$ID <- rep(autovect1,times = nrow(dataPlot1))
        dataPlot1$Index <- 1:nrow(dataPlot1)
        
        dataPlot2 <- data.frame(Yvar=svd1$u[,aV2],Zvar = varDep)
        dataPlot2$ID <- rep(autovect2,times = nrow(dataPlot2))
        dataPlot2$Index <- 1:nrow(dataPlot2)

        #Creamos data frame de ploteo.
        dataPlot <- rbind(dataPlot1,dataPlot2)
        
        #Formateamos.
        dataPlot$ID <- as.factor(dataPlot$ID)

        #Configuramos plot.
        ScatPlot <- ggplot(dataPlot,aes(Index,Yvar,color = Zvar)) + 	 			
            geom_point(size = 4) + facet_grid(.~ID) +
            ylab("Correlación entre la observación y el Autovector") + 
            xlab("Observaciones") +
            scale_color_discrete(name = Zvar) +
            ggtitle(paste0(
                "Autovectores de Izquierda vs Index. Variable: ",
                Zvar))
        
        #Leyenda
        if(leyenda == TRUE){
            ScatPlot 
        }else{
            ScatPlot + theme(legend.position="none")
        }
    })
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: SVD Der //////////////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #FUNCIONES REACTIVAS ///////////////////////////////////////////////////////
    
    #Genera el data frame de ploteo. Util para los graficos de dispersion de 
    #ggplot y para la tabla que se genera con el brush.
    dataPlotReact <- reactive({
        #Inputs
        aV1  <- input$Select_autoVec1
        aV2  <- input$Select_autoVec2
        svd1 <- SReact()[[2]]
        
        #Esperamos actualizacion.
        if(is.null(svd1)){
            return(NULL)
        }
        
        #Corregimos input.
        if(is.null(aV1)){
            aV1 <- 1
        }
        if(is.null(aV2)){
            aV2 <- 2
        }
        
        #Headers
        autovect1 <- paste0("1.Autovector de Derecha #",aV1)
        autovect2 <- paste0("2.Autovector de Derecha #",aV2)
        
        #Si los autovectores son iguales creamos un solo grafico.
        if(aV1 == aV2){
            autovect2 <- autovect1
        }
        #Dataframe para el plot.
        dataPlot1 <- data.frame(Yvar=svd1$v[,aV1])
        dataPlot1$ID <- rep(autovect1,times = nrow(dataPlot1))
        dataPlot1$Index <- 1:nrow(dataPlot1)
        
        dataPlot2 <- data.frame(Yvar=svd1$v[,aV2])
        dataPlot2$ID <- rep(autovect2,times = nrow(dataPlot2))
        dataPlot2$Index <- 1:nrow(dataPlot2)
        
        #Creamos data frame de ploteo.
        dataPlot <- rbind(dataPlot1,dataPlot2)
        
        #Formateamos.
        dataPlot$ID <- as.factor(dataPlot$ID)
        
        #Retorna dataPlot.
        return(dataPlot)	
        
    })
    
    #Realiza la extraccion de los datos del plot cuando se realiza brush.
    BrushSVDderReact <- reactive({
        #Inputs
        dataPlot <- isolate(dataPlotReact())
        brush <- input$Brush
        
        #Esperamos Actualizacion
        if(is.null(dataPlot)){
            return(NULL)
        }
        #Impedimos el Brush antes de plotear.
        if(isolate(input$goButtonCla) == 0){
            return(NULL)
        }
        
        #Generamos tabla del brush.
        B_data <-brushedPoints(dataPlot,brush)
        
        #Selecionar al menos un punto.
        if(nrow(B_data) < 1){
            return(NULL)
        }
        return(B_data)
    })
    
    #GRAFICO DE SINGULAR VALUE DESCOMPOSITION (DER)/////////////////////////////
    
    #Realizamos el grafico del SVD con las opciones solicitadas por el usuario.
    #Autovectores de Derecha.
    output$RPlot_SVDder <- renderPlot({
        #Action Button
        input$goButtonCla
        
        #Ploteo extrano
        if(input$goButtonCla == 0) return(NULL)
        
        #Inputs
        leyenda <- isolate(input$Box_LegendClas)
        dataPlot <- isolate(dataPlotReact())
        Zvar <- isolate(input$Select_ZvarClas)
        
        #Esperamos Actualizacion.
        if(is.null(dataPlot)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Corregimos leyenda
        if(is.null(leyenda)){
            leyenda <- FALSE
        }
        
        #Configuramos plot.
        ScatPlot <- ggplot(dataPlot,aes(Index,Yvar)) +	 			
            geom_point(size = 4) + facet_grid(.~ID) +
            ylab("Correlación entre la variable y el Autovector") + 
            xlab("Variables explicativas") +
            ggtitle(paste0("Autovectores de Derecha vs Index. ",
                "Variables con mayor influencia en el agrupamiento de ",Zvar))
        
        #Leyenda
        if(leyenda == TRUE){
            ScatPlot 
        }else{
            ScatPlot + theme(legend.position="none")
        }	
        
    })
    
    #----------------------------------------------------------
    #Tabla de maximos contribuyentes para el primer autovector.
    output$RTable_MaxCont1 <- renderTable({
        #Action Button
        input$goButtonCla
        
        #Inputs
        aV1  <- isolate(input$Select_autoVec1)
        svd1 <- isolate(SReact()[[2]])
        varEnUso <- isolate(SReact()[[3]])
        infPositiva <- input$Box_InfPositiva
        infNegativa <- input$Box_InfNegativa
        
        #Esperamos actualizacion.
        if(is.null(svd1)){
            return(NULL)	
        }
        if(is.null(varEnUso)){
            return(NULL)	
        }
        if(length(varEnUso) == 0){
            return(NULL)	
        }
        if(infPositiva + infNegativa == 0){
            return(NULL)	
        }
        #Corregimos input.
        if(is.null(aV1)){
            aV1 <- 1
        }
        
        #Ordenamos los nombres de las variables en funcion de su contribucion al 
        #autovector de derecha.
        if(infPositiva + infNegativa == 2){   
            #Influencia positiva o negativa.
            maxCon_aV1 <- varEnUso[order(abs(svd1$v[,aV1]),decreasing = T)]
        }else{
            if(infPositiva == TRUE){ 
                #Solo influencia positiva.
                maxCon_aV1 <- varEnUso[order(svd1$v[,aV1],decreasing = T)]
            }else{         
                #Solo influencia negativa.
                maxCon_aV1 <- varEnUso[order(svd1$v[,aV1],decreasing = F)]
            }
        }

        #Mostramos las primeras 10 variables mas influyentes.
        max <- length(maxCon_aV1)
        if(max > 10){
            max <- 10
        }

        #Creamos tabla de contribuyentes.
        tabla <- data.frame(I1 = maxCon_aV1[1:max])
        names(tabla) <- paste0("Autovector #",aV1)

        #Salida de tabla.
        return(tabla)
    })
    
    #-----------------------------------------------------------
    #Tabla de maximos contribuyentes para el segundo autovector.
    output$RTable_MaxCont2 <- renderTable({
        #Action Button
        input$goButtonCla
        
        #Inputs
        aV1  <- isolate(input$Select_autoVec1)
        aV2  <- isolate(input$Select_autoVec2)
        svd1 <- isolate(SReact()[[2]])
        varEnUso <- isolate(SReact()[[3]])
        infPositiva <- input$Box_InfPositiva
        infNegativa <- input$Box_InfNegativa
        
        #Esperamos actualizacion.
        if(is.null(svd1)){
            return(NULL)	
        }
        if(is.null(varEnUso)){
            return(NULL)	
        }
        if(length(varEnUso) == 0){
            return(NULL)	
        }
        if(infPositiva + infNegativa == 0){
            return(NULL)	
        }

        #Corregimos input.
        if(is.null(aV1)){
            aV1 <- 1
        }
        if(is.null(aV2)){
            aV2 <- 2
        }
        
        #Si los autovectores son iguales escribimos una sola tabla.
        if(aV1 == aV2){
            return(NULL)
        }
        #Ordenamos los nombres de las variables en funcion de su contribucion al 
        #autovector de derecha.
        if(infPositiva + infNegativa == 2){   
            #Influencia positiva o negativa.
            maxCon_aV2 <- varEnUso[order(abs(svd1$v[,aV2]),decreasing = T)]
        }else{
            if(infPositiva == TRUE){ 
                #Solo influencia positiva.
                maxCon_aV2 <- varEnUso[order(svd1$v[,aV2],decreasing = T)]
            }else{         
                #Solo influencia negativa.
                maxCon_aV2 <- varEnUso[order(svd1$v[,aV2],decreasing = F)]
            }
        }
        
        #Mostramos las primeras 10 variables mas influyentes.
        max <- length(maxCon_aV2)
        if(max > 10){
            max <- 10
        }
        
        #Creamos tabla de contribuyentes.
        tabla <- data.frame(I1 = maxCon_aV2[1:max])
        names(tabla) <- paste0("Autovector #",aV2)
        
        #Salida de tabla.
        return(tabla)
    })
    
    #-------------------------------------
    #Tabla de variables seleccionadas con el brush.
    output$RTable_Brushed <- renderTable({
        B_data <- BrushSVDderReact() 
        varEnUso <- isolate(SReact()[[3]])
        
        #Check de inputs
        if(is.null(B_data)){
            return(NULL)
        }
        if(is.null(varEnUso)){
            return(NULL)
        }
        
        #Creamos tabla de nombres
        tabla <- data.frame(vs = varEnUso[B_data$Index])
        names(tabla) <- "Variables Seleccionadas"
        return(tabla)
        
    })
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: Dispersion1 //////////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #UI's RENDERIZADAS /////////////////////////////////////////////////////////
    
    #--------------------------------------------
    #Selector de la variable explicativa.
    #Genera Select_varDis1
    output$RUi_varDis1 <- renderUI({
        #input
        Zvar <- input$Select_ZvarClas
        
        #Esperamos Actualizacion.
        if(is.null(Zvar)){
            return(NULL)
        }
         
        #Genera Select_varDis1
        if(input$Box_Numerica2 == TRUE){
            variables <- NumericVars2[NumericVars2 != Zvar]
            selected <- variables[1]
        }else{
            variables <- NumericVars2
            selected <- variables[1]
        }
        selectInput("Select_varDis1", "Variable de exploración:",
                    choices  = variables,
                    selected = selected
        )
    })
    
    #--------------------------------------------
    #Selector de leyenda1.
    #Genera Box_LegendClasDis1
    output$RUi_checkLegendClasDis1 <- renderUI({
        checkboxInput("Box_LegendClasDis1","Mostrar Leyenda.",value=F)
    })
    
    
    #GRAFICO DE DISPERSION /////////////////////////////////////////////////////
    
    #Diagrama de dispersion de la variable 1 vs Index. Coloreado por Zvar.
    output$RPlot_Dis1 <- renderPlot({
        #Action Button
        input$goButtonCla2
        
        #Inputs
        leyenda <- isolate(input$Box_LegendClasDis1)
        df <- isolate(ZvarFacReactClas())
        Yvar <- isolate(input$Select_varDis1)
        Zvar <- isolate(input$Select_ZvarClas)
        
        #Esperamos Actualizacion.
        if(is.null(df)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(Yvar)){
            return(NULL)
        }
        
        #Corregimos leyenda
        if(is.null(leyenda)){
            leyenda <- FALSE
        }
        
        #Coloreamos en funcion de numeric
        if(input$Box_Numerica2 == TRUE){
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
            #Subsetting de df para el plot.
            df <- df[c("ZvarFac",Yvar)]
            df$Index <- 1:nrow(df)
            
            #Configuramos plot.
            ScatPlot <- ggplot(df,aes(Index,get(Yvar),col=ZvarFac)) +	 			
                geom_point(size = 4) +
                ylab(Yvar) + 
                xlab("Index") +
                scale_color_discrete(name = Zvar) +
                ggtitle(paste0("Variable ",Yvar," vs Index. Coloreando por ",
                               Zvar,"."))
            
        }else{
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
            #Subsetting de df para el plot.
            df <- df[c(Zvar,Yvar)]
            df$Index <- 1:nrow(df)

            #Configuramos plot.
            ScatPlot <- ggplot(df,aes(Index,y=get(Yvar),col=get(Zvar))) +	 			
                geom_point(size = 4) +
                ylab(Yvar) + 
                xlab("Index") +
                scale_color_discrete(name = Zvar) +
                ggtitle(paste0("Variable ",Yvar," vs Index. Coloreando por ",
                               Zvar,"."))
        }
        
        #Leyenda
        if(leyenda == TRUE){
            ScatPlot 
        }else{
            ScatPlot + theme(legend.position="none")
        }	
        
    })
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: Dispersion2 //////////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #UI's RENDERIZADAS /////////////////////////////////////////////////////////
    
    #--------------------------------------------
    #Selector de la variable explicativa.
    #Genera Select_varDis2
    output$RUi_varDis2 <- renderUI({
        #input
        Zvar <- input$Select_ZvarClas
        
        #Esperamos Actualizacion.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Genera Select_varDis1
        if(input$Box_Numerica2 == TRUE){
            variables <- NumericVars2[NumericVars2 != Zvar]
            selected <- variables[1]
        }else{
            variables <- NumericVars2
            selected <- variables[1]
        }
        selectInput("Select_varDis2", "Variable de exploración:",
                    choices  = variables,
                    selected = selected
        )
    })
    
    #--------------------------------------------
    #Selector de leyenda1.
    #Genera Box_LegendClasDis1
    output$RUi_checkLegendClasDis2 <- renderUI({
        checkboxInput("Box_LegendClasDis2","Mostrar Leyenda.",value=F)
    })
    
    
    #GRAFICO DE DISPERSION /////////////////////////////////////////////////////
    
    #Diagrama de dispersion de la variable 1 vs Index. Coloreado por Zvar.
    output$RPlot_Dis2 <- renderPlot({
        #Action Button
        input$goButtonCla3
        
        #Inputs
        leyenda <- isolate(input$Box_LegendClasDis2)
        df <- isolate(ZvarFacReactClas())
        Yvar <- isolate(input$Select_varDis2)
        Zvar <- isolate(input$Select_ZvarClas)
        
        #Esperamos Actualizacion.
        if(is.null(df)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(Yvar)){
            return(NULL)
        }
        
        #Corregimos leyenda
        if(is.null(leyenda)){
            leyenda <- FALSE
        }
        
        #Coloreamos en funcion de numeric
        if(input$Box_Numerica2 == TRUE){
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
            #Subsetting de df para el plot.
            df <- df[c("ZvarFac",Yvar)]
            df$Index <- 1:nrow(df)
            
            #Configuramos plot.
            ScatPlot <- ggplot(df,aes(Index,get(Yvar),col=ZvarFac)) +	 			
                geom_point(size = 4) +
                ylab(Yvar) + 
                xlab("Index") +
                scale_color_discrete(name = Zvar) +
                ggtitle(paste0("Variable ",Yvar," vs Index. Coloreando por ",
                               Zvar,"."))
            
        }else{
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
            #Subsetting de df para el plot.
            df <- df[c(Zvar,Yvar)]
            df$Index <- 1:nrow(df)
            
            #Configuramos plot.
            ScatPlot <- ggplot(df,aes(Index,y=get(Yvar),col=get(Zvar))) +	 			
                geom_point(size = 4) +
                ylab(Yvar) + 
                xlab("Index") +
                scale_color_discrete(name = Zvar) +
                ggtitle(paste0("Variable ",Yvar," vs Index. Coloreando por ",
                               Zvar,"."))
        }
        
        #Leyenda
        if(leyenda == TRUE){
            ScatPlot 
        }else{
            ScatPlot + theme(legend.position="none")
        }	
        
    })
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: Dispersion3 //////////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #UI's RENDERIZADAS /////////////////////////////////////////////////////////
    
    #--------------------------------------------
    #Selector de la variable explicativa.
    #Genera Select_varDis3
    output$RUi_varDis3 <- renderUI({
        #input
        Zvar <- input$Select_ZvarClas
        
        #Esperamos Actualizacion.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Genera Select_varDis1
        if(input$Box_Numerica2 == TRUE){
            variables <- NumericVars2[NumericVars2 != Zvar]
            selected <- variables[1]
        }else{
            variables <- NumericVars2
            selected <- variables[1]
        }
        selectInput("Select_varDis3", "Variable de exploración:",
                    choices  = variables,
                    selected = selected
        )
    })
    
    #--------------------------------------------
    #Selector de leyenda1.
    #Genera Box_LegendClasDis1
    output$RUi_checkLegendClasDis3 <- renderUI({
        checkboxInput("Box_LegendClasDis3","Mostrar Leyenda.",value=F)
    })
    
    
    #GRAFICO DE DISPERSION /////////////////////////////////////////////////////
    
    #Diagrama de dispersion de la variable 1 vs Index. Coloreado por Zvar.
    output$RPlot_Dis3 <- renderPlot({
        #Action Button
        input$goButtonCla4
        
        #Inputs
        leyenda <- isolate(input$Box_LegendClasDis3)
        df <- isolate(ZvarFacReactClas())
        Yvar <- isolate(input$Select_varDis3)
        Zvar <- isolate(input$Select_ZvarClas)
        
        #Esperamos Actualizacion.
        if(is.null(df)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(Yvar)){
            return(NULL)
        }
        
        #Corregimos leyenda
        if(is.null(leyenda)){
            leyenda <- FALSE
        }
        
        #Coloreamos en funcion de numeric
        if(input$Box_Numerica2 == TRUE){
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
            #Subsetting de df para el plot.
            df <- df[c("ZvarFac",Yvar)]
            df$Index <- 1:nrow(df)
            
            #Configuramos plot.
            ScatPlot <- ggplot(df,aes(Index,get(Yvar),col=ZvarFac)) +	 			
                geom_point(size = 4) +
                ylab(Yvar) + 
                xlab("Index") +
                scale_color_discrete(name = Zvar) +
                ggtitle(paste0("Variable ",Yvar," vs Index. Coloreando por ",
                               Zvar,"."))
            
        }else{
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
            #Subsetting de df para el plot.
            df <- df[c(Zvar,Yvar)]
            df$Index <- 1:nrow(df)
            
            #Configuramos plot.
            ScatPlot <- ggplot(df,aes(Index,y=get(Yvar),col=get(Zvar))) +	 			
                geom_point(size = 4) +
                ylab(Yvar) + 
                xlab("Index") +
                scale_color_discrete(name = Zvar) +
                ggtitle(paste0("Variable ",Yvar," vs Index. Coloreando por ",
                               Zvar,"."))
        }
        
        #Leyenda
        if(leyenda == TRUE){
            ScatPlot 
        }else{
            ScatPlot + theme(legend.position="none")
        }	
        
    })
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: Cluster //////////////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #UI's RENDERIZADAS /////////////////////////////////////////////////////////
    
    #Selector de visualizacion de leyenda.
    #Genera Box_Legend
    output$RUi_LegendClasClus <- renderUI({
        checkboxInput("Box_LegendClasClus","Mostrar Leyenda.",value=F)
    })
    
    #--------------------------------------------
    #Conjunto completo de variables exploratorias.
    #Genera SelectList_PredClus
    output$RUi_checkboxGroupClasClus <- renderUI({
        #Input
        Zvar <- input$Select_ZvarClas
        
        #Esperamos Actualizacion.
        if(is.null(Zvar)){
            return(NULL)
        }
        #Genera Select_varDis1
        if(input$Box_Numerica2 == TRUE){
            variables <- NumericVars2[NumericVars2 != Zvar]
        }else{
            variables <- NumericVars2
        }
    
        #Render UI.
        checkboxGroupInput("SelectList_PredClus","Variables de Clusterización:",
                           choices  = variables
                           ,
                           selected = variables[1:2]
        )
    })
    
    #GRAFICO DENDROGRAMA ///////////////////////////////////////////////////////
    
    #Genera dendrograma de clusterizacion jerarquica. 
    output$RPlot_HclustClas <- renderPlot({
        #Action Button
        input$goButtonCla5
        
        #Ingreso de inputs
        df <- isolate(ZvarFacReactClas())
        PredVars <- isolate(input$SelectList_PredClus)
        legend <- isolate(input$Box_LegendClasClus)
        BoxNum <- isolate(input$Box_Numerica2)
        Zvar <- isolate(input$Select_ZvarClas)
        
        #Esperamos Actualizacion.
        if(is.null(PredVars)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(df)){
            return(plot(1:2, type='n'))
        }
        
        #Corrector de Inputs.
        if(is.null(legend)){
            legend <- FALSE
        }
        
        #Chequeamos input y extraemos variable dependiente.
        if(BoxNum == TRUE){
            #Esperamos actualizacion
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
            varDep <- df$ZvarFac
        }else{
            #Esperamos actualizacion
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
            varDep <- df[Zvar][,1]
        }

        #Subsetting especial para hclust. Existe un bug en potencia.
        vecCol <- which(names(df) %in% PredVars)
        nombres <- names(df)[vecCol]
        df <- df[,c(vecCol)] 
        plotear <- TRUE
        
        #Formateamos df
        if(class(df) != "data frame"){
            df <- as.data.frame(df)
            names(df) <- nombres
        }

        #Agregamos variable explicada
        df$varDep <- varDep
        
        #Eliminamos repetidos y faltantes
        df <- df %>% unique()
        df <- df[complete.cases(df),]
        
        #Chequeo del df
        if(nrow(df) < 2){
            plotear <- FALSE
        }
        
        #Chequeo de condiciones para plotear.
        if(plotear == FALSE){
            return(plot(1:2, type='n'))
        }
        
        #Clusterizacion jerarquica.
        d <- dist(df[!(names(df)  %in% "varDep")])											
        cluster <- hclust(d)
        
        #Corrector del Bug.
        cluster$labels <- as.character(1:nrow(df))
        
        #Dendrograma
        myplclust(cluster,lab.col = unclass(df$varDep), main = 
                      paste0("Clusterización de la Variable ",Zvar))
        
        #Agregamos leyenda.
        if(legend == TRUE){
            legend("topright",legend = unique(df$varDep),
                   col = unique(df$varDep),pch=19,title = Zvar)
        }
    })
    
    #===========================================================================
    #TAB PANEL: ANALISIS DE COMPONENTES PRINCIPALES ////////////////////////////
    #===========================================================================
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: Cor-Cov //////////////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #UI's RENDERIZADAS /////////////////////////////////////////////////////////
    
    #Lista de variables a ser incluidas en el ACP.
    #Genera SelectList_ACPvar
    output$RUi_checkboxGroupACP <- renderUI({
        #Input
        df <- ZvarFacReactClas()
        Zvar <- input$Select_ZvarClas
        
        #Esperamos Actualizacion.
        if(is.null(df)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Se habilita cuando la variable ingresada es factor.
        if(input$Box_Numerica2 == FALSE){
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
            valores <- NumericVars2
        }else{
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
            valores <- NumericVars2[!(NumericVars2 %in% c(Zvar,"ZvarFac"))]
        }
        
        #Seleccionadas
        selected <- valores[1:2]
        
        if(length(valores) >= 5){
            selected <- valores[1:5]
        }
        
        #Render UI.
        checkboxGroupInput("SelectList_ACPvar","Selector de variables.",
                           choices  = valores
                           ,
                           selected = selected
        )
    })
    
    #-------------------------------------
    #Selector de covarianza o correlacion.
    #Genera Box_CorACP
    output$RUi_CorACP <- renderUI({
        checkboxInput("Box_CorACP","Covarianzas",value=F)
    })
    
    #-------------------------------------
    #Selector correlacion de Pearson o Spearman.
    #Genera Box_SpearACP
    output$RUi_SpearACP <- renderUI({
        #Input
        covarianzas <- input$Box_CorACP
            
        #Check de input    
        if(is.null(covarianzas)){
            return(NULL)
        }
        
        #Condicional del render
        if(covarianzas == FALSE){
            checkboxInput("Box_SpearACP","Coef. de Spearman",value=T)
        }

    })
    
    #-------------------------------------
    #Selector de covarianza o correlacion.
    #Genera Box_CorACP
    output$RText_CorCov <- renderText({
        covarianza <- input$Box_CorACP
        
        #Check del input
        if(is.null(covarianza)){
            return(NULL)
        }
        
        #Salida de texto
        if(covarianza == TRUE){
            texto <- "Matriz de Covarianzas"
        }else{
            texto <- "Matriz de Correlaciones"
        }
        texto
    })
    
    #-------------------------------------
    #Indicador de ejecucion.
    observeEvent(input$goButtonACPejecutar,{
        if(input$goButtonACPejecutar == 0){
            return(NULL)
        }
        if(is.null(AReact())){
            return(NULL)
        }
        if(input$goButtonACPejecutar %% 2 == 0){
            output$RText_C.EjecACP <-renderText({						
                x <- c("Ejecutado")
                x
            })
        }else{
            output$RText_C.EjecACP <-renderText({						
                x <- c("Realizado")
                x
            })
        }
    })
    
    #FUNCIONES REACTIVAS ///////////////////////////////////////////////////////
    
    #Recibe la tabla arrojada por ZvarFacReactClas y le hace subsetting por 
    #SelectList_ACPvar. Elimina las variables sin desviacion y Retorna el data 
    #frame. LAS VARIABLES NO ESTAN ESTANDARIZADAS
    AReact <- reactive({
        #Action Button
        input$goButtonACPejecutar
        
        #Inputs
        df <- isolate(ZvarFacReactClas())
        Zvar <- isolate(input$Select_ZvarClas)
        prop <- isolate(input$Select_C.Train/100)
        BoxNum <- isolate(input$Box_Numerica2)
        semilla <- isolate(input$Select_C.Seed)
                        
        #Recibimos inputs de los UI's renderizados.
        List_Vars <- isolate(input$SelectList_ACPvar)

        #Esperamos actualizacion.    
        if(is.null(df)){
            return(NULL)   
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(List_Vars)){
            return(NULL)   
        }
        if(is.null(prop)){
            return(NULL)   
        }
        if(is.null(semilla)){
            return(NULL)   
        }
        
        #Minimo dos variables deben ser seleccionadas.
        if(length(List_Vars) < 2){
            return(NULL)   
        }
        
        #Subsetting por variable.
        if(BoxNum == TRUE){
            #Check del input
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
            #Esperamos actualizacion
            if("ZvarFac" %in% List_Vars == TRUE){
                return(NULL)
            }
            df <- df[c(List_Vars,"ZvarFac")]
        }else{
            #Check del input
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
            #Esperamos actualizacion
            if(Zvar %in% List_Vars == TRUE){
                return(NULL)
            }
            df <- df[c(List_Vars,Zvar)]
        }
        
        #Eliminamos las variables que tengan una sd() igual a 0.
        varConDes <- which(sapply(df[,1:(ncol(df)-1)],function(x){
            sd(x) != 0
        }))
        df <- df[,c(varConDes,length(df))]
        
        #Chequeamos dimension del df.
        if(is.null(dim(df))){
            return(NULL)
        }else{
            if(0 %in% dim(df)){
                return(NULL)
            }
        }
        
        #Eliminamos filas repetidas.
        df <- df %>% unique() 
        
        #Check final
        if(nrow(df) <= 2){
            return(NULL)
        }
        
        #Semilla aleatoria
        set.seed(semilla)
        
        #Subsetting Training Set
        if(prop != 1){
            ##training and testing sets con muestreo estratificado.
            if(BoxNum == TRUE){
                inTrain <- createDataPartition(
                            df["ZvarFac"][,1], p = prop,list = FALSE)
            }else{
                inTrain <- createDataPartition(
                            df[Zvar][,1], p = prop,list = FALSE)
            }
            test <- df[-inTrain,]
            df <- df[inTrain,]
        }else{
            test <- NULL
        }
        return(list(df,test))
    })
    
    #MATRIZ DE CORRELACIONES-COVARIANZAS ///////////////////////////////////////
    
    #Matriz de Covarianzas-Correlaciones.
    output$RTable_CorCov <- renderTable({
        #Plot automatico.
        input$Box_CorACP
        input$Box_SpearACP
        
        #Input
        df <- AReact()[[1]]
        covarianza <- input$Box_CorACP
        spearman <- input$Box_SpearACP
        
        #Esperamos actualizacion.
        if(is.null(df)){
            return(NULL)
        }
        if(is.null(covarianza)){
            return(NULL)
        }
        if(is.null(spearman)){
            spearman <- TRUE
        }
        
        #Eliminamos la variable categorica
        df <- df[,1:(ncol(df)-1)]
        
        #Creamos matriz
        if(covarianza == TRUE){
            tabla <- as.data.frame(cov(df))
        }else{
            #Coeficiente de spearman
            if(spearman == TRUE){
                tabla <- as.data.frame(cor(df,method="spearman"))
            }else{
                tabla <- as.data.frame(cor(df))   
            }
        }
        #Agregamos rownames.
        tabla$RN <- rownames(tabla)
        
        #Nombre de tabla:
        if(covarianza == TRUE){
            names(tabla)[length(tabla)] <- "Covarianza"
        }else{
            #Coeficiente de spearman
            if(spearman == TRUE){
                names(tabla)[length(tabla)] <- "Correlación de Spearman"
            }else{
                names(tabla)[length(tabla)] <- "Correlación de Pearson"  
            }
        }
        #Reordemanos
        tabla <- tabla[,c(ncol(tabla),1:(ncol(tabla) - 1))]
        
        #Exportamos
        return(tabla)
        
    })
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: Plot Correlaciones ///////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #PLOT DE CORRELACIONES /////////////////////////////////////////////////////
    
    #Plot de correlaciones
    output$RPlot_C.Matrix <- renderPlot({
        #Input 
        df <- AReact()[[1]]
        spearman <- input$Box_SpearACP
        covarianza <- input$Box_CorACP
        
        #Chequeamos Input
        if(is.null(df)){
            return(NULL)
        }
        if(is.null(covarianza)){
            return(NULL)
        }
        if(is.null(spearman)){
            spearman <- FALSE
        }
        
        #Plot de correlaciones 
        if(covarianza == TRUE){
            psych :: pairs.panels(df[,-length(df)], gap = 0, pch = 21,
                                  bg = df[,length(df)], 
                                  main = "Correlación de Pearson")
        }else{
            if(spearman == TRUE){
                psych :: pairs.panels(df[,-length(df)], gap = 0, pch = 21,
                                      bg = df[,length(df)] , method = "spearman",
                                      main = "Correlación de Spearman")
            }else{
                psych :: pairs.panels(df[,-length(df)], gap = 0, pch = 21,
                                      bg = df[,length(df)], 
                                      main = "Correlación de Pearson")
            }
        }
    })
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: ACP //////////////////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #FUNCIONES REACTIVAS ///////////////////////////////////////////////////////
    
    #Realiza ACP. Hay que estandarizar las variables primero.
    ACPReact <- reactive({
        #Action Button
        input$goButtonACPejecutar
        
        #Inputs
        df <- AReact()[[1]]
        
        #recibimos inputs de los UI's renderizados.
        covarianza <- isolate(input$Box_CorACP)
        spearman <- isolate(input$Box_SpearACP)
        
        #Esperamos actualizacion.    
        if(is.null(df)){
            return(NULL)   
        }
        if(is.null(covarianza)){
            return(NULL)   
        }
        if(is.null(spearman)){
            return(NULL)   
        }
        
        #Eliminamos la variable categorica
        catVar <- df[ncol(df)]
        df <- df[,1:(ncol(df)-1)]

        #Usamos correlacion o covarianza.  #CHEQUEAR ESTE CODIGO PARA NUMERIC
        if(covarianza == TRUE){
            myCors <- cov(scale(df,scale=FALSE))
            pca <- princomp(covmat = myCors)
            pca$scores <- scale(df,scale=FALSE) %*% pca$loadings 
            pca$n.obs <- nrow(df)
            pca$center <- sapply(df,mean,na.rm=TRUE)
        }else{
            if(spearman == TRUE){
                myCors <- cor(scale(df,scale=FALSE), method="spearman")
                pca <- princomp(covmat = myCors)
                pca$scores <- scale(df) %*% pca$loadings
                pca$n.obs <- nrow(df)
                pca$center <- sapply(df,mean,na.rm=TRUE)
            }else{
                myCors <- cor(scale(df,scale=FALSE))
                pca <- princomp(covmat = myCors)#,cor = TRUE)
                pca$scores <- scale(df) %*% pca$loadings 
                pca$n.obs <- nrow(df)
                pca$center <- sapply(df,mean,na.rm=TRUE)
            }
        }

        #Validador. Matrices identicas
        #matriz1 <- pca$loadings[,] %*% diag(pca$sdev^2) %*% t(pca$loadings[,])
        #print(myCors)
        #print(matriz1)

        #print(pca$loadings[,])            #Asi funciona no se por que...
        return(list(pca,catVar))
        
    })
    
    #RESUMEN DEL ACP ///////////////////////////////////////////////////////////
    
    #Matriz de Autovectores
    output$RPrnt_C.ACP2.1 <-renderPrint({ #Asi funciona no se por que...
        #Input 
        pca <- ACPReact()[[1]]
        #Chequeamos Input.
        if(is.null(pca)){
            return(NULL)
        }
        print(pca$loadings[,]) 
    }) 
    
    #----------------------------------------
    #Tabla Resumen del ACP
    output$RPrnt_C.ACP2.2 <-renderPrint({ #Asi funciona no se por que...
        #Input 
        pca <- ACPReact()[[1]]
        #Chequeamos Input.
        if(is.null(pca)){
            return(NULL)
        }
        print(summary(pca))
    }) 
    
    #----------------------------------------
    #Autovalores asociados a cada componente.
    output$RTable_C.ACP2.3 <-renderTable({ 
        #Input 
        pca <- ACPReact()[[1]]
        
        #Chequeamos Input.
        if(is.null(pca)){
            return(NULL)
        }
        tabla <- rep(NA,times = (length(pca$sdev^2)*2))
        dim(tabla) <- c(2,length(pca$sdev^2))
        tabla[1,] <- round(pca$sdev^2,4)
        tabla <- as.data.frame(tabla)
        names(tabla) <- colnames(pca$loadings)
        tabla <- tabla[1,]
        tabla
    }) 
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: SEDIMENTACION ////////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    output$RPlot_C.ACP3 <- renderPlot({
        #Input 
        pca <- ACPReact()[[1]]
        
        #Chequeamos Input.
        if(is.null(pca)){
            return(NULL)
        }
        screeplot(pca,main = "Gráfico de Sedimentación")
    })
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: BIPLOT ///////////////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #UI's RENDERIZADAS /////////////////////////////////////////////////////////
  
    #Selector de la componente 1.
    #Genera Select_C.Comp1
    output$RUi_Select_C.Comp1 <- renderUI({
        #Input
        pca <- ACPReact()[[1]]
        
        #Check de entrada.
        if(is.null(pca)){
            return(NULL)
        }
        #Numero maximo de autovewctores
        maximo <- dim(unclass(pca$loadings))[2]
        
        numericInput("Select_C.Comp1","Componente Principal #1",
                     min = 1,max = maximo,value = 1,step = 1)
    })
    
    #--------------------------------------------
    #Selector de la componente 2.
    #Genera Select_C.Comp2
    output$RUi_Select_C.Comp2 <- renderUI({
        #Input
        pca <- ACPReact()[[1]]
        
        #Check de entrada.
        if(is.null(pca)){
            return(NULL)
        }
        #Numero maximo de autovewctores
        maximo <- dim(unclass(pca$loadings))[2]
        
        numericInput("Select_C.Comp2","Componente Principal #2",
                     min = 1,max = maximo,value = 2,step = 1)
    })
    
    #----------------------------------------
    #Mostrar leyenda.
    #Genera Box_LegendHistLoc
    output$RUi_checkbox.CCP.Biplot.Leg <- renderUI({
        checkboxInput("Box_CCP.Biplot.Leg","Leyenda",value=F)
    })
    
    #GRAFICO BIPLOT ////////////////////////////////////////////////////////////
    
    #Grafico Biplot
    output$RPlot_CCP.Biplot <- renderPlot({
        #Action Button
        input$goButtonACPBiplot
        
        #Input 
        df  <- isolate(AReact()[[1]])
        pca <- isolate(ACPReact()[[1]])
        Zvar  <- isolate(input$Select_ZvarClas)
        comp1 <- isolate(input$Select_C.Comp1)
        comp2 <- isolate(input$Select_C.Comp2)
        leyenda <- isolate(input$Box_CCP.Biplot.Leg)
        BoxNum <- isolate(input$Box_Numerica2)
        
        #Chequeamos Input.
        if(is.null(pca)){
            return(NULL)
        }
        if(is.null(df)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(leyenda)){
            return(NULL)
        }
        if(is.null(comp1)){
            return(NULL)
        }
        if(is.null(comp2)){
            return(NULL)
        }
        if(comp1 == comp2){
            return(NULL)
        }
        if(comp2 < comp1){
            aux <- comp1
            comp1 <- comp2
            comp2 <- aux
        }
        
        #Chequeamos input y extraemos variable dependiente.
        if(BoxNum == TRUE){
            #Esperamos actualizacion
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
        }else{
            #Esperamos actualizacion
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
        }
        assign("varBip",df[,ncol(df)],envir=globalenv())

        #Grafico Biplot 
        biplot <- ggbiplot(pca, choices = c(comp1,comp2), obs.scale = 1, 
                           var.scale = 1, ellipse = TRUE, circle = TRUE,
                           groups = df[,ncol(df)], ellipse.prob = 0.68,
                           varname.size = 4) +
                    geom_point(aes(col = get("varBip",envir=globalenv())), 
                               size = 4) +
                    scale_color_discrete(name = Zvar) +
                    ggtitle(paste0("Gráfico Biplot. Variable ",Zvar))
        
        #Leyenda
        if(leyenda == TRUE){
            biplot 
        }else{
            biplot + theme(legend.position="none")
        }
        
        #Grafico Biplot  
        #biplot(pca,choices = c(comp1,comp2), main = 
        #    paste0("Gráfico Biplot. Variable ",Zvar),col=c("blue","red"))
    })
    
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: Componente vs Componente /////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #UI's RENDERIZADAS /////////////////////////////////////////////////////////
    
    #Selector de la componente 1.
    #Genera Select_C.Comp21
    output$RUi_Select_C.Comp21 <- renderUI({
        #Input
        pca <- ACPReact()[[1]]
        
        #Check de entrada.
        if(is.null(pca)){
            return(NULL)
        }
        #Numero maximo de autovewctores
        maximo <- dim(unclass(pca$loadings))[2]
        
        numericInput("Select_C.Comp21","Componente Principal #1",
                     min = 1,max = maximo,value = 1,step = 1)
    })
    
    #--------------------------------------------
    #Selector de la componente 2.
    #Genera Select_C.Comp22
    output$RUi_Select_C.Comp22 <- renderUI({
        #Input
        pca <- ACPReact()[[1]]
        
        #Check de entrada.
        if(is.null(pca)){
            return(NULL)
        }
        #Numero maximo de autovewctores
        maximo <- dim(unclass(pca$loadings))[2]
        
        numericInput("Select_C.Comp22","Componente Principal #2",
                     min = 1,max = maximo,value = 2,step = 1)
    })
    
    #----------------------------------------
    #Mostrar leyenda..
    #Genera Box_CCP.CvsC.Leg
    output$RUi_checkbox.CCP.CvsC.Leg <- renderUI({
        checkboxInput("Box_CCP.CvsC.Leg","Leyenda",value=F)
    })
    
    #----------------------------------------
    #Opciones de eleccion para los valores de Zvar.
    #Genera SelectList_Zval3
    output$RUi_checkboxGroup.CCP.CvxC <- renderUI({
        #Input
        df <- AReact()[[1]]
        Zvar <- isolate(input$Select_ZvarClas)
        BoxNum <- isolate(input$Box_Numerica2)
        
        #Esperamos Actualizacion.
        if(is.null(df)){
            return(NULL)
        }
        #Esperamos Actualizacion.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Se habilita cuando la variable ingresada es factor.
        if(BoxNum == FALSE){
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
            valores <- as.character(unique(df[Zvar][,1]))
        }else{
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
            valores <- as.character(unique(df["ZvarFac"][,1]))
        }
        
        #Render UI.
        checkboxGroupInput("SelectList_CCP.CvxC",
                           "Selector de valores.",
                           choices  = valores
                           ,
                           selected = valores[1:2]
        )
    })
    
    #GRAFICO SCATTERPLOT ///////////////////////////////////////////////////////
    
    #Grafico Scatterplot
    output$RPlot_CCP.CvsC <- renderPlot({
        #Action Button
        input$goButtonACPCvsC
        
        #Input 
        df  <- isolate(AReact()[[1]])
        pca <- isolate(ACPReact()[[1]])
        Zvar  <- isolate(input$Select_ZvarClas)
        comp1 <- isolate(input$Select_C.Comp21)
        comp2 <- isolate(input$Select_C.Comp22)
        leyenda <- isolate(input$Box_CCP.CvsC.Leg)
        BoxNum <- isolate(input$Box_Numerica2)
        List_vals <- isolate(input$SelectList_CCP.CvxC)
        
        #Chequeamos Input.
        if(is.null(pca)){
            return(NULL)
        }
        if(is.null(df)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(leyenda)){
            return(NULL)
        }
        if(is.null(List_vals)){
            return(NULL)
        }
        if(is.null(comp1)){
            return(NULL)
        }
        if(is.null(comp2)){
            return(NULL)
        }
        if(comp1 == comp2){
            return(NULL)
        }
        if(comp2 < comp1){
            aux <- comp1
            comp1 <- comp2
            comp2 <- aux
        }
        
        #Chequeamos input y extraemos variable dependiente.
        if(BoxNum == TRUE){
            #Esperamos actualizacion
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
            #Subsetting de df
            pca$scores <- pca$scores[df$ZvarFac %in% List_vals,]
            df <- df[df$ZvarFac %in% List_vals,]
        }else{
            #Esperamos actualizacion
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
            #Subsetting de df
            pca$scores <- pca$scores[df[Zvar][,1] %in% List_vals,]
            df <- df[df[Zvar][,1] %in% List_vals,]
        }
        
        #Chequeamos dimension.
        if(nrow(df) == 0){
            return(NULL)
        }   

        #Proporcion de varianza
        prop <- round(pca$sdev^2/sum(pca$sdev^2)*100, 1)
        
        #Plot
        varScat <- names(df)[ncol(df)]

        ScatterP <- ggplot(df,aes(pca$scores[,comp1],pca$scores[,comp2],
                                  col= get(varScat))) +
            xlab(paste0("Componente Principal ",comp1,
                        ". (Prop. Ac. ", prop[comp1], "%)")) +
            ylab(paste0("Componente Principal ",comp2,
                        ". (Prop. Ac. ", prop[comp2], "%)")) +
            geom_point(size = 4) +
            scale_color_discrete(name = Zvar) +
            ggtitle("Diagrama de dispersión CP vs CP.") 
        
        #Leyenda
        if(leyenda == TRUE){
            ScatterP 
        }else{
            ScatterP + theme(legend.position="none")
        }
        
    }) 
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: Evaluacion ///////////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #Numero de componentes a usar.
    output$RUi_Select_C.Evalk <- renderUI({
        #Input
        pca <- ACPReact()[[1]]
        
        #Check de entrada.
        if(is.null(pca)){
            return(NULL)
        }
        #Numero maximo de autovewctores
        maximo <- dim(unclass(pca$loadings))[2]
        
        numericInput("Select_C.Evalk","Número de Componentes a Usar:",
                     min = 1,max = maximo,value = 2,step = 1)
    })
    
    
    #REACTIVE MULTINOMIAL MODEL ------------------------------------------------
    MNmodelReac <- reactive({
        #Input 
        pca <- ACPReact()[[1]]
        training <- isolate(AReact()[[1]])
        Zvar <- isolate(input$Select_ZvarClas)
        k <- input$Select_C.Evalk
        NumBox <- isolate(input$Box_Numerica2)
        
        #Chequeamos Input.
        if(is.null(pca)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(training)){
            return(NULL)
        }
        if(is.null(k)){
            return(NULL)
        }

        pca$loadings <- unclass(pca$loadings)
        #Prediccion
        trg <- pca$scores             #NO USAR PREDICT CON PRINCOMP.
        trg <- data.frame(trg,training[ncol(training)])
        
        #Seleccionamos numero de PCs a implementar.
        trg <- trg[,c(1:k,ncol(trg))]
        
        #trainingPCA <- training
        #View(head(trainingPCA))
        #View(head(pca$loadings))
        #PCAtrg <- trg
        #View(head(PCAtrg))
        #View(head(pca$scores))
        #View(head(scale(training[-ncol(training)]) %*% pca$loadings))
        #Multinomial Logistic Regresion with the First Two PCs.
        if(NumBox == TRUE){
            #Check de ingreso
            if("ZvarFac" %in% names(trg) == FALSE){
                return(NULL)
            }
            names(trg)[names(trg) == "ZvarFac"] <- "dep_Var"
            mymodel <- 
                nnet::multinom(dep_Var ~ .,data = trg,MaxNWts =100000)
        }else{
            #Check de ingreso
            if("ZvarFac" %in% names(trg) == TRUE){
                return(NULL)
            }
            names(trg)[names(trg) == Zvar] <- "dep_Var"
            mymodel <- 
                nnet::multinom(dep_Var ~ .,data = trg,MaxNWts =100000)
        }

        return(list(mymodel,trg))
    })
    
    #PRINTS --------------------------------------------------------------------
    #Evaluacion Training Set
    output$RPrnt_C.ACPEval1 <-renderPrint({ 
        #Input 
        trg <- MNmodelReac()[[2]]
        mymodel <- MNmodelReac()[[1]]
        training <- isolate(AReact()[[1]])
        Zvar <- isolate(input$Select_ZvarClas)
        NumBox <- isolate(input$Box_Numerica2)
        
        #Chequeamos Input.
        if(is.null(mymodel)){
            return(NULL)
        }
        if(is.null(training)){
            return(NULL)
        }
        if(is.null(trg)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }

        p <- predict(mymodel,trg)
        tab <- table(Predicted = p,Actual = trg$dep_Var)
        
        cat("_____________________________________________________________\n")
        print(summary(mymodel))             #Resumen del modelo.
        cat("______________Evaluacion sobre el Training Set_______________\n")
        cat("Matriz de Confusión\n")
        print(tab)                          #Matriz de confusion. 
        cat("_____________________________________________________________\n")
        cat(paste0("Nivel de Precisión: ",sum(diag(tab))/sum(tab)*100,"%\n"))
    }) 
    
    #Evaluacion Testing Set
    output$RPrnt_C.ACPEval2 <-renderPrint({ 
        #Input 
        pca <- isolate(ACPReact()[[1]])
        testing <- isolate(AReact()[[2]])
        mymodel <- MNmodelReac()[[1]]
        Zvar <- isolate(input$Select_ZvarClas)
        NumBox <- isolate(input$Box_Numerica2)
            
        #Chequeamos Input.
        if(is.null(pca)){
            return(NULL)
        }
        if(is.null(mymodel)){
            return(NULL)
        }
        if(is.null(testing)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Prediccion
        tst <- scale(testing[-ncol(testing)]) %*% pca$loadings
        tst <- data.frame(tst,testing[ncol(testing)])

        
        #Confusion Matrix and accuracy - training
        if(NumBox == TRUE){
            #Check de ingreso
            if("ZvarFac" %in% names(tst) == FALSE){
                return(NULL)
            }
            names(tst)[names(tst) == "ZvarFac"] <- "dep_Var"
        }else{
            #Check de ingreso
            if("ZvarFac" %in% names(tst) == TRUE){
                return(NULL)
            }
            names(tst)[names(tst) == Zvar] <- "dep_Var"
        }
        
        #Construccion del modelo
        p <- predict(mymodel,tst)
        tab <- table(Predicted = p,Actual = tst$dep_Var)
        
        cat("_______________Evaluacion sobre el Testing Set_______________\n")
        cat("Matriz de Confusión\n")
        print(tab)                          #Matriz de confusion. 
        cat("_____________________________________________________________\n")
        cat(paste0("Nivel de Precisión: ",sum(diag(tab))/sum(tab)*100,"%\n"))
    }) 
    
    
    
    #===========================================================================
    #TAB PANEL: METRIC MULTIDIMENSIONAL SCALING ////////////////////////////////
    #===========================================================================
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: SELECTOR /////////////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #UI's RENDERIZADAS /////////////////////////////////////////////////////////
    
    #Lista de variables a ser incluidas en el ACP.
    #Genera SelectList_ACPvar
    output$RUi_checkboxGroupACoP <- renderUI({
        #Input
        df <- ZvarFacReactClas()
        Zvar <- input$Select_ZvarClas
        
        #Esperamos Actualizacion.
        if(is.null(df)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Se habilita cuando la variable ingresada es factor.
        if(input$Box_Numerica2 == FALSE){
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
            valores <- NumericVars2
        }else{
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
            valores <- NumericVars2[!(NumericVars2 %in% c(Zvar,"ZvarFac"))]
        }
        
        #Seleccionadas
        selected <- valores[1:2]
        
        if(length(valores) >= 5){
            selected <- valores[1:5]
        }
        
        #Render UI.
        checkboxGroupInput("SelectList_ACoPvar","Variables:",
                           choices  = valores
                           ,
                           selected = selected
        )
    })
    
    #-------------------------------------
    #Selector de distancia.
    output$RUi_Select_c.Distancia <- renderUI({
        
        selectInput("Select_c.Distancia", "Medida:",
                    choices  = c("euclidean","maximum", "manhattan", "canberra",
                                 "binary","minkowski","bray","kulczynski",
                                 "jaccard","gower","altGower","morisita", 
                                 "horn","mountford","raup" ,"binomial","chao",
                                 "cao","mahalanobis")
                    ,
                    selected = "euclidean"
        )
    })
    
    #-------------------------------------
    #Selector Numero de dimensiones.
    output$RUi_Select_PCoADim <- renderUI({
        #Input
        mat <- MatrixReact()[[1]]
        
        #Check de entrada.
        if(is.null(mat)){
            return(NULL)
        }
        #Numero maximo de autovewctores
        maximo <- dim(mat)[1] - 1
        
        numericInput("Select_PCoADim","Cantidad:",
                     min = 1,max = maximo,value = 2,step = 1)
    })
    
    #-------------------------------------
    #Indicador de ejecucion.
    observeEvent(input$goButtonACoPejecutar,{
        if(input$goButtonACoPejecutar == 0){
            return(NULL)
        }
        if(input$goButtonACoPejecutar %% 2 == 0){
            output$RText_C.EjecMDS <-renderText({						
                x <- c("Ejecutado")
                x
            })
        }else{
            output$RText_C.EjecMDS <-renderText({						
                x <- c("Realizado")
                x
            })
        }
    })
     
    #FUNCIONES REACTIVAS ///////////////////////////////////////////////////////
    
    #Recibe la tabla arrojada por ZvarFacReactClas y le hace subsetting por 
    #SelectList_ACPvar. Elimina las variables sin desviacion y Retorna el data 
    #frame. LAS VARIABLES NO ESTAN ESTANDARIZADAS
    MatrixReact <- reactive({
        #Action Button
        #input$goButtonACoPejecutar
        
        #Inputs
        df <- ZvarFacReactClas()
        Zvar <- input$Select_ZvarClas
        prop <- input$Select_C.Train/100
        BoxNum <- input$Box_Numerica2
        semilla <- input$Select_C.Seed
        
        #Recibimos inputs de los UI's renderizados.
        List_Vars <- input$SelectList_ACoPvar
        
        #Esperamos actualizacion.    
        if(is.null(df)){
            return(NULL)   
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(List_Vars)){
            return(NULL)   
        }
        if(is.null(prop)){
            return(NULL)   
        }
        if(is.null(semilla)){
            return(NULL)   
        }
        
        #Minimo dos variables deben ser seleccionadas.
        if(length(List_Vars) < 2){
            return(NULL)   
        }
        
        #Subsetting por variable.
        if(BoxNum == TRUE){
            #Check del input
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
            #Esperamos actualizacion
            if("ZvarFac" %in% List_Vars == TRUE){
                return(NULL)
            }
            df <- df[c(List_Vars,"ZvarFac")]
        }else{
            #Check del input
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
            #Esperamos actualizacion
            if(Zvar %in% List_Vars == TRUE){
                return(NULL)
            }
            df <- df[c(List_Vars,Zvar)]
        }
        
        #Eliminamos las variables que tengan una sd() igual a 0.
        varConDes <- which(sapply(df[,1:(ncol(df)-1)],function(x){
            sd(x) != 0
        }))
        df <- df[,c(varConDes,length(df))]
        
        #Chequeamos dimension del df.
        if(is.null(dim(df))){
            return(NULL)
        }else{
            if(0 %in% dim(df)){
                return(NULL)
            }
        }
        
        #Eliminamos filas repetidas.
        df <- df %>% unique()
        
        #Check final
        if(nrow(df) <= 2){
            return(NULL)
        }
        
        #Semilla Aleatoria
        set.seed(semilla)
        
        #Subsetting Training Set
        if(prop != 1){
            ##training and testing sets con muestreo estratificado.
            if(BoxNum == TRUE){
                inTrain <- createDataPartition(
                    df["ZvarFac"][,1], p = prop,list = FALSE)
            }else{
                inTrain <- createDataPartition(
                    df[Zvar][,1], p = prop,list = FALSE)
            }
            testing <- df[-inTrain,]
            training <- df[inTrain,]
        }else{
            testing <- NULL
            training <- df
        }

        #Damos formato a los datos para el ACoP.
        catVar <- training[names(training)[ncol(training)]]

        return(list(training,catVar,testing))
    })
    
    #---------------------------------------------------------
    #Realiza ACoP. Hay que estandarizar las variables primero.
    ACoPReact <- reactive({
        #Action Button
        input$goButtonACoPejecutar
        
        #Detenemos ejecucion automatica
        if(input$goButtonACoPejecutar == 0){
            return(NULL)
        }
        
        #Inputs
        mat <- isolate(MatrixReact()[[1]])
        Zvar <- isolate(input$Select_ZvarClas)
        dis <- isolate(input$Select_c.Distancia)
        dimension <- isolate(input$Select_PCoADim)
        
        #recibimos inputs de los UI's renderizados.
        List_Vars <- isolate(input$SelectList_ACoPvar)
        
        #Esperamos actualizacion.    
        if(is.null(mat)){
            return(NULL)   
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(dis)){
            return(NULL)
        }
        if(is.null(dimension)){
            return(NULL)
        }
        if(is.null(List_Vars)){
            return(NULL)   
        }
        
        #Estandarizamos la data.
        mat <- mat[,-ncol(mat)]
        mat <- scale(mat)
   
        #Calculamos la matriz de distancias #Chequear el codigo AQUI ///////
        if(dis %in% c("euclidean","maximum","manhattan",
                                            "canberra","binary","minkowski")){
            dmatrix <- dist(mat,method=dis)
        }else{
            dmatrix <- vegdist(mat,method = dis)
        }

        ## do the MDS math (this is basically eigen value decomposition)
        pco <- cmdscale(dmatrix,k = dimension ,eig=TRUE)
   
        #COnstruimos matriz de autovectores.
        pco$loadings <- autoVecMat(mat,pco$points) 
        rownames(pco$loadings) <- colnames(mat)
        return(pco)
        
    })
    
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: RESUMEN //////////////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #Matriz de Autovectores
    output$RPrnt_C.TablaACoP <-renderPrint({ #Asi funciona no se por que...
        #output$RPrnt_C.TablaACoP <-renderTable({
        #Input 
        pco <- ACoPReact()
        
        #Chequeamos Input.
        if(is.null(pco)){
            return(NULL)
        }
        
        print(pco$loadings)
    }) 
    
    #----------------------------------------
    #porcetaje de varianza acumulada
    output$RTable_C.ACoPRes2 <-renderTable({ 
        #Input 
        mds.stuff <- ACoPReact()
        k <- input$Select_PCoADim
        
        #Chequeamos Input.
        if(is.null(mds.stuff)){
            return(NULL)
        }
        if(is.null(k)){
            return(NULL)
        }
        #Corrector de autovalores (casos especiales)
        mds.stuff$eig <- abs(mds.stuff$eig)
        
        ##Porcentaje de variacion capturado
        mds.stuff$eig <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)
        
        #Limitamos numero de autovectores
        mds.stuff$eig <- mds.stuff$eig[1:k]
        
        #Generamos vector de nombres
        nomb1 <- rep("comp.",length(mds.stuff$eig))
        nomb2 <- seq(1,length(mds.stuff$eig))
        nombres <- c(" ",paste0(nomb1,nomb2))
        
        #Creamos tabla
        tabla <- rep(NA,times = (length(mds.stuff$eig)*2))
        dim(tabla) <- c(2,length(mds.stuff$eig))
        tabla[1,] <- round(mds.stuff$eig,4)
        tabla[2,] <- cumsum(round(mds.stuff$eig,4))
        tabla <- as.data.frame(tabla)
        tabla <- tabla[1:2,]
        tabla[,2:(ncol(tabla)+1)] <- tabla[,1:ncol(tabla)]
        names(tabla) <- nombres
        tabla[,1] <- c("Proportion of Variance","Cumulative Proportion")
        tabla
    }) 
    
    #----------------------------------------
    #Autovalores asociados a cada componente.
    output$RTable_C.ACoPRes3 <-renderTable({ 
        #Input 
        mds.stuff <- ACoPReact()
        k <- input$Select_PCoADim
        
        #Chequeamos Input.
        if(is.null(mds.stuff)){
            return(NULL)
        }
        if(is.null(k)){
            return(NULL)
        }
        #Corrector de autovalores (casos especiales)
        mds.stuff$eig <- abs(mds.stuff$eig)
        
        #Limitamos numero de autovectores
        mds.stuff$eig <- mds.stuff$eig[1:k]
        
        #Generamos vector de nombres
        nomb1 <- rep("comp.",length(mds.stuff$eig))
        nomb2 <- seq(1,length(mds.stuff$eig))
        nombres <- paste0(nomb1,nomb2)

        #Creamos tabla
        tabla <- rep(NA,times = (length(mds.stuff$eig)*2))
        dim(tabla) <- c(2,length(mds.stuff$eig))
        tabla[1,] <- round(mds.stuff$eig,4)
        tabla <- as.data.frame(tabla)
        names(tabla) <- nombres
        tabla <- tabla[1,]
        tabla
    }) 
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: SEDIMENTACION ////////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #Gradico de sedimentacion
    output$RPlot_C.ACoPSed <-renderPlot({ 
        #Input 
        mds.stuff <- ACoPReact()
        k <- input$Select_PCoADim
        
        #Chequeamos Input.
        if(is.null(mds.stuff)){
            return(NULL)
        }
        if(is.null(k)){
            return(NULL)
        }
        #Corrector de autovalores (casos especiales)
        mds.stuff$eig <- abs(mds.stuff$eig)
            
        ##Porcentaje de variacion capturado
        mds.stuff$eig <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)
        
        #Limitamos numero de autovectores
        mds.stuff$eig <- mds.stuff$eig[1:k]
        
        #Generamos vector de nombres
        nomb1 <- rep("comp.",length(mds.stuff$eig))
        nomb2 <- seq(1,length(mds.stuff$eig))
        nombres <- paste0(nomb1,nomb2)
        
        #Creamos tabla
        tabla <- rep(NA,times = (length(mds.stuff$eig)*2))
        dim(tabla) <- c(2,length(mds.stuff$eig))
        tabla[1,] <- round(mds.stuff$eig,4)
        tabla[2,] <- nombres
        tabla <- t(tabla)
        tabla <- as.data.frame(tabla)
        tabla$V2 <- as.factor(tabla$V2)
        tabla$V1 <- as.numeric(as.character(tabla$V1))

        ggplot(tabla,aes(V2,V1)) + geom_bar(stat = "identity") +
            xlab("Principal Components") + ylab("Percentage of Variance") +
            ggtitle("Gráfico de Sedimentación")
    }) 
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: CP vs CP /////////////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #UI's RENDERIZADAS /////////////////////////////////////////////////////////
    
    #Selector de la componente 1.
    output$RUi_Select_C.PCoAComp1 <- renderUI({
        #Input
        dimension <- input$Select_PCoADim
        
        #Check de entrada.
        if(is.null(dimension)){
            return(NULL)
        }
        #Numero maximo de autovewctores
        maximo <- dimension
        
        numericInput("Select_C.PCoAComp1","Coordenada Principal #1",
                     min = 1,max = maximo,value = 1,step = 1)
    })
    
    #--------------------------------------------
    #Selector de la componente 2.
    #Genera Select_C.Comp22
    output$RUi_Select_C.PCoAComp2 <- renderUI({
        #Input
        dimension <- input$Select_PCoADim
        
        #Check de entrada.
        if(is.null(dimension)){
            return(NULL)
        }
        #Numero maximo de autovewctores
        maximo <- dimension
        
        numericInput("Select_C.PCoAComp2","Coordenada Principal #2",
                     min = 1,max = maximo,value = 2,step = 1)
    })
    
    #----------------------------------------
    #Mostrar leyenda..
    #Genera Box_CCP.CvsC.Leg
    output$RUi_checkbox.C.ACoP.Leg <- renderUI({
        checkboxInput("Box_C.ACoP.Leg","Leyenda",value=F)
    })
    
    #----------------------------------------
    #Opciones de eleccion para los valores de Zvar.
    #Genera SelectList_Zval3
    output$RUi_checkboxGroup.C.ACoP <- renderUI({
        #Input
        catVar <- as.character(MatrixReact()[[2]][,1])
        
        #Esperamos Actualizacion.
        if(is.null(catVar)){
            return(NULL)
        }
        
        valores <- as.character(unique(catVar))
        
        #Render UI.
        checkboxGroupInput("SelectList_C.ACoP",
                           "Selector de valores.",
                           choices  = valores
                           ,
                           selected = valores[1:2]
        )
    })
    
    #GRAFICO PCoA //////////////////////////////////////////////////////////////
    
    #Grafico PCoA
    output$RPlot_C.PCoA <- renderPlot({
        #Action Button
        input$goButtonACoP

        #Detener plot automatica
        if(input$goButtonACoP == 0){
            return(NULL)
        }
        #Input 
        mds.stuff <- isolate(ACoPReact())
        Zvar  <- isolate(input$Select_ZvarClas)
        dis <- isolate(input$Select_c.Distancia)
        comp1 <- isolate(input$Select_C.PCoAComp1)
        comp2 <- isolate(input$Select_C.PCoAComp2)
        BoxNum <- isolate(input$Box_Numerica2)
        catVar <- isolate(MatrixReact()[[2]])
        leyenda <- isolate(input$Box_C.ACoP.Leg)
        List_vals <- isolate(input$SelectList_C.ACoP)
        
        #Chequeamos Input.
        if(is.null(mds.stuff)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(dis)){
            return(NULL)
        }
        if(is.null(catVar)){
            return(NULL)
        }
        if(is.null(leyenda)){
            return(NULL)
        }
        if(is.null(List_vals)){
            return(NULL)
        }
        if(is.null(comp1)){
            return(NULL)
        }
        if(is.null(comp2)){
            return(NULL)
        }
        if(comp1 == comp2){
            return(NULL)
        }
        if(comp2 < comp1){
            aux <- comp1
            comp1 <- comp2
            comp2 <- aux
        }
        #Corrector de autovalores (casos especiales)
        mds.stuff$eig <- abs(mds.stuff$eig)
        
        ## calculate the percentage of variation that each MDS axis accounts for...
        prop <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)
        
        ## now make a fancy looking plot that shows the MDS axes and the variation:
        mds.values <- mds.stuff$points
        df <- data.frame(catVar=as.factor(catVar[,1]),
                               X=mds.values[,comp1],
                               Y=mds.values[,comp2])
       
        #Subsetting de df
        df <- df[df$catVar %in% List_vals,]
        
        #Chequeamos dimension.
        if(nrow(df) == 0){
            return(NULL)
        } 
        
        #Plot
        scatt <- ggplot(df, aes(x=X, y=Y, col=catVar)) +
            xlab(paste0("Coordenada Principal ",comp1,
                        ". (Prop. Ac. ", prop[comp1], "%)")) +
            ylab(paste0("Coordenada Principal ",comp2,
                        ". (Prop. Ac. ", prop[comp2], "%)")) +
            geom_point(size = 4) +
            scale_color_discrete(name = Zvar) +
            ggtitle(paste0("Diagrama de dispersión CP vs CP. Distancia ",dis))

        
        #Leyenda
        if(leyenda == TRUE){
            scatt 
        }else{
            scatt + theme(legend.position="none")
        }
        
    })
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: Evaluacion ///////////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #Numero de componentes a usar.
    output$RUi_Select_C.EvalMDS <- renderUI({
        #Input
        pco <- ACoPReact()
        
        #Check de entrada.
        if(is.null(pco)){
            return(NULL)
        }
        
        #Numero maximo de autovewctores
        maximo <- dim(unclass(pco$loadings))[2]
        
        numericInput("Select_C.EvalMDS","Número de Componentes a Usar:",
                     min = 1,max = maximo,value = 2,step = 1)
        
    })
    
    #REACTIVE MULTINOMIAL MODEL ------------------------------------------------
    MNmodelMDSReac <- reactive({
        #Input 
        pco <- ACoPReact()
        training <- isolate(MatrixReact()[[1]])
        Zvar <- isolate(input$Select_ZvarClas)
        k <- input$Select_C.EvalMDS
        NumBox <- isolate(input$Box_Numerica2)
        
        #Chequeamos Input.
        if(is.null(pco)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(training)){
            return(NULL)
        }
        if(is.null(k)){
            return(NULL)
        }

        #Prediccion
        trg <- scale(training[-ncol(training)]) %*% pco$loadings
        trg <- data.frame(trg,training[ncol(training)])
        names(trg)[1:(ncol(trg)-1)] <- 
            paste0(rep("PC",(ncol(trg)-1)),1:(ncol(trg)-1))
        
        #Seleccionamos numero de PCs a implementar.
        trg <- trg[,c(1:k,ncol(trg))]
        

        #Multinomial Logistic Regresion with the First Two PCs.
        if(NumBox == TRUE){
            #Check de ingreso
            if("ZvarFac" %in% names(trg) == FALSE){
                return(NULL)
            }
            names(trg)[names(trg) == "ZvarFac"] <- "dep_Var"
            mymodel <- 
                nnet::multinom(dep_Var ~ .,data = trg,MaxNWts =100000)
        }else{
            #Check de ingreso
            if("ZvarFac" %in% names(trg) == TRUE){
                return(NULL)
            }
            names(trg)[names(trg) == Zvar] <- "dep_Var"
            mymodel <- 
                nnet::multinom(dep_Var ~ .,data = trg,MaxNWts =100000)
        }
        
        return(list(mymodel,trg))
    })
    
    #PRINTS --------------------------------------------------------------------
    #Evaluacion Training Set
    output$RPrnt_C.ACoPEval1 <-renderPrint({ 
        #Input 
        trg <- MNmodelMDSReac()[[2]]
        mymodel <- MNmodelMDSReac()[[1]]
        Zvar <- isolate(input$Select_ZvarClas)
        NumBox <- isolate(input$Box_Numerica2)
        
        #Chequeamos Input.
        if(is.null(mymodel)){
            return(NULL)
        }
        if(is.null(trg)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        
        p <- predict(mymodel,trg)
        tab <- table(Predicted = p,Actual = trg$dep_Var)

        cat("_____________________________________________________________\n")
        print(summary(mymodel))             #Resumen del modelo.
        cat("______________Evaluacion sobre el Training Set_______________\n")
        cat("Matriz de Confusión\n")
        print(tab)                          #Matriz de confusion. 
        cat("_____________________________________________________________\n")
        cat(paste0("Nivel de Precisión: ",sum(diag(tab))/sum(tab)*100,"%\n"))
    }) 
    
    #Evaluacion Testing Set
    output$RPrnt_C.ACoPEval2 <-renderPrint({ 
        #Input 
        pco <- isolate(ACoPReact())
        testing <- isolate(MatrixReact()[[3]])
        mymodel <- MNmodelMDSReac()[[1]]
        Zvar <- isolate(input$Select_ZvarClas)
        NumBox <- isolate(input$Box_Numerica2)
        
        #Chequeamos Input.
        if(is.null(pco)){
            return(NULL)
        }
        if(is.null(mymodel)){
            return(NULL)
        }
        if(is.null(testing)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Prediccion
        tst <- scale(testing[-ncol(testing)]) %*% pco$loadings
        tst <- data.frame(tst,testing[ncol(testing)])
        names(tst)[1:(ncol(tst)-1)] <- 
            paste0(rep("PC",(ncol(tst)-1)),1:(ncol(tst)-1))
        

        #Confusion Matrix and accuracy - training
        if(NumBox == TRUE){
            #Check de ingreso
            if("ZvarFac" %in% names(tst) == FALSE){
                return(NULL)
            }
            names(tst)[names(tst) == "ZvarFac"] <- "dep_Var"
        }else{
            #Check de ingreso
            if("ZvarFac" %in% names(tst) == TRUE){
                return(NULL)
            }
            names(tst)[names(tst) == Zvar] <- "dep_Var"
        }
        p <- predict(mymodel,tst)
        tab <- table(Predicted = p,Actual = tst$dep_Var)
        
        cat("_______________Evaluacion sobre el Testing Set_______________\n")
        cat("Matriz de Confusión\n")
        print(tab)                          #Matriz de confusion. 
        cat("_____________________________________________________________\n")
        cat(paste0("Nivel de Precisión: ",sum(diag(tab))/sum(tab)*100,"%\n"))
    }) 
    
    
    
    #===========================================================================
    #TAB PANEL: NON-METRIC MULTIDIMENSIONAL SCALING ////////////////////////////
    #===========================================================================
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: SELECTOR /////////////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #UI's RENDERIZADAS /////////////////////////////////////////////////////////
    
    #Lista de variables a ser incluidas en el ACP.
    #Genera SelectList_ACPvar
    output$RUi_checkboxGroupNMDS <- renderUI({
        #Input
        df <- ZvarFacReactClas()
        Zvar <- input$Select_ZvarClas
        
        #Esperamos Actualizacion.
        if(is.null(df)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Se habilita cuando la variable ingresada es factor.
        if(input$Box_Numerica2 == FALSE){
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
            valores <- NumericVars2
        }else{
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
            valores <- NumericVars2[!(NumericVars2 %in% c(Zvar,"ZvarFac"))]
        }
        
        #Seleccionadas
        selected <- valores[1:2]
        
        if(length(valores) >= 5){
            selected <- valores[1:5]
        }
        
        #Render UI.
        checkboxGroupInput("SelectList_NMDSvar","Variable:",
                           choices  = valores
                           ,
                           selected = selected
        )
    })
    
    #-------------------------------------
    #Selector de distancia.
    output$RUi_Select_NMDS.Distancia <- renderUI({
        
        selectInput("Select_NMDS.Distancia", "Medida:",
                    choices  = c("manhattan","euclidean","canberra","bray",
                        "kulczynski","jaccard","gower","altGower","morisita", 
                        "horn","mountford","raup" ,"binomial","chao","cao",
                        "mahalanobis"),
                    selected = "euclidean"
        )
    })
    
    #-------------------------------------
    #Selector Numero de dimensiones.
    output$RUi_Select_NMDSDim <- renderUI({
        #Input
        mat <- NMDSMatrixReact()[[1]]
        
        #Check de entrada.
        if(is.null(mat)){
            return(NULL)
        }
        #Numero maximo de autovewctores
        maximo <- dim(mat)[1] - 1
        
        numericInput("Select_NMDSDim","Cantidad:",
                     min = 1,max = maximo,value = 2,step = 1)
    })
    
    #-------------------------------------
    #Indicador de ejecucion.
    observeEvent(input$goButtonNMDSejecutar,{
        if(input$goButtonNMDSejecutar == 0){
            return(NULL)
        }
        if(input$goButtonNMDSejecutar %% 2 == 0){
            output$RText_C.EjecNMDS <-renderText({						
                x <- c("Ejecutado")
                x
            })
        }else{
            output$RText_C.EjecNMDS <-renderText({						
                x <- c("Realizado")
                x
            })
        }
    })
     
    #FUNCIONES REACTIVAS ///////////////////////////////////////////////////////
    
    #Recibe la tabla arrojada por ZvarFacReactClas y le hace subsetting por 
    #SelectList_ACPvar. Elimina las variables sin desviacion y Retorna el data 
    #frame. LAS VARIABLES NO ESTAN ESTANDARIZADAS
    NMDSMatrixReact <- reactive({
        #Action Button
        #input$goButtonACoPejecutar
        
        #Inputs
        df <- ZvarFacReactClas()
        Zvar <- input$Select_ZvarClas
        prop <- input$Select_C.Train/100
        BoxNum <- input$Box_Numerica2
        semilla <- input$Select_C.Seed
        
        #Recibimos inputs de los UI's renderizados.
        List_Vars <- input$SelectList_NMDSvar
        
        #Esperamos actualizacion.    
        if(is.null(df)){
            return(NULL)   
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(List_Vars)){
            return(NULL)   
        }
        if(is.null(prop)){
            return(NULL)   
        }
        if(is.null(semilla)){
            return(NULL)   
        }
        
        #Minimo dos variables deben ser seleccionadas.
        if(length(List_Vars) < 2){
            return(NULL)   
        }
        
        #Subsetting por variable.
        if(BoxNum == TRUE){
            #Check del input
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
            #Esperamos actualizacion
            if("ZvarFac" %in% List_Vars == TRUE){
                return(NULL)
            }
            df <- df[c(List_Vars,"ZvarFac")]
        }else{
            #Check del input
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
            #Esperamos actualizacion
            if(Zvar %in% List_Vars == TRUE){
                return(NULL)
            }
            df <- df[c(List_Vars,Zvar)]
        }
        
        #Eliminamos las variables que tengan una sd() igual a 0.
        varConDes <- which(sapply(df[,1:(ncol(df)-1)],function(x){
            sd(x) != 0
        }))
        df <- df[,c(varConDes,length(df))]
        
        #Chequeamos dimension del df.
        if(is.null(dim(df))){
            return(NULL)
        }else{
            if(0 %in% dim(df)){
                return(NULL)
            }
        }
        
        #Eliminamos filas repetidas.
        df <- df %>% unique()
        
        #Check final
        if(nrow(df) <= 2){
            return(NULL)
        }
        
        #Semilla Aleatoria
        set.seed(semilla)
        
        #Subsetting Training Set
        if(prop != 1){
            ##training and testing sets con muestreo estratificado.
            if(BoxNum == TRUE){
                inTrain <- createDataPartition(
                    df["ZvarFac"][,1], p = prop,list = FALSE)
            }else{
                inTrain <- createDataPartition(
                    df[Zvar][,1], p = prop,list = FALSE)
            }
            testing <- df[-inTrain,]
            training <- df[inTrain,]
        }else{
            testing <- NULL
            training <- df
        }

        #Damos formato a los datos para el NMDS.
        catVar <- training[names(training)[ncol(training)]]

        return(list(training,catVar,testing))
    })
    
    #---------------------------------------------------------
    #Realiza NMDS. Hay que estandarizar las variables primero.
    NMDSReact <- reactive({
        #Action Button
        input$goButtonNMDSejecutar
        
        #Detenemos ejecucion automatica
        if(input$goButtonNMDSejecutar == 0){
            return(NULL)
        }
        
        #Inputs
        training <- isolate(NMDSMatrixReact()[[1]])
        testing <- isolate(NMDSMatrixReact()[[3]])
        Zvar <- isolate(input$Select_ZvarClas)
        dis <- isolate(input$Select_NMDS.Distancia)
        dim <- isolate(input$Select_NMDSDim)
        
        #recibimos inputs de los UI's renderizados.
        List_Vars <- isolate(input$SelectList_NMDSvar)
        
        #Esperamos actualizacion.    
        if(is.null(training)){
            return(NULL)   
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(dis)){
            return(NULL)
        }
        if(is.null(dim)){
            return(NULL)
        }
        if(is.null(List_Vars)){
            return(NULL)   
        }
        
        #Eliminamos variable dependiente.
        training <- training[,-ncol(training)]

        #Modelo NMDS
        nmds <- metaMDS(training,distance = dis,k = dim,trace = FALSE,
            trymax = 100)
        
        #Inicializamos el nmds del testing set.
        nmdsTest <- NULL
        advert <- FALSE
        #Eliminamos variable dependiente.
        if(!is.null(testing)){
            testing <-  testing[,-ncol( testing)]
            
            #Modelo NMDS
            if(nrow(testing) <= dim){
                nmdsTest <- NULL
                advert <- TRUE
            }else{
                nmdsTest <- metaMDS(testing,distance = dis,k = dim,trace = FALSE,
                                    trymax = 100) 
            }

        }
        return(list(nmds,nmdsTest,advert))
        
    })
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: STRESS VS DIM ////////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////

    #UI's RENDERIZADAS /////////////////////////////////////////////////////////

    #Selector Numero maximo de dimensiones.
    output$RUi_Select_NMDSMaxDim <- renderUI({
        numericInput("Select_NMDSMaxDim","Selector:",
                     min = 1,max = 12,value = 8,step = 1)
    })
    
    #-------------------------------------
    #Selector Numero de inicios aleatorios para buscar la solucion estable.
    output$RUi_Select_NMDSMaxTry <- renderUI({
        numericInput("Select_NMDSMaxTry","Selector:",
                     min = 10,max = 200,value = 100,step = 1)
    })
    
    #PLOT STRESS VS DIMENSIONS /////////////////////////////////////////////////
    
    #Gradico de stress vs dimensiones
    output$RPlot_C.NMDSstress <-renderPlot({ 
        #Action Button
        input$goButtonNMDSstress
        
        #Input 
        training <- isolate(NMDSMatrixReact()[[1]])
        distance <- isolate(input$Select_NMDS.Distancia)
        kmax <- isolate(input$Select_NMDSMaxDim)
        trymax <- isolate(input$Select_NMDSMaxTry)
        
        #Chequeamos Input.
        if(is.null(training)){
            return(NULL)
        }
        if(is.null(distance)){
            return(NULL)
        }
        if(is.null(kmax)){
            return(NULL)
        }
        if(is.null(trymax)){
            return(NULL)
        }
        
        #Eliminamos variable dependiente.
        training <- training[,-ncol(training)]
        
        #Calculamos la relacion entre las dimensiones y el stress
        Stress <- scree_values(training,distance = distance, kmax = kmax,
            trymax = trymax) 
        
        Dimensions <- as.vector(sapply(1:(length(Stress)/10),function(x){
                rep(x,10)},simplify="vector"))

        dfgg <- data.frame(Dimensions = Dimensions,Stress = Stress)
        graph <- ggplot(dfgg,aes(Dimensions,Stress)) +
            geom_point(size = 2) +
            geom_line() +
            ggtitle("Stress vs Dimensions") + 
            geom_ribbon(aes(ymin=0.2,ymax=max(Stress,0.3)),
                        fill="red",alpha=0.2) + 
            geom_ribbon(ymin=0.1,ymax=0.2,fill="yellow",alpha=0.2) + 
            geom_ribbon(ymin=0.05,ymax=0.1,fill="green",alpha=0.2) + 
            geom_ribbon(ymin=0,ymax=0.05,fill="blue",alpha=0.2) +
            scale_x_continuous(breaks = 1:max(Dimensions))
        graph
    }) 
    
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: RESUMEN //////////////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #Resumen del NMDS
    output$RPrnt_C.TablaNMDS <-renderPrint({ #Asi funciona no se por que...
        #Input
        nmds <- NMDSReact()[[1]]
        advert <- NMDSReact()[[3]]
        
        #Chequeamos Input.
        if(is.null(nmds)){
            return(NULL)
        }
        if(advert == TRUE){
            print("_______________ARVERTENCIA_________________")
            print("El conjunto de prueba tiene muy pocos datos")
            print("Utilice un número menor de dimensiones o incremente la ")
            print("cantidad de datos asignados al set de prueba")
            print("///////////////////////////////////////////")
        }else{
            print(nmds)  
        }
        
        
    })
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: SHEPARD //////////////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////

    output$RPlot_C.NMDSShep <- renderPlot({
        #Action Button
        input$goButtonNMDSShep

        #Detener plot automatica
        if(input$goButtonNMDSShep == 0){
            return(NULL)
        }
        #Input 
        nmds <- isolate(NMDSReact()[[1]])
        Zvar  <- isolate(input$Select_ZvarClas)
        
        #Chequeamos Input.
        if(is.null(nmds)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }

        ##Plot with colors
        stressplot(nmds, main = paste0("Shepard Plot. Variable: ",Zvar))
    })
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: CP vs CP /////////////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #UI's RENDERIZADAS /////////////////////////////////////////////////////////
    
    #Selector de la componente 1.
    output$RUi_Select_C.NMDSComp1 <- renderUI({
        #Input
        dimension <- input$Select_NMDSDim
        
        #Check de entrada.
        if(is.null(dimension)){
            return(NULL)
        }
        #Numero maximo de autovewctores
        maximo <- dimension
        
        numericInput("Select_C.NMDSComp1","NMDS #1",
                     min = 1,max = maximo,value = 1,step = 1)
    })
    
    #--------------------------------------------
    #Selector de la componente 2.
    #Genera Select_C.Comp22
    output$RUi_Select_C.NMDSComp2 <- renderUI({
        #Input
        dimension <- input$Select_NMDSDim
        
        #Check de entrada.
        if(is.null(dimension)){
            return(NULL)
        }
        #Numero maximo de autovewctores
        maximo <- dimension
        
        numericInput("Select_C.NMDSComp2","NMDS #2",
                     min = 1,max = maximo,value = 2,step = 1)
    })
    
    #----------------------------------------
    #Mostrar leyenda..
    #Genera Box_CCP.CvsC.Leg
    output$RUi_checkbox.C.NMDS.Leg <- renderUI({
        checkboxInput("Box_C.NMDS.Leg","Leyenda",value=F)
    })
    
    #----------------------------------------
    #Opciones de eleccion para los valores de Zvar.
    #Genera SelectList_Zval3
    output$RUi_checkboxGroup.C.NMDS <- renderUI({
        #Input
        catVar <- as.character(NMDSMatrixReact()[[2]][,1])
        
        #Esperamos Actualizacion.
        if(is.null(catVar)){
            return(NULL)
        }
        
        valores <- as.character(unique(catVar))
        
        #Render UI.
        checkboxGroupInput("SelectList_C.NMDS",
                           "Selector de valores.",
                           choices  = valores
                           ,
                           selected = valores[1:2]
        )
    })
    
    #GRAFICO NMDS //////////////////////////////////////////////////////////////
    
    #Grafico NMDS
    output$RPlot_C.NMDS <- renderPlot({
        #Action Button
        input$goButtonNMDS

        #Detener plot automatica
        if(input$goButtonNMDS == 0){
            return(NULL)
        }
        #Input 
        nmds <- isolate(NMDSReact()[[1]])
        Zvar  <- isolate(input$Select_ZvarClas)
        dis <- isolate(input$Select_NMDS.Distancia)
        comp1 <- isolate(input$Select_C.NMDSComp1)
        comp2 <- isolate(input$Select_C.NMDSComp2)
        BoxNum <- isolate(input$Box_Numerica2)
        catVar <- isolate(NMDSMatrixReact()[[2]])
        leyenda <- isolate(input$Box_C.NMDS.Leg)
        List_vals <- isolate(input$SelectList_C.NMDS)
        dimension <- isolate(input$Select_NMDSDim)
        
        #Chequeamos Input.
        if(is.null(nmds)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(dis)){
            return(NULL)
        }
        if(is.null(catVar)){
            return(NULL)
        }
        if(is.null(leyenda)){
            return(NULL)
        }
        if(is.null(List_vals)){
            return(NULL)
        }
        if(is.null(dimension)){
            return(NULL)
        }
        if(is.null(comp1)){
            return(NULL)
        }
        if(is.null(comp2)){
            return(NULL)
        }
        if(dimension != 1){
            if(comp1 == comp2){
                return(NULL)
            }
        }
        if(comp2 < comp1){
            aux <- comp1
            comp1 <- comp2
            comp2 <- aux
        }
        ##Plot with colors
        sit.sc <- scores(nmds)
        
        if(dimension == 1){
            df <- data.frame(NMDS1 = sit.sc[,comp1],NMDS2 = rep(0,nrow(sit.sc)),
                 catVar = catVar[,1])
        }else{
            df <- data.frame(NMDS1 = sit.sc[,comp1],NMDS2 = sit.sc[,comp2],
                 catVar = catVar[,1])
        }
        
        #Subsetting de df
        df <- df[df$catVar %in% List_vals,]
        
        #Chequeamos dimension.
        if(nrow(df) == 0){
            return(NULL)
        } 
        
        graph <- ggplot(df,aes(NMDS1,NMDS2,col = as.factor(catVar))) +
            geom_point(size = 4) +
            ggtitle(paste0("NMDS Stress = ",round(nmds$stress,3))) +
            scale_color_discrete(name = Zvar)
        
        #Leyenda
        if(leyenda == TRUE){
            graph 
        }else{
            graph + theme(legend.position="none")
        }
        
    })
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: Goodness /////////////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////

    #UI's RENDERIZADAS /////////////////////////////////////////////////////////
    
    #Selector de la componente 1.
    output$RUi_Select_C.NMDSComp12 <- renderUI({
        #Input
        dimension <- input$Select_NMDSDim
        
        #Check de entrada.
        if(is.null(dimension)){
            return(NULL)
        }
        #Numero maximo de autovewctores
        maximo <- dimension
        
        numericInput("Select_C.NMDSComp12","NMDS #1",
                     min = 1,max = maximo,value = 1,step = 1)
    })
    
    #--------------------------------------------
    #Selector de la componente 2.
    #Genera Select_C.Comp22
    output$RUi_Select_C.NMDSComp22 <- renderUI({
        #Input
        dimension <- input$Select_NMDSDim
        
        #Check de entrada.
        if(is.null(dimension)){
            return(NULL)
        }
        #Numero maximo de autovewctores
        maximo <- dimension
        
        numericInput("Select_C.NMDSComp22","NMDS #2",
                     min = 1,max = maximo,value = 2,step = 1)
    })
    
    #----------------------------------------
    #Mostrar leyenda..
    #Genera Box_CCP.CvsC.Leg
    output$RUi_checkbox.C.NMDS.Leg2 <- renderUI({
        checkboxInput("Box_C.NMDS.Leg2","Leyenda",value=F)
    })
    
    #----------------------------------------
    #Opciones de eleccion para los valores de Zvar.
    #Genera SelectList_Zval3
    output$RUi_checkboxGroup.C.NMDS2 <- renderUI({
        #Input
        catVar <- as.character(NMDSMatrixReact()[[2]][,1])
        
        #Esperamos Actualizacion.
        if(is.null(catVar)){
            return(NULL)
        }
        
        valores <- as.character(unique(catVar))
        
        #Render UI.
        checkboxGroupInput("SelectList_C.NMDS2",
                           "Selector de valores.",
                           choices  = valores
                           ,
                           selected = valores[1:2]
        )
    })
    
    #----------------------------------------
    #Opciones de eleccion de Goodness.
    output$RUi_checkboxGroup.C.NMDSGood <- renderUI({
        #Input
        nmds <- NMDSReact()[[1]]
        
        #Esperamos Actualizacion.
        if(is.null(nmds)){
            return(NULL)
        }
        
        gof <- goodness(nmds)
        max.gof <- max(gof)
        
        #Parche
        if(max.gof != 0){
            point.size <- 5 / max.gof
        }else{
            point.size <- 5
        }
        
        valores <- unique(ceiling(gof*point.size))
        
        #Render UI.
        checkboxGroupInput("SelectList_C.NMDSGood",
                           "Valores de Goodness.",
                           choices  = valores
                           ,
                           selected = valores[1:2]
        )
    })
    #GRAFICO NMDS //////////////////////////////////////////////////////////////
    
    #Grafico NMDS
    output$RPlot_C.NMDSGood <- renderPlot({
        #Action Button
        input$goButtonNMDSGood

        #Detener plot automatica
        if(input$goButtonNMDSGood == 0){
            return(NULL)
        }
        #Input 
        nmds <- isolate(NMDSReact()[[1]])
        Zvar  <- isolate(input$Select_ZvarClas)
        dis <- isolate(input$Select_NMDS.Distancia)
        comp1 <- isolate(input$Select_C.NMDSComp12)
        comp2 <- isolate(input$Select_C.NMDSComp22)
        BoxNum <- isolate(input$Box_Numerica2)
        catVar <- isolate(NMDSMatrixReact()[[2]])
        leyenda <- isolate(input$Box_C.NMDS.Leg2)
        List_vals <- isolate(input$SelectList_C.NMDS2)
        Goodness <- isolate(input$SelectList_C.NMDSGood)
        dimension <- isolate(input$Select_NMDSDim)
        
        #Chequeamos Input.
        if(is.null(nmds)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(dis)){
            return(NULL)
        }
        if(is.null(catVar)){
            return(NULL)
        }
        if(is.null(leyenda)){
            return(NULL)
        }
        if(is.null(List_vals)){
            return(NULL)
        }
        if(is.null(Goodness)){
            return(NULL)
        }
        if(is.null(dimension)){
            return(NULL)
        }
        if(is.null(comp1)){
            return(NULL)
        }
        if(is.null(comp2)){
            return(NULL)
        }
        if(dimension != 1){
            if(comp1 == comp2){
                return(NULL)
            }
        }
        if(comp2 < comp1){
            aux <- comp1
            comp1 <- comp2
            comp2 <- aux
        }
        ##Plot
        gof <- goodness(nmds)
        max.gof <- max(gof)
        
        #Parche
        if(max.gof != 0){
            point.size <- 5 / max.gof
        }else{
            point.size <- 5
        }
        sit.sc <- scores(nmds)  

        ##Agregamos "sitios"
        if(dimension == 1){
            df <- data.frame(NMDS1 = sit.sc[,comp1],NMDS2 = rep(0,nrow(sit.sc)),
                catVar = catVar[,1],Gof = as.integer(ceiling(gof*point.size)))
        }else{
            df <- data.frame(NMDS1 = sit.sc[,comp1],NMDS2 = sit.sc[,comp2],
                catVar = catVar[,1],Gof = as.integer(ceiling(gof*point.size)))
        }

        #Subsetting de df
        df <- df[df$catVar %in% List_vals,]
        
        #Chequeamos dimension.
        if(nrow(df) == 0){
            return(NULL)
        }
        
        #Subsetting de df
        df <- df[df$Gof %in% Goodness,]
        
        #Chequeamos dimension.
        if(nrow(df) == 0){
            return(NULL)
        }
        
        graph <- ggplot(df,aes(NMDS1,NMDS2,col = as.factor(catVar))) +
        	geom_point(aes(size = as.integer(Gof))) +
            ggtitle("NMDS Goodness of Fit") +
            scale_color_discrete(name = Zvar) +
            guides(colour = guide_legend(override.aes = list(size = 4))) +
            scale_size_continuous(range = c(4,8),name = "Goodness") 
        
        ## Agregamos "species"
        dfspe <- as.data.frame(nmds$specie)
        dfspe$Label <- as.character(rownames(dfspe))
        
        #Agregamos en caso de que haga falta
        if(dimension == 1){
            dfspe$MDS2 <- rep(0,nrow(dfspe))
        }
        graph <- graph + 
            geom_point(dfspe, mapping = aes(x=MDS1,y=MDS2), 
                col = "red", shape = "X",size = 4) +
            geom_text(dfspe,mapping = aes(x=MDS1,y=MDS2,label=Label),
                vjust=1.6,size=3,col = "red")
       
        #Leyenda
        if(leyenda == TRUE){
            graph 
        }else{
            graph + theme(legend.position="none")
        }
        
    })
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: BIPLOT ///////////////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////

    #UI's RENDERIZADAS /////////////////////////////////////////////////////////
    
    #Selector de la componente 1.
    output$RUi_Select_C.NMDSComp123 <- renderUI({
        #Input
        dimension <- input$Select_NMDSDim
        
        #Check de entrada.
        if(is.null(dimension)){
            return(NULL)
        }
        #Numero maximo de autovewctores
        maximo <- dimension
        
        numericInput("Select_C.NMDSComp123","NMDS #1",
                     min = 1,max = maximo,value = 1,step = 1)
    })
    
    #--------------------------------------------
    #Selector de la componente 2.
    #Genera Select_C.Comp22
    output$RUi_Select_C.NMDSComp223 <- renderUI({
        #Input
        dimension <- input$Select_NMDSDim
        
        #Check de entrada.
        if(is.null(dimension)){
            return(NULL)
        }
        #Numero maximo de autovewctores
        maximo <- dimension
        
        numericInput("Select_C.NMDSComp223","NMDS #2",
                     min = 1,max = maximo,value = 2,step = 1)
    })
    
    #----------------------------------------
    #Mostrar etiquetas
    output$RUi_checkbox.C.NMDS.Etiq <- renderUI({
        checkboxInput("Box_C.NMDS.Etiq","Etiquetas",value=F)
    })
    
    #GRAFICO NMDS //////////////////////////////////////////////////////////////
    
    #Grafico NMDS
    output$RPlot_C.NMDSBip <- renderPlot({
        #Action Button
        input$goButtonNMDSBip

        #Detener plot automatica
        if(input$goButtonNMDSBip == 0){
            return(NULL)
        }
        #Input 
        nmds <- isolate(NMDSReact()[[1]])
        Zvar  <- isolate(input$Select_ZvarClas)
        comp1 <- isolate(input$Select_C.NMDSComp123)
        comp2 <- isolate(input$Select_C.NMDSComp223)
        catVar <- NMDSMatrixReact()[[2]][,1]
        Etiqueta <- isolate(input$Box_C.NMDS.Etiq)
        training <- isolate(NMDSMatrixReact()[[1]])
        dimension <- isolate(input$Select_NMDSDim)
        
        #Chequeamos Input.
        if(is.null(nmds)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(Etiqueta)){
            return(NULL)
        }
        if(is.null(catVar)){
            return(NULL)
        }
        if(is.null(training)){
            return(NULL)
        }
        if(is.null(dimension)){
            return(NULL)
        }
        if(is.null(comp1)){
            return(NULL)
        }
        if(is.null(comp2)){
            return(NULL)
        }
        if(dimension != 1){
            if(comp1 == comp2){
                return(NULL)
            }
        }
        if(comp2 < comp1){
            aux <- comp1
            comp1 <- comp2
            comp2 <- aux
        }

        #Eliminamos variable dependiente.
        training <- training[,-ncol(training)]
        
        #BIPLOT 
        fit <- envfit(nmds,training,permutations=999) 		 
        fit
        
        sit.sc <- as.data.frame(scores(nmds))
        col.group <- as.factor(catVar)
        
        if(dimension == 1){
            plot(sit.sc[,comp1],rep(0,nrow(sit.sc)),
                 main = paste0("Biplot. Variable: ",Zvar), col = col.group,
                 pch = 19,xlab = paste0("NMDS ",comp1))
            
            fit$vectors$arrows <- as.data.frame(fit$vectors$arrows)
            fit$vectors$arrows[,1] <- fit$vectors$arrows[,1]/5
            fit$vectors$arrows[,2] <- seq(1,nrow(fit$vectors$arrows)) -
                mean(seq(1,nrow(fit$vectors$arrows)))
            colnames(fit$vectors$arrows)[2] <- "NMDS2"
            fit$vectors$arrows <- as.matrix(fit$vectors$arrows)
            
        }else{
            plot(sit.sc[,comp1],sit.sc[,comp2],
                 main = paste0("Biplot. Variable: ",Zvar), col = col.group,
                 pch = 19,xlab = paste0("NMDS ",comp1),
                 ylab = paste0("NMDS ",comp2)) 
        }
        plot(fit,p.max = 0.05) 								

        #Etiqueta
        if(Etiqueta == TRUE){
            text(sit.sc,as.character(catVar),pos = 4, cex = 0.7)
        }
        
    })
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: Procrustes ///////////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #UI's RENDERIZADAS ---------------------------------------------------------
    
    #Selector de distancia.
    output$RUi_Select_NMDS.Distmed <- renderUI({
        
        selectInput("Select_NMDS.Distmed", "Medida de disimilitud a comparar:",
                    choices  = c("manhattan","euclidean","canberra","bray",
                        "kulczynski","jaccard","gower","altGower","morisita", 
                        "horn","mountford","raup" ,"binomial","chao","cao",
                        "mahalanobis"),
                    selected = "euclidean"
        )
    })
    
    #FUNCIONES REACTIVAS -------------------------------------------------------
    
    #Comparador
    NMDSmedReact <- reactive({
        
        #Input 
        nmds <- NMDSReact()[[1]]
        Zvar  <- input$Select_ZvarClas
        training <- NMDSMatrixReact()[[1]]
        dis <- input$Select_NMDS.Distmed
        
        #Chequeamos Input.
        if(is.null(nmds)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(training)){
            return(NULL)   
        }
        if(is.null(dis)){
            return(NULL)
        }
        
        #Eliminamos variable dependiente.
        training <- training[,-ncol(training)]
        
        #Comparador
        nmds2 <- metaMDS(training,distance = dis,trace = FALSE,trymax = 100)
        pro <- procrustes(nmds,nmds2)
        
        return(pro)
    })
    
    #PLOTS ---------------------------------------------------------------------
    
    #Grafico 1
    output$RPlot_C.NMDSmed1 <- renderPlot({
        #Action Button
        input$goButtonNMDSmed
        
        #Detener plot automatica
        if(input$goButtonNMDSmed == 0){
            return(NULL)
        }
        #Input 
        pro <- isolate(NMDSmedReact())
        
        #Chequeamos Input.
        if(is.null(pro)){
            return(NULL)
        }
        
        plot(pro,cex  = 1.5) 
        
    })
    
    #-----------------------------------------
    #Grafico 2
    output$RPlot_C.NMDSmed2 <- renderPlot({
        #Action Button
        input$goButtonNMDSmed
        
        #Detener plot automatica
        if(input$goButtonNMDSmed == 0){
            return(NULL)
        }
        
        #Input 
        pro <- isolate(NMDSmedReact())
        
        #Chequeamos Input.
        if(is.null(pro)){
            return(NULL)
        }
        
        plot(pro,kind = 2)
        
    })
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: Evaluacion ///////////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #REACTIVE MULTINOMIAL MODEL ------------------------------------------------
    MNmodelNMDSReac <- reactive({
        #Input
        nmds <- NMDSReact()[[1]]
        training <- isolate(NMDSMatrixReact()[[1]])
        Zvar <- isolate(input$Select_ZvarClas)
        NumBox <- isolate(input$Box_Numerica2)

        #Chequeamos Input.
        if(is.null(nmds)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(training)){
            return(NULL)
        }

        #Prediccion
        trg <- nmds$points
        #trg <- scale(trg)
        trg <- data.frame(trg,training[ncol(training)])
        
        #Multinomial Logistic Regresion with the First Two PCs.
        if(NumBox == TRUE){
            #Check de ingreso
            if("ZvarFac" %in% names(trg) == FALSE){
                return(NULL)
            }
            names(trg)[names(trg) == "ZvarFac"] <- "dep_Var"
            mymodel <-
                nnet::multinom(dep_Var ~ .,data = trg)#,MaxNWts =100000)
        }else{
            #Check de ingreso
            if("ZvarFac" %in% names(trg) == TRUE){
                return(NULL)
            }
            names(trg)[names(trg) == Zvar] <- "dep_Var"
            mymodel <-
                nnet::multinom(dep_Var ~ .,data = trg)#,MaxNWts =100000)
        }

        return(list(mymodel,trg))
    })

    #PRINTS --------------------------------------------------------------------
    #Evaluacion Training Set
    output$RPrnt_C.NMDSEval1 <-renderPrint({
        #Input
        trg <- MNmodelNMDSReac()[[2]]
        mymodel <- MNmodelNMDSReac()[[1]]
        Zvar <- isolate(input$Select_ZvarClas)
        NumBox <- isolate(input$Box_Numerica2)

        #Chequeamos Input.
        if(is.null(mymodel)){
            return(NULL)
        }
        if(is.null(trg)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }

        p <- predict(mymodel,trg)
        tab <- table(Predicted = p,Actual = trg$dep_Var)

        cat("_____________________________________________________________\n")
        print(summary(mymodel))             #Resumen del modelo.
        cat("______________Evaluacion sobre el Training Set_______________\n")
        cat("Matriz de Confusión\n")
        print(tab)                          #Matriz de confusion.
        cat("_____________________________________________________________\n")
        cat(paste0("Nivel de Precisión: ",sum(diag(tab))/sum(tab)*100,"%\n"))
    })

    #Evaluacion Testing Set
    output$RPrnt_C.NMDSEval2 <-renderPrint({
        #Input
        nmdsTest <- isolate(NMDSReact()[[2]])
        testing <- isolate(NMDSMatrixReact()[[3]])
        mymodel <- MNmodelNMDSReac()[[1]]
        Zvar <- isolate(input$Select_ZvarClas)
        NumBox <- isolate(input$Box_Numerica2)

        #Chequeamos Input.
        if(is.null(nmdsTest)){
            return(NULL)
        }
        if(is.null(mymodel)){
            return(NULL)
        }
        if(is.null(testing)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }

        #Prediccion
        tst <- nmdsTest$points
        #tst <- scale(tst)
        tst <- data.frame(tst,testing[ncol(testing)])
        
        #Confusion Matrix and accuracy - training
        if(NumBox == TRUE){
            #Check de ingreso
            if("ZvarFac" %in% names(tst) == FALSE){
                return(NULL)
            }
            names(tst)[names(tst) == "ZvarFac"] <- "dep_Var"
        }else{
            #Check de ingreso
            if("ZvarFac" %in% names(tst) == TRUE){
                return(NULL)
            }
            names(tst)[names(tst) == Zvar] <- "dep_Var"
        }
        p <- predict(mymodel,tst)
        tab <- table(Predicted = p,Actual = tst$dep_Var)

        cat("_______________Evaluacion sobre el Testing Set_______________\n")
        cat("Matriz de Confusión\n")
        print(tab)                          #Matriz de confusion.
        cat("_____________________________________________________________\n")
        cat(paste0("Nivel de Precisión: ",sum(diag(tab))/sum(tab)*100,"%\n"))
    })

    #===========================================================================
    #TAB PANEL: LINEAR DISCRIMINANT ANALYSIS ///////////////////////////////////
    #===========================================================================
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: Selector Variables ///////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #UI's RENDERIZADAS /////////////////////////////////////////////////////////

    #Lista de variables a ser incluidas en el LDA.
    output$RUi_checkboxGroupLDA <- renderUI({
        #Input
        df <- ZvarFacReactClas()
        Zvar <- input$Select_ZvarClas
        
        #Esperamos Actualizacion.
        if(is.null(df)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Se habilita cuando la variable ingresada es factor.
        if(input$Box_Numerica2 == FALSE){
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
            valores <- NumericVars2
        }else{
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
            valores <- NumericVars2[!(NumericVars2 %in% c(Zvar,"ZvarFac"))]
        }
        
        #Seleccionadas
        selected <- valores[1:2]
        
        if(length(valores) >= 5){
            selected <- valores[1:5]
        }
        
        #Render UI.
        checkboxGroupInput("SelectList_LDAvar","Selector de variables.",
                           choices  = valores
                           ,
                           selected = selected
        )
    })
    
    #-------------------------------------
    #Indicador de ejecucion.
    observeEvent(input$goButtonLDAejecutar,{
        output$RText_C.EjecLDA <-renderText({						
            x <- c("Remueva variables con nzv = true")
            x
        })
    })

    #FUNCIONES REACTIVAS ///////////////////////////////////////////////////////
    
    #Recibe la tabla arrojada por ZvarFacReactClas y le hace subsetting por 
    #SelectList_ACPvar. Elimina las variables sin desviacion y Retorna el data 
    #frame. LAS VARIABLES NO ESTAN ESTANDARIZADAS
    LReact <- reactive({
        #Action Button
        input$goButtonLDAejecutar
        
        #Evitamos ejecucion automatica
        if(input$goButtonLDAejecutar == 0){
            return(NULL)
        }
        
        #Inputs
        df <- isolate(ZvarFacReactClas())
        df2 <- isolate(ZvarFacReactClas())
        Zvar <- isolate(input$Select_ZvarClas)
        prop <- isolate(input$Select_C.Train/100)
        BoxNum <- isolate(input$Box_Numerica2)
        semilla <- isolate(input$Select_C.Seed)
        
        #Recibimos inputs de los UI's renderizados.
        List_Vars <- isolate(input$SelectList_LDAvar)
        CatVar_list <- isolate(input$Select_ClasCV)
        
        #Esperamos actualizacion.    
        if(is.null(df)){
            return(NULL)   
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(List_Vars)){
            return(NULL)   
        }
        if(is.null(prop)){
            return(NULL)   
        }
        if(is.null(semilla)){
            return(NULL)   
        }
        
        #Minimo dos variables deben ser seleccionadas.
        if(length(List_Vars) < 2){
            return(NULL)   
        }

        #Subsetting por variable.
        if(BoxNum == TRUE){
            #Check del input
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
            #Esperamos actualizacion
            if("ZvarFac" %in% List_Vars == TRUE){
                return(NULL)
            }
            df <- df[c(List_Vars,"ZvarFac")]
        }else{
            #Check del input
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
            #Esperamos actualizacion
            if(Zvar %in% List_Vars == TRUE){
                return(NULL)
            }
            df <- df[c(List_Vars,Zvar)]
        }

        #Eliminamos las variables que tengan una sd() igual a 0.
        varConDes <- which(sapply(df[,1:(ncol(df)-1)],function(x){
            sd(x) != 0
        }))
        df <- df[,c(varConDes,length(df))]
        
        #Chequeamos dimension del df.
        if(is.null(dim(df))){
            return(NULL)
        }else{
            if(0 %in% dim(df)){
                return(NULL)
            }
        }
        
        #Agregamos a la lista de variables numericas, las categoricas.
        if(length(CatVar_list) != 0){
            df <- cbind(df2[CatVar_list],df)
        }
        
        #Eliminamos filas repetidas.
        df <- df %>% unique() 
        
        #Check final
        if(nrow(df) <= 2){
            return(NULL)
        }
        
        #Semilla aleatoria
        set.seed(semilla)
        
        #Subsetting Training Set
        if(prop != 1){
            ##training and testing sets con muestreo estratificado.
            if(BoxNum == TRUE){
                inTrain <- createDataPartition(
                    df["ZvarFac"][,1], p = prop,list = FALSE)
            }else{
                inTrain <- createDataPartition(
                    df[Zvar][,1], p = prop,list = FALSE)

            }
            test <- df[-inTrain,]
            df <- df[inTrain,]
        }else{
            test <- NULL
        }
        return(list(df,test))
    })
    
    #--------------------------------------
    #Informacion sobre varianza 
    output$RTable_C.LDA.var <-renderTable({ 
        #Action
        input$Select_ClasCV
        input$SelectList_LDAvar
        
        #Input 
        df <- LReact()[[1]]
        
        #Chequeamos Input.
        if(is.null(df)){
            return(NULL)
        }
        #Lo activamos
        LDAReact()
        
        df <- df[-ncol(df)]
        
        x <- nearZeroVar(df, saveMetrics = TRUE)
        x$Predictors <- rownames(x)
        x <- x[c(ncol(x),1:(ncol(x)-1))]
        x 
    }) 
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: Plot Correlaciones ///////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #PLOT DE CORRELACIONES /////////////////////////////////////////////////////
    
    #Plot de correlaciones
    output$RPlot_C.LDA.PP <- renderPlot({
        #Input 
        df <- LReact()[[1]]
        
        #Chequeamos Input
        if(is.null(df)){
            return(NULL)
        }
    
        #Plot de correlaciones 
        psych :: pairs.panels(df, gap = 0, pch = 21,
                            bg = df[,ncol(df)], 
                            main = "Correlación de Pearson")
    })
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: RESUMEN //////////////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #FUNCIONES REACTIVAS ///////////////////////////////////////////////////////
    
    #Realiza LDA. Hay que estandarizar las variables primero.
    LDAReact <- reactive({
        #Inputs
        df <- LReact()[[1]]
        Zvar  <- isolate(input$Select_ZvarClas)
       
        #Esperamos actualizacion.    
        if(is.null(df)){
            return(NULL)   
        }
        if(is.null(Zvar)){
            return(NULL)   
        }
        
        #Formato
        rownames(df) <- 1:nrow(df)

        #Extraemos la variable categorica
        catVar <- names(df)[ncol(df)] 
        names(df)[ncol(df)] <- "dep_Var"

        #Hacemos LDA.
        linear <- try(MASS :: lda(dep_Var ~.,df),silent= T)
        if(class(linear) == "try-error"){
            output$RText_C.LDAadv <-renderText({						
                adv <- paste0('El conjunto de entrenamiento recibió muy pocos 
                               datos en los niveles de la variable "',Zvar,
                              '" con la seleccion de predictores realizada.')
                adv
            })
            return(NULL)
        }else{
            #Registro de ejecucion.
            if(input$goButtonLDAejecutar == 0){
                return(NULL)
            }
            if(input$goButtonLDAejecutar %% 2 == 0){
                output$RText_C.LDAadv <-renderText({						
                    adv <- paste0("Ejecutado")
                    adv
                })
            }else{
                output$RText_C.LDAadv <-renderText({						
                    adv <- paste0("Realizado")
                    adv
                })
            }
        }
        return(linear)
    })
    
    #RESUMEN DEL LDA ///////////////////////////////////////////////////////////
    
    #Matriz de Autovectores
    output$RPrnt_C.LDA.Res <-renderPrint({ #Asi funciona no se por que...
        #Input 
        linear <- LDAReact()
        #Chequeamos Input.
        if(is.null(linear)){
            return(NULL)
        }
        print(linear) 
    }) 
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: HISTOGRAMA //////////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #UI's RENDERIZADAS /////////////////////////////////////////////////////////
    
    #Selector del discriminante.
    output$RUi_Select_C.LDA.Disc <- renderUI({
        #Input
        linear <- LDAReact()
        
        #Check de entrada.
        if(is.null(linear)){
            return(NULL)
        }
        #Numero maximo de discriminantes
        maximo <-  ncol(linear$scaling)
        
        numericInput("Select_C.LDA.Disc","Discriminante a graficar:",
                     min = 1,max = maximo,value = 1,step = 1)
    })
    
    #HISTOGRAMAS ///////////////////////////////////////////////////////////////
    
    output$RPlot_C.LDA.Hist <- renderPlot({
        #Active Button
        input$goButtonLDAHist
        
        #Input 
        linear <- isolate(LDAReact())
        df <- isolate(LReact()[[1]])
        nroDis <- isolate(input$Select_C.LDA.Disc)
        
        #Chequeamos Input.
        if(is.null(linear)){
            return(NULL)
        }
        if(is.null(df)){
            return(NULL)
        }
        if(is.null(nroDis)){
            return(NULL)
        }
     
        #Histograms
        names(df)[ncol(df)] <- "dep_Var"
        
        p <- predict(linear,df)
        
        graph <- try(MASS :: ldahist(data=p$x[,nroDis],g=df$dep_Var),silent= T)
        if(class(graph) == "try-error"){
            return(plot(1:2, type='n', main = "El número de agrupaciones que se
                                    forman es muy grande para este gráfico."))
        }
        graph
    })
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: BIPLOT ///////////////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #UI's RENDERIZADAS /////////////////////////////////////////////////////////
    
    #Mostrar leyenda.
    output$RUi_checkbox.C.LDA.Biplot.Leg <- renderUI({
        checkboxInput("Box_C.LDA.Biplot.Leg","Leyenda",value=F)
    })
    
    #GRAFICO BIPLOT ////////////////////////////////////////////////////////////
    
    #Grafico Biplot
    output$RPlot_C.LDA.Biplot <- renderPlot({
        #Active Button
        input$goButtonLDABiplot
        
        #Input 
        df  <- isolate(LReact()[[1]])
        linear <- isolate(LDAReact())
        Zvar  <- isolate(input$Select_ZvarClas)
        leyenda <- isolate(input$Box_C.LDA.Biplot.Leg)
        BoxNum <- isolate(input$Box_Numerica2)
        
        #Chequeamos Input.
        if(is.null(linear)){
            return(NULL)
        }
        if(is.null(df)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(leyenda)){
            return(NULL)
        }
        
        #Chequeamos input y extraemos variable dependiente.
        if(BoxNum == TRUE){
            #Esperamos actualizacion
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
        }else{
            #Esperamos actualizacion
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
        }
        names(df)[ncol(df)] <- "dep_Var"
        
        #La variable a explicar debe tener al menos tres niveles para generar el
        #biplot. Caso contrario ploteamos densidad.
        if(length(levels(df$dep_Var)) <= 2){
            #Grafico de Densidad
            p <- predict(linear,df)
            #Data set de ggplot
            dfaux <- data.frame(LD1 = p$x[,1], class = as.factor(df$dep_Var))
            graph <- ggplot(data = dfaux) +
                geom_density(aes(LD1, fill = class), alpha = 0.5) +
                scale_fill_discrete(name = Zvar)
        }else{
            #Grafico Biplot 
            graph <- try(ggord :: ggord(linear,df$dep_Var) +
                             ggtitle(paste0("Gráfico Biplot. Variable ",Zvar)) 
                         ,silent= T)
            if(class(graph) == "try-error"){
                return(plot(1:2, type='n', main = "El plot no se pudo generar. 
                    Posiblemente las variables presentan alta colinealidad."))
            }
        }

        #Leyenda
        if(leyenda == TRUE){
            graph 
        }else{
            graph + theme(legend.position="none")
        }
    })
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: Partition Plot ///////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////

    #GRAFICO SCATTERPLOT ///////////////////////////////////////////////////////
    
    #Grafico Partition
    output$RPlot_C.LDA.Part <- renderPlot({
        #Active Button
        input$goButtonLDAPart
        
        #Evitamos plot automatico
        if(input$goButtonLDAPart == 0){
            return(NULL)
        }
        
        #Input 
        df  <- isolate(LReact()[[1]])
        Zvar  <- isolate(input$Select_ZvarClas)
        BoxNum <- isolate(input$Box_Numerica2)
        
        #Chequeamos Input.
        if(is.null(df)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }

        #Chequeamos input y extraemos variable dependiente.
        if(BoxNum == TRUE){
            #Esperamos actualizacion
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
        }else{
            #Esperamos actualizacion
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
        }
        names(df)[ncol(df)] <- "dep_Var"
        par(mfrow = c(1,1))

        graph <- try(klaR :: partimat(dep_Var ~ ., df,method="lda"),silent= T)
        if(class(graph) == "try-error"){
            return(plot(1:2, type='n', 
                main = "El conjunto de predictores es demasiado amplio."))
        }
        graph
        
    }) 
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: Evaluacion ///////////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #PRINTS --------------------------------------------------------------------
    #Evaluacion Training Set
    output$RPrnt_C.LDAEval1 <-renderPrint({ 
        #Input 
        training  <- LReact()[[1]]
        linear <- LDAReact()
        
        #Chequeamos Input.
        if(is.null(linear)){
            return(NULL)
        }
        if(is.null(training)){
            return(NULL)
        }
        names(training)[ncol(training)] <- "dep_Var"
        
        #Confusion Matrix and accuracy - training
        p1 <- predict(linear,training)$class
        tab <- table(predicted = p1, Actual = training$dep_Var)
        
        cat("______________Evaluacion sobre el Training Set_______________\n")
        cat("Matriz de Confusión\n")
        print(tab)                          #Matriz de confusion. 
        cat("_____________________________________________________________\n")
        cat(paste0("Nivel de Precisión: ",sum(diag(tab))/sum(tab)*100,"%\n"))
    }) 
    
    #Evaluacion Testing Set
    output$RPrnt_C.LDAEval2 <-renderPrint({ 
        #Input 
        testing  <- LReact()[[2]]
        linear <- LDAReact()
        
        #Chequeamos Input.
        if(is.null(linear)){
            return(NULL)
        }
        if(is.null(testing)){
            return(NULL)
        }
        names(testing)[ncol(testing)] <- "dep_Var"
        
        #Confusion Matrix and accuracy - testing
        p1 <- predict(linear,testing)$class
        tab <- table(predicted = p1, Actual = testing$dep_Var)
        
        cat("______________Evaluacion sobre el Testing Set_______________\n")
        cat("Matriz de Confusión\n")
        print(tab)                          #Matriz de confusion. 
        cat("_____________________________________________________________\n")
        cat(paste0("Nivel de Precisión: ",sum(diag(tab))/sum(tab)*100,"%\n"))
    }) 
    
    #===========================================================================
    #TAB PANEL: REGULARIZED DISCRIMINANT ANALYSIS //////////////////////////////
    #===========================================================================
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: Selector Variables ///////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #UI's RENDERIZADAS /////////////////////////////////////////////////////////
    
    #Lista de variables a ser incluidas en el LDA.
    output$RUi_checkboxGroupRDA <- renderUI({
        #Input
        df <- ZvarFacReactClas()
        Zvar <- input$Select_ZvarClas
        
        #Esperamos Actualizacion.
        if(is.null(df)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Se habilita cuando la variable ingresada es factor.
        if(input$Box_Numerica2 == FALSE){
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
            valores <- NumericVars2
        }else{
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
            valores <- NumericVars2[!(NumericVars2 %in% c(Zvar,"ZvarFac"))]
        }
        
        #Seleccionadas
        selected <- valores[1:2]
        
        if(length(valores) >= 5){
            selected <- valores[1:5]
        }
        
        #Render UI.
        checkboxGroupInput("SelectList_RDAvar","Selector de variables.",
                           choices  = valores
                           ,
                           selected = selected
        )
    })
    
    #-------------------------------------
    #Indicador de ejecucion.
    observeEvent(input$goButtonRDAejecutar,{
        output$RText_C.EjecRDA <-renderText({						
            x <- c("Remueva variables con nzv = true")
            x
        })
    })
    
    #-------------------------------------
    #Selector del parametro gamma.
    output$RUi_Select_RDAGamma <- renderUI({
        numericInput("Select_RDAGamma","Parámetro Gamma:",
                     min = 0,max = 1,value = 0,step = 0.1
        )
    })
    
    #-------------------------------------
    #Selector del parametro gamma.
    output$RUi_Select_RDALambda <- renderUI({
        numericInput("Select_RDALambda","Parámetro Lambda:",
                     min = 0,max = 1,value = 1,step = 0.1
        )
    })
    
    #FUNCIONES REACTIVAS ///////////////////////////////////////////////////////
    
    #Recibe la tabla arrojada por ZvarFacReactClas y le hace subsetting por 
    #SelectList_ACPvar. Elimina las variables sin desviacion y Retorna el data 
    #frame. LAS VARIABLES NO ESTAN ESTANDARIZADAS
    RReact <- reactive({
        #Action Button
        input$goButtonRDAejecutar
        
        #Evitamos ejecucion automatica
        if(input$goButtonRDAejecutar == 0){
            return(NULL)
        }
        
        #Inputs
        df <- isolate(ZvarFacReactClas())
        df2 <- isolate(ZvarFacReactClas())
        Zvar <- isolate(input$Select_ZvarClas)
        prop <- isolate(input$Select_C.Train/100)
        BoxNum <- isolate(input$Box_Numerica2)
        semilla <- isolate(input$Select_C.Seed)
        
        #Recibimos inputs de los UI's renderizados.
        List_Vars <- isolate(input$SelectList_RDAvar)
        CatVar_list <- isolate(input$Select_ClasCV)
        
        #Esperamos actualizacion.    
        if(is.null(df)){
            return(NULL)   
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(List_Vars)){
            return(NULL)   
        }
        if(is.null(prop)){
            return(NULL)   
        }
        if(is.null(semilla)){
            return(NULL)   
        }
        
        #Minimo dos variables deben ser seleccionadas.
        if(length(List_Vars) < 2){
            return(NULL)   
        }
        
        #Subsetting por variable.
        if(BoxNum == TRUE){
            #Check del input
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
            #Esperamos actualizacion
            if("ZvarFac" %in% List_Vars == TRUE){
                return(NULL)
            }
            df <- df[c(List_Vars,"ZvarFac")]
        }else{
            #Check del input
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
            #Esperamos actualizacion
            if(Zvar %in% List_Vars == TRUE){
                return(NULL)
            }
            df <- df[c(List_Vars,Zvar)]
        }
        
        #Eliminamos las variables que tengan una sd() igual a 0.
        varConDes <- which(sapply(df[,1:(ncol(df)-1)],function(x){
            sd(x) != 0
        }))
        df <- df[,c(varConDes,length(df))]
        
        #Chequeamos dimension del df.
        if(is.null(dim(df))){
            return(NULL)
        }else{
            if(0 %in% dim(df)){
                return(NULL)
            }
        }
        
        #Agregamos a la lista de variables numericas, las categoricas.
        if(length(CatVar_list) != 0){
            df <- cbind(df2[CatVar_list],df)
        }
        
        #Eliminamos filas repetidas.
        df <- df %>% unique() 
        
        #Check final
        if(nrow(df) <= 2){
            return(NULL)
        }
        
        #Semilla aleatoria
        set.seed(semilla)
        
        #Subsetting Training Set
        if(prop != 1){
            ##training and testing sets con muestreo estratificado.
            if(BoxNum == TRUE){
                inTrain <- createDataPartition(
                    df["ZvarFac"][,1], p = prop,list = FALSE)
            }else{
                inTrain <- createDataPartition(
                    df[Zvar][,1], p = prop,list = FALSE)
                
            }
            test <- df[-inTrain,]
            df <- df[inTrain,]
        }else{
            test <- NULL
        }
        return(list(df,test))
    })
    
    #--------------------------------------
    #Informacion sobre varianza 
    output$RTable_C.RDA.var <-renderTable({ 
        #Action
        input$Select_ClasCV
        input$SelectList_RDAvar
        
        #Input 
        df <- RReact()[[1]]
        
        #Chequeamos Input.
        if(is.null(df)){
            return(NULL)
        }
        #Lo activamos
        RDAReact()
        
        df <- df[-ncol(df)]
        
        x <- nearZeroVar(df, saveMetrics = TRUE)
        x$Predictors <- rownames(x)
        x <- x[c(ncol(x),1:(ncol(x)-1))]
        x 
    }) 
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: Plot Correlaciones ///////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #PLOT DE CORRELACIONES /////////////////////////////////////////////////////
    
    #Plot de correlaciones
    output$RPlot_C.RDA.PP <- renderPlot({
        #Input 
        df <- RReact()[[1]]
        
        #Chequeamos Input
        if(is.null(df)){
            return(NULL)
        }
        
        #Plot de correlaciones 
        psych :: pairs.panels(df, gap = 0, pch = 21,
                              bg = df[,ncol(df)], 
                              main = "Correlación de Pearson")
    })
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: RESUMEN //////////////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #FUNCIONES REACTIVAS ///////////////////////////////////////////////////////
    
    #Realiza LDA. Hay que estandarizar las variables primero.
    RDAReact <- reactive({
        #Inputs
        df <- RReact()[[1]]
        Zvar  <- isolate(input$Select_ZvarClas)
        gamma  <- isolate(input$Select_RDAGamma)
        lambda  <- isolate(input$Select_RDALambda)
        
        #Esperamos actualizacion.    
        if(is.null(df)){
            return(NULL)   
        }
        if(is.null(Zvar)){
            return(NULL)   
        }
        if(is.null(gamma)){
            return(NULL)   
        }
        if(is.null(lambda)){
            return(NULL)   
        }
        
        #Formato
        rownames(df) <- 1:nrow(df)
        
        #Extraemos la variable categorica
        catVar <- names(df)[ncol(df)] 
        names(df)[ncol(df)] <- "dep_Var"
        
        #Hacemos LDA.
        linear <- try(klaR :: rda(dep_Var ~.,df,
                regularization = c(gamma = gamma, lambda = lambda)),silent= T)
        if(class(linear) == "try-error"){
            output$RText_C.RDAadv <-renderText({						
                adv <- paste0('El conjunto de entrenamiento recibió muy pocos 
                              datos en los niveles de la variable "',Zvar,
                              '" con la seleccion de predictores realizada.')
                adv
            })
            return(NULL)
        }else{
            #Registro de ejecucion.
            if(input$goButtonRDAejecutar == 0){
                return(NULL)
            }
            if(input$goButtonRDAejecutar %% 2 == 0){
                output$RText_C.RDAadv <-renderText({						
                    adv <- paste0("Ejecutado")
                    adv
                })
            }else{
                output$RText_C.RDAadv <-renderText({						
                    adv <- paste0("Realizado")
                    adv
                })
            }
        }
        return(linear)
    })
    
    #RESUMEN DEL RDA ///////////////////////////////////////////////////////////
    
    #Matriz de Autovectores
    output$RPrnt_C.RDA.Res <-renderPrint({ #Asi funciona no se por que...
        #Input 
        linear <- RDAReact()
        #Chequeamos Input.
        if(is.null(linear)){
            return(NULL)
        }
        print(linear) 
    }) 
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: Partition Plot ///////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #GRAFICO SCATTERPLOT ///////////////////////////////////////////////////////
    
    #Grafico Partition
    output$RPlot_C.RDA.Part <- renderPlot({
        #Active Button
        input$goButtonRDAPart
        
        #Evitamos plot automatico
        if(input$goButtonRDAPart == 0){
            return(NULL)
        }
        
        #Input 
        df  <- isolate(RReact()[[1]])
        Zvar  <- isolate(input$Select_ZvarClas)
        BoxNum <- isolate(input$Box_Numerica2)
        gamma  <- isolate(input$Select_RDAGamma)
        lambda  <- isolate(input$Select_RDALambda)
        
        #Chequeamos Input.
        if(is.null(df)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(gamma)){
            return(NULL)   
        }
        if(is.null(lambda)){
            return(NULL)   
        }
        
        #Chequeamos input y extraemos variable dependiente.
        if(BoxNum == TRUE){
            #Esperamos actualizacion
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
        }else{
            #Esperamos actualizacion
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
        }
        names(df)[ncol(df)] <- "dep_Var"
        par(mfrow = c(1,1))
        
        graph <- try(klaR :: partimat(dep_Var ~ ., df,method="rda",
                regularization = c(gamma = gamma, lambda = lambda)),silent= T)
        if(class(graph) == "try-error"){
            return(plot(1:2, type='n', 
                        main = "El conjunto de predictores es demasiado amplio."))
        }
        graph
        
    }) 
    
    #///////////////////////////////////////////////////////////////////////////
    #TAB: Evaluacion ///////////////////////////////////////////////////////////
    #///////////////////////////////////////////////////////////////////////////
    
    #PRINTS --------------------------------------------------------------------
    #Evaluacion Training Set
    output$RPrnt_C.RDAEval1 <-renderPrint({ 
        #Input 
        training  <- RReact()[[1]]
        linear <- RDAReact()
        
        #Chequeamos Input.
        if(is.null(linear)){
            return(NULL)
        }
        if(is.null(training)){
            return(NULL)
        }
        names(training)[ncol(training)] <- "dep_Var"
        
        #Confusion Matrix and accuracy - training
        p1 <- try(predict(linear,training)$class)
        if(class( p1) == "try-error"){
            return(cat("El algoritmo no puede obtener un resultado.
                       Posiblemente hay alta colinealidad."))
        }
        tab <- table(predicted = p1, Actual = training$dep_Var)
        
        cat("______________Evaluacion sobre el Training Set_______________\n")
        cat("Matriz de Confusión\n")
        print(tab)                          #Matriz de confusion. 
        cat("_____________________________________________________________\n")
        cat(paste0("Nivel de Precisión: ",sum(diag(tab))/sum(tab)*100,"%\n"))
    }) 
    
    #Evaluacion Testing Set
    output$RPrnt_C.RDAEval2 <-renderPrint({ 
        #Input 
        testing  <- RReact()[[2]]
        linear <- RDAReact()
        
        #Chequeamos Input.
        if(is.null(linear)){
            return(NULL)
        }
        if(is.null(testing)){
            return(NULL)
        }
        names(testing)[ncol(testing)] <- "dep_Var"
        
        #Confusion Matrix and accuracy - testing
        p1 <- try(predict(linear,testing)$class)
        if(class( p1) == "try-error"){
            return(cat("El algoritmo no puede obtener un resultado.
                       Posiblemente hay alta colinealidad."))
        }
        tab <- table(predicted = p1, Actual = testing$dep_Var)
        
        cat("______________Evaluacion sobre el Testing Set_______________\n")
        cat("Matriz de Confusión\n")
        print(tab)                          #Matriz de confusion. 
        cat("_____________________________________________________________\n")
        cat(paste0("Nivel de Precisión: ",sum(diag(tab))/sum(tab)*100,"%\n"))
    }) 
    
})
