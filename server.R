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
    #output$ShowMe <-renderText({          #Print section
    #     print(input$goButtonKme)
    #})

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
    #INPUT: VARIABLE A EXPLICAR (Zvar). 
    output$RUi_Select_Zvar <- renderUI({
        #Genera Select_Zvar
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
    output$RUi_Select_Zval <- renderUI({
        #Genera Select_Zval
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
    #INPUT: VALOR DE LA SEGUNDA VARIABLE DE EXPLORACION (Xvar).
    output$RUi_Select_Xvar <- renderUI({
        #Genera Select_Xvar
        if(!(input$panels %in% c("Diagrama de Cajas","Histogramas"))){
            selectInput("Select_Xvar",
                        "Seleccione la segunda variable de  exploración:",

                        choices  = c(as.character(varDepNum)
                        ),
                        selected = "oxygen"
            )
        }
    })
    
    #---------------------------------------------------------------------------
    #INPUT: CORRIENTES VARIABLE EXPLORATORIA 1 (CorrY).
    output$RUi_Select_CorrY <- renderUI({
        #Genera Select_CorrY
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
    output$RUi_Select_DistY <- renderUI({
        #Genera Select_DistY
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
    output$RUi_Select_CorrX <- renderUI({
        #Esperamos actualizacion
        if(is.null(input$Select_Xvar)){
            return(NULL)
        }
        
        #Genera Select_CorrX
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
    output$RUi_Select_DistX <- renderUI({
        #Esperamos actualizacion
        if(is.null(input$Select_Xvar)){
            return(NULL)
        }
        #Genera Select_DistX
        if(input$Select_Xvar == "Distancia.Euclidea.kms"){
            alt <- as.character(unique(MatrizBiologica["objeto.d"][,1]))  
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
    dfReact <- reactive({
        zona <- input$Select_Zona
        Xvar <- input$Select_Xvar
        Yvar <- input$Select_Yvar
        Zvar <- input$Select_Zvar 
        Zval <- input$Select_Zval 
        
        #Esperamos actualizacion.
        if(is.null(zona)){
            return(NULL)
        }
        if(is.null(Xvar)){
            return(NULL)
        }
        if(is.null(Yvar)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(Zval)){
            return(NULL)
        }
        
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
        if(!is.null(Zval)){
            if(Zval != "Todas"){
                df <- df[df[,2] == Zval,]
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

        #Eliminamos filas con NA's en las variables de exploracion.
        df <- df[!is.na(df[Xvar][,1]),]
        df <- df[!is.na(df[Yvar][,1]),]
        
        #Corregimos nombres. 
        if(nrow(df) != 0){
            row.names(df) <- seq(1,nrow(df))
        }
        
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
    
    #Tabla de datos
    output$RDT_TablaDatos <- DT::renderDataTable(
        DT::datatable({
            #Chequeamos Input.
            if(is.null(dfReact())){
                return(NULL)
            }
            dfReact() 
        })
    )
    
    #--------------------------------------------
    #Resumen de la variable Y.
    output$RTable_DTY <-renderTable({ 
        #Chequeamos Input.
        if(is.null(dfReact())){
            return(NULL)
        }
        #Resumen estadistico.
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
    
    output$RText_DTY <- renderText({
        input$Select_Yvar
    })
    
    #--------------------------------------------
    #Resumen de la variable X.
    output$RText_DTX <- renderText({
        #Esperamos actualizacion.
        if(is.null(input$Select_Xvar)){
            return(NULL)
        }
        input$Select_Xvar
    })
    
    output$RTable_DTX <-renderTable({ 
        #Esperamos actualizacion.
        if(is.null(input$Select_Xvar)){
            return(NULL)
        }
        if(is.null(dfReact())){
            return(NULL)
        }
        
        #Resumen Estadistico.
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
    
    #===========================================================================
    #TAB PANEL: MAPA ___________________________________________________________
    #===========================================================================

    #Mapa con puntos de geolocalizacion.
    output$RPlot_Map <-renderPlot({
        #Chequeamos Input.
        if(is.null(dfReact())){
            return(NULL)
        }
        #Subsetting para el mapa.
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
    
    #===========================================================================
    #TAB PANEL: BOXPLOT ________________________________________________________
    #===========================================================================
    
    #UI's RENDERIZADAS /////////////////////////////////////////////////////////
    
    #Opciones de eleccion para los valores de Zvar.
    #Genera SelectList_Zval
    output$RUi_checkboxGroup.Zvar <- renderUI({
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
                           "Selector de valores.",
                           choices  = valores
                           ,
                           selected = valores[1:2]
        )
    })
    
    #--------------------------------------------
    #Opciones de eleccion para los valores de Localidad.
    #Genera SelectList_Zona
    output$RUi_checkboxGroup.Zona <- renderUI({
        df <- dfReact()
        
        #Chequeamos Input.
        if(is.null(df)){
            return(NULL)
        }
        valores <- as.character(unique(df["Localidad"][,1]))
        #Interfaz.
        checkboxGroupInput("SelectList_Zona","Selector de localidad.",
                           choices  = valores
                           ,
                           selected = valores[1:2]
        )
    })
    
    #FUNCIONES REACTIVAS ///////////////////////////////////////////////////////
    
    #Recibe la tabla arrojada por ZvarFacReact y le habe subsetting por 
    #SelectList_Zval y SelectList_Zona.
    BReact <- reactive({
        #Inputs
        df <- ZvarFacReact()
        Zvar <- input$Select_Zvar
        BoxNum <- input$Box_Numerica
        
        #recibimos inputs de los UI's renderizados.
        List_Zval <- input$SelectList_Zval
        List_Zona <- input$SelectList_Zona
        
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
        if(is.null(List_Zona)){
            return(NULL)
        }
        
        #Variable categorica.
        if(BoxNum == TRUE){
            
            #Subsetting del data frame df.
            df <- df[df$Localidad %in% List_Zona,]
            
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
            df <- df[df$Localidad  %in% List_Zona,]
            df <- df[df[Zvar][,1]  %in% List_Zval,]
            
        }
        #Exportamos dataset.
        return(df)
        
    })
    
    #GRAFICO BOXPLOT ///////////////////////////////////////////////////////////
    
    #Genera Grafico Boxplot.
    output$RPlot_Box <- renderPlot({
        #Action Button
        input$goButtonBox
        
        #Input
        df <- isolate(BReact())
        Yvar <- isolate(input$Select_Yvar)
        Zvar <- isolate(input$Select_Zvar)
        BoxNum <- isolate(input$Box_Numerica)

        #Esperamos Actualizacion.
        if(is.null(df)){
            return(NULL)
        }
        if(is.null(Yvar)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(nrow(df) == 0){
            return(NULL)
        }
        
        #Realizamos boxplots
        if(BoxNum == TRUE){
            #Configuramos plot.
            BoxPlot <- ggplot(df,aes(ZvarFac,get(Yvar),fill = ZvarFac)) + 			
                geom_boxplot()	+ facet_grid(.~Localidad) + 
                ylab(Yvar) + xlab(Yvar) +
                scale_fill_discrete(name = Zvar) +
                ggtitle(
                    paste0("Boxplots de la primera variable de exploración. ",
                           Yvar))									
        }else{
            #Configuramos plot.
            BoxPlot <- ggplot(df,aes(get(Zvar),get(Yvar),fill =get(Zvar))) + 			
                geom_boxplot()	+ facet_grid(.~Localidad) +
                ylab(Yvar) + xlab(Yvar) +
                scale_fill_discrete(name = Zvar) +
                ggtitle(
                    paste0("Boxplots de la primera variable de exploración. ",
                           Yvar))		
        }
        BoxPlot 
        
    })
    
    #===========================================================================
    #TAB PANEL: HISTOGRAMAS ////////////////////////////////////////////////////
    #===========================================================================
    
    #UI's RENDERIZADAS /////////////////////////////////////////////////////////
    
    #Opciones de eleccion para los valores de Zvar.
    #Genera SelectList_Zval2
    output$RUi_checkboxGroup2.Zvar <- renderUI({
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
                           "Selector de valores.",
                           choices  = valores
                           ,
                           selected = valores[1:2]
        )
    })

    #--------------------------------------------
    #Opciones de eleccion para los valores de Localidad.
    #Genera SelectList_Zona2
    output$RUi_checkboxGroup2.Zona <- renderUI({
        df <- dfReact()
        
        #Chequeamos Input.
        if(is.null(df)){
            return(NULL)
        }
        valores <- as.character(unique(df["Localidad"][,1]))
        #Interfaz.
        checkboxGroupInput("SelectList_Zona2","Selector de localidad.",
                           choices  = valores
                           ,
                           selected = valores[1:2]
        )
    })

    #FUNCIONES REACTIVAS ///////////////////////////////////////////////////////
    
    #Recibe la tabla arrojada por ZvarFacReact y le habe subsetting por 
    #SelectList_Zval2 y SelectList_Zona2
    HReact <- reactive({
        #Inputs
        df <- ZvarFacReact()
        Zvar <- input$Select_Zvar
        BoxNum <- input$Box_Numerica
        
        #recibimos inputs de los UI's renderizados.
        List_Zval <- input$SelectList_Zval2
        List_Zona <- input$SelectList_Zona2
        
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
        if(is.null(List_Zona)){
            return(NULL)
        }
        
        #Variable categorica.
        if(BoxNum == TRUE){
            
            #Subsetting del data frame df.
            df <- df[df$Localidad %in% List_Zona,]
            
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
            df <- df[df$Localidad  %in% List_Zona,]
            df <- df[df[Zvar][,1]  %in% List_Zval,]
            
        }
        #Exportamos dataset.
        return(df)
        
    })
    
    #GRAFICO HiSTOGRAMA ////////////////////////////////////////////////////////
    
    #Genera Grafico Boxplot.
    output$RPlot_Hist <- renderPlot({
        #Action Button
        input$goButtonHis
        
        #Input
        df <- isolate(HReact())
        Yvar <- isolate(input$Select_Yvar)
        Zvar <- isolate(input$Select_Zvar)
        BoxNum <- isolate(input$Box_Numerica)
        
        #Esperamos Actualizacion.
        if(is.null(df)){
            return(NULL)
        }
        if(is.null(Yvar)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        if(nrow(df) == 0){
            return(NULL)
        }
        
        #Realizamos boxplots
        if(BoxNum == TRUE){
            #Configuramos plot.
            HistP <- ggplot(df,aes(get(Yvar),fill =ZvarFac)) + 			
                geom_histogram() + facet_grid(ZvarFac ~ Localidad) + 
                xlab(Yvar) + ylab("Frecuencia") + 
                scale_fill_discrete(name = Zvar) +
                ggtitle(
                    paste0("Histogramas de la primera variable de exploración. "
                           ,Yvar))									
        }else{
            #Configuramos plot.
            HistP <- ggplot(df,aes(get(Yvar),fill =get(Zvar))) + 			
                geom_histogram() + 
                facet_grid(get(Zvar) ~ Localidad) + 
                xlab(Yvar) + ylab("Frecuencia") + 
                scale_fill_discrete(name = Zvar) +
                ggtitle(
                    paste0("Histogramas de la primera variable de exploración. "
                           ,Yvar))
        }
        HistP
        
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
    #Opciones de eleccion para los valores de Localidad.
    #Genera SelectList_Zona3
    output$RUi_checkboxGroup3.Zona <- renderUI({
        df <- dfReact()
        
        #Chequeamos Input.
        if(is.null(df)){
            return(NULL)
        }
        valores <- as.character(unique(df["Localidad"][,1]))
        #Interfaz.
        checkboxGroupInput("SelectList_Zona3","Selector de localidad.",
                           choices  = valores
                           ,
                           selected = valores[1:2]
        )
    })
    
    #--------------------------------------------
    #Opciones de eleccion para los valores de Abundancia.
    #Genera SelectList_Size3
    output$RUi_checkboxGroup3.Size<- renderUI({
        df <- dfReact()
        
        #Chequeamos Input.
        if(is.null(df)){
            return(NULL)
        }
        valores <- as.character(unique(df["Abundancia"][,1]))
        #Interfaz.
        checkboxGroupInput("SelectList_Size3","Selector de abundancia.",
                           choices  = valores
                           ,
                           selected = valores[1]
        )
    })
    
    #--------------------------------------------
    #Opciones de eleccion para los valores de Abundancia.
    #Genera Box_Etiqueta
    output$RUi_checkboxEtiqueta<- renderUI({
        checkboxInput("Box_Etiqueta","Mostrar Etiquetas.",value=T)
    })
    
    #--------------------------------------------
    #Opciones de eleccion para los valores de Abundancia.
    #Genera Box_Jitter
    output$RUi_checkboxJitter<- renderUI({
        checkboxInput("Box_Jitter","Dispersar puntos.",value=F)
    })
    
    #FUNCIONES REACTIVAS ///////////////////////////////////////////////////////
    
    #Recibe la tabla arrojada por ZvarFacReact y le habe subsetting por 
    #SelectList_Zval3, SelectList_Zona3 y SelectList_Size3
    KReact <- reactive({
        #Inputs
        df <- ZvarFacReact()
        Zvar <- input$Select_Zvar
        BoxNum <- input$Box_Numerica
        
        #recibimos inputs de los UI's renderizados.
        List_Zval <- input$SelectList_Zval3
        List_Zona <- input$SelectList_Zona3
        List_Abun <- input$SelectList_Size3
        
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
        if(is.null(List_Zona)){
            return(NULL)
        }
        if(is.null(List_Abun)){
            return(NULL)   
        }
        
        #Variable categorica.
        if(BoxNum == TRUE){
            
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
            
        }else{
            #Eliminamos los valores faltantes de la variables estudiada.
            df <- df[!is.na(df[Zvar][,1]),]
            
            #Subsetting del data frame df.
            df <- df[df$Localidad  %in% List_Zona,]
            df <- df[df[Zvar][,1]  %in% List_Zval,]
            df <- df[df$Abundancia %in% List_Abun,]
            
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
        Etiq <- isolate(input$Box_Etiqueta)
        BoxNum <- isolate(input$Box_Numerica)
        Jitter <- isolate(input$Box_Jitter)
        
        #Esperamos Actualizacion.
        if(is.null(df)){
            return(NULL)   
        }
        if(nrow(df) == 0){
            return(NULL) 
        }
        if(is.null(Xvar)){
            return(NULL)
        }
        if(is.null(Yvar)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
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
            #Puntos por abundancia.
            if(length(unique(df$Abundancia)) == 1){
                ScatterP <- ggplot(df,aes(get(Xvar),get(Yvar),
                                          col=ZvarFac)) +
                    geom_point(size = 4) +
                    #Etiquetas.
                    xlab(Xvar) +
                    ylab(Yvar) +
                    scale_color_discrete(name = Zvar) +
                    ggtitle("Diagrama de dispersión.") 
            }else{
                ScatterP <- ggplot(df,aes(get(Xvar),get(Yvar),
                                          col=ZvarFac,shape=Abundancia))  +
                    geom_point(size = 4) +
                    #Etiquetas.
                    xlab(Xvar) +
                    ylab(Yvar) +
                    scale_color_discrete(name = Zvar) +
                    ggtitle("Diagrama de dispersión.") 
            }
        }else{
            #Check de input
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
            #Puntos por abundancia.
            if(length(unique(df$Abundancia)) == 1){
                ScatterP <- ggplot(df,aes(get(Xvar),get(Yvar),
                                          col=get(Zvar))) +
                    geom_point(size = 4) +
                    #Etiquetas.
                    xlab(Xvar) +
                    ylab(Yvar) +
                    scale_color_discrete(name = Zvar) +
                    ggtitle("Diagrama de dispersión.") 
            }else{
                ScatterP <- ggplot(df,aes(get(Xvar),get(Yvar),
                                          col=get(Zvar),shape=Abundancia)) +
                    geom_point(size = 4) +
                    #Etiquetas.
                    xlab(Xvar) +
                    ylab(Yvar) +
                    scale_color_discrete(name = Zvar) +
                    ggtitle("Diagrama de dispersión.") 
            } 
        }
        #Jitter
        if(Jitter == TRUE){
            set.seed("1376")         #Semilla del jitter
            ScatterP <- ScatterP +
                geom_jitter(width= 0.005, height = 0.005,size = 4)
        }
        
        #Etiqueta
        if(Etiq == TRUE){
            ScatterP <- ScatterP + geom_text(aes(label=df$Localidad),
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
        if(is.null(df)){
            return(NULL) 
        }  
        if(is.null(centros)){
            return(NULL) 
        } 
        if(is.null(Xvar)){
            return(NULL) 
        }  
        if(is.null(Yvar)){
            return(NULL) 
        } 
        if(is.null(Zvar)){
            return(NULL) 
        } 
        
        #Corrector de variables renderizadas.
        if(is.null(Jitter)){
            Jitter <- TRUE
        }
        
        #Input Numerico.
        if(BoxNum == TRUE){
            ##Scatterplot con k-means.
            kplot <- ggplot(data=df,aes(get(Xvar),get(Yvar),color = cluster,
                                        shape = as.factor(ZvarFac))) + geom_point(size=4) +
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
                                        shape = get(Zvar))) + geom_point(size = 4) +
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
        names(tabla)[1] <- "#Cluster"
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
        if(is.null(kmed)){
            return(NULL)   
        }
        if(is.null(df)){
            return(NULL)   
        }
        if(is.null(centros)){
            return(NULL)   
        }
        if(is.null(Xvar)){
            return(NULL)   
        }
        if(is.null(Yvar)){
            return(NULL)   
        }
        if(is.null(Zvar)){
            return(NULL)   
        }
        
        ##Creando los datos del heatmap.	
        dfMat <- as.matrix(df[c(Xvar,Yvar)])					
        row.names(dfMat) <- 1:nrow(dfMat)				
        
        #Si no hay datos no ploteamos.
        if(nrow(dfMat) == 0){
            return(NULL)
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
            return(NULL)
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
    
    #===========================================================================
    #TAB PANEL: DENDROGRAMA ////////////////////////////////////////////////////
    #===========================================================================
    
    #UI's RENDERIZADAS /////////////////////////////////////////////////////////
    
    #Selector de visualizacion de leyenda.
    #Genera Box_Legend
    output$RUi_checkboxLegend<- renderUI({
        checkboxInput("Box_Legend","Mostrar Leyenda.",value=T)
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
        
        #esperamos Actualizacion.
        if(is.null(df)){
            return(NULL)
        }
        if(is.null(Xvar)){
            return(NULL)
        }
        if(is.null(Yvar)){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
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
            return(NULL)
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
        
        #CHequeo de condiciones para plotear.
        if(plotear == FALSE){
            return(NULL)
        }
        
        #Clusterizacion jerarquica.
        d <- dist(df1)											
        cluster <- hclust(d)
        
        #Corrector del Bug.
        cluster$labels <- as.character(1:nrow(df))
        
        #Clasificacion de la variable explicada.
        if(input$Box_Numerica == TRUE){
            
            #Dendograma
            myplclust(cluster,lab.col = unclass(df$ZvarFac))
            
            #Agregamos leyenda.
            if(legend == TRUE){
                legend("topright",legend = unique(df$ZvarFac),
                       col = unique(df$ZvarFac),pch=19,title = Zvar)
            }
        }else{
            #Dendograma
            myplclust(cluster,lab.col = unclass(df[Zvar][,]),
                      main="Cluster Jerárquico")
            
            #Agregamos leyenda.
            if(legend == TRUE){
                legend("topright",legend = unique(df[Zvar][,]),
                       col = unique(df[Zvar][,]),pch=19,title = Zvar)
            }
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
        #Genera Select_ZvarClas
        if(input$Box_Numerica2 == FALSE){
            variables <-VarDepFactor2
            variables <- variables[variables != "Localidad"]
            selected <- variables[1]
        }else{
            variables <- varDepNum2
            selected <- variables[1]
        }
        selectInput("Select_ZvarClas", "Seleccione la variable a explicar:",
                    choices  = variables,
                    selected = selected
        )
    })
    
    #===========================================================================
    # FUNCIONES REACTIVAS //////////////////////////////////////////////////////
    #===========================================================================
    
    #Genera la tabla de datos adecuada en funcion de los inputs de usuario.
    #Extrae las variables requeridas para la exploracion basica y hace los 
    #subsetting correspondientes.
    dfReactClas <- reactive({
        List_Zona <- input$SelectList_ZonaClas
        
        #Esperamos actualizacion.
        if(is.null(List_Zona)){
            return(NULL)
        }

        #Subsetting de variables.
        df <- MatrizBiologica2
        df <- df[df$Localidad %in% List_Zona,]
        
        #Eliminamos repetidos
        df <- df %>% unique()

        #Si no tiene datos salimos.
        if(nrow(df) == 0){
            return(NULL)
        }
        
        #Salida
        return(df)
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
                df$ZvarFac <- rep(NA,times = nrow(df))
                df$ZvarFac[posVal]  <- cut(valores,cutpoints)
                df$ZvarFac[posFalt] <- "Sin Datos"
                df$ZvarFac <- as.factor(df$ZvarFac)
            }
        }
        #Exportamos dataset.
        return(df)
    })
    
    
    #===========================================================================
    #TAB PANEL: SINGULAR VALUE DESCOMPOSITION //////////////////////////////////
    #===========================================================================
    
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
        checkboxInput("Box_LegendClas","Mostrar Leyenda.",value=F)
    })
    
    
    #--------------------------------------------
    #Selector del autovector de izquierda 1.
    #Genera Select_autoVec1
    output$RUi_autoVec1 <- renderUI({
        #Action Button
        input$goButtonCla
        
        #Input
        svd1 <- isolate(SReact()[[2]])
        
        #Check de entrada.
        if(is.null(svd1)){
            return(NULL)
        }
        #Numero maximo de autovewctores
        maximo <- dim(svd1$u)[2]
        
        numericInput("Select_autoVec1","Seleccione un autovector",
                     min = 1,max = maximo,value = 1,step = 1)
    })
    
    
    #--------------------------------------------
    #Selector del autovector de izquierda 2.
    #Genera Select_autoVec2
    output$RUi_autoVec2 <- renderUI({
        #Action Button
        input$goButtonCla
        
        #Input
        svd1 <- isolate(SReact()[[2]])
        
        #Check de entrada.
        if(is.null(svd1)){
            return(NULL)
        }
        #Numero maximo de autovewctores
        maximo <- dim(svd1$u)[2]
        
        numericInput("Select_autoVec2","Seleccione otro autovector",
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
    #SelectList_ZvalSVD. Elimina las variables sin desviavion, prepara los datos
    #para el SVD y lo realiza.
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
            vect <- c()
            for (i in 1:length(List_Zval)){
                cadena <- strsplit(List_Zval[i],split=",")[[1]]
                mini <- as.numeric(sub("\\(","",cadena[1]))
                maxi <- as.numeric(sub("]","",cadena[2]))
                vect <- c(vect,df[Zvar][
                    df[Zvar][,1] > mini & df[Zvar][,1] <= maxi,1])
            }
            df <- df[df[Zvar][,1] %in% vect,]
            
            #Retornamos NULL si nos quedamos sin observaciones.
            if(nrow(df) == 0){
                return(NULL)
            }
            
            #Posiciones de la variable explicada (Zvar y ZvarFac).
            pos <- which(names(df) == Zvar)
            pos2 <- which(names(df) == "ZvarFac")
            
            #Eliminamos las variables que tengan una sd() igual a 0.
            varConDes <- which(sapply(df[,-c(1:6,pos,pos2)],function(x){
                sd(x) != 0
            }))
            
            #Retornamos NULL si nos quedamos sin variables utiles para el SVD.
            if(length(varConDes) != 0){
                varConDes <- varConDes + 6
                df <- df[,c(1:6,varConDes)]
                varEnUso <- names(df[,-c(1:6,pos,pos2)])
            }else{
                return(NULL) 
            }
            
            #SVD
            svd1 <- svd(scale(df[,-c(1:6,pos,pos2)]))
            
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
            
            #Eliminamos las variables que tengan una sd() igual a 0.
            varConDes <- which(sapply(df[,-c(1:6)],function(x){sd(x) != 0}))
            
            #Retornamos NULL si nos quedamos sin variables utiles para el SVD.
            if(length(varConDes) != 0){
                varConDes <- varConDes + 6
                df <- df[,c(1:6,varConDes)]
                varEnUso <- names(df[,-c(1:6)])
            }else{
                return(NULL) 
            }
            
            #SVD
            svd1 <- svd(scale(df[,-c(1:6)]))
        }
        return(list(df,svd1,varEnUso))
        
    })
    
    
    #GRAFICO DE SINGULAR VALUE DESCOMPOSITION///////////////////////////////////
    
    #Realizamos el grafico del SVD con las opciones solicitadas por el usuario.
    output$RPlot_SVD <- renderPlot({
        #Action Button
        input$goButtonCla
        
        #Inputs
        aV1  <- isolate(input$Select_autoVec1)
        aV2  <- isolate(input$Select_autoVec2)
        Zvar <- isolate(input$Select_ZvarClas)
        BoxNum2 <- isolate(input$Box_Numerica2)
        leyenda <- isolate(input$Box_LegendClas)
        df   <- isolate(SReact()[[1]])
        svd1 <- isolate(SReact()[[2]])
        
        #Esperamos actualizacion.
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(df)){
            return(NULL)
        }
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
        if(is.null(leyenda)){
            leyenda <- FALSE
        }

        #Dataset para el plot de SVD.
        if(BoxNum2 == TRUE){
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
            dataPlot1 <- data.frame(Yvar=svd1$u[,aV1],ZvarFac=df["ZvarFac"][,1])
            dataPlot1$ID <- rep(
                paste0("Autovector de Izquierda #",aV1),times = nrow(dataPlot1))
            dataPlot1$Index <- 1:nrow(dataPlot1)
            dataPlot2 <- data.frame(Yvar=svd1$u[,aV2],ZvarFac=df["ZvarFac"][,1])
            dataPlot2$ID <- rep(
                paste0("Autovector de Izquierda #",aV2),times = nrow(dataPlot2))
            dataPlot2$Index <- 1:nrow(dataPlot2)

        }else{
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == TRUE){
                return(NULL)
            }
            dataPlot1 <- data.frame(Yvar=svd1$u[,aV1],Zvar = df[Zvar][,1])
            dataPlot1$ID <- rep(
                paste0("Autovector de Izquierda #",aV1),times = nrow(dataPlot1))
            dataPlot1$Index <- 1:nrow(dataPlot1)
            dataPlot2 <- data.frame(Yvar=svd1$u[,aV2],Zvar=df[Zvar][,1])
            dataPlot2$ID <- rep(
                paste0("Autovector de Izquierda #",aV2),times = nrow(dataPlot2))
            dataPlot2$Index <- 1:nrow(dataPlot2)
        }
        
        #Creamos data frame de ploteo.
        dataPlot <- rbind(dataPlot1,dataPlot2)
        
        #Formateamos.
        dataPlot$ID <- as.factor(dataPlot$ID)

        #Tipo de variable a clasificar.
        if(BoxNum2 == TRUE){
            
            #Configuramos plot.
            ScatPlot <- ggplot(dataPlot,aes(Index,Yvar,color = ZvarFac)) + 			
                geom_point() + facet_grid(.~ID) +
                ylab("Correlacion entre la observacion y el Autovector") + 
                xlab("Observaciones") +
                scale_color_discrete(name = Zvar)
            
        }else{
            #Configuramos plot.
            ScatPlot <- ggplot(dataPlot,aes(Index,Yvar,color = Zvar)) + 	 			
                geom_point() + facet_grid(.~ID) +
                ylab("Correlacion entre la observacion y el Autovector") + 
                xlab("Observaciones") +
                scale_color_discrete(name = Zvar)
        }
        
        #Leyenda
        if(leyenda == TRUE){
            ScatPlot 
        }else{
            ScatPlot + theme(legend.position="none")
        }
    })
    
})
