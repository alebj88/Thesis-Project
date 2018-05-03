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
    #    print(input$Brush)
    #    input$Brush$panelvar1
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
        selectInput("Select_Zvar", "Variable a explicar:",
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
            selectInput("Select_Zval", "Valor a estudiar:",
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
            selectInput("Select_Xvar","Variable de exploración #2:",
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
        #Action Button
        input$goButtonMap1
        
        #Input
        zona <- isolate(input$Select_Zona)
        
        #Chequeamos Input.
        if(is.null(zona)){
            return(NULL)
        }
        if(input$goButtonMap1 == 0){
            return(NULL)
        }
        
        #Subsetting para el mapa.
        df <- MatrizBiologica[c("latitude","longitude","Localidad")]
        df <- df %>% unique() 
        
        #Subsetting por Zona.
        if(zona != "Todas"){
            df <- df[df$Localidad %in% zona,]
        }

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
    
    #-------------------------------------
    #Mostrar leyenda..
    #Genera Box_LegendBoxLoc
    output$RUi_LegendBoxLoc <- renderUI({
        checkboxInput("Box_LegendBoxLoc","Mostrar Leyenda",value=T)
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
        leyenda <- isolate(input$Box_LegendBoxLoc)
        
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
                geom_boxplot()	+ facet_grid(.~Localidad) + 
                ylab(Yvar) + xlab(Zvar) +
                scale_fill_discrete(name = Zvar) +
                ggtitle(
                    paste0("Boxplots de ",Yvar," por localidad."))									
        }else{
            #Configuramos plot.
            BoxPlot <- ggplot(df,aes(get(Zvar),get(Yvar),fill =get(Zvar))) + 			
                geom_boxplot()	+ facet_grid(.~Localidad) +
                ylab(Yvar) + xlab(Zvar) +
                scale_fill_discrete(name = Zvar) +
                ggtitle(
                    paste0("Boxplots de ",Yvar," por localidad."))			
        }
        
        #Leyenda
        if(leyenda == TRUE){
            BoxPlot 
        }else{
            BoxPlot + theme(legend.position="none")
        }
        
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
    
    #-------------------------------------
    #Mostrar leyenda..
    #Genera Box_LegendHistLoc
    output$RUi_LegendHistLoc <- renderUI({
        checkboxInput("Box_LegendHistLoc","Mostrar Leyenda",value=T)
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
        leyenda <- isolate(input$Box_LegendHistLoc)
        
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
            HistP <- ggplot(df,aes(get(Yvar),fill =ZvarFac)) + 			
                geom_histogram() + facet_grid(ZvarFac ~ Localidad) + 
                xlab(Yvar) + ylab("Frecuencia") + 
                scale_fill_discrete(name = Zvar) +
                ggtitle(paste0("Histogramas de ",Yvar," por localidad."))									
        }else{
            #Configuramos plot.
            HistP <- ggplot(df,aes(get(Yvar),fill =get(Zvar))) + 			
                geom_histogram() + 
                facet_grid(get(Zvar) ~ Localidad) + 
                xlab(Yvar) + ylab("Frecuencia") + 
                scale_fill_discrete(name = Zvar) +
                ggtitle(paste0("Histogramas de ",Yvar," por localidad."))
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
        #Genera Select_ZvarClas
        if(input$Box_Numerica2 == FALSE){
            variables <-VarDepFactor2
            variables <- variables[variables != "Localidad"]
            selected <- variables[1]
        }else{
            variables <- varDepNum2
            selected <- variables[1]
        }
        selectInput("Select_ZvarClas", "Variable a clasificar:",
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
    
    #--------------------------------------------
    #Tabla de la variable Z.
    output$RTable_DTZClas <-renderTable({
        #input
        Zvar <- input$Select_ZvarClas
        #Chequeamos Input.
        if(is.null(dfReactClas())){
            return(NULL)
        }
        if(is.null(Zvar)){
            return(NULL)
        }
        #Resumen estadistico.
        tabla <- sort(table(dfReactClas()[input$Select_ZvarClas][,1]), decr = T)
        tabla <- as.data.frame(tabla)
        names(tabla) <- c(Zvar,"Frecuencia")
        tabla
    })
    
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
        checkboxInput("Box_LegendClas","Mostrar Leyenda.",value=F)
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
        
        numericInput("Select_autoVec1","Seleccione un autovector",
                     min = 1,max = maximo,value = 1,step = 1)
    })
    
    
    #--------------------------------------------
    #Selector del autovector de izquierda 2.
    #Genera Select_autoVec2
    output$RUi_autoVec2 <- renderUI({
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
            pos <- which(names(df) == Zvar)
            pos2 <- which(names(df) == "ZvarFac")
            
            #Cortamos el dataFrame. Nos quedamos con el conjunto de predictores.
            df <- df[,-c(1:6,pos,pos2)]
            
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
            df <- df[,-c(1:6)]
            
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
            geom_point() + facet_grid(.~ID) +
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
            geom_point() + facet_grid(.~ID) +
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
            variables <- varDepNum2[varDepNum2 != Zvar]
            selected <- variables[1]
        }else{
            variables <- varDepNum2
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
        
        #Subsetting de df para el plot.
        df <- df[c(Zvar,Yvar)]
        df$Index <- 1:nrow(df)
        
        #Coloreamos en funcion de numeric
        if(input$Box_Numerica2 == TRUE){
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
            #Subsetting de df para el plot.
            df <- df[c(ZvarFac,Yvar)]
            df$Index <- 1:nrow(df)
            
            #Configuramos plot.
            ScatPlot <- ggplot(df,aes(Index,get(Yvar),col=get(ZvarFac))) +	 			
                geom_point() +
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
                geom_point() +
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
            variables <- varDepNum2[varDepNum2 != Zvar]
            selected <- variables[1]
        }else{
            variables <- varDepNum2
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
        
        #Subsetting de df para el plot.
        df <- df[c(Zvar,Yvar)]
        df$Index <- 1:nrow(df)
        
        #Coloreamos en funcion de numeric
        if(input$Box_Numerica2 == TRUE){
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
            #Subsetting de df para el plot.
            df <- df[c(ZvarFac,Yvar)]
            df$Index <- 1:nrow(df)
            
            #Configuramos plot.
            ScatPlot <- ggplot(df,aes(Index,get(Yvar),col=get(ZvarFac))) +	 			
                geom_point() +
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
                geom_point() +
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
            variables <- varDepNum2[varDepNum2 != Zvar]
            selected <- variables[1]
        }else{
            variables <- varDepNum2
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
        
        #Subsetting de df para el plot.
        df <- df[c(Zvar,Yvar)]
        df$Index <- 1:nrow(df)
        
        #Coloreamos en funcion de numeric
        if(input$Box_Numerica2 == TRUE){
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
            #Subsetting de df para el plot.
            df <- df[c(ZvarFac,Yvar)]
            df$Index <- 1:nrow(df)
            
            #Configuramos plot.
            ScatPlot <- ggplot(df,aes(Index,get(Yvar),col=get(ZvarFac))) +	 			
                geom_point() +
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
                geom_point() +
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
            variables <- varDepNum2[varDepNum2 != Zvar]
        }else{
            variables <- varDepNum2
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
            valores <- varDepNum2
        }else{
            #Esperamos Actualizacion.
            if("ZvarFac" %in% names(df) == FALSE){
                return(NULL)
            }
            valores <- varDepNum2[!(varDepNum2 %in% c(Zvar,"ZvarFac"))]
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
    #FUNCIONES REACTIVAS ///////////////////////////////////////////////////////
    
    #Recibe la tabla arrojada por ZvarFacReactClas y le hace subsetting por 
    #SelectList_ACPvar. Elimina las variables sin desviacion y Retorna el data 
    #frame. LAS VARIABLES NO ESTAN ESTANDARIZADAS
    AReact <- reactive({
        #Inputs
        df <- ZvarFacReactClas()
        Zvar <- input$Select_ZvarClas
        
        #recibimos inputs de los UI's renderizados.
        List_Vars <- input$SelectList_ACPvar

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
        
        #Minimo dos variables deben ser seleccionadas.
        if(length(List_Vars) < 2){
            return(NULL)   
        }
        
        #Subsetting por variable.
        df <- df[List_Vars]
        
        #Eliminamos las variables que tengan una sd() igual a 0.
        varConDes <- which(sapply(df,function(x){
            sd(x) != 0
        }))
        df <- df[,varConDes]
        
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
        
        return(df)
    })
    
    #MATRIZ DE CORRELACIONES-COVARIANZAS ///////////////////////////////////////
    
    #--------------------------------------------
    #Matriz de Covarianzas-Correlaciones.
    output$RTable_CorCov <- renderTable({
        #Action Button
        input$goButtonACP1
        
        #Input
        df <- isolate(AReact())
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
            spearman <- TRUE
        }
        
        #Plot automatico.
        if(input$goButtonACP1 != 0){
            input$Box_CorACP
            input$Box_SpearACP
        }
        
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
    
    
})
