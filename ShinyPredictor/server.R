#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
    
    #====================================================================
    #Panel: Zona Predefinida.============================================
    #====================================================================
    
    #UI's RENDERIZADAS /////////////////////////////////////////////////////////
    
    
    #Seleccion del ecosistema a plotear.
    output$RUi_Select_Z.Ecosistema <- renderUI({
        #Actualizar lista de ecosistemas
        input$goButton_E.Act
        
        #Render UI.
        selectInput("Select_Z.Ecosistema","Semejantes a:",
                           choices  = unique(AmbDat1$Ecosystem)
                           ,
                           selected = unique(AmbDat1$Ecosystem)[1]
        )
    })
    
    #MAPA //////////////////////////////////////////////////////////////////////
    
    #Mapa con puntos de geolocalizacion.
    output$RPlot_Z.MP.Map <-renderPlot({
        #Actualizar
        input$goButton_E.Act
        
        #Input
        NroTabla <- input$Select_NroTabla
        Tamano <- input$Select_Z.MP.Tamano
        Zvar <- input$Select_Z.Ecosistema
        latMin <- input$Select_Z.MP.latMin
        latMax <- input$Select_Z.MP.latMax
        lonMin <- input$Select_Z.MP.lonMin
        lonMax <- input$Select_Z.MP.lonMax
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(latMin)){
            return(NULL)
        }
        if(is.null(latMax)){
            return(NULL)
        }
        if(is.null(lonMin)){
            return(NULL)
        }
        if(is.null(lonMax)){
            return(NULL)
        }
        if(is.null(NroTabla)){
            return(NULL)
        }
        if(is.null(Tamano)){
            return(NULL)
        }
        

        withProgress(message = 'Creando Mapa', value = 0, {
            
            #Seleccionamos la tabla a usar
            if(NroTabla == "Muestra"){
                df <- AmbDat1
                
                #Subsetting para el mapa.
                df <- df[c("latitude","longitude","Ecosystem")]
                
            }else{
                df <- MarVzla
                
                #Subsetting para el mapa.
                df <- df[c("latitude","longitude","Ecosystem","Confiable")]
                df$Confiable[df$Confiable == TRUE] <- 
                    rep("Afirmativo",sum(df$Confiable))
                df$Confiable[df$Confiable == FALSE] <- 
                    rep("Negativo",sum(df$Confiable == FALSE))
            }
            

            #Vemos si el ecosistema esta
            if(Zvar %in% unique(df$Ecosystem) == FALSE){
                return(plot(1:2, type='n',main = "El ecosistema ingresado no 
                    genera prediccion en los puntos preestablecidos del mapa"))
            }else{
                df <- df[df$Ecosystem == Zvar,]  
            }
            
            #Removemos NA's de lat-lon
            df <- df[!is.na(df$latitude),]
            df <- df[!is.na(df$longitude),]
            
            #==========
            incProgress(1/4)
            #==========
            
            #No ploteamos df vacio
            if(nrow(df) == 0){
                return(plot(1:2, type='n'))
            }
            
            #Eliminamos repetidos
            df <- df %>% unique() 
            
            #No ploteamos df vacio
            if(nrow(df) == 0){
                return(plot(1:2, type='n'))
            }
            
            #==========
            incProgress(1/4)
            #==========
            
            #Plot del mapa
            global <- map_data("world")
            
            #recortamos
            if(latMin >= latMax | lonMin >= lonMax){
                global <- global[global$long <= -60 & global$long >= -74 &
                                     global$lat <= 17 & global$lat >= 8,]
            }else{
                global <- global[global$long <= lonMax & global$long >= lonMin &
                                 global$lat <= latMax & global$lat >= latMin,]
            }

            plotMapa  <- ggplot() + 
                geom_polygon(data = global, aes(x=long, y = lat, group = group),
                             fill = "green4", color = "black") + 
                coord_fixed(1.3)
           
            
            #==========
            incProgress(1/4)
            #==========
            
            #Plot del mapa
            if(NroTabla == "Muestra"){
                plotMapa <- plotMapa  +
                    geom_point(data = df,
                               aes(x = longitude,y = latitude),col = "blue",
                               size = Tamano) +
                    ggtitle(paste0("Mapa del Ecosistema ",Zvar))
                
            }else{
                plotMapa <- plotMapa  +
                    geom_point(data = df,
                               aes(x = longitude,y = latitude,col = Confiable),
                               size = Tamano) +
                    scale_colour_manual(values = c( "blue","red")) +
                    ggtitle(paste0("Mapa del Ecosistema ",Zvar))
                
            }
            
            #==========
            incProgress(1/4)
            #==========
            
            #Exportamos
            return(plotMapa)
            
        })
    })
    
    #ECOSISTEMA ////////////////////////////////////////////////////////////////
    
    #Referencias
    output$Rtext_Z.Ex.Ubic <-renderText({
        #Input
        Zvar <- input$Select_Z.Ecosistema
   
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
  
        x <- paste0("Ecosistemas similares a: ",Zvar)
        x
    })
    
    #Mapa con puntos de geolocalizacion 1.
    output$RPlot_Z.Ex.Eco1 <-renderPlot({

        #Input
        Zvar <- input$Select_Z.Ecosistema

        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        #Tabla1
        pos <- which(!(names(bioDat1) %in% c("latitude","longitude","Ecosystem",
                                             "Locality","Site","InsularCoast",
                                             "Position")))
        df1 <- bioDat1[bioDat1$Ecosystem == Zvar,pos]
        
        #Formateamos
        df1 <- data.frame(bio = names(df1), 
                          vals = as.numeric(as.character(df1[1,])))
        #Plot
        par(mar=c(2,8,1,2))
        barplot(df1$vals,col = rainbow(114),names.arg = df1$bio,las = 1,
                horiz = T,cex.names = 0.45, 
                main = paste0("SCLESpecies. Ecosystem: ",Zvar))
    })
    
    #Tabla de barplot 1.
    output$RTable_Z.Ex.Eco1 <- renderTable({
        
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla1
        pos <- which(!(names(bioDat1) %in% c("latitude","longitude","Ecosystem",
                                             "Locality","Site","InsularCoast",
                                             "Position")))
        df1 <- bioDat1[bioDat1$Ecosystem == Zvar,pos]
        
        #Formateamos
        df1 <- data.frame(bio = names(df1), 
                          vals = as.numeric(as.character(df1[1,])))
        names(df1) <- c("","")
        
        #Invertimos el orden y exportamos
        df1 <- df1[nrow(df1):1,]
        df1

    })
    
    #Mapa con puntos de geolocalizacion 2.
    output$RPlot_Z.Ex.Eco2 <-renderPlot({
      
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla2
        pos <- which(!(names(bioDat2) %in% c("latitude","longitude","Ecosystem",
                                             "Locality","Site","InsularCoast",
                                             "Position")))
        df2 <- bioDat2[bioDat2$Ecosystem == Zvar,pos]
        
        #Formateamos
        df2 <- data.frame(bio = names(df2), 
                          vals = as.numeric(as.character(df2[1,])))

        #Plot
        par(mar=c(5,8,3,2))
        barplot(df2$vals,col = rainbow(13),names.arg = df2$bio,las = 1,
                horiz = T,cex.names = 1,
                main = paste0("SCLEGrowth. Ecosystem: ",Zvar))
    })
    
    #Tabla de barplot 2.
    output$RTable_Z.Ex.Eco2 <- renderTable({
        
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla1
        pos <- which(!(names(bioDat2) %in% c("latitude","longitude","Ecosystem",
                                             "Locality","Site","InsularCoast",
                                             "Position")))
        df2 <- bioDat2[bioDat2$Ecosystem == Zvar,pos]

        #Formateamos
        df2 <- data.frame(bio = names(df2), 
                          vals = as.numeric(as.character(df2[1,])))
        names(df2) <- c("","")
        
        #Invertimos el orden y exportamos
        df2 <- df2[nrow(df2):1,]
        df2
        
    })
    
    #Mapa con puntos de geolocalizacion 3.
    output$RPlot_Z.Ex.Eco3 <-renderPlot({
        
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla3
        pos <- which(!(names(bioDat3) %in% c("latitude","longitude","Ecosystem",
                                             "Locality","Site","InsularCoast",
                                             "Position")))
        df3 <- bioDat3[bioDat3$Ecosystem == Zvar,pos]

        #Formateamos
        df3 <- data.frame(bio = names(df3), 
                          vals = as.numeric(as.character(df3[1,])))
        #Plot
        par(mar=c(5,8,3,2))
        barplot(df3$vals,col = rainbow(9),names.arg = df3$bio,las = 1,
                horiz = T,cex.names = 1,
                main = paste0("GreatGroups. Ecosystem: ",Zvar))
    })
    
    #Tabla de barplot 3.
    output$RTable_Z.Ex.Eco3 <- renderTable({
        
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla1
        pos <- which(!(names(bioDat3) %in% c("latitude","longitude","Ecosystem",
                                             "Locality","Site","InsularCoast",
                                             "Position")))
        df3 <- bioDat3[bioDat3$Ecosystem == Zvar,pos]

        #Formateamos
        df3 <- data.frame(bio = names(df3), 
                          vals = as.numeric(as.character(df3[1,])))
        names(df3) <- c("","")
        
        #Invertimos el orden y exportamos
        df3 <- df3[nrow(df3):1,]
        df3
        
    })
    
    #Mapa con puntos de geolocalizacion 4.
    output$RPlot_Z.Ex.Eco4 <-renderPlot({
        
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla4
        pos <- which(!(names(bioDat4) %in% c("latitude","longitude","Ecosystem",
                                             "Locality","Site","InsularCoast",
                                             "Position")))
        df4 <- bioDat4[bioDat4$Ecosystem == Zvar,pos]

        #Formateamos
        df4 <- data.frame(bio = names(df4), 
                          vals = as.numeric(as.character(df4[1,])))
        #Plot
        par(mar=c(5,9,3,2))
        barplot(df4$vals,col = rainbow(43),names.arg = df4$bio,las = 1,
                horiz = T,cex.names = 0.7,
                main = paste0("BenthicSubstrate. Ecosystem: ",Zvar))
    })
    
    #Tabla de barplot 4.
    output$RTable_Z.Ex.Eco4 <- renderTable({

        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla1
        pos <- which(!(names(bioDat4) %in% c("latitude","longitude","Ecosystem",
                                             "Locality","Site","InsularCoast",
                                             "Position")))
        df4 <- bioDat4[bioDat4$Ecosystem == Zvar,pos]

        #Formateamos
        df4 <- data.frame(bio = names(df4), 
                          vals = as.numeric(as.character(df4[1,])))
        names(df4) <- c("","")
        
        #Invertimos el orden y exportamos
        df4 <- df4[nrow(df4):1,]
        df4
        
    })
    
    #Mapa con puntos de geolocalizacion 4.
    output$RPlot_Z.Ex.Eco5 <-renderPlot({
        
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla4
        pos <- which(!(names(AmbDat1) %in% 
                                        c("latitude","longitude","Ecosystem")))
        df4 <- AmbDat1[AmbDat1$Ecosystem == Zvar,pos]
        
        # continental = 0, insular = 1
        #df4$InsularCoast <- as.numeric(df4$InsularCoast) - 1 
        
        df4 <- as.data.frame(lapply(df4,mean)) 

        #Formateamos
        df4 <- data.frame(bio = names(df4), 
                          vals = as.numeric(as.character(df4[1,])))
        #Plot
        par(mar=c(5,9,3,2))
        barplot(df4$vals,col = rainbow(length(variables)),
                names.arg = df4$bio,las = 1,
                horiz = T,cex.names = 0.7,
                main = paste0("Promedio de Ambientales. Ecosystem: ",Zvar))
    })
    
    #Tabla de barplot 4.
    output$RTable_Z.Ex.Eco5 <- renderTable({
        
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla1
        pos <- which(!(names(AmbDat1) %in% 
                                        c("latitude","longitude","Ecosystem")))
        df4 <- AmbDat1[AmbDat1$Ecosystem == Zvar,pos]

        df4 <- as.data.frame(lapply(df4,mean))  
        
        #Formateamos
        df4 <- data.frame(bio = names(df4), 
                          vals = as.numeric(as.character(df4[1,])))
        names(df4) <- c("","")
        
        #Invertimos el orden y exportamos
        df4 <- df4[nrow(df4):1,]
        df4
        
    })
    
    #Diagrama de cajas de ambientales------
    
    #Mapa con puntos de geolocalizacion 6.
    output$RPlot_Z.Ex.Eco55 <-renderPlot({
        
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla4 
        pos <- which(!(names(entrenador) %in% 
                           c("latitude","longitude","Ecosystem")))
        df4 <- entrenador[entrenador$Ecosystem == Zvar,pos]
        
        boxplot(df4[,1],col = "blue",main = 
                    paste0("Ecosystem: ",Zvar,". Variable: ",names(df4)[1]))

    })
        
    output$RPlot_Z.Ex.Eco6 <-renderPlot({
        
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla4 
        pos <- which(!(names(entrenador) %in% 
                           c("latitude","longitude","Ecosystem")))
        df4 <- entrenador[entrenador$Ecosystem == Zvar,pos]

        boxplot(df4[,2],col = "blue",main = 
                    paste0("Ecosystem: ",Zvar,". Variable: ",names(df4)[2]))
        
        
    })
    output$RPlot_Z.Ex.Eco7 <-renderPlot({
        
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla4 
        pos <- which(!(names(entrenador) %in% 
                           c("latitude","longitude","Ecosystem")))
        df4 <- entrenador[entrenador$Ecosystem == Zvar,pos]
        
        #barplot(table(df4[,1])) 
        boxplot(df4[,3],col = "blue",main = 
                    paste0("Ecosystem: ",Zvar,". Variable: ",names(df4)[3]))
        
        
    })
    output$RPlot_Z.Ex.Eco8 <-renderPlot({
        
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla4 
        pos <- which(!(names(entrenador) %in% 
                           c("latitude","longitude","Ecosystem")))
        df4 <- entrenador[entrenador$Ecosystem == Zvar,pos]
        
        #barplot(table(df4[,1])) 
        boxplot(df4[,4],col = "blue",main = 
                    paste0("Ecosystem: ",Zvar,". Variable: ",names(df4)[4]))
        
        
    })
    output$RPlot_Z.Ex.Eco9 <-renderPlot({
        
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla4 
        pos <- which(!(names(entrenador) %in% 
                           c("latitude","longitude","Ecosystem")))
        df4 <- entrenador[entrenador$Ecosystem == Zvar,pos]
        
        #barplot(table(df4[,1])) 
        boxplot(df4[,5],col = "blue",main = 
                    paste0("Ecosystem: ",Zvar,". Variable: ",names(df4)[5]))
        
        
    })
    output$RPlot_Z.Ex.Eco10 <-renderPlot({
        
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla4 
        pos <- which(!(names(entrenador) %in% 
                           c("latitude","longitude","Ecosystem")))
        df4 <- entrenador[entrenador$Ecosystem == Zvar,pos]
        
        #barplot(table(df4[,1])) 
        boxplot(df4[,6],col = "blue",main = 
                    paste0("Ecosystem: ",Zvar,". Variable: ",names(df4)[6]))
        
        
    })
    output$RPlot_Z.Ex.Eco11 <-renderPlot({
        
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla4 
        pos <- which(!(names(entrenador) %in% 
                           c("latitude","longitude","Ecosystem")))
        df4 <- entrenador[entrenador$Ecosystem == Zvar,pos]
        
        #barplot(table(df4[,1])) 
        boxplot(df4[,7],col = "blue",main = 
                    paste0("Ecosystem: ",Zvar,". Variable: ",names(df4)[7]))
        
        
    })
    output$RPlot_Z.Ex.Eco12 <-renderPlot({
        
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla4 
        pos <- which(!(names(entrenador) %in% 
                           c("latitude","longitude","Ecosystem")))
        df4 <- entrenador[entrenador$Ecosystem == Zvar,pos]
        
        #barplot(table(df4[,1])) 
        boxplot(df4[,8],col = "blue",main = 
                    paste0("Ecosystem: ",Zvar,". Variable: ",names(df4)[8]))
        
        
    })
    output$RPlot_Z.Ex.Eco13 <-renderPlot({
        
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla4 
        pos <- which(!(names(entrenador) %in% 
                           c("latitude","longitude","Ecosystem")))
        df4 <- entrenador[entrenador$Ecosystem == Zvar,pos]
        
        #barplot(table(df4[,1])) 
        boxplot(df4[,9],col = "blue",main = 
                    paste0("Ecosystem: ",Zvar,". Variable: ",names(df4)[9]))
        
        
    })
    output$RPlot_Z.Ex.Eco14 <-renderPlot({
        
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla4 
        pos <- which(!(names(entrenador) %in% 
                           c("latitude","longitude","Ecosystem")))
        df4 <- entrenador[entrenador$Ecosystem == Zvar,pos]
        
        #barplot(table(df4[,1])) 
        boxplot(df4[,10],col = "blue",main = 
                    paste0("Ecosystem: ",Zvar,". Variable: ",names(df4)[10]))
        
        
    })
    output$RPlot_Z.Ex.Eco15 <-renderPlot({
        
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla4 
        pos <- which(!(names(entrenador) %in% 
                           c("latitude","longitude","Ecosystem")))
        df4 <- entrenador[entrenador$Ecosystem == Zvar,pos]
        
        #barplot(table(df4[,1])) 
        boxplot(df4[,11],col = "blue",main = 
                    paste0("Ecosystem: ",Zvar,". Variable: ",names(df4)[11]))
        
        
    })
    output$RPlot_Z.Ex.Eco16 <-renderPlot({
        
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla4 
        pos <- which(!(names(entrenador) %in% 
                           c("latitude","longitude","Ecosystem")))
        df4 <- entrenador[entrenador$Ecosystem == Zvar,pos]
        
        #barplot(table(df4[,1])) 
        boxplot(df4[,12],col = "blue",main = 
                    paste0("Ecosystem: ",Zvar,". Variable: ",names(df4)[12]))
        
        
    })
    output$RPlot_Z.Ex.Eco17 <-renderPlot({
        
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla4 
        pos <- which(!(names(entrenador) %in% 
                           c("latitude","longitude","Ecosystem")))
        df4 <- entrenador[entrenador$Ecosystem == Zvar,pos]
        
        #barplot(table(df4[,1])) 
        boxplot(df4[,13],col = "blue",main = 
                    paste0("Ecosystem: ",Zvar,". Variable: ",names(df4)[13]))
        
        
    })
    output$RPlot_Z.Ex.Eco18 <-renderPlot({
        
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla4 
        pos <- which(!(names(entrenador) %in% 
                           c("latitude","longitude","Ecosystem")))
        df4 <- entrenador[entrenador$Ecosystem == Zvar,pos]
        
        #barplot(table(df4[,1])) 
        boxplot(df4[,14],col = "blue",main = 
                    paste0("Ecosystem: ",Zvar,". Variable: ",names(df4)[14]))
        
        
    })
    output$RPlot_Z.Ex.Eco19 <-renderPlot({
        
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla4 
        pos <- which(!(names(entrenador) %in% 
                           c("latitude","longitude","Ecosystem")))
        df4 <- entrenador[entrenador$Ecosystem == Zvar,pos]
        
        #barplot(table(df4[,1])) 
        boxplot(df4[,15],col = "blue",main = 
                    paste0("Ecosystem: ",Zvar,". Variable: ",names(df4)[15]))
        
        
    })
    output$RPlot_Z.Ex.Eco20 <-renderPlot({
        
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla4 
        pos <- which(!(names(entrenador) %in% 
                           c("latitude","longitude","Ecosystem")))
        df4 <- entrenador[entrenador$Ecosystem == Zvar,pos]
        
        #barplot(table(df4[,1])) 
        boxplot(df4[,16],col = "blue",main = 
                    paste0("Ecosystem: ",Zvar,". Variable: ",names(df4)[16]))
        
        
    })
    output$RPlot_Z.Ex.Eco21 <-renderPlot({
        
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla4 
        pos <- which(!(names(entrenador) %in% 
                           c("latitude","longitude","Ecosystem")))
        df4 <- entrenador[entrenador$Ecosystem == Zvar,pos]
        
        #barplot(table(df4[,1])) 
        boxplot(df4[,17],col = "blue",main = 
                    paste0("Ecosystem: ",Zvar,". Variable: ",names(df4)[17]))
        
        
    })
    output$RPlot_Z.Ex.Eco22 <-renderPlot({
        
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla4 
        pos <- which(!(names(entrenador) %in% 
                           c("latitude","longitude","Ecosystem")))
        df4 <- entrenador[entrenador$Ecosystem == Zvar,pos]
        
        #barplot(table(df4[,1])) 
        boxplot(df4[,18],col = "blue",main = 
                    paste0("Ecosystem: ",Zvar,". Variable: ",names(df4)[18]))
        
        
    })
    output$RPlot_Z.Ex.Eco23 <-renderPlot({
        
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla4 
        pos <- which(!(names(entrenador) %in% 
                           c("latitude","longitude","Ecosystem")))
        df4 <- entrenador[entrenador$Ecosystem == Zvar,pos]
        
        #barplot(table(df4[,1])) 
        boxplot(df4[,19],col = "blue",main = 
                    paste0("Ecosystem: ",Zvar,". Variable: ",names(df4)[19]))
        
        
    })
    output$RPlot_Z.Ex.Eco24 <-renderPlot({
        
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla4 
        pos <- which(!(names(entrenador) %in% 
                           c("latitude","longitude","Ecosystem")))
        df4 <- entrenador[entrenador$Ecosystem == Zvar,pos]
        
        #barplot(table(df4[,1])) 
        boxplot(df4[,20],col = "blue",main = 
                    paste0("Ecosystem: ",Zvar,". Variable: ",names(df4)[20]))
        
        
    })
    output$RPlot_Z.Ex.Eco25 <-renderPlot({
        
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla4 
        pos <- which(!(names(entrenador) %in% 
                           c("latitude","longitude","Ecosystem")))
        df4 <- entrenador[entrenador$Ecosystem == Zvar,pos]
        
        #barplot(table(df4[,1])) 
        boxplot(df4[,21],col = "blue",main = 
                    paste0("Ecosystem: ",Zvar,". Variable: ",names(df4)[21]))
        
        
    })

    output$RPlot_Z.Ex.Eco26 <-renderPlot({
        
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla4 
        pos <- which(!(names(entrenador) %in% 
                           c("latitude","longitude","Ecosystem")))
        df4 <- entrenador[entrenador$Ecosystem == Zvar,pos]
        
        #barplot(table(df4[,1])) 
        boxplot(df4[,22],col = "blue",main = 
                    paste0("Ecosystem: ",Zvar,". Variable: ",names(df4)[22]))
        
        
    })
    
    output$RPlot_Z.Ex.Eco27 <-renderPlot({
        
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla4 
        pos <- which(!(names(entrenador) %in% 
                           c("latitude","longitude","Ecosystem")))
        df4 <- entrenador[entrenador$Ecosystem == Zvar,pos]
        
        #barplot(table(df4[,1])) 
        boxplot(df4[,23],col = "blue",main = 
                    paste0("Ecosystem: ",Zvar,". Variable: ",names(df4)[23]))
        
        
    })
    
    output$RPlot_Z.Ex.Eco28 <-renderPlot({
        
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla4 
        pos <- which(!(names(entrenador) %in% 
                           c("latitude","longitude","Ecosystem")))
        df4 <- entrenador[entrenador$Ecosystem == Zvar,pos]
        
        #barplot(table(df4[,1])) 
        boxplot(df4[,24],col = "blue",main = 
                    paste0("Ecosystem: ",Zvar,". Variable: ",names(df4)[24]))
        
        
    })
    
    output$RPlot_Z.Ex.Eco29 <-renderPlot({
        
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla4 
        pos <- which(!(names(entrenador) %in% 
                           c("latitude","longitude","Ecosystem")))
        df4 <- entrenador[entrenador$Ecosystem == Zvar,pos]
        
        #barplot(table(df4[,1])) 
        boxplot(df4[,25],col = "blue",main = 
                    paste0("Ecosystem: ",Zvar,". Variable: ",names(df4)[25]))
        
        
    })
    
    output$RPlot_Z.Ex.Eco30 <-renderPlot({
        
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla4 
        pos <- which(!(names(entrenador) %in% 
                           c("latitude","longitude","Ecosystem")))
        df4 <- entrenador[entrenador$Ecosystem == Zvar,pos]
        
        #barplot(table(df4[,1])) 
        boxplot(df4[,26],col = "blue",main = 
                    paste0("Ecosystem: ",Zvar,". Variable: ",names(df4)[26]))
        
        
    })
    
    output$RPlot_Z.Ex.Eco31 <-renderPlot({
        
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla4 
        pos <- which(!(names(entrenador) %in% 
                           c("latitude","longitude","Ecosystem")))
        df4 <- entrenador[entrenador$Ecosystem == Zvar,pos]
        
        #barplot(table(df4[,1])) 
        boxplot(df4[,27],col = "blue",main = 
                    paste0("Ecosystem: ",Zvar,". Variable: ",names(df4)[27]))
        
        
    })
    
    output$RPlot_Z.Ex.Eco32 <-renderPlot({
        
        #Input
        Zvar <- input$Select_Z.Ecosistema
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        
        #Tabla4 
        pos <- which(!(names(entrenador) %in% 
                           c("latitude","longitude","Ecosystem")))
        df4 <- entrenador[entrenador$Ecosystem == Zvar,pos]
        
        #barplot(table(df4[,1])) 
        boxplot(df4[,28],col = "blue",main = 
                    paste0("Ecosystem: ",Zvar,". Variable: ",names(df4)[28]))
        
        
    })
    
    #====================================================================
    #Panel: Prediccion Especifica.=======================================
    #====================================================================
    
    #UI's RENDERIZADAS /////////////////////////////////////////////////////////

    
    #Seleccion del ecosistema a plotear.
    tablaBReact <- reactive({
        #Action Button
        input$goButton_U.MP.Map
        
        #Evitamos plot automatico
        if(input$goButton_U.MP.Map == 0){
            return(NULL)
        }
        
        #Input
        lat <- isolate(input$Select_U.Lat)
        lon <- isolate(input$Select_U.Lon)
        Ubic <- isolate(input$Box_U.Ubic)
        R1 <- isolate(input$Select_U.R1)
        R2 <- isolate(input$Select_U.R2)
        R3 <- isolate(input$Select_U.R3)
        R4 <- isolate(input$Select_U.R4)
        measure <- isolate(input$Box_U.Medida)
            
        #CHeck
        if(is.null(Ubic)){
            Ubic <- "Continental"
        }
        
        withProgress(message ='Generando predicción', value = 0,{

            #==========
            incProgress(1/20)
            #==========
            
            # Ingreso de tablas
            tablaB <- data.frame(latitude = lat, longitude = lon)
            tablaA <- read.csv("GoogleEarthRiosLugares.csv")
            
            #==========
            incProgress(1/20)
            #==========
            
            #Calculamos las distancias
            tablaA <- distGoogleEarth(tablaB[c("latitude","longitude")],tablaA,
                                      dlMax = 100,tiempoMax = 0) 
            #Si no consiguio datos retorna NULL
            if(is.null(tablaA)){
                return(NULL)
            }
            rownames(tablaA) <- 1:nrow(tablaA)
            
            tablaA <- as.data.frame(tablaA)
            
            #==========
            incProgress(1/20)
            #==========
            
            #Reducimos dimension de la tablaA /////////////////////////////
            radios <- 1         #Asignamos valores de relleno.
            medida <- "median"  #Asignamos valores de relleno.
            
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
            
            #==========
            incProgress(1/20)
            #==========
            
            #Limpiamos memoria
            rm(tablaAaux)
            gc()
           
            #Unimos datos de google Earth
            tablaB <- impute.per.region2(tablaB,tablaA,radio = radios,
                                         medida = medida,Google = TRUE)
            
            #Check. Si es NULL el punto ingresado no tiene ambientales.
            if(is.null(tablaB)){
                return(NULL)
            }
            
            tablaB <- as.data.frame(tablaB)
           
            #==========
            incProgress(1/20)
            #==========
            
            #Formateamos los datos de Google Earth //////////////////
            
            #Renombramos
            tablaB$objeto <- paste0(tablaB$objeto,".d")
            
            #Eliminamos los lat=lon de los objetos
            tablaB$lon.objeto<- NULL
            tablaB$lat.objeto <- NULL
            tablaB$tipo <- NULL
            
            pos1 <- which(names(tablaB) %in% c("valor"))
            
            names(tablaB)[pos1] <- c("medida")
            
            #==========
            incProgress(1/20)
            #==========
            
            #Redondeamos
            tablaB$medida <- round(tablaB$medida,3)
            
            #Reemplazamos los NA's de medida por 120
            tablaB$medida[is.na(tablaB$medida)] <-
                rep(120,sum(is.na(tablaB$medida)))
            
            #nos quedamos solo con las unicaciones de menor distancia
            tablaB$fila <- 1:nrow(tablaB)
            posiciones <- 1:(ncol(tablaB)-3)
            
            tablaB$clave <-
                apply(tablaB[-(ncol(tablaB):(ncol(tablaB)-2))],1,function(vect){
                    vect <- paste(vect,collapse = "")
                    vect
                })
            
            #==========
            incProgress(1/20)
            #==========
            
            tablaB$clave <- as.factor(tablaB$clave)
            
            proc1 <- tapply(tablaB$fila,tablaB$clave,function(filas){
                dfAux <- tablaB[filas,]
                proc2 <- tapply(dfAux$fila,dfAux$objeto,function(filas){
                    if(length(filas) != 1){
                        filas <- filas[-which.min(tablaB$medida[filas])]
                        tablaB$medida[filas] <<- 
                            rep("Elim",length(tablaB$medida[filas]))
                    }
                    return(NULL)
                })
                return(NULL)
            })
            
            #==========
            incProgress(1/20)
            #==========
            
            #liberamos memoria
            rm(proc1)
            gc()
            
            tablaB$fila <- NULL
            tablaB$clave <- NULL
            
            #Eliminamos los que seleccionamos previamente
            tablaB <- tablaB[tablaB$medida != "Elim",]
            
            #Procedimiento spread para datos no unicos de objeto.
            #Construccion del indice.
            tablaB$index <- rep(NA,times = nrow(tablaB))	
            tab <- table(tablaB$objeto)
            
            proc_app <- sapply(names(tab),function(obj){
                tablaB$index[which(tablaB$objeto == obj)] <<- 1:tab[obj]
            })
            rm(proc_app)
            gc()
            
            #==========
            incProgress(1/20)
            #==========
            
            tablaB <- spread(tablaB, key = objeto, value = medida)
            
            #Eliminamos variables inecesarias
            tablaB$index <- NULL
            tablaB$NA.d <- NULL
            tablaB$NA.t <- NULL
            
            #Juntamos similares
            tablaB$Grupos <-apply(tablaB[posiciones],1,function(vect){
                vect <- paste(vect,collapse = "")
                vect
            })
            tablaB$Grupos <- as.factor(tablaB$Grupos)
            
            #==========
            incProgress(1/20)
            #==========
            
            #Calculamos promedios
            for(i in (posiciones[length(posiciones)]+1):(ncol(tablaB) - 1)){
                t <- tapply(tablaB[i][,1],tablaB$Grupos,median,na.rm = T)	
                tablaB[i][,1] <-  sapply(tablaB$Grupos,function(x){
                    unname(t[as.character(x)])
                })
            }
            
            #Hacemos unique	
            tablaB$Grupos <- NULL
            
            tablaB <- tablaB %>% unique()
            
            #Imputamos NA's colocando el valor 120 para indicar que el objeto no
            #influye sobre el punto de interes.
            for(i in 1 : ncol(tablaB)){
                posNas <- which(is.na(tablaB[,i]))
                tablaB[,i][posNas] <- rep(120,times = length(posNas))
            }
            
            #Agregamos variables faltantes
            Necesarias <- c("Laguna.d",
                "Pueblo.d",
                "Ciudad.d",
                "Puerto.d",
                "Quebrada.Seca.d",
                "Rio.Seco.d",
                "Quebrada.d",
                "Rio.d",
                "Termoelectrica.d",
                "Refineria.d",
                "Petroquimica.d")
            
            #Completamos predictores
            posFalt <- which(!(Necesarias %in% names(tablaB)))
            
            if(length(posFalt) != 0){
                dfFalt <- matrix(rep(120,times =(nrow(tablaB)*length(posFalt))),
                                 nrow(tablaB),length(posFalt))
                colnames(dfFalt) <- Necesarias[posFalt]
                dfFalt <- as.data.frame(dfFalt)
                
                #Juntamos
                tablaB <- cbind(tablaB,dfFalt)
            }
            
            #Extraemos solo los predictores necesarios
            tablaB <- tablaB[c("latitude","longitude",Necesarias)]
            
            #Formateamos
            tablaB$Laguna.d <- as.numeric(as.character(tablaB$Laguna.d))
            tablaB$Pueblo.d <- as.numeric(as.character(tablaB$Pueblo.d))
            tablaB$Puerto.d <- as.numeric(as.character(tablaB$Puerto.d))
            tablaB$Quebrada.d <- as.numeric(as.character(tablaB$Quebrada.d))
            tablaB$Rio.Seco.d <- as.numeric(as.character(tablaB$Rio.Seco.d))
            tablaB$Quebrada.Seca.d <-
                as.numeric(as.character(tablaB$Quebrada.Seca.d))
            tablaB$Rio.d <- 
                as.numeric(as.character(tablaB$Rio.d))
            tablaB$Termoelectrica.d <- 
                as.numeric(as.character(tablaB$Termoelectrica.d))
            tablaB$Refineria.d <- 
                as.numeric(as.character(tablaB$Refineria.d))
            tablaB$Petroquimica.d <- 
                as.numeric(as.character(tablaB$Petroquimica.d))
            
            #Procesamos
            tablaB$Afluente.Seco.d <- unname(apply(tablaB[c(
                "Quebrada.Seca.d","Rio.Seco.d")],1,function(x){
                    vect <- min(x,na.rm = T)
                    return(vect)
                }))

            tablaB$Quebrada.Seca.d <- NULL
            tablaB$Rio.Seco.d <- NULL

            #Unimos objetos costeros
            tablaB$Petroleo.d <- unname(apply(tablaB[c(
            "Termoelectrica.d","Refineria.d","Petroquimica.d")],1,function(x){
                    vect <- min(x,na.rm = T)
                    return(vect)
                }))

            tablaB$Termoelectrica.d <- NULL
            tablaB$Refineria.d <- NULL
            tablaB$Petroquimica.d <- NULL

            #Unimos objetos costeros
            tablaB$Poblado.d <- unname(apply(tablaB[c(
                "Pueblo.d","Ciudad.d")],1,function(x){
                    vect <- min(x,na.rm = T)
                    return(vect)
                }))
            
            tablaB$Pueblo.d <- NULL
            tablaB$Ciudad.d <- NULL
            
            #Frmateamos
            tablaB$Poblado.d <- as.numeric(as.character(tablaB$Poblado.d))
            
            
            #Unimos datos con informacion ambiental_________________________________
            
            #==========
            incProgress(1/20)
            #==========
            
            tablaA <- read.csv("MatrizAmbientalGIO1_Filtrada.csv")
            
            radios <- c(R1,R2,R3,R4)
            medida <- measure
            
            #Reducimos dimension de la tablaA /////////////////////////////
            tablaA$Ind <- rep(NA,nrow(tablaA))
            dmax <- max(radios)     #Radio maximo ingresado
            dmax <- dmax/106        #De kilometros a grados con ampliacion de medida
            
            #Tabla auxiliar
            tablaAaux <- tablaA[c("Ind","latitude","longitude")]
            tablaAaux$fila <- 1:nrow(tablaA)
            
            #==========
            incProgress(1/20)
            #==========
            
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
            
            #==========
            incProgress(1/20)
            #==========
            tablaB <- impute.per.region2(tablaB,tablaA,radio=radios,
                                         medida=medida)
            #Check. Si es NULL el punto ingresado no tiene ambientales.
            if(is.null(tablaB)){
                return(NULL)
            }
            tablaB <- as.data.frame(tablaB)

            #Unimos datos con informacion ambiental_________________________________
            
            #==========
            incProgress(1/20)
            #==========
            
            tablaA <- read.csv("MatrizAmbientalGIO2_Filtrada.csv")
            
            radios <- c(R1,R2,R3,R4)
            medida <- measure
            
            #Reducimos dimension de la tablaA /////////////////////////////
            tablaA$Ind <- rep(NA,nrow(tablaA))
            dmax <- max(radios)     #Radio maximo ingresado
            dmax <- dmax/106        #De kilometros a grados con ampliacion de medida
            
            #Tabla auxiliar
            tablaAaux <- tablaA[c("Ind","latitude","longitude")]
            tablaAaux$fila <- 1:nrow(tablaA)
            
            #==========
            incProgress(1/20)
            #==========
            
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
            
            #==========
            incProgress(1/20)
            #==========
            
            tablaB <- impute.per.region2(tablaB,tablaA,radio=radios,
                                         medida=medida)
            #Check. Si es NULL el punto ingresado no tiene ambientales.
            if(is.null(tablaB)){
                return(NULL)
            }
            tablaB <- as.data.frame(tablaB)
            
            #Unimos datos con informacion ambiental_________________________________
            #==========
            incProgress(1/20)
            #==========
            
            tablaA <- read.csv("CorrientesAux.csv")
            tablaA <- tablaA[,c(8,11,12)]
            
            radios <- c(1,5,20,50)
            medida <- measure
            
            #Reducimos dimension de la tablaA /////////////////////////////
            tablaA$Ind <- rep(NA,nrow(tablaA))
            dmax <- max(radios)     #Radio maximo ingresado
            dmax <- dmax/106        #De kilometros a grados con ampliacion de medida
            
            #Tabla auxiliar
            tablaAaux <- tablaA[c("Ind","latitude","longitude")]
            tablaAaux$fila <- 1:nrow(tablaA)
            
            #==========
            incProgress(1/20)
            #==========
            
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
            
            #==========
            incProgress(1/20)
            #==========
            
            tablaB <- impute.per.region2(tablaB,tablaA,radio=radios,
                                         medida=medida)
            #Check. Si es NULL el punto ingresado no tiene ambientales.
            if(is.null(tablaB)){
                return(NULL)
            }
            tablaB <- as.data.frame(tablaB)
            
            #Agregamos InsularCoast
            tablaB$InsularCoast <- rep(Ubic,nrow(tablaB))
            
            #==========
            incProgress(1/20)
            #==========
        })
    
        #Parche
        if("InsularCoast" %in% names(tablaB) == FALSE){
            return(NULL)
        }
        
        #Exportamos tabla
        return(tablaB)
    })
    
    #Prediccion
    predictReact <- reactive({
        #Input
        tablaB <- tablaBReact()
        
        #Check
        if(is.null(tablaB)){
            return(NULL)
        }

        #Subsetting de predictores
        tablaB <- tablaB[variables]
        
        #Realizamos prediccion solo cuando no hay NAS
        if(any(sapply(tablaB,is.na))){
            pred <- NULL
        }else{
            pred <- predict(modFit,tablaB, type = "class") 
        }
        
        #Ordenamos nombres
        tablaB <-tablaB[names(MarVzla)[-c(1,2,(ncol(MarVzla)-1),ncol(MarVzla))]]
        
        #Evaluamos confiabilidad de la prediccion
        confiable <- unname(apply(tablaB[,1:ncol(tablaB)],1,function(vars){
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
        
        #Retornamos resultados
        return(list(pred,confiable))
    })
    
    #SALIDA DE PREDICCION //////////////////////////////////////////////////////
    
    #Prediccion
    output$RText_U.Prediccion <- renderText({

        Zvar <- predictReact()[[1]]
        
        #Check
        if(is.null(Zvar)){
            return(NULL)
        }
        
        paste0("Predicción del Ecosistema: ",Zvar)
    })
    
    #Confiabilidad
    output$RText_U.Confiabilidad <- renderText({
        
        conf <- predictReact()[[2]]
        
        #Check
        if(is.null(conf)){
            return(NULL)
        }
        
        paste0("Predicción confiable: ",conf)
    })
    
    #Warning prediction
    output$RText_U.WarnPred <- renderText({
        #Action Button
        input$goButton_U.MP.Map
        
        #Evitamos plot automatico
        if(input$goButton_U.MP.Map == 0){
            return(NULL)
        }
        
        Zvar <- isolate(predictReact()[[1]])
        
        #Check
        if(is.null(Zvar)){
            text <- "Hay datos faltantes.Incremente los radios de busqueda"
        }else{
            text <- "" 
        }
        return(text) 
    })
    
    #Prediccion
    output$RTable_U.Prediccion <- renderTable({
        #Action Button
        input$goButton_U.MP.Map
        
        #Evitamos plot automatico
        if(input$goButton_U.MP.Map == 0){
            return(NULL)
        }
        
        tablaB <- tablaBReact()
        
        #Check
        if(is.null(tablaB)){
            return(NULL)
        }
        
        #Salida de tabla
        tab <- as.data.frame(t(tablaB))
        tab$nb <- names(tablaB)
        tab <- tab[c(2,1)]
        names(tab) <- c("","")
        tab <- tab[3:nrow(tab),]
        tab
    })
    
    #MAPA //////////////////////////////////////////////////////////////////////

    
    #Mapa con puntos de geolocalizacion.
    output$RPlot_U.MP.Map <-renderPlot({
        #Actualizar
        input$goButton_E.Act2
        
        #Action Button
        input$goButton_U.MP.Map
        
        #Evitamos plot automatico
        if(input$goButton_U.MP.Map == 0){
            return(NULL)
        }
        
        #Input
        lat <- isolate(input$Select_U.Lat)
        lon <- isolate(input$Select_U.Lon)
        Zvar <- isolate(predictReact()[[1]])

        latMin <- isolate(input$Select_U.MP.latMin)
        latMax <- isolate(input$Select_U.MP.latMax)
        lonMin <- isolate(input$Select_U.MP.lonMin)
        lonMax <- isolate(input$Select_U.MP.lonMax)
        
        #Chequeamos Input.
        if(is.null(Zvar)){
            return(NULL)
        }
        if(is.null(latMin)){
            return(NULL)
        }
        if(is.null(latMax)){
            return(NULL)
        }
        if(is.null(lonMin)){
            return(NULL)
        }
        if(is.null(lonMax)){
            return(NULL)
        }
    
        
        withProgress(message = 'Creando Mapa', value = 0, {
            
            #Creamos data set
            df <- data.frame(lat = lat, lon = lon)

            #==========
            incProgress(1/4)
            #==========
            
            #Plot del mapa
            global <- map_data("world")
            
            #recortamos
            if(latMin >= latMax | lonMin >= lonMax){
                global <- global[global$long <= -60 & global$long >= -74 &
                                     global$lat <= 17 & global$lat >= 8,]
            }else{
                global <- global[global$long <= lonMax & global$long >= lonMin &
                                     global$lat <= latMax & global$lat >= latMin,]
            }
            
            #==========
            incProgress(1/4)
            #==========
            
            plotMapa  <- ggplot() + 
                geom_polygon(data = global, aes(x=long, y = lat, group = group),
                             fill = "green4", color = "black") + 
                coord_fixed(1.3)
            
            
            #==========
            incProgress(1/4)
            #==========
            
            #Plot del mapa
            plotMapa <- plotMapa  +
                geom_point(data = df,aes(x = lon,y = lat),
                           col = "blue",size = 6) +
                ggtitle(paste0("Mapa del Ecosistema ",Zvar))
            
            
            #==========
            incProgress(1/4)
            #==========
            
            #Exportamos
            return(plot(plotMapa))
            
        })
    })

})
