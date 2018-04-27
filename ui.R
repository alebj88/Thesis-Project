library(shiny)

#INTERFAZ DE USUARIO.////////////////////////////////////////////////////////

#Archive ui.R.
shinyUI(fluidPage(
    titlePanel("Analisis Exploratorio Data Corales"),					
    sidebarLayout(													
        sidebarPanel(	
            
            #Evaluador principal ShowMe
            #textOutput("RUi_Variables"),
            #textOutput("ShowMe"),

            #Configuracion general...................................
            h3("Configuracion General"),
            checkboxInput("Box_Numerica","Marque para indicar que la variable 
                                        de estudio es numerica.",value=F), 
            
            #Input zona de Estudio.----------------------------------
            selectInput("Select_Zona","Especifique la zona de estudio:",
                        choices  = c("Todas",as.character(
                                            unique(MatrizBiologica$Localidad))
                        ),
                        selected = "Todas"
            ),
            
            #Input variable explicada.-------------------------------
            uiOutput("RUi_Select_Zvar"),

            #Input valor de la variable a explicar.------------------
            uiOutput("RUi_Select_Zval"),
            
            #Input variable exploratoria 1.--------------------------
            selectInput("Select_Yvar", "Seleccione la primera variable de 
                                                        exploracion:",
                        choices  = c(as.character(varDepNum)),
                        selected = "nitrate"
            ),
            
            #Input objeto corrientes.--------------------------------
            uiOutput("RUi_Select_CorrY"),
            
            #Input objeto distancia.---------------------------------
            uiOutput("RUi_Select_DistY"),
            
            #Input variable exploratoria 2.--------------------------
            selectInput("Select_Xvar", "Seleccione la segunda variable de 
                                                        exploracion:",
                        choices  = c(as.character(varDepNum)),
                        selected = "oxygen"
            ),
            
            #Input objeto corrientes.--------------------------------
            uiOutput("RUi_Select_CorrX"),
            
            #Input objeto distancia.---------------------------------
            uiOutput("RUi_Select_DistX")
            
        ), 
        
        #Panel de Resultados/////////////////////////////////////////////////
        mainPanel(
            tabsetPanel(type="tabs",
                #Tabla de datos----------------------------------------------
                tabPanel("Tabla De Datos",br(),
                    dataTableOutput("RDT_TablaDatos"),
                    h5(strong("Resumen estadistico de la variable: ")),
                    h5(strong(textOutput("RText_DTY"))),
                    h6(tableOutput("RTable_DTY")),
                    h5(strong("Resumen estadistico de la variable: ")),
                    h5(strong(textOutput("RText_DTX"))),
                    h6(tableOutput("RTable_DTX"))
                ),	
                #Mapa--------------------------------------------------------
                tabPanel("Mapa",br(),
                    plotOutput("RPlot_Map",width = "100%", height = "550px"),
                    h6(textOutput("Rtext_text1")),
                    h6(textOutput("Rtext_text2")),
                    h6(textOutput("Rtext_text3"))
                ),	
                #Boxplot-----------------------------------------------------
                tabPanel("Diagrama de Cajas",br(),
                    verticalLayout( 
                        #Grafico
                        plotOutput("RPlot_Box")
                        ,
                        #uiOutput("RUi_submitButton"),br()
                        #actionButton("R_Action","Aplicar Cambios")
                        #,
                        #Controles del grafico
                        splitLayout( 
                            uiOutput("RUi_checkboxGroup.Zvar")
                            ,
                            uiOutput("RUi_checkboxGroup.Zona")
                        )
                    )
                ),
                #Histogramas-------------------------------------------------
                tabPanel("Histogramas",br(),
                    plotOutput("RPlot_Hist")
                    ,
                    #Controles del grafico
                    splitLayout( 
                        uiOutput("RUi_checkboxGroup2.Zvar")
                        ,
                        uiOutput("RUi_checkboxGroup2.Zona")
                    )
                ),
                #Scatterplot-------------------------------------------------
                tabPanel("Diagrama de Dispersion",br(),
                         navbarPage(title="Plots",
                                #Diagrama de Dispersion----------------------
                                tabPanel("Diagrama de Dispersion",br(),
                                         plotOutput("RPlot_Scat")
                                         ,
                                         splitLayout( 
                                             uiOutput("RUi_checkboxEtiqueta")
                                             ,
                                             uiOutput("RUi_checkboxJitter")
                                         ),
                                         #Controles del grafico
                                         splitLayout( 
                                             uiOutput("RUi_checkboxGroup3.Zvar")
                                             ,
                                             uiOutput("RUi_checkboxGroup3.Zona")
                                             ,
                                             uiOutput("RUi_checkboxGroup3.Size")
                                         )
                                ),
                                #K-Medias------------------------------------
                                tabPanel("K-Medias",br(),
                                         plotOutput("RPlot_Kmeans")
                                         ,
                                         h6(uiOutput("RUi_SliderK"))
                                         ,
                                         h4("Tabla de Frecuencias")
                                         ,
                                         h6(tableOutput("RTable_FrecK"))
                                ),	
                                #Heatmap-------------------------------------
                                tabPanel("Heatmap",br(),
                                         plotOutput("RPlot_HeatM")
                                )
                     )  
                ),
                #Clusterizacion Jerarquica-----------------------------------
                tabPanel("Cluster Jerarquico",br(),
                         plotOutput("RPlot_Hclust")
                         ,
                         uiOutput("RUi_checkboxLegend")
                         ,
                         uiOutput("RUi_checkboxVarX")
                         ,
                         uiOutput("RUi_checkboxVarY")
                ),
                #Support Vector Machine--------------------------------------
                tabPanel("Clasificacion por SVM",br()#,
                    #textOutput("RText_summaryB")
                ),
                #Singular Value Descomposition-------------------------------
                tabPanel("SVD",br()#,
                    #textOutput("RText_Distribucion")
                )					
            )
        )
    )
))
