library(shiny)
library(shinythemes)

#INTERFAZ DE USUARIO.///////////////////////////////////////////////////////////

#Archive ui.R.
shinyUI(fluidPage(
    titlePanel("Análisis Exploratorio Data Corales"),
    
    #Panel de Exploracion con 2 predictores.=================================
    navbarPage(title = "Tipo",theme = shinytheme("cerulean"),
        tabPanel("Exploración Basica",fluidPage(
             sidebarLayout(													
                 sidebarPanel(width = 4,
                     
                     #Evaluador principal ShowMe
                     #textOutput("RUi_Variables"),
                     #textOutput("ShowMe"),
                     
                     #Configuracion general...................................
                     h3("Configuración General:"),
                     checkboxInput("Box_Numerica","Marque para indicar que la 
                                   variable  de estudio es numérica.",value=F), 
                     
                     #Input zona de Estudio.----------------------------------
                     selectInput("Select_Zona","Especifique la zona de estudio:"
                             ,choices  = c("Todas",as.character(
                                            unique(MatrizBiologica$Localidad))
                             ),
                             selected = "Todas"
                     ),
                     
                     #Input variable explicada.-------------------------------
                     uiOutput("RUi_Select_Zvar"),
                     
                     #Input valor de la variable a explicar.------------------
                     uiOutput("RUi_Select_Zval"),
                     
                     #Input variable exploratoria 1.--------------------------
                     selectInput("Select_Yvar", 
                             "Seleccione la primera variable de exploración:",
                             choices  = c(as.character(varDepNum)
                             ),
                             selected = "nitrate"
                     ),
                     
                     #Input objeto corrientes.--------------------------------
                     uiOutput("RUi_Select_CorrY"),
                     
                     #Input objeto distancia.---------------------------------
                     uiOutput("RUi_Select_DistY"),
                     
                     #Input variable exploratoria 2.--------------------------
                     selectInput("Select_Xvar", 
                            "Seleccione la segunda variable de  exploración:",
                             
                             choices  = c(as.character(varDepNum)
                             ),
                             selected = "oxygen"
                     ),
                     
                     #Input objeto corrientes.--------------------------------
                     uiOutput("RUi_Select_CorrX"),
                     
                     #Input objeto distancia.---------------------------------
                     uiOutput("RUi_Select_DistX")
                     
                     ), 
                 
                 #Panel de Resultados/////////////////////////////////////////
                 mainPanel(
                     tabsetPanel(type="tabs",
                         #Tabla de datos--------------------------------------
                         tabPanel("Tabla de Datos",br(),
                              dataTableOutput("RDT_TablaDatos"),
                              h5(strong(
                                  "Resumen estadístico de la variable: ")),
                              h5(strong(textOutput("RText_DTY"))),
                              h6(tableOutput("RTable_DTY")),
                              h5(strong(
                                  "Resumen estadístico de la variable: ")),
                              h5(strong(textOutput("RText_DTX"))),
                              h6(tableOutput("RTable_DTX"))
                         ),	
                         #Mapa------------------------------------------------
                         tabPanel("Mapa",br(),
                              plotOutput("RPlot_Map",
                                         width = "100%", height = "550px"),
                              h6(textOutput("Rtext_text1")),
                              h6(textOutput("Rtext_text2")),
                              h6(textOutput("Rtext_text3"))
                         ),	
                         #Boxplot---------------------------------------------
                         tabPanel("Diagrama de Cajas",br(),
                              verticalLayout( 
                                  #Grafico
                                  plotOutput("RPlot_Box")
                                  ,
                                  #Controles del grafico
                                  splitLayout( 
                                      uiOutput("RUi_checkboxGroup.Zvar")
                                      ,
                                      uiOutput("RUi_checkboxGroup.Zona")
                                  )
                              )
                         ),
                         #Histogramas-----------------------------------------
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
                         #Scatterplot-----------------------------------------
                         tabPanel("Gráfico de Dispersión",br(),
                            navbarPage(title="Plots",
                                 #Diagrama de Dispersion----------------------
                                 tabPanel("Diagrama de Dispersión",br(),
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
                                 tabPanel("Mapa de Calor",br(),
                                      plotOutput("RPlot_HeatM")
                                 )
                            )  
                         ),
                         #Clusterizacion Jerarquica---------------------------
                         tabPanel("Dendrograma",br(),
                              plotOutput("RPlot_Hclust")
                              ,
                              uiOutput("RUi_checkboxLegend")
                              ,
                              uiOutput("RUi_checkboxVarX")
                              ,
                              uiOutput("RUi_checkboxVarY")
                         )
                     )
                 )
             )
        )),
        #Panel de Clasificacion General.======================================
        tabPanel("Clasificación",fluidPage(
            sidebarLayout(													
                sidebarPanel(width = 4,
                     
                     #Evaluador principal ShowMe
                     #textOutput("RUi_Variables"),
                     #textOutput("ShowMe"),
                     
                     #Configuracion general...................................
                     h3("Configuración General:"),
                     checkboxInput("Box_Numerica2","Marque si desea clasificar
                                            una variable numérica.",value=F), 
                     
                     #Input zona de Estudio.----------------------------------
                     selectInput("Select_Zona2","Indique la zona de estudio:"
                             ,choices  = c("Todas",as.character(
                                 unique(MatrizBiologica$Localidad))
                             ),
                             selected = "Todas"
                     ),
                     
                     #Input variable explicada.-------------------------------
                     uiOutput("RUi.Clas_Select_Zvar"),
                     
                     #Clasificadores independientes.--------------------------
                     hr(),
                     h5("Seleccione 3 variables de clasificación que desee
                        estudiar de forma independiente."),
                     
                     #Input variable 1.
                     selectInput("Select_var1", "variable 1:",
                             choices  = c(as.character(varDepNum2)
                             ),
                             selected = "nitrate"
                     ),
                     
                     #Input variable 2.
                     selectInput("Select_var2", "variable 2:",
                                 choices  = c(as.character(varDepNum2)
                                 ),
                                 selected = "oxygen"
                     ),
                     
                     #Input variable 3.
                     selectInput("Select_var3", "variable 3:",
                             choices  = c(as.character(varDepNum2)
                             ),
                             selected = "phosphate"
                     )

                ), 
                
                #Panel de Resultados/////////////////////////////////////////
                mainPanel(
                    tabsetPanel(type="tabs",
                        #Tabla de datos--------------------------------------
                        tabPanel("SVD",br(),
                            plotOutput("RPlot_SVD"),
                            splitLayout( 
                                uiOutput("RUi_autoVec1")
                                ,
                                uiOutput("RUi_autoVec2")
                            )
                            # dataTableOutput("RDT_TablaDatos"),
                            # h5(strong(
                            #     "Resumen estadístico de la variable: ")),
                            # h5(strong(textOutput("RText_DTY"))),
                            # h6(tableOutput("RTable_DTY")),
                            # h5(strong(
                            #     "Resumen estadístico de la variable: ")),
                             #h5(strong(textOutput("RText_DTX"))),
                            # h6(tableOutput("RTable_DTX"))
                        ),	
                        #Mapa------------------------------------------------
                        tabPanel("Mapa",br()#,
                             #plotOutput("RPlot_Map",
                                        #width = "100%", height = "550px"),
                             #h6(textOutput("Rtext_text1")),
                             #h6(textOutput("Rtext_text2")),
                             #h6(textOutput("Rtext_text3"))
                        ),	
                        #Boxplot---------------------------------------------
                        tabPanel("Diagrama de Cajas",br(),
                             verticalLayout( 
                                 #Grafico
                                 #plotOutput("RPlot_Box")
                                 #,
                                 #Controles del grafico
                                 #sidebarLayout( 
                                     #uiOutput("RUi_checkboxGroup.Zvar")
                                     
                                     #uiOutput("RUi_checkboxGroup.Zona")
                                 #)
                             )
                        ),
                        #Histogramas-----------------------------------------
                        tabPanel("Histogramas",br(),
                             #plotOutput("RPlot_Hist")
                             #,
                             #Controles del grafico
                             splitLayout( 
                                 #uiOutput("RUi_checkboxGroup2.Zvar")
                                 
                                 #uiOutput("RUi_checkboxGroup2.Zona")
                             )
                        ),
                        #Scatterplot-----------------------------------------
                        tabPanel("Gráfico de Dispersión",br(),
                             navbarPage(title="Plots",
                                #Diagrama de Dispersion----------------------
                                tabPanel("Diagrama de Dispersión",br(),
                                     #plotOutput("RPlot_Scat")
                                     #,
                                     splitLayout( 
                                         #uiOutput("RUi_checkboxEtiqueta")
                                         
                                         #uiOutput("RUi_checkboxJitter")
                                     ),
                                     #Controles del grafico
                                     splitLayout( 
                                         #uiOutput("RUi_checkboxGroup3.Zvar")
                                         #,
                                         #uiOutput("RUi_checkboxGroup3.Zona")
                                         #,
                                         #uiOutput("RUi_checkboxGroup3.Size")
                                     )
                                ),
                                #K-Medias------------------------------------
                                tabPanel("K-Medias",br()#,
                                     #plotOutput("RPlot_Kmeans")
                                     ##,
                                     #h6(uiOutput("RUi_SliderK"))
                                     
                                     #h4("Tabla de Frecuencias")
                                     #,
                                     #h6(tableOutput("RTable_FrecK"))
                                ),	
                                #Heatmap-------------------------------------
                                tabPanel("Mapa de Calor",br()#,
                                     #plotOutput("RPlot_HeatM")
                                )
                             )  
                        ),
                        #Clusterizacion Jerarquica---------------------------
                        tabPanel("Dendrograma",br()#,
                             #plotOutput("RPlot_Hclust")
                             
                             #uiOutput("RUi_checkboxLegend")
                             
                             #uiOutput("RUi_checkboxVarX")
                             
                             #uiOutput("RUi_checkboxVarY")
                        )
                    )
                )
            )
      
        )),
        #Panel de Regresion General.==========================================
        tabPanel("Regresión","Regresion")
    )
))