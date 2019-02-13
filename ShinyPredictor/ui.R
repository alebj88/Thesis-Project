#


library(shiny)
library(shinythemes)
options(encoding = 'UTF-8')

#INTERFAZ DE USUARIO.///////////////////////////////////////////////////////////

#Archive ui.R.
shinyUI(fluidPage(
    titlePanel(em("Predictor de Ecosistemas Coralinos"))
    ,
    navbarPage(title = "",theme = shinytheme("darkly"),
        
        #====================================================================
        #Panel: Zona Predefinida.============================================
        #====================================================================
        tabPanel("Zona Predefinida",
            sidebarLayout(													
                sidebarPanel(width = 3,
                     
                    #Configuracion general...................................
                    h4(strong("Ecosistemas a estudiar"))
                    ,
                    hr()
                    ,
                    #Ecosistema
                    h5(uiOutput("RUi_Select_Z.Ecosistema"))
                    ,
                    #Tabla a emplear
                    h5(selectInput("Select_NroTabla", "Valores a usar:",
                                   choices  = c("Muestra","Predicción"),
                                   selected = "Muestra"
                    )),
                    numericInput("Select_Z.MP.Tamano","Tamaño del Punto: ",
                                 min = 1, max = 6, value = 4,step = 1
                    ),
                    fluidRow(
                        column(6,numericInput("Select_Z.MP.latMin",
                                              "Lat - Min",
                                              min = -90, max = 90,
                                              value = 8,step = 1)
                        ),
                        column(6,numericInput("Select_Z.MP.latMax",
                                              "Lat - Max",
                                              min = -90, max = 90, 
                                              value = 17,step = 1)
                        )
                    ),
                    fluidRow(
                        column(6,numericInput("Select_Z.MP.lonMin",
                                              "Lon - Min",
                                              min = -180, max = 180,
                                              value = -74,step = 1)
                        ),
                        column(6,numericInput("Select_Z.MP.lonMax",
                                              "Lon - Max",
                                              min = -180, max = 180, 
                                              value = -60,step = 1)
                        )
                    ),
                    #Actualizacion
                    h5("En caso de haber realizado un reentrenamiento del 
                       algoritmo, actualice la lista de ecosistemas marcando
                       el boton 'Actualizar'.")
                    ,
                    actionButton("goButton_E.Act",
                                 "Actualizar",
                                 icon("refresh"),
                                 style = "color: white; 
                                 background:#0066cc")
                ), 
                #Panel de Resultados/////////////////////////////////////////
                mainPanel(width = 9,
                    tabsetPanel(id="panels",type="tabs",
                        #Mapa------------------------------------------------
                        tabPanel("Mapa",br(),
     
                            h4(strong("Spatial Visualization")) 
                            ,
                            plotOutput("RPlot_Z.MP.Map",
                                       width = "100%", height = "550px")
                        ),
                        #Graficos-------------------------------------------
                        tabPanel("Exploración",br(),
          
                            h4(textOutput("Rtext_Z.Ex.Ubic"))
                            ,
                            plotOutput("RPlot_Z.Ex.Eco2",
                                       width = "100%", height = "550px"
                            ),
                            h6(tableOutput("RTable_Z.Ex.Eco2")
                            ), 
                            plotOutput("RPlot_Z.Ex.Eco3",
                                       width = "100%", height = "550px"
                            ), 
                            h6(tableOutput("RTable_Z.Ex.Eco3")
                            ), 
                            plotOutput("RPlot_Z.Ex.Eco1",
                                       width = "100%", height = "550px"
                            ),  
                            h6(tableOutput("RTable_Z.Ex.Eco1")
                            ),
                            plotOutput("RPlot_Z.Ex.Eco4",
                                       width = "100%", height = "550px"
                            ),
                            h6(tableOutput("RTable_Z.Ex.Eco4")
                            ),
                            plotOutput("RPlot_Z.Ex.Eco5",
                                       width = "100%", height = "550px"
                            ),
                            h6(tableOutput("RTable_Z.Ex.Eco5")
                            ),
                            plotOutput("RPlot_Z.Ex.Eco55",
                                        width = "100%", height = "550px"
                            ),
                            plotOutput("RPlot_Z.Ex.Eco6",
                                       width = "100%", height = "550px"
                            ),
                            plotOutput("RPlot_Z.Ex.Eco7",
                                       width = "100%", height = "550px"
                            ),
                            plotOutput("RPlot_Z.Ex.Eco8",
                                       width = "100%", height = "550px"
                            )#,
                            # plotOutput("RPlot_Z.Ex.Eco9",
                            #            width = "100%", height = "550px"
                            # ),
                            # plotOutput("RPlot_Z.Ex.Eco10",
                            #            width = "100%", height = "550px"
                            # ),
                            # plotOutput("RPlot_Z.Ex.Eco11",
                            #            width = "100%", height = "550px"
                            # ),
                            # plotOutput("RPlot_Z.Ex.Eco12",
                            #            width = "100%", height = "550px"
                            # ),
                            # plotOutput("RPlot_Z.Ex.Eco13",
                            #            width = "100%", height = "550px"
                            # ),
                            # plotOutput("RPlot_Z.Ex.Eco14",
                            #            width = "100%", height = "550px"
                            # ),
                            # plotOutput("RPlot_Z.Ex.Eco15",
                            #            width = "100%", height = "550px"
                            # ),
                            # plotOutput("RPlot_Z.Ex.Eco16",
                            #            width = "100%", height = "550px"
                            # ),
                            # plotOutput("RPlot_Z.Ex.Eco17",
                            #            width = "100%", height = "550px"
                            # ),
                            # plotOutput("RPlot_Z.Ex.Eco18",
                            #            width = "100%", height = "550px"
                            # ),
                            # plotOutput("RPlot_Z.Ex.Eco19",
                            #            width = "100%", height = "550px"
                            # ),
                            # plotOutput("RPlot_Z.Ex.Eco20",
                            #            width = "100%", height = "550px"
                            # ),
                            # plotOutput("RPlot_Z.Ex.Eco21",
                            #            width = "100%", height = "550px"
                            # ),
                            # plotOutput("RPlot_Z.Ex.Eco22",
                            #            width = "100%", height = "550px"
                            # ),
                            # plotOutput("RPlot_Z.Ex.Eco23",
                            #            width = "100%", height = "550px"
                            # ),
                            # plotOutput("RPlot_Z.Ex.Eco24",
                            #            width = "100%", height = "550px"
                            # ),
                            # plotOutput("RPlot_Z.Ex.Eco25",
                            #            width = "100%", height = "550px"
                            # ),
                            # plotOutput("RPlot_Z.Ex.Eco26",
                            #            width = "100%", height = "550px"
                            # ),
                            # plotOutput("RPlot_Z.Ex.Eco27",
                            #            width = "100%", height = "550px"
                            # ),
                            # plotOutput("RPlot_Z.Ex.Eco28",
                            #            width = "100%", height = "550px"
                            # ),
                            # plotOutput("RPlot_Z.Ex.Eco29",
                            #            width = "100%", height = "550px"
                            # ),
                            # plotOutput("RPlot_Z.Ex.Eco30",
                            #            width = "100%", height = "550px"
                            # ),
                            # plotOutput("RPlot_Z.Ex.Eco31",
                            #            width = "100%", height = "550px"
                            # ),
                            # plotOutput("RPlot_Z.Ex.Eco32",
                            #            width = "100%", height = "550px"
                            # )
                            
                        )
                    )
                )
            )
        ),
        
        #====================================================================
        #Panel: Prediccion Especifica.=======================================
        #====================================================================
        tabPanel("Ubicación Especifica",
            sidebarLayout(													
                sidebarPanel(width = 3,
                     
                    #Configuracion general...................................
                    h4(strong("Configurar Predicción"))
                    ,
                    hr()
                    ,
                    h5(strong("Coordenadas del ecosistema:"))
                    ,
                    #Latitud
                    numericInput("Select_U.Lat","Latitud",
                                 min = 8, max = 17, step = 1, value = 11
                    ),
                    #Longitud
                    numericInput("Select_U.Lon","Longitud",
                                 min = -73, max = -60, step = 1, value = -66
                    ),
                    radioButtons("Box_U.Ubic", "Ubicación:",
                                 choices = c("Continental","Insular"), 
                                 selected = "Continental",
                                 inline = T
                    ),
                    hr()
                    ,
                    h5(strong("Radios de búsqueda:"))
                    ,
                    numericInput("Select_U.R1","1er Radio (Kms)",
                                 min = 0, max = 5, step = 1, value = 0.5
                    ),
                    numericInput("Select_U.R2","2edo Radio (Kms)",
                                 min = 0, max = 25, step = 1, value = 1.5
                    ),
                    numericInput("Select_U.R3","3er Radio (Kms)",
                                 min = 0, max = 100, step = 1, value = 5
                    ),
                    numericInput("Select_U.R4","4to Radio (Kms)",
                                 min = 0, max = 250, step = 1, value = 20
                    ),
                    #Medida
                    radioButtons("Box_U.Medida", "Medida a utilizar:",
                                 choices = c("median","mean"), 
                                 selected = "mean",
                                 inline = T
                    ),
                    fluidRow(
                        column(6,numericInput("Select_U.MP.latMin",
                                              "Lat - Min",
                                              min = -90, max = 90,
                                              value = 8,step = 1)
                        ),
                        column(6,numericInput("Select_U.MP.latMax",
                                              "Lat - Max",
                                              min = -90, max = 90, 
                                              value = 17,step = 1)
                        )
                    ),
                    fluidRow(
                        column(6,numericInput("Select_U.MP.lonMin",
                                              "Lon - Min",
                                              min = -180, max = 180,
                                              value = -74,step = 1)
                        ),
                        column(6,numericInput("Select_U.MP.lonMax",
                                              "Lon - Max",
                                              min = -180, max = 180, 
                                              value = -60,step = 1)
                        )
                    )
                ), 
                #Panel de Resultados/////////////////////////////////////////
                mainPanel(width = 9,
                    #Mapa------------------------------------------------
                    splitLayout(cellWidths =
                                    c("50%","25%","25%"),
                        h4(strong("Spatial Visualization")) 
                        ,
                        actionButton("goButton_E.Act2",
                                     "Actualizar",
                                     icon("refresh"),
                                     style = "color: white; 
                                     background:#0066cc")
                        ,
                        actionButton("goButton_U.MP.Map",
                                     "Run Prediction",
                                     icon("paper-plane"),
                                     style = "color: white; 
                                     background: #35e51d")
                    ),
                    h5(span(strong(textOutput("RText_U.Prediccion"))),
                            style="color:#e6e600")
                    ,
                    h5(span(strong(textOutput("RText_U.Confiabilidad"))),
                       style="color:green")
                    ,
                    h5(span(textOutput("RText_U.WarnPred"), 
                            style="color:red")
                    ),
                    plotOutput("RPlot_U.MP.Map",
                                 width = "100%", height = "550px")
                    ,
                    h6(strong(tableOutput("RTable_U.Prediccion")))
                )
            )
        )
    )
))