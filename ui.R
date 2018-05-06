library(shiny)
library(shinythemes)
options(encoding = 'UTF-8')

#INTERFAZ DE USUARIO.///////////////////////////////////////////////////////////

#Archive ui.R.
shinyUI(fluidPage(
    titlePanel(em("Análisis Exploratorio Data Corales (Versión de Prueba)")),
    #titlePanel(em("Análisis Exploratorio Data Corales (Versión de Prueba)")),
    #ANALISIS EXPLORATORIO DATA CORALES
    navbarPage(title = "Tipo",theme = shinytheme("cerulean"),
               
        #====================================================================
        #Panel: Exploracion Basica.==========================================
        #====================================================================
        tabPanel("Exploración Básica",fluidPage(
            sidebarLayout(													
                sidebarPanel(width = 3,
                     
                    #Configuracion general...................................
                    h4(strong("Configuración General")),hr(),
                            checkboxInput("Box_Numerica","Marque si desea 
                                explicar una variable numérica.",value=F), 
                     
                    #Input zona de Estudio.----------------------------------
                    selectInput("Select_Zona","Zona de estudio:",
                            choices  = c("Todas",as.character(
                                            unique(MatrizBiologica$Localidad)))
                            ,
                            selected = "Todas"
                    ),
                     
                    #Input variable explicada.-------------------------------
                    h5(uiOutput("RUi_Select_Zvar")),
                     
                    #Input valor de la variable a explicar.------------------
                    uiOutput("RUi_Select_Zval"),
                     
                    #Input variable exploratoria 1.--------------------------
                    h5(selectInput("Select_Yvar","Variable de exploración #1",
                            choices  = c(as.character(varDepNum))
                            ,
                            selected = "nitrate"
                    )),
                     
                    #Input objeto corrientes.--------------------------------
                    uiOutput("RUi_Select_CorrY"),
                     
                    #Input objeto distancia.---------------------------------
                    uiOutput("RUi_Select_DistY"),
                     
                    #Input variable exploratoria 2.--------------------------
                    h5(uiOutput("RUi_Select_Xvar")),
                     
                    #Input objeto corrientes.--------------------------------
                    uiOutput("RUi_Select_CorrX"),
                     
                    #Input objeto distancia.---------------------------------
                    uiOutput("RUi_Select_DistX")
                     
                    ), 
                 
                #Panel de Resultados/////////////////////////////////////////
                mainPanel(
                    tabsetPanel(id="panels",type="tabs",
                        #Tabla de datos--------------------------------------
                        tabPanel("Tabla de Datos",br(),
                                dataTableOutput("RDT_E.TablaD1")
                                ,
                                h5(strong("Tabla de Frecuencias:"))
                                ,
                                h6(tableOutput("RTable_E.TablaD2"))
                        ),	
                        #Mapa------------------------------------------------
                        tabPanel("Mapa",br(),
                            splitLayout( 
                                br()
                                ,
                                br()
                                ,
                                #Boton de action.
                                actionButton("goButtonMap1","Submit Map",
                                            icon("paper-plane"),
                                            style = "color: white; 
                                            background: #35e51d")
                            ),
                            plotOutput("RPlot_Map",
                                         width = "100%", height = "550px"),
                            h6(textOutput("Rtext_text1"))
                            ,
                            h6(textOutput("Rtext_text2"))
                            ,
                            h6(textOutput("Rtext_text3"))
                        ),	
                        #Boxplot---------------------------------------------
                        tabPanel("Diagrama de Cajas",br(),
                            navlistPanel(widths = c(2, 10),
                                tabPanel("Boxplot por Localidad",
                                    plotOutput("RPlot_E.Box1")
                                    ,
                                    splitLayout( 
                                        uiOutput("RUi_checkbox.EBP1.Leg")
                                        ,
                                        br()
                                        ,
                                        #Boton de action.
                                        actionButton("goButtonE.Box1",
                                                     "Submit Plot",
                                                     icon("paper-plane"),
                                                     style = "color: white; 
                                                     background: #35e51d")
                                    ),
                                    #Controles del grafico
                                    splitLayout( 
                                        uiOutput("RUi_checkboxGroup.EBP1.Zval")
                                        ,
                                        uiOutput("RUi_checkboxGroup.EBP1.Loc")
                                        ,
                                        br()
                                    )
                                ),
                                tabPanel("Boxplot Var #1",
                                    plotOutput("RPlot_E.Box2")
                                    ,
                                    splitLayout( 
                                        uiOutput("RUi_checkbox.EBP2.Leg")
                                        ,
                                        br()
                                        ,
                                        #Boton de action.
                                        actionButton("goButtonE.Box2",
                                                     "Submit Plot",
                                                     icon("paper-plane"),
                                                     style = "color: white; 
                                                     background: #35e51d")
                                    ),
                                    splitLayout(cellWidths=c("45%","55%"),
                                        uiOutput("RUi_checkboxGroup.EBP2.Zval")
                                        ,
                                        verticalLayout(
                                            h5(strong(
                                                textOutput("RText_E.Box2")))
                                            ,
                                            h6(tableOutput("RTable_E.Box2")) 
                                        )
                                    )
                                )
                            )       
                        ),
                        #Histogramas-----------------------------------------
                        tabPanel("Histogramas",br(),
                            navlistPanel(widths = c(2, 10),
                                tabPanel("Histograma por Localidad",
                                    plotOutput("RPlot_E.His1")
                                    ,
                                    splitLayout( 
                                        uiOutput("RUi_checkbox.EHG1.Leg")
                                        ,
                                        br()
                                        ,
                                        #Boton de action.
                                        actionButton("goButtonE.His1",
                                                     "Submit Plot",
                                                     icon("paper-plane"),
                                                     style = "color: white; 
                                                     background: #35e51d")
                                    ),
                                    #Controles del grafico
                                    splitLayout( 
                                        uiOutput("RUi_checkboxGroup.EHG1.Zval")
                                        ,
                                        uiOutput("RUi_checkboxGroup.EHG1.Loc")
                                        ,
                                        br()
                                    )
                                ),
                                tabPanel("Histograma Var #1",
                                    plotOutput("RPlot_E.His2")
                                    ,
                                    splitLayout( 
                                        uiOutput("RUi_checkbox.EHG2.Leg")
                                        ,
                                        br()
                                        ,
                                        #Boton de action.
                                        actionButton("goButtonE.His2",
                                                      "Submit Plot",
                                                      icon("paper-plane"),
                                                      style = "color: white; 
                                                      background: #35e51d")
                                    ),
                                    splitLayout(cellWidths=c("45%","55%"),
                                        uiOutput("RUi_checkboxGroup.EHG2.Zval")
                                        ,
                                        verticalLayout(
                                            h5(strong(
                                                textOutput("RText_E.His2.1")))
                                            ,
                                            verbatimTextOutput("RPrnt_E.His2.2") 
                                        )
                                    )
                                )
                            )
                        ),
                        #Scatterplot-----------------------------------------
                        tabPanel("Gráfico de Dispersión",br(),
                            #navbarPage(title="Plots",
                            navlistPanel(widths = c(2, 10),
                                #Diagrama de Dispersion----------------------
                                tabPanel("Dispersión V1 vs V2",
                                     plotOutput("RPlot_Scat")
                                     ,
                                     splitLayout( 
                                         uiOutput("RUi_checkboxEtiqueta")
                                         ,
                                         uiOutput("RUi_checkboxJitter")
                                         ,
                                         actionButton("goButtonSca",
                                                      "Submit Plot",
                                                      icon("paper-plane"),
                                                      style = "color: white; 
                                                      background: #35e51d")
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
                                tabPanel("Agrupación K-Medias",
                                     plotOutput("RPlot_Kmeans")
                                     ,
                                     splitLayout( 
                                         h6(uiOutput("RUi_SliderK"))
                                         ,
                                         br()
                                         ,
                                         actionButton("goButtonKme",
                                                      "Submit Plot",
                                                      icon("paper-plane"),
                                                      style = "color: white; 
                                                      background: #35e51d")
                                     ),
                                     h4("Tabla de Frecuencias")
                                     ,
                                     h6(tableOutput("RTable_FrecK"))
                                ),	
                                #Heatmap-------------------------------------
                                tabPanel("Mapa de Calor",
                                     plotOutput("RPlot_HeatM")
                                     ,
                                     splitLayout( 
                                         br()
                                         ,
                                         br()
                                         ,
                                         actionButton("goButtonHea",
                                                      "Submit Plot",
                                                      icon("paper-plane"),
                                                      style = "color: white; 
                                                      background: #35e51d")
                                     )
                                ),
                                #Dispersion V1 vs Index----------------------
                                tabPanel("Dispersión V1 vs Idx"#,
                                
                                ),
                                #Dispersion V2 vs Index----------------------
                                tabPanel("Dispersión V2 vs Idx"#,
                                          
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
        
        #====================================================================
        #Panel: Clasificacion. ==============================================
        #====================================================================
        tabPanel("Clasificación",fluidPage(
            sidebarLayout(													
                sidebarPanel(width = 3,
                
                    #Evaluador principal ShowMe
                    #textOutput("ShowMe"),
                             
                    #Configuracion general...................................
                    h4(strong("Configuración General")),hr(),
                    checkboxInput("Box_Numerica2","Marque si desea clasificar
                                            una variable numérica.",value=F), 
                     

                    #Input variable explicada.-------------------------------
                    h5(uiOutput("RUi_Select_ZvarClas")),
                     
                    #Input zona de Estudio.----------------------------------
                    checkboxGroupInput("SelectList_ZonaClas",
                            "Zonas de estudio:",
                            choices  = as.character(
                                        unique(MatrizBiologica2$Localidad))
                            ,
                            selected = as.character(
                                        unique(MatrizBiologica2$Localidad))
                    ),
                    #Clasificadores independientes.--------------------------
                    hr()
                    ,
                    h4(textOutput("RText_varEnUso"))
                ), 
                
                #Panel de Resultados/////////////////////////////////////////
                mainPanel(
                    tabsetPanel(type="tabs",
                        #Tabla de datos--------------------------------------
                        tabPanel("Tabla de Datos",br(),
                                dataTableOutput("RDT_TablaDatosClas")
                        ),	
                        #Singular Value Descomposition-----------------------
                        tabPanel("Valores Singulares",br(),
                            navlistPanel(widths = c(2, 10),
                                #SVD Autovectores de Izquierda---------------
                                tabPanel("DVS Izquierda",
                                    plotOutput("RPlot_SVDizq"),
                                    splitLayout(
                                        h5(uiOutput("RUi_autoVec1"))
                                        ,
                                        h5(uiOutput("RUi_autoVec2"))
                                    ),
                                    splitLayout(cellWidths = 
                                                      c("50%","25%","25%"),
                                        uiOutput("RUi_checkboxGroupSVD.Zvar")
                                        ,
                                        uiOutput("RUi_checkboxLegendClas")
                                        ,
                                        actionButton("goButtonCla",
                                                     "Submit Plot",
                                                     icon("paper-plane"),
                                                     style = "color: white; 
                                                     background: #35e51d")
                                    )  

                                ),
                                #SVD Autovectores de Derecha-----------------
                                tabPanel("DVS Derecha",
                                    plotOutput("RPlot_SVDder",
                                                brush = brushOpts(id="Brush")),
                                    br(),
                                    splitLayout(cellWidths=c("54%","23%","23%")
                                        ,
                                        h5(strong("Top 10: Variables con mayor
                                                influencia"))
                                        ,
                                        checkboxInput("Box_InfPositiva",
                                                "Influencia Positiva",value=T)
                                        ,
                                        checkboxInput("Box_InfNegativa",
                                                "Influencia Negativa",value=F)
                                    ),
                                    splitLayout(
                                        h6(tableOutput("RTable_MaxCont1"))
                                        ,
                                        h6(tableOutput("RTable_MaxCont2"))
                                        ,
                                        h6(tableOutput("RTable_Brushed"))
                                    )
                                ),	
                                #Cluster Jerárquico--------------------------
                                tabPanel("Clúster Jerárquico",
                                    plotOutput("RPlot_HclustClas")
                                    ,
                                    splitLayout(cellWidths = 
                                                         c("50%","25%","25%"),
                                        uiOutput("RUi_checkboxGroupClasClus")
                                        ,
                                        uiOutput("RUi_LegendClasClus")
                                        ,
                                        actionButton("goButtonCla5",
                                                     "Submit Plot",
                                                     icon("paper-plane"),
                                                     style = "color: white; 
                                                     background: #35e51d")
                                    )       
                                ),
                                #Predictor vs Index--------------------------
                                tabPanel("Dispersión",
                                    plotOutput("RPlot_Dis1")
                                    ,
                                    splitLayout(cellWidths = 
                                                    c("50%","25%","25%"),
                                        h5(uiOutput("RUi_varDis1"))
                                        ,
                                        uiOutput("RUi_checkLegendClasDis1")
                                        ,
                                        actionButton("goButtonCla2",
                                                     "Submit Plot",
                                                     icon("paper-plane"),
                                                     style = "color: white; 
                                                     background: #35e51d")
                                    )
                                ),
                                #Predictor vs Index--------------------------
                                tabPanel("Dispersión",
                                    plotOutput("RPlot_Dis2")
                                    ,
                                    splitLayout(cellWidths = 
                                                     c("50%","25%","25%"),
                                        h5(uiOutput("RUi_varDis2"))
                                        ,
                                        uiOutput("RUi_checkLegendClasDis2")
                                        ,
                                        actionButton("goButtonCla3",
                                                     "Submit Plot",
                                                     icon("paper-plane"),
                                                     style = "color: white; 
                                                     background: #35e51d")
                                    )
                                ),
                                #Predictor vs Index--------------------------
                                tabPanel("Dispersión",
                                     plotOutput("RPlot_Dis3")
                                     ,
                                     splitLayout(cellWidths = 
                                                     c("50%","25%","25%"),
                                        h5(uiOutput("RUi_varDis3"))
                                        ,
                                        uiOutput("RUi_checkLegendClasDis3")
                                        ,
                                        actionButton("goButtonCla4",
                                                     "Submit Plot",
                                                     icon("paper-plane"),
                                                     style = "color: white; 
                                                     background: #35e51d")
                                    )
                                )
                            ) 
                        ),	
                        #Principal Component Analysis------------------------
                        tabPanel("Componentes Principales",br(),
                            navlistPanel(widths = c(2, 10),
                                tabPanel("Selector Cov-Cor",
                                    splitLayout(cellWidths = 
                                                    c("35%","20%","23%","22%"),
                                        h4(strong(textOutput("RText_CorCov")))
                                        ,
                                        uiOutput("RUi_CorACP")
                                        ,
                                        uiOutput("RUi_SpearACP")
                                    ),
                                    splitLayout(cellWidths = c("30%","70%"),
                                        uiOutput("RUi_checkboxGroupACP")
                                        ,       
                                        h6(tableOutput("RTable_CorCov"))
                                    )
                                ),
                                tabPanel("Matriz Correlación",
                                         plotOutput("RPlot_C.Matrix")      
                                ),
                                tabPanel("ACP",
                                    h5("Matriz de Autovectores:")
                                    ,     
                                    verbatimTextOutput("RPrnt_C.ACP2.1")
                                    ,
                                    h5("Resumen del Análisis de Componentes
                                       Principales:")
                                    ,
                                    verbatimTextOutput("RPrnt_C.ACP2.2")
                                    ,
                                    h5("Autovalores:")
                                    ,
                                    h6(tableOutput("RTable_C.ACP2.3"))
                                ),
                                tabPanel("Gráfico Sedimentos",
                                    plotOutput("RPlot_C.ACP3")      
                                ),
                                tabPanel("Biplot",
                                    plotOutput("RPlot_CCP.Biplot") 
                                    ,
                                    splitLayout(cellWidths = 
                                                c("30%","30%","5%","15%","20%"),
                                        uiOutput("RUi_Select_C.Comp1")
                                        ,
                                        uiOutput("RUi_Select_C.Comp2")
                                        ,
                                        br()
                                        ,
                                        uiOutput("RUi_checkbox.CCP.Biplot.Leg")
                                        ,
                                        actionButton("goButtonACPBiplot",
                                                     "Submit Plot",
                                                     icon("paper-plane"),
                                                     style = "color: white; 
                                                     background: #35e51d")
                                    )
                                ),
                                tabPanel("CP vs CP",
                                    plotOutput("RPlot_CCP.CvsC") 
                                    ,
                                    splitLayout(cellWidths = 
                                               c("30%","30%","5%","15%","20%"),
                                        uiOutput("RUi_Select_C.Comp21")
                                        ,
                                        uiOutput("RUi_Select_C.Comp22")
                                        ,
                                        br()
                                        ,
                                        uiOutput("RUi_checkbox.CCP.CvsC.Leg")
                                        ,
                                        actionButton("goButtonACPCvsC",
                                                     "Submit Plot",
                                                     icon("paper-plane"),
                                                     style = "color: white; 
                                                     background: #35e51d")
                                    ),
                                    uiOutput("RUi_checkboxGroup.CCP.CvxC")
                                )
                            )
                        ),	
                        #Boxplot---------------------------------------------
                        tabPanel("Random Forest",br()#,

                        )
                    )
                )
            )
        )),
        
        #====================================================================
        #Panel: Regresion. ==================================================
        #====================================================================
        tabPanel("Regresión","Regresion")
    )
))