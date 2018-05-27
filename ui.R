library(shiny)
library(shinythemes)
options(encoding = 'UTF-8')

#INTERFAZ DE USUARIO.///////////////////////////////////////////////////////////

#Archive ui.R.
shinyUI(fluidPage(
    titlePanel(em("Análisis Exploratorio (Datos de Prueba)")),
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
                                clasificar una variable numérica (Z).",value=F)
                    , 
                    #Input variable categorica Z.----------------------------
                    h5(uiOutput("RUi_Select_Zvar")),

                    #Input variable categorica W.----------------------------
                    h5(uiOutput("RUi_Select_Wvar")),
                    
                    #Input variable numerica Y.------------------------------
                    h5(uiOutput("RUi_Select_Yvar")),
                    
                    #Input objeto corrientes.-------
                    uiOutput("RUi_Select_CorrY"),
                    
                    #Input objeto distancia.--------
                    uiOutput("RUi_Select_DistY"),
                    
                    #Input variable numerica X.------------------------------
                    h5(uiOutput("RUi_Select_Xvar")),
                    
                    #Input objeto corrientes.-------
                    uiOutput("RUi_Select_CorrX"),
                    
                    #Input objeto distancia.--------
                    uiOutput("RUi_Select_DistX")
                     
                ), 
                 
                #Panel de Resultados/////////////////////////////////////////
                mainPanel(
                    tabsetPanel(id="panels",type="tabs",
                        #Tabla de datos--------------------------------------
                        tabPanel("Tabla de Datos",br(),
                                dataTableOutput("RDT_E.TablaD1")
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
                        #Tabla de Frecuencias--------------------------------
                        tabPanel("Cross Tables",br(),
                                 splitLayout( 
                                     h5(strong("Variable Categórica Z"))
                                     ,
                                     h5(strong("Variable Categórica W"))
                                 ),
                                 splitLayout( 
                                     h6(tableOutput("RTable_E.TablaD2"))
                                     ,
                                     h6(tableOutput("RTable_E.TablaD22"))
                                 ),
                                 h5(strong("Tabla Cruzada Z vs W"))
                                 ,
                                 tableOutput("RPrint_E.TablaC")
                        ),
                        #Boxplot---------------------------------------------
                        tabPanel("Boxplots",br(),
                            navlistPanel(widths = c(2, 10),
                                tabPanel("Boxplots",
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
                        tabPanel("Histograms",br(),
                            navlistPanel(widths = c(2, 10),
                                tabPanel("Histogramas",
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
                        tabPanel("Scatterplots",br(),
                            #navbarPage(title="Plots",
                            navlistPanel(widths = c(2, 10),
                                #Diagrama de Dispersion----------------------
                                tabPanel("Dispersión Vx vs Vy",
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
                                tabPanel("Dispersión Vy vs Idx",
                                    plotOutput("RPlot_EGD.V1vsIdx")
                                    ,
                                    splitLayout(
                                        br()
                                        ,
                                        uiOutput("RUi_EGD.V1vsIdx.Leg")
                                        ,
                                        actionButton("goButtonV1vsIdx",
                                                     "Submit Plot",
                                                     icon("paper-plane"),
                                                     style = "color: white; 
                                                     background: #35e51d")
                                    )
                                ),
                                #Dispersion V2 vs Index----------------------
                                tabPanel("Dispersión Vx vs Idx",
                                    plotOutput("RPlot_EGD.V2vsIdx")
                                    ,
                                    splitLayout(
                                        br()
                                        ,
                                        uiOutput("RUi_EGD.V2vsIdx.Leg")
                                        ,
                                        actionButton("goButtonV2vsIdx",
                                                     "Submit Plot",
                                                     icon("paper-plane"),
                                                     style = "color: white; 
                                                     background: #35e51d")
                                    )  
                                )
                            )  
                        ),
                        #Clusterizacion Jerarquica---------------------------
                        tabPanel("Dendrogram",br(),
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
                    #tableOutput('show_inputs'), 
                    
                    #Configuracion general...................................
                    h4(strong("Configuración General")),hr(),
                    checkboxInput("Box_Numerica2","Marque si desea clasificar
                                            una variable numérica.",value=F), 
                     

                    #Input variable explicada.-------------------------------
                    h5(uiOutput("RUi_Select_ZvarClas")),
                    
                    #Input porcentaje entrenamiento.-------------------------
                    h5(strong("Cantidad de datos al conjunto de entrenamiento"))
                    ,
                    splitLayout(cellWidths = c("45%","55%"),
                        numericInput("Select_C.Train","Porcentaje",
                                     min = 60,max = 100,value = 100,step = 10)
                        ,
                        numericInput("Select_C.Seed","Semilla Aleatoria",
                                     min = 1,max = Inf,value = 1234,step = 1)
                    ),
                    #Input variables clasificadoras cualitativas.------------
                    h5(uiOutput("RUi_Select_ClasCV")),
                    
                    #Clasificadores independientes.--------------------------
                    h4(textOutput("RText_varEnUso"))
                ), 
               
                #Panel de Resultados/////////////////////////////////////////
                mainPanel(
                    tabsetPanel(type="tabs",
                        #Tabla de datos--------------------------------------
                        tabPanel("Tabla de Datos",br(),
                                dataTableOutput("RDT_TablaDatosClas")
                        ),	
                        #Linear Discriminant Analysis------------------------
                        tabPanel("LDA",br(),
                            navlistPanel(widths = c(2, 10),
                                tabPanel("Selector Variables",
                                    splitLayout(cellWidths = 
                                                           c("60%","15%","25%"),
                                        h4(strong(
                                            "Linear Discriminant Analysis")) 
                                        ,
                                        br()
                                        ,
                                        actionButton("goButtonLDAejecutar",
                                                     "Ejecutar LDA",
                                                     icon("paper-plane"),
                                                     style = "color: white; 
                                                     background: #35e51d")
                                    ),
                                    h4(span(textOutput("RText_C.LDAadv"), 
                                         style="color:red"))
                                    ,
                                    splitLayout(cellWidths = c("30%","70%"),
                                        h4("Variables a usar")
                                        ,
                                        h4(textOutput("RText_C.EjecLDA"))
                                    ),
                                    splitLayout(cellWidths = c("30%","70%"),
                                        uiOutput("RUi_checkboxGroupLDA")
                                        ,
                                        tableOutput("RTable_C.LDA.var")
                                    )
                                ),
                                tabPanel("Matriz Correlación",
                                    h4(strong("Linear Discriminant Analysis")) 
                                    ,
                                    h4(span("Evite multicolinealidad",
                                            style="color:red"))
                                    ,
                                    plotOutput("RPlot_C.LDA.PP")
                                ),
                                tabPanel("Resumen",
                                    h4(strong("Linear Discriminant Analysis")) 
                                    ,
                                    verbatimTextOutput("RPrnt_C.LDA.Res")
                                ),
                                tabPanel("LDA Hist",
                                    splitLayout(cellWidths = 
                                                           c("60%","15%","25%"),
                                        h4(strong("Linear Discriminant Analysis")) 
                                        ,
                                        br()
                                        ,
                                        actionButton("goButtonLDAHist",
                                                     "Submit Plot",
                                                     icon("paper-plane"),
                                                     style = "color: white; 
                                                     background: #35e51d")
                                    ),
                                    uiOutput("RUi_Select_C.LDA.Disc")
                                    ,
                                    plotOutput("RPlot_C.LDA.Hist")      
                                ),
                                tabPanel("Biplot",
                                    h4(strong("Linear Discriminant Analysis")) 
                                    ,
                                    plotOutput("RPlot_C.LDA.Biplot") 
                                    ,
                                    splitLayout(
                                        br()
                                        ,
                                        uiOutput("RUi_checkbox.C.LDA.Biplot.Leg")
                                        ,
                                        actionButton("goButtonLDABiplot",
                                                     "Submit Plot",
                                                     icon("paper-plane"),
                                                     style = "color: white; 
                                                     background: #35e51d") 
                                    )
                                ),
                                tabPanel("Partición",
                                    splitLayout(cellWidths = 
                                                           c("60%","15%","25%"),
                                        h4(strong("Linear Discriminant Analysis")) 
                                        ,
                                        br()
                                        ,
                                        actionButton("goButtonLDAPart",
                                                     "Submit Plot",
                                                     icon("paper-plane"),
                                                     style = "color: white; 
                                                     background: #35e51d")
                                    ),
                                    plotOutput("RPlot_C.LDA.Part") 
                                ),
                                tabPanel("Evaluación",
                                    h4(strong("Linear Discriminant Analysis")) 
                                    ,
                                    h5("Evaluación sobre el Conjunto de 
                                      Entrenamiento")
                                    ,     
                                    verbatimTextOutput("RPrnt_C.LDAEval1")
                                    ,
                                    h5("Evaluación sobre el Conjunto de 
                                      Prueba")
                                    ,  
                                    verbatimTextOutput("RPrnt_C.LDAEval2")
                                )
                            )
                        ),	
                        #Regularized Discriminant Analysis--------------------
                        tabPanel("RDA",br(),
                            navlistPanel(widths = c(2, 10),
                                tabPanel("Selector Variables",
                                    splitLayout(cellWidths = 
                                                    c("60%","15%","25%"),
                                        h4(strong(
                                           "Regularized Discriminant Analysis")) 
                                        ,
                                        br()
                                        ,
                                        actionButton("goButtonRDAejecutar",
                                                     "Ejecutar RDA",
                                                      icon("paper-plane"),
                                                      style = "color: white; 
                                                      background: #35e51d")
                                    ),
                                    h4(span(textOutput("RText_C.RDAadv"), 
                                        style="color:red"))
                                    ,
                                    splitLayout(cellWidths = 
                                                    c("30%","35%","35%"),
                                        br()
                                        ,
                                        uiOutput("RUi_Select_RDAGamma")
                                        ,
                                        uiOutput("RUi_Select_RDALambda")
                                    ),
                                    splitLayout(cellWidths = c("30%","70%"),
                                        h4("Variables a usar")
                                        ,
                                        h4(textOutput("RText_C.EjecRDA"))
                                    ),
                                    splitLayout(cellWidths = c("30%","70%"),
                                        uiOutput("RUi_checkboxGroupRDA")
                                        ,
                                        tableOutput("RTable_C.RDA.var")
                                    )
                                ),
                                tabPanel("Matriz Correlación",
                                    h4(strong(
                                        "Regularized Discriminant Analysis")) 
                                    ,
                                    h4(span("Evite multicolinealidad",
                                            style="color:red"))
                                    ,
                                    plotOutput("RPlot_C.RDA.PP")
                                ),
                                tabPanel("Resumen",
                                    h4(strong(
                                        "Regularized Discriminant Analysis")) 
                                    ,
                                    verbatimTextOutput("RPrnt_C.RDA.Res")
                                ),
                                tabPanel("Partición",
                                    splitLayout(cellWidths = 
                                                   c("60%","15%","25%"),
                                        h4(strong
                                           ("Regularized Discriminant Analysis")) 
                                        ,
                                        br()
                                        ,
                                        actionButton("goButtonRDAPart",
                                                     "Submit Plot",
                                                     icon("paper-plane"),
                                                     style = "color: white; 
                                                     background: #35e51d")
                                    ),
                                    plotOutput("RPlot_C.RDA.Part") 
                                ),
                                tabPanel("Evaluación",
                                    h4(strong(
                                        "Regularized Discriminant Analysis")) 
                                     ,
                                    h5("Evaluación sobre el Conjunto de 
                                        Entrenamiento")
                                    ,     
                                    verbatimTextOutput("RPrnt_C.RDAEval1")
                                    ,
                                    h5("Evaluación sobre el Conjunto de Prueba")
                                    ,  
                                    verbatimTextOutput("RPrnt_C.RDAEval2")
                                )
                            )
                        ),	
                        
                        #Singular Value Descomposition-----------------------
                        tabPanel("SVD",br(),
                            navlistPanel(widths = c(2, 10),
                                #SVD Autovectores de Izquierda---------------
                                tabPanel("Autovector Izquierdo",
                                    h4(strong("Singular Value Descomposition"))     
                                    ,
                                    plotOutput("RPlot_SVDizq"),
                                    splitLayout(cellWidths = 
                                                c("30%","30%","5%","15%","20%"),
                                        h5(uiOutput("RUi_autoVec1"))
                                        ,
                                        h5(uiOutput("RUi_autoVec2"))
                                        ,
                                        br()
                                        ,
                                        uiOutput("RUi_checkboxLegendClas")
                                        ,
                                        actionButton("goButtonCla",
                                                     "Submit Plot",
                                                     icon("paper-plane"),
                                                     style = "color: white; 
                                                     background: #35e51d")
                                        
                                    ),
                                    splitLayout(cellWidths = 
                                                      c("50%","25%","25%"),
                                        uiOutput("RUi_checkboxGroupSVD.Zvar")
                                        ,
                                        br()
                                        ,
                                        br()
                                    )  
                                ),
                                #SVD Autovectores de Derecha-----------------
                                tabPanel("Autovector Derecho",
                                    h4(strong("Singular Value Descomposition"))     
                                    ,
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
                                    h4(strong("Singular Value Descomposition"))     
                                    ,
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
                        tabPanel("PCA",br(),
                            navlistPanel(widths = c(2, 10),
                                tabPanel("Selector Variables",
                                    splitLayout(cellWidths = 
                                                    c("60%","15%","25%"),
                                        h4(strong("Principal Component Analysis")) 
                                        ,
                                        br()
                                        ,
                                        actionButton("goButtonACPejecutar",
                                                    "Ejecutar PCA",
                                                     icon("paper-plane"),
                                                     style = "color: white; 
                                                     background: #35e51d")
                                    ),
                                    h4(span(textOutput("RText_C.EjecACP"), 
                                            style="color:red"))
                                    ,
                                    splitLayout(cellWidths = 
                                                        c("40%","20%","40%"),
                                        h4("Variables a usar")
                                        ,
                                        h4("Medida a usar:")
                                        ,
                                        h4(textOutput("RText_CorCov"))
                                    ),
                                    splitLayout(cellWidths = c("30%","70%"),
                                        uiOutput("RUi_checkboxGroupACP")
                                        ,       
                                        verticalLayout(
                                            splitLayout(cellWidths = 
                                                    c("15%","35%","35%","15%"),
                                                br()
                                                ,
                                                uiOutput("RUi_CorACP")
                                                ,
                                                uiOutput("RUi_SpearACP")
                                                ,
                                                br()
                                            ),
                                            h6(tableOutput("RTable_CorCov"))
                                        )
                                    )
                                ),
                                tabPanel("Matriz Correlación",
                                    h4(strong("Principal Component Analysis")) 
                                    ,
                                    plotOutput("RPlot_C.Matrix")
                                ),
                                tabPanel("Resumen",
                                    h4(strong("Principal Component Analysis")) 
                                    ,
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
                                    h4(strong("Principal Component Analysis")) 
                                    ,
                                    plotOutput("RPlot_C.ACP3")      
                                ),
                                tabPanel("Biplot",
                                    h4(strong("Principal Component Analysis")) 
                                    ,
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
                                    h4(strong("Principal Component Analysis")) 
                                    ,
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
                                ),
                                tabPanel("Evaluación",
                                    h4(strong("PCA and Multinomial Logistic 
                                              Regression Model"))
                                    ,
                                    uiOutput("RUi_Select_C.Evalk")
                                    ,
                                    h5("Evaluación sobre el Conjunto de 
                                       Entrenamiento")
                                    ,     
                                    #h6(tableOutput("RPrnt_C.ACPEval1"))
                                    verbatimTextOutput("RPrnt_C.ACPEval1")
                                    ,
                                    h5("Evaluación sobre el Conjunto de 
                                       Prueba")
                                    ,  
                                    verbatimTextOutput("RPrnt_C.ACPEval2")
                                )
                            )
                        ),	
                        #PCoA------------------------------------------------
                        tabPanel("MDS",br(),
                            navlistPanel(widths = c(2, 10),
                                tabPanel("Selector Variables",
                                    splitLayout(cellWidths = 
                                                    c("60%","15%","25%"),
                                        h4(strong("Metric Multidimensional 
                                                  Scaling")) 
                                        ,
                                        br()
                                        ,
                                        actionButton("goButtonACoPejecutar",
                                                     "Ejecutar MDS",
                                                     icon("paper-plane"),
                                                     style = "color: white; 
                                                     background: #35e51d")
                                    ),
                                    h4(span(textOutput("RText_C.EjecMDS"), 
                                            style="color:red"))
                                    ,
                                    splitLayout(cellWidths = 
                                                        c("30%","35%","35%"),
                                        h4("Variables a usar")
                                        ,
                                        h4("Distancia-disimilitud")
                                        ,
                                        h4("Número de dimensiones")
                                    ),
                                    fluidRow(
                                        column(4,uiOutput("RUi_checkboxGroupACoP")), 
                                        column(4,uiOutput("RUi_Select_c.Distancia")),
                                        column(4,uiOutput("RUi_Select_PCoADim"))
                                    )
                                    # splitLayout(cellWidths = 
                                    #                 c("30%","35%","35%"),
                                    #     uiOutput("RUi_checkboxGroupACoP") 
                                    #     ,
                                    #     uiOutput("RUi_Select_c.Distancia")
                                    #     ,
                                    #     uiOutput("RUi_Select_PCoADim")
                                    # )
                                ),
                                tabPanel("Resumen",
                                    h4(strong("Metric Multidimensional Scaling")) 
                                    ,
                                    h5("Matriz de Autovectores:")
                                    ,
                                    verbatimTextOutput("RPrnt_C.TablaACoP")
                                    ,
                                    h5("Resumen del Análisis:")
                                    ,
                                    h6(tableOutput("RTable_C.ACoPRes2"))
                                    ,
                                    h5("Autovalores:")
                                    ,
                                    h6(tableOutput("RTable_C.ACoPRes3"))
                                ),
                                tabPanel("Gráfico Sedimentos",
                                    h4(strong("Metric Multidimensional Scaling"))
                                    ,
                                    plotOutput("RPlot_C.ACoPSed")      
                                ),
                                tabPanel("CP vs CP",
                                    h4(strong("Metric Multidimensional Scaling")) 
                                    ,
                                    plotOutput("RPlot_C.PCoA") 
                                    ,
                                    splitLayout(cellWidths = 
                                                c("30%","30%","5%","15%","20%"),
                                        uiOutput("RUi_Select_C.PCoAComp1")
                                        ,
                                        uiOutput("RUi_Select_C.PCoAComp2")
                                        ,
                                        br()
                                        ,
                                        uiOutput("RUi_checkbox.C.ACoP.Leg")
                                        ,
                                        actionButton("goButtonACoP",
                                                     "Submit Plot",
                                                     icon("paper-plane"),
                                                     style = "color: white; 
                                                     background: #35e51d")
                                    ),
                                    uiOutput("RUi_checkboxGroup.C.ACoP")
                                ),
                                tabPanel("Evaluación",
                                    h4(strong("MDS and Multinomial Logistic 
                                                 Regression Model"))
                                    ,
                                    uiOutput("RUi_Select_C.EvalMDS")
                                    ,
                                    h5("Evaluación sobre el Conjunto de 
                                            Entrenamiento")
                                    ,     
                                    verbatimTextOutput("RPrnt_C.ACoPEval1")
                                    ,
                                    h5("Evaluación sobre el Conjunto de 
                                            Prueba")
                                    ,  
                                    verbatimTextOutput("RPrnt_C.ACoPEval2")
                                )
                            )               
                        ),
                        #Boxplot---------------------------------------------
                        tabPanel("NMDS",br(),
                            navlistPanel(widths = c(2, 10),
                                tabPanel("Selector Variables",
                                    splitLayout(cellWidths = 
                                                    c("60%","15%","25%"),
                                        h4(strong("Non-Metric Multidimensional 
                                                  Scaling")) 
                                        ,
                                        br()
                                        ,
                                        actionButton("goButtonNMDSejecutar",
                                                     "Ejecutar NMDS",
                                                     icon("paper-plane"),
                                                     style = "color: white; 
                                                     background: #35e51d")
                                    ),
                                    h5(strong(span("Utilice el gráfico Stress Vs 
                                        Dimensions para elegir el número 
                                        apropiado de dimensiones"
                                            ,style="color:#2CBC0C")))
                                    ,
                                    h4(span(textOutput("RText_C.EjecNMDS"), 
                                            style="color:red"))
                                    ,
                                    splitLayout(cellWidths = 
                                                        c("30%","35%","35%"),
                                        h4("Variables a usar")
                                        ,
                                        h4("Índice de disimilitud")
                                        ,
                                        h4("Número de dimensiones")
                                    ),
                                    fluidRow(
                                        column(4,uiOutput("RUi_checkboxGroupNMDS")), 
                                        column(4,uiOutput("RUi_Select_NMDS.Distancia")),
                                        column(4,uiOutput("RUi_Select_NMDSDim"))
                                    )
                                    # splitLayout(cellWidths = 
                                    #                 c("30%","35%","35%"),
                                    #     uiOutput("RUi_checkboxGroupNMDS") 
                                    #     ,
                                    #     uiOutput("RUi_Select_NMDS.Distancia")
                                    #     ,
                                    #     uiOutput("RUi_Select_NMDSDim")
                                    # )
                                ),
                                tabPanel("Stress vs Dimensions",
                                    splitLayout(cellWidths = 
                                                    c("60%","15%","25%"),
                                        h4(strong("Non-Metric Multidimensional 
                                                  Scaling")) 
                                        ,
                                        br()
                                        ,
                                        actionButton("goButtonNMDSstress",
                                                     "Submit Plot",
                                                     icon("paper-plane"),
                                                     style = "color: white; 
                                                     background: #fab144")
                                    ),
                                    h5(strong(span("Gráfico para elegir el
                                        número óptimo de dimensiones a utilizar"
                                            ,style="color:#2CBC0C")))
                                    ,
                                    h5(strong(span("Ejecutar este procedimiento 
                                        puede llevar varios minutos"
                                            ,style="color:#EA8710")))
                                    ,
                                    splitLayout(
                                        h4("Número máximo de dimensiones")
                                        ,
                                        h4("Cantidad de inicios aleatorios")
                                    ),
                                    splitLayout(
                                        uiOutput("RUi_Select_NMDSMaxDim")
                                        ,
                                        uiOutput("RUi_Select_NMDSMaxTry")
                                    ),
                                    plotOutput("RPlot_C.NMDSstress") 
                                ),
                                tabPanel("Resumen",
                                    h4(strong("Non-Metric Multidimensional Scaling")) 
                                    ,
                                    h5("Resumen del Procedimiento:")
                                    ,
                                    verbatimTextOutput("RPrnt_C.TablaNMDS")
                                ),
                                tabPanel("Shepard",
                                    splitLayout(cellWidths = 
                                                    c("60%","15%","25%"),
                                        h4(strong("Non-Metric Multidimensional 
                                                  Scaling")) 
                                        ,
                                        br()
                                        ,
                                        actionButton("goButtonNMDSShep",
                                                     "Submit Plot",
                                                     icon("paper-plane"),
                                                     style = "color: white; 
                                                     background: #35e51d")
                                    ),
                                    plotOutput("RPlot_C.NMDSShep")      
                                ),
                                tabPanel("CP vs CP",
                                    h4(strong("Non-Metric Multidimensional Scaling")) 
                                    ,
                                    plotOutput("RPlot_C.NMDS") 
                                    ,
                                    splitLayout(cellWidths = 
                                                c("30%","30%","5%","15%","20%"),
                                        uiOutput("RUi_Select_C.NMDSComp1")
                                        ,
                                        uiOutput("RUi_Select_C.NMDSComp2")
                                        ,
                                        br()
                                        ,
                                        uiOutput("RUi_checkbox.C.NMDS.Leg")
                                        ,
                                        actionButton("goButtonNMDS",
                                                     "Submit Plot",
                                                     icon("paper-plane"),
                                                     style = "color: white; 
                                                     background: #35e51d")
                                    ),
                                    uiOutput("RUi_checkboxGroup.C.NMDS")
                                ),
                                tabPanel("Goodness of Fit",
                                    h4(strong("Non-Metric Multidimensional Scaling")) 
                                    ,
                                    plotOutput("RPlot_C.NMDSGood") 
                                    ,
                                    splitLayout(cellWidths = 
                                                c("30%","30%","5%","15%","20%"),
                                        uiOutput("RUi_Select_C.NMDSComp12")
                                        ,
                                        uiOutput("RUi_Select_C.NMDSComp22")
                                        ,
                                        br()
                                        ,
                                        uiOutput("RUi_checkbox.C.NMDS.Leg2")
                                        ,
                                        actionButton("goButtonNMDSGood",
                                                     "Submit Plot",
                                                     icon("paper-plane"),
                                                     style = "color: white; 
                                                     background: #35e51d")
                                    ),
                                    splitLayout(
                                        uiOutput("RUi_checkboxGroup.C.NMDS2")
                                        ,
                                        uiOutput("RUi_checkboxGroup.C.NMDSGood")
                                    )
                                ),
                                tabPanel("Biplot",
                                    h4(strong("Non-Metric Multidimensional Scaling")) 
                                    ,
                                    plotOutput("RPlot_C.NMDSBip") 
                                    ,
                                    splitLayout(cellWidths = 
                                                c("30%","30%","5%","15%","20%"),
                                        uiOutput("RUi_Select_C.NMDSComp123")
                                        ,
                                        uiOutput("RUi_Select_C.NMDSComp223")
                                        ,
                                        br()
                                        ,
                                        uiOutput("RUi_checkbox.C.NMDS.Etiq")
                                        ,
                                        actionButton("goButtonNMDSBip",
                                                     "Submit Plot",
                                                     icon("paper-plane"),
                                                     style = "color: white; 
                                                     background: #35e51d")
                                    )
                                ),
                                tabPanel("Procrustes",
                                    splitLayout(cellWidths = 
                                                    c("60%","15%","25%"),
                                        h4(strong("Non-Metric Multidimensional 
                                                  Scaling")) 
                                        ,
                                        br()
                                        ,
                                        actionButton("goButtonNMDSmed",
                                                     "Submit Plot",
                                                     icon("paper-plane"),
                                                     style = "color: white; 
                                                     background: #35e51d")
                                    ),
                                    uiOutput("RUi_Select_NMDS.Distmed")
                                    ,
                                    splitLayout(
                                        plotOutput("RPlot_C.NMDSmed1") 
                                        ,
                                        plotOutput("RPlot_C.NMDSmed2") 
                                    )
                                ),
                                tabPanel("Evaluación"#,
                                    # h4(strong("MDS and Multinomial Logistic 
                                    #              Regression Model"))
                                    # ,
                                    # uiOutput("RUi_Select_C.EvalMDS")
                                    # ,
                                    # h5("Evaluación sobre el Conjunto de 
                                    #         Entrenamiento")
                                    # ,     
                                    # verbatimTextOutput("RPrnt_C.ACoPEval1")
                                    # ,
                                    # h5("Evaluación sobre el Conjunto de 
                                    #         Prueba")
                                    # ,  
                                    # verbatimTextOutput("RPrnt_C.ACoPEval2")
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
        ,
        
        #====================================================================
        #Panel: Ayuda. ======================================================
        #====================================================================
        tabPanel("Ayuda",br(),
                 h4(em("Aplicación desarrollada como parte de un proyecto de
                       tesis de maestría."))
                 ,
                 br()
                 ,
                 h4(em("Autor: Lic. Alejandro José Bravo Jiménez"))
                 ,
                 h4(em("Departamento de Cómputo Científico y Estadística"))
                 ,
                 h4(em("Universidad Simón Bolívar"))
                 ,
                 h4(em("Sartenejas-Venezuela. 2018"))
                 ,
                 h4(em("Contacto: alebj88@gmail.com"))
        )
    )
))