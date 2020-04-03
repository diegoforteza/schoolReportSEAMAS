#'@title resultadosDeseaMasUI
#'@description resultadosDeseaMasUI
#'@export
resultadosDeseaMasUI <- function(id){
  ns <- NS(id)
  
  tabsetPanel(
    tabPanel(i18n$t("Resultados"),
             fluidRow(
               column(width = 12,
                      br(), 
                      shiny::p(i18n$t("Los siguientes gráficos muestran los niveles de desempeño de los alumnos de su centro que aplicaron SEA+.")), 
                      shiny::p(i18n$t("Esta visualización permite comparar los resultados del centro educativo con resultados a nivel de inspección o nacionales.")),
                      shiny::p(i18n$t("Es posible, también aplicar diferentes filtros que permiten realizar otras comparaciones: por quintil a nivel nacional o a nivel inspección."))
               ),
               box(width = 4, title = i18n$t("Primer Gráfico"), 
                   fluidRow(
                     column(width = 6,
                            selectInput(ns("tipo1"), i18n$t("Seleccionar:"),
                                        choices = c("Centro", "Inspección", "Nacional", 
                                                    "Inspección Quintil", "Nacional Quintil"),
                                        selected = "Centro")
                     ),
                     column(width = 6,
                            selectInput(ns("anio1"), i18n$t("Año de evaluación:"),
                                        choices = aniosAplicaciones, 
                                        selected = max(aniosAplicaciones))
                     )
                   )
               ),
               box(width = 4, title = i18n$t("Segundo Gráfico"), 
                   fluidRow(
                     column(width = 6,
                            selectInput(ns("tipo2"), i18n$t("Seleccionar:"),
                                        choices = c("Centro", "Inspección", "Nacional", 
                                                    "Inspección Quintil", "Nacional Quintil"),
                                        selected = "Inspección")
                     ),
                     column(width = 6,
                            selectInput(ns("anio2"), i18n$t("Año de evaluación:"),
                                        choices = aniosAplicaciones,
                                        selected = max(aniosAplicaciones))
                     )
                   )
               ),
               
               box(width = 4, title = i18n$t("Tercer Gráfico"), 
                   fluidRow(
                     column(width = 6,
                            selectInput(ns("tipo3"), i18n$t("Seleccionar:"),
                                        choices = c("Centro", "Inspección", "Nacional", 
                                                    "Inspección Quintil", "Nacional Quintil"),
                                        selected = "Nacional")
                     ),
                     column(width = 6,
                            selectInput(ns("anio3"), i18n$t("Año de evaluación:"),
                                        choices = aniosAplicaciones,
                                        selected = max(aniosAplicaciones))
                     )
                   )
               ),
               
               column(width = 12, verbatimTextOutput(ns("ksTest"))),
               column(width = 12,
                      plotOutput(ns("graficoBarras"), height = "500px")
               )
             )
    ),
    
    tabPanel(i18n$t("Transiciones de nivel"),
             fluidRow(
               column(width = 4, br(), 
                      selectInput(ns("anioAplicacion"), i18n$t("Seleccionar año de evaluación:"),
                                  choices = aniosAplicaciones,
                                  multiple=FALSE, selectize=FALSE)
               ),
               column(width = 8, br(),
                      i18n$t("En la tabla a continuación se presentan en las columnas los niveles de desempeño obtenidos en el año seleccionado y en las filas los niveles obtenidos en el año previo al seleccionado."),
                      i18n$t("Se indica con color la cantidad de alumnos que mantienen el mismo nivel de desempeño en los dos años de aplicación."),  
                      i18n$t("Los alumnos que se encuentran por debajo de la franja coloreada, son aquellos que bajaron de nivel, mientras que los alumnos que se encuentran sobre la franja son aquellos que subieron de nivel."),
                      br(),
                      i18n$t("En el gráfico se muestra la transición entre los niveles de desempeño que refleja los movimientos entre niveles que se muestran en la tabla.")
               )
             ),
             fluidRow(
               br(),
               br(),
               column(width = 6, plotOutput(ns("tablaTransiciones"))),
               
               column(width = 6, plotOutput(ns("graficoTransiciones")))
               
             )
    )
  )
}

#'@title resultadosDeseaMas
#'@description resultadosDeseaMas
#'@export
resultadosDeseaMas = function(input, output, session, areaCodigo){
  ns <- session$ns
  
  ###  tabPanel1
  output$graficoBarras <- renderPlot({ 
    tryCatch({ plotNivelesTAIxGrado(input, areaCodigo = areaCodigo) }, 
             error=function(e){ plotHandler("Error") } )
  });
  
  output$ksTest <- renderText( ksTestAplicacionAlumnoTheta(input, areaCodigo = areaCodigo) )
  
  ###  tabPanel2
  wideData <- reactive({   
    
    anios <- c((as.numeric(input$anioAplicacion)-1), input$anioAplicacion)
    anios <- intersect(anios, aniosAplicaciones)
    
    myLevels <- c("Nivel Bajo 1", "Nivel 1", "Nivel 2", 
                  "Nivel 3", "Nivel 4", "Nivel 5",
                  "Nivel 6", "Nivel 7");
    if(areaCodigo == 3){
      myLevels <- c("Nivel Bajo 1", "Nivel 1", "Nivel 2", 
                    "Nivel 3", "Nivel 4", "Nivel 5", "Nivel 6");
    }
    
    myFilteredData <- myData %>%
      filter(AplicacionAnio %in% anios) %>%
      filter(AreaCodigo %in% areaCodigo) %>%
      filter(!duplicated(paste(AlumnoCodigo, AplicacionAnio, AreaCodigo)))
    
    myFilteredData$Nivel <- factor(myFilteredData$Nivel, levels=myLevels, labels=myLevels)
    
    
    
    myFilteredData %>% filter(CentroRUEE == centroRUEE) %>%
      select(AlumnoCodigo, AplicacionAnio,
             AplicacionGrupoCodigo, CentroRUEE, GradoCodigo) %>% 
      pivot_wider(names_from = c(AplicacionAnio), 
                  values_from = c(GradoCodigo, AplicacionGrupoCodigo, CentroRUEE)) %>%
      left_join(myFilteredData %>%
                  select(AlumnoCodigo, AplicacionAnio, AreaCodigo, 
                         AplicacionAlumnoTheta, Nivel, DuracionPrueba) %>% 
                  pivot_wider(names_from = c(AplicacionAnio, AreaCodigo), 
                              values_from = c(Nivel, AplicacionAlumnoTheta, DuracionPrueba)), 
                by="AlumnoCodigo") %>% as.data.frame()
    
  })
  
  output$tablaTransiciones <- renderPlot({
    anios <- c((as.numeric(input$anioAplicacion)-1), input$anioAplicacion)
    anios <- intersect(anios, aniosAplicaciones)
    myVars <- paste("Nivel", anios, areaCodigo, sep="_")
    
    if(length(myVars) == 2){
      plotTable(table(wideData()[, myVars[1]], wideData()[, myVars[2]]),
                myTitle=paste0("Niveles \n", paste(anios, collapse="/")))
    } else {
      plotHandler("No hay información suficiente para realizar este análisis.")
    }
  })
  
  output$graficoTransiciones <- renderPlot({
    alluvialPlot( wideData(), cols=levels(myData$color) )
  });
  
  
}
