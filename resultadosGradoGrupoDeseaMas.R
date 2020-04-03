#' @title resultadosGradoGrupoDeseaMasUI
#' @description resultadosGradoGrupoDeseaMasUI
#' @note Asume que alumnoNombre (TRUE/FALSE) exisite en el ambiente global
#' @import shiny
#' @importFrom DT DTOutput
#' @export
resultadosGradoGrupoDeseaMasUI <- function(id){
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(width = 4,
             selectInput(ns("grupoCodigo_gradoGrupo"), 
                         paste0(i18n$t("Seleccionar grupo correspondiente al año "), max(aniosAplicaciones), ":"),
                         choices = gruposChoices[order(names(gruposChoices))],
                         multiple = TRUE)
      )
    ),
    tabsetPanel(
      tabPanel(i18n$t("Resultados"),
               fluidRow(
                 column(width = 12,
                        br(),
                        shiny::p(i18n$t("Los siguientes gráficos muestran los niveles de desempeño de los alumnos de su centro que aplicarion SEA+.")), 
                        shiny::p(i18n$t("Si selecciona varios grupos, el gráfico mostrará el total del alumnado. De esta manera se puede ver la distribución de niveles de desempeño de todos los 3ros, por ejemplo, o de un ciclo si se seleccionan todos los 3ros y todos los 4tos.")),
                        shiny::p(i18n$t("Esta visualización permite comparar los resultados del centro educativo con resultados a nivel de inspección o nacionales.")), 
                        shiny::p(i18n$t("Es posible realizar comparaciones, también, con los resultados del año anterior, ya que debajo aparecen los gráficos con esos datos."))
                 ),
                 box(width = 4, title = i18n$t("Primer Gráfico"),
                     p(i18n$t("En esta visualizacion el primer grafico hace referencia a los resultados del centro para el grado o grupo seleccionado."))
                 ),
                 box(width = 4, title = i18n$t("Segundo Gráfico"), 
                     fluidRow(
                       column(width = 6,
                              selectInput(ns("gradoGrupo_t2"), i18n$t("Seleccionar:"),
                                          choices = c("Centro", "Inspección", "Nacional", 
                                                      "Inspección Quintil", "Nacional Quintil"),
                                          selected = "Inspección")
                       )
                     )
                 ),
                 box(width = 4, title = i18n$t("Tercer Gráfico"), 
                     fluidRow(
                       column(width = 6,
                              selectInput(ns("gradoGrupo_t3"), i18n$t("Seleccionar:"),
                                          choices = c("Centro", "Inspección", "Nacional", 
                                                      "Inspección Quintil", "Nacional Quintil"),
                                          selected = "Nacional")
                       )
                     )
                 ),
                 
                 column(width = 12,
                        plotOutput(ns("graficoBarrasGradoGrupo"), height = "500px")
                 )
               )
      ),
      
      tabPanel(i18n$t("Transiciones de nivel"),
               br(),               
               shiny::p(i18n$t("El gráfico presenta la transición de nivel y puntaje de cada alumno en todas sus aplicaciones de SEA+ del área identificando el grado que cursó en cada año.")),
               br(),
               plotlyOutput(ns("graficoTrayectoriasGradoGrupo_plotly")),
               br(),
               shiny::p(i18n$t("En el gráfico de las transiciones entre niveles aparecen unicamente los alumnos que realizaron todas las aplicaciones.")),
               br(),
               plotOutput(ns("graficoTransicionesGradoGrupo"))
               
      ),
      
      tabPanel(i18n$t("Tabla de puntajes"),
               fluidRow(
                 div(style="overflow-x: scroll;",
                     DT::DTOutput(ns("tablaAlumnos")))
               )        
      )
      
    )
  )
}

#' @title resultadosGradoGrupoDeseaMas
#' @description resultadosGradoGrupoDeseaMas
#' @import shiny dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom DT renderDT datatable
#' @export
resultadosGradoGrupoDeseaMas = function(input, output, session, areaCodigo){
  # ns <- session$ns
  
  ###  reactive elements transform data
  wideData <- reactive({
    
    myLevels <- c("Nivel Bajo 1", "Nivel 1", "Nivel 2",
                  "Nivel 3", "Nivel 4", "Nivel 5",
                  "Nivel 6", "Nivel 7");
    if(areaCodigo == 3){
      myLevels <- c("Nivel Bajo 1", "Nivel 1", "Nivel 2",
                    "Nivel 3", "Nivel 4", "Nivel 5", "Nivel 6");
    }
    
    ## filtro por grupo codigo
    if(!is.null(input$grupoCodigo_gradoGrupo)){
      myIds <- estructuraCentro$AlumnoCodigo[which(estructuraCentro$GrupoCodigo %in% 
                                                     input$grupoCodigo_gradoGrupo)];
    } else {
      if(!is.null(input$gradoCodigo_gradoGrupo)){
        myIds <- estructuraCentro$AlumnoCodigo[which(estructuraCentro$GradoCodigo %in% 
                                                       input$gradoCodigo_gradoGrupo)];
      }
    }
    
    myFilteredData <- myData %>%
      filter(AlumnoCodigo %in% myIds) %>%
      filter(AreaCodigo %in% areaCodigo) %>%
      filter(!duplicated(paste(AlumnoCodigo, AplicacionAnio, AreaCodigo))) %>%
      mutate(Puntaje=round(AplicacionAlumnoTheta*100+1000),
             DuracionPrueba=round(as.numeric(DuracionPrueba), 2)) %>%
      arrange(AplicacionAnio)
    
    auxColNames <- paste0("GradoCodigo_", sort(unique(myFilteredData$AplicacionAnio)))
    
    aux <- myFilteredData %>% filter(CentroRUEE == centroRUEE) %>%
      select(AlumnoCodigo, AplicacionAnio,
             AplicacionGrupoCodigo, CentroRUEE, GradoCodigo) %>%
      pivot_wider(names_from = c(AplicacionAnio),
                  values_from = c(GradoCodigo, AplicacionGrupoCodigo, CentroRUEE)) %>%
      select(AlumnoCodigo, auxColNames) %>% #, starts_with("Grado")) %>%
      left_join(myFilteredData %>%
                  select(AlumnoCodigo, AplicacionAnio, AreaCodigo,
                         Puntaje, Nivel, DuracionPrueba) %>%
                  pivot_wider(names_from = c(AplicacionAnio, AreaCodigo),
                              values_from = c(Nivel, Puntaje, DuracionPrueba)),
                by="AlumnoCodigo") %>% as.data.frame();
    
    if(alumnoNombre){
      bux <- alumno[, c("AlumnoCodigo", "Nombre")];
    } else {
      bux <- data.frame("AlumnoCodigo"=aux$AlumnoCodigo, 
                        "Nombre"=aux$AlumnoCodigo);
    }
    aux <- merge(bux, aux, all.y=TRUE);
    aux$AlumnoCodigo <- NULL;
    aux;
    
  })
  
  longData <- reactive({
    ## filtro por grupo codigo
    if(!is.null(input$grupoCodigo_gradoGrupo)){
      myIds <- estructuraCentro$AlumnoCodigo[which(estructuraCentro$GrupoCodigo %in% 
                                                     input$grupoCodigo_gradoGrupo)];
    } else {
      if(!is.null(input$gradoCodigo_gradoGrupo)){
        myIds <- estructuraCentro$AlumnoCodigo[which(estructuraCentro$GradoCodigo %in% 
                                                       input$gradoCodigo_gradoGrupo)];
      }
    }

    if(alumnoNombre){
      bux <- alumno[, c("AlumnoCodigo", "Nombre")];
    } else {
      bux <- data.frame("AlumnoCodigo"=myIds, 
                        "Nombre"=myIds);
    }
    
    merge(bux, 
          myData %>%
            filter(AlumnoCodigo %in% myIds) %>%
            filter(AreaCodigo %in% areaCodigo) %>%
            filter(!duplicated(paste(AlumnoCodigo, AplicacionAnio, AreaCodigo))) %>%
            mutate(Grado=factor(as.character(GradoCodigo)),
                   Puntaje=round(AplicacionAlumnoTheta * 100 + 1000)), all.y=TRUE)
    
  })
  
  ###  tabPanel1
  output$graficoBarrasGradoGrupo <- renderPlot({ 
    tryCatch({ plotNivelesTAIxGradoGrupo(input, areaCodigo = areaCodigo) }, 
             error=function(e){ 
               plotHandler("Debe seleccionar un grupo para visualizar los resultados") } )
    });
  
  ###  tabPanel2
  output$graficoTransicionesGradoGrupo <- renderPlot({
    tryCatch({ alluvialPlot( wideData(), cols=levels(myData$color)  ) }, 
      error=function(e){ 
        plotHandler("Debe seleccionar un grupo para visualizar los resultados") } )
    });
  
  output$graficoTrayectoriasGradoGrupo_plotly <-  renderPlotly({
    tryCatch({ 
      cols <- levels(longData()$color)
      
      aux <- longData() %>% rename("Año"=AplicacionAnio)
      
      ggTrayectorias <- ggplot(aux, aes(x=Año, y=Puntaje, group=Nombre)) +
        geom_line(alpha=0.4) + geom_point(aes(shape=Grado, color=Nivel)) + 
        scale_color_manual(values=cols, drop=FALSE) +
        xlab("Año de aplicación") + theme_bw() + 
        ggtitle("Resultados en por puntaje y nivel de desempeño según grado y año de aplicación") +
        theme(legend.position = "none");
      
      ggplotly(ggTrayectorias) }, 
      error=function(e){ 
        plotHandler("Debe seleccionar un grupo para visualizar los resultados") } )
  });
  

  # output$graficoTrayectoriasGradoGrupo <- renderPlot({
  #   cols <- levels(longData()$color)
  #   
  #   ggTrayectorias <- ggplot(longData(), 
  #                            aes(x=AplicacionAnio, y=Puntaje, group=AlumnoCodigo)) +
  #     geom_line(alpha=0.4) + geom_point(aes(shape=Grado, color=Nivel)) + 
  #     scale_color_manual(values=cols, drop=FALSE) +
  #     xlab("Año de aplicación") + theme_bw() + 
  #     ggtitle("Resultados en por puntaje y nivel de desempeño según grado y año de aplicación") +
  #     guides(color = guide_legend(order = 1), shape = guide_legend(order = 0));
  #   
  #   print(ggTrayectorias)
  # })
  
  ###  tabPanel2
  output$tablaAlumnos <- DT::renderDT({
    
    aux <- tryCatch({ 
      aux <- wideData()
      names(aux) <- sub(paste0("_", areaCodigo, "$"), "", names(aux));
      names(aux) <- sub("_", "<br>", names(aux));
      names(aux) <- sub("GradoCodigo", "Grado", names(aux));
      names(aux) <- sub("DuracionPrueba", "Duración", names(aux));
      
      aux;
    }, error=function(e){ data.frame() })
    
    DT::datatable(aux,
                  filter=c("top"), 
                  selection=c("multiple"), 
                  rownames=FALSE, 
                  escape=FALSE,
                  options=list(#language = list(zeroRecords = "No records to display - custom text"), 
                    pageLength=10, autoWidth=TRUE,
                    lengthMenu = c(4, 6, 8, 10, 15, 20)) )},
    options=list(scrollX=TRUE)
  );
  
  
}
