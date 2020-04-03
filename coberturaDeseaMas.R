#'@title coberturaDeseaMasUI
#'@description coberturaDeseaMasUI
#'@export
coberturaDeseaMasUI <- function(id){
  ns <- NS(id)
  fluidPage(
    
    titlePanel(i18n$t("Cobertura")),
    fluidRow(
      column(width = 4,
             selectInput(ns("anioAplicacion"), i18n$t("Seleccionar año de evaluación:"),
                         choices = aniosAplicaciones,
                         multiple=FALSE, selectize=FALSE)
      ),
      column(width = 8, 
             i18n$t("A continuación se presenta una tabla y un gráfico con las aplicaciones de SEA+ por grado y por área, para el año seleccionado. "),
             i18n$t("En la tabla se muestra la cantidad de alumnos de cada grupo y la cantidad de ellos que participaron en cada prueba. "),
             i18n$t("Mientras que en el gráfico se presenta el porcentaje de cobertura de cada grupo."))
    ),

    br(),
    br(),
    fluidRow(
      column(width = 4, htmlOutput(ns("tablaCobertura"))),
      column(width = 8, plotOutput(ns("graficoCobertura"))),
    )
    
  )
}

#'@title coberturaDeseaMas
#'@description coberturaDeseaMas
#'@export
coberturaDeseaMas = function(input, output, session){
  ns <- session$ns
  
  cobertura_reactive <- reactive({ 
    cobertura %>%
      filter(CentroRUEE %in% centroRUEE) %>% 
      filter(Año %in% input$anioAplicacion) %>%
      mutate(Grupo=paste(GradoCodigo, GrupoNombre),
             nAlumnos=replace_na(nAlumnos, 0),
             nAlumnosCie=replace_na(nAlumnosCie, 0),
             nAlumnosLec=replace_na(nAlumnosLec, 0),
             nAlumnosMat=replace_na(nAlumnosMat, 0)) %>%
      filter(nAlumnos != 0);
  })
    
  output$tablaCobertura <- renderTable({ 
    cobertura_reactive() %>% 
      select(Grupo, nAlumnos, nAlumnosCie, nAlumnosLec, nAlumnosMat) %>%
      dplyr::rename("Alumnos"=nAlumnos, 
             Ciencias=nAlumnosCie, Lectura=nAlumnosLec, "Matemática"=nAlumnosMat)
    }, include.rownames=FALSE, digits = 0);
  
  output$graficoCobertura <- renderPlot({ plotCobertura(cobertura_reactive()) });
  

}
