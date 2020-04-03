#' @title introUI
#' @description introUI
#' @export
introUI <- function(id){
  ns <- NS(id)
  
  fluidPage(
    
    fluidRow(
      box(width = 12,
          column(width = 6,
                 # br(),
                 br(),
                 div(#class="image-container",
                 img(src="logo.jpg", height="200px"),
                 style="text-align: center;"
                 )
          ),
          column(width = 6,
                 # br(),
                 h3(i18n$t("Es una evaluación adaptativa desarrollada para la medición de habilidades de Ciencias, Lectura y Matemática para alumnos de tercer a sexto grado escolar.")),
                 p(i18n$t("SEA + parte de la premisa de que cada niño posee diferentes tiempos y formas de acercarse al conocimiento. Este instrumento se adapta al nivel de cada estudiante que al enfrentarse a la prueba realiza un recorrido propio (de acuerdo a sus respuestas) dentro de un banco de actividades.")),
                 br(),
                 actionButton(ns("previewSEAMAS"), i18n$t("Saber más"))
          )
      )
    ),
    
    # br(),
    h3(i18n$t("Contenido:")),
    # br(),
    fluidRow(
      
      box(width = 3, title=span(icon("percentage"), i18n$t("Cobertura")), 
          status="primary", solidHeader = TRUE, #background = "maroon",
          p(i18n$t("En esta sección se presenta una tabla con la cantidad de aplicaciones de SEA+, por grado, grupo y por área.")),
          actionButton(ns("previewCobertura"), i18n$t("Saber más"))
      ),
      
      box(width = 3, title=span(icon("school"), i18n$t("Resultados del Centro")),
          status="primary", solidHeader = TRUE, #background = "maroon",
          p(i18n$t("En esta sección se presentan los niveles de desempeño de los alumnos del centro que realizaron evaluaciones SEA+.")),
          p(i18n$t("Estos resultados se organizan por área de conocimiento evaluada y por grado.")),
          actionButton(ns("previewResultados"), i18n$t("Saber más"))
      ),
      
      box(width = 3, title=span(icon("users"), i18n$t("Resultados por Grado/Grupo")),
          status="primary", solidHeader = TRUE, #background = "maroon",
          p(i18n$t("En esta sección se presentan tanto los niveles de desempeño, como los puntajes de los alumnos que realizaron evaluaciones SEA+.")),
          p(i18n$t("Estos resultados se organizan por área de conocimiento evaluada y se permite filtrar por grupo(s) para la visualización de los resultados.")),
          actionButton(ns("previewResultadosGradoGrupo"), i18n$t("Saber más"))
      ),
      
      box(width = 3, title=span(icon("list"), i18n$t("Niveles de desempeño")),
          status="primary", solidHeader = TRUE, #background = "maroon",
          p(i18n$t("Para cada área se presentan los niveles de desempeño, el tramo de puntaje al que corresponden y su descripción pedagógica.")), 
          p(i18n$t("Cada nivel esta descrito sobre un fondo de color que se mantiene en el resto de las visualizaciones de la app.")),
          p(i18n$t("Para acceder a la descripción de cada nivel se debe seleccionar el área deseada en el menú lateral."))
      )
      
    )
  ) 
}

#' @title intro
#' @description intro
#' @export
intro = function(input, output, session){
  
  observeEvent(input$previewSEAMAS, {
    showModal(modalDialog(
      box(width = 12, title = span(img(src="logo.jpg", height="70px")),
          status="primary", solidHeader = TRUE, 
          tags$p(i18n$t("La evaluación adaptativa en línea que denominamos SEA+, es un método eficaz de identificar el nivel de desempeño de los estudiantes y a partir de ello desarrollar acciones que tiendan a brindar oportunidades a todos los niños.")),
          tags$p(i18n$t("A diferencia de otras evaluaciones, ella se adapta al nivel de desempeño progresivo, es decir, a medida que el estudiante responde, si la respuesta es correcta avanza hacia un nivel de desempeño superior y en caso contrario, hacia un nivel inferior. El resultado final, identifica el nivel de desempeño alcanzado y a su vez se describe qué es lo que logra hacer con lo que sabe.")),
          tags$p(i18n$t("El objetivo de SEA+, es construir un referente común de evaluación que permita determinar el dominio de conocimientos adquiridos y a partir de ello, promover entre los docentes y diferentes actores, la reflexión y el análisis de los resultados que lleven a repensar mecanismos de acompañamiento de las diferentes trayectorias educativas."))
      ),
      footer = modalButton(i18n$t("Cerrar"))
    ))
  })
  
  observeEvent(input$previewCobertura, {
    showModal(modalDialog(
      box(width = 12, title=span(icon("percentage"), i18n$t("Cobertura")),
          status="primary", solidHeader = TRUE, 
          tags$p(i18n$t("En esta seccion se presenta una tabla con la cantidad de aplicaciones de SEA+, por grado, grupo y por área.")),
          tags$p(i18n$t("A su vez se permite modificar el ciclo de evaluación a visualizar de modo de poder comparar la cobertura de cada ciclo.")), 
      ),
      footer = modalButton(i18n$t("Cerrar"))
    ))
  })
  
  
  observeEvent(input$previewResultados, {
    showModal(modalDialog(
      box(width = 12, title=span(icon("school"), i18n$t("Resultados del Centro")),
          status="primary", solidHeader = TRUE, 
          tags$p(i18n$t("Los resultados para cada centro se presentan del mismo modo para cada área, si bien algunas visualizaciones especifican el grado de los alumnos en general se presentan para todo el centro en su conjunto, los mismos se muestran en dos sub pestañas:")), 
          tags$li(strong(i18n$t("Resultados:")), i18n$t("Muestra la distribución de los alumnos por niveles de desempeño y por grado, permite realizar hasta tres comparaciones. Se puede visualizar los resultados históricos del centro y a su vez los resultados a nivel nacional o de la inspección a la que el centro corresponde, teniendo la opción de esta comparación realizarla únicamente con resultados del mismo quintil socio económico. Todas las comparaciones realizadas son acompañadas de los resultados de una prueba de hipótesis en la que se compara la distribución de puntajes de las tres opciones seleccionadas. El resultado de estas pruebas solo se presenta si existen diferencias entre las distribuciones correspondientes a las opciones seleccionadas y si el centro es parte de las mismas. El texto se genera de manera automática y deberá ser revisado.")),
          tags$li(strong(i18n$t("Transiciones de nivel:")), i18n$t("Esta pestaña muestra los cambios entre niveles de desempeño de los alumnos que realizaron dos aplicaciones sucesivas de SEA+, contiene una tabla de doble entrada que muestra en las filas los niveles de partida y en las columnas los niveles de llegada y un diagrama de flujos que refleja los resultados de esta tabla. Se puede modificar el año de destino y automática mente se ajusta el año de partida al inmediatamente anterior al mismo."))
      ),
      footer = modalButton(i18n$t("Cerrar"))
    ))
  })
  
  
  observeEvent(input$previewResultadosGradoGrupo, {
    showModal(modalDialog(
      box(width = 12, title=span(icon("users"), i18n$t("Resultados por Grado/Grupo")),
          status="primary", solidHeader = TRUE, 
          tags$p(i18n$t("Los resultados a nivel de grado/grupo para cada centro se presentan de manera muy similar a los resultados a nivel de centro, con la diferencia que permiten visualizar los resultados para uno más grupos de manera específica, estos resultados se presentan en tres sub pestañas:")), 
          tags$li(strong(i18n$t("Resultados:")), i18n$t("Muestra información similar a la presentada en la misma pestaña pero de resultados a nivel de centro. Con la diferencia que el primer gráfico es fijo y refiere a los datos del centro y grupos seleccionados. Se pueden seleccionar múltiples grupos, esto, está pensado principalmente para ver cohortes generacionales. A diferencia de los resultados para el centro estas visualizaciones no están acompañadas de pruebas de hipótesis, podrían agregarse.")),
          tags$li(strong(i18n$t("Transiciones de nivel:")), i18n$t("Esta pestaña muestra los cambios entre niveles de desempeño de los alumnos que realizaron más de una aplicación de SEA+, contiene un gráfico interactivo en el cual se muestra en el eje “y” los puntajes obtenidos y en el eje “x” los ciclos de evaluación, a su vez cada individuo es presentado con un color asociado al nivel de desempeño y una forma asociada al grado. Además al posar el cursor sobre cada alumno se detalla toda la información relativa al mismo en el ciclo de evaluación correspondiente. Luego al igual que en los resultados por centro se presenta un diagrama de flujos que muestra los cambios en niveles de desempeño a lo largo de los ciclos de vacacional para los alumnos de los grupos seleccionados.")),
          tags$li(strong(i18n$t("Tabla de puntajes:")), i18n$t("Muestra para todos los alumnos del grupo seleccionado los resultados de todas las aplicaciones de SEA+, la tabla contiene información relativa al nivel de desempeño, puntaje, duración de la prueba y grado cursado en cada ciclo de evaluación."))
      ),
      footer = modalButton(i18n$t("Cerrar"))
    ))
  })
  
}
