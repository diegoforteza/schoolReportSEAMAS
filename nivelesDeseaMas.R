#' @title nivelesDeseaMasUI
#' @description Interfaz grafica para la visualizacion de la desripcion de los niveles.
#' @export
nivelesDeseaMasUI <- function(id){
  ns <- NS(id)
  
  fluidPage(
    
    titlePanel(i18n$t("Descripción niveles")),
    
    mainPanel(
      tableOutput(ns("niveles_kable"))
    )
  ) 
}

#' @title nivelesDeseaMas
#' @description Carga descripcion de niveles para el area seleccionada y genera 
#'  tabla con los colores correspondientes para cada nivel
#' @import knitr
#' @importFrom kableExtra kable_styling column_spec row_spec
#' @export
nivelesDeseaMas = function(input, output, session, areaCodigo){
  
  descripcionNiveles <- reactive({
    # aux <- getSTD_nivelesTAI(areaCodigo) %>% ## this should load an rda
    aux <- nivelesTAI %>% filter(AreaCodigo == areaCodigo) %>%
      mutate(
        Nivel=paste(PruebaNivelTAIDesc, '\n (', PruebaNivelTAIPunInicial, '-', PruebaNivelTAIPunFinal, ')'),
        color=as.character(factor(PruebaNivelTAIDesc, levels=levels(myData$Nivel), labels=levels(myData$color)))
      );
    aux$PruebaNivelTAIDescPedagoica <- gsub("\\n", "<br>", aux$PruebaNivelTAIDescPedagoica);
    # aux$PruebaNivelTAIDescPedagoica <- gsub("\\u0095", "\\&bull;", aux$PruebaNivelTAIDescPedagoica);
    # aux$PruebaNivelTAIDescPedagoica <- gsub("\u0095", "\u2022", aux$PruebaNivelTAIDescPedagoica);
    aux;
  })
  
  # library(kableExtra)
  output$niveles_kable <- function() {
    if(areaCodigo == 3){
      myTable <- 
        descripcionNiveles() %>%
        select(Nivel, PruebaNivelTAIDescPedagoica) %>%
        rename("Descripción Pedagógica"=PruebaNivelTAIDescPedagoica) %>%
        knitr::kable("html", escape = F) %>%
        kable_styling(fixed_thead = T) %>%
        column_spec(1, bold = T, border_right = T, width = "15em") %>%
        row_spec(1, bold = F, color = "white", background = descripcionNiveles()$color[1]) %>%
        row_spec(2, bold = F, color = "white", background = descripcionNiveles()$color[2]) %>%
        row_spec(3, bold = F, color = "white", background = descripcionNiveles()$color[3]) %>%
        row_spec(4, bold = F, color = "white", background = descripcionNiveles()$color[4]) %>%
        row_spec(5, bold = F, color = "white", background = descripcionNiveles()$color[5]) %>%
        row_spec(6, bold = F, color = "white", background = descripcionNiveles()$color[6]) %>%
        row_spec(7, bold = F, color = "white", background = descripcionNiveles()$color[7]);
    } else {
      myTable <-
        descripcionNiveles() %>%
        select(Nivel, PruebaNivelTAIDescPedagoica) %>%
        rename("Descripción Pedagógica"=PruebaNivelTAIDescPedagoica) %>%
        knitr::kable("html", escape = F) %>%
        kable_styling(fixed_thead = T) %>%
        column_spec(1, bold = T, border_right = T, width = "15em") %>%
        row_spec(1, bold = F, color = "white", background = descripcionNiveles()$color[1]) %>%
        row_spec(2, bold = F, color = "white", background = descripcionNiveles()$color[2]) %>%
        row_spec(3, bold = F, color = "white", background = descripcionNiveles()$color[3]) %>%
        row_spec(4, bold = F, color = "white", background = descripcionNiveles()$color[4]) %>%
        row_spec(5, bold = F, color = "white", background = descripcionNiveles()$color[5]) %>%
        row_spec(6, bold = F, color = "white", background = descripcionNiveles()$color[6]) %>%
        row_spec(7, bold = F, color = "white", background = descripcionNiveles()$color[7]) %>%
        row_spec(8, bold = F, color = "white", background = descripcionNiveles()$color[8]);
    }
    
    myTable;
  }
}
