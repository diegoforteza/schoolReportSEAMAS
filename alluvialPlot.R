#' @title alluvialPlot
#' @description grafica los cambios de nivel entre evaluaciones sucesivas.
#' @param myPlotData data.frame en el que cada fila corresponde a un alumno y en sus columnas 
#' contiene los resultados por nivel de cada una de las evaluaciones SEA+ que haya participado.
#' Ejemplo de nombre de columnas: Nivel_2018, Nivel_2019.
#' @include plotHandler.R 
#' @import dplyr ggplot2 ggalluvial
#' @export
alluvialPlot = function(myPlotData, cols=NULL){
  
  if(is.null(myPlotData)){ 
    plotHandler("No hay información suficiente para realizar este análisis.")
  } else {
    if(is.null(cols)){
      plotHandler("Es necesario especificar colores, argumento cols.")
    } else {
      data_allu <- myPlotData %>% select(starts_with("Nivel")) %>%  
        rename_all(funs(substr(., 1, 10)))
      
      myVars <- names(data_allu)
      
      if(length(myVars) >= 2){
        
        data_allu <- data_allu %>% filter(complete.cases(data_allu)) %>% 
          dplyr::group_by(.dots = myVars) %>% dplyr::summarise(Freq = n()) %>% 
          rename_all(funs(sub("_", " ", .)))
        
        # cols <- levels(myData$color)
        
        if(nrow(data_allu) > 0){
          data_lodes <- to_lodes_form(data_allu, axes = 1:(ncol(data_allu)-1))
          
          ggAlluvial <- ggplot(data_lodes, aes(x = x, stratum = stratum, alluvium = alluvium,
                                               y = Freq, fill = stratum, label = stratum)) +
            geom_flow(stat = "alluvium", color = "darkgray", reverse = F) +
            geom_stratum(reverse = F) + 
            scale_fill_manual(values=cols, drop=FALSE) +
            geom_text(stat = "stratum", size = 4, reverse = F) + 
            ylab("") + xlab("") + theme_bw() + 
            ggtitle("Transición entre niveles de desempeño") + 
            theme(legend.position = "none", axis.title.y = element_blank(),
                  axis.text.y = element_blank(), axis.ticks.y = element_blank())
          
        } else { ggAlluvial <- NULL }
      } else { ggAlluvial <- plotHandler("No hay información suficiente para realizar este análisis.") }
      
      return(list(ggAlluvial=ggAlluvial,
                  data_allu=data_allu))
    }
  }
}
