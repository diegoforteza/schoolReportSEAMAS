#' @title plotCobertura
#' @description plotea el heatmap con la cobertura por grado/grupo del centro.
#' @param cobertura datos de cobertura del centro y anio seleccionados
#' @import ggplot2 dplyr
#' @importFrom tidyr pivot_longer
#' @export
plotCobertura = function(cobertura){
  
  aux <- cobertura %>% 
    mutate("Ciencias"=ifelse(is.nan(nAlumnosCie/nAlumnos), 0, round(nAlumnosCie/nAlumnos, 2)*100),
           "Lectura"=ifelse(is.nan(nAlumnosLec/nAlumnos), 0, round(nAlumnosCie/nAlumnos, 2)*100), 
           "Matemática"=ifelse(is.nan(nAlumnosMat/nAlumnos), 0, round(nAlumnosCie/nAlumnos, 2)*100)) %>% 
    pivot_longer(cols=c("Ciencias", "Lectura", "Matemática"), values_to = "Cobertura")
  
  aux <- aux %>% arrange(GradoCodigo, GrupoNombre) %>% arrange(desc(Año));
  aux$name <- sub("nAlumnos", "", aux$name);
  aux$myY <- paste0("Grupo ", aux$GradoCodigo, " ", aux$GrupoNombre, "(", aux$nAlumnos, " alumnos)");
  
  ggplot(aux, aes(x=name, y=myY, fill=Cobertura)) + geom_tile() +
    scale_y_discrete(limits=rev(unique(aux$myY))) +
    scale_x_discrete(position = "top") +
    geom_text(aes(label = paste(Cobertura, "%"))) +
    scale_fill_gradient(low="darkred", high="darkgreen", limits=c(0, 100)) + 
    ggtitle(paste0("Cobertura por área año ", unique(aux$Año))) +
    ylab("") + xlab("") + theme_bw()
  
}