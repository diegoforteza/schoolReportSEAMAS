#' @title plotHandler
#' @description Handles plots, which would otherwise crash.
#' @import ggplot2
#' @export
plotHandler = function(text){
  
  theme_blank <- function(...) {
    ret <- theme_bw(...)
    ret$line <- element_blank()
    ret$rect <- element_blank()
    ret$strip.text <- element_blank()
    ret$axis.text <- element_blank()
    ret$plot.title <- element_blank()
    ret$axis.title <- element_blank()
    ret$plot.margin <- structure(c(0, 0, -1, -1), unit = "lines", valid.unit = 3L, class = "unit")
    ret
  }
  
  # text <- XRtranslate(text);
  aux <- unlist(strsplit(text, ' '));
  if(length(unlist(aux)) > 8){
    text <- paste(paste(aux[1:8], collapse=" "), '\n', paste(aux[9:length(aux)], collapse=" "), sep=" ");
  }
  g <-  ggplot() + 
    ggplot2::annotate("text", x = 3, y = 25, label = text) +
    theme_blank();
  
  print(g);
}
