#' @title plotTable
#' @description Plotea cuadro de doble entrada con transicion de niveles 
#' entre la evaluacion seleccionada y la anterior.
#' @param my.data tabla de doble entrada a ser ploteada
#' @param myTitle titulo a incluir en el grafico
#' @include plotHandler.R
#' @import ggplot2
#' @export
plotTable = function(my.data, myTitle=NULL){
  
  # if(sum(my.data) == 0) plotHandler("No hay información suficiente para realizar este análisis.")
  if(is.null(my.data)){ 
    plotHandler("No hay información suficiente para realizar este análisis.")
  } else {
    
    x = 1:ncol(my.data)
    y = 1:nrow(my.data)
    centers <- expand.grid(y,x)
    
    my.data.col <- matrix(0, nrow(my.data), ncol(my.data))
    diag(my.data.col) <- 1
    
    image(x, y, my.data.col[rev(x), y], 
          col = c("white", "gray"),
          xaxt = 'n', 
          yaxt = 'n', 
          xlab = '', 
          ylab = '')
    
    text(centers[, 2], rev(centers[, 1]), c(my.data), col= "black", cex=0.7)
    #add black lines
    abline(h=y + 0.5)
    abline(v=x + 0.5)
    
    mtext(colnames(my.data), at=1:ncol(my.data), padj = -1, cex=0.7)
    mtext(rev(rownames(my.data)), at=1:nrow(my.data), side = 2, las = 1, adj = 1.2, cex=0.7)
    # mtext('Niveles 2018', side=2, line=2.5, at=3)
    if(!is.null(myTitle)) mtext(myTitle, side=3, line=1.5, at=0.3)
  }
}