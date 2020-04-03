#' @title plotNivelesTAIxGrado
#' @description Realiza grafico de barras por nivel y grado segun las opciones indicadas en input
#' @note Asume que el objeto myData esta salvado globalmente, el mismo contiene los resultados de SEA+
#' @param input objeto list  debe contener los siguientes campos: 
#' tipo1, tipo2, tipo3 cada uno de ellos pude ser 
#' ("Centro", "Inspeccion", "Inspeccion Quintil", "Nacional", "Nacional Quintil")
#' anio1, anio2, anio3 cada uno de ellos corresponde a anios en los que se realizo la evaluacion.
#' @param areaCodigo 1-Ciencias, 2-Lectura, 3-Matemática
#' @param titleArea se debe incluir el area en el titulo del grafico, por defecto FALSE
#' @import ggplot2
#' @include myHeader.R
#' @export
plotNivelesTAIxGrado = function(input, areaCodigo, titleArea=FALSE){
  
  area <- switch(as.numeric(areaCodigo), "Matemática", "Lectura", "Ciencias");
  
  myLevels <- c("Nivel Bajo 1", "Nivel 1", "Nivel 2", 
                "Nivel 3", "Nivel 4", "Nivel 5",
                "Nivel 6", "Nivel 7");
  if(areaCodigo == 3){
    myLevels <- c("Nivel Bajo 1", "Nivel 1", "Nivel 2", 
                  "Nivel 3", "Nivel 4", "Nivel 5", "Nivel 6");
  }
  
  ###
  myPlotData <- auxTitle <- NULL
  for(i in 1:3){
    
    strAnio <- names(input)[which(grepl("anio", names(input)) & grepl(i, names(input)))];
    anio <- input[[ strAnio ]];
    strTipo <- names(input)[which(grepl("tipo", names(input)) & grepl(i, names(input)))];
    tipo <- input[[ strTipo ]];
      
    auxData <- myData[which(myData$AreaCodigo == areaCodigo & 
                                 myData$AplicacionAnio == anio), ];
    auxData$Nivel <- factor(auxData$Nivel, levels=myLevels, ordered=TRUE);
    
    if(tipo == "Centro") {
      auxData <- auxData[which(auxData$CentroRUEE == centroRUEE), ];
      myTitle <- myHeader();
    }
    if(tipo == "Inspección"){
      insp <- unique(auxData$AplicacionInspeccionCodigo[which(auxData$CentroRUEE == centroRUEE)]);
      auxData <- auxData[which(auxData$AplicacionInspeccionCodigo == insp), ];
      myTitle <- "Inspección";
    }
    if(tipo == "Inspección Quintil"){
      insp <- unique(auxData$AplicacionInspeccionCodigo[which(auxData$CentroRUEE == centroRUEE)]);
      quintil <- unique(auxData$QUINTIL[which(auxData$CentroRUEE == centroRUEE)]);
      auxData <- auxData[which(auxData$AplicacionInspeccionCodigo == insp &
                                 auxData$QUINTIL == quintil), ];
      myTitle <- "Inspección Quintil";
    }
    if(tipo == "Nacional"){
      myTitle <- "Nacional";
    }
    if(tipo == "Nacional Quintil"){
      quintil <- unique(auxData$QUINTIL[which(auxData$CentroRUEE == centroRUEE)]);
      auxData <- auxData[which(auxData$QUINTIL == quintil), ];
      myTitle <- "Nacional Quintil";
    }
    
    ## add title for facet warp
    auxPlotData <- data.frame(round(prop.table(table(auxData$GradoCodigo, auxData$Nivel), 1), 2)*100);
    auxPlotData$myTitle <- paste(myTitle, anio);
    if(titleArea) auxPlotData$myTitle <- paste(area, anio);
    
    auxTitle <- c(auxTitle, paste(myTitle, anio));
    myPlotData <- rbind(myPlotData, auxPlotData);
  }
  ### 
  myPlotData$myTitle <- factor(myPlotData$myTitle, levels=unique(auxTitle));
  
  cols <- levels(myData$color);
  myLim <- c(0, round(ceiling(max(myPlotData$Freq, na.rm = TRUE)/10 * 1.1)*10));
  myNCols <- length(unique(myPlotData$myTitle));
  
  gg1 <- ggplot(myPlotData, aes(x=Var2, y=Freq, group=Var2, fill=Var2)) +
    geom_bar(stat = "identity") + 
    scale_fill_manual(values=cols, drop=FALSE) +
    ylim(myLim) + ylab("") + xlab("") +
    facet_wrap(~ myTitle + Var1, ncol=myNCols, dir="v") + 
    geom_text(aes(y = Freq - 3, label = paste0(round(Freq, 2), '%')),
              position = position_dodge(width = .9), size = 3) +
    labs(fill="") + theme_bw() + 
    theme(axis.text.x = element_text(angle = 30, vjust = 0.8), legend.position = "none");
  
  print(gg1)
}