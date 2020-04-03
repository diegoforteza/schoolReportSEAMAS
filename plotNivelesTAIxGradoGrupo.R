#'@title plotNivelesTAIxGradoGrupo
#'@description Realiza grafico de barras por nivel y grado segun las opciones indicadas en input, 
#' la primera opcion es fija y corresponde al centro
#'@note Asume que el objeto myData esta salvado globalmente, el mismo contiene los resultados de SEA+
#'@param input objeto list  debe contener los siguientes campos: 
#' gradoGrupo_t2, gradoGrupo_t3 cada uno de ellos pude ser 
#' ("Centro", "Inspeccion", "Inspeccion Quintil", "Nacional", "Nacional Quintil")
#'@param areaCodigo 1-Ciencias, 2-Lectura, 3-Matemática
#'@import ggplot2
#' @include myHeader.R
#'@export
plotNivelesTAIxGradoGrupo = function(input, areaCodigo){
  
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
    if(i == 1){
      tipo <- "Centro"
    } else {
      strTipo <- names(input)[grep("gradoGrupo_t", names(input))[(i-1)]]
      tipo <- input[[ strTipo ]];
    }
    
    auxData <- myData[which(myData$AreaCodigo == areaCodigo), ];
    auxData$GradoCodigo <- as.character(auxData$GradoCodigo);
    auxData$Nivel <- factor(auxData$Nivel, levels=myLevels, ordered=TRUE);
    
    if(tipo == "Centro"){
      auxData <- auxData[which(auxData$CentroRUEE == centroRUEE), ];
      ## filtro por grupo codigo
      if(!is.null(input$grupoCodigo_gradoGrupo)){
        myIds <- estructuraCentro$AlumnoCodigo[which(estructuraCentro$GrupoCodigo %in% 
                                                       input$grupoCodigo_gradoGrupo)];
        auxData <- auxData[which(auxData$AlumnoCodigo %in% myIds), ]
        myGrados <- unique(auxData$GradoCodigo)
      } else {
        if(!is.null(input$gradoCodigo_gradoGrupo)){
          myIds <- estructuraCentro$AlumnoCodigo[which(estructuraCentro$GradoCodigo %in% 
                                                         input$gradoCodigo_gradoGrupo)];
          auxData <- auxData[which(auxData$AlumnoCodigo %in% myIds), ]
          myGrados <- unique(auxData$GradoCodigo)
        }
      }
      myTitle <- myHeader()
    }
    if(tipo == "Inspección"){
      insp <- unique(auxData$AplicacionInspeccionCodigo[which(auxData$CentroRUEE == centroRUEE)])
      auxData <- auxData[which(auxData$AplicacionInspeccionCodigo == insp), ];
      myTitle <- "Inspección"
    }
    if(tipo == "Inspección Quintil"){
      insp <- unique(auxData$AplicacionInspeccionCodigo[which(auxData$CentroRUEE == centroRUEE)])
      quintil <- unique(auxData$QUINTIL[which(auxData$CentroRUEE == centroRUEE)])
      auxData <- auxData[which(auxData$AplicacionInspeccionCodigo == insp &
                                 auxData$QUINTIL == quintil), ];
      myTitle <- "Inspección Quintil"
    }
    if(tipo == "Nacional"){
      myTitle <- "Nacional"
    }
    if(tipo == "Nacional Quintil"){
      quintil <- unique(auxData$QUINTIL[which(auxData$CentroRUEE == centroRUEE)])
      auxData <- auxData[which(auxData$QUINTIL == quintil), ];
      myTitle <- "Nacional Quintil"
    }
    
    ## filtro por grado
    if(!is.null(myGrados)){ auxData <- auxData[which(auxData$GradoCodigo %in% myGrados), ]}
    
    ## add title for facet warp
    auxData <- data.frame(round(prop.table(table(auxData$AplicacionAnio, auxData$Nivel), 1), 2)*100)
    auxData$myTitle <- myTitle
    
    auxTitle <- c(auxTitle, myTitle);
    myPlotData <- rbind(myPlotData, auxData)
  }
  
  myPlotData$myTitle <- factor(myPlotData$myTitle, levels=unique(auxTitle));
  ###  
  cols <- levels(myData$color)
  myLim <- c(0, round(ceiling(max(myPlotData$Freq, na.rm = TRUE)/10 * 1.1)*10));
  
  gg1 <- ggplot(myPlotData, aes(x=Var2, y=Freq, group=Var2, fill=Var2)) +
    geom_bar(stat = "identity") + scale_fill_manual(values=cols, drop=FALSE) +
    ylim(myLim) + ylab("") + xlab("") +
    facet_wrap(~ myTitle + Var1, ncol=3, dir="v") + 
    geom_text(aes(y = Freq - 3, label = paste0(round(Freq, 2), '%')),
              position = position_dodge(width = .9), size = 3) +
    labs(fill="") + theme_bw() + 
    theme(axis.text.x = element_text(angle = 30, vjust = 0.8), legend.position = "none");
  
  print(gg1)
}