#' @title ksTestAplicacionAlumnoTheta
#' @description Realiza prueba de hipotesis (ks.test) para testear si existen diferencias significativas 
#' enrte las distriubciones seleccionadas en input. Imprime texto por defecto.
#' @note Asume que el objeto myData esta salvado globalmente, el mismo contiene los resultados de SEA+
#' @param input objeto list  debe contener los siguientes campos: 
#' tipo1, tipo2, tipo3 cada uno de ellos pude ser 
#' ("Centro", "Inspeccion", "Inspeccion Quintil", "Nacional", "Nacional Quintil")
#' anio1, anio2, anio3 cada uno de ellos corresponde a anios en los que se realizo la evaluacion.
#' @param areaCodigo 1-Ciencias, 2-Lectura, 3-Matemática
#' @include myHeader.R
#' @export
ksTestAplicacionAlumnoTheta = function(input, areaCodigo){
  
  area <- switch(as.numeric(areaCodigo), "Matemática", "Lectura", "Ciencias");

  myTestData <- list()
  for(i in 1:3){
    
    strAnio <- names(input)[which(grepl("anio", names(input)) & grepl(i, names(input)))];
    anio <- input[[ strAnio ]];
    strTipo <- names(input)[which(grepl("tipo", names(input)) & grepl(i, names(input)))];
    tipo <- input[[ strTipo ]];
    
    auxData <- myData[which(myData$AreaCodigo == areaCodigo & 
                              myData$AplicacionAnio == anio), ];

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
    
    myTestData[[i]] <- auxData[, c("CentroCodigo", "CentroRUEE", "AreaCodigo", "GradoCodigo", "AplicacionAlumnoTheta")];
  }
  
  alternative <- c(#"two.sided", 
                   "greater", "less")
  resultados <- list()
  a <- 1;
  for(h1 in alternative){
    
    res1 <- ks.test(myTestData[[1]]$AplicacionAlumnoTheta, 
                    myTestData[[3]]$AplicacionAlumnoTheta, 
                    alternative = h1);
    
    interpretacion1 <- NULL
    if(res1$p.value < 0.05){
      x <- paste(input$tipo1, input$anio1)
      y <- paste(input$tipo3, input$anio3)
      interpretacion1 <- switch(h1,
                                "two.sided"=paste(x, "y", y, "no tienen la misma distribución"),
                                "greater"=paste(x, "tiene un peor desempeño que el observado en", y),
                                "less"=paste(x, "tiene un mejor desempeño que el observado en", y) );
    }
    
    res2 <- ks.test(myTestData[[1]]$AplicacionAlumnoTheta, 
                    myTestData[[2]]$AplicacionAlumnoTheta, 
                    alternative = h1);
    
    interpretacion2 <- NULL
    if(res2$p.value < 0.05){
      x <- paste(input$tipo1, input$anio1)
      y <- paste(input$tipo2, input$anio2)
      interpretacion2 <- switch(h1,
                                "two.sided"=paste(x, "y", y, "no tienen la misma distribución"),
                                "greater"=paste(x, "tiene un peor desempeño que el observado en", y),
                                "less"=paste(x, "tiene un mejor desempeño que el observado en", y) );
    }
    
    res3 <- ks.test(myTestData[[2]]$AplicacionAlumnoTheta, 
                    myTestData[[3]]$AplicacionAlumnoTheta, 
                    alternative = h1);
    
    interpretacion3 <- NULL  
    if(res3$p.value < 0.05){
      x <- paste(input$tipo2, input$anio2)
      y <- paste(input$tipo3, input$anio3)
      interpretacion3 <- switch(h1,
                                "two.sided"=paste(x, "y", y, "no tienen la misma distribución"),
                                "greater"=paste(x, "tiene un peor desempeño que el observado en", y),
                                "less"=paste(x, "tiene un mejor desempeño que el observado en", y) );
    }
    
    resultados[[a]] <- c(interpretacion1, interpretacion2, interpretacion3);
    a <- a + 1;
  }
  
  resultados <- unlist(resultados);
  resultados <- resultados[!duplicated(resultados)];
  resultados <- paste(resultados[grepl("Centro", resultados)], collapse="\n");
  return(resultados)
}

