library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggalluvial)
# library(shinylogs)
library(plotly)
library(knitr)
library(kableExtra)
library(shiny.i18n)
i18n <- Translator$new(translation_json_path=here::here("translations/translations.json"))
i18n$set_translation_language("en")


storeLogs <<-  FALSE;
alumnoNombre <<- FALSE;
centroRUEE <<- 199
##############
source("intro.R")
source("alluvialPlot.R")
source("coberturaDeseaMas.R")
source("ksTestAplicacionAlumnoTheta.R")
source("myHeader.R")
source("nivelesDeseaMas.R")
source("plotCobertura.R")
source("plotHandler.R")
source("plotNivelesTAIxGrado.R")
source("plotNivelesTAIxGradoGrupo.R")
source("plotTable.R")
source("resultadosDeseaMas.R")
source("resultadosGradoGrupoDeseaMas.R")

##############
## cargo datos de cobertura y resultados.
##############
load(here::here("cobertura.rda"))
load(here::here("myData.rda"))

areaChoices <- setNames(3:1, c("Ciencias", "Lectura", "Matemática"));
aniosAplicaciones <- sort(unique(myData$AplicacionAnio[which(myData$CentroRUEE == centroRUEE)]), decreasing=TRUE); 

##############
## cargo datos de cobertura y resultados.
##############
load(here::here("nivelesTAI.rda"))

coloresNiveles <- nivelesTAI[, c("PruebaNivelTAIDesc", "color")]
coloresNiveles <- coloresNiveles[!duplicated(coloresNiveles), ];

##############
## cargo estructura centros
##############
# load(here::here("inst/centros.rda"))
# centro$CentroNombre <- iconv(centro$CentroNombre, from="latin1", to="utf-8")

load(here::here("estructuraCentros.rda"))
estructuraCentro <- grupoAlumno[which(grupoAlumno$CentroRUEE == centroRUEE & 
                                        grupoAlumno$CentroAnio == max(grupoAlumno$CentroAnio) &
                                        grupoAlumno$GradoCodigo %in% 3:6), ];

gruposChoices <- setNames(unique(estructuraCentro$GrupoCodigo), 
                          unique(paste0(estructuraCentro$GradoCodigo, estructuraCentro$GrupoNombre)))

# load(here::here("inst/alumno.rda"))

## los breaks y los colores deberian venir predefinidos.
# brks <- quantile(aux$Puntaje_2018, probs = seq(.05, .95, .05), na.rm = TRUE)
# clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
#   {paste0("rgb(255,", ., ",", ., ")")}
# clrs
# [1] "rgb(255,255,255)" "rgb(255,244,244)" "rgb(255,232,232)" "rgb(255,221,221)" "rgb(255,210,210)" "rgb(255,198,198)"
# [7] "rgb(255,187,187)" "rgb(255,176,176)" "rgb(255,164,164)" "rgb(255,153,153)" "rgb(255,142,142)" "rgb(255,131,131)"
# [13] "rgb(255,119,119)" "rgb(255,108,108)" "rgb(255,97,97)"   "rgb(255,85,85)"   "rgb(255,74,74)"   "rgb(255,63,63)"  
# [19] "rgb(255,51,51)"   "rgb(255,40,40)"  
# help(col2rgb)
# colors <- c("#d11141", "#ffc425", "#00b159", "#ffc425", "#d11141")
# colorRampPalette(colors)(20)
# paste0("rgb(", apply(col2rgb(colorRampPalette(colors)(20)), 2, paste, collapse=", "), ")")


# ## pasarlo todo a una funcion y hacer testing
# 
# ### todo esto hay que pasarlo al momento de calcular wideData.
# ## hay que pensar como calcular ganancia sin harcodear.
# # tomar las columnas que tienen puntaje como nombre, ordenarlas por anio creciente y realizar restas
# colorIndicatorData <- aux[, grepl("Puntaje", names(aux))];
# names(colorIndicatorData) <- sub(paste0("_", areaCodigo, "$"), "", names(colorIndicatorData));
# auxMat <- do.call(rbind, strsplit(names(colorIndicatorData), "_"));
# ganancia <- data.frame(matrix(NA, nrow=nrow(colorIndicatorData), ncol=nrow(auxMat)));
# names(ganancia) <- paste0("Ganancia_", auxMat[, 2]);
# if(nrow(auxMat) > 1){
#   for(ind in 2:nrow(auxMat)){
#     ganancia[, ind] <- colorIndicatorData[, ind] - colorIndicatorData[, (ind-1)];
#   }
# }
# 
# aux <- dplyr::bind_cols(aux, ganancia)
# aux <- aux[, which(!apply(is.na(aux), 2, all))];
# 
# ## estos dos deberian estar predefinidos
# brks <- quantile(ganancia[, "Ganancia_2019"], probs = c(.05, .15, .85, .95), na.rm = TRUE);
# clrs <- paste0("rgb(", apply(col2rgb(colorRampPalette(colors)(length(brks) + 1)), 2, paste, collapse=", "), ")");
