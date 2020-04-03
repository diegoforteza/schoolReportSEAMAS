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

