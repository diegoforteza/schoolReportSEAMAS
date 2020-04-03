#' @title myHeader
#' @description genera el header para la escuela
#' @note Asume que centroRUEE y alumnoNombre (TRUE/FALSE) existen en el ambiente global
#' @export
myHeader <- function(){
  centroNro <- substr(centroRUEE, 5, 7);
  centroNro <- as.character(as.numeric(centroNro));
  
  if(!alumnoNombre) centroNro <- 0;
  
  paste(i18n$t("Escuela"), "N°", centroNro);
}