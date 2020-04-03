library(shiny)
source('global.R')

ui <- dashboardPage(
  
  dashboardHeader(
    title=myHeader()
  ),
  
  dashboardSidebar(
    sidebarMenu(id="myTabs",
                # tags$img(src = "índice.jpeg"),
                menuItem(i18n$t("Inicio"), tabName="intro", icon=icon("home")),
                menuItem(i18n$t("Cobertura"), tabName="cobertura", icon=icon("percentage")),
                menuItem(i18n$t("Resultados Centro"), startExpanded = TRUE, icon=icon("school"),
                         menuSubItem(i18n$t("Ciencias"), tabName="resultadosCiencias"),
                         menuSubItem(i18n$t("Lectura"), tabName="resultadosLectura"),
                         menuSubItem(i18n$t("Matemática"), tabName="resultadosMatematica")),
                menuItem(i18n$t("Resultados Grado/Grupo"), startExpanded = TRUE, icon=icon("users"),
                         menuSubItem(i18n$t("Ciencias"), tabName="resultadosGradoGrupoCiencias"),
                         menuSubItem(i18n$t("Lectura"), tabName="resultadosGradoGrupoLectura"),
                         menuSubItem(i18n$t("Matemática"), tabName="resultadosGradoGrupoMatematica")),
                menuItem(i18n$t("Niveles de desempeño"), startExpanded = TRUE, icon=icon("list"),
                         menuSubItem(i18n$t("Ciencias"), tabName="nivelesCiencias"),
                         menuSubItem(i18n$t("Lectura"), tabName="nivelesLectura"),
                         menuSubItem(i18n$t("Matemática"), tabName="nivelesMatematica"))
                
    )
  ),
  
  dashboardBody(
    tabItems(      
      tabItem(tabName="intro", introUI("intro") ),
      tabItem(tabName="cobertura", coberturaDeseaMasUI("coberturaDeseaMas")),
      tabItem(tabName="resultadosCiencias", resultadosDeseaMasUI("resultadosCiencias")),
      tabItem(tabName="resultadosLectura", resultadosDeseaMasUI("resultadosLectura")),
      tabItem(tabName="resultadosMatematica", resultadosDeseaMasUI("resultadosMatematica")),
      
      tabItem(tabName="resultadosGradoGrupoCiencias", resultadosGradoGrupoDeseaMasUI("resultadosGradoGrupoCiencias")),
      tabItem(tabName="resultadosGradoGrupoLectura", resultadosGradoGrupoDeseaMasUI("resultadosGradoGrupoLectura")),
      tabItem(tabName="resultadosGradoGrupoMatematica", resultadosGradoGrupoDeseaMasUI("resultadosGradoGrupoMatematica")),

      tabItem(tabName="nivelesCiencias", nivelesDeseaMasUI("nivelesCiencias")),
      tabItem(tabName="nivelesLectura", nivelesDeseaMasUI("nivelesLectura")),
      tabItem(tabName="nivelesMatematica", nivelesDeseaMasUI("nivelesMatematica"))
    )
    
  )
)


server <- shinyServer(function(input, output) {
  
  if(storeLogs) track_usage(storage_mode = store_json(path = "logs/"));
  callModule(intro, "intro")
  
  callModule(coberturaDeseaMas, "coberturaDeseaMas")
  
  callModule(resultadosDeseaMas, "resultadosCiencias", areaCodigo=3)
  callModule(resultadosDeseaMas, "resultadosLectura", areaCodigo=2)
  callModule(resultadosDeseaMas, "resultadosMatematica", areaCodigo=1)
  
  callModule(resultadosGradoGrupoDeseaMas, "resultadosGradoGrupoCiencias", areaCodigo=3)
  callModule(resultadosGradoGrupoDeseaMas, "resultadosGradoGrupoLectura", areaCodigo=2)
  callModule(resultadosGradoGrupoDeseaMas, "resultadosGradoGrupoMatematica", areaCodigo=1)

  callModule(nivelesDeseaMas, "nivelesCiencias", areaCodigo=3)
  callModule(nivelesDeseaMas, "nivelesLectura", areaCodigo=2)
  callModule(nivelesDeseaMas, "nivelesMatematica", areaCodigo=1)
  
})

shinyApp(ui = ui, server = server)