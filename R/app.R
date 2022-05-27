# Aplicação Gráfica para
# Cálculo de Investimento em Saneamento
# Clique em Run App para rodar a aplicação
#

library(shiny)
library(shinythemes)
library(shinycssloaders)

library(rlog)

library(dplyr, warn.conflicts = FALSE)
library(tidyr)

library(ggplot2)
library(plotly)


library(curl)
library(httr)
library(readxl)
library(jsonlite)

source("app_state.R")
source("data_loader.R")

source("ui_projecao.R")
source("server_projecao.R")

source("ui_agua_esgoto.R")
source("server_agua_esgoto.R")


rlog::log_info("Starting")

# checking data folder
check_and_create_datasets()

painelProjecao <- navbarMenu(
  "Projeção Populacional",
  icon = icon("users"),
  tabPanel("Fazer Projeção", projecao_populacional_ui("projecao"))
)


painelAguaEsgoto <- navbarMenu(
  "Água e Esgoto",
  icon = icon("faucet"),
  tabPanel("Módulo Demográfico", modulo_demografico_ui("aguaesgoto")),
  tabPanel("Módulo Tecnológico"),
  tabPanel("Módulo Orçamentário"),
  tabPanel("Módulo Financeiro")
)


painelResiduosSolidos <-
  tabPanel("Resíduos Sólidos",
           icon = icon("recycle"),
           fluid = TRUE)


painelDrenagemUrbana <-
  tabPanel("Drenagem Urbana",
           icon = icon("water"),
           fluid = TRUE)


painelConfig <- navbarMenu("Configurações",
                           icon = icon("cog"),
                           tabPanel("Atualização de dados"),
)


ui <- fluidPage(
  navbarPage(
    "InvestSan",
    theme = shinytheme("cosmo"),
    painelProjecao,
    painelAguaEsgoto,
    painelResiduosSolidos,
    painelDrenagemUrbana,
    painelConfig,
  )
)



server <- function(input, output, session) {
  rlog::log_info(sprintf('Started new session'))
  projecao_server("projecao")
  agua_esgoto_server("aguaesgoto")
}

shinyApp(ui = ui, server = server)
