# Aplicação Gráfica para
# Cálculo de Investimento em Saneamento
# Clique em Run App para rodar a aplicação
#

library(shiny)
library(shinythemes)
library(shinycssloaders)

library(rlog)

library(tidyverse)
library(dplyr, warn.conflicts = FALSE)

library(ggplot2)
library(plotly)


library(curl)
library(httr)
library(readxl)
library(jsonlite)

source("data_loader.R")
source("projecao.R")
source("agua_esgoto.R")
source("app_state.R")

rlog::log_info("Starting")

painelProjecao <- navbarMenu(
  "Projeção Populacional",
  icon = icon("users"),
  tabPanel("Fazer Projeção", projecao_populacional_ui("projecao"))
)


painelAguaEsgoto <- navbarMenu(
  "Água e Esgoto",
  icon = icon("faucet"),
  tabPanel("Módulo Demográfico", moduloDemografico("demografico")),
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


# checking data folder
check_and_create_datasets()

server <- function(input, output, session) {
  rlog::log_info(sprintf('Started new session'))
  projecao_server("projecao")
  aguaEsgotoServer("aguaesgoto")
}

shinyApp(ui = ui, server = server)
