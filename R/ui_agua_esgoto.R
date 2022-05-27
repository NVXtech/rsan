get_snis_list <- function() {
  data("snis")
  snis_choices <- as.list(snis$caminho)
  names(snis_choices) <- snis$nome
  return(snis_choices)
}

modulo_demografico_ui <- function(id) {
  ns <- shiny::NS(id)
  sidebarLayout(
    sidebarPanel(
      titlePanel("Dados de Entrada"),
      selectInput(
        inputId = ns("snis"),
        label = strong("Selecione o ano do SNIS"),
        choices = get_snis_list(),
        selected = app_state$demografico$snis
      ),
      sliderInput(
        inputId = ns("ano"),
        strong("Realizar cálculo para o ano de:"),
        step = 1,
        min = 2021,
        max = 2040,
        value = 2033
      ),
      numericInput(
        inputId = ns("meta_agua"),
        label = strong("Meta de atendimento para abastecimento de água (%)"),
        value=99,
        min = 0,
        max = 100
      ),
      numericInput(
        inputId = ns("meta_esgoto"),
        label = strong("Meta de atendimento para esgoto (%)"),
        value=90,
        min = 0,
        max = 100
      ),
      numericInput(
        inputId = ns("proporcao"),
        label = strong("Proporção entre a densidade esgoto e abastecimento (%)"),
        value=80,
        min = 0,
        max = 100
      ),
      fluidRow(actionButton(ns("rodar"), label = "Calcular"))
    ),
    mainPanel(
      plotlyOutput(ns("grafico_total")),
      DT::dataTableOutput(ns("tabela")),
      ),
  )
}

modulo_orcamentario_ui <- function(id) {
  ns <- shiny::NS(id)
  sidebarLayout(
    sidebarPanel(
      titlePanel("Dados de Entrada"),
      selectInput(
        inputId = ns("sinapi"),
        label = strong("Selecione o ano e mês do SINAPI"),
        choices = get_snis_list(),
        selected = app_state$orcamentario$snis
      ),
      numericInput(
        inputId = ns("perda_agua"),
        label = strong("Estimativa de Perda de água (%)"),
        value=25,
        min = 0,
        max = 100
      ),
      fluidRow(actionButton(ns("rodar"), label = "Calcular"))
    ),
    mainPanel(
      plotlyOutput(ns("grafico_total")),
      DT::dataTableOutput(ns("tabela")),
    ),
  )
}

modulo_financeiro_ui <- function(id) {
  ns <- shiny::NS(id)
  sidebarLayout(
    sidebarPanel(
      titlePanel("Dados de Entrada"),
      selectInput(
        inputId = ns("snis"),
        label = strong("Selecione o ano do SNIS"),
        choices = get_snis_list(),
        selected = app_state$demografico$snis
      ),
      fluidRow(actionButton(ns("rodar"), label = "Calcular"))
    ),
    mainPanel(
      plotlyOutput(ns("grafico_total")),
      DT::dataTableOutput(ns("tabela")),
    ),
  )
}
