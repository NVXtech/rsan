#
# Projeção Populacional
#
source("projecao_populacional.R", local=TRUE)

get_fonte1_list <- function() {
  populacoes <- load_data("populacao")
  pop <- filter(populacoes, tipo == "CENSO")
  fonte1_choices <- as.list(pop$caminho)
  names(fonte1_choices) <- pop$nome
  return(fonte1_choices)
}

get_fonte2_list <- function() {
  populacoes <- load_data("populacao")
  fonte1_choices <- as.list(populacoes$caminho)
  names(fonte1_choices) <- populacoes$nome
  return(fonte1_choices)
}


projecao_populacional_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      titlePanel("Dados de entrada"),
      selectInput(
        inputId = ns("fonte1"),
        label = strong("Fonte de dados do Primeiro Ano"),
        choices = get_fonte1_list(),
        selected = app_state$projecao$fonte1
      ),
      selectInput(
        inputId = ns("fonte2"),
        label = strong("Fonte de dados do Segundo Ano"),
        choices = get_fonte2_list(),
        selected = app_state$projecao$fonte2
      ),
      sliderInput(
        inputId = ns("ano"),
        strong("Fazer projeção até o ano:"),
        step = 1,
        min = 2021,
        max = 2040,
        value = app_state$projecao$modelar_ate
      ),
      actionButton(ns("rodar"), label = "Calcular projeção"),
    ),
    mainPanel(
          plotlyOutput(ns("grafico"))
      ),
  )
}

get_year_from_path <- function(path) {
  strtoi(gsub('[^0-9]', '', path))
}

rodar_projecao_populacional <- function (input) {
  fonte1 <- as.tibble(load_data(input$fonte1))
  fonte1 <- adicionar_proporcao_urbana_rural(fonte1)
  if (grepl(".*censo.*", input$fonte2)){
    # TODO: implementar para caso a fonte seja outro censo
    showNotification("Fonte de dados 2 não pode ser censo!", type="error")
    return()
  }
  fonte2 <- as.tibble(load_data(input$fonte2))
  ano1 <- get_year_from_path(input$fonte1)
  ano2 <- get_year_from_path(input$fonte2)
  consolidado <- junta_fontes_populacao(fonte1, fonte2)
  consolidado <- calcula_taxa_crescimento(consolidado, ano1, ano2)
  consolidado <- calcular_urbana_rural_fonte2(consolidado)
  app_state$projecao$resultado <<- calcula_projecao(consolidado, ano2, input$ano)
}

projecao_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$fonte2, {
      updateSliderInput(session, inputId = "ano", min = get_year_from_path(input$fonte2), value=app_state$projecao$modelar_ate)
    })

    resultado_projecao <- reactiveVal(app_state$projecao$resultado)

    output$grafico = renderPlotly({
      df <- drop_na(resultado_projecao())
      df <- group_by(df, tipo_populacao, ano)
      df <- summarize(df, populacao=sum(populacao),.groups="drop_last")
      p <- ggplot(data=df, aes(x=ano, y=populacao, fill=tipo_populacao))
      p <- p + geom_bar(position="dodge", stat="identity")
      p <- p + labs(x="tempo (anos)", y="População", fill="Classe")
      ggplotly(p)
    })

    observeEvent(input$rodar, {
      rlog::log_info('Running projeção populacional')
      rodar_projecao_populacional(input)
      resultado_projecao(app_state$projecao$resultado)
      save_projecao_state(input)
    })

  })
}

