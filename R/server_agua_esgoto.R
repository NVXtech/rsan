snis_fields <- c("codigo_municipio", "POP_TOT", "Estado", "AG001", "AG005", "AG010", "AG026", "ES001", "ES004", "ES026")

get_populacao <- function(ano, tipo) {
  year <- ano
  pop <- dplyr::filter(app_state$projecao$resultado, tipo_populacao == tipo & ano == year)
  return(pop)
}

get_snis_data <- function(snis, fields) {
  df <- load_data(snis)
  df <- select(df, fields)
  return(df)
}

add_density <- function(data) {
  df <- mutate(data,
    densidade_distribuicao_agua = AG005 * 1000 / AG001,
    densidade_producao_agua = AG010 * 1000 / AG001,
    densidade_coleta_esgoto = ES004 * 1000 / ES001
  )
  return(df)
}

fill_missing_density <- function(density) {
  fields <- c("densidade_distribuicao_agua", "densidade_producao_agua", "densidade_coleta_esgoto")
  for (i in fields) {
    density[is.infinite(unlist(density[, i])), i] <- NA
    density[is.nan(unlist(density[, i])), i] <- NA
    density[, i][density[, i] == 0] <- NA

    Q3 <- quantile(unlist(density[, i]), 0.75, na.rm = TRUE)
    IQR <- IQR(unlist(density[, i]), na.rm = TRUE)
    density[, i][density[, i] >= Q3 + 1.5 * IQR] <- NA

    fit <- lm(log(get(i)) ~ 0 + I(POP_TOT / 1000000) + Estado, data = density)

    density <- density %>%
      mutate(pred = exp(predict(fit, .))) %>%
      mutate_(.dots = setNames(list(paste0("ifelse(is.na(", i, "), pred,", i, ")")), i))

    density <- density[, -14]
  }
  return(density)
}

join_geografico <- function(populacao_total, populacao_urbana, snis) {
  df <- full_join(
    populacao_total,
    populacao_urbana,
    by = "codigo_municipio",
    suffix = c("_total", "_urbana")
  )
  df <- full_join(
    df,
    snis,
    by = "codigo_municipio"
  )
  df <- select(df, -c("tipo_populacao_total", "tipo_populacao_urbana", "ano_total", "ano_urbana"))
  return(df)
}

calculate_geografico <- function(df, meta_agua, meta_esgoto, proporcao) {
  df$AG001[is.na(df$AG001)] <- 0
  df$AG026[is.na(df$AG026)] <- 0
  df$ES001[is.na(df$ES001)] <- 0
  df$ES026[is.na(df$ES026)] <- 0
  df <- mutate(
    df,
    deficit_agua_total = pmax(populacao_total - AG001, 0.0),
    deficit_agua_urbana = pmax(populacao_urbana - AG026, 0.0),

    deficit_esgoto_total = pmax(populacao_total - ES001, 0.0),
    deficit_esgoto_urbana = pmax(populacao_urbana - ES026, 0.0),

    deficit_agua_rural = pmax(deficit_agua_total - deficit_agua_urbana, 0.0),
    deficit_esgoto_rural = pmax(deficit_esgoto_total - deficit_esgoto_urbana, 0.0),

    demanda_distribuicao_agua = meta_agua/100.0 * deficit_agua_urbana * densidade_distribuicao_agua,
    demanda_producao_agua = meta_agua/100.0 * deficit_agua_urbana * densidade_producao_agua,

    densidade_tratamento_esgoto = proporcao/100.0 * densidade_producao_agua,

    demanda_coleta_esgoto = meta_esgoto/100.0 * deficit_esgoto_urbana * densidade_coleta_esgoto,
    demanda_tratamento_esgoto = meta_esgoto/100.0 * deficit_esgoto_urbana * densidade_tratamento_esgoto,
    )
  df <- select(
    df,
    codigo_municipio,
    demanda_distribuicao_agua,
    demanda_producao_agua,
    demanda_coleta_esgoto,
    demanda_tratamento_esgoto
  )
}

rodar_modulo_demografico <- function(input) {
  ano <- input$ano
  populacao_total <- get_populacao(ano, "total")
  populacao_urbana <- get_populacao(ano, "urbana")
  snis_data <- get_snis_data(input$snis, snis_fields)
  snis_data <- add_density(snis_data)
  snis_data <- fill_missing_density(snis_data)
  consolidado <- join_geografico(populacao_total, populacao_urbana, snis_data)
  consolidado <- calculate_geografico(consolidado, input$meta_agua, input$meta_esgoto, input$proporcao)
  app_state$modulo_demografico$resultado <<- consolidado
}

agua_esgoto_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    resultado_modelo_demografico <- reactiveVal(app_state$modulo_demografico$resultado)

    observeEvent(input$rodar, {
      rlog::log_info("Running modelo demografico")
      rodar_modulo_demografico(input)
      resultado_modelo_demografico(app_state$modulo_demografico$resultado)
      save_modulo_demografico_state(input)
    })

    output$grafico_total <- renderPlotly({
      df <- drop_na(resultado_modelo_demografico())
      df <- gather(df, "tipo_demanda", "demanda", -codigo_municipio)
      df <- group_by(df, tipo_demanda)
      df <- summarize(df, demanda=sum(demanda),.groups="drop_last")
      p <- ggplot(data=df, aes(x=tipo_demanda, y=demanda))
      p <- p + geom_bar(position="dodge", stat="identity")
      #p <- p + labs(x="tempo (anos)", y="População")
      ggplotly(p)
    })

    output$tabela = DT::renderDataTable({
      resultado_modelo_demografico()
    })
  })
}
