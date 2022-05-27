snis_fields = c("codigo_municipio", "POP_TOT", "Estado", "AG001", "AG005", "AG010", "AG026", "ES001", "ES004", "ES026")

get_populacao <- function(ano, tipo){
  year <- ano
  pop <- dplyr::filter(app_state$projecao$resultado, tipo_populacao == tipo & ano == year)
  return(pop)
}

get_snis_data <- function(snis, fields){
  df <- load_data(snis)
  df <- select(df, fields)
  return(df)
}

add_density <- function(data) {
  df <- mutate(data,
               DEN_EXT_AA= AG005*1000/AG001,
               DEN_VOL_AA= AG010*1000/AG001,
               DEN_EXT_ES= ES004*1000/ES001
               )
  return(df)
}

fill_missing_density <- function(density) {
  fields <- c("DEN_EXT_AA","DEN_VOL_AA","DEN_EXT_ES")
  for (i in fields) {
    density[is.infinite(unlist(density[ , i])), i] <- NA
    density[is.nan(unlist(density[ , i])), i] <- NA
    density[ , i][density[ , i] == 0] <- NA

    Q3 <- quantile(unlist(density[ , i]), 0.75, na.rm = TRUE)
    IQR <- IQR(unlist(density[ , i]), na.rm = TRUE)
    density[ , i][density[ , i] >= Q3 + 1.5 * IQR] <- NA

    fit <- lm(log(get(i)) ~ 0 + I(POP_TOT/1000000) + Estado, data = density)

    density <- density %>%
      mutate(pred = exp(predict(fit, .))) %>%
      mutate_(.dots = setNames(list(paste0("ifelse(is.na(", i, "), pred,", i, ")")), i))

    density <- density[ , -14]
  }
  return(density)
}

consolida_geografico <- function(df1, df2) {
  return(full_join(
    df1,
    df2,
    by = "codigo_municipio"
  ))
}
rodar_modulo_demografico <- function(input){
  ano <- input$ano
  populacao_total <- get_populacao(ano, "total")
  populacao_urbana <- get_populacao(ano, "urbana")
  snis_data <- get_snis_data(input$snis, snis_fields)
  snis_data <- add_density(snis_data)
  snis_data <- fill_missing_density(snis_data)
  consolidado <- consolida_geografico(populacao_total, snis_data)
  app_state$modulo_demografico$resutado <<- consolidado

}

agua_esgoto_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    resultado_modelo_demografico <- reactiveVal(app_state$modulo_demografico$resultado)
    observeEvent(input$rodar, {
      rlog::log_info('Running modelo demografico')
      rodar_modulo_demografico(input)
      resultado_modelo_demografico(app_state$modulo_demografico$resultado)
      save_modulo_demografico_state(input)
    })

    output$plot1 <- renderPlot({
      plot(1:input$ano)
    })
  })

}
