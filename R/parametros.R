#' Salva os parametros da interface gráfica no estado da aplicação
#'
#'
#' @param state estrutura de dados (`list`) que guarda o estado atual da aplicação
#' @param input estrutura de dados (`reactive`) que guarda os parâmetros da interface gráfica
#' @param name um `character` como nome do campo para ser guardado o dados do parâmetro `input`
#'
#' @return a estrutura de dados (`list`) do novo estado atual da aplicação
#' @export
#'
#' @examples
#' \dontrun{
#' salva_parametros_projecao(state, input, "teste")
#' }
salva_parametros <- function(state, input, name) {
  if (is.null(state[["input"]])) {
    rlog::log_info("Sem parâmetros anteriores")
    state$input <- list()
  }
  params <- shiny::isolate(shiny::reactiveValuesToList(input))
  params <- remove_shiny_classes(params)
  state$input[[name]] <- params
  save_state(state)
  return(state)
}

#' Salva os parametros do cálculo da projeção populacional
#'
#'
#' @param state estrutura de dados (`list`) que guarda o estado atual da aplicação
#' @param input estrutura de dados (`reactive`) que guarda os parâmetros da interface gráfica
#'
#' @return a estrutura de dados (`list`) do novo estado atual da aplicação
#' @export
#'
#' @examples
#' \dontrun{
#' salva_parametros_projecao(state, input)
#' }
salva_parametros_projecao <- function(state, input) {
  projecao_state <-
    list(
      fonte1 = input$fonte1,
      fonte2 = input$fonte2,
      modelar_ate = input$ano
    )
  if (is.null(state[["input"]])) {
    rlog::log_info("Sem parâmetros anteriores")
    state$input <- list()
  }
  state$input$projecao <- projecao_state
  save_state(state)
  return(state)
}


#' Salva os parâmetros do cálculo de investimento em água e esgoto
#'
#'
#' @param state estrutura de dados (`list`) que guarda o estado atual da aplicação
#' @param input estrutura de dados (`reactive`) que guarda os parâmetros da interface gráfica
#'
#' @return a estrutura de dados (`list`) do novo estado atual da aplicação
#' @export
#'
#' @examples
#' \dontrun{
#' salva_parametros_agua_esgoto(state, input)
#' }
salva_parametros_agua_esgoto <- function(state, input) {
  agua_esgoto <- list(
    snis = input$snis,
    sinapi = input$sinapi,
    ano = input$ano,
    meta_agua = input$meta_agua,
    meta_esgoto = input$meta_esgoto,
    proporcao = input$proporcao,
    vida_util = input$vida_util,
    fator_servicos = input$fator_servicos,
    fator_materiais = input$fator_materiais,
    fator_composicao = input$fator_composicao,
    fator_insumo = input$fator_insumo,
    perda_agua = input$perda_agua
  )
  if (is.null(state[["input"]])) {
    rlog::log_info("Sem parâmetros anteriores")
    state$input <- list()
  }
  state$input$agua_esgoto <- agua_esgoto
  save_state(state)
  return(state)
}

projecao <- list(
  fonte1 = "populacao_censo_2010",
  fonte2 = "populacao_estimada_2021",
  ano = 2033
)

agua_esgoto <- list(
  snis = "snis_2020",
  ano = 2033,
  meta_agua = 99,
  meta_esgoto = 90,
  proporcao = 80,
  sinapi = "sinapi_202112",
  perda_agua = 25,
  snis = "snis_2020",
  vida_util = 30
)

valores_aterro <- c(
  43.24,
  19.45,
  23.78,
  23.78,
  19.45,
  8.65,
  8.65
)

valores_compostagem <- c(
  18.37,
  5.4,
  6.92,
  6.92,
  11.89,
  8,
  8
)

valores_triagem <- c(
  70.25,
  34.58,
  37.82,
  37.82,
  23.78,
  12.97,
  12.97
)

residuos <- list(
  ano = 2033,
  snis = "snis_2020",
  # coleta comum
  valor_caminhao = 484709.23,
  deprec_coleta_comum = 10,
  # coleta seletiva
  valor_caminhao_bau = 336490.00,
  deprec_coleta_seletiva = 10,
  # aterro
  vida_util_aterro = 20,
  deprec_aterro = 5,
  # compostagem
  vida_util_compostagem = 20,
  deprec_compostagem = 5,
  # triagem
  deprec_triagem = 20,
  vida_util_triagem = 20
)

drenagem <- list(
  snis_ap = "snis_2020",
  deprec_drenagem = 2
)

#' Valores padrões da aplicação
#'
#' Retorna os valores padrões dos parâmetros de todos os módulos do cálculo de investimento.
#'
#' @return estrutura de dados que guarda os valores padrões da interface gráfica
#' @export
#'
#' @examples
#' get_default_input()
get_default_input <- function() {
  default_input <- list(
    projecao = projecao,
    agua_esgoto = agua_esgoto,
    residuos = residuos,
    drenagem = drenagem
  )
  default_input$residuos <- adiciona_preco_unidade_residuos(default_input$residuos, "aterro", valores_aterro)
  default_input$residuos <- adiciona_preco_unidade_residuos(default_input$residuos, "compostagem", valores_compostagem)
  default_input$residuos <- adiciona_preco_unidade_residuos(default_input$residuos, "triagem", valores_triagem)
  return(default_input)
}
