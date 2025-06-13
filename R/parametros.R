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
  geral <- list(
    ano = 2033,
    ano_corrente = 2023
  )

  taxa_igp <- fator_correcao_incc(
    as.Date("2018-07-01"),
    as.Date(paste0(geral$ano_corrente, "-12-31"))
  )

  taxa_residuos <- fator_correcao_incc(
    as.Date("2021-12-31"),
    as.Date(paste0(geral$ano_corrente, "-12-31"))
  )
  taxa_drenagem <- fator_correcao_ipca(
    as.Date("2021-12-31"),
    as.Date(paste0(geral$ano_corrente, "-12-31"))
  )
  taxa_veiculos <- fator_correcao_ipca(
    as.Date("2021-12-31"),
    as.Date(paste0(geral$ano_corrente, "-12-31"))
  )

  projecao <- list(
    fonte1 = "censo_2010",
    fonte2 = "censo_2022"
  )

  agua <- list(
    fonte_nome = "sinisa",
    fonte_ano = 2023,
    atendimento = "censo",
    atendimento_ano = 2022,
    sinapi = "sinapi_202212",
    meta_agua = 99,
    perda_agua = 25,
    fator_servicos = 26,
    fator_materiais = 18,
    fator_composicao = 26,
    fator_insumo = 18,
    vida_util = 50,
    custo_rural_individual = 10682.16 * taxa_igp,
    custo_rural_individual_sem = 14682.16 * taxa_igp
  )

  esgoto <- list(
    fonte_nome = "sinisa",
    fonte_ano = 2023,
    atendimento = "censo",
    atendimento_ano = 2022,
    sinapi = "sinapi_202212",
    meta_esgoto = 90,
    proporcao = 80,
    fator_servicos = 26,
    fator_materiais = 18,
    fator_composicao = 26,
    fator_insumo = 18,
    vida_util = 50,
    custo_individual_esgoto_faixa1 = 3318.83 * taxa_igp,
    custo_individual_esgoto_faixa2 = 3487.6 * taxa_igp,
    custo_individual_esgoto_faixa3 = 3656.36 * taxa_igp,
    custo_individual_esgoto_faixa4 = 3860.22 * taxa_igp,
    custo_individual_esgoto_faixa5 = 4300.83 * taxa_igp,
    custo_individual_esgoto_faixa6 = 4641.6 * taxa_igp,
    custo_individual_esgoto_faixa7 = 4977.54 * taxa_igp,
    custo_individual_esgoto_faixa8 = 5638.27 * taxa_igp,
    custo_individual_esgoto_faixa9 = 1884.98 * taxa_igp,
    custo_individual_esgoto_faixa10 = 1988.74 * taxa_igp
  )

  valores_aterro <- c(
    43.24 * taxa_residuos,
    19.45 * taxa_residuos,
    23.78 * taxa_residuos,
    23.78 * taxa_residuos,
    19.45 * taxa_residuos,
    8.65 * taxa_residuos,
    8.65 * taxa_residuos
  )

  valores_compostagem <- c(
    18.37 * taxa_residuos,
    5.4 * taxa_residuos,
    6.92 * taxa_residuos,
    6.92 * taxa_residuos,
    11.89 * taxa_residuos,
    8 * taxa_residuos,
    8 * taxa_residuos
  )

  valores_triagem <- c(
    70.25 * taxa_residuos,
    34.58 * taxa_residuos,
    37.82 * taxa_residuos,
    37.82 * taxa_residuos,
    23.78 * taxa_residuos,
    12.97 * taxa_residuos,
    12.97 * taxa_residuos
  )

  residuos <- list(
    fonte_nome = "sinisa",
    fonte_ano = 2023,
    atendimento = "censo",
    antendimento_ano = 2022,

    # coleta comum
    valor_caminhao = 484709.23 * taxa_veiculos,
    deprec_coleta_indiferenciada = 10,
    # coleta seletiva
    valor_caminhao_bau = 336490.00 * taxa_veiculos,
    deprec_coleta_seletiva = 10,
    # aterro
    vida_util_aterro = 20,
    deprec_aterro = 5,
    # compostagem
    vida_util_compostagem = 20,
    deprec_compostagem = 5,
    # triagem
    deprec_triagem = 20,
    vida_util_triagem = 20,
    # regionalização
    cenario_regionalizacao = "A", # A, B ou C
    # transbordo
    custo_transbordo = 857816.82 * taxa_residuos
  )

  drenagem <- list(
    fonte_nome = "sinisa",
    fonte_ano = 2023,
    modo = 2,
    deprec_drenagem = 2,
    investimento_per_capita = 10000,
    custo_cadastro = 7738.89 * taxa_drenagem,
    peso_pluviometria = 0.063933104088543,
    peso_densidade = -0.189155004725778,
    peso_fisicas = 3477.79720206452,
    peso_infraestrutura = 519.474326911018,
    peso_constante = 791.359914329392
  )

  default_input <- list(
    geral = geral,
    projecao = projecao,
    agua = agua,
    esgoto = esgoto,
    residuos = residuos,
    drenagem = drenagem
  )
  default_input$residuos <- adiciona_preco_unidade_residuos(
    default_input$residuos,
    "aterro",
    valores_aterro
  )
  default_input$residuos <- adiciona_preco_unidade_residuos(
    default_input$residuos,
    "compostagem",
    valores_compostagem
  )
  default_input$residuos <- adiciona_preco_unidade_residuos(
    default_input$residuos,
    "triagem",
    valores_triagem
  )
  return(default_input)
}
