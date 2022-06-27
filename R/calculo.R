#' Calcula a projeção populacional
#'
#' @param input estrutura de dados (`reactive`) que guarda os parâmetros da interface gráfica
#'
#' @return estado da aplicação (`list`) que guarda todos os resultado e parâmetros dos cálculos
#' @export
#'
#' @examples
#' \dontrun{
#' app_state <- rodar_projecao_populacional(app_state)
#' }
rodar_projecao_populacional <- function(state) {
  input <- state$input$projecao
  fonte1 <- dplyr::as_tibble(load_data(input$fonte1))
  fonte1 <- adicionar_proporcao_urbana_rural(fonte1)
  if (grepl(".*censo.*", input$fonte2)) {
    # TODO: implementar para caso a fonte seja outro censo
    showNotification("Fonte de dados 2 não pode ser censo!", type = "error")
    return()
  }
  fonte2 <- dplyr::as_tibble(load_data(input$fonte2))
  ano1 <- get_year_from_path(input$fonte1)
  ano2 <- get_year_from_path(input$fonte2)

  rlog::log_info("consolidando fontes")
  consolidado <- junta_fontes_populacao(fonte1, fonte2)

  rlog::log_info("calculando taxa de crescimento")
  consolidado <- calcula_taxa_crescimento(consolidado, ano1, ano2)

  rlog::log_info("calculando crescimento_urbano_rural")
  consolidado <- calcular_urbana_rural_fonte2(consolidado)

  rlog::log_info("calculando projecao")
  state$projecao <- calcula_projecao(consolidado, ano2, input$ano)
  return(state)
}


#' Title
#'
#' @param state
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' investimento_agua_esgoto(state)
#' }
investimento_agua_esgoto <- function(state) {
  rlog::log_info("pegando projecao populacional")
  projecao <- state$projecao
  rlog::log_info("pegando projecao populacional")
  input <- state$input$agua_esgoto
  rlog::log_info("demografico")
  state$agua_esgoto$demografico <- rsan::rodar_modulo_demografico(input, projecao)
  rlog::log_info("orcamentario")
  state$agua_esgoto$orcamentario <- rsan::rodar_modulo_orcamentario(input, state$agua_esgoto$demografico)
  rlog::log_info("financeiro")
  state$agua_esgoto$finaceiro <- rsan::rodar_modulo_financeiro(input, state$agua_esgoto$orcamentario)
  return(state)
}


#' Cálculo da Necessidade de Investimento para Drenagem Urbana
#'
#' @param input estrutura de dados (`reactive`) que guarda os parâmetros da interface gráfica
#'
#' @return estado da aplicação (`list`) que guarda todos os resultado e parâmetros dos cálculos
#' @export
#'
#' @examples
#' \dontrun{
#' state <- investimento_drenagem(state)
#' }
investimento_drenagem <- function(state) {
  input <- state$input$drenagem
  data("snis_ap")
  ano <- "2020"
  ano_inicial <- 2021
  ano_final <- 2033
  ano_corrente <- 2022
  depreciacao <- rsan::depreciacao_para_vida_util(input$deprec_drenagem)

  tabela <- snis_ap[[paste0("ano", ano)]]
  tabela <- densidade_urbana(tabela)
  tabela <- precipitacao(tabela)
  tabela <- coeficiente_pd(tabela)

  plano <- corrige_plano_drenagem("2021-12-01")
  plano <- prepara_regressao(plano, tabela)
  modelo <- regressao_drenagem(plano)
  tabela <- aplica_regressao_drenagem(tabela, modelo)
  tabela <- capacidade_instalada_drenagem(tabela)
  tabela <- rsan::calcula_reposicao_parcial(
    tabela,
    "capacidade_instalada",
    "investimento_expansao",
    "investimento_reposicao",
    ano_inicial,
    ano_final,
    ano_corrente,
    depreciacao
  )
  tabela <- investimento_total(tabela)
  tabela <- rsan::adiciona_pais(tabela)
  state$drenagem <- tabela
  return(state)
}

residuos_snis_fields <- c(
  "codigo_municipio", "POP_TOT", "POP_URB", "Estado",
  "CO164", "CO050", "CO119", "CS026",
  "CO054", "CO055", "CO056", "CO057", "CO058", "CO059",
  "CO063", "CO064", "CO065", "CO066", "CO067", "CO068",
  "CS001", "CS009", "CS050"
)
#' Title
#'
#' @param input
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' app_state <- investimento_residuos(app_state)
#' }
investimento_residuos <- function(state) {
  input <- state$input$residuos
  # parâmetros de entradar (TODO: colocar como parametros da funcao)
  ano <- input$ano
  valor_caminhao <- input$valor_caminhao
  valor_caminhao_bau <- input$valor_caminhao_bau
  preco_unidade_aterro <- tabela_preco_unidade_residuos(input, "aterro")
  vida_util_aterro <- input$vida_util_aterro
  preco_unidade_compostagem <- tabela_preco_unidade_residuos(input, "compostagem")
  vida_util_compostagem <- input$vida_util_compostagem
  preco_unidade_triagem <- tabela_preco_unidade_residuos(input, "triagem")
  vida_util_triagem <- input$vida_util_triagem
  ano_inicial <- 2021
  ano_final <- 2033
  ano_corrente <- 2022

  # Consolida os dados de Unidades de Processamento (SNIS-prestadores)
  rlog::log_info("residuos: carregando snis-rs data")
  data(snis_rs)
  compostagem <- rsan::quantidade_compostagem_municipio(snis_rs$ano2020)

  # Consolidação dos dados para classificação
  rlog::log_info("residuos: consolidando dados para classificação")
  tabela <- rsan::get_snis_data(input$snis, residuos_snis_fields)
  tabela <- rsan::consolida_populacao_snis(state$projecao, ano, tabela)
  tabela <- dplyr::left_join(tabela, compostagem, by = "codigo_municipio")
  tabela <- rsan::adiciona_pais(tabela)
  tabela <- rsan::adiciona_estado(tabela)
  tabela <- rsan::adiciona_regiao(tabela)

  # Dados coleta comum
  rlog::log_info("residuos: preparando dados para coleta comum")
  tabela <- atendimento_relativo_residuos(tabela)
  tabela <- geracao_residuos(tabela)
  tabela <- numero_caminhoes(tabela)
  tabela <- densidade_caminhoes(tabela)

  # Dados coleta seletiva
  rlog::log_info("residuos: preparando dados para coleta seletiva")
  tabela <- numero_caminhoes_bau(tabela)
  tabela <- densidade_caminhoes_bau(tabela)

  limites <- as.integer(c(0, 10e3, 30e3, 100e3, 250e3, 1e6, 4e6))
  tabela <- rsan::classifica_faixa_populacional(tabela, limites)
  antes <- tabela

  # Agrupamento por região e faixa populacional
  rlog::log_info("residuos: agrupando por região e faixa populacional")
  media_regiao <- rsan::media_por_estado_faixa(tabela, campo_estado = "regiao")
  soma_regiao <- rsan::soma_por_estado_faixa(tabela, campo_estado = "regiao")

  # Agrupamento por faixa
  rlog::log_info("residuos: agrupando somente por faixa populacional")
  media_faixa <- rsan::media_por_faixa(tabela)
  soma_faixa <- rsan::soma_por_faixa(tabela)
  soma_faixa <- rsan::densidade_caminhoes_bau(soma_faixa)

  # Preenchimentos

  # Agrupamento por estado e faixa populacional
  rlog::log_info("residuos: agrupando por estado e faixa populacional")
  media <- rsan::media_por_estado_faixa(tabela)
  tabela <- rsan::soma_por_estado_faixa(tabela)
  tabela$densidade_caminhoes <- media$densidade_caminhoes
  tabela$densidade_caminhoes_bau <- media$densidade_caminhoes_bau

  # Coleta comum
  rlog::log_info("residuos: investimento em coleta comum")
  tabela <- meta_plansab_residuo(tabela)
  tabela <- deficit_coleta_comum(tabela)
  tabela <- investimento_coleta_comum(tabela, valor_caminhao)
  tabela <- capacidade_instalada_coleta_comum(tabela, valor_caminhao)
  tabela <- rsan::calcula_reposicao_parcial(
    tabela,
    "capacidade_instalada_coleta_comum",
    "investimento_coleta_comum",
    "reposicao_coleta_comum",
    ano_inicial,
    ano_final,
    ano_corrente,
    rsan::depreciacao_para_vida_util(input$deprec_coleta_comum)
  )

  # Coleta seletiva
  rlog::log_info("residuos: investimento em coleta seletiva")
  tabela <- preenche_caminhoes_bau(tabela, soma_faixa, campo_densidade = "densidade_caminhoes_bau")
  tabela <- atendimento_relativo_coleta_seletiva(tabela)
  tabela <- deficit_coleta_seletiva(tabela)
  tabela <- investimento_coleta_seletiva(tabela, valor_caminhao_bau)
  tabela <- capacidade_instalada_coleta_seletiva(tabela, valor_caminhao_bau)
  tabela <- rsan::calcula_reposicao_parcial(
    tabela,
    "capacidade_instalada_coleta_seletiva",
    "investimento_coleta_seletiva",
    "reposicao_coleta_seletiva",
    ano_inicial,
    ano_final,
    ano_corrente,
    rsan::depreciacao_para_vida_util(input$deprec_coleta_seletiva)
  )

  # Compostagem
  rlog::log_info("residuos: investimento em compostagem")
  tabela <- demanda_compostagem(tabela)
  tabela <- preco_unidade_faixa(tabela, preco_unidade_compostagem)
  tabela <- investimento_compostagem(tabela, vida_util_compostagem)
  tabela <- capacidade_instalada_compostagem(tabela, vida_util_compostagem)
  tabela <- rsan::calcula_reposicao_parcial(
    tabela,
    "capacidade_instalada_compostagem",
    "investimento_compostagem",
    "reposicao_compostagem",
    ano_inicial,
    ano_final,
    ano_corrente,
    rsan::depreciacao_para_vida_util(input$deprec_compostagem)
  )

  # Aterro
  rlog::log_info("residuos: investimento em aterro")
  tabela <- preco_unidade_faixa(tabela, preco_unidade_aterro)
  tabela <- investimento_aterro(tabela, vida_util_aterro)
  tabela <- capacidade_instalada_aterro(tabela, vida_util_aterro)
  tabela <- rsan::calcula_reposicao_parcial(
    tabela,
    "capacidade_instalada_aterro",
    "investimento_aterro",
    "reposicao_aterro",
    ano_inicial,
    ano_final,
    ano_corrente,
    rsan::depreciacao_para_vida_util(input$deprec_aterro)
  )

  # Triagem
  rlog::log_info("residuos: investimento em triagem")
  tabela <- demanda_triagem(tabela)
  tabela <- preco_unidade_faixa(tabela, preco_unidade_triagem)
  tabela <- investimento_triagem(tabela, vida_util_triagem)
  tabela <- capacidade_instalada_triagem(tabela, vida_util_triagem)
  tabela <- rsan::calcula_reposicao_parcial(
    tabela,
    "capacidade_instalada_triagem",
    "investimento_triagem",
    "reposicao_triagem",
    ano_inicial,
    ano_final,
    ano_corrente,
    rsan::depreciacao_para_vida_util(input$deprec_triagem)
  )

  rlog::log_info("residuos: totalizando investimentos")
  tabela <- investimento_residuos_total(tabela)
  state$residuos <- tabela
  return(state)
}

#' Roda todos os modelos de cálculo de investimento
#'
#' @param state
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' rodar_modelo(state)
#' }
rodar_modelo <- function(state) {
  rlog::log_info("iniciando módulo de projecao populacional")
  state <- rodar_projecao_populacional(state)

  rlog::log_info("iniciando módulo água e esgoto")
  state <- investimento_agua_esgoto(state)

  rlog::log_info("iniciando módulo drenagem")
  state <- investimento_drenagem(state)

  rlog::log_info("iniciando módulo residuos")
  state <- investimento_residuos(state)

  rlog::log_info("rodada terminada")
  return(state)
}
