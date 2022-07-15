#' Calcula a projeção populacional
#'
#' @param state estrutura de dados (`list`) que guarda o estado atual da aplicação
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

  rlog::log_info("projecao: consolidando fontes")
  consolidado <- junta_fontes_populacao(fonte1, fonte2)

  rlog::log_info("projecao: calculando taxa de crescimento")
  consolidado <- calcula_taxa_crescimento(consolidado, ano1, ano2)

  rlog::log_info("projecao: calculando crescimento_urbano_rural")
  consolidado <- calcular_urbana_rural_fonte2(consolidado)

  rlog::log_info("projecao: calculando projecao")
  state$projecao <- calcula_projecao(consolidado, ano2, state$input$geral$ano)
  rlog::log_info(sprintf(
    "projecao: dimensao %s x %s",
    nrow(state$projecao), ncol(state$projecao)
  ))
  return(state)
}


estimar_parametros_drenagem <- function() {
  plano <- corrige_plano_drenagem("2021-12-01")
  plano <- prepara_regressao(plano, tabela)
  modelo <- regressao_drenagem(plano)
}

#' Cálculo da Necessidade de Investimento para Drenagem Urbana
#'
#' @param state estrutura de dados (`list`) que guarda o estado atual da aplicação
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
  data("snis_ap", package = "rsan")
  ano <- get_year_from_path(input$snis_ap)
  ano_inicial <- 2021
  ano_final <- 2033
  ano_corrente <- 2022
  depreciacao <- rsan::depreciacao_para_vida_util(input$deprec_drenagem)

  tabela <- snis_ap[[paste0("ano", ano)]]
  tabela <- rsan:::densidade_urbana(tabela)
  tabela <- rsan:::precipitacao(tabela)
  tabela <- rsan:::adiciona_indices_drenagem(tabela)
  tabela <- rsan:::adiciona_projecao_populacao(state$projecao, ano_final, tabela)
  tabela <- rsan:::area_urbana(tabela)

  if (input$modo == 1) { # Investimento per capita constante
    tabela <- investimento_constante(tabela, input$investimento_per_capita)
  } else { # Investimento per capita por regressão
    tabela <- aplica_regressao_multipla_drenagem(tabela, input)
  }
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
  tabela <- rsan:::investimento_cadastro(tabela, input$custo_cadastro)
  tabela <- investimento_total_drenagem(tabela)
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

#' Investimento em residuos
#'
#' @param state estrutura de dados (`list`) que guarda o estado atual da aplicação
#'
#' @return estado da aplicação (`list`) que guarda todos os resultado e parâmetros dos cálculos
#' @export
#'
#' @examples
#' \dontrun{
#' app_state <- investimento_residuos(app_state)
#' }
investimento_residuos <- function(state) {
  ano <- state$input$geral$ano
  input <- state$input$residuos
  # parâmetros de entradar (TODO: colocar como parametros da funcao)
  valor_caminhao <- input$valor_caminhao
  valor_caminhao_bau <- input$valor_caminhao_bau
  custo_transbordo <- input$custo_transbordo

  preco_unidade_aterro <- tabela_preco_unidade_residuos(input, "aterro")
  vida_util_aterro <- input$vida_util_aterro
  preco_unidade_compostagem <- tabela_preco_unidade_residuos(input, "compostagem")
  vida_util_compostagem <- input$vida_util_compostagem
  preco_unidade_triagem <- tabela_preco_unidade_residuos(input, "triagem")
  vida_util_triagem <- input$vida_util_triagem

  ano_inicial <- 2021
  ano_final <- ano
  ano_corrente <- 2022
  cenario_regionalizacao <- "B"

  # Consolida os dados de Unidades de Processamento (SNIS-prestadores)
  rlog::log_info("residuos: carregando snis-rs data")
  data(snis_rs, package = "rsan")
  compostagem <- rsan::quantidade_compostagem_municipio(snis_rs$ano2020)

  # Consolidação dos dados para classificação
  rlog::log_info("residuos: consolidando dados para classificação")
  tabela <- rsan::get_snis_data(input$snis, residuos_snis_fields)
  tabela <- rsan::adiciona_projecao_populacao(state$projecao, ano, tabela)
  tabela <- dplyr::left_join(tabela, compostagem, by = "codigo_municipio")
  tabela <- rsan::adiciona_pais(tabela)
  tabela <- rsan::adiciona_estado(tabela)
  tabela <- rsan::adiciona_regiao(tabela)

  # Dados coleta indiferenciada
  rlog::log_info("residuos: preparando dados para coleta indiferenciada")
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

  # Agrupamento por faixa nao usado
  rlog::log_info("residuos: agrupando somente por faixa populacional")
  media_faixa <- rsan::media_por_faixa(tabela)
  soma_faixa <- rsan::soma_por_faixa(tabela)
  soma_faixa <- rsan::densidade_caminhoes_bau(soma_faixa)

  # Preenchimentos

  # Agrupamento por estado e faixa populacional
  rlog::log_info("residuos: agrupando por estado e faixa populacional")
  media <- rsan::media_por_estado_faixa(tabela)
  conta <- rsan::conta_municipios_por_estado_faixa(tabela, campo_estado = "estado")

  tabela <- rsan:::soma_por_estado_faixa(tabela)
  tabela$densidade_caminhoes <- media$densidade_caminhoes
  tabela$densidade_caminhoes_bau <- media$densidade_caminhoes_bau
  tabela$numero_municipios <- conta$numero_municipios
  tabela <- rsan:::cria_faixas_vazias(tabela)
  # Fim do preenchimento

  # Transbordo
  tabela <- rsan:::demanda_transbordo(tabela)
  tabela <- rsan:::regionaliza_transbordo(tabela, cenario_regionalizacao)
  tabela <- rsan:::investimento_transbordo(tabela, custo_transbordo)

  # Coleta indiferenciada
  rlog::log_info("residuos: investimento em coleta indiferenciada")
  tabela <- meta_plansab_residuo(tabela)
  tabela <- deficit_coleta_indiferenciada(tabela)
  tabela <- investimento_coleta_indiferenciada(tabela, valor_caminhao)
  tabela <- capacidade_instalada_coleta_indiferenciada(tabela, valor_caminhao)
  tabela <- rsan:::calcula_reposicao_parcial(
    tabela,
    "capacidade_instalada_coleta_indiferenciada",
    "investimento_coleta_indiferenciada",
    "reposicao_coleta_indiferenciada",
    ano_inicial,
    ano_final,
    ano_corrente,
    rsan::depreciacao_para_vida_util(input$deprec_coleta_indiferenciada)
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
  tabela <- regionaliza_compostagem(tabela, cenario_regionalizacao)
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
  tabela <- demanda_aterro(tabela, vida_util_aterro)
  tabela <- regionaliza_aterro(tabela, cenario_regionalizacao)
  tabela <- investimento_aterro(tabela)
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
  tabela <- regionaliza_triagem(tabela, cenario_regionalizacao)
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

#' Consolida as necessidade de investimento em saneamento
#'
#' @param state estrutura de dados (`list`) que guarda o estado atual da aplicação
#'
#' @return estado da aplicação (`list`) que guarda todos os resultado e parâmetros dos cálculos
#' @export
#'
#' @examples
#' \dontrun{
#' state <- consolida_investimentos(state)
#' }
consolida_investimentos <- function(state) {
  vars <- c("codigo_municipio", "investimento_total")
  by <- "codigo_municipio"
  tipos <- c("agua", "esgoto", "drenagem")
  state$geral <- dplyr::tibble(
    codigo_municipio = character(),
    investimento_total = double()
  )
  col_names <- c()
  for (tipo in tipos) {
    suffix <- paste0("_", tipo)
    col_names <- c(col_names, paste0("investimento_total", suffix))
    tbl <- dplyr::select(state[[tipo]], dplyr::all_of(vars))
    state$geral <- dplyr::full_join(
      state$geral, tbl,
      suffix = c("", suffix),
      by = by
    )
  }
  state$geral <- dplyr::mutate(
    state$geral,
    investimento_total = rowSums(
      dplyr::select(state$geral, dplyr::all_of(col_names)),
      na.rm = TRUE
    )
  )
  state$geral <- rsan:::adiciona_pais(state$geral)
  state$geral <- rsan:::adiciona_estado(state$geral)
  state$geral <- rsan:::adiciona_regiao(state$geral)
  return(state)
}

#' Roda todos os modelos de cálculo de investimento
#'
#' @param state estrutura de dados (`list`) que guarda o estado atual da aplicação
#'
#' @return estado da aplicação (`list`) que guarda todos os resultado e parâmetros dos cálculos
#' @export
#'
#' @examples
#' \dontrun{
#' rodar_modelo(state)
#' }
rodar_modelo <- function(state) {
  rlog::log_info("iniciando módulo de projecao populacional")
  state <- rodar_projecao_populacional(state)

  rlog::log_info("água: iniciando módulo")
  state <- rsan:::investimento_agua(state)

  rlog::log_info("esgoto: iniciando módulo")
  state <- rsan:::investimento_esgoto(state)

  rlog::log_info("drenagem: iniciando módulo")
  state <- investimento_drenagem(state)

  rlog::log_info("residuos: iniciando módulo")
  state <- investimento_residuos(state)

  rlog::log_info("consolidando investimentos")
  state <- consolida_investimentos(state)

  rlog::log_info("rodada terminada")
  return(state)
}
