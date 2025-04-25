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
  fonte1 <- load_ibge(input$fonte1)
  fonte2_nao_censo <- !grepl(".*censo.*", input$fonte2)
  fonte2 <- load_ibge(input$fonte2)
  if (fonte2_nao_censo) {
    fonte2 <- preenche_situacao(fonte1, fonte2)
  }
  ano_inicial <- state$input$geral$ano_corrente
  ano_final <- state$input$geral$ano
  ano_fonte1 <- nome_para_ano(input$fonte1)
  ano_fonte2 <- nome_para_ano(input$fonte2)
  state$input$geral$ano_populacao_fonte1 <- ano_fonte1
  state$input$geral$ano_populacao_fonte2 <- ano_fonte2

  rlog::log_info("projecao: consolidando fontes")
  consolidado <- junta_fontes_populacao(fonte1, fonte2)
  rlog::log_info("projecao: calculando taxa de crescimento")
  consolidado <- calcula_taxa_crescimento(consolidado, ano_fonte1, ano_fonte2)
  state$taxas_projecao <- consolidado

  rlog::log_info("projecao: calculando projecao")
  state$projecao <- calcula_projecao(
    consolidado, ano_inicial, ano_final, ano_fonte1
  )

  rlog::log_info(sprintf(
    "projecao: dimensao %s x %s",
    nrow(state$projecao), ncol(state$projecao)
  ))
  return(state)
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
  ano_inicial <- nome_para_ano(input$snis_ap) + 1
  ano_final <- state$input$geral$ano
  ano_corrente <- state$input$geral$ano_corrente
  depreciacao <- depreciacao_para_vida_util(input$deprec_drenagem)
  rlog::log_info(sprintf("drenagem anoi=%s anof=%s", ano_inicial, ano_final))
  tabela <- base_municipios()
  tabela <- dplyr::left_join(
    tabela,
    carrega_base_calculo("aguas_pluviais", input$fonte_nome, input$fonte_ano),
    by = "codigo_municipio"
  )
  tabela <- adiciona_populacao_corrente(state$projecao, ano_corrente, tabela)
  tabela <- adiciona_projecao_populacao(state$projecao, ano_final, tabela)
  tabela <- area_urbana(tabela)
  tabela <- densidade_urbana(tabela)
  tabela <- precipitacao(tabela)
  tabela <- adiciona_indices_drenagem(tabela)
  tabela <- capacidade_instalada_drenagem(tabela)

  if (input$modo == 1) { # Investimento per capita constante
    tabela <- investimento_constante(tabela, input$investimento_per_capita)
  } else { # Investimento per capita por regressão
    tabela <- aplica_regressao_multipla_drenagem(tabela, input)
  }
  tabela <- calcula_reposicao_parcial(
    tabela,
    "capacidade_instalada",
    "investimento_expansao",
    "investimento_reposicao",
    ano_inicial,
    ano_final,
    ano_corrente,
    depreciacao
  )
  tabela <- investimento_cadastro(tabela, input$custo_cadastro)
  tabela <- investimento_total_drenagem(tabela)
  rlog::log_info("drenagem: removendo não críticos")
  tabela <- remove_nao_criticos(tabela)
  state$drenagem <- tabela
  state$necessidade <- dplyr::bind_rows(
    tbl_longa_investimento_drenagem(tabela),
    state$necessidade
  )
  return(state)
}


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

  valor_caminhao <- input$valor_caminhao
  valor_caminhao_bau <- input$valor_caminhao_bau
  custo_transbordo <- input$custo_transbordo

  preco_unidade_aterro <- tabela_preco_unidade_residuos(input, "aterro")
  vida_util_aterro <- input$vida_util_aterro
  preco_unidade_compostagem <- tabela_preco_unidade_residuos(input, "compostagem")
  vida_util_compostagem <- input$vida_util_compostagem
  preco_unidade_triagem <- tabela_preco_unidade_residuos(input, "triagem")
  vida_util_triagem <- input$vida_util_triagem

  ano_inicial <- input$fonte_ano + 1
  ano_final <- ano
  rlog::log_info(sprintf("residuos anoi=%s anof=%s", ano_inicial, ano_final))
  ano_corrente <- state$input$geral$ano_corrente
  cenario_regionalizacao <- "A"

  # Consolidação dos dados para classificação
  rlog::log_info("residuos: consolidando dados para classificação")
  tabela <- base_municipios()
  tabela <- dplyr::left_join(
    tabela,
    carrega_base_calculo("residuos", input$fonte_nome, input$fonte_ano),
    by = "codigo_municipio"
  )
  if (input$atendimento == "censo") {
    tabela <- adiciona_atendimento_censo_2022(tabela, "residuos")
  }
  if (input$atendimento == "pnadc") {
    tabela <- adiciona_atendimento_pnadc(tabela, "residuos", input$atendimento_ano)
  }
  tabela <- adiciona_populacao_corrente(state$projecao, ano_corrente, tabela)
  tabela <- adiciona_projecao_populacao(state$projecao, ano, tabela)
  tabela <- adiciona_tipo_disposicao(tabela)
  tabela <- adiciona_classificacao_litoranea(tabela)

  # Densidade Veicular (per capita)
  tabela <- densidade_caminhoes_compactadores(tabela)
  # densidade_caminhoes_bau(tabela)

  # Classificação por faixas populacionais
  rlog::log_info("residuos: classificando por faixa populacional")
  limites <- as.integer(c(0, 10e3, 30e3, 100e3, 250e3, 1e6, 4e6))
  tabela <- classifica_faixa_populacional(tabela, limites)

  # Preenchimentos
  rlog::log_info("residuos: preenchendo dados - coleta seletiva")
  tabela <- mascara_coleta_seletiva(tabela)
  tabela <- preenche_atendimento_coleta_seletiva(tabela)
  rlog::log_info("residuos: preenchendo dados - coleta indiferenciada")
  tabela <- preenche_atendimento_coleta_indiferenciada(tabela)
  rlog::log_info("residuos: preenchendo dados - geracao de residuos")
  tabela <- preenche_taxa_geracao_residuos(tabela)
  tabela <- preenche_quantidade_coletada(tabela)
  tabela <- geracao_residuos(tabela)
  tabela <- disposicao_inadequada(tabela)

  tabela_por_municipio <- tabela

  # Agrupamento por estado e faixa populacional
  rlog::log_info("residuos: agregando por estado e faixa populacional")
  # media <- media_por_estado_faixa(tabela)
  conta <- conta_municipios_por_estado_faixa(tabela, campo_estado = "estado")

  tabela <- soma_por_estado_faixa(tabela)
  tabela$numero_municipios <- conta$numero_municipios
  tabela <- cria_faixas_vazias(tabela)

  # Transbordo
  tabela <- demanda_transbordo(tabela)
  tabela <- regionaliza_transbordo(tabela, cenario_regionalizacao)
  tabela <- investimento_expansao_transbordo(tabela, custo_transbordo)

  # Coleta indiferenciada
  rlog::log_info("residuos: investimento em coleta indiferenciada")
  tabela <- densidade_caminhao_por_estado_faixa(tabela, tabela_por_municipio)
  tabela <- meta_plansab_residuo(tabela)
  tabela <- deficit_coleta_indiferenciada(tabela)
  tabela <- investimento_expansao_coleta_indiferenciada(tabela, valor_caminhao)
  tabela <- capacidade_instalada_coleta_indiferenciada(tabela, valor_caminhao)
  tabela <- calcula_reposicao_parcial(
    tabela,
    "capacidade_instalada_coleta_indiferenciada",
    "investimento_expansao_coleta_indiferenciada",
    "investimento_reposicao_coleta_indiferenciada",
    ano_inicial,
    ano_final,
    ano_corrente,
    depreciacao_para_vida_util(input$deprec_coleta_indiferenciada)
  )

  # Coleta seletiva
  rlog::log_info("residuos: investimento em coleta seletiva")
  tabela <- preenche_densidade_caminhao_bau(tabela_por_municipio, tabela)
  tabela <- atendimento_relativo_coleta_seletiva(tabela)
  tabela <- deficit_coleta_seletiva(tabela)
  tabela <- investimento_expansao_coleta_seletiva(tabela, valor_caminhao_bau)
  tabela <- capacidade_instalada_coleta_seletiva(tabela, valor_caminhao_bau)
  tabela <- calcula_reposicao_parcial(
    tabela,
    "capacidade_instalada_coleta_seletiva",
    "investimento_expansao_coleta_seletiva",
    "investimento_reposicao_coleta_seletiva",
    ano_inicial,
    ano_final,
    ano_corrente,
    depreciacao_para_vida_util(input$deprec_coleta_seletiva)
  )

  # Compostagem
  rlog::log_info("residuos: investimento em compostagem")
  tabela <- demanda_compostagem(tabela)
  tabela <- preco_unidade_faixa(tabela, preco_unidade_compostagem)
  tabela <- regionaliza_compostagem(tabela, cenario_regionalizacao)
  tabela <- investimento_expansao_compostagem(tabela, vida_util_compostagem)
  tabela <- capacidade_instalada_compostagem(tabela, vida_util_compostagem)
  tabela <- calcula_reposicao_parcial(
    tabela,
    "capacidade_instalada_compostagem",
    "investimento_expansao_compostagem",
    "investimento_reposicao_compostagem",
    ano_inicial,
    ano_final,
    ano_corrente,
    depreciacao_para_vida_util(input$deprec_compostagem)
  )

  # Aterro
  rlog::log_info("residuos: investimento em aterro")
  tabela <- preco_unidade_faixa(tabela, preco_unidade_aterro)
  tabela <- demanda_aterro(tabela, vida_util_aterro)
  tabela <- regionaliza_aterro(tabela, cenario_regionalizacao)
  tabela <- investimento_expansao_aterro(tabela)
  tabela <- capacidade_instalada_aterro(tabela, vida_util_aterro)
  tabela <- calcula_reposicao_parcial(
    tabela,
    "capacidade_instalada_aterro",
    "investimento_expansao_aterro",
    "investimento_reposicao_aterro",
    ano_inicial,
    ano_final,
    ano_corrente,
    depreciacao_para_vida_util(input$deprec_aterro)
  )

  # Triagem
  rlog::log_info("residuos: investimento em triagem")
  tabela <- reaproveitamento_relativo(tabela, tabela_por_municipio)
  tabela <- projecao_reaproveitamento(tabela)
  tabela <- demanda_triagem(tabela)
  tabela <- preco_unidade_faixa(tabela, preco_unidade_triagem)
  tabela <- regionaliza_triagem(tabela, cenario_regionalizacao)
  tabela <- investimento_expansao_triagem(tabela, vida_util_triagem)
  tabela <- capacidade_instalada_triagem(tabela, vida_util_triagem)
  tabela <- calcula_reposicao_parcial(
    tabela,
    "capacidade_instalada_triagem",
    "investimento_expansao_triagem",
    "investimento_reposicao_triagem",
    ano_inicial,
    ano_final,
    ano_corrente,
    depreciacao_para_vida_util(input$deprec_triagem)
  )

  rlog::log_info("residuos: totalizando investimentos")
  tabela <- investimento_residuos_total(tabela)
  state$residuos <- tabela

  rlog::log_info("residuos: consolidando")
  state$necessidade <- dplyr::bind_rows(
    tbl_longa_investimento_residuos(tabela),
    state$necessidade
  )
  state$deficit <- dplyr::bind_rows(
    tbl_longa_deficit_residuos(tabela),
    state$deficit
  )
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

  rlog::log_info("criando novas tabelas de consolidação")
  state$necessidade <- tabela_necessidade_vazia()
  state$deficit <- tabela_deficit_vazia()

  rlog::log_info("água: iniciando módulo")
  state <- investimento_agua(state)

  rlog::log_info("esgoto: iniciando módulo")
  state <- investimento_esgoto(state)

  rlog::log_info("drenagem: iniciando módulo")
  state <- investimento_drenagem(state)

  rlog::log_info("residuos: iniciando módulo")
  state <- investimento_residuos(state)

  state$necessidade <- adiciona_pais(state$necessidade)
  state$deficit <- adiciona_pais(state$deficit)

  readr::write_excel_csv2(
    state$necessidade,
    file = "dados/resultados/necessidade.csv",
    append = FALSE
  )
  readr::write_excel_csv2(
    state$deficit,
    file = "dados/resultados/deficit.csv",
    append = FALSE
  )

  rlog::log_info("rodada terminada")
  return(state)
}
