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
  fonte1 <- rsan:::load_ibge(input$fonte1)
  fonte2_nao_censo <- !grepl(".*censo.*", input$fonte2)
  fonte2 <- rsan:::load_ibge(input$fonte2)
  if (fonte2_nao_censo) {
    fonte2 <- rsan:::preenche_situacao(fonte1, fonte2)
  }

  ano1 <- rsan:::nome_para_ano(input$fonte1)
  ano2 <- rsan:::nome_para_ano(input$fonte2)
  state$input$geral$ano_populacao_fonte1 <- ano1
  state$input$geral$ano_populacao_fonte2 <- ano2

  rlog::log_info("projecao: consolidando fontes")
  consolidado <- rsan:::junta_fontes_populacao(fonte1, fonte2)

  rlog::log_info("projecao: calculando taxa de crescimento")
  consolidado <- rsan:::calcula_taxa_crescimento(consolidado, ano1, ano2)
  state$taxas_projecao <- consolidado

  rlog::log_info("projecao: calculando projecao")
  state$projecao <- rsan:::calcula_projecao(
    consolidado, ano2, state$input$geral$ano
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
  ano_inicial <- 2021
  ano_final <- state$input$geral$ano
  ano_corrente <- state$input$geral$ano_corrente
  depreciacao <- rsan::depreciacao_para_vida_util(input$deprec_drenagem)

  tabela <- rsan:::load_snis_ap(input$snis_ap)
  tabela <- rsan:::area_urbana(tabela)
  tabela <- rsan:::densidade_urbana(tabela)
  tabela <- rsan:::precipitacao(tabela)
  tabela <- rsan:::adiciona_indices_drenagem(tabela)
  tabela <- rsan:::adiciona_projecao_populacao(state$projecao, ano_final, tabela)
  tabela <- rsan:::capacidade_instalada_drenagem(tabela)

  if (input$modo == 1) { # Investimento per capita constante
    tabela <- rsan:::investimento_constante(tabela, input$investimento_per_capita)
  } else { # Investimento per capita por regressão
    tabela <- rsan:::aplica_regressao_multipla_drenagem(tabela, input)
  }

  tabela <- rsan:::calcula_reposicao_parcial(
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
  tabela <- rsan:::investimento_total_drenagem(tabela)
  tabela <- rsan:::adiciona_pais(tabela)
  state$drenagem <- tabela
  state$necessidade <- dplyr::bind_rows(
    tbl_longa_investimento_drenagem(tabela),
    state$necessidade
  )
  return(state)
}

residuos_snis_fields <- c(
  "codigo_municipio", "POP_TOT", "POP_URB", "Estado",
  "CO164", "CO050", "CO119", "CO021", "CS026", "CS009",
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
  ano_corrente <- state$input$geral$ano_corrente
  cenario_regionalizacao <- "A"

  # Consolida os dados de Unidades de Processamento (SNIS-prestadores)
  rlog::log_info("residuos: carregando snis-rs data")
  snis_rs <- rsan:::load_snis_rs(input$snis_rs)
  compostagem <- rsan::quantidade_compostagem_municipio(snis_rs)

  # Consolidação dos dados para classificação
  rlog::log_info("residuos: consolidando dados para classificação")
  tabela <- rsan:::get_snis_data(input$snis, residuos_snis_fields)
  tabela <- rsan:::adiciona_projecao_populacao(state$projecao, ano, tabela)
  tabela <- dplyr::left_join(tabela, compostagem, by = "codigo_municipio")
  tabela <- rsan:::adiciona_pais(tabela)
  tabela <- rsan:::adiciona_estado(tabela)
  tabela <- rsan:::adiciona_regiao(tabela)
  tabela <- rsan:::adiciona_tipo_disposicao(tabela)
  tabela <- rsan:::adiciona_classificacao_litoranea(tabela)
  tabela <- rsan:::numero_caminhoes(tabela)
  tabela <- rsan:::densidade_caminhoes(tabela)

  # Classificação por faixas populacionais
  rlog::log_info("residuos: classificando por faixa populacional")
  limites <- as.integer(c(0, 10e3, 30e3, 100e3, 250e3, 1e6, 4e6))
  tabela <- rsan:::classifica_faixa_populacional(tabela, limites)

  # Preenchimentos
  tabela <- rsan:::mascara_coleta_seletiva(tabela)
  tabela <- rsan:::preenche_atendimento_coleta_seletiva(tabela)
  tabela <- rsan:::preenche_atendimento_coleta_indiferenciada(tabela)
  tabela <- rsan:::preenche_taxa_geracao_residuos(tabela)
  tabela <- rsan:::preenche_quantidade_coletada(tabela)
  tabela <- rsan:::geracao_residuos(tabela)
  tabela <- rsan:::disposicao_inadequada(tabela)

  tabela_por_municipio <- tabela

  # Agrupamento por estado e faixa populacional
  rlog::log_info("residuos: agregando por estado e faixa populacional")
  # media <- rsan::media_por_estado_faixa(tabela)
  conta <- rsan::conta_municipios_por_estado_faixa(tabela, campo_estado = "estado")

  tabela <- rsan:::soma_por_estado_faixa(tabela)
  tabela$numero_municipios <- conta$numero_municipios
  tabela <- rsan:::cria_faixas_vazias(tabela)

  # Transbordo
  tabela <- rsan:::demanda_transbordo(tabela)
  tabela <- rsan:::regionaliza_transbordo(tabela, cenario_regionalizacao)
  tabela <- rsan:::investimento_expansao_transbordo(tabela, custo_transbordo)

  # Coleta indiferenciada
  rlog::log_info("residuos: investimento em coleta indiferenciada")
  tabela <- rsan:::densidade_caminhao_por_estado_faixa(tabela, tabela_por_municipio)
  tabela <- rsan::meta_plansab_residuo(tabela)
  tabela <- rsan::deficit_coleta_indiferenciada(tabela)
  tabela <- rsan::investimento_expansao_coleta_indiferenciada(tabela, valor_caminhao)
  tabela <- rsan::capacidade_instalada_coleta_indiferenciada(tabela, valor_caminhao)
  tabela <- rsan:::calcula_reposicao_parcial(
    tabela,
    "capacidade_instalada_coleta_indiferenciada",
    "investimento_expansao_coleta_indiferenciada",
    "investimento_reposicao_coleta_indiferenciada",
    ano_inicial,
    ano_final,
    ano_corrente,
    rsan::depreciacao_para_vida_util(input$deprec_coleta_indiferenciada)
  )

  # Coleta seletiva
  rlog::log_info("residuos: investimento em coleta seletiva")
  tabela <- rsan:::preenche_densidade_caminhao_bau(tabela_por_municipio, tabela)
  tabela <- rsan:::atendimento_relativo_coleta_seletiva(tabela)
  tabela <- rsan:::deficit_coleta_seletiva(tabela)
  tabela <- rsan::investimento_expansao_coleta_seletiva(tabela, valor_caminhao_bau)
  tabela <- rsan::capacidade_instalada_coleta_seletiva(tabela, valor_caminhao_bau)
  tabela <- rsan::calcula_reposicao_parcial(
    tabela,
    "capacidade_instalada_coleta_seletiva",
    "investimento_expansao_coleta_seletiva",
    "investimento_reposicao_coleta_seletiva",
    ano_inicial,
    ano_final,
    ano_corrente,
    rsan::depreciacao_para_vida_util(input$deprec_coleta_seletiva)
  )

  # Compostagem
  rlog::log_info("residuos: investimento em compostagem")
  tabela <- rsan:::demanda_compostagem(tabela)
  tabela <- rsan:::preco_unidade_faixa(tabela, preco_unidade_compostagem)
  tabela <- rsan:::regionaliza_compostagem(tabela, cenario_regionalizacao)
  tabela <- rsan:::investimento_expansao_compostagem(tabela, vida_util_compostagem)
  tabela <- rsan:::capacidade_instalada_compostagem(tabela, vida_util_compostagem)
  tabela <- rsan:::calcula_reposicao_parcial(
    tabela,
    "capacidade_instalada_compostagem",
    "investimento_expansao_compostagem",
    "investimento_reposicao_compostagem",
    ano_inicial,
    ano_final,
    ano_corrente,
    rsan::depreciacao_para_vida_util(input$deprec_compostagem)
  )

  # Aterro
  rlog::log_info("residuos: investimento em aterro")
  tabela <- rsan:::preco_unidade_faixa(tabela, preco_unidade_aterro)
  tabela <- rsan:::demanda_aterro(tabela, vida_util_aterro)
  tabela <- rsan:::regionaliza_aterro(tabela, cenario_regionalizacao)
  tabela <- rsan:::investimento_expansao_aterro(tabela)
  tabela <- rsan:::capacidade_instalada_aterro(tabela, vida_util_aterro)
  tabela <- rsan:::calcula_reposicao_parcial(
    tabela,
    "capacidade_instalada_aterro",
    "investimento_expansao_aterro",
    "investimento_reposicao_aterro",
    ano_inicial,
    ano_final,
    ano_corrente,
    rsan::depreciacao_para_vida_util(input$deprec_aterro)
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
  tabela <- rsan::calcula_reposicao_parcial(
    tabela,
    "capacidade_instalada_triagem",
    "investimento_expansao_triagem",
    "investimento_reposicao_triagem",
    ano_inicial,
    ano_final,
    ano_corrente,
    rsan::depreciacao_para_vida_util(input$deprec_triagem)
  )

  rlog::log_info("residuos: totalizando investimentos")
  tabela <- rsan:::investimento_residuos_total(tabela)
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
  state$necessidade <- rsan:::tabela_necessidade_vazia()
  state$deficit <- rsan:::tabela_deficit_vazia()

  rlog::log_info("água: iniciando módulo")
  state <- rsan:::investimento_agua(state)

  rlog::log_info("esgoto: iniciando módulo")
  state <- rsan:::investimento_esgoto(state)

  rlog::log_info("drenagem: iniciando módulo")
  state <- investimento_drenagem(state)

  rlog::log_info("residuos: iniciando módulo")
  state <- investimento_residuos(state)
  state$necessidade <- rsan:::adiciona_pais(state$necessidade)
  state$deficit <- rsan:::adiciona_pais(state$deficit)

  rlog::log_info("rodada terminada")
  return(state)
}
