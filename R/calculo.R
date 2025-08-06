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
  fonte1 <- carrega_dado_populacao(input$fonte1)
  fonte2_nao_censo <- !grepl(".*censo.*", input$fonte2)
  fonte2 <- carrega_dado_populacao(input$fonte2)
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
  ano_inicial <- state$input$geral$ano_corrente
  ano_corrente <- state$input$geral$ano_corrente
  ano_final <- state$input$geral$ano
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


#' Exporta os dados para Power BI
#'
#' @param tabela tabela de dados com as necessidades de investimentos com o seguintes campos:
#' "estado";"regiao";"destino";"subsistema";"necessidade_investimento";"componente";"situacao";"pais"
#' @param file caminho do arquivo de saída
#'
#' @return Não retorna nada, apenas salva o arquivo
#' @export
to_power_bi <- function(tabela, file) {
  rlog::log_info("exportando dados para Power BI")
  componentes <- unique(tabela$componente)
  # Criar uma planilha por componente
  sheets <- list()
  for (comp in componentes) {
    tabela_componente <- dplyr::filter(tabela, componente == comp)
    tabela_componente <- dplyr::select(tabela_componente, -c(componente, pais))
    if (comp == "residuos" || comp == "drenagem") {
      colunas_para_larga <- c("destino", "subsistema")
      tabela_componente <- dplyr::select(tabela_componente, -situacao)
    } else {
      colunas_para_larga <- c("destino", "subsistema", "situacao")
    }

    tabela_total <- dplyr::group_by(tabela_componente, estado)
    tabela_total <- dplyr::summarise(
      tabela_total,
      total = sum(necessidade_investimento, na.rm = TRUE)
    )
    tabela_larga <- tidyr::pivot_wider(
      tabela_componente,
      names_from = colunas_para_larga,
      values_from = "necessidade_investimento",
      values_fill = 0
    )
    tabela_destino <- tidyr::pivot_wider(
      tabela_componente,
      names_from = "destino",
      values_from = "necessidade_investimento",
      values_fill = 0
    )
    tabela_destino <- dplyr::select(tabela_destino, -dplyr::any_of(c("regiao", "subsistema", "situacao")))
    tabela_destino <- dplyr::group_by(tabela_destino, estado)
    tabela_destino <- dplyr::summarise(tabela_destino, across(is.numeric, sum))


    tabela_final <- dplyr::left_join(tabela_total, tabela_larga, by = dplyr::join_by("estado"))
    tabela_final <- dplyr::left_join(tabela_final, tabela_destino, by = dplyr::join_by("estado"))

    if (comp == "agua" || comp == "esgoto") {
      tabela_situacao <- tidyr::pivot_wider(
        tabela_componente,
        names_from = "situacao",
        values_from = "necessidade_investimento",
        values_fill = 0
      )
      tabela_situacao <- dplyr::select(tabela_situacao, -dplyr::all_of(c("regiao", "destino", "subsistema")))
      tabela_situacao <- dplyr::group_by(tabela_situacao, estado)
      tabela_situacao <- dplyr::summarise(tabela_situacao, across(is.numeric, sum))
      tabela_final <- dplyr::left_join(tabela_final, tabela_situacao, by = dplyr::join_by("estado"))

      tabela_repo_situ <- tidyr::pivot_wider(
        tabela_componente,
        names_from = c("destino", "situacao"),
        values_from = "necessidade_investimento",
        values_fill = 0
      )
      tabela_repo_situ <- dplyr::select(tabela_repo_situ, -dplyr::any_of(c("regiao", "subsistema")))
      tabela_repo_situ <- dplyr::group_by(tabela_repo_situ, estado)
      tabela_repo_situ <- dplyr::summarise(tabela_repo_situ, across(is.numeric, sum))
      tabela_final <- dplyr::left_join(tabela_final, tabela_repo_situ, by = dplyr::join_by("estado"))
    }

    # Renomear as colunas
    residuos <- c(
      "UF" = "estado",
      "Nome Região" = "regiao",
      "Expansão da coleta indiferenciada" = "expansao_coleta_indiferenciada",
      "Reposição da coleta indiferenciada" = "reposicao_coleta_indiferenciada",
      "Expansão da coleta seletiva" = "expansao_coleta_seletiva",
      "Reposição da coleta seletiva" = "reposicao_coleta_seletiva",
      "Expansão da triagem" = "expansao_triagem",
      "Reposição da triagem" = "reposicao_triagem",
      "Expansão do transbordo" = "expansao_transbordo",
      "Reposição do transbordo" = "reposicao_transbordo",
      "Reposição de aterros" = "reposicao_aterro",
      "Expansão de aterros" = "expansao_aterro",
      "Reposição da compostagem" = "reposicao_compostagem",
      "Expansão da compostagem" = "expansao_compostagem",
      "Expansão" = "expansao",
      "Reposição" = "reposicao",
      "RSU Total" = "total"
    )
    drenagem <- c(
      "UF" = "estado",
      "Nome Região" = "regiao",
      "Expansão das infraestruturas de drenagem" = "expansao",
      "Reposição das Infraestruturas de drenagem" = "reposicao",
      "Cadastro técnico da drenagem" = "cadastro",
      "Investimento total em DMAPU" = "total"
    )
    agua <- c(
      "UF" = "estado",
      "Nome Região" = "regiao",
      "Expansão da produção - U" = "expansao_producao_agua_urbana",
      "Reposição da produção - U" = "reposicao_producao_agua_urbana",
      "Expansão da produção - R" = "expansao_producao_agua_rural",
      "Reposição da produção - R" = "reposicao_producao_agua_rural",
      "Expansão da distribuição - U" = "expansao_distribuicao_agua_urbana",
      "Reposição da distribuição - U" = "reposicao_distribuicao_agua_urbana",
      "Expansão da distribuição - R" = "expansao_distribuicao_agua_rural",
      "Reposição da distribuição - R" = "reposicao_distribuicao_agua_rural",
      "Expansão da solução individual - R" = "expansao_individual_rural",
      "Expansão total - U" = "expansao_urbana",
      "Expansão total - R" = "expansao_rural",
      "Reposição total - U" = "reposicao_urbana",
      "Reposição total - R" = "reposicao_rural",
      "Expansão" = "expansao",
      "Reposição" = "reposicao",
      "Investimento total em AA rural" = "rural",
      "Investimento total em AA urbano" = "urbana",
      "Investimento total em AA" = "total"
    )
    esgoto <- c(
      "UF" = "estado",
      "Nome Região" = "regiao",
      "Expansão" = "expansao",
      "Reposição" = "reposicao",
      "Expansão da coleta - U" = "expansao_coleta_esgoto_urbana",
      "Expansão da coleta - R" = "expansao_coleta_esgoto_rural",
      "Reposição da coleta - U" = "reposicao_coleta_esgoto_urbana",
      "Reposição da coleta - R" = "reposicao_coleta_esgoto_rural",
      "Expansão do tratamento - U" = "expansao_tratamento_esgoto_urbana",
      "Expansão do tratamento - R" = "expansao_tratamento_esgoto_rural",
      "Reposição do tratamento - U" = "reposicao_tratamento_esgoto_urbana",
      "Reposição do tratamento - R" = "reposicao_tratamento_esgoto_rural",
      "Expansão total - U" = "expansao_urbana",
      "Expansão total - R" = "expansao_rural",
      "Reposição total - U" = "reposicao_urbana",
      "Reposição total - R" = "reposicao_rural",
      "Expansão da solução individual - R" = "expansao_individual_rural",
      "Investimento total em ES - R" = "rural",
      "Investimento total em ES - U" = "urbana",
      "Invenstimento total em ES" = "total"
    )

    col_renamer <- list("residuos" = residuos, "drenagem" = drenagem, "agua" = agua, "esgoto" = esgoto)
    to_rename <- col_renamer[[comp]]
    tabela_final <- dplyr::rename(tabela_final, dplyr::any_of(to_rename))

    # Create new column Regiao based on Nome Região
    map_regiao <- c(
      "Norte" = "N",
      "Nordeste" = "NE",
      "Centro-Oeste" = "CO",
      "Sudeste" = "SE",
      "Sul" = "S"
    )
    tabela_final <- dplyr::mutate(tabela_final, Região = map_regiao[`Nome Região`])
    sheets[[comp]] <- tabela_final
  }
  # salva a tabela no arquivo xlsx
  writexl::write_xlsx(
    sheets,
    path = file,
    col_names = TRUE
  )
}


# Adiciona colunas com nomes por extenso
# respectivos aos destinos, situação, subsistemas e componentes
#'
#' @param tabela tabela de dados com as necessidades de investimentos
#' com os seguintes campos: "destino";"situacao";"subsistema";"componente"
#'
#' @return tabela com as colunas adicionais "Destino", "Situação", "Subsistema" e "Componente"
add_colunas_por_extenso <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    RegiãoNome = regiao,
    regiao = dplyr::case_when(
      regiao == "Norte" ~ "N",
      regiao == "Nordeste" ~ "NE",
      regiao == "Centro-Oeste" ~ "CO",
      regiao == "Sudeste" ~ "SE",
      regiao == "Sul" ~ "S",
    ),
    DestinoNome = dplyr::case_when(
      destino == "expansao" ~ "Expansão",
      destino == "reposicao" ~ "Reposição",
      destino == "cadastro" ~ "Cadastro técnico",
    ),
    SituaçãoNome = dplyr::case_when(
      situacao == "rural" ~ "Rural",
      situacao == "urbana" ~ "Urbana",
    ),
    SubsistemaNome = dplyr::case_when(
      subsistema == "aterro" ~ "Aterro sanitário",
      subsistema == "coleta_indiferenciada" ~ "Coleta indiferenciada",
      subsistema == "coleta_seletiva" ~ "Coleta seletiva",
      subsistema == "compostagem" ~ "Compostagem",
      subsistema == "drenagem_urbana" ~ "Drenagem urbana",
      subsistema == "producao_agua" ~ "Produção de água",
      subsistema == "distribuicao_agua" ~ "Distribuição de água",
      subsistema == "tratamento_esgoto" ~ "Tratamento de esgoto",
      subsistema == "coleta_esgoto" ~ "Coleta de esgoto",
      subsistema == "individual" ~ "Solução individual",
      subsistema == "transbordo" ~ "Transbordo",
      subsistema == "triagem" ~ "Unidade de Triagem",
    ),
    ComponenteNome = dplyr::case_when(
      componente == "agua" ~ "Abastecimento de água",
      componente == "esgoto" ~ "Esgotamento sanitário",
      componente == "residuos" ~ "Resíduos sólidos",
      componente == "drenagem" ~ "Drenagem urbana",
    )
  )

  return(tabela)
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

  # remover linhas com NA
  state$necessidade <- dplyr::filter(
    state$necessidade,
    !is.na(estado)
  )
  state$deficit <- dplyr::filter(
    state$deficit,
    !is.na(estado)
  )

  por_estado <- state$necessidade
  por_estado <- add_colunas_por_extenso(por_estado)
  readr::write_excel_csv2(
    por_estado,
    file = "dados/resultados/necessidade_por_estado.csv",
    append = FALSE
  )

  necessidade_regiao <- dplyr::group_by(
    state$necessidade,
    regiao, destino, subsistema, componente, situacao
  )
  necessidade_regiao <- dplyr::summarise(
    necessidade_regiao,
    necessidade_investimento = sum(necessidade_investimento, na.rm = TRUE)
  )
  readr::write_excel_csv2(
    necessidade_regiao,
    file = "dados/resultados/necessidade_por_regiao.csv",
    append = FALSE
  )
  necessidade_componente <- dplyr::group_by(
    state$necessidade,
    componente, situacao, destino
  )
  necessidade_componente <- dplyr::summarise(
    necessidade_componente,
    necessidade_investimento = sum(necessidade_investimento, na.rm = TRUE)
  )
  readr::write_excel_csv2(
    necessidade_componente,
    file = "dados/resultados/necessidade_por_componente.csv",
    append = FALSE
  )

  readr::write_excel_csv2(
    state$deficit,
    file = "dados/resultados/deficit_por_estado.csv",
    append = FALSE
  )
  deficit_regiao <- dplyr::group_by(
    state$deficit,
    regiao, subsistema, componente, situacao
  )
  deficit_regiao <- dplyr::summarise(
    deficit_regiao,
    deficit = sum(deficit, na.rm = TRUE)
  )
  readr::write_excel_csv2(
    deficit_regiao,
    file = "dados/resultados/deficit_por_regiao.csv",
    append = FALSE
  )
  readr::write_excel_csv2(
    state$agua_rural,
    file = "dados/resultados/agua_rural.csv",
    append = FALSE
  )
  readr::write_excel_csv2(
    state$esgoto_rural,
    file = "dados/resultados/esgoto_rural.csv",
    append = FALSE
  )
  rlog::log_info("rodada terminada")
  return(state)
}
