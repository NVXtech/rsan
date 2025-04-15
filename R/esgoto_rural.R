#' Necessidade de Investimento - Componente Esgoto - Situação Rural

#' Custo individual para sistemas de estações de esgoto
#'
#' Adiciona coluna com custo individual para sistemas de estações de esgoto.
#'
#' @param tabela um `data.frame` contendo as colunas `seguranca_hidrica` e `morador_per_domicilio`.
#' @param params uma `list` contendo os preços para cada faixa
#' @export
#'
#' @return um `data.frame` contendo a coluna adicional `custo_esgoto_individual `.
custo_individual_esgoto <- function(tabela, params) {
  classes_seguranca <- c("Baixa", "Mínima")
  tabela <- dplyr::mutate(
    tabela,
    custo_esgoto_individual = dplyr::case_when(
      !(seguranca_hidrica %in% classes_seguranca) & morador_per_domicilio >= 0 & morador_per_domicilio < 2
      ~ params$custo_individual_esgoto_faixa1,
      !(seguranca_hidrica %in% classes_seguranca) & morador_per_domicilio >= 2 & morador_per_domicilio < 3
      ~ params$custo_individual_esgoto_faixa2,
      !(seguranca_hidrica %in% classes_seguranca) & morador_per_domicilio >= 3 & morador_per_domicilio < 4
      ~ params$custo_individual_esgoto_faixa3,
      !(seguranca_hidrica %in% classes_seguranca) & morador_per_domicilio >= 4 & morador_per_domicilio < 5
      ~ params$custo_individual_esgoto_faixa4,
      !(seguranca_hidrica %in% classes_seguranca) & morador_per_domicilio >= 5 & morador_per_domicilio < 6
      ~ params$custo_individual_esgoto_faixa5,
      !(seguranca_hidrica %in% classes_seguranca) & morador_per_domicilio >= 6 & morador_per_domicilio < 7
      ~ params$custo_individual_esgoto_faixa6,
      !(seguranca_hidrica %in% classes_seguranca) & morador_per_domicilio >= 7 & morador_per_domicilio < 8
      ~ params$custo_individual_esgoto_faixa7,
      !(seguranca_hidrica %in% classes_seguranca) & morador_per_domicilio >= 8
      ~ params$custo_individual_esgoto_faixa8,
      (seguranca_hidrica %in% classes_seguranca) & morador_per_domicilio < 5
      ~ params$custo_individual_esgoto_faixa9,
      (seguranca_hidrica %in% classes_seguranca) & morador_per_domicilio >= 5
      ~ params$custo_individual_esgoto_faixa10
    )
  )
}

#' Fração de investimentos em esgoto coletivos e individuais
#'
#' Define a fração coletiva e individual para necessidade de investimento em esgoto.
#'
#' @param tabela um `data.frame` contendo as colunas `situacao_setor`, `deficit_agua_relativo_rural` e `regiao`.
#' @export
#'
#' @return um `data.frame` contendo as colunas adicionais `fracao_investimento_coletivo_esgoto e `fracao_investimento_individual_esgoto`.
fracao_coletivo_individual_esgoto <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    fracao_investimento_coletivo_esgoto = dplyr::case_when(
      # PS. foi utilizado somente 1 fator para agua e esgoto
      codigo_situacao == 7 & deficit_agua_relativo_rural < 0.7 ~ 0.1,
      codigo_situacao == 8 & deficit_agua_relativo_rural < 0.8 ~ 0.1,
      codigo_situacao == 5 & deficit_agua_relativo_rural < 0.6 ~ 0.25,
      codigo_situacao == 6 & deficit_agua_relativo_rural < 0.6 ~ 0.25,
      codigo_situacao == 7 & deficit_agua_relativo_rural >= 0.7 ~ 0.25,
      codigo_situacao == 8 & deficit_agua_relativo_rural >= 0.8 ~ 0.25,
      codigo_situacao == 4 & deficit_agua_relativo_rural < 0.1 & regiao == "Sudeste" ~ 0.25,
      codigo_situacao == 5 & deficit_agua_relativo_rural >= 0.6 ~ 0.4,
      codigo_situacao == 6 & deficit_agua_relativo_rural >= 0.6 ~ 0.4,
      codigo_situacao == 4 & deficit_agua_relativo_rural >= 0.1 & regiao == "Sudeste" ~ 1,
      codigo_situacao == 4 & regiao != "Sudeste" ~ 1
    ),
    fracao_investimento_individual_esgoto =
      1 - fracao_investimento_coletivo_esgoto
  )
}

#' Número de domicílios com deficit de esgoto
#'
#' Calcula o número de domicílios com déficit de sistemas de estação de esgoto.
#'
#' @param tabela um `data.frame` contendo as colunas `domicilios_projecao` e `deficit_esgoto_relativo_rural`.
#' @export
#'
#' @return um `data.frame` contendo a coluna adicional `domicilios_deficit_esgoto`.
domicilios_com_deficit_esgoto <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    domicilios_deficit_esgoto = domicilios_projecao * deficit_esgoto_relativo_rural,
  )
}

#' Número de habitantes com deficit de esgoto
#'
#' Calcula o número de habitantes com déficit de sistemas de estação de esgoto.
#'
#' @param tabela um `data.frame` contendo as colunas `domicilios_projecao` e `deficit_esgoto_relativo_rural`.
#' @export
#'
#' @return um `data.frame` contendo a coluna adicional `domicilios_deficit_esgoto`.
habitantes_com_deficit_esgoto <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    deficit_rural = domicilios_deficit_esgoto * morador_per_domicilio,
  )
}

#' Número de domicílios com demanda adequada de esgoto
#'
#' Calcula o número de domicílios com déficit de sistemas de estação de esgoto.
#'
#' @param tabela um `data.frame` contendo as colunas `domicilios_projecao` e `deficit_esgoto_relativo_rural`.
#' @export
#'
#' @return um `data.frame` contendo a coluna adicional `domicilios_deficit_esgoto`.
domicilios_adequados_com_esgoto <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    domicilios_adequados_esgoto = domicilios_projecao * (1.0 - deficit_esgoto_relativo_rural),
  )
}

#' Custo coletivos para sistemas de coleta de esgoto
#'
#' Adiciona coluna com custo coletivos para sistemas de coleta de esgoto.
#'
#' @param tabela um `data.frame` contendo as colunas `seguranca_hidrica` e `semiarido`.
#' @export
#'
#' @return um `data.frame` contendo as colunas adicionais: "custo_esgoto_coleta_rural", "classe_densidade", "regiao".
custo_coleta_esgoto <- function(tabela, coleta_esgoto) {
  manter <- c("custo_esgoto_coleta_rural", "classe_densidade", "regiao")
  coleta_esgoto <- dplyr::select(coleta_esgoto, dplyr::all_of(manter))
  juntar_por <- c("regiao", "classe_densidade")
  tabela <- dplyr::left_join(tabela, coleta_esgoto, by = juntar_por)
}

#' Custo coletivos para sistemas de tratamento de esgoto
#'
#' Adiciona coluna com custo coletivos para sistemas de tratamento de esgoto.
#'
#' @param tabela um `data.frame` contendo as colunas `seguranca_hidrica` e `semiarido`.
#' @export
#'
#' @return um `data.frame` contendo a coluna adicional `custo_agua_individual`.
custo_tratamento_esgoto <- function(tabela, custo_tratamento) {
  manter <- c("custo_esgoto_tratamento_rural", "classe_densidade", "regiao")
  custo_tratamento <- dplyr::select(custo_tratamento, dplyr::all_of(manter))
  juntar_por <- c("regiao", "classe_densidade")
  tabela <- dplyr::left_join(tabela, custo_tratamento, by = juntar_por)
}

#' Necessidade de investimento rural para sistema de estação de esgoto
#'
#' Calcula a necessidade de investimento para expansão do sistema de estação de esgoto.
#'
#' @param tabela um `data.frame` contendo as colunas:
#' `custo_esgoto_individual`, `custo_esgoto_producao_rural`, `custo_esgoto_distribuicao_rural`,
#' `fracao_investimento_individual_esgoto`, `fracao_investimento_coletivo_esgoto` e
#' `domicilios_deficit_esgoto`
#' @export
#'
#' @return um `data.frame` contendo as colunas adicionais:
#' investimento_expansao_individual, investimento_expansao_tratamento_esgoto e investimento_expansao_coleta_esgoto
investimento_rural_esgoto <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    investimento_expansao_individual = custo_esgoto_individual *
      fracao_investimento_individual_esgoto * domicilios_deficit_esgoto,
    investimento_expansao_tratamento_esgoto = domicilios_deficit_esgoto *
      custo_esgoto_tratamento_rural *
      fracao_investimento_coletivo_esgoto,
    investimento_expansao_coleta_esgoto = custo_esgoto_coleta_rural *
      fracao_investimento_coletivo_esgoto * domicilios_deficit_esgoto,
  )
}

#' Capacidade instalada esgoto situação rural
#'
#' Calcula a capacidade instalada para expansão do sistema de abastecimento de esgoto situação rural.
#'
#' @param tabela um `data.frame` contendo as colunas:
#' `domicilios_adequados_esgoto`, `custo_esgoto_distribuicao_rural` e `custo_esgoto_producao_rural`
#' @export
#'
#' @return um `data.frame` contendo as colunas adicional capacidade_instalada_coleta_esgoto  e capacidade_instalada_tratamento_esgoto.
capacidade_instalada_rural_esgoto <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    capacidade_instalada_coleta_esgoto = domicilios_adequados_esgoto *
      custo_esgoto_coleta_rural,
    capacidade_instalada_tratamento_esgoto = domicilios_adequados_esgoto *
      custo_esgoto_tratamento_rural
  )
}

#' Déficit rural esgoto PNAD
#'
#' Adiciona o déficit rural para sistema de abastecimento de esgoto (PNAD).
#'
#' @param tabela um `data.frame` contendo a coluna `estado`
#' @param deficit um `data.frame` contendo a coluna `estado` e `deficit_esgoto_relativo_rural`
#' @export
#'
#' @return um `data.frame` contendo a coluna adicional `taxa_crescimento`.
adiciona_deficit_rural_esgoto_pnad <- function(tabela, deficit) {
  manter <- c("estado", "deficit_esgoto_relativo_rural")
  deficit <- dplyr::select(deficit, dplyr::all_of(manter))
  tabela <- dplyr::left_join(tabela, deficit, by = "estado")
}

#' Consolida os dados de investimentos para água situação rural
#'
#' Totaliza os dados investimento em expansão, reposição e total.
#'
#' @param tabela contendo as colunas:
#' \itemize{
#' \item{investimento_expansao_individual}
#' \item{investimento_expansao_coleta_esgoto}
#' \item{investimento_expansao_tratamento_esgoto}
#' \item{investimento_reposicao_coleta_esgoto}
#' \item{investimento_reposicao_tratamento_esgoto}
#' }
#'
#' @return tabela contendo os campos:
#' \itemize{
#'  \item{investimento_expansao}
#'  \item{investimento_reposicao}
#'  \item{investimento_total}
#' }
#'
#' @export
consolida_investimentos_rural_esgoto <- function(tabela) {
  tabela <- somar_por_campo(tabela, "codigo_municipio")
  tabela <- dplyr::mutate(
    tabela,
    # investimento_esgoto_rural_coletivo = investimento_expansao_tratamento_esgoto + investimento_expansao_coleta_esgoto,
    investimento_expansao = investimento_expansao_coleta_esgoto +
      investimento_expansao_tratamento_esgoto +
      investimento_expansao_individual,
    investimento_reposicao = investimento_reposicao_tratamento_esgoto +
      investimento_reposicao_coleta_esgoto,
    investimento_total = investimento_expansao + investimento_reposicao,
  )
}

#' Cria tabela longa de necessidade de investimento do componente esgoto
#'
#' @param tabela contendo as colunas:
#' \itemize{
#' \item{investimento_expansao_individual}
#' \item{investimento_expansao_coleta_esgoto}
#' \item{investimento_expansao_tratamento_esgoto}
#' \item{investimento_reposicao_coleta_esgoto}
#' \item{investimento_reposicao_tratamento_esgoto}
#' }
#'
#' @return tabela contendo os campos:
#' \itemize{
#'  \item{estado}
#'  \item{regiao}
#'  \item{componente}
#'  \item{situacao}
#'  \item{destino}
#'  \item{subsistema}
#' }
#'
#' @export
tbl_longa_investimentos_esgoto_rural <- function(tabela) {
  colunas <- c(
    "estado", "regiao",
    "investimento_expansao_individual",
    "investimento_expansao_coleta_esgoto",
    "investimento_expansao_tratamento_esgoto",
    "investimento_reposicao_coleta_esgoto",
    "investimento_reposicao_tratamento_esgoto"
  )
  tabela <- dplyr::select(tabela, dplyr::all_of(colunas))
  tabela <- somar_por_campo(tabela, "estado")
  tabela <- tidyr::pivot_longer(
    tabela,
    cols = starts_with("investimento_"),
    names_to = c("destino", "subsistema"),
    names_pattern = "investimento_(.*?)_(.*)",
    values_to = "necessidade_investimento"
  )
  tabela <- dplyr::mutate(
    tabela,
    componente = "esgoto",
    situacao = "rural"
  )
}

#' Cria tabela longa de deficit do componente esgoto e situacao rural
#'
#' @param tabela contendo as colunas:
#' \itemize{
#' \item{estado}
#' \item{regiao}
#' \item{deficit_rural}
#' }
#'
#' @return tabela contendo os campos:
#' \itemize{
#'  \item{estado}
#'  \item{regiao}
#'  \item{componente}
#'  \item{situacao}
#'  \item{subsistema}
#'  \item{deficit}
#' }
#'
#' @export
tbl_longa_deficit_esgoto_rural <- function(tabela) {
  colunas <- c("estado", "regiao", "deficit_rural")
  tabela <- dplyr::select(tabela, dplyr::all_of(colunas))
  tabela <- somar_por_campo(tabela, "estado")
  tabela <- tidyr::pivot_longer(
    tabela,
    cols = starts_with("deficit_"),
    names_to = "situacao",
    names_pattern = "deficit_(.*)",
    values_to = "deficit"
  )
  tabela <- dplyr::mutate(
    tabela,
    componente = "esgoto",
    subsistema = "coleta_tratamento",
    deficit = as.integer(deficit)
  )
}

#' Calcula deficit relativo para agua situação rural usando dados do censo
#'
#' @param tabela um `data.frame` contendo as colunas `populacao` e `atendimento_esgoto`.
#'
#' @return um `data.frame` contendo a coluna adicional `deficit_esgoto_relativo_rural`.
#' @export
calcula_deficit_esgoto_relativo_censo <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    deficit_esgoto_relativo_rural = pmax(populacao - atendimento_esgoto, 0) / populacao
  )
  return(tabela)
}

#' Calcula deficit relativo para agua situação rural usando dados do pnadc
#'
#' @param tabela um `data.frame` contendo as colunas `populacao` e `atendimento_esgoto_relativo_rural`.
#' @param ano um `integer` com o ano de referência para os dados do PNADC.
#'
#' @return um `data.frame` contendo a coluna adicional `deficit_esgoto_relativo_rural`.
#' @export
calcula_deficit_esgoto_relativo_pnadc <- function(tabela, ano) {
  tabela <- adiciona_atendimento_relativo_pnadc(tabela, "esgoto", ano)
  tabela <- dplyr::mutate(
    tabela,
    deficit_esgoto_relativo_rural = pmax(1 - atendimento_esgoto_relativo_rural, 0)
  )
  return(tabela)
}

#' Módulo de cálculo para demanda rural de esgoto
#'
#' Esta função organiza a ordem de execução das tarefas necessárias
#' para o cálculo de necessidades de investimento em sistema de abastecimento de esgoto.
#'
#' @param input estrutura de dados (`reactive`) que guarda os parâmetros da interface gráfica
#' @param taxas_projecao um `data.frame` contendo as taxas de projecao populacional
#' @export
#'
#' @return um `data.frame` contendo as necessidade de investimentos e todos campos utilizados
rodar_modulo_rural_esgoto <- function(state) {
  input <- state$input
  taxas_projecao <- state$taxas_projecao
  ano <- input$geral$ano
  ano_inicial <- 2022

  ano_corrente <- input$geral$ano_corrente
  ano_censo <- nome_para_ano(input$projecao$fonte2)

  data("agua_esgoto_rural", package = "rsan")
  agua_esgoto_rural <- get("agua_esgoto_rural")
  seguranca_hidrica <- agua_esgoto_rural$seguranca_hidrica
  custo_coleta <- agua_esgoto_rural$custo_coleta
  custo_tratamento <- agua_esgoto_rural$custo_tratamento

  tabela <- carrega_censo2022_setor()
  tabela <- filtra_setores_rurais(tabela)
  tabela <- adiciona_taxa_crescimento(tabela, taxas_projecao)
  tabela <- fazer_projecao_domicilio(tabela, ano_censo, ano)
  tabela <- classifica_densidade_setor(tabela)
  tabela <- classifica_deficit_setor(tabela)
  tabela <- adiciona_estado(tabela)
  tabela <- adiciona_regiao(tabela)

  # Usado para classificação coletivo_individual
  # tabela <- adiciona_deficit_rural_agua_pnad(tabela, agua_esgoto_rural$deficit_pnad)
  # tabela <- adiciona_deficit_rural_esgoto_pnad(tabela, agua_esgoto_rural$deficit_pnad)
  if (input$agua$atendimento == "censo") {
    tabela <- calcula_deficit_agua_relativo_censo(tabela)
    tabela <- calcula_deficit_esgoto_relativo_censo(tabela)
  }
  if (input$esgoto$atendimento == "pnadc") {
    tabela <- calcula_deficit_agua_relativo_pnadc(tabela, input$agua$atendimento_ano)
    tabela <- calcula_deficit_esgoto_relativo_pnadc(tabela, input$esgoto$atendimento_ano)
  }

  tabela <- adiciona_seguranca_hidrica(tabela, seguranca_hidrica)
  tabela <- fracao_coletivo_individual_esgoto(tabela)

  tabela <- custo_individual_esgoto(tabela, input$esgoto)
  tabela <- custo_coleta_esgoto(tabela, custo_coleta)
  tabela <- custo_tratamento_esgoto(tabela, custo_tratamento)

  tabela <- domicilios_com_deficit_esgoto(tabela)
  tabela <- habitantes_com_deficit_esgoto(tabela)
  tabela <- domicilios_adequados_com_esgoto(tabela)
  tabela <- investimento_rural_esgoto(tabela)
  tabela <- capacidade_instalada_rural_esgoto(tabela)

  tabela <- calcula_reposicao_parcial(
    tabela,
    "capacidade_instalada_coleta_esgoto",
    "investimento_expansao_coleta_esgoto",
    "investimento_reposicao_coleta_esgoto",
    ano_corrente,
    ano,
    input$geral$ano_corrente,
    input$esgoto$vida_util
  )
  tabela <- calcula_reposicao_parcial(
    tabela,
    "capacidade_instalada_tratamento_esgoto",
    "investimento_expansao_tratamento_esgoto",
    "investimento_reposicao_tratamento_esgoto",
    ano_corrente,
    ano,
    input$geral$ano_corrente,
    input$esgoto$vida_util
  )
  state$esgoto_rural <- consolida_investimentos_rural_esgoto(tabela)

  state$necessidade <- dplyr::bind_rows(
    tbl_longa_investimentos_esgoto_rural(state$esgoto_rural),
    state$necessidade
  )
  state$deficit <- dplyr::bind_rows(
    tbl_longa_deficit_esgoto_rural(state$esgoto_rural),
    state$deficit
  )
  return(state)
}
