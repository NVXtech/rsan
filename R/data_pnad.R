pnad_base_path <- file.path("dados", "brutos", "pnadc")

#' Carrega PNAD continua
#'
#' @param componente tema a ser adicionado
#' @param ano ano do pnad
#'
#' @return tabela com os dados de atendimento
#' @export
carrega_pnad_continua <- function(componente, ano) {
  regiao_sigla <- c("N", "NE", "SE", "S", "CO")
  output <- dplyr::tibble(regiao_sigla)
  if (componente == "agua") {
    files <- c("pnadc_a1.csv", "pnadc_a2.csv", "pnadc_a3.csv")
    labels <- c(
      "atendimento_agua_relativo_total",
      "atendimento_agua_relativo_urbano",
      "atendimento_agua_relativo_rural"
    )
  } else if (componente == "esgoto") {
    files <- c("pnadc_e1.csv", "pnadc_e2.csv", "pnadc_e3.csv")
    labels <- c(
      "atendimento_esgoto_relativo_total",
      "atendimento_esgoto_relativo_urbano",
      "atendimento_esgoto_relativo_rural"
    )
  } else {
    stop(sprintf("Carrega PNADc Componente %s inválido.", componente))
  }
  for (i in seq(3)) {
    path <- file.path(pnad_base_path, files[i])
    if (!file.exists(path)) {
      stop(paste("Arquivo não encontrado:", path))
    }
    tabela <- readr::read_delim(path,
      skip = 1,
      delim = ";",
      col_types = "idddddd",
      locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
    )
    tabela <- dplyr::filter(tabela, ANO == ano)
    if (nrow(tabela) == 0) {
      stop(paste("Carrega PNADc não encontrou dados para o ano de ", ano))
    }
    tabela <- dplyr::select(tabela, regiao_sigla)
    tabela <- tidyr::pivot_longer(tabela, cols = regiao_sigla, names_to = "regiao_sigla", values_to = labels[i])
    output <- dplyr::left_join(output, tabela, by = "regiao_sigla")
  }
  return(output)
}


#' Adiciona atendimento PNAD continua
#'
#' @param tabela contendo as regiao_sigla, populacao_total_corrente, populacao_urbana_corrente, populacao_rural_corrente
#' @param componente componente a ser adicionado
#' @param ano ano do pnad
#'
#' @return tabela com os dados de atendimento
#' @export
adiciona_atendimento_pnadc <- function(tabela, componente, ano) {
  required_columns <- c("regiao_sigla", "populacao_total_corrente", "populacao_urbana_corrente", "populacao_rural_corrente")
  missing_columns <- setdiff(required_columns, names(tabela))
  if (length(missing_columns) > 0) {
    stop(paste("A tabela deve conter as seguintes colunas:", paste(missing_columns, collapse = ", ")))
  }
  pnad <- carrega_pnad_continua(componente, ano)
  tabela <- dplyr::left_join(tabela, pnad, by = "regiao_sigla")
  if (componente == "agua") {
    tabela <- dplyr::mutate(
      tabela,
      atendimento_tot_agua_hab = populacao_total_corrente * atendimento_agua_relativo_total,
      atendimento_urb_agua_hab = populacao_urbana_corrente * atendimento_agua_relativo_urbano,
      atendimento_rur_agua_hab = populacao_rural_corrente * atendimento_agua_relativo_rural,
    )
  } else if (componente == "esgoto") {
    tabela <- dplyr::mutate(
      tabela,
      atendimento_tot_esgoto_hab = populacao_total_corrente * atendimento_esgoto_relativo_total,
      atendimento_urb_esgoto_hab = populacao_urbana_corrente * atendimento_esgoto_relativo_urbano,
      atendimento_rur_esgoto_hab = populacao_rural_corrente * atendimento_esgoto_relativo_rural,
    )
  } else if (componente == "residuos") {
    tabela <- dplyr::mutate(
      tabela,
      atendimento_tot_coleta_indiferenciada_hab = populacao_total_corrente * atendimento_coleta_indiferenciada_relativo_total,
      atendimento_urb_coleta_indiferenciada_hab = populacao_urbana_corrente * atendimento_coleta_indiferenciada_relativo_urbano,
      atendimento_rur_coleta_indiferenciada_hab = populacao_rural_corrente * atendimento_coleta_indiferenciada_relativo_rural,
    )
  } else {
    stop(sprintf("Adiciona Atendimento PNADc Componente %s inválido.", componente))
  }
  cols_to_remove <- c(
    sprintf("atendimento_%s_relativo_total", componente),
    sprintf("atendimento_%s_relativo_urbano", componente),
    sprintf("atendimento_%s_relativo_rural", componente)
  )
  tabela <- dplyr::select(tabela, -dplyr::any_of(cols_to_remove))
  return(tabela)
}


#' Adiciona atendimento relativo PNAD continua
#'
#' @param tabela contendo as regiao_sigla
#' @param componente componente a ser adicionado
#' @param ano ano do pnad
#'
#' @return tabela com os dados de atendimento
#' @export
adiciona_atendimento_relativo_pnadc <- function(tabela, componente, ano) {
  required_columns <- c("regiao_sigla")
  missing_columns <- setdiff(required_columns, names(tabela))
  if (length(missing_columns) > 0) {
    stop(paste("A tabela deve conter as seguintes colunas:", paste(missing_columns, collapse = ", ")))
  }
  pnad <- carrega_pnad_continua(componente, ano)
  tabela <- dplyr::left_join(tabela, pnad, by = "regiao_sigla")
  return(tabela)
}
