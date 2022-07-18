states <- c(
  "AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO",
  "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR",
  "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO"
)

#' Lista de siglas do estados mais distrito federal
#'
#' @return lista com siglas dos estados em ordem alfabética
#' @export
#'
#' @examples
#' states <- states_acronym()
states_acronym <- function() {
  return(states)
}

#' Adiciona coluna de estados na tabela
#'
#' @param tabela contendo coluna codigo_municipio
#'
#' @return tabela com coluna adiciona estado
#' @export
#'
#' @examples
#' codigo_municipio <- c("1200013", "1200054", "1200104", "1200138")
#' input <- dplyr::tibble(codigo_municipio)
#' tabela <- adiciona_estado(input)
#'
adiciona_estado <- function(tabela) {
  data(municipio, package = "rsan")
  original_colnames <- colnames(tabela)
  output_cols <- c(original_colnames, "estado")
  municipio <- dplyr::select(municipio, c("codigo_municipio", "estado", "estado_sigla"))
  tabela <- dplyr::left_join(tabela, municipio, by = "codigo_municipio")
  tabela <- dplyr::rename(tabela, estado_nome = estado, estado = estado_sigla)
  tabela <- dplyr::select(tabela, dplyr::all_of(output_cols))
  return(tabela)
}

#' Adiciona coluna de regiões
#'
#' Adiciona a tabela de entrada um coluna contendo as sigla da região em que o município se encontra.
#'
#' @param tabela contendo coluna codigo_municipio
#'
#' @return tabela com coluna adicional regiao
#' @export
#'
#' @examples
#' codigo_municipio <- c("1200013", "1200054", "1200104", "1200138")
#' input <- dplyr::tibble(codigo_municipio)
#' tabela <- adiciona_regiao(input)
#'
adiciona_regiao <- function(tabela) {
  data(municipio, package = "rsan")
  original_colnames <- colnames(tabela)
  output_cols <- c(original_colnames, "regiao")
  municipio <- dplyr::select(municipio, c("codigo_municipio", "regiao"))
  tabela <-
    dplyr::left_join(tabela, municipio, by = "codigo_municipio")
  return(tabela)
}

#' Classificação litorânea
#'
#' Adiciona se o municipio é litorâneo ou não.
#'
#' @param tabela contendo coluna codigo_municipio
#'
#' @return tabela com coluna adicional `litoral`
#' @export
#'
#' @examples
#' codigo_municipio <- c("1200013", "1200054", "1200104", "1200138")
#' input <- dplyr::tibble(codigo_municipio)
#' tabela <- adiciona_classificacao_litoranea(input)
adiciona_classificacao_litoranea <- function(tabela) {
  data(municipio_litoraneos, package = "rsan")
  original_colnames <- colnames(tabela)
  output_cols <- c(original_colnames, "regiao")
  municipio_litoraneos <- dplyr::select(municipio_litoraneos, c("codigo_municipio", "litoral"))
  tabela <-
    dplyr::left_join(tabela, municipio_litoraneos, by = "codigo_municipio")
  return(tabela)
}

#' Adiciona coluna de País
#'
#' Adiciona na tabela de entrada uma coluna contendo o nome do País.
#'
#' @param tabela
#'
#' @return tabela com uma coluna contendo a palabra "Brasil" em todas linhas
#' @export
#'
#' @examples
#' codigo_municipio <- c("1200013", "1200054", "1200104", "1200138")
#' input <- dplyr::tibble(codigo_municipio)
#' tabela <- adiciona_pais(input)
#'
adiciona_pais <- function(tabela) {
  tabela <-
    dplyr::mutate(tabela, pais = "Brasil")
  return(tabela)
}

#' Converte codigo de munícipio com 6digitos para codigo completo do IBGE
#'
#' @param tabela contendo codigo de 6 digitos
#' @param nome_campo nome do campo que contém o codigo de 6 digitos
#'
#' @return uma tabela contendo o campo codigo_municipio (código IBGE)
#' @export
#'
#' @examples
#' codes <- c("110001", "110002")
#' tabela <- dplyr::tibble(codes)
#' tabela <- codigo6_para_codigo_ibge(tabela, "codes")
codigo6_para_codigo_ibge <- function(tabela, nome_campo) {
  data(municipio, package = "rsan")
  municipio <- dplyr::mutate(municipio, codigo6 = strtrim(codigo_municipio, 6))
  municipio <- dplyr::select(municipio, "codigo6", "codigo_municipio")
  tabela <- dplyr::left_join(tabela, municipio, by = c("Código" = "codigo6"))
}
