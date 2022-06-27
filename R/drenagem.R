#' Investimento total
#'
#' Soma os investimentos de expansao e resposicao
#'
#' @param tabela contendo os campos `investimento_expansao` e `investimento_reposicao`
#'
#' @return tabela com a soma dos investimento no campo `investimento_total`
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
investimento_total <- function(tabela) {
  # TODO: fazer capacidade intalada
  tabela <- dplyr::mutate(
    tabela,
    investimento_total = investimento_expansao + investimento_reposicao
  )
  return(tabela)
}

#' Capacidade instalada para drenagem
#'
#' @param tabela `data.frame` contendo os campos:
#'
#' @return um `data.frame` contendo o campo `capacidade instalada`
#' @export
#'
#' @examples
#' \dontrun{
#' tabela <- capacidade_instalada_drenagem(tabela)
#' }
capacidade_instalada_drenagem <- function(tabela) {
  # TODO: fazer capacidade intalada
  tabela <- dplyr::mutate(
    tabela,
    capacidade_instalada = investimento_expansao * 0.0
  )
  return(tabela)
}

#'
#'
#' @param
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
aplica_regressao_drenagem <- function(tabela, modelo) {
  tabela <- dplyr::mutate(
    tabela,
    modelo = stats::predict(modelo, tabela),
    investimento_modelo = modelo * GE006,
    investimento_expansao = investimento_modelo # TODO arrumar para trocar pelo investimento do plano
  )
  return(tabela)
}

#' Regressão Investimento em Drenagem
#'
#' Calcula a regressão linear entre densidade de investimento em drenagem e o índice PD
#'
#' @param plano é um `data.frame` contendo os valores do plano
#'
#' @return objeto da classe `lm` contendo os parâmetros da regressão
#' @export
#'
#' @examples
#' \dontrun{
#' equacao <- regressao_drenagem(plano)
#' }
regressao_drenagem <- function(plano) {
  # TODO: corrigir com investimento anterior
  plano <- dplyr::mutate(
    plano,
    densidade_investimento = investimento_corrigido / GE006
  )
  return(stats::lm(formula = densidade_investimento ~ pd, plano))
}

#' Concatena dados para regressão
#'
#' @param plano um `data.frame` contendo as colunas `codigo_muncipio` e 'investimento_corrigido'
#' @param tabela um `data.frame` contendo as colunas `codigo_muncipio`, `pd` e `GE006`
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' plano <- prepara_regressao(plano, tabela)
#' }
prepara_regressao <- function(plano, tabela) {
  tabela <- dplyr::select(tabela, "codigo_municipio", "pd", "GE006")
  plano <- dplyr::left_join(plano, tabela, by = "codigo_municipio")
}

#' Corrige preços do plano de drenagem
#'
#' Realiza a correção dos preços para data passada.
#'
#' @param data um `Date` contendo a data para correção do preço
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
corrige_plano_drenagem <- function(data) {
  data(plano_drenagem)
  igp <- rsan::get_igp()
  plano_drenagem <- dplyr::mutate(
    plano_drenagem,
    data_inicial = as.Date(paste0(as.character(ano_plano), "-06-30")),
    data_final = as.Date(data),
    taxa_igp = rsan::get_taxa_igp(igp, data_inicial, data_final), # fix get_taxa_igp
    investimento_corrigido = investimento * taxa_igp
  )
  return(plano_drenagem)
}

#' Calcula o coeficiente PD
#'
#' O coeficiente PD é a multiplicação entre a moda da precipitação(mm) e a densidadade urbana (hab/km²) dividos por 1000.
#'
#' @param tabela é um `data.frame` contendo as colunas `precipitacao_moda` e `densidade urbana`
#'
#' @return um `data.frame` contendo a coluna `pd`
#' @export
#'
#' @examples
#' tabela <- dplyr::tibble(precipitacao_moda = c(1, 1), densidade_urbana = c(2, 2))
#' tabela <- coeficiente_pd(tabela)
#'
coeficiente_pd <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    pd = precipitacao_moda * densidade_urbana / 1e3
  )
  return(tabela)
}

#' Precipitação
#'
#' Adiciona a coluna de precipitacao_moda na tabela de acordo com o munícipio.
#'
#' @param tabela um `data.frame` contendo a coluna `codigo_municipio`.
#'
#' @return a tabela inicial (`data.frame`) com a coluna `precipitacao_moda`
#' @export
#'
#' @examples
#' \dontrun{
#' tabela <- precipitacao(tabela)
#' }
precipitacao <- function(tabela) {
  data(pluviometria)
  pluviometria <- dplyr::select(
    pluviometria,
    codigo_municipio,
    precipitacao_moda
  )
  tabela <- dplyr::left_join(
    tabela,
    pluviometria,
    by = "codigo_municipio"
  )
  return(tabela)
}

#' Densidade urbana
#'
#' @param tabela contendo as colunas do SNIS: `GE006` e `GE002`
#'
#' @return uma tabela contendo a densidade urbana
#' @export
#'
#' @examples
#' \dontrun{
#' tabela <- densidade_urbana(tabela)
#' }
densidade_urbana <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    densidade_urbana = GE006 / GE002
  )
  return(tabela)
}

