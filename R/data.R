#' SNIS 2020
#'
#' Dataset contendo as informações e indicadores do SNIS do ano de 2020.
#'
#' @format A data frame with 5570 rows and 608 variables
#' \describe{
#'   \item{codigo_municipio}{codigo ibge do municipio}
#'   \item{`CODIGO`}{codigo do indicador ou informacao do SNIS ex: AG001}
#'   ... todas variáveis do snis
#' }
#' @source \url{http://www.snis.gov.br/}
"snis2020"

#' SINAPI Dezembro de 2021
#'
#' Dataset contendo preços e índices para a construção civil.
#'
#' @format Um data frame com 12414 items e 31 variáveis
#' \describe{
#'   \item{TIPO}{tipo do item}
#'   \item{CODIGO}{código do item}
#'   \item{DESCRICAO}{descrição do item}
#'   \item{UNIDADE}{unidade de medida (ex: m)}
#'   \item{PRECO_XX}{preço do estado com sigla XX}
#' }
#' @source \url{https://www.caixa.gov.br/poder-publico/modernizacao-gestao/sinapi/}
"sinapi202112"

#' Municípios do Brasil
#'
#' Dataset contendo lista de municípios, códigos ibge e regiões
#'
#' @format Um data frame com 5570 municípios e 16 colunas com as seguintes informações adicionais:
#' \describe{
#'   \item{TIPO}{tipo do item}
#'   \item{CODIGO}{código do item}
#'   \item{DESCRICAO}{descrição do item}
#'   \item{UNIDADE}{unidade de medida (ex: m)}
#'   \item{PRECO_XX}{preço do estado com sigla XX}
#' }
#' @source \url{https://www.caixa.gov.br/poder-publico/modernizacao-gestao/sinapi/}
"municipio"
