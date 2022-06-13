library(ipeadatar)


#' Indice IGP-DI
#'
#' Retorna o indíce IGP-DI mês a mês até o dia mais atual.
#' Fonte: http://www.ipeadata.gov.br/
#'
#' @return tabela contendo o indice IGP-DI mais atualizado, contendo os campos:
#' \itemize{
#'   \item{data}{é a Data}
#'   \item{igp}{é o valor do índice}
#' }
#' @export
#'
#' @examples
#' igp <- get_igp()
get_igp <- function(){
  igp_code <- "IGP12_IGPDI12"
  igp <- ipeadatar::ipeadata(code=igp_code)
  igp <- igp[c("date", "value")]
  colnames(igp) <- c("data", "igp")
  return(igp)
}

#' Calcula a taxa de correção de preço usando o IGP
#'
#' A taxa inclui o mês da data inicial
#' @param igp é a tabela do IGP.
#' @param data_inicial é um date contendo a data inicial
#' @param data_final é um date contendo a data final
#'
#' @return a taxa que deve ser multiplicada pelo valor em R$ para corrígi-lo.
#' @export
#'
#' @examples
#' data(igp)
#' valor <- function(igp, as.Date("2016-01-01"), as.Date("2026-01-01"))
get_taxa_igp <- function(igp, data_inicial, data_final){
  i <- which.min(abs(igp$data-data_inicial))
  f <- which.min(abs(igp$data-data_final))
  igp_i <- as.double(igp[i-1,2])
  igp_f <- as.double(igp[f,2])
  return(igp_f/igp_i)
}
