
#' Transforma depreciacao em porcentagem para vida_util
#'
#' @param valor é o valor da depreciação em porcentagem (ex. 2% o valor = 2)
#'
#' @return valor em anos representando vida útil
#' @export
#'
#' @examples
#' vida_util <- depreciacao_para_vida_util(2)
depreciacao_para_vida_util <- function(valor) {
  return(100.0 / valor)
}
