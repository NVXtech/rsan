library(ipeadatar)


#' Retorna a taxa de correção de preços
#'
#' Retorna o índice IGP-DI mês a mês até o dia mais atual.
#' Fonte: http://www.ipeadata.gov.br/
#'
#' @param ipea_code Código do índice no IPEA Data.
#' @return Um `data.frame` contendo o índice IGP-DI mais atualizado, com as colunas:
#' \itemize{
#'   \item{data}{Data do índice.}
#'   \item{igp}{Valor do índice.}
#' }
#' @export
#'
#' @examples
#' igp <- taxa_correcao_ipea("IGP12_INCC12")
taxa_correcao_ipea <- function(ipea_code) {
  taxa <- ipeadatar::ipeadata(code = ipea_code)
  taxa <- taxa[c("date", "value")]
  colnames(taxa) <- c("data", "igp")
  return(taxa)
}


#' Fator de Correção de Preços - IPEA
#'
#' Calcula o fator de correção de preços com base no índice fornecido pelo IPEA para um intervalo de datas.
#'
#' @param ipea_code Código do índice no IPEA Data.
#' @param data_inicial Um vetor de datas (`Date`) contendo as datas iniciais para o cálculo.
#' @param data_final Um vetor de datas (`Date`) contendo as datas finais para o cálculo.
#'
#' @return Um vetor numérico (`double`) com os fatores de correção correspondentes para cada par de datas.

#' @export
#' @examples
#' data_inicial <- as.Date(c("2016-01-01", "2017-01-01"))
#' data_final <- as.Date(c("2021-01-01", "2022-01-01"))
#' fator <- fator_correcao_ipea("IGP12_INCC12", data_inicial, data_final)
fator_correcao_ipea <- function(ipea_code, data_inicial, data_final) {
  indice <- taxa_correcao_ipea(ipea_code)
  len_ini <- length(data_inicial)
  len_fin <- length(data_final)
  if (len_ini != len_fin) {
    stop("data_incial e data_final devem ter mesmo tamanho")
  }
  indice_i <- double(len_ini)
  indice_f <- double(len_ini)
  for (j in 1:len_ini) {
    i <- which.min(abs(indice$data - data_inicial[j]))
    f <- which.min(abs(indice$data - data_final[j]))
    indice_i[j] <- as.double(indice[i - 1, 2])
    indice_f[j] <- as.double(indice[f, 2])
  }
  return(indice_f / indice_i)
}


#' Fator de Correção de Preços - INCC
#'
#' Calcula o fator de correção de preços com base no índice INCC-DI para um intervalo de datas.
#'
#' @param data_inicial Um vetor de datas (`Date`) contendo as datas iniciais para o cálculo.
#' @param data_final Um vetor de datas (`Date`) contendo as datas finais para o cálculo.
#'
#' @return Um vetor numérico (`double`) com os fatores de correção correspondentes para cada par de datas.
#'
#' @export
#' @examples
#' data_inicial <- as.Date(c("2016-01-01", "2017-01-01"))
#' data_final <- as.Date(c("2021-01-01", "2022-01-01"))
#' fator <- fator_correcao_incc(data_inicial, data_final)
fator_correcao_incc <- function(data_inicial, data_final) {
  igp_code <- "IGP12_INCC12" # INCC-DI
  return(fator_correcao_ipea(igp_code, data_inicial, data_final))
}


#' Fator de Correção de Preços - IPCA
#'
#' Calcula o fator de correção de preços com base no índice IPCA para um intervalo de datas.
#'
#' @param data_inicial Um vetor de datas (`Date`) contendo as datas iniciais para o cálculo.
#' @param data_final Um vetor de datas (`Date`) contendo as datas finais para o cálculo.
#'
#' @return Um vetor numérico (`double`) com os fatores de correção correspondentes para cada par de datas.
#'
#' @export
#' @examples
#' data_inicial <- as.Date(c("2016-01-01", "2017-01-01"))
#' data_final <- as.Date(c("2021-01-01", "2022-01-01"))
#' fator <- fator_correcao_ipca(data_inicial, data_final)
fator_correcao_ipca <- function(data_inicial, data_final) {
  ipca_code <- "PRECOS12_IPCA12" # IPCA
  return(fator_correcao_ipea(ipca_code, data_inicial, data_final))
}
