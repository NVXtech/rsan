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
  igp_code <- "IGP12_INCC12" # INCC-DI
  igp <- ipeadatar::ipeadata(code=igp_code)
  igp <- igp[c("date", "value")]
  colnames(igp) <- c("data", "igp")
  return(igp)
}

#' Calculo da taxa de correção de preço - IGP
#'
#' A função retorna a taxa de correção de preço para um determinado intervalo de datas.
#' O algoritmo busca as taxas mais próximas das datas passadas e retorna a taxa de correção.
#'
#'
#' @param igp é um `data.frame` com a série histórica do IGP com as colunas data e igp(taxa).
#' @param data_inicial é um `date` contendo a data em que o preço foi atribuido
#' @param data_final é um `date` contendo uma data mais recente pra qual se quer corrigir o preço
#'
#' @return um `double` que é a taxa pela qual o preço deve ser multiplicada para corrígi-lo.
#' @export
#'
#' @examples
#' data(igp)
#' valor <- function(igp, as.Date("2016-01-01"), as.Date("2021-01-01"))
get_taxa_igp <- function(data_inicial, data_final){
  igp <- get_igp()
  len_ini <- length(data_inicial)
  len_fin <- length(data_final)
  if (len_ini != len_fin){
    stop("data_incial e data_final devem ter mesmo tamanho")
  }
  igp_i <- double(len_ini)
  igp_f <- double(len_ini)
  for(j in 1:len_ini){
    i <- which.min(abs(igp$data-data_inicial[j]))
    f <- which.min(abs(igp$data-data_final[j]))
    igp_i[j] <- as.double(igp[i-1,2])
    igp_f[j] <- as.double(igp[f,2])
  }
  return(igp_f/igp_i)
}

