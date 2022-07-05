#' Cria armazenamento local dos dados do SNIS consolidado
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' create_snis()
#' }
create_snis <- function() {
  data("snis2020", package = "rsan")
  df_snis <- snis2020
  save(df_snis, file = rsan:::get_data_path("snis_2020"))

  nome <- c("SNIS 2020")
  ano <- c(2020)
  caminho <- c("snis_2020")
  snis <- data.frame(nome, ano, caminho)

  save(snis, file = rsan:::get_data_path("snis"))
}


#' Verifica a integridade dos dados do SNIS
#'
#' @return
#' @export
#'
#' @examples
integrity_snis <- function() {
  pop <- rsan:::load_data("snis")
  for (caminho in pop$caminho) {
    if (!file.exists(get_data_path(caminho))) {
      rlog::log_info(sprintf("%s dataset not found", caminho))
      return(FALSE)
    } else {
      rlog::log_info(sprintf("%s dataset is OK", caminho))
    }
  }
  return(TRUE)
}

#' Retorna dados do SNIS
#'
#' @param snis caminho do SNIS
#' @param fields lista com campos a serem retornados
#'
#' @return um tibble() contendo os dados do SNIS
#' @export
#'
#' @examples
#' \dontrun{
#' df <- get_snis_data("snis_2020", c("AG001", "AG010"))
#' }
get_snis_data <- function(snis, fields) {
  df <- rsan:::load_data(snis)
  df <- dplyr::select(df, dplyr::all_of(fields))
  return(df)
}
