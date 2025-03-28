#' Cria armazenamento local dos dados do SNIS consolidado
#'
#' @return NULL
#' @export
create_snis <- function() {
  data("snis2020", package = "rsan")
  df_snis2020 <- snis2020
  save(df_snis2020, file = get_data_path("snis_2020"))

  data("snis2021", package = "rsan")
  df_snis2021 <- snis2021
  save(df_snis2021, file = get_data_path("snis_2021"))

  data("snis2022", package = "rsan")
  df_snis2022 <- snis2022
  save(df_snis2022, file = get_data_path("snis_2022"))

  nome <- c("SNIS_2022", "SNIS 2021", "SNIS 2020")
  ano <- c(2022, 2021, 2020)
  caminho <- c("snis_2022", "snis_2021", "snis_2020")
  snis <- data.frame(nome, ano, caminho)
  save(snis, file = get_data_path("snis"))
}


#' Verifica a integridade dos dados do SNIS
#'
#' @return um `logical` sendo `TRUE` integridade OK.
#' @export
integrity_snis <- function() {
  pop <- load_data("snis")
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
#' @return um `data.frame` contendo os dados do SNIS
#' @export
get_snis_data <- function(snis, fields) {
  df <- load_data(snis)
  df <- dplyr::select(df, dplyr::all_of(fields))
  return(df)
}
