#' Último ano e mês
#'
#' @return uma `list` com o último ano e mes
#' @export
get_last_month_and_year <- function() {
  last_date <- lubridate::floor_date(as.Date(Sys.Date()), "month") - months(1)
  return(list(
    year = substr(last_date, 1, 4),
    month = substr(last_date, 6, 7)
  ))
}

#' Baixa dados do SINAPI
#'
#' Baixa dados do SINAPI para um determinado ano e mês

#' @param year é um `number` com o ano desejado
#' @param month é um `number` com o mês desejado
#'
#' @return um `data.frame` com os dados do SINAPI
#' @export
download_sinapi <- function(year, month) {
  type <- "NaoDesonerado"
  inputs <- dplyr::tibble()
  compositions <- dplyr::tibble()
  for (state in states_acronym()) {
    url <- paste0(
      "https://www.caixa.gov.br/Downloads/sinapi-a-partir-jul-2009-", tolower(state),
      "/SINAPI_ref_Insumos_Composicoes_", state, "_", month, year, "_", type, ".zip"
    )
    tmp_file <- tempfile()
    tmp_dir <- tempdir()
    curl::curl_download(url, tmp_file)
    utils::unzip(tmp_file, exdir = tmp_dir)

    unlink(tmp_file)
    unlink(tmp_dir)
  }
}


#' Armazena dados do SINAPI
#'
#' @return NULL
#' @export
create_sinapi <- function() {
  data("sinapi_202112", package = "rsan")
  sinapi_202112 <- get("sinapi_202112")
  save(sinapi_202112, file = rsan:::get_data_path("sinapi_202112"))

  nome <- c("SINAPI 2021-12")
  ano <- c(2020)
  caminho <- c("sinapi_202112")
  sinapi <- data.frame(nome, ano, caminho)

  save(sinapi, file = rsan:::get_data_path("sinapi"))
}

#' Armazena dados sinapi
#'
#' @return um `logical` sendo `TRUE` integridade OK.
#' @export
integrity_sinapi <- function() {
  pop <- rsan:::load_data("sinapi")
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

#' Retorna dados do SINAPI
#'
#' @param sinapi caminho do dados do SINAPI
#'
#' @return um tibble() com dados do SINAPI
#' @export
#'
#' @examples
#' \dontrun{
#' df <- get_sinapi_data(sinapi)
#' }
get_sinapi_data <- function(sinapi) {
  df <- rsan:::load_data(sinapi)
  return(df)
}
