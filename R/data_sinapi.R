library(lubridate)
#' Title
#'
#' @return
#' @export
#'
#' @examples
get_last_month_and_year <- function() {
  last_date <- floor_date(as.Date(Sys.Date()), "month") - months(1)
  return(list(
    year = substr(last_date, 1, 4),
    month = substr(last_date, 6, 7)
  ))
}

#' Title
#'
#' @param year
#' @param month
#'
#' @return
#' @export
#'
#' @examples
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


#' Title
#'
#' @return
#' @export
#'
#' @examples
create_sinapi <- function() {
  data("sinapi202112")
  sinapi_202112 <- sinapi202112
  save(sinapi_202112, file = rsan:::get_data_path("sinapi_202112"))

  nome <- c("SINAPI 2021-12")
  ano <- c(2020)
  caminho <- c("sinapi_202112")
  sinapi <- data.frame(nome, ano, caminho)

  save(sinapi, file = rsan:::get_data_path("sinapi"))
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
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
