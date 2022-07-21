# Codigo Estados e Municípios
library(jsonlite)

municipios_url <-
  "https://servicodados.ibge.gov.br/api/v1/localidades/municipios?view=nivelado"

#' Cria dataset com informações gerais
#' sobre os munícipios do Brasil
#' regiões, estado e códigos do IBGE
#'
#' @return NULL
#' @export
#'
#' @examples
#' create_municipio()
create_municipio <- function() {
  res <- httr::GET(municipios_url)
  municipio <- jsonlite::fromJSON(rawToChar(res$content))
  labels <- c("codigo_municipio", "municipio", "codigo_microregiao", "microregiao", "codigo_mesoregiao", "mesoregiao", "codgio_regiao_imediata", "regiao_imediata", "codigo_regiaointermediaria", "regiaintermediaria", "codigo_UF", "UF", "Estado", "codigo_regiao", "regiao_sigla", "regiao")
  names(municipio) <- labels
  save(municipio, file = rsan::get_data_path("municipio"))
}

#' Verifica a integridade dos dados locais de munícipio
#'
#' @return um `logical` sendo `TRUE` integridade OK.
#' @export
#'
#' @examples
#' is_ok <- integrity_muncipio()
integrity_municipio <- function() {
  # TODO: create an better integrity check
  exists <- rsan::check_data_exists("municipio")
  if (exists) {
    rlog::log_info("municipio dataset is OK")
  } else {
    rlog::log_warn("integrity check failed to municipio dataset")
  }
  return(exists)
}
