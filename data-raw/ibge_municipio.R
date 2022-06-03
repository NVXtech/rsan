# Codigo Estados e Municípios
library(jsonlite)
library(httr)
library(dplyr)

municipios_url <-
  "https://servicodados.ibge.gov.br/api/v1/localidades/municipios?view=nivelado"

#' Cria dataset com informações gerais
#' sobre os munícipios do Brasil
#' regiões, estado e códigos do IBGE
#'
#' @return
#' @export
#'
#' @examples
#' create_municipio()
create_municipio <- function() {
  res <- httr::GET(municipios_url)
  municipio <- jsonlite::fromJSON(rawToChar(res$content))
  labels <- c("codigo_municipio", "municipio", "codigo_microregiao", "microregiao", "codigo_mesoregiao", "mesoregiao", "codgio_regiao_imediata", "regiao_imediata", "codigo_regiaointermediaria",  "regiaintermediaria", "codigo_UF", "estado_sigla", "estado", "codigo_regiao", "regiao_sigla", "regiao")
  names(municipio) <- labels
  municipio <- dplyr::select(municipio, codigo_municipio, municipio, regiao, regiao_sigla, estado, estado_sigla)
  usethis::use_data(municipio, overwrite=TRUE)
}

create_municipio()
