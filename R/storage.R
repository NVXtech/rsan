data_root <- "data"

#' Retorna o caminho de um conjunto de dados local
#'
#' @param name nome do conjunto de dados
#'
#' @return path to dataset .rda file
#' @export
#'
#' @examples
#' path <- get_data_path("populacao")
get_data_path <- function(name) {
  file.path(get_data_dir(), sprintf("%s.rda", name))
}

#' get dataset dir
#'
#' @return The directory where dataset is stored
#' @export
#'
#' @examples
#' path <- get_data_dir()
get_data_dir <- function() {
  root <- file.path(getwd(), "data")
  return(root)
}

#' load app dataset
#'
#' @param name The variable name
#'
#' @return the content of the dataset
#' @export
#'
#' @examples
#' populacao <- load_data("populacao")
load_data <- function(name) {
  tmp <- new.env()
  load(file = get_data_path(name), envir = tmp)
  return(tmp[[ls(tmp)[1]]])
}

#' check dataset file exists
#'
#' @param name The variable name
#'
#' @return If file exists (boolean)
#' @export
#'
#' @examples
#' check_data_exists("populacao")
check_data_exists <- function(name) {
  return(file.exists(file.path(get_data_dir(), sprintf("%s.rda", name))))
}

#' Retorna o caminho de um conjunto de dados
#'
#' @param nome é o nome do conjunto de dados
#'
#' @return o local do arquivo
#' @export
caminho_do <- function(nome) {
  file.path(get_data_dir(), sprintf("%s.xlsx", nome))
}

#' Transforma datasets da biblioteca em xlsx
#'
#' @return NULL
#' @export
cria_armazenamento_padrao <- function() {
  dir.create(get_data_dir(), showWarnings = FALSE)
  dados <- c(
    "agua_esgoto_rural",
    "area_municipio",
    "area_urbana_municipio",
    "indices_drenagem",
    "municipio_litoraneos",
    "municipio",
    "plano_drenagem",
    "plansab",
    "pluviometria",
    # "populacao_censo_2010",
    # "populacao_estimada_2021",
    "potencial_regionalizacao",
    "projeto_coleta_esgoto",
    "projeto_distribuicao_agua",
    "projeto_predominancia_tipo_producao",
    "projeto_producao_agua_unidades",
    "projeto_producao_agua",
    "projeto_tratamento_esgoto_unidades",
    "projeto_tratamento_esgoto",
    "sinapi_202112",
    # "snis_2020",
    "snis_ap",
    "snis_rs",
    "snis2020",
    "tipo_disposicao"
  )
  test <- data(list = dados, package = "rsan")
  print(test)
  for (dado in dados) {
    tmp <- get(dado)
    salvar_em <- caminho_do(dado)
    writexl::write_xlsx(tmp, path = salvar_em)
  }
}
