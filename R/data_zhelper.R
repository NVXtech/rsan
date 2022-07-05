datasets <- c("populacao", "municipio", "snis", "sinapi", "projeto")

create_functions <- list(
  "populacao" = rsan:::create_populacao,
  "municipio" = rsan:::create_municipio,
  "snis" = rsan:::create_snis,
  "sinapi" = rsan:::create_sinapi,
  "projeto" = rsan:::create_projeto
)

integrity_functions <- list(
  "populacao" = rsan:::integrity_populacao,
  "municipio" = rsan:::integrity_municipio,
  "snis" = rsan:::integrity_snis,
  "sinapi" = rsan:::integrity_sinapi,
  "projeto" = rsan:::integrity_projeto
)

#' Verifica se o conjunto de dados existe
#'
#' @param name é um `character` com o nome do conjunto de dados
#'
#' @return boleano sendo TRUE conjunto de dados encontrado e FALSE conjunto inexistente
#' @export
dataset_exists <- function(name) {
  file.exists(rsan:::get_data_path(name))
}

#' Cria um dataset
#'
#' @param name é um `character` com o nome do conjunto de dados
#'
#' @return
#' @export
create_dataset <- function(name) {
  rlog::log_info(sprintf("Creating %s dataset..", name))
  create_functions[[name]]()
}

#' Limpa o armazenamendo do conjunto de dados
#'
#' @param name é um `character` com o nome do conjunto de dados
#'
#' @return
#' @export
clean_dataset <- function(name) {
  rlog::log_info(sprintf("Cleaning %s dataset..", name))
  files <- list.files(get_data_dir())
  for (file in files) {
    if (grepl(sprintf("^%s.*", name), file)) {
      base::unlink(file.path(get_data_dir(), file))
    }
  }
}

#' Verifica a integridade do conjunto de dados
#'
#' @param name é um `character` com o nome do conjunto de dados
#'
#' @return
#' @export
check_dataset_integrity <- function(name) {
  rlog::log_info(sprintf("Checking %s dataset..", name))
  return(integrity_functions[[name]]())
}

#' Verifica a integridade e cria amarzenamento de conjunto de dados
#'
#' @return
#' @export
check_and_create_datasets <- function() {
  for (dataset in datasets) {
    if (!dataset_exists(dataset)) {
      create_dataset(dataset)
    } else {
      if (!check_dataset_integrity(dataset)) {
        rlog::log_warn(sprintf("Data integrity checked failed for %s", dataset))
        # clean_dataset(dataset)
        # create_dataset(dataset)
      }
    }
  }
}
