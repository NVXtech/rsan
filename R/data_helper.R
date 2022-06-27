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

#' Title
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples
dataset_exists <- function(name) {
  file.exists(get_data_path(name))
}

#' Title
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples
create_dataset <- function(name) {
  rlog::log_info(sprintf("Creating %s dataset..", name))
  create_functions[[name]]()
}

#' Title
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples
clean_dataset <- function(name) {
  rlog::log_info(sprintf("Cleaning %s dataset..", name))
  files <- list.files(get_data_dir())
  for (file in files) {
    if (grepl(sprintf("^%s.*", name), file)) {
      base::unlink(file.path(get_data_dir(), file))
    }
  }
}

#' Title
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples
check_dataset_integrity <- function(name) {
  rlog::log_info(sprintf("Checking %s dataset..", name))
  return(integrity_functions[[name]]())
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
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
