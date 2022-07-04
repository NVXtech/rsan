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
#' path <- get_data_dir("populacao")
get_data_dir <- function() {
  root <- system.file("data", package = "rsan")
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
