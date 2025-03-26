# GLOBAL VARIABLES
state_file_name <- "app_state.rda"
saving <- FALSE

#' Carrega o estado do aplicativo
#' caso nunca tenha sido aberto
#' cria um estado padrão
#'
#' @return estado do aplicativo
#' @export
#'
#' @examples
#' app_state <- load_app_state()
load_app_state <- function() {
  e <- new.env()
  load(file = app_state_filename(), envir = e)
  return(get("app_state", envir = e))
}


#' Local do arquivo de estado da aplicação
#'
#' @return o caminho do arquivo de estado da aplicação
#' @export
#'
#' @examples
#' path <- app_state_file_name()
app_state_filename <- function() {
  file.path(rsan::get_data_dir(), state_file_name)
}

#' Verifica se o arquivo de estado da aplicação ja foi salvo
#'
#' @return `TRUE` se o arquivo existe e `FALSE`caso não exista.
#' @export
#'
#' @examples
#' app_state_exists()
app_state_exists <- function() {
  file.exists(app_state_filename())
}

#' Verifica e cria a pasta de armazenamento
#'
#' Verifica se a pasta de armazenamento existe e caso a pasta não exista ela será criada.
#' @return NULL
#' @export
#'
#' @examples
#' check_and_create_data_folder()
check_and_create_data_folder <- function() {
  if (!file.exists((rsan::get_data_dir()))) {
    dir.create((rsan::get_data_dir()))
  }
}


#' Salva o estado da aplicação
#'
#' @param app_state
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' save_state(app_state)
#' }
save_state <- function(app_state) {
  if (!saving) {
    saving <- TRUE
    check_and_create_data_folder()
    save(app_state, file = app_state_filename())
    saving <- FALSE
  } else {
    warning("Arquivo já esta sendo salvo, nada foi salvo!")
  }
}

#' Padrão de estado da aplicação
#'
#' Cria um estado de aplicação padrão
#' @return estado padrão do aplicativo
#' @export
get_default_state <- function() {
  default_state <- list(
    input = get_default_input()
  )
  return(default_state)
}

#' Verfica e cria estado da aplicação
#'
#' Verifica se o estado da aplicação existe e caso não exista cria uma aplicação com valores padrões.
#' @return estado do aplicativo
#' @export
#' @examples
#' app_state <- check_and_create_state()
check_and_create_state <- function() {
  if (app_state_exists()) {
    return(load_app_state())
  }
  app_state <- get_default_state()
  save_state(app_state)
  return(app_state)
}
