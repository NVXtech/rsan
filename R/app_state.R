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
  file.path(get_data_dir(), state_file_name)
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
  if (!file.exists((get_data_dir()))) {
    dir.create((get_data_dir()))
  }
}


#' Salva o estado da aplicação
#'
#' @param app_state estrutura de dados (`list`) que guarda o estado atual da aplicação
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

#' Compara o estado atual com o estado salvo
#' Retorna `TRUE` se o estado atual é igual ao estado salvo no arquivo JSON
#'
#' @param state estrutura de dados (`list`) que guarda o estado atual da aplicação
#'
#' @return `TRUE` se o estado atual é igual ao estado salvo, `FALSE` caso contrário
#' @export
compara_state_json <- function(state) {
  json_path <- file.path("dados", "resultados", "parametros.json")
  if (!file.exists(json_path)) {
    rlog::log_info("Arquivo JSON de parâmetros não encontrado")
    return(FALSE)
  }
  saved_state <- jsonlite::read_json(json_path)
  current_state <- state$input
  return(isTRUE(all.equal(saved_state, current_state)))
}


#' Restaura os parâmetros da ultima sessão calculada (JSON)
#'
#' @return um boleano indicando se os parâmetros foram restaurados com sucesso
#' @export
restaura_parametros_json <- function() {
  json_path <- file.path("dados", "resultados", "parametros.json")
  if (!file.exists(json_path)) {
    rlog::log_info("Arquivo JSON de parâmetros não encontrado")
    return(FALSE)
  }
  saved_state <- jsonlite::read_json(json_path)
  app_state <- load_app_state()
  app_state$input <- saved_state
  save_state(app_state)
  rlog::log_info("Parâmetros restaurados com sucesso")
  return(TRUE)
}

#' Restaura os parâmertos padrão
#'
#' @return um boleano indicando se os parâmetros foram restaurados com sucesso
#' @export
restaura_parametros_padrao <- function() {
  app_state <- load_app_state()
  app_state$input <- get_default_input()
  save_state(app_state)
  rlog::log_info("Parâmetros padrão restaurados com sucesso")
  return(TRUE)
}

#' Padrão de estado da aplicação
#'
#' Cria um estado de aplicação padrão
#' @return estado padrão do aplicativo
#' @export
get_default_state <- function() {
  default_state <- list(
    was_run = FALSE,
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
