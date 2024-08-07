#' Armazena dados dos projetos tipos
#'
#' @return NULL
#' @export
create_projeto <- function() {
  projetos <-
    c(
      "projeto_coleta_esgoto",
      "projeto_distribuicao_agua",
      "projeto_producao_agua",
      "projeto_producao_agua_unidades",
      "projeto_tratamento_esgoto",
      "projeto_tratamento_esgoto_unidades",
      "projeto_predominancia_tipo_producao"
    )

  nome <- c()
  caminho <- c()
  for (projeto in projetos) {
    rlog::log_info(sprintf("Creating %s", projeto))
    e <- new.env()
    data(list = c(projeto), package = "rsan", envir = e)
    save(list = c(projeto), file = get_data_path(projeto), envir = e)
    nome <- c(nome, gsub("_", " ", projeto))
    caminho <- c(caminho, projeto)
  }

  projeto <- data.frame(nome, caminho)
  save(projeto, file = get_data_path("projeto"))
}


#' Verifica integridade dos dados dos projetos tipos
#'
#' @return um `logical` sendo `TRUE` integridade OK.
#' @export
integrity_projeto <- function() {
  pop <- load_data("projeto")
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
