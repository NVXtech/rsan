#' Remove items com classes shiny
#'
#' Percorre todos os itens procurando itens em que a classe contenha shiny no nome e as remove.
#'
#' @param lista
#'
#' @return list sem items que são classes shiny
#' @export
#'
#' @examples
#' a <- list()
#' remove_shiny_classes(a)
remove_shiny_classes <- function (lista) {
  for (name in names(lista)){
    is_shiny_class <- any(grepl("shiny", class(lista[[name]])))
    if (is_shiny_class){
      lista[[name]] <- NULL
    }
  }
  return(lista)
}
