#' Soma dados agrupados pelo valor de um determinado campo
#'
#' Soma todas os campos numéricos, os campos de caracteres são mantidos se únicos por grupo.
#'
#' @param tabela um `data.frame` com as colunas para serem somadas e com o nome do campo
#' @param campo um `character` contendo o nome do campo para agrupar
#'
#' @return a mesma tabela de entrada mais agrupada pelo campo
#' @export
somar_por_campo <- function(tabela, campo) {
    tabela <- dplyr::group_by(tabela, .data[[campo]])
    tabela <- dplyr::summarise(
        tabela,
        dplyr::across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
        dplyr::across(
            where(is.character),
            ~ ifelse(length(unique(.x)) == 1 &
                length(.x) >= 1, dplyr::first(.x), NA)
        ),
        .groups = "drop"
    )
    tabela <- tabela[, colSums(is.na(tabela)) < nrow(tabela)]
    return(tabela)
}
