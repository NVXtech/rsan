tabela_consolidada_vazia <- function() {
    tabela <- dplyr::tibble(
        "estado" = character(),
        "regiao" = character(),
        "situacao" = character(),
        "destino" = character(),
        "etapa" = character(),
        "necessidade_investimento" = double()
    )
}

#' Consolida as necessidade de investimento em saneamento em uma tabela longa
#' @param state estrutura de dados (`list`) que guarda o estado atual da aplicação
#'
#' @return estado da aplicação (`list`) que guarda todos os resultado e parâmetros dos cálculos
#' @export
#'
#' @examples
#' \dontrun{
#' state <- consolida_investimentos(state)
#' }
consolida_investimentos_tabela_longa <- function(state) {
    componentes <- c("agua", "esgoto", "residuos", "drenagem")
    chaves <- c("estado", "regiao")
    colunas <- c(chaves, "situacao", "destino", "etapa", "investimento")

    tabelas_para_juntar <- c()
    for (componente in componentes) {
        nome_componente <- paste0(componente, "_longa")
        tbl <- dplyr::select(state[[nome_componente]], dplyr::all_of(colunas))
        tbl <- dplyr::mutate(tbl, componente = componente)
        tabelas_para_juntar <- c(tabelas_para_juntar, tbl)
    }
    tabela <- dplyr::bind_rows(tabelas_para_juntar)
    tabela <- rsan:::adiciona_pais(tabela)
    state$geral_longa <- tabela
    return(state)
}
