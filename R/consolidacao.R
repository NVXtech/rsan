tabela_necessidade_vazia <- function() {
    tabela <- dplyr::tibble(
        "componente" = character(),
        "estado" = character(),
        "regiao" = character(),
        "situacao" = character(),
        "destino" = character(),
        "subsistema" = character(),
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
    colunas <- c(chaves, "situacao", "destino", "subsistema", "investimento")

    tabelas_para_juntar <- c()
    for (componente in componentes) {
        nome_componente <- paste0(componente, "_longa")
        tbl <- dplyr::select(state[[nome_componente]], dplyr::all_of(colunas))
        tbl <- dplyr::mutate(tbl, componente = componente)
        tabelas_para_juntar <- c(tabelas_para_juntar, tbl)
    }
    tabela <- dplyr::bind_rows(tabelas_para_juntar)
    tabela <- rsan::adiciona_pais(tabela)
    state$necessidade <- tabela
    return(state)
}

#' Prepara tabela para plotar gráfico estilo sankey
#' @param tabela o `data.frame` longo com dados de investimento
#' @param colunas um `vector` com a lista de coluns para serem utilizadas.
#' Os items podem ser "situacao", "destino", "componente", "subsistema".
#' @return uma lista (`list`) que guarda todos os dados para o sankey plot
#' @export
prepara_sankey <- function(tabela, colunas) {
    # cria lista de fontes
    n <- length(colunas)
    source_names <- c()
    for (i in 1:n) {
        tmp <- dplyr::distinct(
            tabela, .data[[colunas[i]]],
        )
        source_names <- c(source_names, tmp[[colunas[i]]])
        rm(tmp)
    }
    sources <- data.frame(
        name = source_names, ind = 0:(length(source_names) - 1)
    )
    consolidado <- dplyr::tibble(
        "source_name" = character(),
        "target_name" = character(),
        "necessidade_investimento" = double()
    )
    n <- length(colunas) - 1
    for (i in 1:n) {
        tmp <- dplyr::group_by(
            tabela, .data[[colunas[i]]], .data[[colunas[i + 1]]]
        )
        tmp <- dplyr::summarise(
            tmp,
            necessidade_investimento = sum(
                necessidade_investimento,
                na.rm = TRUE
            )
        )
        tmp <- dplyr::rename(
            tmp,
            source_name = .data[[colunas[i]]],
            target_name = .data[[colunas[i + 1]]]
        )
        consolidado <- dplyr::bind_rows(consolidado, tmp)
        rm(tmp)
    }
    consolidado <- dplyr::left_join(
        consolidado, sources,
        by = c("source_name" = "name")
    )
    consolidado <- dplyr::rename(
        consolidado,
        source = ind
    )
    consolidado <- dplyr::left_join(
        consolidado, sources,
        by = c("target_name" = "name")
    )
    consolidado <- dplyr::rename(
        consolidado,
        target = ind
    )
    output <- list(
        labels = sources$name,
        link = list(
            source = consolidado$source,
            target = consolidado$target,
            values = consolidado$necessidade_investimento
        )
    )
    return(output)
}

tabela_deficit_vazia <- function() {
    tabela <- dplyr::tibble(
        "estado" = character(),
        "regiao" = character(),
        "componente" = character(),
        "situacao" = character(),
        "subsistema" = character(),
        "deficit" = integer()
    )
}
