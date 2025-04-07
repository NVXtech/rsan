#' Soma todas as variáveis numéricas agrupando por faixa populacional
#'
#' @param tabela tabela contendo as colunas de população, estado
#' @param campo_faixa parâmetro opcional para definir o campo
#' que contém a faixa populacional (default: "faixa")
#'
#' @return tabela contendo todas variáveis somadas por faixa populacional
#' @export
#'
#' @examples
#' faixa <- c(rep(1, 2), rep(3, 2), rep(2, 2), rep(4, 2))
#' valor <- c(1, 1, 2, 2, 3, 3, 4, 4)
#' input <- dplyr::tibble(faixa, valor)
#' output <- soma_por_faixa(input)
soma_por_faixa <- function(tabela, campo_faixa = "faixa") {
  tabela <- dplyr::group_by(tabela, .data[[campo_faixa]])
  tabela <-
    dplyr::summarise(tabela,
      dplyr::across(where(is.numeric), \(x) sum(x, na.rm = TRUE)),
      dplyr::across(
        where(is.character),
        \(x) ifelse(length(unique(x)) == 1 &
          length(x) >= 1, dplyr::first(x), NA)
      ),
      .groups = "drop"
    )
  tabela <- tabela[, colSums(is.na(tabela)) < nrow(tabela)]
  return(tabela)
}


#' Média de todas as variáveis numéricas agrupando por faixa populacional
#'
#' @param tabela tabela contendo as colunas de população, estado
#' @param campo_faixa parâmetro opcional para definir o campo
#' que contém a faixa populaciona (default: "faixa")
#'
#' @return tabela contendo a média de todas variáveis por faixa populacional
#' @export
#'
#' @examples
#' faixa <- c(rep(1, 2), rep(3, 2), rep(2, 2), rep(4, 2))
#' valor <- c(1, 1, 2, 2, 3, 3, 4, 4)
#' input <- dplyr::tibble(faixa, valor)
#' output <- media_por_faixa(input)
media_por_faixa <- function(tabela, campo_faixa = "faixa") {
  tabela <-
    dplyr::group_by(tabela, .data[[campo_faixa]])
  tabela <-
    dplyr::summarise(tabela,
      dplyr::across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),
      dplyr::across(
        where(is.character),
        \(x) ifelse(length(unique(x)) == 1 &
          length(x) >= 1, dplyr::first(x), NA)
      ),
      .groups = "drop"
    )
  tabela <- tabela[, colSums(is.na(tabela)) < nrow(tabela)]
  return(tabela)
}

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
    dplyr::across(where(is.numeric), \(x) sum(x, na.rm = TRUE)),
    dplyr::across(
      where(is.character),
      \(x) ifelse(length(unique(x)) == 1 &
        length(x) >= 1, dplyr::first(x), NA)
    ),
    .groups = "drop"
  )
  tabela <- tabela[, colSums(is.na(tabela)) < nrow(tabela)]
  return(tabela)
}


#' Cria faixa populacional nula em grupos
#'
#' É criada uma faixa com valores vazio (NA) quando não existe uma faixa populacional em um grupo.
#'
#' @param tabela tabela contendo as colunas de população, estado
#' @param nome_grupo o agrupando dentro de uma faixa populacional (padrão: "estado")
#'
#' @return tabela contendo a média de todas variáveis por faixa populacional
#' @export
cria_faixas_vazias <- function(tabela, nome_grupo = "estado") {
  tabela <- dplyr::group_by(tabela, .data[[nome_grupo]])
  cria_faixas <- function(group, group_name) {
    faixas_faltantes <- 1:7
    faixas_faltantes <- faixas_faltantes[!faixas_faltantes %in% group$faixa]
    for (faixa in faixas_faltantes) {
      group <- dplyr::add_row(group, faixa = faixa)
    }
    group <- tidyr::fill(group, where(is.character), .direction = "downup")
    return(group)
  }
  tabela <- dplyr::group_modify(tabela, cria_faixas)
  tabela <- dplyr::ungroup(tabela)
  return(tabela)
}


#' Faixa populacional
#'
#' Cria classificação por limites populacionas.
#'
#' @param tabela tabela contendo a colunas de população total.
#' @param limites limites para faixa populacional. Ex: limites<-c(0,1,2) cria as seguintes faixas:
#' \itemize{
#'   \item{faixa 0}{pop<0}
#'   \item{faixa 1}{0<=pop<=1}
#'   \item{faixa 2}{0<pop<=2}
#'   \item{faixa 3}{pop>2}
#' }
#' @param campo_populacao parâmetro opcional para definir o campo que contém a populacao (default: "POP_TOT")
#' @param campo_faixa parâmetro opcional para definir o campo que contém a populacao (default: "faixa")
#'
#' @return tabela contendo um coluna adcional com as faixas populacionais
#' @export
#'
#' @examples
#' limites <- as.integer(c(0, 10e3, 30e3, 100e3, 250e3, 1e6, 4e6))
#' POP_TOT <- as.integer(c(0, 10e3, 30e3, 100e3, 250e3, 1e6, 4e6, 5e6))
#' input <- dplyr::tibble(POP_TOT)
#' output <- classifica_faixa_populacional(input, limites)
classifica_faixa_populacional <-
  function(tabela,
           limites,
           campo_populacao = "populacao_total_corrente",
           campo_faixa = "faixa") {
    tabela <-
      dplyr::mutate(tabela,
        faixa = findInterval(
          tabela[[campo_populacao]],
          limites,
          rightmost.closed = TRUE,
          left.open = TRUE
        )
      )
    colnames(tabela)[colnames(tabela) == "faixa"] <- campo_faixa
    return(tabela)
  }


#' Soma todas as variáveis numéricas agrupando por estado e faixa populacional
#'
#' @param tabela tabela contendo as colunas de população, estado
#' @param campo_estado parâmetro opcional para definir o campo que contém a sigla do estado (default: "Estado")
#' @param campo_faixa parâmetro opcional para definir o campo que contém a faixa populaciona (default: "faixa")
#'
#' @return tabela contendo todas variáveis somadas por grupos de estado e faixa populacional
#' @export
#'
#' @examples
#' Estado <- c(rep("AC", 4), rep("AL", 4))
#' faixa <- c(rep(1, 2), rep(3, 2), rep(2, 2), rep(4, 2))
#' valor <- c(1, 1, 2, 2, 3, 3, 4, 4)
#' input <- dplyr::tibble(Estado, faixa, valor)
#' output <- soma_por_estado_faixa(input)
soma_por_estado_faixa <- function(tabela,
                                  campo_estado = "estado",
                                  campo_faixa = "faixa") {
  tabela <- dplyr::group_by(tabela, .data[[campo_estado]], .data[[campo_faixa]])
  tabela <- dplyr::summarise(tabela,
    dplyr::across(where(is.numeric), \(x) sum(x, na.rm = TRUE)),
    dplyr::across(
      where(is.character),
      \(x) ifelse(length(unique(x)) == 1 &
        length(x) >= 1, dplyr::first(x), NA)
    ),
    .groups = "drop"
  )
  tabela <- tabela[, colSums(is.na(tabela)) < nrow(tabela)]
  return(tabela)
}


#' Conta o número de munícipios que se encontram em cada faixa populacional
#'
#' @param tabela tabela contendo as colunas de estado e faixa
#' @param campo_estado parâmetro opcional para definir o campo que contém a sigla do estado (default: "Estado")
#' @param campo_faixa parâmetro opcional para definir o campo que contém a faixa populaciona (default: "faixa")
#'
#' @return tabela inicial mais a coluna numero_municipios
#' @export
#'
#' @examples
#' Estado <- c(rep("AC", 4), rep("AL", 4))
#' faixa <- c(rep(1, 2), rep(3, 2), rep(2, 2), rep(4, 2))
#' valor <- c(1, 1, 2, 2, 3, 3, 4, 4)
#' input <- dplyr::tibble(Estado, faixa, valor)
#' output <- conta_municipios_por_estado_faixa(input)
conta_municipios_por_estado_faixa <- function(tabela,
                                              campo_estado = "estado",
                                              campo_faixa = "faixa") {
  tabela <-
    dplyr::group_by(tabela, .data[[campo_estado]], .data[[campo_faixa]])
  tabela <-
    dplyr::summarise(tabela,
      numero_municipios = dplyr::n(),
      .groups = "drop"
    )
  return(tabela)
}


#' Média de todas as variáveis numéricas agrupando por estado e faixa populacional
#'
#' @param tabela tabela contendo as colunas de população, estado
#' @param campo_estado parâmetro opcional para definir o campo que
#' contém a sigla do estado (default: "Estado")
#' @param campo_faixa parâmetro opcional para definir o campo que
#' contém a faixa populaciona (default: "faixa")
#'
#' @return tabela contendo a média de todas variáveis
#' por grupos de estado e faixa populacional
#' @export
#'
#' @examples
#' Estado <- c(rep("AC", 4), rep("AL", 4))
#' faixa <- c(rep(1, 2), rep(3, 2), rep(2, 2), rep(4, 2))
#' valor <- c(1, 1, 2, 2, 3, 3, 4, 4)
#' input <- dplyr::tibble(Estado, faixa, valor)
#' output <- media_por_estado_faixa(input)
media_por_estado_faixa <- function(tabela,
                                   campo_estado = "estado",
                                   campo_faixa = "faixa") {
  tabela <-
    dplyr::group_by(tabela, .data[[campo_estado]], .data[[campo_faixa]])
  tabela <-
    dplyr::summarise(tabela,
      dplyr::across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),
      dplyr::across(
        where(is.character),
        \(x) ifelse(length(unique(x)) == 1 &
          length(x) >= 1, dplyr::first(x), NA)
      ),
      .groups = "drop"
    )
  tabela <- tabela[, colSums(is.na(tabela)) < nrow(tabela)]
  return(tabela)
}

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
remove_shiny_classes <- function(lista) {
  for (name in names(lista)) {
    is_shiny_class <- any(grepl("shiny", class(lista[[name]])))
    if (is_shiny_class) {
      lista[[name]] <- NULL
    }
  }
  return(lista)
}
