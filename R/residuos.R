#' Faixa populaciona;
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
#' POP_TOT <-as.integer(c(0, 10e3, 30e3, 100e3, 250e3, 1e6, 4e6, 5e6))
#' input <- dplyr::tibble(POP_TOT)
#' output <- classifica_faixa_populacional(input, limites)
classifica_faixa_populacional <-
  function(tabela,
           limites,
           campo_populacao = "POP_TOT",
           campo_faixa = "faixa") {
    tabela <-
      dplyr::mutate(tabela,
                    faixa = findInterval(
                      tabela[[campo_populacao]],
                      limites,
                      rightmost.closed = TRUE,
                      left.open = TRUE
                    ))
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
#' Estado <- c(rep("AC",4), rep("AL",4))
#' faixa <- c(rep(1,2), rep(3,2), rep(2,2), rep(4,2))
#' valor <- c(1, 1, 2, 2, 3, 3 , 4, 4)
#' input <- dplyr::tibble(Estado, faixa, valor)
#' output <- soma_por_estado_faixa(input)
soma_por_estado_faixa <-
  function(tabela,
           campo_estado = "Estado",
           campo_faixa = "faixa") {
    tabela <-
      dplyr::group_by(tabela, .data[[campo_estado]], .data[[campo_faixa]])
    tabela <-
      dplyr::summarise(
        tabela,
        across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
        across(where(is.character), ~ ifelse(length(unique(.x))==1 & length(.x)>1,dplyr::first(.x), NA)),
        .groups="drop")
    tabela <- tabela[,colSums(is.na(tabela))<nrow(tabela)]
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
#' Estado <- c(rep("AC",4), rep("AL",4))
#' faixa <- c(rep(1,2), rep(3,2), rep(2,2), rep(4,2))
#' valor <- c(1, 1, 2, 2, 3, 3 , 4, 4)
#' input <- dplyr::tibble(Estado, faixa, valor)
#' output <- conta_municipios_por_estado_faixa(input)
conta_municipios_por_estado_faixa <-
  function(tabela,
           campo_estado = "Estado",
           campo_faixa = "faixa") {
    tabela <-
      dplyr::group_by(tabela, .data[[campo_estado]], .data[[campo_faixa]])
    tabela <-
      dplyr::summarise(tabela, numero_municipios = dplyr::n(), .groups="drop")
    return(tabela)
  }

#' Média de todas as variáveis numéricas agrupando por estado e faixa populacional
#'
#' @param tabela tabela contendo as colunas de população, estado
#' @param campo_estado parâmetro opcional para definir o campo que contém a sigla do estado (default: "Estado")
#' @param campo_faixa parâmetro opcional para definir o campo que contém a faixa populaciona (default: "faixa")
#'
#' @return tabela contendo a média de todas variáveis por grupos de estado e faixa populacional
#' @export
#'
#' @examples
#' Estado <- c(rep("AC",4), rep("AL",4))
#' faixa <- c(rep(1,2), rep(3,2), rep(2,2), rep(4,2))
#' valor <- c(1, 1, 2, 2, 3, 3 , 4, 4)
#' input <- dplyr::tibble(Estado, faixa, valor)
#' output <- media_por_estado_faixa(input)
media_por_estado_faixa <-
  function(tabela,
           campo_estado = "Estado",
           campo_faixa = "faixa") {
    tabela <-
      dplyr::group_by(tabela, .data[[campo_estado]], .data[[campo_faixa]])
    tabela <-
      dplyr::summarise(
        tabela,
        across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
        across(where(is.character), ~ ifelse(length(unique(.x))==1 & length(.x)>1,dplyr::first(.x), NA)),
        .groups="drop")
    tabela <- tabela[,colSums(is.na(tabela))<nrow(tabela)]
    return(tabela)
  }


#' Quantidade de resíduos destinada à compostagem (t/ano)
#'
#' Agrupa por município os dados do SNIS (prestadores)
#' e estimar a quantidade de resíduos destinados a compostagem.
#'
#' @param tabela contendo a coluna UP003
#'
#' @return tabela contendo a quantidade de resíduos destinados a compostagem por município
#' @export
#'
#' @examples
#' \dontrun{tabela <- quantidade_compostagem_municipio(tabela)}
quantidade_compostagem_municipio <- function(tabela) {
  tabela <- dplyr::filter(
    tabela,
    grepl("compostagem", .data[["UP003"]], fixed = TRUE)
  )
  tabela <- codigo6_para_codigo_ibge(tabela, "Código")
  tabela <- dplyr::group_by(tabela, codigo_municipio)
  tabela <- dplyr::summarise( tabela, quantidade_compostagem=sum(UP080))
  return(tabela)
}



