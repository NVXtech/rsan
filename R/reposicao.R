#' Parâmetros de reposição
#'
#' Retorna uma lista com os parâmetros para o cálculo da reposição
#'
#' @param capacidade é um `character` com o nome dado para coluna capacidade instalada
#' @param custo é um `character` com o nome dado para coluna de custo unitário
#' @param reposicao é um `character` com o nome que será dado para coluna de reposição
#' @param vida_util é um `numeric` com o valor da vida util da instalação
#'
#' @return uma lista com todos parâmetros necessários parao o cálculo de reposição
#' @export
#'
#' @examples
#' params <- campos_reposicao("capacidade", "custo", "reposicao", 5)
campos_reposicao <- function(capacidade, custo, reposicao, vida_util) {
  return(list(
    capacidade = capacidade,
    custo = custo,
    reposicao = reposicao,
    vida_util = vida_util
  ))
}

#' Transforma depreciacao em porcentagem para vida_util
#'
#' @param valor é o valor da depreciação em porcentagem (ex. 2% o valor = 2)
#'
#' @return valor em anos representando vida útil
#' @export
#'
#' @examples
#' vida_util <- depreciacao_para_vida_util(2)
depreciacao_para_vida_util <- function(valor) {
  return(100.0 / valor)
}

#' Calcula reposicação total
#'
#' A reposição total refere-se ao investimento em reposição total entre o ano inicial e o ano final.
#'
#' @param tabela tabela contendo os dados de capacidade e investimento
#' @param campo_capacidade nome do campo de capacidade
#' @param campo_investimento nome do campo de investimento
#' @param campo_reposicao nome do campo de saida com o valor do custo de reposição total
#' @param ano_inicial ano da estimativa de capacidade instalada
#' @param ano_final último ano de projeção
#' @param vida_util vida útil média dos ativos (anos)
#'
#' @return tabela contendo a coluna adiciona de reposição
#' @export
#'
#' @examples
#' investimento <- c(1, 3)
#' capacidade <- c(0, 1)
#' df_in <- dplyr::tibble(capacidade, investimento)
#' df_out <- calcula_reposicao_total(df_in, "capacidade", "investimento", "reposicao", 2021, 2033, 30)
calcula_reposicao_total <- function(tabela,
                                    campo_capacidade,
                                    campo_investimento,
                                    campo_reposicao,
                                    ano_inicial,
                                    ano_final,
                                    vida_util) {
  k1 <- (ano_final - ano_inicial + 1.0)
  k2 <- ano_final - ano_inicial

  tabela <-
    dplyr::mutate(tabela, repo = (k1 * .data[[campo_capacidade]] + k2 * .data[[campo_investimento]] /
      2) / vida_util)
  colnames(tabela)[colnames(tabela) == "repo"] <- campo_reposicao
  return(tabela)
}

#' Calcula reposicação parcial
#'
#' A reposição parcial refere-se ao investimento em resposição entre o ano corrente e o ano final.
#' Dessa maneira é desconsiderado investimento em reposições que teoricamente já aconteceram.
#'
#' @param tabela tabela contendo os dados de capacidade e investimento
#' @param campo_capacidade nome do campo de capacidade
#' @param campo_investimento nome do campo de investimento
#' @param campo_reposicao nome do campo de saida com o valor do custo de reposição total
#' @param ano_inicial ano da estimativa de capacidade instalada
#' @param ano_final último ano de projeção
#' @param ano_corrente ano corrente
#' @param vida_util vida útil média dos ativos (anos)
#'
#' @return tabela contendo a coluna adiciona de reposição
#' @export
#'
#' @examples
#' investimento <- c(1, 3)
#' capacidade <- c(0, 1)
#' df_in <- dplyr::tibble(capacidade, investimento)
#' df_out <- calcula_reposicao_parcial(
#'   df_in, "capacidade", "investimento", "reposicao", 2021, 2033, 2022, 30
#' )
calcula_reposicao_parcial <- function(tabela,
                                      campo_capacidade,
                                      campo_investimento,
                                      campo_reposicao,
                                      ano_inicial,
                                      ano_final,
                                      ano_corrente,
                                      vida_util) {
  ano1 <- ano_inicial
  ano3 <- ano_final
  ano2 <- ano_corrente

  k1 <- ano2 - ano3 - 1
  k2 <- -2 * ano1 + 2 * ano3 + 2
  k3 <- -2 * ano1 + ano2 + ano3
  k4 <- 2 * (ano1 - ano3 - 1)

  tabela <-
    dplyr::mutate(tabela, repo = (k1 * (k2 * .data[[campo_capacidade]] + k3 *
      .data[[campo_investimento]]) / k4) / vida_util)
  colnames(tabela)[colnames(tabela) == "repo"] <- campo_reposicao
  return(tabela)
}
