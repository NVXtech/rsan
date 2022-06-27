#
#' Adiciona colunas de proporção relativa para populações urbana e rural.
#' tabela precisa dos seguintes campos:
#' codigo_municipio, populacao_total, populacao_urbana e populacao_rural
#'
#'
#' @param df é um tibble contendo os dados de populações total, urbana e rural.
#'
#' @return Um tibble() contendo as colunas relativa_urbana e relativa_rural.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- adicionar_proporcao_urbana_rural(df)
#' }
adicionar_proporcao_urbana_rural <- function(df) {
  df <- dplyr::as_tibble(df)
  df <-
    dplyr::select(df,
           codigo_municipio,
           populacao_urbana,
           populacao_rural,
           populacao_total)
  df <-
    dplyr::mutate(
      df,
      relativa_urbana = populacao_urbana / populacao_total,
      relativa_rural = populacao_rural / populacao_total
    )
}

#' Junta as duas fontes de população.
#' São adicionados os sufixos _fonte1 e _fonte2 nos campos com mesmo nome.
#'
#' @param fonte1 é a fonte de dados de população mais antiga.
#' @param fonte2 é a fonte de dados de população mais recente.
#'
#' @return Um tibble() contendo as duas fontes unidas pelo código do munícipio.
#' @export
#'
#' @examples
#' \dontrun{
#' df3 <- junta_fontes_populacao(df1, df2)
#' }
junta_fontes_populacao <- function(fonte1, fonte2) {
  return(dplyr::full_join(
    fonte1,
    fonte2,
    by = "codigo_municipio",
    suffix = c("_fonte1", "_fonte2")
  ))
}


#' Calcula a taxa de crescimento populacional
#' Utiliza método de crescimento geométrico
#'
#' @param tabela é um tibble() contentando os dados consolidados de população
#' @param ano_fonte1 é o ano de obtenção da população (fonte 1)
#' @param ano_fonte2 é o ano de obtenção da população (fonte 2)
#'
#' @return Um tibble() contendo a coluna taxa_de_crescimento
#' @export
#'
#' @examples
#' \dontrun{
#' df <- calcula_taxa_crescimento(df, 2010, 2021)
#' }
calcula_taxa_crescimento <-
  function(tabela, ano_fonte1, ano_fonte2) {
    potencia <- 1.0 / (ano_fonte2 - ano_fonte1)
    tabela$taxa_de_crescimento <-
      ((
        tabela$populacao_total_fonte2 / tabela$populacao_total_fonte1
      ) ^ potencia) - 1
    return(tabela)
  }


#' Calcula a população urbana e rural da fonte 2
#' Assume-se que a porcentagem obtida na fonte 1 não é alterada
#' Assim necessita que a fonte 1 tenha os campos:
#' relativa_urbana e relativa_rural calculados pela funcao
#' adicionar_proporcao_urbana_rural()
#'
#' @param data Um tibble() contendo os dados de populacao
#'
#' @return Um tibble() contendo as colunas populacao_urbana_fonte2 e populacao_rural_fonte2
#' @export
#'
#' @examples
#' \dontrun{
#' df <- calcular_urbana_rural_fonte2(df)
#' }
calcular_urbana_rural_fonte2 <- function(data) {
  data <-
    dplyr::mutate(
      data,
      populacao_urbana_fonte2 = populacao_total_fonte2 * relativa_urbana,
      populacao_rural_fonte2 = populacao_total_fonte2 * relativa_rural
    )
}


#' Calcula a projeção populacional para cada ano dentro do período requerido.
#'
#' @param tabela Um tibble() contendo os dados de populacao (total, rural, urbana) e taxa de crescimento
#' @param ano_inicial é o ano inicial da simulação (ano da fonte 2)
#' @param ano_final é o último ano que será simulado
#'
#' @return Um tibble() contendo a projeção populacional.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- calcula_projecao(df, 2021, 2033)
#' }
calcula_projecao <- function (tabela, ano_inicial, ano_final) {
  tipo <- c("total", "urbana",  "rural")
  nome_campo <-
    c("populacao_total_fonte2",
      "populacao_urbana_fonte2",
      "populacao_rural_fonte2")
  dfs <- tibble::tibble()
  for (i in 1:length(tipo)) {
    time <- (ano_inicial:ano_final)
    for (t in time) {
      new_table <- dplyr::select(tabela, codigo_municipio)
      new_table <- dplyr::mutate(new_table, tipo_populacao = tipo[i], ano = t)
      new_table["populacao"] <-
        tabela[[nome_campo[i]]] * ((1 + tabela$taxa_de_crescimento) ^ (t - ano_inicial))
      dfs <- dplyr::bind_rows(dfs, new_table)
    }
  }
  return(dfs)
}


#' Retorna o ano do dataset de população
#'
#' @param path Caminho do dataset de população
#'
#' @return ano do dataset de populacao
#' @export
#'
#' @examples
#' ano <- get_year_from_path("populaca_estimada_2021")
get_year_from_path <- function(path) {
  strtoi(gsub('[^0-9]', '', path))
}


#' Retorna a projecao de população de um tipo e ano
#'
#' @param tabela projecao da populacao
#' @param ano é o Ano que se deseja a projeção
#' @param tipo é o tipo de população (total, rural ou urbana)
#'
#' @return dados da projeção para um determinado tipo de população
#' @export
#'
#' @examples
#' \dontrun{
#' df <- get_populacao(df, 2033, "urbana")
#' }
get_populacao <- function(tabela, ano, tipo) {
  year <- ano
  pop <- dplyr::filter(tabela, tipo_populacao == tipo & ano == year)
  return(pop)
}
