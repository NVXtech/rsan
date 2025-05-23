#' Proporção relativa para populações urbana e rural.
#'
#' Calcula frações da população urbana e rural em relação a população total.
#'
#' @param tabela é um tibble contendo os dados de populações total, urbana e rural.
#'
#' @return Um tibble() contendo as colunas relativa_urbana e relativa_rural.
#' @export
#'
#' @examples
#' \dontrun{
#' tabela <- adicionar_proporcao_urbana_rural(tabela)
#' }
adicionar_proporcao_urbana_rural <- function(tabela) {
  tabela <- dplyr::as_tibble(tabela)
  tabela <-
    dplyr::select(
      tabela,
      codigo_municipio,
      populacao_urbana,
      populacao_rural,
      populacao_total
    )
  tabela <-
    dplyr::mutate(
      tabela,
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
#'
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
#' tabela <- calcula_taxa_crescimento(tabela, 2010, 2021)
#' }
calcula_taxa_crescimento <-
  function(tabela, ano_fonte1, ano_fonte2) {
    potencia <- 1.0 / (ano_fonte2 - ano_fonte1)
    tabela <- dplyr::mutate(
      tabela,
      taxa_de_crescimento = -1.0 +
        (populacao_total_fonte2 / populacao_total_fonte1)^potencia,
      taxa_de_crescimento_urbana = -1.0 +
        (populacao_urbana_fonte2 / populacao_urbana_fonte1)^potencia,
      taxa_de_crescimento_rural = -1.0 +
        (populacao_urbana_fonte2 / populacao_urbana_fonte1)^potencia
    )
    return(tabela)
  }

#' Preenche situação
#' As situações urbana e rural são estimadas mantendo-se a mesma proporção da fonte1.
#'
#' @param fonte1 é a fonte de dados de população mais antiga.
#' @param fonte2 é a fonte de dados de população mais recente.
#'
#' @return Um `data.frame` contendo
#' @export
preenche_situacao <- function(fonte1, fonte2) {
  fonte1 <- dplyr::mutate(
    fonte1,
    relativa_urbana = populacao_urbana / populacao_total,
    relativa_rural = populacao_rural / populacao_total
  )
  fonte1 <- dplyr::select(
    fonte1, codigo_municipio, relativa_urbana, relativa_rural
  )
  fonte2 <- dplyr::left_join(fonte2, fonte1, by = "codigo_municipio")
  fonte2 <- dplyr::mutate(
    fonte2,
    populacao_urbana = populacao_total * relativa_urbana,
    populacao_rural = populacao_total * relativa_rural
  )
  fonte2 <- dplyr::select(
    fonte2, codigo_municipio,
    populacao_total, populacao_urbana, populacao_rural
  )
  return(fonte2)
}

#' Calcula a população urbana e rural da fonte 2
#'
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
#' tabela <- calcular_urbana_rural_fonte2(tabela)
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
#' tabela <- calcula_projecao(tabela, 2021, 2033)
#' }
calcula_projecao <- function(tabela, ano_inicial, ano_final, ano_fonte1) {
  tipo <- c("total", "urbana", "rural")
  nome_campo <-
    c(
      "populacao_total_fonte1",
      "populacao_urbana_fonte1",
      "populacao_rural_fonte1"
    )
  nome_campo_taxa <-
    c(
      "taxa_de_crescimento",
      "taxa_de_crescimento_urbana",
      "taxa_de_crescimento_rural"
    )
  dfs <- tibble::tibble()
  for (i in 1:length(tipo)) {
    time <- (ano_inicial:ano_final)
    for (t in time) {
      new_table <- dplyr::select(tabela, codigo_municipio)
      new_table <- dplyr::mutate(new_table, tipo_populacao = tipo[i], ano = t)
      new_table["populacao"] <- tabela[[nome_campo[i]]] *
        ((1 + tabela[[nome_campo_taxa[i]]])^(t - ano_fonte1))
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
#' ano <- nome_para_ano("populaca_estimada_2021")
nome_para_ano <- function(path) {
  strtoi(gsub("[^0-9]", "", path))
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
#' tabela <- get_populacao(tabela, 2033, "urbana")
#' }
get_populacao <- function(tabela, ano, tipo) {
  year <- ano
  pop <- dplyr::filter(tabela, tipo_populacao == tipo & ano == year)
  return(pop)
}

#' Junta as estimativas de populacao urbana
#'
#' @param populacao é os dados da projeção populacional
#' @param ano da estimativa de população
#' @param tabela
#'
#' @return tabela com todos os dados consolidados
#' @export
#'
#' @examples
#' \dontrun{
#' df <- adiciona_projecao_populacao(populacao, ano, tabela)
#' }
adiciona_populacao_corrente <- function(populacao, ano, tabela) {
  pop <- get_populacao(populacao, ano, "urbana")
  # rename populacao to popularcao_urbana_corrente
  pop <- dplyr::rename(pop, populacao_urbana_corrente = populacao)
  pop <- dplyr::select(pop, c("codigo_municipio", "populacao_urbana_corrente"))
  tabela <- dplyr::full_join(
    pop,
    tabela,
    by = "codigo_municipio"
  )
  pop <- get_populacao(populacao, ano, "total")
  pop <- dplyr::rename(pop, populacao_total_corrente = populacao)
  pop <- dplyr::select(pop, c("codigo_municipio", "populacao_total_corrente"))
  tabela <- dplyr::full_join(
    pop,
    tabela,
    by = "codigo_municipio"
  )
  pop <- get_populacao(populacao, ano, "rural")
  pop <- dplyr::rename(pop, populacao_rural_corrente = populacao)
  pop <- dplyr::select(pop, c("codigo_municipio", "populacao_rural_corrente"))
  tabela <- dplyr::full_join(
    pop,
    tabela,
    by = "codigo_municipio"
  )
  return(tabela)
}

test_projecao <- function() {
  app_state <- list(input = get_default_input())
  app_state$input$projecao$fonte1 <- "censo_2022"
  app_state$input$projecao$fonte2 <- "estimativa_2024"
  app_state <- rodar_projecao_populacional(app_state)
  projecao <- app_state$projecao
  projecao <- dplyr::select(projecao, -codigo_municipio)
  projecao <- dplyr::group_by(projecao, ano, tipo_populacao)
  projecao <- dplyr::summarise(projecao, populacao = sum(populacao, na.rm = TRUE))
  projecao <- dplyr::ungroup(projecao)
  projecao <- tidyr::pivot_wider(projecao, names_from = tipo_populacao, values_from = populacao)
  writexl::write_xlsx(projecao, "projecao_2024.xlsx")
}
