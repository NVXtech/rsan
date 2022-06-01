


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

#' Retorna dados do SNIS
#'
#' @param snis caminho do SNIS
#' @param fields lista com campos a serem retornados
#'
#' @return um tibble() contendo os dados do SNIS
#' @export
#'
#' @examples
#' \dontrun{
#' df <- get_snis_data("snis_2020", c("AG001", "AG010"))
#' }
get_snis_data <- function(snis, fields) {
  df <- load_data(snis)
  df <- dplyr::select(df, fields)
  return(df)
}


#' Retorna dados do SINAPI
#'
#' @param sinapi caminho do dados do SINAPI
#'
#' @return um tibble() com dados do SINAPI
#' @export
#'
#' @examples
#' \dontrun{
#' df <- get_sinapi_data(sinapi)
#' }
get_sinapi_data <- function(sinapi) {
  df <- load_data(sinapi)
  return(df)
}


#' Calcula as densidades
#'
#' @param data um tibble() contendo os dados do SNIS
#'
#' @return um tibble() contendo as densidades
#' @export
#'
#' @examples
#' \dontrun{
#' df <- add_density(df)
#' }
add_density <- function(df) {
  df <- dplyr::mutate(
    df,
    densidade_distribuicao_agua = AG005 * 1000 / AG001,
    densidade_producao_agua = AG010 * 1000 / AG001,
    densidade_coleta_esgoto = ES004 * 1000 / ES001
  )
  return(df)
}

#' Completa os dados faltantes de densidade.
#'
#' @param density um tibble() contendo os dados de densidades
#'
#' @return um tibble() com a mesma tabela mas sem dados faltantes
#' @export
#'
#' @examples
#' \dontrun{
#' df <- fill_missing_density(density)
#' }
fill_missing_density <- function(density) {
  fields <-
    c(
      "densidade_distribuicao_agua",
      "densidade_producao_agua",
      "densidade_coleta_esgoto"
    )
  for (i in fields) {
    density[is.infinite(unlist(density[, i])), i] <- NA
    density[is.nan(unlist(density[, i])), i] <- NA
    density[, i][density[, i] == 0] <- NA

    Q3 <- stats::quantile(unlist(density[, i]), 0.75, na.rm = TRUE)
    IQR <- stats::IQR(unlist(density[, i]), na.rm = TRUE)
    density[, i][density[, i] >= Q3 + 1.5 * IQR] <- NA

    fit <-
      stats::lm(log(get(i)) ~ 0 + I(POP_TOT / 1000000) + Estado, data = density)

    density <- density %>%
      dplyr::mutate(pred = exp(stats::predict(fit, .))) %>%
      dplyr::mutate_(.dots = stats::setNames(list(
        paste0("ifelse(is.na(", i, "), pred,", i, ")")
      ), i))

    density <- density[, -14]
  }
  return(density)
}

#' Junta as estimativas de populacao total, urbana, rural e dados de snis
#'
#' @param populacao é os dados da projeção populacional
#' @param ano da estimativa de população
#' @param snis dados do snis
#'
#' @return tabela com todos os dados consolidados
#' @export
#'
#' @examples
#' \dontrun{
#' df <- consolida_populacao_snis(fonte1, fonte2, snis2020)
#' }
consolida_populacao_snis <- function(populacao, ano, snis) {
    populacao_total <- get_populacao(populacao, ano, "total")
    populacao_urbana <- get_populacao(populacao, ano, "urbana")
    populacao_rural <- get_populacao(populacao, ano, "rural")

    df <- dplyr::full_join(
      populacao_total,
      populacao_urbana,
      by = "codigo_municipio",
      suffix = c("_total", "_urbana")
    )

    df <- dplyr::full_join(
      df,
      populacao_rural,
      by = "codigo_municipio",
    )
    df <- dplyr::rename(df, populacao_rural=populacao)

    df <- dplyr::full_join(df,
                           snis,
                           by = "codigo_municipio")
    df <-
      dplyr::select(
        df,-c(
          "tipo_populacao_total",
          "tipo_populacao_urbana",
          "tipo_populacao",
          "ano",
          "ano_total",
          "ano_urbana"
        )
      )
    return(df)
  }

#' Calcula deficits e demandas para água e esgoto
#'
#' @param df um tibble() contendo os dados do SNIS e de população
#' @param meta_agua meta em % de atendimento para abastecimento de água
#' @param meta_esgoto meta em % de atendimento para tratamento de esgoto
#' @param proporcao proporção da densidade de água que equivale a densiade de esgoto
#'
#' @return um tibble() contendo os dados de deficits e demandas
#' @export
#'
#' @examples
#' \dontrun{
#' df <- calculate_geografico(df, 99, 90, 80)
#' }
calculate_geografico <-
  function(df, meta_agua, meta_esgoto, proporcao) {
    df$AG001[is.na(df$AG001)] <- 0
    df$AG026[is.na(df$AG026)] <- 0
    df$ES001[is.na(df$ES001)] <- 0
    df$ES026[is.na(df$ES026)] <- 0
    df <- dplyr::mutate(
      df,
      deficit_agua_total = pmax(populacao_total - AG001, 0.0),
      deficit_agua_urbana = pmax(populacao_urbana - AG026, 0.0),

      deficit_esgoto_total = pmax(populacao_total - ES001, 0.0),
      deficit_esgoto_urbana = pmax(populacao_urbana - ES026, 0.0),

      deficit_agua_rural = pmax(deficit_agua_total - deficit_agua_urbana, 0.0),
      deficit_esgoto_rural = pmax(deficit_esgoto_total - deficit_esgoto_urbana, 0.0),

      demanda_distribuicao_agua = meta_agua / 100.0 * deficit_agua_urbana * densidade_distribuicao_agua,
      demanda_producao_agua = meta_agua / 100.0 * deficit_agua_urbana * densidade_producao_agua,

      densidade_tratamento_esgoto = proporcao / 100.0 * densidade_producao_agua,

      demanda_coleta_esgoto = meta_esgoto / 100.0 * deficit_esgoto_urbana * densidade_coleta_esgoto,
      demanda_tratamento_esgoto = meta_esgoto / 100.0 * deficit_esgoto_urbana * densidade_tratamento_esgoto,
    )
    df <- dplyr::select(
      df,
      codigo_municipio,
      deficit_agua_total,
      deficit_agua_urbana,
      deficit_agua_rural,
      deficit_esgoto_total,
      deficit_esgoto_urbana,
      deficit_esgoto_rural,
      demanda_distribuicao_agua,
      demanda_producao_agua,
      demanda_coleta_esgoto,
      demanda_tratamento_esgoto,
      populacao_total,
      populacao_urbana,
      populacao_rural
    )
  }


#' Calcula preços distribuicao de água e coleta de esgoto
#'
#' @param df_projeto um tibble() contendo os itens do projeto
#' @param sinapi o tibble() com os preços do SINAPI
#' @param taxa_servico fator de correção do preço para serviços
#' @param taxa_materiais fator de correção do preço para materiais
#'
#' @return um tibble() com preços por projeto e por estado
#' @export
#'
#' @examples
#' \dontrun{
#' df <- calcula_precos_distribuicao(projeto, sinapi, 26, 18)
#' }
calcula_precos_distribuicao <-
  function(df_projeto,
           sinapi,
           taxa_servico = 26,
           taxa_materiais = 18) {
    lista_projetos <- c("07a", "07b", "08a", "08b", "09a", "09b")
    taxa_servico <- taxa_servico / 100.0 + 1.0
    taxa_materiais <- taxa_materiais / 100.0 + 1.0
    states <- rsan::states_acronym()
    consolidate <-
      dplyr::left_join(df_projeto, sinapi, by = c("codigo" = "CODIGO"))
    consolidate <-
      dplyr::mutate(consolidate,
                    fator = ifelse(
                      grupo == "SERVIÇOS",
                      taxa_servico,
                      ifelse(grupo == "MATERIAIS", taxa_materiais, 1.0)
                    ))
    df <-
      tibble(codigo = consolidate$codigo, tipo = consolidate$tipo)
    porcentagem <-
      dplyr::filter(df_projeto, codigo == "PORCENTAGEM")
    porcentagem <-
      tidyr::pivot_longer(porcentagem,
                          all_of(lista_projetos),
                          names_to = "projeto",
                          values_to = "porcento")
    for (projeto in lista_projetos) {
      for (state in states) {
        name <- paste(projeto, state, sep = "_")
        price_column <- paste0("PRECO_", state)
        df[[name]] <-
          consolidate[[projeto]] * consolidate[[price_column]] * consolidate$fator
      }
    }
    df <- dplyr::filter(df, codigo != "PORCENTAGEM")
    df <-
      tidyr::pivot_longer(
        df,-c("codigo", "tipo"),
        names_to = c("projeto", "estado"),
        names_pattern = "(.*)_(.*)",
        values_to = "preco"
      )

    # Calcula porcentagems por tipo
    for (row in 1:nrow(porcentagem)) {
      tipo_row <- as.character(porcentagem[row, "tipo"])
      projeto_row <- as.character(porcentagem[row, "projeto"])
      ratio <- as.double(porcentagem[row, 'porcento'])
      filtered <-
        dplyr::filter(df, projeto == projeto_row & tipo == tipo_row)
      filtered <- dplyr::group_by(filtered, estado)
      filtered <- dplyr::summarise(filtered, preco = sum(preco))
      filtered <- dplyr::mutate(
        filtered,
        codigo = "PORCENTAGEM",
        tipo = tipo_row,
        projeto = projeto_row,
        preco = preco * ratio
      )
      df <- dplyr::bind_rows(df, filtered)
    }
    df <- dplyr::group_by(df, projeto, estado)
    df <-
      dplyr::summarise(df, across(-c("codigo", "tipo"), sum, na.rm = TRUE))
    df <- dplyr::mutate(df, preco = round(preco, 2))
    return(df)
  }

#' Classifica município em relação ao seu tamanho e custo de aquisição de materiais
#'
#' A classificação é composta de 3 digitos (XXY)
#' os dois primeiro (XX) é a classificação do tamanho da população e o último (y) refere-se ao custo.
#' A classificação do tamanho da população é:
#' \itemize{
#'   \item{07}{ - populações menores ou igual a 40 000 habitantes}
#'   \item{08}{ - populações maiores que 40 000 habitantes e menores ou igual a 400 000 habitantes}
#'   \item{09}{ - populações maiores que 400 000 habitantes}
#' }
#' Quanto ao custo:
#' \itemize{
#' \item{a}{ - indica alto custo}
#' \item{b}{ - indica baixo custo}
#' }
#' Exemplo: população 07b é um município com população menor que 40000 e com baixo custo para aquisição de materiais e serviços
#'
#' @param tabela Um dataframe contendo as colunas: populacao_total, populacao_rural e populacao_urbana
#'
#' @return Dataframe contendo a coluna adicional \code{classificacao}
#' @export
#'
#' @examples
#' populacao_total =  c(3000, 3000, 300000, 300000, 600000, 600000)
#' populacao_urbana = c(2999,    1, 299999,      1, 599999,      1)
#' populacao_rural =  c(   1, 2999,      1, 299999,      1, 599999)
#' tabela <- dplyr::tibble(populacao_total, populacao_urbana, populacao_rural)
#' tabela <- classifica_municipio(tabela)
classifica_municipio <- function(tabela) {
  limite_populacao_classe_1 <- 4e4
  limite_populacao_classe_2 <- 4e5
  tabela <- dplyr::mutate(
    tabela,
    class_pop = ifelse(
      populacao_total <= limite_populacao_classe_1,
      "07",
      ifelse(populacao_total <= limite_populacao_classe_2, "08", "09")
    ),
    class_custo = ifelse(populacao_urbana > populacao_rural, "a", "b"),
    classificacao = paste0(class_pop, class_custo)
  )
  tabela <- dplyr::select(tabela, -c("class_pop", "class_custo"))
  return(tabela)
}

#' Adiciona coluna de estados na tabela
#'
#' @param tabela contendo coluna codigo_municipio
#'
#' @return tabela com coluna adiciona estado
#' @export
#'
#' @examples
#' codigo_municipio <- c("1200013","1200054", "1200104", "1200138")
#' estado <- rep("AC", 4)
#' input <- dplyr::tibble(codigo_municipio)
#' tabela <- adiciona_estado(input)
#'
adiciona_estado <- function(tabela){
  data(municipio)
  original_colnames <- colnames(tabela)
  output_cols <- c(original_colnames, "estado")

  tabela <- dplyr::left_join(tabela, municipio, by="codigo_municipio")
  tabela <- dplyr::rename(tabela, estado=UF)
  tabela <- dplyr::select(tabela, output_cols)
  return(tabela)
}
