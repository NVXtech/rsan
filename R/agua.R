#' Necessidade de Investimento - Componente Água - Situação Urbana
agua_required_fields <- c(
  "codigo_municipio",
  "atendimento_tot_agua_hab",
  "extensao_rede_agua_km",
  "volume_agua_produzido_dam3_ano",
  "volume_agua_consumido_dam3_ano",
  "atendimento_urb_agua_hab",
  "atendimento_tot_esgoto_hab",
  "extensao_rede_esgoto_km",
  "volume_esgoto_tratado_dam3_ano",
  "volume_esgoto_tratado_dam3_ano"
)

#' Calcula as necessidades produção e distribuição
#'
#' Calcula as necessidades para distribuição de água ($m/hab$) e para produção de água ($m^3/hab$).
#'
#' @param data um `data.frame` contendo os dados do SINISA
#'
#' @return um `data.frame` contendo as densidades
#' @export
#'
#' @examples
#' \dontrun{
#' df <- necessidade_agua_esgoto(df)
#' }
necessidade_agua_esgoto <- function(df) {
  df <- dplyr::mutate(
    df,
    densidade_distribuicao_agua = extensao_rede_agua_km * 1000 / atendimento_tot_agua_hab,
    densidade_producao_agua = volume_agua_consumido_dam3_ano * 1000 / atendimento_tot_agua_hab,
    densidade_coleta_esgoto = extensao_rede_esgoto_km * 1000 / atendimento_tot_esgoto_hab
  )
  return(df)
}


#' Completa os dados faltantes de densidade.
#'
#' @param density um `data.frame` contendo os dados de densidades
#' @param fields lista dos campos para preencher
#'
#' @return um `data.frame` com a mesma tabela mas sem dados faltantes
#' @export
#'
#' @examples
#' \dontrun{
#' df <- fill_missing_density(density, fields)
#' }
fill_missing_density <- function(density, fields) {
  n <- ncol(density) + 1
  for (i in fields) {
    density[is.infinite(unlist(density[, i])), i] <- NA
    density[is.nan(unlist(density[, i])), i] <- NA
    density[, i][density[, i] == 0] <- NA

    Q3 <- stats::quantile(unlist(density[, i]), 0.75, na.rm = TRUE)
    IQR <- stats::IQR(unlist(density[, i]), na.rm = TRUE)
    density[, i][density[, i] >= Q3 + 1.5 * IQR] <- NA
    fit <-
      stats::lm(log(get(i)) ~ 0 + I(populacao_total_corrente / 1000000) + estado, data = density)

    density <- dplyr::mutate(
      density,
      pred = exp(stats::predict(fit, density))
    )
    mask <- as.vector(is.na(density[, i]))
    density[mask, i] <- density[mask, "pred"]
    density <- density[, -n]
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
#' df <- adiciona_projecao_populacao(populacao, ano, tabela)
#' }
adiciona_projecao_populacao <- function(populacao, ano, tabela) {
  populacao_total <- get_populacao(populacao, ano, "total")
  populacao_urbana <- get_populacao(populacao, ano, "urbana")
  populacao_rural <- get_populacao(populacao, ano, "rural")

  pop <- dplyr::left_join(
    populacao_total,
    populacao_urbana,
    by = "codigo_municipio",
    suffix = c("_total", "_urbana")
  )

  pop <- dplyr::left_join(pop,
    populacao_rural,
    by = "codigo_municipio",
  )
  pop <- dplyr::rename(pop, populacao_rural = populacao)
  tabela <- dplyr::left_join(
    tabela,
    pop,
    by = "codigo_municipio"
  )
  fields_to_remove <- c(
    "tipo_populacao_total",
    "tipo_populacao_urbana",
    "tipo_populacao",
    "ano",
    "ano_total",
    "ano_urbana"
  )
  tabela <- dplyr::select(
    tabela,
    -all_of(fields_to_remove)
  )
  return(tabela)
}


#' Calcula deficits e demandas para água
#'
#' @param df um `data.frame` contendo os dados do SNIS e de população
#' @param meta_agua meta em % de atendimento para abastecimento de água
#'
#' @return um `data.frame` contendo os dados de deficits e demandas
#' @export
#'
#' @examples
#' \dontrun{
#' df <- calculate_demografico_agua(df, 99, 90, 80)
#' }
calculate_demografico_agua <- function(df, meta_agua) {
  df$atendimento_tot_agua_hab[is.na(df$atendimento_tot_agua_hab)] <- 0
  df$atendimento_urb_agua_hab[is.na(df$atendimento_urb_agua_hab)] <- 0
  df <- dplyr::mutate(
    df,
    deficit_total = pmax(populacao_total - atendimento_tot_agua_hab, 0.0),
    deficit_urbana = pmax(populacao_urbana - atendimento_urb_agua_hab, 0.0),
    deficit_rural = pmax(deficit_total - deficit_urbana, 0.0),
    demanda_distribuicao_agua = meta_agua / 100.0 * deficit_urbana * densidade_distribuicao_agua,
    demanda_producao_agua = meta_agua / 100.0 * deficit_urbana * densidade_producao_agua
  )
  df <- dplyr::select(
    df,
    codigo_municipio,
    estado,
    deficit_total,
    deficit_urbana,
    deficit_rural,
    demanda_distribuicao_agua,
    demanda_producao_agua,
    populacao_total,
    populacao_urbana,
    populacao_rural,
    densidade_distribuicao_agua,
    densidade_producao_agua
  )
}


#' Verifica códigos faltantes no SINAPI
#'
#' Esta função verifica se há códigos no dataframe de projeto que não estão presentes no dataframe do SINAPI.
#'
#' @param df Um `data.frame` contendo os códigos do projeto.
#' @param sinapi Um `data.frame` contendo os códigos e descrições do SINAPI.
#'
#' @return Um valor lógico. Retorna `TRUE` se todos os códigos estão presentes no SINAPI, caso contrário, retorna `FALSE` e registra os códigos faltantes.
#' @export
#'
#' @examples
#'
#' projeto <- data.frame(codigo = c("001", "002", "003"))
#' sinapi <- data.frame(CODIGO = c("001", "002"), DESCRICAO = c("Desc1", "Desc2"))
#' verifica_codigos_faltantes(projeto, sinapi)
verifica_codigos_faltantes <- function(df, sinapi) {
  df <- dplyr::select(df, c("codigo"))
  df <- dplyr::filter(df, codigo != "PORCENTAGEM")
  df <- dplyr::distinct(df)
  sinapi <- dplyr::select(sinapi, c("CODIGO", "DESCRICAO"))
  junto <- dplyr::left_join(df, sinapi, by = c("codigo" = "CODIGO"))
  faltantes <- dplyr::filter(junto, is.na(DESCRICAO))
  nrow_faltantes <- nrow(faltantes)
  if (nrow_faltantes > 0) {
    rlog::log_error(
      sprintf(
        "Faltam %d códigos no SINAPI: %s",
        nrow_faltantes,
        paste(faltantes$codigo, collapse = ", ")
      )
    )
    return(FALSE)
  }
  return(TRUE)
}

#' Calcula preços distribuicao de água e coleta de esgoto
#'
#' @param df_projeto um `data.frame` contendo os itens do projeto coluna quantidade e codigo sinapi
#' @param sinapi o `data.frame` com os preços do SINAPI
#' @param taxa_servico fator de correção do preço para serviços
#' @param taxa_materiais fator de correção do preço para materiais
#' @param fator_correcao fator de correção monetária (1+% de correção)
#'
#' @return um `data.frame` com preços por projeto e por estado
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
           taxa_materiais = 18,
           fator_correcao = 1.0) {
    lista_projetos <- c("07a", "07b", "08a", "08b", "09a", "09b")
    taxa_servico <- taxa_servico / 100.0 + 1.0
    taxa_materiais <- taxa_materiais / 100.0 + 1.0
    states <- states_acronym()
    verifica_codigos_faltantes(df_projeto, sinapi)

    consolidate <- dplyr::left_join(
      df_projeto,
      sinapi,
      by = c("codigo" = "CODIGO")
    )

    consolidate <- dplyr::mutate(
      consolidate,
      fator = ifelse(
        grupo == "SERVIÇOS",
        taxa_servico,
        ifelse(grupo == "MATERIAIS", taxa_materiais, 1.0)
      )
    )
    df <- dplyr::tibble(codigo = consolidate$codigo, tipo = consolidate$tipo)
    porcentagem <- dplyr::filter(df_projeto, codigo == "PORCENTAGEM")
    porcentagem <- tidyr::pivot_longer(
      porcentagem,
      all_of(lista_projetos),
      names_to = "projeto",
      values_to = "porcento"
    )

    for (projeto in lista_projetos) {
      for (state in states) {
        name <- paste(projeto, state, sep = "_")
        price_column <- paste0("PRECO_", state)
        df[[name]] <-
          consolidate[[projeto]] * consolidate[[price_column]] * consolidate$fator * fator_correcao
      }
    }

    df <- dplyr::filter(df, codigo != "PORCENTAGEM")
    df <- tidyr::pivot_longer(
      df,
      -c("codigo", "tipo"),
      names_to = c("projeto", "estado"),
      names_pattern = "(.*)_(.*)",
      values_to = "preco"
    )

    # Calcula porcentagems por tipo
    for (row in 1:nrow(porcentagem)) {
      tipo_row <- as.character(porcentagem[row, "tipo"])
      projeto_row <- as.character(porcentagem[row, "projeto"])
      ratio <- as.double(porcentagem[row, "porcento"])
      filtered <- dplyr::filter(
        df,
        projeto == projeto_row & tipo == tipo_row
      )
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
    df <- dplyr::summarise(df, dplyr::across(-c("codigo", "tipo"), \(x) sum(x, na.rm = TRUE)))
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
#' @return Dataframe contendo a coluna adicional \code{classificacao} e \code{cenario}
#' @export
#'
#' @examples
#' populacao_total <- c(3000, 3000, 300000, 300000, 600000, 600000)
#' populacao_urbana <- c(2999, 1, 299999, 1, 599999, 1)
#' populacao_rural <- c(1, 2999, 1, 299999, 1, 599999)
#' tabela <- dplyr::tibble(populacao_total, populacao_urbana, populacao_rural)
#' tabela <- classifica_municipio(tabela)
classifica_municipio <- function(tabela) {
  limite_populacao_classe_1 <- 4e4
  limite_populacao_classe_2 <- 4e5
  tabela <- dplyr::mutate(
    tabela,
    cenario = ifelse(
      populacao_total <= limite_populacao_classe_1,
      "07",
      ifelse(populacao_total <= limite_populacao_classe_2, "08", "09")
    ),
    class_custo = ifelse(populacao_urbana > populacao_rural, "a", "b"),
    classificacao = paste0(cenario, class_custo)
  )
  tabela <- dplyr::select(tabela, -c("class_custo"))
  return(tabela)
}


#' Calcula custo relativo para produção de água
#'
#' Custo relativo é o valor monetário necessário para produzir 1m3 de água tratada por ano R$/(m3/ano).
#'
#' @param preco_unidade tabela contendo as colunas estado, unidade e preco
#' @param projeto_tratamento tabela contendo cenario, unidade e quantidade
#'
#' @return tabela com as colunas estado, cenario e custo_relativo
#' @export
#'
#' @examples
#' estado <- c("AC", "AC", "AC", "AL", "AL", "AL")
#' unidade <- c("ETA200", "EEA200", "POÇO40", "ETA200", "EEA200", "POÇO40")
#' preco <- c(3.055, 0.129, 0.69, 2.88, 0.12, 0.66)
#' preco_unidade <- dplyr::tibble(estado, unidade, preco)
#'
#' cenario <- c("08", "08", "08", "08", "09", "09", "09", "09")
#' unidade <- c("ETA200", "EEA200", "POÇO40", "EEA200", "ETA200", "EEA200", "POÇO40", "EEA200")
#' tipo <- c(
#'   "superficial", "superficial", "subterranea", "subterranea",
#'   "superficial", "superficial", "subterranea", "subterranea"
#' )
#' quantidade <- c(0.854106686284393, 1.3, 1, 1, 0.549559117416313, 1.3, 1, 1)
#' projeto_producao <- dplyr::tibble(cenario, unidade, tipo, quantidade)
#' df_out <- calcula_custo_relativo_producao(preco_unidade, projeto_producao)
calcula_custo_relativo_producao <-
  function(preco_unidade, projeto_producao) {
    tabela <-
      dplyr::full_join(projeto_producao, preco_unidade, by = "unidade")

    projeto_tipo_producao <- carrega_dado_auxiliar(
      "agua_projeto_producao_tipos"
    )

    predominancia <-
      tidyr::pivot_longer(
        projeto_tipo_producao,
        all_of(c("superficial", "subterranea")),
        names_to = "tipo",
        values_to = "predominancia"
      )
    tabela <-
      dplyr::left_join(tabela, predominancia, by = c("estado", "tipo"))
    tabela <-
      dplyr::mutate(tabela, custo_relativo = quantidade * preco * predominancia)
    tabela <- dplyr::group_by(tabela, estado, cenario)
    tabela <-
      dplyr::summarise(tabela, custo_relativo = sum(custo_relativo))
  }



#' Fator de perda de água
#'
#' O fator de perda de água é valor pelo qual a demanda de água deve ser multiplicada para compensar as perdas.
#'
#' @param perda é a porcentagem de perda em %.
#'
#' @return O fator de perda,
#' @export
#'
#' @examples
#' fator <- fator_perda_agua(25)
fator_perda_agua <- function(perda) {
  return(1.0 / (1.0 - perda / 100.0))
}


#' Calcula o preço das unidades de produção de água
#'
#' @param df_projeto tabela com os dados do projeto tipo de produção de água
#' @param sinapi tabela com os dados do SINAPI
#' @param fator_insumo fator de correção do preços na categoria insumo (%)
#' @param fator_composicao fator de correção do preços na categoria composição (%)
#' @param fator_correcao fator de correção monetária (1+% de correção)
#' @param sub_total Se FALSE a porcentagem é calculada como juros sobre juros. Se TRUE a porcentagem é a soma dos juros.
#'
#' @return uma tabela com os preços das unidades de produção de água por estado e cenário
#' @export
#'
#' @examples
#' \dontrun{
#' tabela <- calcula_preco_unidades_producao(df_projeto, sinapi, 26, 18, TRUE)
#' }
calcula_preco_unidades_producao <-
  function(df_projeto,
           sinapi,
           fator_insumo = 26,
           fator_composicao = 18,
           fator_correcao = 1.0,
           sub_total = TRUE) {
    fator_insumo <- fator_insumo / 100.0 + 1
    fator_composicao <- fator_composicao / 100.0 + 1
    lsTom3ano <- 365.25 * 86400 * 1e-3

    verifica_codigos_faltantes(df_projeto, sinapi)

    consolidate <-
      dplyr::left_join(df_projeto, sinapi, by = c("codigo" = "CODIGO"))

    porcentagem <-
      dplyr::filter(df_projeto, codigo == "PORCENTAGEM")
    porcentagem <-
      dplyr::select(porcentagem, !c("codigo", "observacao"))
    porcentagem <- dplyr::group_by(porcentagem, unidade)
    if (sub_total) {
      porcentagem <-
        dplyr::summarise(porcentagem, percent = 1 + sum(quantidade, na.rm = TRUE))
    } else {
      porcentagem <-
        dplyr::summarise(porcentagem, percent = prod(1.0 + quantidade, na.rm = TRUE))
    }

    df <- dplyr::filter(consolidate, codigo != "PORCENTAGEM")

    fields_to_remove <- c("codigo", "observacao", "DESCRICAO", "UNIDADE")
    df <- dplyr::select(df, -dplyr::all_of(fields_to_remove))

    fields_to_ignore <- c("unidade", "quantidade", "TIPO", "vazao")
    df <-
      tidyr::pivot_longer(
        df, -dplyr::all_of(fields_to_ignore),
        names_to = "estado",
        names_pattern = "_(.*)",
        values_to = "preco"
      )

    df <-
      dplyr::mutate(
        df,
        fator = ifelse(
          TIPO == "COMPOSICAO",
          fator_composicao,
          ifelse(TIPO == "INSUMO", fator_insumo, 1.0)
        ),
        preco = quantidade * preco * fator / (vazao * lsTom3ano) * fator_correcao
      )
    df <- dplyr::group_by(df, unidade, estado)
    df <-
      dplyr::summarise(df, dplyr::across(-c("TIPO", "fator", "quantidade", "vazao"), \(x) sum(x, na.rm = TRUE)))

    df <- dplyr::left_join(df, porcentagem, by = "unidade")
    df <- dplyr::mutate(df, preco = round(preco * percent, 2))
    df <- dplyr::select(df, !"percent")
    return(df)
  }

#' Calcula o custo de expansão para distribuição de água ou coleta de esgoto
#'
#' @param demanda tabela com a demanda de distribuição de água
#' @param extensao tabela com o custo relativo de distribuição de água
#' @param tipo define o tipo de custo (\code{"distribuicao_agua"} ou \code{"coleta_esgoto"}).
#'
#' @return o custo de expansão para o tipo escolhido
#' @export
#'
#' @examples
#' \dontrun{
#' custo <- calcula_custo_extensao(demanda, extensao, tipo)
#' }
calcula_custo_extensao <- function(demanda, extensao, tipo) {
  demanda_name <- paste0("demanda_", tipo)
  preco_name <- paste0("preco_", tipo)
  output_field <- paste0("custo_expansao_", tipo)
  tabela <- dplyr::left_join(
    demanda,
    extensao,
    by = c("classificacao" = "projeto", "estado")
  )
  tabela <- dplyr::mutate(
    tabela,
    cexp = preco * .data[[demanda_name]]
  )
  colnames(tabela)[colnames(tabela) == "preco"] <- preco_name
  colnames(tabela)[colnames(tabela) == "cexp"] <- output_field
  return(tabela)
}

#' Calcula a necessidade de investimento para produção de água
#'
#' @param demanda tabela com as demandas por município
#' @param producao tabela com os custos das unidades de produção de água
#' @param perda_agua perda de água a ser compensada (%)
#'
#' @return tabela com a necessidade de investimento para produção de água
#' @export
#'
#' @examples
#' \dontrun{
#' custo <- calcula_custo_expansao_producao(demanda, producao, perda_agua)
#' }
calcula_custo_expansao_producao <-
  function(demanda, producao, perda_agua) {
    fator_perda <- fator_perda_agua(perda_agua)
    projeto_producao_agua <- carrega_dado_auxiliar("agua_projeto_producao")
    custo_relativo <-
      calcula_custo_relativo_producao(producao, projeto_producao_agua)
    tabela <-
      dplyr::left_join(demanda, custo_relativo, by = c("estado", "cenario"))
    tabela <-
      dplyr::mutate(tabela,
        custo_expansao_producao_agua = custo_relativo * demanda_producao_agua *
          fator_perda
      )
    colnames(tabela)[colnames(tabela) == "custo_relativo"] <-
      "custo_relativo_producao"
    return(tabela)
  }


#' Calcula o custo de expansão do sistema de água
#'
#' @param demanda tabela com as demandas de distribuição e produção de água, coleta e tratamento de egoto.
#' @param distribuicao tabela com custos relativos (R$/m) de distribuição de água por estado
#' @param producao tabela com custos relativos R$/(m3/ano) de produção de água
#' @param perda_agua Parâmetro de correção de perda de água em (%)
#'
#' @return tabela com as necessidade de investimento
#' @export
#'
#' @examples
#' \dontrun{
#' invest <- calcula_custo_expansao_agua(demanda, distribuicao, coleta, producao, tratamento, perda_agua)
#' }
calcula_custo_expansao_agua <- function(demanda,
                                        distribuicao,
                                        producao,
                                        perda_agua) {
  tabela <- calcula_custo_extensao(demanda, distribuicao, "distribuicao_agua")
  tabela <- calcula_custo_expansao_producao(tabela, producao, perda_agua)
  return(tabela)
}

#' Consolida os dados de investimentos para água
#'
#' Totaliza os dados investimento em expansão, reposição e total.
#' Adiciona coluna com o país para totalização.
#'
#' @param tabela contendo as colunas:
#' \itemize{
#' \item{custo_expansao_distribuicao_agua}
#' \item{custo_expansao_producao_agua}
#' \item{custo_reposicao_producao_agua}
#' \item{custo_reposicao_distribuicao_agua}
#' }
#'
#' @return tabela contendo os campos:
#' \itemize{
#'  \item{investimento_expansao}
#'  \item{investimento_reposicao}
#'  \item{investimento_total}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tabela <- consolida_investimentos_agua(tabela)
#' }
consolida_investimentos_agua <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    investimento_expansao = custo_expansao_distribuicao_agua + custo_expansao_producao_agua,
    investimento_reposicao = custo_reposicao_producao_agua + custo_reposicao_distribuicao_agua,
    investimento_total = investimento_expansao + investimento_reposicao,
  )
}

#' Cria tabela longa de necessidade de investimento do componente água
#'
#' @param tabela contendo as colunas:
#' \itemize{
#' \item{deficit_urbana}
#' \item{custo_expansao_producao_agua}
#' \item{custo_reposicao_producao_agua}
#' \item{custo_reposicao_distribuicao_agua}
#' }
#'
#' @return tabela contendo os campos:
#' \itemize{
#'  \item{estado}
#'  \item{regiao}
#'  \item{componente}
#'  \item{situacao}
#'  \item{destino}
#'  \item{subsistema}
#'  \item{necessidade_investimento}
#' }
#'
#' @export
tbl_longa_investimentos_agua <- function(tabela) {
  colunas <- c(
    "estado", "regiao",
    "custo_expansao_distribuicao_agua",
    "custo_expansao_producao_agua",
    "custo_reposicao_producao_agua",
    "custo_reposicao_distribuicao_agua"
  )
  tabela <- dplyr::select(tabela, dplyr::all_of(colunas))
  tabela <- somar_por_campo(tabela, "estado")
  tabela <- tidyr::pivot_longer(
    tabela,
    cols = starts_with("custo_"),
    names_to = c("destino", "subsistema"),
    names_pattern = "custo_(.*?)_(.*)",
    values_to = "necessidade_investimento"
  )
  tabela <- dplyr::mutate(
    tabela,
    componente = "agua",
    situacao = "urbana"
  )
}

#' Cria tabela longa de deficit do componente água e situacao urbana
#'
#' @param tabela contendo as colunas:
#' \itemize{
#' \item{estado}
#' \item{regiao}
#' \item{deficit_urbana}
#' }
#'
#' @return tabela contendo os campos:
#' \itemize{
#'  \item{estado}
#'  \item{regiao}
#'  \item{componente}
#'  \item{situacao}
#'  \item{subsistema}
#'  \item{deficit}
#' }
#'
#' @export
tbl_longa_deficit_agua <- function(tabela) {
  colunas <- c("estado", "regiao", "deficit_urbana")
  tabela <- dplyr::select(tabela, dplyr::all_of(colunas))
  tabela <- somar_por_campo(tabela, "estado")
  tabela <- tidyr::pivot_longer(
    tabela,
    cols = starts_with("deficit_"),
    names_to = "situacao",
    names_pattern = "deficit_(.*)",
    values_to = "deficit"
  )
  tabela <- dplyr::mutate(
    tabela,
    componente = "agua",
    subsistema = "producao_distribuicao",
    deficit = as.integer(deficit)
  )
}



#' Módulo demográfico
#'
#' Calcula as necessidades
#'
#' @param input estrutura de dados (`reactive`) que guarda os parâmetros da interface gráfica
#' @param projecao é o `data.frame` com o estado atual da projeção populacional
#' @param tema define qual tema será caculado (`agua` ou `esgoto`)
#'
#' @return um `data.frame` contendo as necessidades de intraestrutura para um dado `tema`
#' @export
#'
#' @examples
#' \dontrun{
#' tabela <- rodar_modulo_demografico(input, projecao, "agua")
#' }
rodar_modulo_demografico <- function(input, projecao, tema) {
  ano <- input$geral$ano
  ano_corrente <- input$geral$ano_corrente
  tabela <- base_municipios()
  tabela <- dplyr::left_join(
    tabela,
    carrega_base_calculo("agua", input$agua$fonte_nome, input$agua$fonte_ano),
    by = "codigo_municipio"
  )
  tabela <- dplyr::left_join(
    tabela,
    carrega_base_calculo("esgoto", input$agua$fonte_nome, input$agua$fonte_ano),
    by = "codigo_municipio"
  )
  tabela <- adiciona_populacao_corrente(projecao, ano_corrente, tabela)
  if (input$agua$atendimento == "censo") {
    rlog::log_info("Adicionando atendimento Água CENSO 2022")
    tabela <- adiciona_atendimento_censo_2022(tabela, "agua")
  }
  if (input$esgoto$atendimento == "censo") {
    rlog::log_info("Adicionando atendimento Esgoto CENSO 2022")
    tabela <- adiciona_atendimento_censo_2022(tabela, "esgoto")
  }
  if (input$agua$atendimento == "pnadc") {
    rlog::log_info("Adicionando atendimento Água PNADc")
    tabela <- adiciona_atendimento_pnadc(tabela, "agua", input$agua$atendimento_ano)
  }
  if (input$esgoto$atendimento == "pnadc") {
    rlog::log_info("Adicionando atendimento Esgoto PNADc")
    tabela <- adiciona_atendimento_pnadc(tabela, "esgoto", input$esgoto$atendimento_ano)
  }

  rlog::log_info(sprintf("%s: carregado sinisa (%s, %s)", tema, nrow(tabela), ncol(tabela)))
  tabela <- necessidade_agua_esgoto(tabela)
  rlog::log_info(sprintf("%s: preenchendo dados de densidade", tema))
  tabela <- fill_missing_density(tabela, c(
    "densidade_distribuicao_agua", "densidade_producao_agua", "densidade_coleta_esgoto"
  ))
  rlog::log_info(sprintf("%s: adicionando projecao", tema))
  tabela <- adiciona_projecao_populacao(projecao, ano, tabela)
  rlog::log_info(sprintf("%s: calculando demandas", tema))
  if (tema == "agua") {
    tabela <- calculate_demografico_agua(tabela, input$agua$meta_agua)
  }
  if (tema == "esgoto") {
    tabela <- calculate_demografico_esgoto(tabela, input$esgoto$meta_esgoto, input$esgoto$proporcao)
  }
  rlog::log_info(sprintf("%s: classificando municipios", tema))
  tabela <- classifica_municipio(tabela)
  return(tabela)
}


#' Calcula o fator de correção monetária baseado no índice IGP
#'
#' Esta função calcula o fator de correção monetária para ajustar os preços de acordo com o índice IGP (Índice Geral de Preços).
#'
#' @param sinapi Uma string representando o nome do arquivo SINAPI, que inclui o ano e o mês no formato "SINAPI_YYYYMM".
#' @param ano_corrente Um número representando o ano corrente para o cálculo do fator de correção.
#'
#' @return Um valor numérico representando o fator de correção monetária.
#' @export
#'
#' @examples
#' \dontrun{
#' fator <- calcula_fator_correcao_sinapi("SINAPI_202112", 2025)
#' }
calcula_fator_correcao_sinapi <- function(sinapi, ano_corrente) {
  sinapi_anomes <- strsplit(sinapi, "_")[[1]][2]
  sinapi_ano <- as.numeric(substr(sinapi_anomes, 1, 4))
  sinapi_mes <- as.numeric(substr(sinapi_anomes, 5, 6))
  initial_date <- as.Date(paste0(sinapi_ano, "-", sinapi_mes, "-01"))
  current_date <- as.Date(paste0(ano_corrente, "-12-31"))
  taxa <- fator_correcao_incc(initial_date, current_date)
  return(taxa)
}

#' Módulo orçamentário para água
#'
#' @param input estrutura de dados (`reactive`) que guarda os parâmetros da interface gráfica
#' @param demografico estrutura de dados (`data.frame`) que armazena os resultados do módulo demográfico
#'
#' @return  estrutura de dados (`data.frame`) que armazenas os resultados do módulo orçamentário
#' @export
#'
#' @examples
#' \dontrun{
#' orca <- rodar_modulo_orcamentario_agua(input, demografico)
#' }
rodar_modulo_orcamentario_agua <- function(input, demografico) {
  sinapi <- carrega_sinapi(input$agua$sinapi)
  ano_corrente <- input$geral$ano_corrente
  fator_correcao <- calcula_fator_correcao_sinapi(input$agua$sinapi, ano_corrente)
  projeto_distribuicao_agua <- carrega_dado_auxiliar("agua_projeto_distribuicao")

  distribuicao <- calcula_precos_distribuicao(
    projeto_distribuicao_agua,
    sinapi,
    taxa_servico = input$agua$fator_servicos,
    taxa_materiais = input$agua$fator_materiais,
    fator_correcao = fator_correcao
  )
  salva_resultado_intermediario(
    tidyr::pivot_wider(
      distribuicao,
      names_from = "projeto",
      values_from = "preco"
    ), "agua_custo_distribuicao",
    "base"
  )
  projeto_producao_agua_unidades <- carrega_dado_auxiliar("agua_projeto_producao_unidades")
  producao <- calcula_preco_unidades_producao(
    projeto_producao_agua_unidades,
    sinapi,
    input$agua$fator_insumo,
    input$agua$fator_composicao,
    fator_correcao
  )
  salva_resultado_intermediario(
    tidyr::pivot_wider(
      producao,
      names_from = "unidade",
      values_from = "preco"
    ), "agua_custo_unidade_producao",
    "base"
  )

  custo <- calcula_custo_expansao_agua(
    demografico,
    distribuicao,
    producao,
    input$agua$perda_agua
  )
  resultado <- list(
    distribuicao = distribuicao,
    producao = producao,
    custo = custo
  )

  return(resultado)
}




#' Capacidade instalada sistema de abastecimento de água
#'
#' @param snis um `data.frame` contendo as colunas `codigo_municipio`, `extensao_rede_agua_km` e `volume_agua_produzido_dam3_ano`
#' @param custo um `data.frame` contendo as colunas `codigo_municipio`, `custo_relativo_producao`, `preco_distribuicao_agua`
#'
#' @return um `data.frame` contendo as colunas `capacidade_instalada_distribuicao` e `capacidade_instalada_producao`
#' @export
capacidade_instalada_agua <- function(snis, custo) {
  # Quando não informada assume extensões igual a 0
  snis$extensao_rede_agua_km[is.na(snis$extensao_rede_agua_km)] <- 0
  snis$volume_agua_produzido_dam3_ano[is.na(snis$volume_agua_produzido_dam3_ano)] <- 0

  tabela <- dplyr::left_join(snis, custo, by = "codigo_municipio")
  tabela <- dplyr::mutate(
    tabela,
    capacidade_instalada_distribuicao = extensao_rede_agua_km * 1e3 * preco_distribuicao_agua,
    capacidade_instalada_producao = volume_agua_produzido_dam3_ano * 1e3 * custo_relativo_producao
  )
  return(tabela)
}


#' Módulo financeiro para demanda urbana de água
#'
#' Esta função organiza a ordem de execução das tarefas necessárias
#' para o cálculo de necessidades de investimento em sistema de abastecimento de água.
#'
#' @param input estrutura de dados (`reactive`) que guarda os parâmetros da interface gráfica
#' @param orcamentario um `data.frame` contendo a saida do modulo orcamentário
#' @export
#'
#' @return um `data.frame` contendo as necessidade de investimentos e todos campos utilizados
rodar_modulo_financeiro_agua <- function(input, orcamentario) {
  snis_data <- carrega_base_calculo("agua", input$agua$fonte_nome, input$agua$fonte_ano)
  if (input$agua$atendimento == "censo") {
    snis_data <- adiciona_atendimento_censo_2022(snis_data, "agua")
  }
  custo <- orcamentario$custo
  tabela <- capacidade_instalada_agua(snis_data, custo)
  ano_final <- input$geral$ano
  ano_inicial <- input$geral$ano_corrente
  rlog::log_info(sprintf("agua anoi=%s anof=%s", ano_inicial, ano_final))
  ano_corrente <- input$geral$ano_corrente

  vars <- list(
    campos_reposicao(
      "capacidade_instalada_distribuicao",
      "custo_expansao_distribuicao_agua",
      "custo_reposicao_distribuicao_agua",
      input$agua$vida_util
    ),
    campos_reposicao(
      "capacidade_instalada_producao",
      "custo_expansao_producao_agua",
      "custo_reposicao_producao_agua",
      input$agua$vida_util
    )
  )

  for (var in vars) {
    tabela <- calcula_reposicao_parcial(
      tabela,
      var$capacidade,
      var$custo,
      var$reposicao,
      ano_inicial,
      ano_final,
      ano_corrente,
      var$vida_util
    )
  }
  return(tabela)
}

#' Calcula a necessidade de investimento para água
#'
#' @param state estado da aplicação
#'
#' @return estado da aplicação
#' @export
#'
#' @examples
#' \dontrun{
#' app_state <- investimento_agua(state)
#' }
investimento_agua <- function(state) {
  rlog::log_info("água: carregando projecao populacional")
  projecao <- state$projecao
  rlog::log_info("água: carregando parâmetros")
  input <- state$input
  rlog::log_info("água: rodando módulo rural")
  state <- rodar_modulo_rural_agua(state)
  rlog::log_info("água: rodando módulo demografico")
  demografico <- rodar_modulo_demografico(input, projecao, "agua")
  rlog::log_info("água: rodando módulo orcamentario")
  orcamentario <- rodar_modulo_orcamentario_agua(input, demografico)
  rlog::log_info("água: rodando módulo financeiro")
  financeiro <- rodar_modulo_financeiro_agua(input, orcamentario)
  tabela <- consolida_investimentos_agua(financeiro)
  tabela <- adiciona_pais(tabela)
  tabela <- adiciona_regiao(tabela)
  state$agua <- tabela

  state$necessidade <- dplyr::bind_rows(
    tbl_longa_investimentos_agua(tabela),
    state$necessidade
  )
  state$deficit <- dplyr::bind_rows(
    tbl_longa_deficit_agua(tabela),
    state$deficit
  )
  return(state)
}
