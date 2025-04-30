#' Necessidade de Investimento - Componente Resíduos - Situação Urbana

#' Une tabelas por faixa populacional
#'
#' Faz a união entre duas tabelas, a tabela 1 manterá todas suas linhas.
#' Os valores serão `NA` caso não haja uma faixa populacional na tabela2 mas haja na tabela 1.
#' Ambas tabelas deve ter o campo `faixa`.
#'
#' @param tabela1 o `data.frame` que manterá todas as linhas
#' @param tabela2 o `data.frame` com campos adicionais
#'
#' @return um `data.frame` com as tabelas unidas
#' @export
preco_unidade_faixa <- function(tabela1, tabela2) {
  tabela <- dplyr::left_join(
    tabela1,
    tabela2,
    by = "faixa"
  )
}

#' Totaliza os valores de investimentos em resíduos
#'
#' Consolida os valores de investimentos para reposicao, expansão e total
#'
#' @param tabela contendo os valores de reposição e investimento em resíduos:
#' \itemize{
#' \item[investimento_reposicao_aterro]
#' \item[investimento_reposicao_compostagem]
#' \item[investimento_reposicao_triagem]
#' \item[investimento_reposicao_coleta_indiferenciada]
#' \item[investimento_reposicao_coleta_seletiva]
#' \item[investimento_expansao_aterro]
#' \item[investimento_expansao_compostagem]
#' \item[investimento_expansao_triagem]
#' \item[investimento_expansao_transbordo]
#' \item[investimento_expansao_coleta_indiferenciada]
#' \item[investimento_expansao_coleta_seletiva]
#' }
#'
#' @return um `data.frame` contendo as colunas adicionais `investimento_reposicao`, `investimento_expansao` e `investimento_total`
#' @export
investimento_residuos_total <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    investimento_reposicao = investimento_reposicao_aterro +
      investimento_reposicao_compostagem +
      investimento_reposicao_triagem +
      investimento_reposicao_coleta_indiferenciada +
      investimento_reposicao_coleta_seletiva,
    investimento_expansao = investimento_expansao_aterro +
      investimento_expansao_compostagem +
      investimento_expansao_triagem +
      investimento_expansao_transbordo +
      investimento_expansao_coleta_indiferenciada +
      investimento_expansao_coleta_seletiva,
    investimento_total = investimento_reposicao + investimento_expansao
  )
}

#' Cria tabela longa de necessidade de investimento do componente residuos
#'
#' @param tabela contendo as colunas:
#' \itemize{
#' \item[investimento_reposicao_aterro]
#' \item[investimento_reposicao_compostagem]
#' \item[investimento_reposicao_triagem]
#' \item[investimento_reposicao_coleta_indiferenciada]
#' \item[investimento_reposicao_coleta_seletiva]
#' \item[investimento_expansao_aterro]
#' \item[investimento_expansao_compostagem]
#' \item[investimento_expansao_triagem]
#' \item[investimento_expansao_transbordo]
#' \item[investimento_expansao_coleta_indiferenciada]
#' \item[investimento_expansao_coleta_seletiva]
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
#' }
#'
#' @export
tbl_longa_investimento_residuos <- function(tabela) {
  colunas <- c(
    "estado", "regiao",
    "investimento_reposicao_aterro",
    "investimento_reposicao_compostagem",
    "investimento_reposicao_triagem",
    "investimento_reposicao_coleta_indiferenciada",
    "investimento_reposicao_coleta_seletiva",
    "investimento_expansao_aterro",
    "investimento_expansao_compostagem",
    "investimento_expansao_triagem",
    "investimento_expansao_transbordo",
    "investimento_expansao_coleta_indiferenciada",
    "investimento_expansao_coleta_seletiva"
  )
  tabela <- dplyr::select(tabela, dplyr::all_of(colunas))
  tabela <- somar_por_campo(tabela, "estado")
  tabela <- tidyr::pivot_longer(
    tabela,
    cols = colunas[3:length(colunas)],
    names_to = c("destino", "subsistema"),
    names_pattern = "investimento_(.*?)_(.*)",
    values_to = "necessidade_investimento"
  )
  tabela <- dplyr::mutate(
    tabela,
    componente = "residuos",
    situacao = "urbana",
  )
}

#' Cria tabela longa de deficit do componente residuos
#'
#' @param tabela contendo as colunas:
#' \itemize{
#' \item{estado}
#' \item{regiao}
#' \item{deficit_coleta_seletiva}
#' \item{deficit_coleta_indiferenciada}
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
tbl_longa_deficit_residuos <- function(tabela) {
  colunas <- c(
    "estado", "regiao",
    "deficit_coleta_seletiva",
    "deficit_coleta_indiferenciada"
  )
  tabela <- dplyr::select(tabela, dplyr::all_of(colunas))
  tabela <- somar_por_campo(tabela, "estado")
  tabela <- tidyr::pivot_longer(
    tabela,
    cols = starts_with("deficit_"),
    names_to = "subsistema",
    names_pattern = "deficit_(.*)",
    values_to = "deficit"
  )
  tabela <- dplyr::mutate(
    tabela,
    componente = "residuos",
    situacao = "urbana",
    deficit = as.integer(deficit)
  )
}


# ATERRO -----------------------------------------------------------------------

#' Demanda por aterro
#'
#' @param tabela um `data.frame` contendo a coluna `total_residuos_projecao`
#' @param vida_util valor em anos da vida util do aterro
#' @return um `data.frame` contendo a coluna `demanda_aterro`
#' @export
demanda_aterro <- function(tabela, vida_util) {
  tabela <- dplyr::mutate(
    tabela,
    demanda_aterro = ((total_residuos_projecao - total_residuos) + residuos_disposicao_inadequada) * vida_util
  )
}

#' Investimento em Aterros
#'
#' Estima o investimento necessários em aterro como a multiplicação de: total de residuos, preço unitario por massa e vida útil do aterro.
#'
#' @param tabela um `data.frame` contendo as colunas `demanda_aterro` e `preco_unidade_aterro`
#'
#' @return um `data.frame` contendo a coluna `investimento_expansao_aterro`
#' @export
investimento_expansao_aterro <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    investimento_expansao_aterro = demanda_aterro * preco_unidade_aterro
  )
}

#' Capacidade instalada aterro
#'
#' @param tabela um `data.frame` contendo as colunas total_residuos, preco_unidade_aterro
#' @param vida_util um campo numérico contendo a vida util do aterro
#'
#' @return um `data.frame` contendo o valor da capacidade instalada do aterro (`capacidade_instalada_aterro`)
#' @export
#'
#' @examples
#' \dontrun{
#' tabela <- capacidade_instalada_aterro(tabela, 20)
#' }
capacidade_instalada_aterro <- function(tabela, vida_util) {
  tabela <- dplyr::mutate(
    tabela,
    capacidade_instalada_aterro = total_residuos * preco_unidade_aterro * vida_util
  )
}

# TRIAGEM

#' Demanda por triagem
#'
#' @param tabela um `data.frame` contendo as colunas `meta_reaproveitamento`, `total_residuos_projecao` e `CS026`
#'
#' @return um `data.frame` com a coluna `demanda_triagem`
#' @export
demanda_triagem <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    demanda_triagem = pmax(meta_reaproveitamento / 100.0 * total_residuos_projecao -
      projecao_reaproveitamento, 0.0),
  )
}

#' Investimento em triagem
#'
#' @param tabela  um `data.frame` contendo as colunas `demanda_triagem` e `preco_unidade_triagem`
#' @param vida_util um `number` com o numero de anos da vida útil do sistema de triagem
#'
#' @return um `data.frame` com a coluna `investimento_expansao_triagem`
#' @export
investimento_expansao_triagem <- function(tabela, vida_util) {
  tabela <- dplyr::mutate(
    tabela,
    investimento_expansao_triagem = demanda_triagem * preco_unidade_triagem * vida_util
  )
}

#' Capacidade instalada de triagem
#'
#' @param tabela um `data.frame` contendo as colunas `meta_reaproveitamento`, `total_residuos` e `preco_unidade_triagem`
#' @param vida_util um `number` com o numero de anos da vida útil do sistema de triagem
#'
#' @return um `data.frame` com a coluna `capacidade_instalada_triagem`
#' @export
capacidade_instalada_triagem <- function(tabela, vida_util) {
  tabela <- dplyr::mutate(
    tabela,
    capacidade_instalada_triagem = meta_reaproveitamento / 100.0 * total_residuos * preco_unidade_triagem * vida_util
  )
}

# COMPOSTAGEM ------------------------------------------------------------------

#' Demanda por compostagem
#'
#' @param tabela um `data.frame` contendo as colunas `meta_compostagem`, `total_residuos_projecao` e `residuo_compostagem_ton_ano`.
#'
#' @return um `data.frame` com a coluna `demanda_compostagem`
#' @export
demanda_compostagem <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    demanda_compostagem = meta_compostagem / 100.0 * total_residuos_projecao - pmax(residuo_compostagem_ton_ano, 0.0),
  )
}

#' Investimento em compostagem
#'
#' @param tabela um `data.frame` contendo as colunas `demanda_compostagem` e `preco_unidade_compostagem`
#' @param vida_util um `number` com o numero de anos da vida útil do sistema
#'
#' @return um `data.frame` com a coluna `investimento_expansao_compostagem`
#' @export
investimento_expansao_compostagem <- function(tabela, vida_util) {
  tabela <- dplyr::mutate(
    tabela,
    investimento_expansao_compostagem = demanda_compostagem * preco_unidade_compostagem * vida_util
  )
}

#' Capacidade instalada de compostagem
#'
#' @param tabela um `data.frame` contendo as colunas `meta_compostagem`, `total_residuos` e `preco_unidade_compostagem`
#' @param vida_util um `number` com o numero de anos da vida útil do sistema
#'
#' @return um `data.frame` com a coluna `capacidade_instalada_compostagem`
#' @export
capacidade_instalada_compostagem <- function(tabela, vida_util) {
  tabela <- dplyr::mutate(
    tabela,
    capacidade_instalada_compostagem = meta_compostagem / 100.0 * total_residuos * preco_unidade_compostagem * vida_util
  )
}

# COLETA SELETIVA --------------------------------------------------------------

#' Densidade de caminhões basculante
#'
#' @param tabela um `data.frame` contendo as colunas `quantidade_caminhoes_basculantes` e `atendimento_coleta_seletiva_hab` (SNIS)
#'
#' @return tabela com o campo adicional densidade de caminhoes baú (`densidade_caminhoes_bau`)
#' @export
densidade_caminhoes_bau <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    densidade_caminhoes_bau = ifelse(
      quantidade_caminhoes_basculantes > 0,
      atendimento_coleta_seletiva_hab / quantidade_caminhoes_basculantes,
      NA
    )
  )
}

#' Preenche por faixa populacional
#'
#' O dado faltante de um estado e uma faixa populacional é preenchido com valores que variam somente com a faixa populacional (`tabela_faixa`)
#'
#' @param tabela um `data.frame` com as colunas a serem preenchidas
#' @param tabela_faixa um `data.frame` contendo os valores que serão base para o preenchimento devem conter a coluna com os nomes fornecidos em `campo` e `campo_faixa`
#' @param preencher_campo um `character` com o nome da coluna a ser preenchida
#' @param campo_faixa um `character` com o nome da coluna da faixa populacional
#'
#' @return o mesmo `data.frame` com o campo preenchido
#' @export
#'
#' @examples
#' \dontrun{
#' tabela <- preenche_por_faixa_populacional(tabela, densidade)
#' }
preenche_por_faixa_populacional <- function(tabela,
                                            tabela_faixa,
                                            preencher_campo = "densidade",
                                            campo_faixa = "faixa") {
  tabela_faixa <- dplyr::select(
    tabela_faixa,
    dplyr::all_of(c(
      preencher_campo,
      campo_faixa
    ))
  )

  # transforma valores negativos e nulos em 0
  mask <- tabela[[preencher_campo]] <= 0
  tabela[[preencher_campo]][mask] <- NA

  tabela_faixa <- dplyr::rename(tabela_faixa, estimativa = .data[[preencher_campo]])
  tabela <- dplyr::left_join(tabela, tabela_faixa, by = campo_faixa)

  mask <- is.na(tabela[[preencher_campo]])
  tabela[[preencher_campo]][mask] <- tabela$estimativa[mask]

  tabela <- dplyr::select(
    tabela,
    -estimativa
  )
  return(tabela)
}

#' Preenche por regiao e faixa populacional
#'
#' O dado faltante de um estado e uma faixa populacional é preenchido com valores que variam somente com a faixa populacional (`tabela_faixa`)
#'
#' @param tabela um `data.frame` com as colunas a serem preenchidas
#' @param tabela_faixa um `data.frame` contendo os valores que serão base para o preenchimento devem conter a coluna com os nomes fornecidos em `campo` e `campo_faixa`
#' @param preencher_campo um `character` com o nome da coluna a ser preenchida
#' @param campo_regiao um `character` com o nome da coluna região
#' @param campo_faixa um `character` com o nome da coluna faixa populacional
#'
#' @return o mesmo `data.frame` com o campo preenchido
#' @export
#'
#' @examples
#' \dontrun{
#' tabela <- preenche_por_regiao_faixa_populacional(tabela, densidade)
#' }
preenche_por_regiao_faixa_populacional <-
  function(tabela,
           tabela_faixa,
           preencher_campo = "densidade",
           campo_regiao = "regiao",
           campo_faixa = "faixa") {
    tabela_faixa <- dplyr::select(
      tabela_faixa,
      dplyr::all_of(c(
        preencher_campo,
        campo_regiao,
        campo_faixa
      ))
    )

    # transforma valores negativos e nulos em 0
    mask <- tabela[[preencher_campo]] <= 0
    tabela[[preencher_campo]][mask] <- NA

    tabela_faixa <- dplyr::rename(
      tabela_faixa,
      estimativa = .data[[preencher_campo]]
    )
    tabela <- dplyr::left_join(
      tabela, tabela_faixa,
      by = c(campo_regiao, campo_faixa)
    )

    mask <- is.na(tabela[[preencher_campo]])
    tabela[[preencher_campo]][mask] <- tabela$estimativa[mask]

    tabela <- dplyr::select(
      tabela,
      -estimativa
    )
    return(tabela)
  }

#' Substitui por faixa populacional
#'
#' O dados faltante de um campo é preenchido com valores que variam somente com a faixa populacional (`tabela_faixa`)
#'
#' @param tabela um `data.frame` com as colunas a serem preenchidas
#' @param tabela_faixa um `data.frame` contendo os valores que serão base para o preenchimento devem conter a coluna com os nomes fornecidos em `campo` e `campo_faixa`
#' @param preencher_campo um `character` com o nome da coluna a ser preenchida
#' @param campo_faixa um `character` com o nome da coluna da faixa populacional
#'
#' @return o mesmo `data.frame` com o campo substituido
#' @export
#'
#' @examples
#' \dontrun{
#' tabela <- substitui_por_faixa_populacional(tabela, densidade)
#' }
substitui_por_faixa_populacional <- function(tabela,
                                             tabela_faixa,
                                             preencher_campo = "densidade",
                                             campo_faixa = "faixa") {
  campos_para_filtrar <- dplyr::all_of(c(preencher_campo, campo_faixa))
  tabela_faixa <- dplyr::select(
    tabela_faixa,
    campos_para_filtrar
  )
  tabela_faixa <- dplyr::rename(
    tabela_faixa,
    estimativa = .data[[preencher_campo]]
  )
  tabela <- dplyr::left_join(tabela, tabela_faixa, by = campo_faixa)
  tabela[[preencher_campo]] <- tabela$estimativa
  tabela <- dplyr::select(
    tabela,
    -estimativa
  )
  return(tabela)
}

#' Mascara de municipios que contém coleta seletiva
#'
#' Retorna um vetor de `boolean` sobre a existência (`TRUE`) ou não (`FALSE`) de coleta seletiva.
#' A análise é baseada no campo tem_coleta_seletiva do SNIS.
#' @param tabela um `data.frame` contendo a coluna tem_coleta_seletiva.
#'
#' @return vetor de boleanos para cada municipio que não contém coleta seletiva
#' @export
#'
#' @examples
#' tabela <- dplyr::tibble(tem_coleta_seletiva = c("Sim", "Não"))
#' mask <- mascara_coleta_seletiva(tabela)
mascara_coleta_seletiva <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    quantidade_caminhoes_basculantes = ifelse(tem_coleta_seletiva == "Sim", quantidade_caminhoes_basculantes, NA),
  )
  return(tabela)
}


#' Preenche atendimento coleta seletiva
#'
#' 1. Filtra os municipios que tem coleta seletiva
#' 2. Calcula o atendimento relativo por regiao e faixa
#' 3. Preenche os municípios que tem coleta seletiva e que nao forneceram dados.
#'    Os valores do atendimento é calculado pela populacao_urbana * média relativa de atendimento da regiao e faixa populacional do município.
#' 4. Se a populacao atentida for maior que a populacao urbana define a populacao atendida igual a populacao urbana
#'
#' @param tabela um `data.frame` com as colunas "codigo_municipio", "regiao", "faixa", "populacao_urbana_corrente", "atendimento_coleta_seletiva_hab", "tem_coleta_seletiva"
#'
#' @return um `data.frame` contendo a coluna atendimento_relativo_coleta_seletiva
#' @export
preenche_atendimento_coleta_seletiva <- function(tabela) {
  vars <- c("codigo_municipio", "regiao", "faixa", "populacao_urbana_corrente", "atendimento_coleta_seletiva_hab", "tem_coleta_seletiva")
  regiao_faixa <- dplyr::select(tabela, dplyr::all_of(vars))
  regiao_faixa <- dplyr::filter(regiao_faixa, tem_coleta_seletiva == "Sim")
  regiao_faixa <- dplyr::mutate(regiao_faixa,
    populacao_urbana_corrente = ifelse(populacao_urbana_corrente > 0, populacao_urbana_corrente, NA),
    atendimento_coleta_seletiva_hab = ifelse(atendimento_coleta_seletiva_hab > 0, atendimento_coleta_seletiva_hab, NA)
  )
  regiao_faixa <- stats::na.omit(regiao_faixa)
  regiao_faixa <- dplyr::group_by(regiao_faixa, regiao, faixa)
  regiao_faixa <- dplyr::summarise(regiao_faixa, populacao_urbana_corrente = sum(populacao_urbana_corrente), atendimento_coleta_seletiva_hab = sum(atendimento_coleta_seletiva_hab))
  regiao_faixa <- dplyr::transmute(
    regiao_faixa,
    regiao = regiao,
    faixa = faixa,
    relativo = atendimento_coleta_seletiva_hab / populacao_urbana_corrente
  )
  tabela <- dplyr::left_join(
    tabela,
    regiao_faixa,
    by = c("regiao", "faixa")
  )
  tabela <- dplyr::mutate(
    tabela,
    atendimento_coleta_seletiva_hab = ifelse(
      tem_coleta_seletiva == "Sim" & is.na(atendimento_coleta_seletiva_hab),
      relativo * populacao_urbana_corrente,
      atendimento_coleta_seletiva_hab
    ),
    atendimento_coleta_seletiva_hab = ifelse(
      atendimento_coleta_seletiva_hab > populacao_urbana_corrente,
      populacao_urbana_corrente,
      atendimento_coleta_seletiva_hab
    )
  )
  tabela$relativo <- NULL
  return(tabela)
}

#' Preenche caminhao basculante
#'
#' 1. Filtra os municipios que tem coleta seletiva
#' 2. Calcula a quantidade de caminhões basculante por faixa
#'
#' @param tabela_por_municipio um `data.frame` com as colunas "codigo_municipio", "faixa", "tem_coleta_seletiva", "atendimento_coleta_seletiva_hab", "quantidade_caminhoes_basculantes"
#' @param estado_faixa um `data.frame` com as colunas "estado", "faixa", "populacao_urbana_corrente", "atendimento_coleta_seletiva_hab", "tem_coleta_seletiva"
#'
#' @return um `data.frame` contendo a coluna atendimento_relativo_coleta_seletiva
#' @export
preenche_densidade_caminhao_bau <- function(tabela_por_municipio, estado_faixa) {
  vars <- c("codigo_municipio", "faixa", "tem_coleta_seletiva", "atendimento_coleta_seletiva_hab", "quantidade_caminhoes_basculantes")
  por_faixa <- dplyr::select(tabela_por_municipio, dplyr::all_of(vars))
  por_faixa <- dplyr::filter(por_faixa, tem_coleta_seletiva == "Sim")
  por_faixa[is.na(por_faixa)] <- 0.0
  por_faixa <- dplyr::transmute(por_faixa,
    faixa = faixa,
    atendimento_coleta_seletiva_hab = atendimento_coleta_seletiva_hab,
    quantidade_caminhoes_basculantes = quantidade_caminhoes_basculantes
  )
  por_faixa <- dplyr::group_by(por_faixa, faixa)
  por_faixa <- dplyr::summarise(por_faixa,
    quantidade_caminhoes_basculantes = sum(quantidade_caminhoes_basculantes), atendimento_coleta_seletiva_hab = sum(atendimento_coleta_seletiva_hab)
  )
  por_faixa <- dplyr::transmute(
    por_faixa,
    faixa = faixa,
    densidade_caminhoes_bau = ifelse(atendimento_coleta_seletiva_hab > 0, atendimento_coleta_seletiva_hab / quantidade_caminhoes_basculantes, NA)
  )
  # Valores obtidos do relatório
  por_faixa$densidade_caminhoes_bau[1] <- 2172.700
  por_faixa$densidade_caminhoes_bau[2] <- 4771.364
  por_faixa$densidade_caminhoes_bau[3] <- 8992.435
  por_faixa$densidade_caminhoes_bau[4] <- 13433.316
  por_faixa$densidade_caminhoes_bau[5] <- 21839.400
  por_faixa$densidade_caminhoes_bau[6] <- 13146.273
  por_faixa$densidade_caminhoes_bau[7] <- 48173.000

  estado_faixa <- dplyr::left_join(
    estado_faixa, por_faixa,
    by = "faixa"
  )
  return(estado_faixa)
}

#' Atendimento relativo para coleta seletiva
#'
#' @param tabela um `data.frame` com as colunas `atendimento_coleta_seletiva_hab` e `populacao_urbana_corrente`
#'
#' @return um `data.frame` contendo a coluna atendimento_relativo_coleta_seletiva
#' @export
atendimento_relativo_coleta_seletiva <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    atendimento_relativo_seletiva_urbano = ifelse(
      populacao_urbana_corrente > 0, atendimento_coleta_seletiva_hab / populacao_urbana_corrente, NA
    ),
  )
}

#' Déficit de coleta seletiva
#'
#' @param tabela um `data.frame` contendo as colunas `populacao_urbana` e `atendimento_relativo_seletiva_urbano`
#'
#' @return um `data.frame` contendo a coluna deficit_coleta_seletiva
#' @export
deficit_coleta_seletiva <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    deficit_coleta_seletiva = pmax(populacao_urbana * (1.0 - atendimento_relativo_seletiva_urbano), 0)
  )
}

#' Necessidade de investimento em coleta seletiva
#'
#' @param tabela um `data.frame` contendo as colunas `deficit_coleta_seletiva` e `densidade_caminhoes_bau`
#' @param valor um `double` contendo o preço de um caminhão basculante
#'
#' @return um `data.frame` contendo a coluna `investimento_expansao_coleta_seletiva`
#' @export
investimento_expansao_coleta_seletiva <- function(tabela, valor) {
  tabela <- dplyr::mutate(
    tabela,
    investimento_expansao_coleta_seletiva = ifelse(
      densidade_caminhoes_bau > 0,
      deficit_coleta_seletiva / densidade_caminhoes_bau * valor,
      NA
    )
  )
}

#' Capacidade instalada coleta seletiva
#'
#' @param tabela um `data.frame` contendo as colunas `populacao_urbana_corrente`, `atendimento_relativo_seletiva_urbano` e `densidade_caminhoes_bau `
#' @param valor valor um `double` contendo o preço de um caminhão basculante
#'
#' @return um `data.frame` contendo a coluna capacidade_instalada_coleta_seletiva
#' @export
capacidade_instalada_coleta_seletiva <- function(tabela, valor) {
  tabela <- dplyr::mutate(
    tabela,
    capacidade_instalada_coleta_seletiva = ifelse(
      densidade_caminhoes_bau > 0,
      populacao_urbana_corrente * (1.0 - atendimento_relativo_seletiva_urbano) /
        densidade_caminhoes_bau * valor,
      NA
    )
  )
}

# COLETA REGULAR -----------------------------------------------------------------

#' Déficit para coleta indiferenciada
#'
#' O déficit é a meta em % multiplicada pela populacao total estimada menos o atendimento atu
#'
#' @param tabela é um `data.frame` contendo as colunas `meta_coleta`, `populacao_total` e `atendimento_coleta_indiferenciada_hab`
#'
#' @return um `data.frame` contendo a coluna `coleta_indiferenciada`
#' @export
#'
#' @examples
#' \dontrun{
#' tabela <- deficit_coleta_indiferenciada(tabela)
#' }
deficit_coleta_indiferenciada <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    deficit_coleta_indiferenciada = pmax(meta_coleta / 100.0 * populacao_total - atendimento_coleta_indiferenciada_hab, 0),
  )
}

#' Necessidade de investimento em coleta indiferenciada
#'
#' @param tabela um `data.frame` contendo as colunas `deficit_coleta_indiferenciada` e `densidade_caminhoes_compactadores`
#' @param valor um `double` contendo o preço de um caminhão
#'
#' @return um `data.frame` contendo a coluna `investimento_expansao_coleta_indiferenciada`
#' @export
investimento_expansao_coleta_indiferenciada <- function(tabela, valor) {
  tabela <- dplyr::mutate(
    tabela,
    investimento_expansao_coleta_indiferenciada = ifelse(
      densidade_caminhoes_compactadores > 0,
      deficit_coleta_indiferenciada / densidade_caminhoes_compactadores * valor,
      NA
    )
  )
}

#' Capacidade instalada coleta indiferenciada
#'
#' @param tabela um `data.frame` contendo as colunas `populacao_urbana_corrente`, `atendimento_relativo_seletiva_urbano` e `densidade_caminhoes_compactadores `
#' @param valor valor um `double` contendo o preço de um caminhão
#'
#' @return um `data.frame` contendo a coluna capacidade_instalada_coleta_indiferenciada
#' @export
capacidade_instalada_coleta_indiferenciada <- function(tabela, valor) {
  tabela <- dplyr::mutate(
    tabela,
    capacidade_instalada_coleta_indiferenciada = ifelse(
      densidade_caminhoes_compactadores > 0,
      atendimento_coleta_indiferenciada_hab / densidade_caminhoes_compactadores * valor,
      NA
    )
  )
}

#' Preenche atendimento coleta indiferenciada
#'
#' 1. Filtra os municipios que tem coleta indiferenciada
#' 2. Calcula o atendimento relativo por regiao e faixa
#' 3. Preenche os municípios que tem coleta indiferenciada e que nao forneceram dados.
#'    Os valores do atendimento é calculado pela populacao_urbana * média relativa de atendimento da regiao e faixa populacional do município.
#' 4. Se a populacao atentida for maior que a populacao urbana define a populacao atendida igual a populacao urbana
#'
#' @param tabela um `data.frame` com as colunas "codigo_municipio", "regiao", "faixa", "populacao_urbana_corrente", "atendimento_coleta_indiferenciada_hab"
#'
#' @return um `data.frame` contendo a coluna atendimento_relativo_coleta_indiferenciada
#' @export
preenche_atendimento_coleta_indiferenciada <- function(tabela) {
  vars <- c("codigo_municipio", "regiao", "faixa", "populacao_total_corrente", "atendimento_coleta_indiferenciada_hab", "tem_pesagem")
  regiao_faixa <- dplyr::select(tabela, dplyr::all_of(vars))
  regiao_faixa <- dplyr::mutate(regiao_faixa,
    populacao_total_corrente = ifelse(populacao_total_corrente > 0, populacao_total_corrente, NA),
    atendimento_coleta_indiferenciada_hab = ifelse(atendimento_coleta_indiferenciada_hab > 0, atendimento_coleta_indiferenciada_hab, NA)
  )
  # TODO: Excel considera a populacao_total_corrente quando não tem atendimento_coleta_indiferenciada_hab, creio que o certo é não considerar
  # regiao_faixa <- stats::na.omit(regiao_faixa)
  regiao_faixa <- dplyr::group_by(regiao_faixa, regiao, faixa)
  regiao_faixa <- dplyr::summarise(regiao_faixa, populacao_total_corrente = sum(populacao_total_corrente, na.rm = TRUE), atendimento_coleta_indiferenciada_hab = sum(atendimento_coleta_indiferenciada_hab, na.rm = TRUE))
  regiao_faixa <- dplyr::transmute(
    regiao_faixa,
    regiao = regiao,
    faixa = faixa,
    relativo = atendimento_coleta_indiferenciada_hab / populacao_total_corrente
  )
  tabela <- dplyr::left_join(
    tabela,
    regiao_faixa,
    by = c("regiao", "faixa")
  )
  tabela <- dplyr::mutate(
    tabela,
    atendimento_coleta_indiferenciada_hab = ifelse(
      is.na(atendimento_coleta_indiferenciada_hab),
      round(relativo * populacao_total_corrente),
      atendimento_coleta_indiferenciada_hab
    ),
    atendimento_coleta_indiferenciada_hab = ifelse(
      atendimento_coleta_indiferenciada_hab > populacao_total_corrente,
      populacao_total_corrente,
      atendimento_coleta_indiferenciada_hab
    )
  )
  tabela$relativo <- NULL
  return(tabela)
}


#' Número de caminhões baú
#'
#' Cálcula o número de caminhões baú
#'
#' @param tabela um `data.frame` contendo as colunas CO063, CO064, CO065, CO066, CO067 e CO068
#'
#' @return um `data.frame` contendo a coluna `quantidade_caminhoes_basculantes`
#' @export
quantidade_caminhoes_basculantes <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    quantidade_caminhoes_basculantes = CO063 + CO064 + CO065 + CO066 + CO067 + CO068
  )
}

#' Densidade de caminhões compactadores
#'
#' Cálcula a densidade de caminhões compactadores
#'
#' @param tabela um `data.frame` contendo as colunas atendimento_coleta_indiferenciada_hab e quantidade_caminhoes_compactadores
#'
#' @return um `data.frame` contendo a coluna `densidade_caminhoes_compactadores`
#' @export
densidade_caminhoes_compactadores <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    densidade_caminhoes_compactadores = ifelse(
      quantidade_caminhoes_compactadores > 0,
      atendimento_coleta_indiferenciada_hab / quantidade_caminhoes_compactadores,
      NA
    )
  )
}

#' Adiciona metas de atendimento por regio
#'
#' @param tabela um `data.frame` com a coluna `regiao`
#'
#' @return um `data.frame` com as colunas adicionais `meta_coleta`, `meta_compostagem`e `meta_reaproveitamento`
#' @export
meta_plansab_residuo <- function(tabela) {
  plansab <- carrega_dado_auxiliar("residuos_meta_plansab")
  vars <- c("regiao", "meta_coleta", "meta_compostagem", "meta_reaproveitamento")
  plansab <- dplyr::select(plansab, dplyr::all_of(vars))
  tabela <- dplyr::left_join(
    tabela,
    plansab,
    by = "regiao"
  )
}

#' Adiciona tipo de disposição de resíduos
#'
#' @param tabela um `data.frame` com a coluna `codigo_municipio`
#'
#' @return um `data.frame` com as colunas adicionais `meta_coleta`, `meta_compostagem`e `meta_reaproveitamento`
#' @export
adiciona_tipo_disposicao <- function(tabela) {
  tipo_disposicao <- carrega_dado_auxiliar("residuos_tipo_disposicao")
  vars <- c("codigo_municipio", "tipo_disposicao")
  tipo_disposicao <- dplyr::select(tipo_disposicao, dplyr::all_of(vars))
  tabela <- dplyr::left_join(
    tabela,
    tipo_disposicao,
    by = "codigo_municipio"
  )
}

#' Adiciona quantidade de residuos com disposição inadequada
#'
#' @param tabela um `data.frame` com a coluna `residuo_coletado_ton_ano`
#'
#' @return um `data.frame` com a colunas adicional `residuos_disposicao_inadequada`
#' @export
disposicao_inadequada <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    disposicao_inadequada = ifelse(tipo_disposicao == "Inadequada", 1.0, 0.0),
    residuos_disposicao_inadequada = total_residuos * disposicao_inadequada
  )
}

#' Atendimento relativo para residuo
#'
#' Calcula o atendimento relativo total, urbano e rural.
#'
#' @param tabela um `data.frame` com as colunas `atendimento_coleta_indiferenciada_hab`, `CO050`, `populacao_total_corrente` e `populacao_urbana_corrente`.
#'
#' @return um `data.frame` contendo as coliunas `atendimento_relativo_total`, `atendimento_relativo_urbano` e `atendimento_relativo_rural`
#' @export
atendimento_relativo_residuos <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    atendimento_relativo_total = atendimento_coleta_indiferenciada_hab / populacao_total_corrente,
    atendimento_relativo_urbano = CO050 / populacao_urbana_corrente,
    atendimento_relativo_rural = ifelse(
      (populacao_total_corrente - populacao_urbana_corrente) > 0,
      (atendimento_coleta_indiferenciada_hab - CO050) / (populacao_total_corrente - populacao_urbana_corrente),
      NA
    )
  )
}

#' Geração de resíduos
#'
#' @param tabela um `data.frame` contendo as colunas populacao_total_corrente, residuo_coletado_ton_ano, atendimento_coleta_indiferenciada_hab, residuo_recuperado_ton_ano e populacao_total
#'
#' @return um `data.frame` contendo as colunas taxa_geracao_residuos, total_residuos, total_residuos_projecaoe
#' @export
geracao_residuos <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    total_residuos = taxa_geracao_residuos * populacao_total_corrente,
    total_residuos_projecao = taxa_geracao_residuos * populacao_total,
  )
}

#' Taxa de geração de resíduos (t/hab)
#'
#' Calcula geração de resíduos para municípios com pesagem
#'
#' @param tabela um `data.frame` contendo as colunas residuo_coletado_ton_ano, atendimento_coleta_indiferenciada_hab, residuo_coletado_ton_ano_litoral e atendimento_coleta_indiferenciada_hab_litoral
#'
#' @return um `data.frame` contendo as colunas taxa_geracao_residuos, taxa_geracao_residuos_litoral
#' @export
taxa_geracao_residuos <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    taxa_geracao_residuos = ifelse(
      tem_pesagem == "Sim" & atendimento_coleta_indiferenciada_hab > 0,
      residuo_coletado_ton_ano / atendimento_coleta_indiferenciada_hab, NA
    ),
    taxa_geracao_residuos_litoral = ifelse(tem_pesagem == "Sim" & atendimento_coleta_indiferenciada_hab_litoral > 0,
      residuo_coletado_ton_ano_litoral / atendimento_coleta_indiferenciada_hab_litoral, NA
    ),
  )
}

#' Ignora quantidade de residuo litorâneo
#'
#' @param tabela um `data.frame` contendo as colunas residuo_coletado_ton_ano, atendimento_coleta_indiferenciada_hab e litoral
#'
#' @return um `data.frame` onde os municipios litoraneos não tem dados de quantidade de residuos
#' @export
#'
#' @examples
#' \dontrun{
#' tbl <- ignora_residuo_litoraneo(tbl)
#' }
ignora_residuo_litoraneo <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    residuo_coletado_ton_ano = ifelse(litoral == "Não", residuo_coletado_ton_ano, NA),
    atendimento_coleta_indiferenciada_hab = ifelse(litoral == "Não", atendimento_coleta_indiferenciada_hab, NA),
  )
}

#' Ignora quantidade de residuo em municipio sem pesagem
#'
#' @param tabela um `data.frame` contendo as colunas residuo_coletado_ton_ano, atendimento_coleta_indiferenciada_hab e tem_pesagem
#'
#' @return um `data.frame` onde os municipios litoraneos não tem dados de quantidade de residuos
#' @export
#'
#' @examples
#' \dontrun{
#' tbl <- ignora_residuo_litoraneo(tbl)
#' }
ignora_residuo_sem_pesagem <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    residuo_coletado_ton_ano = ifelse(tem_pesagem == "Sim", residuo_coletado_ton_ano, NA),
    atendimento_coleta_indiferenciada_hab = ifelse(tem_pesagem == "Sim", atendimento_coleta_indiferenciada_hab, NA),
  )
}

#' Ignora quantidade de residuo litorâneo
#'
#' @param tabela um `data.frame` contendo as colunas residuo_coletado_ton_ano, atendimento_coleta_indiferenciada_hab e litoral
#'
#' @return um `data.frame` onde os municipios litoraneos não tem dados de quantidade de residuos
#' @export
#'
#' @examples
#' \dontrun{
#' tbl <- ignora_residuo_litoraneo(tbl)
#' }
ignora_residuo_litoraneo <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    residuo_coletado_ton_ano = ifelse(litoral == "Não", residuo_coletado_ton_ano, NA),
    atendimento_coleta_indiferenciada_hab = ifelse(litoral == "Não", atendimento_coleta_indiferenciada_hab, NA),
  )
}

#' Divide quantidade de residuo litorâneo e nao litoraneo
#'
#' @param tabela um `data.frame` contendo as colunas residuo_coletado_ton_ano, atendimento_coleta_indiferenciada_hab e litoral
#'
#' @return um `data.frame` onde os municipios litoraneos não tem dados de quantidade de residuos
#' @export
#'
#' @examples
#' \dontrun{
#' tbl <- divide_residuo_litoraneo(tbl)
#' }
divide_residuo_litoraneo <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    residuo_coletado_ton_ano_litoral = ifelse(litoral == "Sim", residuo_coletado_ton_ano, NA),
    atendimento_coleta_indiferenciada_hab_litoral = ifelse(litoral == "Sim", atendimento_coleta_indiferenciada_hab, NA),
    residuo_coletado_ton_ano = ifelse(litoral == "Não", residuo_coletado_ton_ano, NA),
    atendimento_coleta_indiferenciada_hab = ifelse(litoral == "Não", atendimento_coleta_indiferenciada_hab, NA),
  )
}

#' Tabela de preco de projetos de residuos
#'
#' Converte os dados da interface gráfica para formato de tabela.
#'
#' @param input estrutura de dados (`reactive`) que guarda os parâmetros da interface gráfica
#' @param name um `character` com o nome do projeto
#'
#' @return um `data.frame` com a tabela de preços
#' @export
tabela_preco_unidade_residuos <- function(input, name) {
  faixa <- seq.int(1, 7)
  precos <- c()
  for (i in 1:7) {
    id <- sprintf("%s_faixa%s", name, i)
    precos <- c(precos, input[[id]])
  }
  output <- dplyr::tibble(faixa, precos)
  colnames(output)[colnames(output) == "precos"] <-
    sprintf("preco_unidade_%s", name)
  return(output)
}

#' Preço unidade de resíduos
#'
#' Salva preços por faixa populacional no formato da interface gráfica.
#'
#' @param input estrutura de dados (`reactive`) que guarda os parâmetros da interface gráfica
#' @param name um `character` com o nome do projeto
#' @param precos uma `list` com os preços por faixa
#'
#' @return os parâmetros da interface gráfica com os preços corrigidos
#' @export
adiciona_preco_unidade_residuos <- function(input, name, precos) {
  faixa <- seq.int(1, 7)
  for (i in faixa) {
    id <- sprintf("%s_faixa%s", name, i)
    input[[id]] <- precos[i]
  }
  return(input)
}

#' Regionaliza 100% da demanda
#'
#' @param tabela é um `data.frame` contendo as colunas `demanda_triagem`, `estado` e `faixa` populacional
#' @param campo é um `character` com o nome da coluna de demanda
#'
#' @return um `data.frame` onde a `demanda_triagem esta regionalizada`
#' @export
#'
#' @examples
#' \dontrun{
#' tabela <- regionalizacao_triagem(tabela, potencial)
#' }
regionaliza100 <- function(tabela, campo) {
  tabela <- dplyr::group_by(tabela, estado)
  regionaliza <- function(group, group_name) {
    group <- dplyr::arrange(group, faixa)
    group[[campo]][6] <- sum(group[[campo]][5:6], na.rm = TRUE)
    group[[campo]][5] <- sum(group[[campo]][1:4], na.rm = TRUE)
    group[[campo]][1:4] <- 0
    return(group)
  }
  tabela <- dplyr::group_modify(tabela, regionaliza)
  tabela <- dplyr::ungroup(tabela)
  return(tabela)
}

#' Regionaliza faixa populacional 1
#'
#' @param tabela é um `data.frame` contendo as colunas `demanda_triagem`, `regiao`, `estado` e `faixa` populacional
#' @param potencial é um `data.frame`contendo as colunas `regiao` e `potencial_regionalização`
#' @param campo_demanda é um `character` com o nome da coluna de demanda
#' @param campo_potencial é um `character` com o nome da coluna do potencial
#'
#' @return um `data.frame` onde a coluna de demanda esta regionalizada
#' @export
#'
#' @examples
#' \dontrun{
#' tabela <- regionaliza_faixa1(tabela, potencial)
#' }
regionaliza_faixa1 <- function(tabela, potencial, campo_demanda, campo_potencial) {
  potencial <- dplyr::select(potencial, all_of(c("regiao", campo_potencial)))
  tabela <- dplyr::left_join(tabela, potencial, by = "regiao")
  tabela <- dplyr::mutate(
    tabela,
    sem_potencial = 1.0 - .data[[campo_potencial]],
  )
  tabela <- dplyr::group_by(tabela, estado)
  regionaliza <- function(group, group_name) {
    group <- dplyr::arrange(group, faixa)
    group[[campo_demanda]][2] <- group[[campo_demanda]][1] *
      group[[campo_potencial]][1] + group[[campo_demanda]][2]
    group[[campo_demanda]][1] <- group[[campo_demanda]][1] * group$sem_potencial[1]
    return(group)
  }
  tabela <- dplyr::group_modify(tabela, regionaliza)
  tabela <- dplyr::ungroup(tabela)
  tabela <- dplyr::select(tabela, -all_of(c(campo_potencial, "sem_potencial")))
  return(tabela)
}

#' Regionaliza faixa populacional 2 e 6
#'
#' @param tabela é um `data.frame` contendo as colunas `demanda_triagem`, `regiao`, `estado` e `faixa` populacional
#' @param potencial é um `data.frame`contendo as colunas `regiao` e `potencial_regionalização`
#' @param campo_demanda é um `character` com o nome da coluna de demanda
#' @param campo_potencial é um `character` com o nome da coluna do potencial
#'
#' @return um `data.frame` onde a coluna de demanda esta regionalizada
#' @export
#'
#' @examples
#' \dontrun{
#' tabela <- regionaliza_faixa2e6(tabela, potencial)
#' }
regionaliza_faixa2e6 <- function(tabela, potencial, campo_demanda, campo_potencial) {
  potencial <- dplyr::select(potencial, all_of(c("regiao", campo_potencial)))
  tabela <- dplyr::left_join(tabela, potencial, by = "regiao")
  tabela <- dplyr::mutate(
    tabela,
    sem_potencial = 1.0 - .data[[campo_potencial]],
  )
  tabela <- dplyr::group_by(tabela, estado)
  regionaliza <- function(group, group_name) {
    group <- dplyr::arrange(group, faixa)
    group[[campo_demanda]][6] <- group[[campo_demanda]][5] *
      group[[campo_potencial]][5] + group[[campo_demanda]][6]
    group[[campo_demanda]][5] <- group[[campo_demanda]][5] * group$sem_potencial[5]
    group[[campo_demanda]][2] <- group[[campo_demanda]][1] *
      group[[campo_potencial]][1] + group[[campo_demanda]][2]
    group[[campo_demanda]][1] <- group[[campo_demanda]][1] * group$sem_potencial[1]
    return(group)
  }
  tabela <- dplyr::group_modify(tabela, regionaliza)
  tabela <- dplyr::ungroup(tabela)
  tabela <- dplyr::select(tabela, -all_of(c(campo_potencial, "sem_potencial")))
  return(tabela)
}

#' Regionaliza faixa populacional 5 e 6
#'
#' @param tabela é um `data.frame` contendo as colunas `demanda_triagem`, `regiao`, `estado` e `faixa` populacional
#' @param potencial é um `data.frame`contendo as colunas `regiao` e `potencial_regionalização`
#' @param campo é um `character` com o nome da coluna de demanda
#'
#' @return um `data.frame` onde a coluna de demanda esta regionalizada
#' @export
#'
#' @examples
#' \dontrun{
#' tabela <- regionaliza_faixa5e6(tabela, potencial)
#' }
regionaliza_faixa5e6 <- function(tabela, potencial, campo_demanda, campo_potencial) {
  potencial <- dplyr::select(potencial, all_of(c("regiao", campo_potencial)))
  tabela <- dplyr::left_join(tabela, potencial, by = "regiao")
  tabela <- dplyr::mutate(
    tabela,
    sem_potencial = 1.0 - .data[[campo_potencial]],
  )
  tabela <- dplyr::group_by(tabela, estado)
  regionaliza <- function(group, group_name) {
    group <- dplyr::arrange(group, faixa)
    group[[campo_demanda]][6] <- group[[campo_potencial]][5] *
      group[[campo_demanda]][5] + group[[campo_demanda]][6]
    group[[campo_demanda]][5] <- group[[campo_potencial]][5] *
      sum(group[[campo_demanda]][1:4], na.rm = TRUE) + group$sem_potencial[5] * group[[campo_demanda]][5]
    for (i in seq(4, 1)) {
      group[[campo_demanda]][i] <- group[[campo_demanda]][i] * group$sem_potencial[i]
    }
    return(group)
  }
  tabela <- dplyr::group_modify(tabela, regionaliza)
  tabela <- dplyr::ungroup(tabela)
  tabela <- dplyr::select(tabela, -all_of(c(campo_potencial, "sem_potencial")))
  return(tabela)
}

#' Demanda por transbordo
#'
#' A demanda por transbordo é considerada 1 estacao de transbordo e 1 caminhao por municipio
#'
#' @param tabela um `data.frame` contendo numero_municipio
#'
#' @return um `data.frame` contendo a coluna `demanda_tranbordo`
#' @export
demanda_transbordo <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    demanda_transbordo = numero_municipios
  )

  return(tabela)
}

#' Regionaliza demanda por transbordo
#'
#' Regionaliza a demanda dependendo do cenario de regionalização
#'
#' @param tabela um `data.frame` contendo numero_municipio
#' @param cenario um `character` contendo o nome do cenario ("A" "B" ou "C")
#' @return um `data.frame` contendo a coluna `demanda_tranbordo`
#' @export
regionaliza_transbordo <- function(tabela, cenario) {
  if (cenario == "C") {
    potencial_regionalizacao <- carrega_dado_auxiliar(
      "residuos_potencial_regionalizacao"
    )
    pesos <- dplyr::select(
      potencial_regionalizacao,
      all_of(c("regiao", "potencial_transbordo_c"))
    )
    tabela <- dplyr::left_join(tabela, pesos, by = "regiao")
    tabela <- dplyr::rename(tabela, peso = potencial_transbordo_c)
  } else if (cenario == "B") {
    potencial_regionalizacao <- carrega_dado_auxiliar(
      "residuos_potencial_regionalizacao"
    )
    pesos <- dplyr::select(
      potencial_regionalizacao,
      all_of(c("regiao", "potencial_transbordo_b"))
    )
    tabela <- dplyr::left_join(tabela, pesos, by = "regiao")
    tabela <- dplyr::rename(tabela, peso = potencial_transbordo_b)
  } else {
    tabela <- dplyr::mutate(tabela, peso = 0.0)
  }
  tabela <- dplyr::mutate(
    tabela,
    demanda_transbordo = peso * demanda_transbordo
  )
  tabela <- dplyr::select(tabela, -peso)
  return(tabela)
}

#' Investimento em transbordo
#'
#' Estima o investimento necessários em transbordo como a multiplicação da demanda pelo custo.
#'
#' @param tabela um `data.frame` contendo a coluna `demanda_transbordo`
#' @param vida_util valor em anos da vida util do aterro
#'
#' @return um `data.frame` contendo a coluna `investimento_expansao_transbordo`
#' @export
investimento_expansao_transbordo <- function(tabela, custo) {
  tabela <- dplyr::mutate(
    tabela,
    investimento_expansao_transbordo = demanda_transbordo * custo
  )
  return(tabela)
}

#' Regionaliza demanda por triagem
#'
#' Faz a regionalização da demanda em acordo com o cenário de regionalização.
#'
#' @param tabela um `data.frame` contendo numero_municipio
#' @param cenario um `character` contendo o nome do cenario ("A" "B" ou "C")
#' @return um `data.frame` contendo a coluna `demanda_tranbordo`
#' @export
regionaliza_triagem <- function(tabela, cenario) {
  rlog::log_info(sprintf("residuos: regionalizando cenario %s", cenario))
  if (cenario == "C") {
    tabela <- regionaliza100(tabela, campo = "demanda_triagem")
    return(tabela)
  } else if (cenario == "B") {
    potencial <- carrega_dado_auxiliar(
      "residuos_potencial_regionalizacao"
    )
    tabela <- regionaliza_faixa1(
      tabela,
      potencial,
      campo_demanda = "demanda_triagem",
      campo_potencial = "potencial_triagem"
    )
    return(tabela)
  } else {
    return(tabela)
  }
}

#' Regionaliza demanda por compostagem
#'
#' Faz a regionalização da demanda em acordo com o cenário de regionalização.
#'
#' @param tabela um `data.frame` contendo numero_municipio
#' @param cenario um `character` contendo o nome do cenario ("A" "B" ou "C")
#' @return um `data.frame` contendo a coluna `demanda_tranbordo`
#' @export
regionaliza_compostagem <- function(tabela, cenario) {
  rlog::log_info(sprintf("residuos: compostagem regionalizando cenario %s", cenario))
  if (cenario == "C") {
    tabela <- regionaliza100(tabela, campo = "demanda_compostagem")
    return(tabela)
  } else if (cenario == "B") {
    potencial <- carrega_dado_auxiliar(
      "residuos_potencial_regionalizacao"
    )
    tabela <- regionaliza_faixa2e6(
      tabela,
      potencial,
      campo_demanda = "demanda_compostagem",
      campo_potencial = "potencial_compostagem"
    )
    return(tabela)
  } else {
    return(tabela)
  }
}

#' Regionaliza demanda por compostagem
#'
#' Faz a regionalização da demanda em acordo com o cenário de regionalização.
#'
#' @param tabela um `data.frame` contendo numero_municipio
#' @param cenario um `character` contendo o nome do cenario ("A" "B" ou "C")
#' @return um `data.frame` contendo a coluna `demanda_tranbordo`
#' @export
regionaliza_compostagem <- function(tabela, cenario) {
  rlog::log_info(sprintf("residuos: regionalizando cenario %s", cenario))
  if (cenario == "C") {
    tabela <- regionaliza100(tabela, campo = "demanda_compostagem")
    return(tabela)
  } else if (cenario == "B") {
    potencial <- carrega_dado_auxiliar(
      "residuos_potencial_regionalizacao"
    )
    tabela <- regionaliza_faixa2e6(
      tabela,
      potencial,
      campo_demanda = "demanda_compostagem",
      campo_potencial = "potencial_compostagem"
    )
    return(tabela)
  } else {
    return(tabela)
  }
}

#' Regionaliza demanda por aterro
#'
#' Faz a regionalização da demanda em acordo com o cenário de regionalização.
#'
#' @param tabela um `data.frame` contendo numero_municipio
#' @param cenario um `character` contendo o nome do cenario ("A" "B" ou "C")
#' @return um `data.frame` contendo a coluna `demanda_tranbordo`
#' @export
regionaliza_aterro <- function(tabela, cenario) {
  rlog::log_info(sprintf("residuos: regionalizando cenario %s", cenario))
  if (cenario == "C") {
    tabela <- regionaliza100(tabela, campo = "demanda_aterro")
    return(tabela)
  } else if (cenario == "B") {
    potencial <- carrega_dado_auxiliar(
      "residuos_potencial_regionalizacao"
    )
    tabela <- regionaliza_faixa2e6(
      tabela,
      potencial,
      campo_demanda = "demanda_aterro",
      campo_potencial = "potencial_aterro"
    )
    return(tabela)
  } else {
    return(tabela)
  }
}


#' Preenche taxa de geração de resíduos
#'
#' O campo de quantidade gerada de residuos (residuo_coletado_ton_ano) é prenchido pela taxa média por regiao e estado.
#'
#' @param tabela um `data.frame` com as colunas a serem preenchidas
#'
#' @return o mesmo `data.frame` com a coluna preenchida (residuo_coletado_ton_ano)
#' @export
#'
#' @examples
#' \dontrun{
#' tabela <- preenche_por_regiao_faixa_populacional(tabela, densidade)
#' }
preenche_taxa_geracao_residuos <- function(tabela) {
  regiao_faixa <- divide_residuo_litoraneo(tabela)
  regiao_faixa <- taxa_geracao_residuos(regiao_faixa)
  regiao_faixa <- dplyr:::filter(regiao_faixa, tem_pesagem == "Sim")
  regiao_faixa <- soma_por_estado_faixa(
    regiao_faixa,
    campo_estado = "regiao"
  )
  regiao_faixa <- dplyr::transmute(
    regiao_faixa,
    regiao = regiao,
    faixa = faixa,
    taxa_geracao_residuos_rf = ifelse(
      atendimento_coleta_indiferenciada_hab > 0, residuo_coletado_ton_ano / atendimento_coleta_indiferenciada_hab, NA
    ),
    taxa_geracao_residuos_litoral_rf = ifelse(
      atendimento_coleta_indiferenciada_hab_litoral > 0, residuo_coletado_ton_ano_litoral / atendimento_coleta_indiferenciada_hab_litoral, NA
    ),
  )

  # preenche taxa_geracao
  tabela <- dplyr::left_join(tabela,
    regiao_faixa,
    by = c("regiao", "faixa")
  )
  tabela <- dplyr::mutate(
    tabela,
    taxa_geracao_residuos = ifelse(
      tem_pesagem == "Sim",
      ifelse(atendimento_coleta_indiferenciada_hab > 0, residuo_coletado_ton_ano / atendimento_coleta_indiferenciada_hab, NA),
      ifelse(
        litoral == "Sim",
        taxa_geracao_residuos_litoral_rf,
        taxa_geracao_residuos_rf
      )
    ),
    taxa_geracao_residuos = ifelse(
      is.na(taxa_geracao_residuos),
      ifelse(
        litoral == "Sim",
        taxa_geracao_residuos_litoral_rf,
        taxa_geracao_residuos_rf
      ),
      taxa_geracao_residuos
    )
  )
  campos_para_remover <- c(
    "taxa_geracao_residuos_rf",
    "taxa_geracao_residuos_litoral_rf"
  )
  tabela <- dplyr::select(tabela, -dplyr::all_of(campos_para_remover))
  return(tabela)
}

#' Preenche quantidade coletada de residuos
#'
#' O campo de quantidade coletada de residuos (residuo_coletado_ton_ano) é prenchido pela taxa média por regiao e estado.
#'
#' @param tabela um `data.frame` com as colunas a serem preenchidas
#'
#' @return o mesmo `data.frame` com a coluna preenchida (residuo_coletado_ton_ano)
#' @export
preenche_quantidade_coletada <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    residuo_coletado_ton_ano = ifelse(
      is.na(residuo_coletado_ton_ano) | tem_pesagem != "Sim",
      taxa_geracao_residuos * atendimento_coleta_indiferenciada_hab,
      residuo_coletado_ton_ano
    )
  )
  return(tabela)
}

#' Densidade de caminhão compactadores por estado e faixa
#'
#' Calcula a quantidade de caminhões por faixa
#' método soma: divisão da população atendida (atendimento_coleta_indiferenciada_hab) pela soma do numero de caminhoes
#' método media: média das densidades por munícipio (padrão)
#'
#' @param tabela_faixa um `data.frame` com as colunas "estado", "faixa" para adicionar
#' @param tabela_por_municipio  um `data.frame` contendo as colunas "estado", "faixa", "densidade_caminhoes_compactadores", "atendimento_coleta_indiferenciada_hab", "quantidade_caminhoes_compactadores"
#' @param metodo metodo "soma" ou "media"
#'
#' @return um `data.frame` contendo a coluna densidade_caminhoes_compactadores
#' @export
densidade_caminhao_por_estado_faixa <- function(tabela_faixa, tabela_por_municipio, metodo = "media") {
  vars <- c("estado", "faixa", "densidade_caminhoes_compactadores", "atendimento_coleta_indiferenciada_hab", "quantidade_caminhoes_compactadores")
  tabela_por_municipio <- dplyr::select(
    tabela_por_municipio, dplyr::all_of(vars)
  )
  tabela_por_municipio <- dplyr::group_by(
    tabela_por_municipio,
    estado, faixa
  )
  if (metodo == "soma") {
    tabela_por_municipio <- dplyr::summarise(
      tabela_por_municipio,
      atendimento_coleta_indiferenciada_hab = sum(atendimento_coleta_indiferenciada_hab, na.rm = TRUE),
      quantidade_caminhoes_compactadores = sum(quantidade_caminhoes_compactadores, na.rm = TRUE),
      densidade_caminhoes_compactadores = atendimento_coleta_indiferenciada_hab / quantidade_caminhoes_compactadores
    )
  } else {
    tabela_por_municipio <- dplyr::summarise(
      tabela_por_municipio,
      densidade_caminhoes_compactadores = mean(densidade_caminhoes_compactadores, na.rm = TRUE),
    )
  }
  tabela_faixa$densidade_caminhoes_compactadores <- NULL
  tabela_faixa <- dplyr::left_join(
    tabela_faixa,
    tabela_por_municipio,
    by = c("estado", "faixa")
  )
  return(tabela_faixa)
}

#' Reaproveitamento de resíduos relativo
#'
#' Calcula o reaproveitamento de resíduos recuperados pela divisão da quantidade total recuperada (residuo_recuperado_ton_ano)
#' divida pela quandidade total coletada.
#'
#' @param tabela_faixa um `data.frame` com as colunas "estado", "faixa" para adicionar
#' @param tabela_por_municipio  um `data.frame` contendo as colunas "estado", "faixa", "tem_coleta_seletiva", "residuo_recuperado_ton_ano", "residuo_coletado_ton_ano"
#'
#' @return um `data.frame` contendo a coluna reaproveitamento_relativo
#' @export
reaproveitamento_relativo <- function(tabela_faixa, tabela_por_municipio) {
  vars <- c("estado", "faixa", "tem_coleta_seletiva", "residuo_recuperado_ton_ano", "residuo_coletado_ton_ano")
  tabela_por_municipio <- dplyr::select(
    tabela_por_municipio, dplyr::all_of(vars)
  )
  # tabela_por_municipio <- dplyr::filter(tabela_por_municipio, tem_coleta_seletiva == "Sim")
  tabela_por_municipio <- dplyr::group_by(
    tabela_por_municipio,
    estado,
    faixa
  )
  tabela_por_municipio <- dplyr::summarise(
    tabela_por_municipio,
    recuperado = sum(residuo_recuperado_ton_ano, na.rm = TRUE),
    residuo_coletado_ton_ano = sum(residuo_coletado_ton_ano, na.rm = TRUE)
  )

  tabela_por_municipio <- dplyr::transmute(
    tabela_por_municipio,
    estado = estado,
    faixa = faixa,
    reaproveitamento_relativo = ifelse(residuo_coletado_ton_ano > 0, recuperado / residuo_coletado_ton_ano, 0)
  )
  tabela_faixa <- dplyr::left_join(
    tabela_faixa,
    tabela_por_municipio,
    by = c("estado", "faixa")
  )
  return(tabela_faixa)
}

#' Projeção do reaproveitamento
#'
#' @param tabela um `data.frame` contendo as colunas "estado", "faixa", "reaproveitamento_relativo", "total_residuos_projecao"
#'
#' @return um `data.frame` contendo a coluna reaproveitamento_relativo
#' @export
#'
projecao_reaproveitamento <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    projecao_reaproveitamento = reaproveitamento_relativo * total_residuos_projecao,
    projecao_reaproveitamento = ifelse(is.na(projecao_reaproveitamento), 0.0, projecao_reaproveitamento)
  )
}
