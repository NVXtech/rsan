#' Necessidade de Investimento - Componente Resíduos - Situação Urbana

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
#' \dontrun{
#' tabela <- quantidade_compostagem_municipio(tabela)
#' }
quantidade_compostagem_municipio <- function(tabela) {
  tabela <- dplyr::filter(
    tabela,
    grepl("compostagem", .data[["UP003"]], fixed = TRUE)
  )
  tabela <- rsan:::codigo6_para_codigo_ibge(tabela, "Código")
  tabela <- dplyr::group_by(tabela, codigo_municipio)
  tabela <-
    dplyr::summarise(tabela, quantidade_compostagem = sum(UP080))
  return(tabela)
}

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
#'  \item{etapa}
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
  tabela <- rsan:::somar_por_campo(tabela, "estado")
  tabela <- tidyr::pivot_longer(
    tabela,
    cols = colunas[3:length(colunas)],
    names_to = c("destino", "etapa"),
    names_pattern = "investimento_(.*?)_(.*)",
    values_to = "necessidade_investimento"
  )
  tabela <- dplyr::mutate(
    tabela,
    componente = "residuos",
    situacao = "urbana",
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
    capacidade_instalada_aterro =  total_residuos * preco_unidade_aterro * vida_util
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
    demanda_triagem = meta_reaproveitamento / 100.0 * total_residuos_projecao - pmax(CS026, 0.0),
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
#' @param tabela um `data.frame` contendo as colunas `meta_compostagem`, `total_residuos_projecao` e `quantidade_compostagem`.
#'
#' @return um `data.frame` com a coluna `demanda_compostagem`
#' @export
demanda_compostagem <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    demanda_compostagem = meta_compostagem / 100.0 * total_residuos_projecao - pmax(quantidade_compostagem, 0.0),
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

#' Densidade de caminhões bau
#'
#' @param tabela um `data.frame` contendo as colunas `numero_caminhoes_bau` e `CS050` (SNIS)
#'
#' @return tabela com o campo adicional densidade de caminhoes baú (`densidade_caminhoes_bau`)
#' @export
densidade_caminhoes_bau <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    densidade_caminhoes_bau = ifelse(
      numero_caminhoes_bau > 0,
      CS050 / numero_caminhoes_bau,
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
#' A análise é baseada no campo CS001 do SNIS.
#' @param tabela um `data.frame` contendo a coluna CS001.
#'
#' @return vetor de boleanos para cada municipio que não contém coleta seletiva
#' @export
#'
#' @examples
#' tabela <- dplyr::tibble(CS001 = c("Sim", "Não"))
#' mask <- mascara_coleta_seletiva(tabela)
mascara_coleta_seletiva <- function(tabela) {
  mask <- tabela$CS001 == "Sim"
  mask[is.na(mask)] <- FALSE
  return(mask)
}

#' Atendimento relativo para coleta seletiva
#'
#' @param tabela um `data.frame` com as colunas `CS050` e `POP_URB`
#'
#' @return um `data.frame` contendo a coluna atendimento_relativo_coleta_seletiva
#' @export
atendimento_relativo_coleta_seletiva <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    atendimento_relativo_seletiva_urbano = ifelse(
      POP_URB > 0, CS050 / POP_URB, NA
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
#' @param valor um `double` contendo o preço de um caminhão bau
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
#' @param tabela um `data.frame` contendo as colunas `POP_URB`, `atendimento_relativo_seletiva_urbano` e `densidade_caminhoes_bau `
#' @param valor valor um `double` contendo o preço de um caminhão bau
#'
#' @return um `data.frame` contendo a coluna capacidade_instalada_coleta_seletiva
#' @export
capacidade_instalada_coleta_seletiva <- function(tabela, valor) {
  tabela <- dplyr::mutate(
    tabela,
    capacidade_instalada_coleta_seletiva = ifelse(
      densidade_caminhoes_bau > 0,
      POP_URB * (1.0 - atendimento_relativo_seletiva_urbano) /
        densidade_caminhoes_bau * valor,
      NA
    )
  )
}

# COLETA REGULAR -----------------------------------------------------------------

#' Quantidade de caminhões compactador
#'
#' Calcula o número de caminhões compactadores utilizando dados do SNIS.
#' Soma-se as colunas CO054, CO055, CO056, CO057, CO058 e CO059.
#'
#' @param tabela um `data.frame` contendo as colunas CO054, CO055, CO056, CO057, CO058 e CO059.
#'
#' @return um `data.frame` contendo a coluna `numero_caminhoes`
#' @export
#'
#' @examples
#' \dontrun{
#' tabela <- numero_caminhoes(tabela)
#' }
numero_caminhoes <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    numero_caminhoes = CO054 + CO055 + CO056 + CO057 + CO058 + CO059
  )
}

#' Déficit para coleta indiferenciada
#'
#' O déficit é a meta em % multiplicada pela populacao total estimada menos o atendimento atual.
#'
#' @param tabela é um `data.frame` contendo as colunas `meta_coleta`, `populacao_total` e `CO164`
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
    deficit_coleta_indiferenciada = pmax(meta_coleta / 100.0 * populacao_total - CO164, 0),
  )
}

#' Necessidade de investimento em coleta indiferenciada
#'
#' @param tabela um `data.frame` contendo as colunas `deficit_coleta_indiferenciada` e `densidade_caminhoes`
#' @param valor um `double` contendo o preço de um caminhão
#'
#' @return um `data.frame` contendo a coluna `investimento_expansao_coleta_indiferenciada`
#' @export
investimento_expansao_coleta_indiferenciada <- function(tabela, valor) {
  tabela <- dplyr::mutate(
    tabela,
    investimento_expansao_coleta_indiferenciada = ifelse(
      densidade_caminhoes > 0,
      deficit_coleta_indiferenciada / densidade_caminhoes * valor,
      NA
    )
  )
}

#' Capacidade instalada coleta indiferenciada
#'
#' @param tabela um `data.frame` contendo as colunas `POP_URB`, `atendimento_relativo_seletiva_urbano` e `densidade_caminhoes `
#' @param valor valor um `double` contendo o preço de um caminhão
#'
#' @return um `data.frame` contendo a coluna capacidade_instalada_coleta_indiferenciada
#' @export
capacidade_instalada_coleta_indiferenciada <- function(tabela, valor) {
  tabela <- dplyr::mutate(
    tabela,
    capacidade_instalada_coleta_indiferenciada = ifelse(
      densidade_caminhoes > 0,
      CO164 / densidade_caminhoes * valor,
      NA
    )
  )
}


#' Número de caminhões baú
#'
#' Cálcula o número de caminhões baú
#'
#' @param tabela um `data.frame` contendo as colunas CO063, CO064, CO065, CO066, CO067 e CO068
#'
#' @return um `data.frame` contendo a coluna `numero_caminhoes_bau`
#' @export
numero_caminhoes_bau <- function(tabela) {
  tabela$mask <- mascara_coleta_seletiva(tabela)
  tabela <- dplyr::mutate(
    tabela,
    numero_caminhoes_bau = ifelse(mask, CO063 + CO064 + CO065 + CO066 + CO067 + CO068, NA)
  )
  tabela <- dplyr::select(
    tabela,
    -mask
  )
}

#' Densidade de caminhões
#'
#' Cálcula a densidade de caminhões
#'
#' @param tabela um `data.frame` contendo as colunas CO164 e numero_caminhoes
#'
#' @return um `data.frame` contendo a coluna `densidade_caminhoes`
#' @export
densidade_caminhoes <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    densidade_caminhoes = ifelse(
      numero_caminhoes > 0,
      CO164 / numero_caminhoes,
      NA
    )
  )
}

#' Adiciona metas de atendimento por região
#'
#' @param tabela um `data.frame` com a coluna `regiao`
#'
#' @return um `data.frame` com as colunas adicionais `meta_coleta`, `meta_compostagem`e `meta_reaproveitamento`
#' @export
meta_plansab_residuo <- function(tabela) {
  data("plansab", package = "rsan")
  plansab <- get("plansab")
  vars <- c("regiao", "meta_coleta", "meta_compostagem", "meta_reaproveitamento")
  plansab <- dplyr::select(plansab, all_of(vars))
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
  data("tipo_disposicao", package = "rsan")
  tipo_disposicao <- get("tipo_disposicao")
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
#' @param tabela um `data.frame` com a coluna `CO119`
#'
#' @return um `data.frame` com a colunas adicional `residuos_disposicao_inadequada`
#' @export
disposicao_inadequada <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    disposicao_inadequada = ifelse(tipo_disposicao == "Inadequada", 1.0, 0.0),
    residuos_disposicao_inadequada = CO119 * disposicao_inadequada
  )
}

#' Atendimento relativo para residuo
#'
#' Calcula o atendimento relativo total, urbano e rural.
#'
#' @param tabela um `data.frame` com as colunas `CO164`, `CO050`, `POP_TOT` e `POP_URB`.
#'
#' @return um `data.frame` contendo as coliunas `atendimento_relativo_total`, `atendimento_relativo_urbano` e `atendimento_relativo_rural`
#' @export
atendimento_relativo_residuos <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    atendimento_relativo_total = CO164 / POP_TOT,
    atendimento_relativo_urbano = CO050 / POP_URB,
    atendimento_relativo_rural = ifelse(
      (POP_TOT - POP_URB) > 0,
      (CO164 - CO050) / (POP_TOT - POP_URB),
      NA
    )
  )
}

#' Geração de resíduos
#'
#' @param tabela um `data.frame` contendo as colunas POP_TOT, CO119, CO164, CS009 e populacao_total
#'
#' @return um `data.frame` contendo as colunas taxa_geracao_residuos, total_residuos, total_residuos_projecaoe e percentual_recuperado
#' @export
geracao_residuos <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    taxa_geracao_residuos = ifelse(
      CO164 > 0,
      CO119 / CO164,
      NA
    ),
    total_residuos = taxa_geracao_residuos * POP_TOT,
    total_residuos_projecao = taxa_geracao_residuos * populacao_total,
    percentual_recuperado = ifelse(
      CO119 > 0,
      CS009 / CO119,
      NA
    )
  )
}

#' Taxa de geração de resíduos (t/hab)
#'
#' @param tabela um `data.frame` contendo as colunas CO119, CO164, CO119_litoral e CO164_litoral
#'
#' @return um `data.frame` contendo as colunas taxa_geracao_residuos, taxa_geracao_residuos_litoral
#' @export
taxa_geracao_residuos <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    taxa_geracao_residuos = CO119 / CO164,
    taxa_geracao_residuos_litoral = CO119_litoral / CO164_litoral,
  )
}

#' Ignora quantidade de residuo litorâneo
#'
#' @param tabela um `data.frame` contendo as colunas CO119, CO164 e litoral
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
    CO119 = ifelse(litoral == "Não", CO119, NA),
    CO164 = ifelse(litoral == "Não", CO164, NA),
  )
}

#' Ignora quantidade de residuo em municipio sem pesagem
#'
#' @param tabela um `data.frame` contendo as colunas CO119, CO164 e CO021
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
    CO119 = ifelse(CO021 == "Sim", CO119, NA),
    CO164 = ifelse(CO021 == "Sim", CO164, NA),
  )
}

#' Ignora quantidade de residuo litorâneo
#'
#' @param tabela um `data.frame` contendo as colunas CO119, CO164 e litoral
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
    CO119 = ifelse(litoral == "Não", CO119, NA),
    CO164 = ifelse(litoral == "Não", CO164, NA),
  )
}

#' Divide quantidade de residuo litorâneo e nao litoraneo
#'
#' @param tabela um `data.frame` contendo as colunas CO119, CO164 e litoral
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
    CO119_litoral = ifelse(litoral == "Sim", CO119, NA),
    CO164_litoral = ifelse(litoral == "Sim", CO164, NA),
    CO119 = ifelse(litoral == "Não", CO119, NA),
    CO164 = ifelse(litoral == "Não", CO164, NA),
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
    data("potencial_regionalizacao", package = "rsan")
    pesos <- dplyr::select(
      potencial_regionalizacao,
      all_of(c("regiao", "potencial_transbordo_c"))
    )
    tabela <- dplyr::left_join(tabela, pesos, by = "regiao")
    tabela <- dplyr::rename(tabela, peso = potencial_transbordo_c)
  } else if (cenario == "B") {
    data("potencial_regionalizacao", package = "rsan")
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
    data("potencial_regionalizacao", package = "rsan")
    potencial <- get("potencial_regionalizacao")
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
  rlog::log_info(sprintf("residuos: regionalizando cenario %s", cenario))
  if (cenario == "C") {
    tabela <- regionaliza100(tabela, campo = "demanda_compostagem")
    return(tabela)
  } else if (cenario == "B") {
    data("potencial_regionalizacao", package = "rsan")
    potencial <- get("potencial_regionalizacao")
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
    data("potencial_regionalizacao", package = "rsan")
    potencial <- get("potencial_regionalizacao")
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
    data("potencial_regionalizacao", package = "rsan")
    potencial <- get("potencial_regionalizacao")
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


#' Preenche residuo gerado
#'
#' O campo de quantidade gerada de residuos (CO119) é prenchido pela taxa média por regiao e estado.
#'
#' @param tabela um `data.frame` com as colunas a serem preenchidas
#' @param tabela_faixa um `data.frame` contendo os valores que serão base para o preenchimento devem conter a coluna com os nomes fornecidos em `campo` e `campo_faixa`
#' @param preencher_campo um `character` com o nome da coluna a ser preenchida
#' @param campo_regiao um `character` com o nome da coluna região
#' @param campo_faixa um `character` com o nome da coluna faixa populacional
#'
#' @return o mesmo `data.frame` com a coluna preenchida (CO119)
#' @export
#'
#' @examples
#' \dontrun{
#' tabela <- preenche_por_regiao_faixa_populacional(tabela, densidade)
#' }
preenche_geracao_residuos <- function(tabela,
                                      tabela_faixa,
                                      preencher_campo = "CO119",
                                      campo_regiao = "regiao",
                                      campo_faixa = "faixa") {
  tabela_faixa <- dplyr::select(
    tabela_faixa,
    dplyr::all_of(c(
      "taxa_geracao_residuos",
      "taxa_geracao_residuos_litoral",
      campo_regiao,
      campo_faixa
    ))
  )

  # transforma valores negativos e nulos em NA
  mask <- tabela[[preencher_campo]] <= 0
  tabela[[preencher_campo]][mask] <- NA
  # transforma muncípios sem pesagem em NA
  mask <- tabela$CO021 == "Não"
  tabela[[preencher_campo]][mask] <- NA

  tabela <- dplyr::left_join(tabela,
    tabela_faixa,
    by = c(campo_regiao, campo_faixa)
  )

  tabela <- dplyr::mutate(
    tabela,
    estimativa_nao_litoral = taxa_geracao_residuos * POP_TOT,
    estimativa_litoral = taxa_geracao_residuos_litoral * POP_TOT,
    estimativa = ifelse(litoral == "Sim", estimativa_litoral, estimativa_nao_litoral)
  )

  mask <- is.na(tabela[[preencher_campo]])
  tabela[[preencher_campo]][mask] <- tabela$estimativa[mask]

  campos_para_remover <- c(
    "estimativa", "estimativa_litoral", "estimativa_nao_litoral",
    "taxa_geracao_residuos", "taxa_geracao_residuos_litoral"
  )

  tabela <- dplyr::select(
    tabela,
    -dplyr::all_of(campos_para_remover)
  )
  return(tabela)
}
