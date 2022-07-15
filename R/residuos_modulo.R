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
#'
#' @examples
#' \dontrun{
#'
#' }
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
#' \item[resposicao_aterro]
#' \item[resposicao_compostagem]
#' \item[resposicao_triagem]
#' \item[resposicao_coleta_indiferenciada]
#' \item[resposicao_coleta_seletiva]
#' \item[investimento_aterro]
#' \item[investimento_compostagem]
#' \item[investimento_triagem]
#' \item[investimento_transbordo]
#' \item[investimento_coleta_indiferenciada]
#' \item[investimento_coleta_seletiva]
#' }
#'
#' @return um `data.frame` contendo as colunas adicionais `investimento_reposicao`, `investimento_expansao` e `investimento_total`
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
investimento_residuos_total <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    investimento_reposicao = reposicao_aterro +
      reposicao_compostagem +
      reposicao_triagem +
      reposicao_coleta_indiferenciada +
      reposicao_coleta_seletiva,
    investimento_expansao = investimento_aterro +
      investimento_compostagem +
      investimento_triagem +
      investimento_transbordo +
      investimento_coleta_indiferenciada +
      investimento_coleta_seletiva,
    investimento_total = investimento_reposicao + investimento_expansao
  )
}

# ATERRO -----------------------------------------------------------------------

#' Demanda por aterro
#'
#' @param tabela um `data.frame` contendo a coluna `total_residuos_projecao`
#' @param vida_util valor em anos da vida util do aterro
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
demanda_aterro <- function(tabela, vida_util) {
  tabela <- dplyr::mutate(
    tabela,
    demanda_aterro = total_residuos_projecao * vida_util
  )
}

#' Investimento em Aterros
#'
#' Estima o investimento necessários em aterro como a multiplicação de: total de residuos, preço unitario por massa e vida útil do aterro.
#'
#' @param tabela um `data.frame` contendo as colunas `demanda_aterro` e `preco_unidade_aterro`
#'
#' @return um `data.frame` contendo a coluna `investimento_aterro`
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
investimento_aterro <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    investimento_aterro = demanda_aterro * preco_unidade_aterro
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
#' @param tabela
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
demanda_triagem <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    demanda_triagem = meta_reaproveitamento / 100.0 * total_residuos_projecao - pmax(CS026, 0.0),
  )
}

#' Investimento em triagem
#'
#' @param tabela
#' @param vida_util
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
investimento_triagem <- function(tabela, vida_util) {
  tabela <- dplyr::mutate(
    tabela,
    investimento_triagem = demanda_triagem * preco_unidade_triagem * vida_util
  )
}

#' Capacidade instalada de triagem
#'
#' @param tabela
#' @param vida_util
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
capacidade_instalada_triagem <- function(tabela, vida_util) {
  tabela <- dplyr::mutate(
    tabela,
    capacidade_instalada_triagem = meta_reaproveitamento / 100.0 * total_residuos * preco_unidade_triagem * vida_util
  )
}

# COMPOSTAGEM ------------------------------------------------------------------

#' Demanda por compostagem
#'
#' @param tabela
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
demanda_compostagem <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    demanda_compostagem = meta_compostagem / 100.0 * total_residuos_projecao - pmax(quantidade_compostagem, 0.0),
  )
}

#' Investimento em compostagem
#'
#' @param tabela
#' @param vida_util
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
investimento_compostagem <- function(tabela, vida_util) {
  tabela <- dplyr::mutate(
    tabela,
    investimento_compostagem = demanda_compostagem * preco_unidade_compostagem * vida_util
  )
}

#' Capacidade instalada de compostagem
#'
#' @param tabela
#' @param vida_util
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
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
#'
#' @examples
#' \dontrun{
#'
#' }
densidade_caminhoes_bau <- function(tabela) {
  # TODO: avaliar como foi preenchido na planilha de investimento
  # solucao usar tabela fixa?
  tabela <- dplyr::mutate(
    tabela,
    densidade_caminhoes_bau = ifelse(
      numero_caminhoes_bau > 0,
      CS050 / numero_caminhoes_bau,
      NA
    )
  )
}

#' Title
#'
#' @param tabela
#' @param tabela_densidade
#' @param campo_densidade
#' @param campo_faixa
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
preenche_caminhoes_bau <- function(tabela,
                                   tabela_densidade,
                                   campo_densidade = "densidade",
                                   campo_faixa = "faixa") {
  tabela_densidade <- dplyr::select(
    tabela_densidade,
    dplyr::all_of(
      c(
        campo_densidade,
        campo_faixa
      )
    )
  )
  tabela <- dplyr::select(tabela, -dplyr::all_of(campo_densidade))
  tabela <- dplyr::left_join(tabela, tabela_densidade, by = campo_faixa)
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

#' Title
#'
#' @param tabela
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
atendimento_relativo_coleta_seletiva <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    atendimento_relativo_seletiva_urbano = CS050 / POP_URB,
  )
}

#' Title
#'
#' @param tabela
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
deficit_coleta_seletiva <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    deficit_coleta_seletiva = pmax(populacao_urbana * (1.0 - atendimento_relativo_seletiva_urbano), 0)
  )
}

#' Title
#'
#' @param tabela
#' @param valor
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
investimento_coleta_seletiva <- function(tabela, valor) {
  tabela <- dplyr::mutate(
    tabela,
    investimento_coleta_seletiva = deficit_coleta_seletiva / densidade_caminhoes_bau * valor
  )
}

#' Title
#'
#' @param tabela
#' @param valor
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
capacidade_instalada_coleta_seletiva <- function(tabela, valor) {
  tabela <- dplyr::mutate(
    tabela,
    capacidade_instalada_coleta_seletiva = POP_URB * (1.0 - atendimento_relativo_seletiva_urbano) / densidade_caminhoes_bau * valor
  )
}

# COLETA REGULAR -----------------------------------------------------------------

#' Title
#'
#' @param tabela
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
numero_caminhoes <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    numero_caminhoes = CO054 + CO055 + CO056 + CO057 + CO058 + CO059
  )
}

#' Title
#'
#' @param tabela
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
deficit_coleta_indiferenciada <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    deficit_coleta_indiferenciada = pmax(meta_coleta / 100.0 * populacao_total - CO164, 0),
  )
}

#' Title
#'
#' @param tabela
#' @param valor
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
investimento_coleta_indiferenciada <- function(tabela, valor) {
  tabela <- dplyr::mutate(
    tabela,
    investimento_coleta_indiferenciada = deficit_coleta_indiferenciada / densidade_caminhoes * valor
  )
}

#' Title
#'
#' @param tabela
#' @param valor
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
capacidade_instalada_coleta_indiferenciada <- function(tabela, valor) {
  tabela <- dplyr::mutate(
    tabela,
    capacidade_instalada_coleta_indiferenciada = CO164 / densidade_caminhoes * valor
  )
}


#' Title
#'
#' @param tabela
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
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

#' Title
#'
#' @param tabela
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
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
#' @param tabela
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
meta_plansab_residuo <- function(tabela) {
  data("plansab", package = "rsan")
  tabela <- dplyr::left_join(
    tabela,
    plansab,
    by = "regiao"
  )
}

#' Title
#'
#' @param tabela
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
atendimento_relativo_residuos <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    atendimento_relativo_total = CO164 / POP_TOT,
    atendimento_relativo_urbano = CO050 / POP_URB,
    atendimento_relativo_rural = (CO164 - CO050) / (POP_TOT - POP_URB)
  )
}

#' Title
#'
#' @param tabela
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
geracao_residuos <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    taxa_geracao_residuos = CO119 / CO164,
    total_residuos = taxa_geracao_residuos * POP_TOT,
    total_residuos_projecao = taxa_geracao_residuos * populacao_total,
    percentual_recuperado = CS009 / CO119
  )
}

#' Title
#'
#' @param input
#' @param name
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
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

#' Title
#'
#' @param input
#' @param name
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
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
    group[[campo]][6] <- sum(group[[campo]][5:6])
    group[[campo]][5] <- sum(group[[campo]][1:4])
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
  print(names(tabela))
  print(campo_potencial)
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
      sum(group[[campo_demanda]][1:4]) + group$sem_potencial[5] * group[[campo_demanda]][5]
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
#'
#' @examples
#' \dontrun{
#'
#' }
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
#'
#' @examples
#' \dontrun{
#'
#' }
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
#' @return um `data.frame` contendo a coluna `investimento_transbordo`
#' @export
#'
#' @examples
#' \dontrun{
#' tabela <- investimento_transbordo(tabela, 1000)
#' }
investimento_transbordo <- function(tabela, custo) {
  tabela <- dplyr::mutate(
    tabela,
    investimento_transbordo = demanda_transbordo * custo
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
#'
#' @examples
#' \dontrun{
#'
#' }
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
#'
#' @examples
#' \dontrun{
#'
#' }
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
#'
#' @examples
#' \dontrun{
#'
#' }
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
#'
#' @examples
#' \dontrun{
#'
#' }
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
