#' Necessidade de Investimento - Componente Água - Situação Rural

#' Fração de investimentos em água coletivos e individuais
#'
#' Define a fração coletiva e individual para necessidade de investimento em água.
#'
#' @param tabela um `data.frame` contendo as colunas `codigo_situacao`, `deficit_agua_relativo_rural` e `regiao`.
#' @export
#'
#' @return um `data.frame` contendo as colunas adicionais `fracao_investimento_coletivo_agua` e `fracao_investimento_individual_agua`.
fracao_coletivo_individual_agua <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    fracao_investimento_coletivo_agua = dplyr::case_when(
      codigo_situacao == 7 & deficit_agua_relativo_rural < 0.7 ~ 0.1,
      codigo_situacao == 7 & deficit_agua_relativo_rural >= 0.7 ~ 0.25,
      codigo_situacao == 8 & deficit_agua_relativo_rural < 0.8 ~ 0.1,
      codigo_situacao == 8 & deficit_agua_relativo_rural >= 0.8 ~ 0.25,
      codigo_situacao == 5 & deficit_agua_relativo_rural < 0.6 ~ 0.25,
      codigo_situacao == 5 & deficit_agua_relativo_rural >= 0.6 ~ 0.4,
      codigo_situacao == 6 & deficit_agua_relativo_rural < 0.6 ~ 0.25,
      codigo_situacao == 6 & deficit_agua_relativo_rural >= 0.6 ~ 0.4,
      codigo_situacao == 4 & regiao != "Sudeste" ~ 1,
      codigo_situacao == 4 & deficit_agua_relativo_rural < 0.1 & regiao == "Sudeste" ~ 0.25,
      codigo_situacao == 4 & deficit_agua_relativo_rural >= 0.1 & regiao == "Sudeste" ~ 1
    ),
    fracao_investimento_individual_agua =
      1 - fracao_investimento_coletivo_agua
  )
}

#' Classifica densidade populacional no setor censitário
#'
#' Classifica setor censitário baseando-se na densidade populacional.
#'
#' @param tabela um `data.frame` contendo a coluna `densidade`.
#' @export
#'
#' @return um `data.frame` contendo a coluna adicional `classe_densidade`.
classifica_densidade_setor <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    classe_densidade = dplyr::case_when(
      densidade >= 0 & densidade < 450 ~ "F01",
      densidade >= 450 & densidade < 802.5 ~ "F02",
      densidade >= 802.5 & densidade < 1500 ~ "F03",
      densidade >= 1500 & densidade < 2500 ~ "F04",
      densidade >= 2500 & densidade < 3500 ~ "F05",
      densidade >= 3500 & densidade < 4500 ~ "F06",
      densidade >= 4500 & densidade < 5500 ~ "F07",
      densidade >= 5500 & densidade < 7000 ~ "F08",
      densidade >= 7000 & densidade < 9000 ~ "F09",
      densidade >= 9000 & densidade < 15000 ~ "F10",
      densidade >= 15000 & densidade < 25000 ~ "F11",
      densidade >= 25000 & densidade < 35000 ~ "F12",
      densidade >= 35000 & densidade < 45000 ~ "F13",
      densidade >= 45000 ~ "F14"
    )
  )
}

#' Classifica défict no setor censitário
#'
#' Classifica setor de censitário baseando-se no numero de moradores
#'
#' @param tabela um `data.frame` contendo a coluna `populacao`.
#' @export
#'
#' @return um `data.frame` contendo a coluna adicional `classe_densidade`.
classifica_deficit_setor <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    classe_deficit = dplyr::case_when(
      populacao > 0 & populacao <= 2000 ~ "A",
      populacao > 2000 & populacao <= 4000 ~ "B",
      populacao > 4000 & populacao <= 10000 ~ "C",
      populacao > 10000 & populacao <= 20000 ~ "D",
      populacao > 20000 & populacao <= 34000 ~ "E",
      populacao > 34000 ~ "F"
    )
  )
}

#' Custo individual para sistemas de abastecimento de água
#'
#' Adiciona coluna com custo individual para sistemas de abastecimento de água.
#'
#' @param tabela um `data.frame` contendo as colunas `seguranca_hidrica` e `semiarido`.
#' @export
#'
#' @return um `data.frame` contendo a coluna adicional `custo_agua_individual`.
custo_individual_agua <- function(tabela, custo, custo_sem_disponibilidade) {
  classes_seguranca <- c("Baixa", "Mínima")
  tabela <- dplyr::mutate(
    tabela,
    custo_agua_individual = ifelse(
      seguranca_hidrica %in% classes_seguranca & semiarido == "SIM",
      custo_sem_disponibilidade, custo
    )
  )
}

#' Custo coletivos para sistemas de abastecimento de água
#'
#' Adiciona coluna com custo coletivos para sistemas de abastecimento de água.
#'
#' @param tabela um `data.frame` contendo as colunas `seguranca_hidrica` e `semiarido`.
#' @export
#'
#' @return um `data.frame` contendo a coluna adicional `custo_agua_individual`.
custo_producao_agua <- function(tabela, custo_producao) {
  manter <- c("custo_agua_producao_rural", "classe_deficit", "regiao")
  custo_producao <- dplyr::select(custo_producao, dplyr::all_of(manter))
  juntar_por <- c("regiao", "classe_deficit")
  tabela <- dplyr::left_join(tabela, custo_producao, by = juntar_por)
}

#' Custo coletivos para sistemas de abastecimento de água
#'
#' Adiciona coluna com custo coletivos para sistemas de abastecimento de água.
#'
#' @param tabela um `data.frame` contendo as colunas `seguranca_hidrica` e `semiarido`.
#' @export
#'
#' @return um `data.frame` contendo a coluna adicional `custo_agua_individual`.
custo_distribuicao_agua <- function(tabela, custo_distribuicao) {
  manter <- c("custo_agua_distribuicao_rural", "classe_densidade", "regiao")
  custo_distribuicao <- dplyr::select(custo_distribuicao, dplyr::all_of(manter))
  juntar_por <- c("regiao", "classe_densidade")
  tabela <- dplyr::left_join(tabela, custo_distribuicao, by = juntar_por)
}


#' Número de domicílios com deficit de água
#'
#' Calcula o número de domicílios com déficit de sistemas de abastecimento de água.
#'
#' @param tabela um `data.frame` contendo as colunas  `domicilios_projecao` e `deficit_agua_relativo_rural`.
#' @export
#'
#' @return um `data.frame` contendo a coluna adicional `domicilios_deficit_agua`.
domicilios_com_deficit_agua <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    domicilios_deficit_agua = domicilios_projecao * deficit_agua_relativo_rural,
  )
}

#' Número de habitantes com deficit de água
#'
#' Calcula o número de habitantes com déficit de sistemas de abastecimento de água.
#'
#' @param tabela um `data.frame` contendo as colunas  `domicilios_projecao` e `deficit_agua_relativo_rural`.
#' @export
#'
#' @return um `data.frame` contendo a coluna adicional `domicilios_deficit_agua`.
habitantes_com_deficit_agua <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    deficit_rural = domicilios_deficit_agua * morador_per_domicilio,
  )
}

#' Número de domicílios sistemas de água adequados
#'
#' Calcula o número de domicílios com sistemas de abastecimento de água adequados.
#'
#' @param tabela um `data.frame` contendo as colunas `domicilios_projecao` e `deficit_agua_relativo_rural`.
#' @export
#'
#' @return um `data.frame` contendo a coluna adicional `domicilios_adequados_agua`.
domicilios_adequados_com_agua <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    domicilios_adequados_agua = (domicilios_projecao) * (1.0 - deficit_agua_relativo_rural)
  )
}

#' Necessidade de investimento rural para sistema de abastecimento de água
#'
#' Calcula a necessidade de investimento para expansão do sistema de abastecimento de água.
#'
#' @param tabela um `data.frame` contendo as colunas:
#' `custo_agua_individual`, `custo_agua_producao_rural`, `custo_agua_distribuicao_rural`,
#' `fracao_investimento_individual_agua`, `fracao_investimento_coletivo_agua` e
#' `domicilios_deficit_agua`
#' @export
#'
#' @return um `data.frame` contendo a coluna adicional `domicilios_adequados_agua`.
investimento_rural_agua <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    investimento_expansao_individual = custo_agua_individual *
      fracao_investimento_individual_agua * domicilios_deficit_agua,
    investimento_expansao_producao_agua = custo_agua_producao_rural *
      fracao_investimento_coletivo_agua * domicilios_deficit_agua,
    investimento_expansao_distribuicao_agua = domicilios_deficit_agua *
      custo_agua_distribuicao_rural *
      fracao_investimento_coletivo_agua,
    investimento_agua_rural_expansao = investimento_expansao_individual +
      investimento_expansao_producao_agua +
      investimento_expansao_distribuicao_agua,
    # investimento_agua_rural_coletivo = investimento_expansao_producao_agua + investimento_expansao_distribuicao_agua,
  )
}

#' Capacidade instalada Água Rural
#'
#' Calcula a capacidade instalada para expansão do sistema de abastecimento de água rural.
#'
#' @param tabela um `data.frame` contendo as colunas:
#' `domicilios_adequados_agua`, `custo_agua_distribuicao_rural` e `custo_agua_producao_rural`
#' @export
#'
#' @return um `data.frame` contendo as colunas adicional capacidade_instalada_distribuicao_agua  e capacidade_instalada_producao_agua.
capacidade_instalada_rural_agua <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    capacidade_instalada_distribuicao_agua = domicilios_adequados_agua * custo_agua_distribuicao_rural,
    capacidade_instalada_producao_agua = domicilios_adequados_agua * custo_agua_producao_rural
  )
}

#' Consolida os dados de investimentos para água situação rural
#'
#' Totaliza os dados investimento em expansão, reposição e total.
#'
#' @param tabela contendo as colunas:
#' \itemize{
#' \item{investimento_expansao_individual}
#' \item{investimento_expansao_distribuicao_agua}
#' \item{investimento_expansao_producao_agua}
#' \item{investimento_reposicao_producao_agua}
#' \item{investimento_reposicao_distribuicao_agua}
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
consolida_investimentos_rural_agua <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    investimento_expansao = investimento_expansao_distribuicao_agua +
      investimento_expansao_producao_agua +
      investimento_expansao_individual,
    investimento_reposicao = investimento_reposicao_producao_agua +
      investimento_reposicao_distribuicao_agua,
    investimento_total = investimento_expansao + investimento_reposicao,
  )
}

#' Taxa de crescimento populacional
#'
#' Adiciona taxa de crescimento populacional
#'
#' @param tabela um `data.frame` contendo a coluna codigo_municipio
#' @param projecao um `data.frame` contendo a coluna `codigo_municipio` e `taxa_de_crescimento`
#' @export
#'
#' @return um `data.frame` contendo a coluna adicional `taxa_crescimento`.
adiciona_taxa_crescimento <- function(tabela, projecao) {
  manter <- c("codigo_municipio", "taxa_de_crescimento")
  projecao <- dplyr::select(projecao, dplyr::all_of(manter))
  tabela <- dplyr::left_join(tabela, projecao, by = "codigo_municipio")
}

#' Déficit rural água PNAD
#'
#' Adiciona o déficit rural para sistema de abastecimento de água (PNAD).
#'
#' @param tabela um `data.frame` contendo a coluna `estado`
#' @param deficit um `data.frame` contendo a coluna `estado` e `deficit_agua_relativo_rural`
#' @export
#'
#' @return um `data.frame` contendo a coluna adicional `taxa_crescimento`.
adiciona_deficit_rural_agua_pnad <- function(tabela, deficit) {
  manter <- c("estado", "deficit_agua_relativo_rural")
  deficit <- dplyr::select(deficit, dplyr::all_of(manter))
  tabela <- dplyr::left_join(tabela, deficit, by = "estado")
}

#' Segurança Hídrica
#'
#' Adiciona dados de segurança hídrica
#'
#' @param tabela um `data.frame` contendo a coluna `codigo_municipio`
#' @param deficit um `data.frame` contendo a coluna `codigo_municipio`, `seguranca_hidrica` e `semiarido`.
#' @export
#'
#' @return um `data.frame` contendo a coluna adicional `taxa_crescimento`.
adiciona_seguranca_hidrica <- function(tabela, seguranca_hidrica) {
  manter <- c("codigo_municipio", "seguranca_hidrica", "semiarido")
  seguranca_hidrica <- dplyr::select(seguranca_hidrica, dplyr::all_of(manter))
  tabela <- dplyr::left_join(tabela, seguranca_hidrica, by = "codigo_municipio")
}

#' Filtra setores censitarios
#'
#' Fitra a tabela deixando-a somente com as linhas com o tipo de situação desejada.
#'
#' @param tabela um `data.frame` contendo a coluna `codigo_municipio`
#' @param setores um `list` contendo as situações dos setores que se quer manter.
#' @export
#'
#' @return um `data.frame` contendo a coluna adicional `taxa_crescimento`.
filtra_setores_rurais <- function(tabela) {
  tabela <- dplyr::filter(tabela, situacao == "Rural")
}


#' Faz projeção de domicílios
#'
#' Projeta o número de domícilio pela projeção populacional e considera que a relação domicílio/habitantes se mantém constante.
#'
#' @param tabela um `data.frame` contendo a coluna `codigo_municipio`
#' @param ano_inicial um `number` contendo o ano do conjunto de dados de população (populacao)
#' @param ano_final um `number` contendo o ano da projeção
#'
#' @export
#'
#' @return um `data.frame` contendo a coluna adicional `taxa_crescimento`.
fazer_projecao_domicilio <- function(tabela, ano_inicial, ano_final) {
  tabela <- dplyr::mutate(
    tabela,
    populacao_projecao = populacao * ((1 + taxa_de_crescimento)^(ano_final - ano_inicial)),
    domicilios_projecao = ifelse(morador_per_domicilio == 0, 0, populacao_projecao / morador_per_domicilio)
  )
}

#' Cria tabela longa de necessidade de investimento do componente água
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
#'  \item{estado}
#'  \item{regiao}
#'  \item{componente}
#'  \item{situacao}
#'  \item{destino}
#'  \item{subsistema}
#' }
#'
#' @export
tbl_longa_investimentos_agua_rural <- function(tabela) {
  colunas <- c(
    "estado", "regiao",
    "investimento_expansao_individual",
    "investimento_expansao_distribuicao_agua",
    "investimento_expansao_producao_agua",
    "investimento_reposicao_producao_agua",
    "investimento_reposicao_distribuicao_agua"
  )
  tabela <- dplyr::select(tabela, dplyr::all_of(colunas))
  tabela <- somar_por_campo(tabela, "estado")
  tabela <- tidyr::pivot_longer(
    tabela,
    cols = starts_with("investimento_"),
    names_to = c("destino", "subsistema"),
    names_pattern = "investimento_(.*?)_(.*)",
    values_to = "necessidade_investimento"
  )
  tabela <- dplyr::mutate(
    tabela,
    componente = "agua",
    situacao = "rural"
  )
}

#' Cria tabela longa de deficit do componente água e situacao rural
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
tbl_longa_deficit_agua_rural <- function(tabela) {
  colunas <- c("estado", "regiao", "deficit_rural")
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

#' Calcula deficit relativo para agua situação rural usando dados do censo
#'
#' @param tabela um `data.frame` contendo as colunas `populacao` e `atendimento_agua`.
#'
#' @return um `data.frame` contendo a coluna adicional `deficit_agua_relativo_rural`.
#' @export
calcula_deficit_agua_relativo_censo <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    deficit_agua_relativo_rural = pmax(populacao - atendimento_agua, 0) / populacao
  )
  return(tabela)
}

#' Calcula deficit relativo para agua situação rural usando dados da pnadc
#'
#' @param tabela um `data.frame` contendo as colunas `populacao` e `atendimento_rur_agua`.
#' @param ano um `number` contendo o ano do PNADC.
#'
#' @return um `data.frame` contendo a coluna adicional `deficit_agua_relativo_rural`.
#' @export
calcula_deficit_agua_relativo_pnadc <- function(tabela, ano) {
  tabela <- adiciona_atendimento_relativo_pnadc(tabela, "agua", ano)
  tabela <- dplyr::mutate(
    tabela,
    deficit_agua_relativo_rural = 1 - atendimento_agua_relativo_rural
  )
  return(tabela)
}
#' Módulo de cálculo para demanda rural de água
#'
#' Esta função organiza a ordem de execução das tarefas necessárias
#' para o cálculo de necessidades de investimento em sistema de abastecimento de água.
#'
#' @param input estrutura de dados (`reactive`) que guarda os parâmetros da interface gráfica
#' @param taxas_projecao um `data.frame` contendo as taxas de projecao populacional
#' @export
#'
#' @return um `data.frame` contendo as necessidade de investimentos e todos campos utilizados
rodar_modulo_rural_agua <- function(state) {
  input <- state$input
  taxas_projecao <- state$taxas_projecao
  ano_final <- input$geral$ano
  param <- input$agua # parametros
  ano_censo <- nome_para_ano(input$agua$atendimento_ano)
  ano_inicial <- input$agua$atendimento_ano

  rlog::log_info("água:rural: carregando dados")
  data("agua_esgoto_rural", package = "rsan")
  agua_esgoto_rural <- get("agua_esgoto_rural")
  seguranca_hidrica <- agua_esgoto_rural$seguranca_hidrica
  custo_producao <- agua_esgoto_rural$custo_producao
  custo_distribuicao <- agua_esgoto_rural$custo_distribuicao

  rlog::log_info("água:rural: carregando setores censitários")
  tabela <- base_municipios()
  tabela <- dplyr::left_join(
    tabela,
    carrega_censo2022_setor(),
    by = "codigo_municipio"
  )
  tabela <- filtra_setores_rurais(tabela)

  rlog::log_info("água:rural: projecao de domicilios")
  tabela <- adiciona_taxa_crescimento(tabela, taxas_projecao)
  tabela <- fazer_projecao_domicilio(tabela, ano_censo, ano_final)

  if (input$agua$atendimento == "pnadc") {
    rlog::log_info("água:rural: adicionando atendimento do PNADC")
    tabela <- calcula_deficit_agua_relativo_pnadc(
      tabela,
      input$agua$atendimento_ano
    )
  } else {
    rlog::log_info("água:rural: adicionando atendimento do CENSO")
    tabela <- calcula_deficit_agua_relativo_censo(tabela)
  }

  rlog::log_info("água:rural: classificando setores")
  tabela <- classifica_densidade_setor(tabela)
  tabela <- classifica_deficit_setor(tabela)
  tabela <- adiciona_seguranca_hidrica(tabela, seguranca_hidrica)
  tabela <- fracao_coletivo_individual_agua(tabela)
  rlog::log_info("água:rural: calculando custos")
  tabela <- custo_individual_agua(
    tabela,
    custo = param$custo_rural_individual,
    custo_sem_disponibilidade = param$custo_rural_individual_sem
  )
  tabela <- custo_producao_agua(tabela, custo_producao)
  tabela <- custo_distribuicao_agua(tabela, custo_distribuicao)

  tabela <- domicilios_com_deficit_agua(tabela)
  tabela <- habitantes_com_deficit_agua(tabela)
  tabela <- domicilios_adequados_com_agua(tabela)

  rlog::log_info("água:rural: calculando necessidade")
  tabela <- investimento_rural_agua(tabela)
  tabela <- capacidade_instalada_rural_agua(tabela)

  tabela <- calcula_reposicao_parcial(
    tabela,
    "capacidade_instalada_distribuicao_agua",
    "investimento_expansao_distribuicao_agua",
    "investimento_reposicao_distribuicao_agua",
    ano_inicial,
    ano_final,
    input$geral$ano_corrente,
    input$agua$vida_util
  )

  tabela <- domicilios_com_deficit_agua(tabela)
  tabela <- domicilios_adequados_com_agua(tabela)
  tabela <- investimento_rural_agua(tabela)
  tabela <- calcula_reposicao_parcial(
    tabela,
    "capacidade_instalada_producao_agua",
    "investimento_expansao_producao_agua",
    "investimento_reposicao_producao_agua",
    ano_inicial,
    ano_final,
    input$geral$ano_corrente,
    input$agua$vida_util
  )
  rlog::log_info("água:rural: consolidando necessidade")
  tabela <- consolida_investimentos_rural_agua(tabela)

  state$agua_rural <- adiciona_pais(tabela)

  state$necessidade <- dplyr::bind_rows(
    tbl_longa_investimentos_agua_rural(state$agua_rural),
    state$necessidade
  )

  state$deficit <- dplyr::bind_rows(
    tbl_longa_deficit_agua_rural(tabela),
    state$deficit
  )
  return(state)
}
