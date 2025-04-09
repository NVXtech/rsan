#' Necessidade de Investimento - Componente Esgoto - Situação Urbana

esgoto_required_fields <- c(
  "codigo_municipio",
  "atendimento_tot_agua_hab",
  "atendimento_urb_agua_hab",
  "extensao_rede_agua_km",
  "volume_agua_produzido_dam3_ano",
  "volume_agua_consumido_dam3_ano",
  "extensao_rede_esgoto_km",
  "volume_esgoto_tratado_dam3_ano",
  "volume_esgoto_tratado_dam3_ano"
)
#' Calcula deficits e demandas para esgoto
#'
#' @param df um `data.frame` contendo os dados do SNIS e de população
#' @param meta_esgoto meta em % de atendimento para tratamento de esgoto
#' @param proporcao proporção da densidade de água que equivale a densidade de esgoto
#'
#' @return um `data.frame` contendo os dados de deficits e demandas
#' @export
#'
#' @examples
#' \dontrun{
#' df <- calculate_demografico_esgoto(df, 99, 90, 80)
#' }
calculate_demografico_esgoto <- function(df, meta_esgoto, proporcao) {
  df$atendimento_tot_esgoto_hab[is.na(df$atendimento_tot_esgoto_hab)] <- 0
  df$atendimento_urb_esgoto_hab[is.na(df$atendimento_urb_esgoto_hab)] <- 0
  df <- dplyr::mutate(
    df,
    deficit_total = pmax(populacao_total - atendimento_tot_esgoto_hab, 0.0),
    deficit_urbana = pmax(populacao_urbana - atendimento_urb_esgoto_hab, 0.0),
    deficit_rural = pmax(deficit_total - deficit_urbana, 0.0),
    densidade_tratamento_esgoto = proporcao / 100.0 * densidade_producao_agua,
    demanda_coleta_esgoto = meta_esgoto / 100.0 * deficit_urbana * densidade_coleta_esgoto,
    demanda_tratamento_esgoto = meta_esgoto / 100.0 * deficit_urbana * densidade_tratamento_esgoto
  )
  df <- dplyr::select(
    df,
    codigo_municipio,
    estado,
    deficit_total,
    deficit_urbana,
    deficit_rural,
    demanda_coleta_esgoto,
    demanda_tratamento_esgoto,
    populacao_total,
    populacao_urbana,
    populacao_rural,
    densidade_coleta_esgoto,
    densidade_tratamento_esgoto
  )
}

#' Calcula custo relativo para tratamento de esgoto
#'
#' Custo relativo é o valor monetário necessário para tratar 1m3 de esgoto por ano R$/(m3/ano).
#'
#' @param preco_unidade tabela contendo as colunas estado, unidade e preco
#' @param projeto_tratamento tabela contendo cenario, unidade e quantidade
#'
#' @return tabela com as colunas estado, cenario e custo_relativo
#' @export
#'
#' @examples
#' estado <- c("AC", "AC", "AC", "AC", "AL", "AL", "AL", "AL")
#' unidade <- c(
#'   "LAGOA125", "REATORANA180", "EE85", "LODOBAT400",
#'   "LAGOA125", "REATORANA180", "EE85", "LODOBAT400"
#' )
#' preco <- c(2.476, 0.762, 0.209, 2.31, 2.051, 0.4282, 0.2008, 2.2114)
#' preco_unidade <- dplyr::tibble(estado, unidade, preco)
#'
#' cenario <- c("07", "07", "07", "09", "09")
#' unidade <- c("LAGOA125", "REATORANA180", "EE85", "LODOBAT400", "EE85")
#' quantidade <- c(1.2, 1, 1, 2.05 * 1.71361608179778, 1)
#' projeto_tratamento <- dplyr::tibble(cenario, unidade, quantidade)
#'
#' df_out <- calcula_custo_relativo_tratamento(preco_unidade, projeto_tratamento)
calcula_custo_relativo_tratamento <-
  function(preco_unidade, projeto_tratamento) {
    tabela <-
      dplyr::full_join(projeto_tratamento, preco_unidade, by = "unidade")

    tabela <- dplyr::mutate(tabela, custo_relativo = quantidade * preco)
    tabela <- dplyr::group_by(tabela, estado, cenario)
    tabela <-
      dplyr::summarise(tabela, custo_relativo = sum(custo_relativo, na.rm = TRUE))
  }

#' Calcula a necessidade de investimento para tratamendo de esgoto
#'
#' @param demanda  tabela com as demandas de tratamento de esgoto (m3/ano) por município
#' @param tratamento tabela com os custos das unidades de tratamento de esgoto por município
#'
#' @return tabela com a  necessidade de investimento para tratamendo de esgoto
#' @export
#'
#' @examples
#' \dontrun{
#' custo_relativo <- calcula_custo_expansao_tratamento(demanda, tratamento)
#' }
calcula_custo_expansao_tratamento <- function(demanda, tratamento) {
  data("projeto_tratamento_esgoto", package = "rsan")
  custo_relativo <-
    calcula_custo_relativo_tratamento(tratamento, projeto_tratamento_esgoto)
  tabela <-
    dplyr::left_join(demanda, custo_relativo, by = c("estado", "cenario"))
  tabela <-
    dplyr::mutate(tabela,
      custo_expansao_tratamento_esgoto = custo_relativo * demanda_tratamento_esgoto
    )
  colnames(tabela)[colnames(tabela) == "custo_relativo"] <-
    "custo_relativo_tratamento"
  return(tabela)
}


#' Calcula o custo de expansão do sistema de esgoto
#'
#' @param demanda tabela com as demandas de distribuição e produção de água, coleta e tratamento de egoto.
#' @param coleta tabela com custos relativos (R$/m) de coleta de esgoto por estado
#' @param tratamento tabela com custos relativos R$/(m3/ano) de tratamento de esgoto
#'
#' @return tabela com as necessidade de investimento
#' @export
#'
#' @examples
#' \dontrun{
#' invest <- calcula_custo_expansao_esgoto(
#'   demanda, distribuicao, coleta, producao, tratamento, perda_agua
#' )
#' }
calcula_custo_expansao_esgoto <- function(demanda, coleta, tratamento) {
  tabela <- calcula_custo_extensao(demanda, coleta, "coleta_esgoto")
  tabela <- calcula_custo_expansao_tratamento(tabela, tratamento)
  return(tabela)
}

#' Consolida os dados de investimentos esgoto
#'
#' Totaliza os dados investimento em expansão, reposição e total.
#' Adiciona coluna com o país para totalização.
#'
#' @param tabela contendo as colunas:
#' \itemize{
#' \item{custo_expansao_coleta_esgoto}
#' \item{custo_expansao_tratamento_esgoto}
#' \item{custo_reposicao_tratamento_esgoto}
#' \item{custo_reposicao_coleta_esgoto}
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
#' tabela <- consolida_investimentos_esgoto(tabela)
#' }
consolida_investimentos_esgoto <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    investimento_expansao = custo_expansao_coleta_esgoto + custo_expansao_tratamento_esgoto,
    investimento_reposicao = custo_reposicao_tratamento_esgoto + custo_reposicao_coleta_esgoto,
    investimento_total = investimento_expansao + investimento_reposicao,
  )
}


#' Cria tabela longa de necessidade de investimento do componente esgoto
#'
#' @param tabela contendo as colunas:
#' \itemize{
#' \item{custo_expansao_coleta_esgoto}
#' \item{custo_expansao_tratamento_esgoto}
#' \item{custo_reposicao_tratamento_esgoto}
#' \item{custo_reposicao_coleta_esgoto}
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
tbl_longa_investimentos_esgoto <- function(tabela) {
  colunas <- c(
    "estado", "regiao",
    "custo_expansao_coleta_esgoto",
    "custo_expansao_tratamento_esgoto",
    "custo_reposicao_tratamento_esgoto",
    "custo_reposicao_coleta_esgoto"
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
    componente = "esgoto",
    situacao = "urbana"
  )
}

#' Cria tabela longa de deficit do componente esgoto
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
tbl_longa_deficit_esgoto <- function(tabela) {
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
    componente = "esgoto",
    subsistema = "coleta_tratamento",
    deficit = as.integer(deficit)
  )
}

#' Módulo orçamentário para esgoto
#'
#' @param input estrutura de dados (`reactive`) que guarda os parâmetros da interface gráfica
#' @param demografico estrutura de dados (`data.frame`) que armazena os resultados do módulo demográfico
#'
#' @return  estrutura de dados (`data.frame`) que armazenas os resultados do módulo orçamentário
#' @export
#'
#' @examples
#' \dontrun{
#' orca <- rodar_modulo_orcamentario_esgoto(input, demografico)
#' }
rodar_modulo_orcamentario_esgoto <- function(input, demografico) {
  sinapi <- load_sinapi(input$esgoto$sinapi)

  data("projeto_coleta_esgoto", package = "rsan")
  coleta <- calcula_precos_distribuicao(
    projeto_coleta_esgoto,
    sinapi,
    input$esgoto$fator_servicos,
    input$esgoto$fator_materiais
  )

  data("projeto_tratamento_esgoto_unidades", package = "rsan")
  tratamento <- calcula_preco_unidades_producao(
    projeto_tratamento_esgoto_unidades,
    sinapi,
    input$esgoto$fator_insumo,
    input$esgoto$fator_composicao
  )

  custo <- calcula_custo_expansao_esgoto(
    demografico,
    coleta,
    tratamento
  )
  resultado <- list(
    coleta = coleta,
    tratamento = tratamento,
    custo = custo
  )

  return(resultado)
}

#' Capacidade instalada sistema de abastecimento de água
#'
#' @param snis um `data.frame` contendo as colunas `codigo_municipio`, `extensao_rede_esgoto_km` e `volume_esgoto_tratado_dam3_ano`
#' @param custo um `data.frame` contendo as colunas `codigo_municipio`, `custo_relativo_tratamento`, `preco_coleta_esgoto`
#'
#' @return um `data.frame` contendo as colunas `capacidade_instalada_coleta` e `capacidade_instalada_tratamento`
#' @export
capacidade_instalada_esgoto <- function(snis, custo) {
  # Quando não informada assume extensões igual a 0
  snis$extensao_rede_esgoto_km[is.na(snis$extensao_rede_esgoto_km)] <- 0
  snis$volume_esgoto_tratado_dam3_ano[is.na(snis$volume_esgoto_tratado_dam3_ano)] <- 0

  tabela <- dplyr::left_join(snis, custo, by = "codigo_municipio")
  tabela <- dplyr::mutate(
    tabela,
    capacidade_instalada_coleta = extensao_rede_esgoto_km * 1e3 * preco_coleta_esgoto,
    capacidade_instalada_tratamento = volume_esgoto_tratado_dam3_ano * 1e3 * custo_relativo_tratamento
  )
  return(tabela)
}

#' Módulo financeiro para demanda urbana de esgoto
#'
#' Esta função organiza a ordem de execução das tarefas necessárias
#' para o cálculo de necessidades de investimento em sistema de abastecimento de esgoto.
#'
#' @param input estrutura de dados (`reactive`) que guarda os parâmetros da interface gráfica
#' @param orcamentario um `data.frame` contendo a saida do modulo orcamentário
#' @export
#'
#' @return um `data.frame` contendo as necessidade de investimentos e todos campos utilizados
rodar_modulo_financeiro_esgoto <- function(input, orcamentario) {
  snis_data <- carrega_base_calculo("esgoto", input$esgoto$fonte_nome, input$esgoto$fonte_ano)
  custo <- orcamentario$custo
  tabela <- capacidade_instalada_esgoto(snis_data, custo)
  ano_final <- input$geral$ano
  ano_inicial <- nome_para_ano(input$esgoto$snis) + 1
  rlog::log_info(sprintf("esgoto anoi=%s anof=%s", ano_inicial, ano_final))
  ano_corrente <- input$geral$ano_corrente

  vars <- list(
    campos_reposicao(
      "capacidade_instalada_coleta",
      "custo_expansao_coleta_esgoto",
      "custo_reposicao_coleta_esgoto",
      input$esgoto$vida_util
    ),
    campos_reposicao(
      "capacidade_instalada_tratamento",
      "custo_expansao_tratamento_esgoto",
      "custo_reposicao_tratamento_esgoto",
      input$esgoto$vida_util
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

#' Calcula a necessidade de investimento para esgoto
#'
#' @param state estado da aplicação
#'
#' @return estado da aplicação
#' @export
#'
#' @examples
#' \dontrun{
#' app_state <- investimento_esgoto(state)
#' }
investimento_esgoto <- function(state) {
  rlog::log_info("esgoto: carregando projecao populacional")
  projecao <- state$projecao
  rlog::log_info("esgoto: carregando parâmetros")
  input <- state$input
  rlog::log_info("esgoto: rodando módulo demografico")
  demografico <- rodar_modulo_demografico(input, projecao, "esgoto")
  rlog::log_info("esgoto: rodando módulo orcamentario")
  orcamentario <- rodar_modulo_orcamentario_esgoto(input, demografico)
  rlog::log_info("esgoto: rodando módulo financeiro")
  financeiro <- rodar_modulo_financeiro_esgoto(input, orcamentario)
  tabela <- consolida_investimentos_esgoto(financeiro)
  tabela <- adiciona_pais(tabela)
  tabela <- adiciona_regiao(tabela)
  state$esgoto <- tabela

  rlog::log_info("esgoto: rodando módulo rural")
  state <- rodar_modulo_rural_esgoto(state)

  state$necessidade <- dplyr::bind_rows(
    tbl_longa_investimentos_esgoto(tabela),
    state$necessidade
  )
  state$deficit <- dplyr::bind_rows(
    tbl_longa_deficit_esgoto(tabela),
    state$deficit
  )
  return(state)
}
