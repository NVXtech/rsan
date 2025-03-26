#' Necessidade de Investimento - Componente Drenagem - Situação Urbana
#'
#' Investimento total
#'
#' Soma os investimentos de expansao e resposicao
#'
#' @param tabela contendo os campos `investimento_expansao` e `investimento_reposicao`
#'
#' @return tabela com a soma dos investimento no campo `investimento_total`
#' @export
investimento_total_drenagem <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    investimento_total = investimento_expansao + investimento_reposicao + investimento_cadastro
  )
  return(tabela)
}

#' Cria tabela longa de necessidade de investimento do componente drenagem
#'
#' @param tabela contendo as colunas:
#' \itemize{
#' \item{investimento_expansao}
#' \item{investimento_reposicao}
#' \item{investimento_cadastro}
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
tbl_longa_investimento_drenagem <- function(tabela) {
  colunas <- c(
    "estado", "regiao",
    "investimento_expansao",
    "investimento_reposicao",
    "investimento_cadastro"
  )
  tabela <- dplyr::select(tabela, dplyr::all_of(colunas))
  tabela <- somar_por_campo(tabela, "estado")
  tabela <- tidyr::pivot_longer(
    tabela,
    cols = c("investimento_expansao", "investimento_reposicao", "investimento_cadastro"),
    names_to = c("destino"),
    names_pattern = "investimento_(.*)",
    values_to = "necessidade_investimento"
  )
  tabela <- dplyr::mutate(
    tabela,
    componente = "drenagem",
    situacao = "urbana",
    subsistema = "drenagem_urbana"
  )
}

#' Capacidade instalada para drenagem
#'
#' @param tabela `data.frame` contendo os campos:
#'
#' @return um `data.frame` contendo o campo `capacidade instalada`
#' @export
#'
#' @examples
#' \dontrun{
#' tabela <- capacidade_instalada_drenagem(tabela)
#' }
capacidade_instalada_drenagem <- function(tabela) {
  data("investimento_existente", package = "rsan")
  investimento_existente <- get("investimento_existente")
  tabela <- dplyr::left_join(
    tabela, investimento_existente,
    by = "codigo_municipio"
  )
  tabela <- dplyr::mutate(
    tabela,
    capacidade_instalada = investimento_existente
  )
  return(tabela)
}

#' Investimento constante
#'
#' Valor
#'
#' @param tabela um `data.frame` contendo a projeção da população urbana
#' @param valor um `double` contendo o custo de expansão per capita (R$/hab)
#'
#' @return tabela com os valores de investimento
#' @export
investimento_constante <- function(tabela, valor) {
  tabela <- dplyr::mutate(
    tabela,
    investimento_expansao = populacao_urbana * valor - capacidade_instalada
  )
  return(tabela)
}

#' Aplica regressao simples drenagem
#'
#' @param tabela um `data.frame` contendo as colunas GE006
#' @param modelo um objeto da classe `lm` contendo os parâmetros do modelo de regressão
#'
#' @return tabela com os valores de investimento
#' @export
aplica_regressao_drenagem <- function(tabela, modelo) {
  tabela <- dplyr::mutate(
    tabela,
    modelo = stats::predict(modelo, tabela),
    investimento_modelo = modelo * GE006,
    investimento_expansao = investimento_modelo - capacidade_instalada # TODO arrumar para trocar pelo investimento do plano
  )
  return(tabela)
}

#' Aplica regressao multipla drenagem
#'
#' @param tabela um `data.frame` contendo as colunas `precipitacao`, `densidade_urbana`, `caracteristicas_fisicas` e `infrestrutura`
#' @param parametros um `list` contendo os parâmetros do modelo de regressão
#'
#' @return tabela com a coluna adicional `investimento_expansao`
#' @export
aplica_regressao_multipla_drenagem <- function(tabela, parametros) {
  fp <- parametros$peso_pluviometria
  fd <- parametros$peso_densidade
  fc <- parametros$peso_fisicas
  fi <- parametros$peso_infraestrutura
  c <- parametros$peso_constante
  tabela <- dplyr::mutate(
    tabela,
    investimento_expansao = pmax((c +
      precipitacao_moda * fp +
      densidade_urbana * fd +
      caracteristicas_fisicas * fc +
      infraestrutura * fi) * populacao_urbana - capacidade_instalada, 0.0)
  )
  return(tabela)
}


#' Regressão Investimento em Drenagem
#'
#' Calcula a regressão linear entre densidade de investimento em drenagem e o índice PD
#'
#' @param plano é um `data.frame` contendo os valores do plano
#'
#' @return objeto da classe `lm` contendo os parâmetros da regressão
#' @export
#'
#' @examples
#' \dontrun{
#' equacao <- regressao_drenagem(plano)
#' }
regressao_drenagem <- function(plano) {
  plano <- dplyr::mutate(
    plano,
    densidade_investimento = investimento_corrigido / GE006
  )
  return(stats::lm(formula = densidade_investimento ~ pd, plano))
}

#' Regressão Múltipla Investimento em Drenagem
#'
#' Calcula a regressão linear entre densidade de investimento em drenagem e o índice PD
#'
#' @param plano é um `data.frame` contendo os valores do plano
#'
#' @return objeto da classe `lm` contendo os parâmetros da regressão
#' @export
#'
#' @examples
#' \dontrun{
#' equacao <- regressao_multipla_drenagem(plano)
#' }
regressao_multipla_drenagem <- function(plano) {
  plano <- dplyr::mutate(
    plano,
    densidade_investimento = investimento_corrigido / GE006
  )
  return(stats::lm(
    formula =
      densidade_investimento ~ pluviometria + densidade_urbana +
        caracteristicas_fisicas + infraestrutura,
    plano
  ))
}

#' Concatena dados para regressão
#'
#' @param plano um `data.frame` contendo as colunas `codigo_muncipio` e 'investimento_corrigido'
#' @param tabela um `data.frame` contendo as colunas `codigo_muncipio`, `pd` e `GE006`
#'
#' @return um `data.frame` contendo os campos necessários para regressão linear
#' @export
#'
#' @examples
#' \dontrun{
#' plano <- prepara_regressao(plano, tabela)
#' }
prepara_regressao <- function(plano, tabela) {
  tabela <- dplyr::select(tabela, "codigo_municipio", "pd", "GE006")
  plano <- dplyr::left_join(plano, tabela, by = "codigo_municipio")
}

#' Corrige preços do plano de drenagem
#'
#' Realiza a correção dos preços para data passada.
#'
#' @param data um `Date` contendo a data para correção do preço
#'
#' @return o `data.frame` do plano drenagem com os preços corrigidos
#' @export
corrige_plano_drenagem <- function(data) {
  data(plano_drenagem, package = "rsan")
  igp <- get_igp()
  plano_drenagem <- dplyr::mutate(
    plano_drenagem,
    data_inicial = as.Date(paste0(as.character(ano_plano), "-06-30")),
    data_final = as.Date(data),
    taxa_igp = get_taxa_igp(igp, data_inicial, data_final), # fix get_taxa_igp
    investimento_corrigido = investimento * taxa_igp
  )
  return(plano_drenagem)
}

#' Calcula o coeficiente PD
#'
#' O coeficiente PD é a multiplicação entre a moda da precipitação(mm) e a densidadade urbana (hab/km²) dividos por 1000.
#'
#' @param tabela é um `data.frame` contendo as colunas `precipitacao_moda` e `densidade urbana`
#'
#' @return um `data.frame` contendo a coluna `pd`
#' @export
#'
#' @examples
#' tabela <- dplyr::tibble(precipitacao_moda = c(1, 1), densidade_urbana = c(2, 2))
#' tabela <- coeficiente_pd(tabela)
#'
coeficiente_pd <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    pd = precipitacao_moda * densidade_urbana / 1e3
  )
  return(tabela)
}

#' Precipitação
#'
#' Adiciona a coluna de precipitacao_moda na tabela de acordo com o munícipio.
#'
#' @param tabela um `data.frame` contendo a coluna `codigo_municipio`.
#'
#' @return a tabela inicial (`data.frame`) com a coluna `precipitacao_moda`
#' @export
#'
#' @examples
#' \dontrun{
#' tabela <- precipitacao(tabela)
#' }
precipitacao <- function(tabela) {
  data(pluviometria, package = "rsan")
  pluviometria <- dplyr::select(
    pluviometria,
    codigo_municipio,
    precipitacao_moda
  )
  tabela <- dplyr::left_join(
    tabela,
    pluviometria,
    by = "codigo_municipio"
  )
  return(tabela)
}

#' Densidade urbana
#'
#' @param tabela contendo as colunas do SNIS: `GE006` e `GE002`
#'
#' @return uma tabela contendo a densidade urbana
#' @export
#'
#' @examples
#' \dontrun{
#' tabela <- densidade_urbana(tabela)
#' }
densidade_urbana <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    densidade_urbana = GE006 / area_urbana
  )
  return(tabela)
}

#' Área urbana
#'
#'
#' @param tabela contendo as colunas do SNIS `GE002`
#'
#' @return uma tabela contendo a densidade urbana
#' @export
#'
#' @examples
#' \dontrun{
#' tabela <- area_urbana(tabela)
#' }
area_urbana <- function(tabela) {
  data("area_urbana_municipio", package = "rsan")
  area_urbana_municipio <- get("area_urbana_municipio")
  tabela <- dplyr::left_join(
    tabela, area_urbana_municipio,
    by = "codigo_municipio"
  )
  return(tabela)
}

#' Índices de drenagem
#'
#' Adiciona a tabela os índices de drenagem relativos as caracteristicas físicas e infraestrutura.
#'
#' @param tabela contendo a coluna código do município
#'
#' @return a tabela de entrada com os campos adicionais dos índices (`caracteriticas_fisicas` e `infraestrutura`)
#' @export
adiciona_indices_drenagem <- function(tabela) {
  data("indices_drenagem", package = "rsan")
  tabela <- dplyr::left_join(tabela, indices_drenagem, by = "codigo_municipio")
  return(tabela)
}

#' Investimento em Cadastro Técnico
#'
#' Calcula o valor necessário para investir em cadastro técnico.
#'
#' @param tabela contendo a coluna código do município
#'
#' @return a tabela de entrada com os campos adicionais dos índices (`caracteriticas_fisicas` e `infraestrutura`)
#' @export
investimento_cadastro <- function(tabela, valor) {
  tabela <- dplyr::mutate(
    tabela,
    cadastro_tecnico = ifelse(IE012 == "Sim", 0.0, 1.0),
    investimento_cadastro = cadastro_tecnico * valor * area_urbana
  )
  return(tabela)
}
