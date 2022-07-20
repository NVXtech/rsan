#' Custo individual para sistemas de estações de esgoto
#'
#' Adiciona coluna com custo individual para sistemas de estações de esgoto.
#'
#' @param tabela um `data.frame` contendo as colunas `seguranca_hidrica` e `V003`.
#' @export
#'
#' @return um `data.frame` contendo a coluna adicional `custo_esgoto_individual `.
custo_individual_esgoto <- function(tabela) {
    classes_seguranca <- c("Baixa", "Mínima")
    tabela <- dplyr::mutate(
        tabela,
        custo_esgoto_individual = dplyr::case_when(
            !(seguranca_hidrica %in% classes_seguranca) & V003 >= 0 & V003 < 2
            ~ 3318.83 * 1.6169335,
            !(seguranca_hidrica %in% classes_seguranca) & V003 >= 2 & V003 < 3
            ~ 3487.6 * 1.6169335,
            !(seguranca_hidrica %in% classes_seguranca) & V003 >= 3 & V003 < 4
            ~ 3656.36 * 1.6169335,
            !(seguranca_hidrica %in% classes_seguranca) & V003 >= 4 & V003 < 5
            ~ 3860.22 * 1.6169335,
            !(seguranca_hidrica %in% classes_seguranca) & V003 >= 5 & V003 < 6
            ~ 4300.83 * 1.6169335,
            !(seguranca_hidrica %in% classes_seguranca) & V003 >= 6 & V003 < 7
            ~ 4641.6 * 1.6169335,
            !(seguranca_hidrica %in% classes_seguranca) & V003 >= 7 & V003 < 8
            ~ 4977.54 * 1.6169335,
            !(seguranca_hidrica %in% classes_seguranca) & V003 >= 8 ~ 5638.27 * 1.6169335,
            (seguranca_hidrica %in% classes_seguranca) & V003 < 5 ~ 1884.98 * 1.6169335,
            (seguranca_hidrica %in% classes_seguranca) & V003 >= 5 ~ 1988.74 * 1.6169335
        )
    )
}

#' Fração de investimentos em esgoto coletivos e individuais
#'
#' Define a fração coletiva e individual para necessidade de investimento em esgoto.
#'
#' @param tabela um `data.frame` contendo as colunas `situacao_setor`, `deficit_esgoto_relativo_rural` e `regiao`.
#' @export
#'
#' @return um `data.frame` contendo as colunas adicionais `fracao_investimento_coletivo_esgoto e `fracao_investimento_individual_esgoto`.
fracao_coletivo_individual_esgoto <- function(tabela) {
    tabela <- dplyr::mutate(
        tabela,
        fracao_investimento_coletivo_esgoto = dplyr::case_when(
            # PS. foi utilizado somente 1 fator para agua e esgoto
            situacao_setor == 7 & deficit_agua_relativo_rural < 0.7 ~ 0.1,
            situacao_setor == 8 & deficit_agua_relativo_rural < 0.8 ~ 0.1,
            situacao_setor == 5 & deficit_agua_relativo_rural < 0.6 ~ 0.25,
            situacao_setor == 6 & deficit_agua_relativo_rural < 0.6 ~ 0.25,
            situacao_setor == 7 & deficit_agua_relativo_rural >= 0.7 ~ 0.25,
            situacao_setor == 8 & deficit_agua_relativo_rural >= 0.8 ~ 0.25,
            situacao_setor == 4 & deficit_agua_relativo_rural < 0.1 & regiao == "Sudeste" ~ 0.25,
            situacao_setor == 5 & deficit_agua_relativo_rural >= 0.6 ~ 0.4,
            situacao_setor == 6 & deficit_agua_relativo_rural >= 0.6 ~ 0.4,
            situacao_setor == 4 & deficit_agua_relativo_rural >= 0.1 & regiao == "Sudeste" ~ 1,
            situacao_setor == 4 & regiao != "Sudeste" ~ 1
        ),
        fracao_investimento_individual_esgoto =
            1 - fracao_investimento_coletivo_esgoto
    )
}

#' Número de domícilios com deficit de esgoto
#'
#' Calcula o número de domícilios com déficit de sistemas de estação de esgoto.
#'
#' @param tabela um `data.frame` contendo as colunas `V001_projecao` e `deficit_esgoto_relativo_rural`.
#' @export
#'
#' @return um `data.frame` contendo a coluna adicional `domicilios_deficit_esgoto`.
domicilios_com_deficit_esgoto <- function(tabela) {
    tabela <- dplyr::mutate(
        tabela,
        domicilios_deficit_esgoto = V001_projecao * deficit_esgoto_relativo_rural,
    )
}

#' Número de domícilios com demanda adequada de esgoto
#'
#' Calcula o número de domícilios com déficit de sistemas de estação de esgoto.
#'
#' @param tabela um `data.frame` contendo as colunas `V001_projecao` e `deficit_esgoto_relativo_rural`.
#' @export
#'
#' @return um `data.frame` contendo a coluna adicional `domicilios_deficit_esgoto`.
domicilios_adequados_com_esgoto <- function(tabela) {
    tabela <- dplyr::mutate(
        tabela,
        domicilios_adequados_esgoto = V001_projecao * (1.0 - deficit_esgoto_relativo_rural),
    )
}

#' Custo coletivos para sistemas de coleta de esgoto
#'
#' Adiciona coluna com custo coletivos para sistemas de coleta de esgoto.
#'
#' @param tabela um `data.frame` contendo as colunas `seguranca_hidrica` e `semiarido`.
#' @export
#'
#' @return um `data.frame` contendo a coluna adicional `custo_agua_individual`.
custo_coleta_esgoto <- function(tabela, coleta_esgoto) {
    manter <- c("custo_esgoto_coleta_rural", "classe_densidade", "regiao")
    coleta_esgoto <- dplyr::select(coleta_esgoto, dplyr::all_of(manter))
    juntar_por <- c("regiao", "classe_densidade")
    tabela <- dplyr::left_join(tabela, coleta_esgoto, by = juntar_por)
}

#' Custo coletivos para sistemas de tratamento de esgoto
#'
#' Adiciona coluna com custo coletivos para sistemas de tratamento de esgoto.
#'
#' @param tabela um `data.frame` contendo as colunas `seguranca_hidrica` e `semiarido`.
#' @export
#'
#' @return um `data.frame` contendo a coluna adicional `custo_agua_individual`.
custo_tratamento_esgoto <- function(tabela, custo_tratamento) {
    manter <- c("custo_esgoto_tratamento_rural", "classe_densidade", "regiao")
    custo_tratamento <- dplyr::select(custo_tratamento, dplyr::all_of(manter))
    juntar_por <- c("regiao", "classe_densidade")
    tabela <- dplyr::left_join(tabela, custo_tratamento, by = juntar_por)
}

#' Necessidade de investimento rural para sistema de estação de esgoto
#'
#' Calcula a necessidade de investimento para expansão do sistema de estação de esgoto.
#'
#' @param tabela um `data.frame` contendo as colunas:
#' `custo_esgoto_individual`, `custo_esgoto_producao_rural`, `custo_esgoto_distribuicao_rural`,
#' `fracao_investimento_individual_esgoto`, `fracao_investimento_coletivo_esgoto` e
#' `domicilios_deficit_esgoto`
#' @export
#'
#' @return um `data.frame` contendo as colunas adicionais:
#' investimento_esgoto_rural_individual, investimento_esgoto_rural_producao, investimento_esgoto_rural_distribuicao e investimento_esgoto_rural_expansao
investimento_rural_esgoto <- function(tabela) {
    tabela <- dplyr::mutate(
        tabela,
        investimento_esgoto_rural_individual = custo_esgoto_individual *
            fracao_investimento_individual_esgoto * domicilios_deficit_esgoto,
        investimento_esgoto_rural_tratamento = custo_esgoto_tratamento_rural *
            fracao_investimento_coletivo_esgoto * domicilios_deficit_esgoto,
        investimento_esgoto_rural_coleta = custo_esgoto_coleta_rural *
            fracao_investimento_coletivo_esgoto * domicilios_deficit_esgoto,
        investimento_esgoto_rural_expansao = investimento_esgoto_rural_individual +
            investimento_esgoto_rural_tratamento + investimento_esgoto_rural_coleta,
        investimento_esgoto_rural_coletivo = 0.0,
        # investimento_esgoto_rural_coletivo = investimento_esgoto_rural_tratamento + investimento_esgoto_rural_coleta,
        capacidade_instalada_esgoto_rural = domicilios_adequados_esgoto *
            (custo_esgoto_tratamento_rural + custo_esgoto_coleta_rural)
    )
}

#' Déficit rural esgoto PNAD
#'
#' Adiciona o déficit rural para sistema de abastecimento de esgoto (PNAD).
#'
#' @param tabela um `data.frame` contendo a coluna `estado`
#' @param deficit um `data.frame` contendo a coluna `estado` e `deficit_esgoto_relativo_rural`
#' @export
#'
#' @return um `data.frame` contendo a coluna adicional `taxa_crescimento`.
adiciona_deficit_rural_esgoto <- function(tabela, deficit) {
    manter <- c("estado", "deficit_esgoto_relativo_rural")
    deficit <- dplyr::select(deficit, dplyr::all_of(manter))
    tabela <- dplyr::left_join(tabela, deficit, by = "estado")
}

rodar_modulo_rural_esgoto <- function(input, taxas_projecao) {
    ano <- input$geral$ano
    ano_censo <- 2010

    data("agua_esgoto_rural", package = "rsan")
    agua_esgoto_rural <- get("agua_esgoto_rural")
    seguranca_hidrica <- agua_esgoto_rural$seguranca_hidrica
    custo_coleta <- agua_esgoto_rural$custo_coleta
    custo_tratamento <- agua_esgoto_rural$custo_tratamento

    setores_rurais <- c(4:8)
    tabela <- agua_esgoto_rural$censo
    tabela <- rsan:::filtra_setores_rurais(tabela, setores_rurais)
    tabela <- rsan:::codigo_setor_para_municipio(tabela)
    tabela <- rsan:::adiciona_taxa_crescimento(tabela, taxas_projecao)
    tabela <- rsan:::fazer_projecao_domicilio(tabela, ano_censo, ano)
    tabela <- rsan:::densidade_setor(tabela)
    tabela <- rsan:::classifica_densidade_setor(tabela)
    tabela <- rsan:::classifica_deficit_setor(tabela)
    tabela <- rsan:::adiciona_estado(tabela)
    tabela <- rsan:::adiciona_regiao(tabela)
    tabela <- rsan:::adiciona_deficit_rural_agua( # Usado para classificação coletivo_individual
        tabela, agua_esgoto_rural$deficit_pnad
    )
    tabela <- adiciona_deficit_rural_esgoto(
        tabela, agua_esgoto_rural$deficit_pnad
    )
    tabela <- rsan:::adiciona_seguranca_hidrica(tabela, seguranca_hidrica)
    tabela <- fracao_coletivo_individual_esgoto(tabela)

    tabela <- custo_individual_esgoto(tabela)
    tabela <- custo_coleta_esgoto(tabela, custo_coleta)
    tabela <- custo_tratamento_esgoto(tabela, custo_tratamento)

    tabela <- domicilios_com_deficit_esgoto(tabela)
    tabela <- domicilios_adequados_com_esgoto(tabela)
    tabela <- investimento_rural_esgoto(tabela)
    tabela <- rsan::calcula_reposicao_parcial(
        tabela,
        "capacidade_instalada_esgoto_rural",
        "investimento_esgoto_rural_coletivo",
        "investimento_esgoto_rural_reposicao",
        2021,
        ano,
        2022,
        30
    )
    return(tabela)
}
