#' Código setor para codigo do município
#'
#' Transforma os códigos do setor censitário do IBGE para o código de município (IBGE).
#'
#' @param tabela um `data.frame` contendo a coluna `codigo_setor`
#'
#' @return um `data.frame` contendo a coluna adicional `codigo_municipio`
#' @export
codigo_setor_para_municipio <- function(tabela) {
    tabela <- dplyr::mutate(
        tabela,
        codigo_municipio = substr(codigo_setor, start = 1, stop = 7)
    )
}

#' Fração de investimentos em água coletivos e individuais
#'
#' Define a fração coletiva e individual para necessidade de investimento em água.
#'
#' @param tabela um `data.frame` contendo as colunas `situacao_setor`, `deficit_agua_relativo_rural` e `regiao`.
#' @export
#'
#' @return um `data.frame` contendo as colunas adicionais `fracao_investimento_coletivo_agua` e `fracao_investimento_individual_agua`.
fracao_coletivo_individual_agua <- function(tabela) {
    tabela <- dplyr::mutate(
        tabela,
        fracao_investimento_coletivo_agua = dplyr::case_when(
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
        fracao_investimento_individual_agua =
            1 - fracao_investimento_coletivo_agua
    )
}

#' Densidade populacional no setor censitário
#'
#' Cálcula densidade populacional no setor (V002/area_setor_km2)
#'
#' @param tabela um `data.frame` contendo as colunas `V002_projecao` e `area_setor_km2`.
#' @export
#'
#' @return um `data.frame` contendo a coluna adicional `densidade_setor`.
densidade_setor <- function(tabela) {
    tabela <- dplyr::mutate(
        tabela,
        densidade_setor = V002 / area_setor_km2
    )
}

#' Classifica densidade populacional no setor censitário
#'
#' Classifica setor censitário baseando-se na densidade populacional.
#'
#' @param tabela um `data.frame` contendo a coluna `densidade_setor`.
#' @export
#'
#' @return um `data.frame` contendo a coluna adicional `classe_densidade`.
classifica_densidade_setor <- function(tabela) {
    tabela <- dplyr::mutate(
        tabela,
        classe_densidade = dplyr::case_when(
            densidade_setor >= 0 & densidade_setor < 450 ~ "F01",
            densidade_setor >= 450 & densidade_setor < 802.5 ~ "F02",
            densidade_setor >= 802.5 & densidade_setor < 1500 ~ "F03",
            densidade_setor >= 1500 & densidade_setor < 2500 ~ "F04",
            densidade_setor >= 2500 & densidade_setor < 3500 ~ "F05",
            densidade_setor >= 3500 & densidade_setor < 4500 ~ "F06",
            densidade_setor >= 4500 & densidade_setor < 5500 ~ "F07",
            densidade_setor >= 5500 & densidade_setor < 7000 ~ "F08",
            densidade_setor >= 7000 & densidade_setor < 9000 ~ "F09",
            densidade_setor >= 9000 & densidade_setor < 15000 ~ "F10",
            densidade_setor >= 15000 & densidade_setor < 25000 ~ "F11",
            densidade_setor >= 25000 & densidade_setor < 35000 ~ "F12",
            densidade_setor >= 35000 & densidade_setor < 45000 ~ "F13",
            densidade_setor >= 45000 ~ "F14"
        )
    )
}

#' Classifica défict no setor censitário
#'
#' Classifica setor de censitário baseando-se na densidade populacional para tratamento do déficit.
#'
#' @param tabela um `data.frame` contendo a coluna `densidade_setor`.
#' @export
#'
#' @return um `data.frame` contendo a coluna adicional `classe_densidade`.
classifica_deficit_setor <- function(tabela) {
    tabela <- dplyr::mutate(
        tabela,
        classe_deficit = dplyr::case_when(
            V002 > 0 & V002 <= 2000 ~ "A",
            V002 > 2000 & V002 <= 4000 ~ "B",
            V002 > 4000 & V002 <= 10000 ~ "C",
            V002 > 10000 & V002 <= 20000 ~ "D",
            V002 > 20000 & V002 <= 34000 ~ "E",
            V002 > 34000 ~ "F"
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
custo_individual_agua <- function(tabela) {
    classes_seguranca <- c("Baixa", "Mínima")
    tabela <- dplyr::mutate(
        tabela,
        custo_agua_individual = ifelse(
            seguranca_hidrica %in% classes_seguranca & semiarido == "SIM",
            14682.16 * 1.6169335, 10682.16 * 1.6169335
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


#' Número de domícilios com deficit de água
#'
#' Calcula o número de domícilios com déficit de sistemas de abastecimento de água.
#'
#' @param tabela um `data.frame` contendo as colunas `V001`, `V001_projecao` e `deficit_agua_relativo_rural`.
#' @export
#'
#' @return um `data.frame` contendo a coluna adicional `domicilios_deficit_agua`.
domicilios_com_deficit_agua <- function(tabela) {
    tabela <- dplyr::mutate(
        tabela,
        domicilios_deficit_agua = (V001_projecao) * deficit_agua_relativo_rural,
    )
}

#' Número de domícilios sistemas de água adequados
#'
#' Calcula o número de domícilios com sistemas de abastecimento de água adequados.
#'
#' @param tabela um `data.frame` contendo as colunas `V001` e `deficit_agua_relativo_rural`.
#' @export
#'
#' @return um `data.frame` contendo a coluna adicional `domicilios_adequados_agua`.
domicilios_adequados_com_agua <- function(tabela) {
    tabela <- dplyr::mutate(
        tabela,
        domicilios_adequados_agua = (V001_projecao) * (1.0 - deficit_agua_relativo_rural)
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
        investimento_agua_rural_individual = custo_agua_individual *
            fracao_investimento_individual_agua * domicilios_deficit_agua,
        investimento_agua_rural_producao = custo_agua_producao_rural *
            fracao_investimento_coletivo_agua * domicilios_deficit_agua,
        investimento_agua_rural_distribuicao = custo_agua_distribuicao_rural *
            fracao_investimento_coletivo_agua * domicilios_deficit_agua,
        investimento_agua_rural_expansao = investimento_agua_rural_individual +
            investimento_agua_rural_producao + investimento_agua_rural_distribuicao,
        investimento_agua_rural_coletivo = 0.0,
        # investimento_agua_rural_coletivo= investimento_agua_rural_producao +
        #    investimento_agua_rural_distribuicao,
        capacidade_instalada_agua_rural = domicilios_adequados_agua *
            (custo_agua_producao_rural + custo_agua_distribuicao_rural)
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
adiciona_deficit_rural_agua <- function(tabela, deficit) {
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
    deficit <- dplyr::select(seguranca_hidrica, dplyr::all_of(manter))
    tabela <- dplyr::left_join(tabela, seguranca_hidrica, by = "codigo_municipio")
}


filtra_setores_rurais <- function(tabela, setores) {
    tabela <- dplyr::filter(tabela, situacao_setor %in% setores)
}

fazer_projecao_domicilio <- function(tabela, ano_inicial, ano_final, atualizar_moradores_por_domicilio = FALSE) {
    if (atualizar_moradores_por_domicilio) {
        tabela <- dplyr::mutate(
            tabela,
            V003 = V003_new
        )
    }
    tabela <- dplyr::mutate(
        tabela,
        V002_projecao = V002 * ((1 + taxa_de_crescimento)^(ano_final - ano_inicial)),
        V001_projecao = ifelse(V003 == 0, 0, V002_projecao / V003)
    )
}

rodar_modulo_rural_agua <- function(input, taxas_projecao) {
    ano <- input$geral$ano
    ano_censo <- 2010

    data("agua_esgoto_rural", package = "rsan")
    agua_esgoto_rural <- get("agua_esgoto_rural")
    seguranca_hidrica <- agua_esgoto_rural$seguranca_hidrica
    custo_producao <- agua_esgoto_rural$custo_producao
    custo_distribuicao <- agua_esgoto_rural$custo_distribuicao

    setores_rurais <- c(4:8)
    tabela <- agua_esgoto_rural$censo
    tabela <- filtra_setores_rurais(tabela, setores_rurais)
    tabela <- codigo_setor_para_municipio(tabela)
    tabela <- adiciona_taxa_crescimento(tabela, taxas_projecao)
    tabela <- fazer_projecao_domicilio(tabela, ano_censo, ano)
    tabela <- densidade_setor(tabela)
    tabela <- classifica_densidade_setor(tabela)
    tabela <- classifica_deficit_setor(tabela)
    tabela <- rsan:::adiciona_estado(tabela)
    tabela <- rsan:::adiciona_regiao(tabela)
    tabela <- rsan:::adiciona_deficit_rural_agua(
        tabela, agua_esgoto_rural$deficit_pnad
    )
    tabela <- adiciona_seguranca_hidrica(tabela, seguranca_hidrica)
    tabela <- fracao_coletivo_individual_agua(tabela)

    tabela <- custo_individual_agua(tabela)
    tabela <- custo_producao_agua(tabela, custo_producao)
    tabela <- custo_distribuicao_agua(tabela, custo_distribuicao)

    tabela <- domicilios_com_deficit_agua(tabela)
    tabela <- domicilios_adequados_com_agua(tabela)
    tabela <- investimento_rural_agua(tabela)
    tabela <- rsan::calcula_reposicao_parcial(
        tabela,
        "capacidade_instalada_agua_rural",
        "investimento_agua_rural_coletivo",
        "investimento_agua_rural_reposicao",
        2021,
        ano,
        2022,
        30
    )
    return(tabela)
}
