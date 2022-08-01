#' Necessidade de Investimento - Componente Água - Situação Rural

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
            situacao_setor == 7 & deficit_agua_relativo_rural >= 0.7 ~ 0.25,
            situacao_setor == 8 & deficit_agua_relativo_rural < 0.8 ~ 0.1,
            situacao_setor == 8 & deficit_agua_relativo_rural >= 0.8 ~ 0.25,
            situacao_setor == 5 & deficit_agua_relativo_rural < 0.6 ~ 0.25,
            situacao_setor == 5 & deficit_agua_relativo_rural >= 0.6 ~ 0.4,
            situacao_setor == 6 & deficit_agua_relativo_rural < 0.6 ~ 0.25,
            situacao_setor == 6 & deficit_agua_relativo_rural >= 0.6 ~ 0.4,
            situacao_setor == 4 & regiao != "Sudeste" ~ 1,
            situacao_setor == 4 & deficit_agua_relativo_rural < 0.1 & regiao == "Sudeste" ~ 0.25,
            situacao_setor == 4 & deficit_agua_relativo_rural >= 0.1 & regiao == "Sudeste" ~ 1
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

#' Número de habitantes com deficit de água
#'
#' Calcula o número de habitantes com déficit de sistemas de abastecimento de água.
#'
#' @param tabela um `data.frame` contendo as colunas `V001`, `V001_projecao` e `deficit_agua_relativo_rural`.
#' @export
#'
#' @return um `data.frame` contendo a coluna adicional `domicilios_deficit_agua`.
habitantes_com_deficit_agua <- function(tabela) {
    tabela <- dplyr::mutate(
        tabela,
        deficit_rural = domicilios_deficit_agua * V003,
    )
}

#' Número de domicílios sistemas de água adequados
#'
#' Calcula o número de domicílios com sistemas de abastecimento de água adequados.
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
        investimento_agua_rural_coletivo = 0.0,
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
filtra_setores_rurais <- function(tabela, setores) {
    tabela <- dplyr::filter(tabela, situacao_setor %in% setores)
}


#' Faz projeção de domicílios
#'
#' Projeta o número de domícilio pela projeção populacional e considera que a relação domicílio/habitantes se mantém constante.
#'
#' @param tabela um `data.frame` contendo a coluna `codigo_municipio`
#' @param ano_inicial um `number` contendo o ano do conjunto de dados de população (V002)
#' @param ano_final um `number` contendo o ano da projeção
#' @param atualizar_moradores_por_domicilio um `boolean` dizendo se existe uma atualização da relação domicílio/habitantes (V003). O padrão é `FALSE`.
#'
#' @export
#'
#' @return um `data.frame` contendo a coluna adicional `taxa_crescimento`.
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
#'  \item{etapa}
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
    tabela <- rsan:::somar_por_campo(tabela, "estado")
    tabela <- tidyr::pivot_longer(
        tabela,
        cols = starts_with("investimento_"),
        names_to = c("destino", "etapa"),
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
#'  \item{etapa}
#'  \item{deficit}
#' }
#'
#' @export
tbl_longa_deficit_agua_rural <- function(tabela) {
    colunas <- c("estado", "regiao", "deficit_rural")
    tabela <- dplyr::select(tabela, dplyr::all_of(colunas))
    tabela <- rsan:::somar_por_campo(tabela, "estado")
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
        etapa = "producao_distribuicao",
        deficit = as.integer(deficit)
    )
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
    ano <- input$geral$ano
    param <- input$agua # parametros
    ano_censo <- 2010
    rlog::log_info("água:rural: carregando dados")
    data("agua_esgoto_rural", package = "rsan")
    agua_esgoto_rural <- get("agua_esgoto_rural")
    seguranca_hidrica <- agua_esgoto_rural$seguranca_hidrica
    custo_producao <- agua_esgoto_rural$custo_producao
    custo_distribuicao <- agua_esgoto_rural$custo_distribuicao

    rlog::log_info("água:rural: calculando setores")
    setores_rurais <- c(4:8)
    tabela <- agua_esgoto_rural$censo
    tabela <- filtra_setores_rurais(tabela, setores_rurais)
    tabela <- codigo_setor_para_municipio(tabela)
    tabela <- adiciona_taxa_crescimento(tabela, taxas_projecao)
    rlog::log_info("água:rural: projecao de domicilios")
    tabela <- fazer_projecao_domicilio(tabela, ano_censo, ano)
    tabela <- densidade_setor(tabela)
    rlog::log_info("água:rural: classificando setores")
    tabela <- classifica_densidade_setor(tabela)
    tabela <- classifica_deficit_setor(tabela)
    rlog::log_info("água:rural: adicionando novos campos")
    tabela <- rsan:::adiciona_estado(tabela)
    tabela <- rsan:::adiciona_regiao(tabela)
    tabela <- rsan:::adiciona_deficit_rural_agua(
        tabela, agua_esgoto_rural$deficit_pnad
    )
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

    tabela <- rsan::calcula_reposicao_parcial(
        tabela,
        "capacidade_instalada_distribuicao_agua",
        "investimento_expansao_distribuicao_agua",
        "investimento_reposicao_distribuicao_agua",
        2021,
        ano,
        input$geral$ano_corrente,
        input$agua$vida_util
    )

    tabela <- domicilios_com_deficit_agua(tabela)
    tabela <- domicilios_adequados_com_agua(tabela)
    tabela <- investimento_rural_agua(tabela)
    tabela <- rsan::calcula_reposicao_parcial(
        tabela,
        "capacidade_instalada_producao_agua",
        "investimento_expansao_producao_agua",
        "investimento_reposicao_producao_agua",
        2021,
        ano,
        input$geral$ano_corrente,
        input$agua$vida_util
    )
    rlog::log_info("água:rural: consolidando necessidade")
    tabela <- consolida_investimentos_rural_agua(tabela)

    state$agua_rural <- rsan:::adiciona_pais(tabela)

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
