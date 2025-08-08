#' Cria armazenamento local dos dados do SNIS consolidado
#'
#' @return NULL
#' @export
create_snis <- function() {
  data("snis2020", package = "rsan")
  df_snis2020 <- snis2020
  save(df_snis2020, file = get_data_path("snis_2020"))

  data("snis2021", package = "rsan")
  df_snis2021 <- snis2021
  save(df_snis2021, file = get_data_path("snis_2021"))

  data("snis2022", package = "rsan")
  df_snis2022 <- snis2022
  save(df_snis2022, file = get_data_path("snis_2022"))

  nome <- c("SNIS 2022", "SNIS 2021", "SNIS 2020")
  ano <- c(2022, 2021, 2020)
  caminho <- c("snis_2022", "snis_2021", "snis_2020")
  snis <- data.frame(nome, ano, caminho)
  save(snis, file = get_data_path("snis"))
}


#' Verifica a integridade dos dados do SNIS
#'
#' @return um `logical` sendo `TRUE` integridade OK.
#' @export
integrity_snis <- function() {
  pop <- load_data("snis")
  for (caminho in pop$caminho) {
    if (!file.exists(get_data_path(caminho))) {
      rlog::log_info(sprintf("%s dataset not found", caminho))
      return(FALSE)
    } else {
      rlog::log_info(sprintf("%s dataset is OK", caminho))
    }
  }
  return(TRUE)
}

#' Retorna dados do SNIS
#'
#' @param snis caminho do SNIS
#' @param fields lista com campos a serem retornados
#'
#' @return um `data.frame` contendo os dados do SNIS
#' @export
get_snis_data <- function(snis, fields) {
  df <- load_data(snis)
  df <- dplyr::select(df, dplyr::all_of(fields))
  return(df)
}

#' Prepara os dados do SNIS para o componente água
#'
#' @param df Data frame com os dados do SNIS
#'
#' @return Data frame com os dados do SNIS para o componente água
#' @export
snis_prepara_agua <- function(df) {
  snis_fields <- c(
    "codigo_municipio",
    "AG001",
    "AG005",
    "AG006",
    "AG010",
    "AG026"
  )
  df <- dplyr::select(df, snis_fields)
  df <- dplyr::mutate(df,
    codigo_municipio = as.character(codigo_municipio),
    atendimento_tot_agua_hab = AG001,
    atendimento_urb_agua_hab = AG026,
    atendimento_rural_agua_hab = AG001 - AG026,
    volume_agua_produzido_dam3_ano = AG006,
    extensao_rede_agua_km = AG005,
    volume_agua_consumido_dam3_ano = AG010,
  )
  snis_fields <- snis_fields[-1]
  df <- dplyr::select(df, -dplyr::all_of(snis_fields))
  return(df)
}

snis_prepara_esgoto <- function(df) {
  snis_fields <- c(
    "codigo_municipio",
    "ES001",
    "ES004",
    "ES006",
    "ES026"
  )
  df <- dplyr::select(df, snis_fields)
  df <- dplyr::mutate(df,
    codigo_municipio = as.character(codigo_municipio),
    atendimento_tot_esgoto_hab = ES001,
    atendimento_urb_esgoto_hab = ES026,
    atendimento_rural_esgoto_hab = ES001 - ES026,
    extensao_rede_esgoto_km = ES004,
    volume_esgoto_tratado_dam3_ano = ES006
  )
  snis_fields <- snis_fields[-1]
  df <- dplyr::select(df, -dplyr::all_of(snis_fields))
  return(df)
}

snis_prepara_aguas_pluviais <- function(df) {
  snis_fields <- c("codigo_municipio", "POP_TOT", "GE006", "IE012")
  df <- dplyr::select(df, snis_fields)
  df <- dplyr::mutate(df,
    codigo_municipio = as.character(codigo_municipio),
    tem_cadastro_tecnico = IE012
  )
  snis_fields <- snis_fields[4]
  df <- dplyr::select(df, -dplyr::all_of(snis_fields))
  return(df)
}

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
  tabela <- dplyr::select(tabela, c("Código", "UP003", "UP080"))
  tabela <- dplyr::filter(
    tabela,
    grepl("compostagem", .data[["UP003"]], fixed = TRUE)
  )
  tabela <- codigo6_para_codigo_ibge(tabela, "Código")
  tabela <- dplyr::group_by(tabela, codigo_municipio)
  tabela <- dplyr::summarise(
    tabela,
    residuo_compostagem_ton_ano = sum(UP080)
  )
  tabela <- dplyr::ungroup(tabela)
  return(tabela)
}

snis_prepara_residuos <- function(ano) {
  snis_rs <- load_snis_rs(sprintf("ano%s", ano))
  snis_rs <- quantidade_compostagem_municipio(snis_rs)
  snis <- load_data(paste("snis_", ano, sep = ""))
  residuos_snis_fields <- c(
    "codigo_municipio",
    "POP_TOT", "POP_URB",
    "CO164", "CO050", "CO119", "CO021",
    "CO054", "CO055", "CO056", "CO057", "CO058", "CO059",
    "CO063", "CO064", "CO065", "CO066", "CO067", "CO068",
    "CS001", "CS050", "CS026", "CS009"
  )
  snis <- dplyr::select(snis, dplyr::all_of(residuos_snis_fields))
  snis <- dplyr::mutate(snis,
    codigo_municipio = as.character(codigo_municipio),
    atendimento_coleta_indiferenciada_hab = CO164,
    atendimento_coleta_seletiva_hab = CS050,
    tem_pesagem = CO021,
    tem_coleta_seletiva = CS001,
    residuo_coletado_ton_ano = CO119,
    residuo_recuperado_ton_ano = CS009,
    quantidade_caminhoes_compactadores = round(CO054 + CO055 + CO056 + CO057 + CO058 + CO059),
    quantidade_caminhoes_basculantes = round(CO063 + CO064 + CO065 + CO066 + CO067 + CO068)
  )
  snis <- dplyr::full_join(snis, snis_rs, by = "codigo_municipio")
  snis <- dplyr::select(snis, -dplyr::all_of(residuos_snis_fields[4:length(residuos_snis_fields)]))
  return(snis)
}
#' Carrega os dados do SNIS
#'
#' @param ano ano do SNIS a ser carregado
#'
#' @return um `data.frame` contendo os dados do SNIS para o ano especificado
#' @export
prepara_snis <- function(ano) {
  df <- load_data(paste0("snis_", ano))
  salva_base_calculo(snis_prepara_agua(df), "agua", "snis", ano)
  salva_base_calculo(snis_prepara_esgoto(df), "esgoto", "snis", ano)
  df_ap <- load_snis_ap(paste("ano", ano, sep = ""))
  df <- dplyr::select(df, c("codigo_municipio", "POP_TOT", "POP_URB"))
  df <- dplyr::left_join(df, df_ap, by = "codigo_municipio")
  salva_base_calculo(
    snis_prepara_aguas_pluviais(df),
    "drenagem", "snis", ano
  )
  salva_base_calculo(snis_prepara_residuos(ano), "residuos", "snis", ano)
}
