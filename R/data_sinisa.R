# Base Path for Raw SNISA Data
base_path <- file.path("dados", "brutos", "sinisa")
# Base Path for Processed SNISA Data
base_path_processed <- file.path("dados", "processados")

#' Download SINISA data
#'
#' @param year Year of the data to download (e.g., 2022)
#'
#' @return Path to the downloaded files
#' @export
download_sinisa <- function(year) {
  path <- file.path(base_path, year)
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  base_url <- "https://www.gov.br/cidades/pt-br/acesso-a-informacao/"
  folders <- "acoes-e-programas/saneamento/sinisa/resultados-sinisa/"
  url <- paste0(base_url, folders)
  files_to_download <- c(
    sprintf("SINISA_AGUA_Planilhas_%s.zip", year),
    sprintf("SINISA_ESGOTO_Planilhas_%s.zip", year),
    sprintf("SINISA_RESIDUOS_Solidos_Planilhas_%s.rar", year),
    sprintf("SINISA_PLUVIAIS_%s.zip", year)
  )
  for (file in files_to_download) {
    file_url <- paste0(url, file)
    rlog::log_warn(sprintf("Downloading %s", file_url))
    # if file is zip save to temp file and unzip it
    if (tools::file_ext(file) == "zip") {
      temp_file <- tempfile(fileext = ".zip")
      curl::curl_download(file_url, temp_file)
      unzip(temp_file, exdir = path)
      file.remove(temp_file)
    } else {
      curl::curl_download(file_url, file.path(path, file))
    }
  }
}

#' Agua Pre-processing
#' Read the SINISA water data and process it into a data frame.
#'
#' @param year Year of the data to process (e.g., 2022)
#'
#' @return A data frame containing the processed water data
#' @export
agua_preprocess <- function(year) {
  path <- file.path(base_path, year)
  folder <- "Água - Base Municipal"
  file_name <- sprintf("SINISA_AGUA_Informacoes_Gestao Tecnica Agua_Base Municipal_%s.xlsx", year)
  file_path <- file.path(path, folder, file_name)
  df <- readxl::read_excel(file_path, sheet = 1, skip = 9)
  df <- dplyr::select(df, c("cod_IBGE", "GTA0001", "GTA0002", "GTA1001", "GTA1102", "GTA1211"))
  df <- dplyr::rename(
    df,
    codigo_municipio = "cod_IBGE",
    atendimento_urb_agua_hab = "GTA0001",
    atendimento_rural_agua_hab = "GTA0002",
    extensao_rede_agua_km = "GTA1102",
    volume_agua_produzido_dam3_ano = "GTA1001",
    volume_agua_consumido_dam3_ano = "GTA1211"
  )
  df <- dplyr::mutate(df,
    codigo_municipio = as.character(codigo_municipio),
    atendimento_tot_agua_hab = atendimento_urb_agua_hab + atendimento_rural_agua_hab,
  )
  return(df)
}

#' Esgoto Pre-processing
#' Read the SINISA sewage data and process it into a data frame.
#'
#' @param year Year of the data to process (e.g., 2022)
#'
#' @return A data frame containing the processed sewage data
#' @export
esgoto_preprocess <- function(year) {
  path <- file.path(base_path, year)
  folder <- "Esgoto - Base Municipal"
  file_name <- sprintf("SINISA_ESGOTO_Informacoes_Gestao Tecnica Esgoto_Base Municipal_%s.xlsx", year)
  file_path <- file.path(path, folder, file_name)
  df <- readxl::read_excel(file_path, sheet = 1, skip = 10)
  df <- dplyr::select(
    df,
    c("cod_IBGE", "GTE0001", "GTE0002", "GTE1001", "GTE1014")
  )
  df <- dplyr::rename(
    df,
    codigo_municipio = "cod_IBGE",
    atendimento_urb_esgoto_hab = "GTE0001",
    atendimento_rural_esgoto_hab = "GTE0002",
    extensao_rede_esgoto_km = "GTE1001",
    volume_esgoto_tratado_dam3_ano = "GTE1014",
  )
  df <- dplyr::mutate(df,
    codigo_municipio = as.character(codigo_municipio),
    atendimento_tot_esgoto_hab = atendimento_urb_esgoto_hab + atendimento_rural_esgoto_hab,
  )
  return(df)
}

#' Residuos Pre-processing Cobertura
#' Read the SINISA waste coverage data and process it into a data frame.
#'
#' @param year Year of the data to process (e.g., 2022)
#'
#' @return A data frame containing the processed waste coverage date
residuos_cobertura_preprocess <- function(year) {
  path <- file.path(base_path, year)
  folder <- sprintf("SINISA_RESIDUOS_Planilhas_%s", year) # Folder where user unrar the rar file

  fname_cobertura <- sprintf("SINISA_RESIDUOS_Informacoes_Formulario_Cobertura_%s.xlsx", year)
  df <- readxl::read_excel(file.path(path, folder, fname_cobertura), sheet = 1, skip = 12)
  df <- dplyr::select(df, c("Cod_IBGE", "GTR0201*", "GTR0204*"))
  df <- dplyr::rename(
    df,
    codigo_municipio = "Cod_IBGE",
    atendimento_coleta_indiferenciada_hab = "GTR0201*",
    atendimento_coleta_seletiva_hab = "GTR0204*",
  )
  return(df)
}

#' Residuos Pre-processing Destinação Final
#' Read the SINISA waste final destination data and process it into a data frame.
#'
#' @param year Year of the data to process (e.g., 2022)
#'
#' @return A data frame containing the processed waste final destination data
residuos_destinacao_preprocess <- function(year) {
  path <- file.path(base_path, year)
  folder <- sprintf("SINISA_RESIDUOS_Planilhas_%s", year) # Folder where user unrar the rar file
  fname_final <- sprintf("SINISA_RESIDUOS_Informacoes_Formulario_Infraestrutura_Destinacao_Final_%s.xlsx", year)
  df <- readxl::read_excel(file.path(path, folder, fname_final), sheet = 1, skip = 12)
  df <- dplyr::select(df, c("Cod_IBGE", "GTR3226*"))
  df <- dplyr::rename(
    df,
    codigo_municipio = "Cod_IBGE",
    GTR3226 = "GTR3226*",
  )
  # remove all non-numeric characters from GTR3226
  df <- dplyr::mutate(df, GTR3226 = gsub("[^0-9]", "", GTR3226))
  # group by codigo_municipio and concatenate GTR3226
  df <- dplyr::group_by(df, codigo_municipio)
  df <- dplyr::summarise(df, GTR3226 = paste(GTR3226, collapse = " "))
  df <- dplyr::ungroup(df)
  df <- dplyr::mutate(df, tem_pesagem = ifelse(grepl("[2-7]", GTR3226), "Sim", "Não"))
  df <- dplyr::select(df, c("codigo_municipio", "tem_pesagem"))
  return(df)
}

#' Residuos Pre-processing Manejo
#' Read the SINISA waste management data and process it into a data frame.
#'
#' @param year Year of the data to process (e.g., 2022)
#'
#' @return A data frame containing the processed waste management data
residuos_manejo_preprocess <- function(year) {
  path <- file.path(base_path, year)
  folder <- sprintf("SINISA_RESIDUOS_Planilhas_%s", year) # Folder where user unrar the rar file
  fname_manejo <- sprintf("SINISA_RESIDUOS_Informacoes_Formulario_Manejo_%s.xlsx", year)
  df_manejo <- readxl::read_excel(file.path(path, folder, fname_manejo), sheet = "Manejo_Resíduos_Sólidos_Urbanos", skip = 12)
  df_manejo <- dplyr::select(df_manejo, c("Cod_IBGE", "GTR1028", "GTR1029"))
  df_manejo <- dplyr::rename(
    df_manejo,
    codigo_municipio = "Cod_IBGE",
    residuo_coletado_ton_ano = "GTR1028",
    residuo_recuperado_ton_ano = "GTR1029",
  )
  df_seletiva <- readxl::read_excel(file.path(path, folder, fname_manejo), sheet = "Manejo_Coleta_e_Destinação", skip = 12)
  df_seletiva <- dplyr::select(df_seletiva, c("Cod_IBGE", "GTR1001*"))
  df_seletiva <- dplyr::rename(
    df_seletiva,
    codigo_municipio = "Cod_IBGE",
    GTR1001 = "GTR1001*",
  )
  df_seletiva <- dplyr::group_by(df_seletiva, codigo_municipio)
  df_seletiva <- dplyr::summarise(df_seletiva, GTR1001 = paste(GTR1001, collapse = " "))
  df_seletiva <- dplyr::ungroup(df_seletiva)
  df_seletiva <- dplyr::mutate(df_seletiva, tem_coleta_seletiva = ifelse(grepl("seletiva", GTR1001, ignore.case = TRUE), "Sim", "Não"))
  df_seletiva <- dplyr::select(df_seletiva, c("codigo_municipio", "tem_coleta_seletiva"))

  df_veiculos <- readxl::read_excel(file.path(path, folder, fname_manejo), sheet = "Manejo_Veículos", skip = 12)
  df_veiculos <- dplyr::select(df_veiculos, c("Cod_IBGE", "GTR1201*", "GTR1204*"))
  df_veiculos <- dplyr::rename(
    df_veiculos,
    codigo_municipio = "Cod_IBGE",
    GTR1201 = "GTR1201*",
    GTR1204 = "GTR1204*"
  )
  df_veiculos <- dplyr::filter(
    df_veiculos,
    (GTR1201 == "Caminhão basculante, baú ou carroceria" | GTR1201 == "Caminhão compactador")
  )
  df_veiculos <- dplyr::mutate(df_veiculos, GTR1204 = as.numeric(GTR1204))
  df_veiculos <- dplyr::group_by(df_veiculos, codigo_municipio, GTR1201)
  df_veiculos <- dplyr::summarise(df_veiculos, GTR1204 = sum(GTR1204, na.rm = TRUE))
  df_veiculos <- dplyr::ungroup(df_veiculos)
  df_veiculos <- tidyr::pivot_wider(df_veiculos, names_from = GTR1201, values_from = GTR1204)
  df_veiculos <- dplyr::rename(
    df_veiculos, c(
      quantidade_caminhoes_basculantes = "Caminhão basculante, baú ou carroceria",
      quantidade_caminhoes_compactadores = "Caminhão compactador"
    )
  )
  df <- dplyr::full_join(df_manejo, df_seletiva, by = "codigo_municipio")
  df <- dplyr::full_join(df, df_veiculos, by = "codigo_municipio")
  return(df)
}

#' Residuos Pre-processing Unidades de Processamento
#' Read the SINISA waste processing units data and process it into a data frame.
#'
#' @param year Year of the data to process (e.g., 2022)
#'
#' @return A data frame containing the processed waste processing units data
residuos_unidades_preprocess <- function(year) {
  path <- file.path(base_path, year)
  folder <- sprintf("SINISA_RESIDUOS_Planilhas_%s", year) # Folder where user unrar the rar file
  fname <- sprintf("SINISA_RESIDUOS_Informacoes_Formulario_Infraestrutura_Unidades_de_Processamento_e_Tratamento_%s.xlsx", year)
  df <- readxl::read_excel(file.path(path, folder, fname), sheet = "Proces_Trat_entradas_residuos", skip = 12)
  df <- dplyr::select(df, c("GTR3120*", "GTR3101*", "Cod_IBGE"))
  df <- dplyr::rename(
    df,
    codigo_municipio = "Cod_IBGE",
    tipo_disposicao = "GTR3101*",
    quantidade = "GTR3120*"
  )
  df <- dplyr::mutate(df, residuo_compostagem_ton_ano = ifelse(grepl("compostagem", tipo_disposicao, ignore.case = TRUE), quantidade, 0))
  df <- dplyr::select(df, c("codigo_municipio", "residuo_compostagem_ton_ano"))
  df <- dplyr::group_by(df, codigo_municipio)
  df <- dplyr::summarise(df, residuo_compostagem_ton_ano = sum(residuo_compostagem_ton_ano, na.rm = TRUE))
  df <- dplyr::ungroup(df)
  return(df)
}

#' Residuos Pre-processing
#' Read the SINISA waste data and process it into a data frame.
#'
#'
#' @param year Year of the data to process (e.g., 2022)
#'
#' @return A data frame containing the processed waste data
#' @export
residuos_preprocess <- function(year) {
  df <- dplyr::full_join(residuos_cobertura_preprocess(year), residuos_destinacao_preprocess(year), by = "codigo_municipio")
  df <- dplyr::full_join(df, residuos_manejo_preprocess(year), by = "codigo_municipio")
  df <- dplyr::full_join(df, residuos_unidades_preprocess(year), by = "codigo_municipio")
  df <- dplyr::mutate(df,
    codigo_municipio = as.character(codigo_municipio),
    atendimento_coleta_indiferenciada_hab = as.numeric(atendimento_coleta_indiferenciada_hab),
    atendimento_coleta_seletiva_hab = as.numeric(atendimento_coleta_seletiva_hab),
    tem_pesagem = as.character(tem_pesagem),
    tem_coleta_seletiva = as.character(tem_coleta_seletiva),
    residuo_coletado_ton_ano = as.numeric(residuo_coletado_ton_ano),
    residuo_recuperado_ton_ano = as.numeric(residuo_recuperado_ton_ano),
    quantidade_caminhoes_basculantes = as.numeric(quantidade_caminhoes_basculantes),
    quantidade_caminhoes_compactadores = as.numeric(quantidade_caminhoes_compactadores),
    residuo_compostagem_ton_ano = as.numeric(residuo_compostagem_ton_ano)
  )
  return(df)
}

#' Aguas Pluviais Pre-processing
#' Read the SINISA rainwater data and process it into a data frame.
#'
#' @param year Year of the data to process (e.g., 2022)
#'
#' @return A data frame containing the processed rainwater data
#' @export
aguas_pluviais_preprocess <- function(year) {
  path <- file.path(base_path, year)
  file_name <- sprintf("SINISA_AGUASPLUVIAIS_Informacoes_Formularios_Tecnicos_%s.xlsx", year)
  file_path <- file.path(path, file_name)
  df <- readxl::read_excel(file_path, sheet = "Dados Operacionais", skip = 10)
  df <- dplyr::select(df, c("Cod_IBGE", "GAP0102*"))
  df <- dplyr::rename(
    df,
    codigo_municipio = "Cod_IBGE",
    tem_cadastro_tecnico = "GAP0102*"
  )
  print(df)
  df <- dplyr::mutate(df,
    codigo_municipio = as.character(codigo_municipio),
    tem_cadastro_tecnico = ifelse(tem_cadastro_tecnico == "Integral", "Sim", "Não")
  )
  return(df)
}

#' SINISA Pre-processing
#' Reads the downloaded SINISA files and preprocess them into csv files
#'
#' @param year Year of the data to process (e.g., 2022)
#'
#' @export
preprocess_sinisa_data <- function(year) {
  if (!dir.exists(base_path_processed)) {
    dir.create(base_path_processed, recursive = TRUE)
  }
  agua <- agua_preprocess(year)
  readr::write_csv(agua,
    file.path(base_path_processed, sprintf("agua_sinisa_%s.csv", year)),
    quote = "needed", append = FALSE
  )
  esgoto <- esgoto_preprocess(year)
  readr::write_csv(esgoto,
    file.path(base_path_processed, sprintf("esgoto_sinisa_%s.csv", year)),
    quote = "needed", append = FALSE
  )
  residuos <- residuos_preprocess(year)
  readr::write_csv(residuos,
    file.path(base_path_processed, sprintf("residuos_sinisa_%s.csv", year)),
    quote = "needed", append = FALSE
  )
  aguas_pluviais <- aguas_pluviais_preprocess(year)
  readr::write_csv(aguas_pluviais,
    file.path(
      base_path_processed,
      sprintf("aguas_pluviais_sinisa_%s.csv", year)
    ),
    quote = "needed", append = FALSE
  )
  return(NULL)
}

#' Carrega os dados pré-processados do SINISA
#'
#' @param componente Componente a ser carregado: "agua", "esgoto" ou "residuos"
#' @ano Ano dos dados a serem carregados (e.g., 2022)
#'
#' @return Data frame com os dados do SINISA
#' @export
carrega_dados_sinisa <- function(componente, ano) {
  if (!componente %in% c("agua", "esgoto", "residuos", "aguas_pluviais")) {
    stop("Componente deve ser 'agua', 'esgoto', 'residuos' ou 'aguas_pluviais'")
  }
  file_path <- file.path(base_path_processed, sprintf("%s_sinisa_%s.csv", componente, ano))
  if (!file.exists(file_path)) {
    rlog::log_warn(file_path)
    download_sinisa(ano)
    preprocess_sinisa_data(ano)
  }
  df <- readr::read_csv(file_path, col_types = "c")
  return(df)
}
