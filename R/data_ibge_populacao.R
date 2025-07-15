#
# Funções auxiliarias para criar os datasets do IBGE
#

# CONSTANTS --------------------------------------------------------------------

ibge_tag <- "ibge_populacao"

# FTP dados de população

# Censo
censo_url <- "ftp.ibge.gov.br/Censos/"
censo_suffix <- "Censo_Demografico_%s/resultados/"
censo_file <- "^total_populacao_.*.zip"

# Estimativas de população
estimativa_pop_url <- "ftp.ibge.gov.br/Estimativas_de_Populacao/"
estimativa_pop_suffix <- "Estimativas_%s/"
estimativa_pop_file <- "estimativa_dou_%s.xls"

# FUNÇÕES ----------------------------------------------------------------------

#' Lista os arquivos e pastas de um FTP
#'
#' @param url local (URL) que deseja a lista de arquivo
#'
#' @return list of files and folders
#' @export
#'
#' @examples
#' list_files_from_ftp("ftp.ibge.gov.br/Censos/")
list_files_from_ftp <- function(url) {
  list_files <- curl::new_handle()
  curl::handle_setopt(list_files, ftp_use_epsv = TRUE, dirlistonly = TRUE)
  con <- curl::curl(
    url = paste0("ftp://", url),
    "r",
    handle = list_files
  )
  files <- readLines(con)
  close(con)
  return(files)
}


#' Lista de anos da população estimada pelo Censo
#' que estão disponíveis para download no ftp do IBGE
#'
#' @return lista de anos do censo disponíveis
#' @export
#'
#' @examples
#' get_censo_years()
get_censo_years <- function() {
  files <- list_files_from_ftp(censo_url)
  output <- c()
  for (file in files) {
    if (grepl("^Censo_Demografico.*", file)) {
      only_number <- gsub("Censo_Demografico_", "", file)
      year <- strtoi(only_number)
      if (year >= 2010) {
        output <- c(output, year)
      }
    }
  }
  return(output)
}

#' Retorna o diretório de trabalho onde os dados serão armazenados
#' @return The directory where datasets are stored
#' @export
#'
#' @examples
#' wd <- workspace_dir()
workspace_dir <- function() {
  path <- file.path(getwd(), "dados")
  if (!dir.exists(path)) {
    dir.create(path)
  }
  subdirs <- c("brutos", "processados", "resultados")
  for (subdir in subdirs) {
    subpath <- file.path(path, subdir)
    if (!dir.exists(subpath)) {
      dir.create(subpath)
    }
  }
  return(path)
}


raw_data_dir <- function() {
  return(file.path(workspace_dir(), "brutos"))
}

#' Agrega dados de população por município
#'
#' @param tabela é um `data.frame` com os dados agregados por setor censitário da população
#'
#' @return um `data.frame` com os dados agregados por município
#'
#' @examples
#' populacao_agrega_municipio(tabela)
populacao_agrega_municipio <- function(tabela) {
  tabela <- dplyr::select(tabela, codigo_municipio, municipio, situacao, populacao, area_km2, domicilios, atendimento_agua, atendimento_esgoto, atendimento_coleta_lixo)
  tabela <- dplyr::filter(tabela, !is.na(situacao) & situacao != "")
  tabela <- dplyr::group_by(tabela, codigo_municipio, municipio, situacao)
  tabela <- dplyr::summarise(tabela,
    populacao = sum(populacao, na.rm = TRUE),
    area_km2 = sum(area_km2, na.rm = TRUE),
    domicilios = sum(domicilios, na.rm = TRUE),
    atendimento_agua = round(sum(atendimento_agua, na.rm = TRUE)),
    atendimento_esgoto = round(sum(atendimento_esgoto, na.rm = TRUE)),
    atendimento_coleta_lixo = round(sum(atendimento_coleta_lixo, na.rm = TRUE))
  )
  tabela <- dplyr::ungroup(tabela)
  tabela <- tidyr::pivot_wider(tabela, names_from = situacao, values_from = c(populacao, area_km2, domicilios, atendimento_agua, atendimento_esgoto, atendimento_coleta_lixo), values_fill = 0)
  # convert all col names to lowercase
  names(tabela) <- tolower(names(tabela))
  tabela <- dplyr::mutate(tabela,
    populacao_rural = dplyr::if_else(is.na(populacao_rural), 0, populacao_rural),
    populacao_urbana = dplyr::if_else(is.na(populacao_urbana), 0, populacao_urbana),
  )
  tabela <- dplyr::mutate(tabela,
    codigo_municipio = as.character(codigo_municipio),
    populacao_total = populacao_rural + populacao_urbana,
    domicilios_total = domicilios_rural + domicilios_urbana,
    area_km2_total = area_km2_rural + area_km2_urbana,
    atendimento_tot_agua_hab = atendimento_agua_rural + atendimento_agua_urbana,
    atendimento_tot_esgoto_hab = atendimento_esgoto_rural + atendimento_esgoto_urbana,
    atendimento_coleta_indiferenciada_hab = atendimento_coleta_lixo_rural + atendimento_coleta_lixo_urbana,
  )
  tabela <- dplyr::rename(tabela,
    atendimento_urb_agua_hab = atendimento_agua_urbana,
    atendimento_rur_agua_hab = atendimento_agua_rural,
    atendimento_urb_esgoto_hab = atendimento_esgoto_urbana,
    atendimento_rur_esgoto_hab = atendimento_esgoto_rural,
  )
  return(tabela)
}

#' Cria indicador de cobertura de rede de água
#' indicador = tem_rede /populacao
#'
#' @param tabela é um `data.frame` com os dados agregados por setor censitário da população
#'
#' @return um `data.frame` com os dados agregados por município e o indicador de cobertura de rede de água
#' @export
indicador_cobertura_rede_agua <- function(tabela) {
  tabela <- dplyr::select(tabela, codigo_municipio, populacao, tem_rede)
  tabela <- dplyr::group_by(tabela, codigo_municipio)
  tabela <- dplyr::summarise(tabela,
    populacao = sum(populacao, na.rm = TRUE),
    tem_rede = sum(tem_rede, na.rm = TRUE)
  )
  tabela <- dplyr::ungroup(tabela)
  tabela <- dplyr::mutate(tabela,
    indicador_cobertura_rede_agua = dplyr::if_else(tem_rede / populacao >= 0.99, 1, 0),
    indicador_cobertura_rede_agua = dplyr::if_else(is.na(indicador_cobertura_rede_agua), 0, indicador_cobertura_rede_agua),
  )
  tabela <- dplyr::select(tabela, codigo_municipio, indicador_cobertura_rede_agua)
  return(tabela)
}

# ´ Cria conjunto de indicadores domícilos IBGE 2022
#'
#' @return um `data.frame` com o índice de atendimento de água, esgoto e resíduos sólidos
censo_2022_atendimento <- function() {
  url <- "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios/Agregados_por_Setor_csv/Agregados_por_setores_caracteristicas_domicilio3_BR_20250417.zip"
  dest <- file.path(raw_data_dir(), "ibge", "censo", "censo_2022_domicilios3.csv")
  if (!file.exists(dest)) {
    if (!dir.exists(dirname(dest))) {
      dir.create(dirname(dest), recursive = TRUE)
    }
    tmp_file <- tempfile(fileext = ".zip")
    curl::curl_download(url, tmp_file)
    lista <- utils::unzip(tmp_file, list = TRUE)
    utils::unzip(tmp_file, exdir = dirname(dest))
    rlog::log_info(sprintf("Renaming %s", lista[1]))
    file.rename(file.path(dirname(dest), lista[1]), dest)
  }
  tabela <- data.table::fread(dest, dec = ",", integer64 = "double", na.strings = "X")
  tabela <- dplyr::select(tabela, setor, V00508, V00509, V00510, V00511, V00513, V00540, V00580, V00581, V00582, V00612, V00613, V00636, V00637)
  tabela <- dplyr::mutate_if(tabela, is.character, as.double)
  tabela <- dplyr::mutate_all(tabela, \(x) dplyr::if_else(is.na(x), 0, x))
  tabela <- dplyr::mutate(tabela,
    CD_setor = setor,
    atendimento_agua = V00508 + V00509 + V00510 + V00511 + V00513,
    atendimento_encanada = V00540,
    tem_rede = V00508 + V00636,
    atendimento_esgoto = V00580 + V00581 + V00582,
    atendimento_coleta_lixo = V00612 + V00613
  )
  tabela <- dplyr::mutate(tabela, atendimento_agua = pmin(atendimento_agua, atendimento_encanada, na.rm = TRUE))
  tabela <- dplyr::select(tabela, c(CD_setor, atendimento_agua, atendimento_esgoto, atendimento_coleta_lixo, tem_rede))
  return(tabela)
}

adiciona_atendimento <- function(tabela) {
  atendimento <- censo_2022_atendimento()
  tabela <- dplyr::left_join(tabela, atendimento, by = "CD_setor")
  return(tabela)
}

# ´ Cria conjunto de dados populacionais do censo IBGE 2022
#'
#' @return um `data.frame` com a estimativa populacional pelo Censo
#' @export
#'
#' @examples
#' censo_2022()
censo_2022 <- function() {
  url <- "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios/Agregados_por_Setor_csv/Agregados_por_setores_basico_BR_20250417.zip"
  dest <- file.path(raw_data_dir(), "ibge", "censo", "censo_2022_basico.csv")
  if (!file.exists(dest)) {
    if (!dir.exists(dirname(dest))) {
      dir.create(dirname(dest), recursive = TRUE)
    }
    tmp_file <- tempfile(fileext = ".zip")
    curl::curl_download(url, tmp_file)
    lista <- utils::unzip(tmp_file, list = TRUE)
    utils::unzip(tmp_file, exdir = dirname(dest))
    rlog::log_info(sprintf("Renaming %s", lista[1]))
    file.rename(file.path(dirname(dest), lista[1]), dest)
  }
  tabela <- data.table::fread(dest, dec = ",", integer64 = "double", na.strings = "X")
  tabela <- dplyr::select(tabela, CD_SETOR, CD_SIT, CD_MUN, NM_MUN, SITUACAO, v0001, AREA_KM2, v0007)
  tabela <- dplyr::rename(tabela,
    CD_setor = CD_SETOR,
    codigo_municipio = CD_MUN,
    municipio = NM_MUN,
    situacao = SITUACAO,
    codigo_situacao = CD_SIT,
    populacao = v0001,
    area_km2 = AREA_KM2,
    domicilios = v0007,
  )
  tabela <- dplyr::mutate(tabela,
    densidade = populacao / area_km2,
    morador_per_domicilio = populacao / domicilios,
  )
  tabela <- adiciona_atendimento(tabela)
  return(tabela)
}


#' Cria conjunto de dados do censo IBGE 2022
#'
#' Exporta dados do censo 2022 para CSV e ser utilizado como base de calculo
#' @export
preprocess_censo2022_data <- function() {
  tabela <- censo_2022()
  readr::write_excel_csv2(
    tabela,
    file.path(dir_base_calculo, "censo_2022_setor.csv"),
    quote = "needed",
    append = FALSE
  )
  por_municipio <- populacao_agrega_municipio(tabela)
  readr::write_excel_csv2(
    por_municipio,
    file.path(dir_base_calculo, "censo_2022.csv"),
    quote = "needed",
    append = FALSE
  )
  cobertura <- indicador_cobertura_rede_agua(tabela)
  readr::write_excel_csv2(
    cobertura,
    file.path(dir_base_calculo, "censo_2022_cobertura_rede_agua.csv"),
    quote = "needed",
    append = FALSE
  )
  return(NULL)
}


#' Carrega dados de cobertura de rede de água do censo IBGE 2022
#' @return um `data.frame` com a cobertura de rede de água por município
#' @export
carrega_cobertura_rede <- function() {
  path <- file.path(dir_base_calculo, "agua_cobertura_rede.csv")
  tabela <- readr::read_delim(
    path,
    delim = ";",
    locale = readr::locale(decimal_mark = ",", grouping_mark = "."),
    col_types = list(
      codigo_municipio = readr::col_character(),
      indicador_cobertura_rede_agua = readr::col_integer()
    )
  )

  return(tabela)
}

#' Carrega dados do censo IBGE 2022 agregado por municipio
#'
#' @return um `data.frame` com a estimativa populacional pelo Censo
#' @export
carrega_censo2022 <- function() {
  path <- file.path(dir_base_calculo, "censo_2022.csv")
  rlog::log_info(sprintf("Carregando dados do censo 2022 de %s", path))
  col_types <- "cccccccccccccccccccc"
  tabela <- readr::read_csv2(path, col_types = col_types)
  integer_cols <- c(
    "populacao_rural",
    "populacao_urbana",
    "domicilios_rural",
    "domicilios_urbana",
    "atendimento_rur_agua_hab",
    "atendimento_urb_agua_hab",
    "atendimento_rur_esgoto_hab",
    "atendimento_urb_esgoto_hab",
    "atendimento_coleta_lixo_rural",
    "atendimento_coleta_lixo_urbana",
    "populacao_total",
    "domicilios_total",
    "atendimento_tot_agua_hab",
    "atendimento_tot_esgoto_hab",
    "atendimento_coleta_indiferenciada_hab"
  )
  double_cols <- c(
    "area_km2_rural",
    "area_km2_urbana",
    "area_km2_total"
  )
  for (col in double_cols) {
    tabela[[col]] <- as.double(sub(",", ".", tabela[[col]]))
  }
  for (col in integer_cols) {
    tabela[[col]] <- as.integer(tabela[[col]])
  }
  return(tabela)
}

#' Carrega dados censo IBGE 2022 agregado por setor
#'
#' @return um `data.frame` com a estimativa populacional pelo Censo por setor censitário
#' @export
carrega_censo2022_setor <- function() {
  path <- file.path(dir_base_calculo, "censo_2022_setor.csv")
  rlog::log_info(sprintf("Carregando dados do censo 2022 de %s", path))
  tabela <- readr::read_delim(
    path,
    delim = ";",
    locale = readr::locale(decimal_mark = ",", grouping_mark = "."),
    col_types = list(
      CD_setor = readr::col_character(),
      codigo_municipio = readr::col_character(),
      municipio = readr::col_character(),
      situacao = readr::col_character(),
      codigo_situacao = readr::col_integer(),
      area_km2 = readr::col_double(),
      densidade = readr::col_double(),
      morador_per_domicilio = readr::col_double(),
      populacao = readr::col_integer(),
      domicilios = readr::col_integer(),
      atendimento_agua = readr::col_integer(),
      atendimento_esgoto = readr::col_integer(),
      atendimento_coleta_lixo = readr::col_integer()
    )
  )
  return(tabela)
}

#' Adiciona dados de atendimento do censo IBGE 2022
#'
#' @param tabela é um `data.frame` com no minimo a coluna codigo_municipio
#' @param componente é um `character` com o componente desejado (agua, esgoto ou residuos)
#'
#' @export
adiciona_atendimento_censo_2022 <- function(tabela, componente) {
  df <- carrega_censo2022()
  readr::write_csv2(df, "atendimento.csv")
  if (componente == "agua") {
    cols <- c("codigo_municipio", "atendimento_tot_agua_hab", "atendimento_urb_agua_hab", "atendimento_rur_agua_hab")
  } else if (componente == "esgoto") {
    cols <- c("codigo_municipio", "atendimento_tot_esgoto_hab", "atendimento_urb_esgoto_hab", "atendimento_rur_esgoto_hab")
  } else if (componente == "residuos") {
    cols <- c("codigo_municipio", "atendimento_coleta_indiferenciada_hab")
  } else {
    stop(sprintf("Componente %s não reconhecido", componente))
  }
  cols_to_remove <- names(tabela)[names(tabela) %in% cols[-1]]
  tabela <- dplyr::select(tabela, -dplyr::all_of(cols_to_remove))
  atendimento <- dplyr::select(df, dplyr::all_of(cols))
  tabela <- dplyr::left_join(tabela, atendimento, by = "codigo_municipio")
  return(tabela)
}

#' Cria conjunto de dados do censo IBGE
#'
#' @param year ano que deseja baixa`1
#'
#' @return um `data.frame` com a estimativa populacional pelo Censo
#' @export
#'
#' @examples
#' download_censo_raw()
download_censo_raw <- function(year) {
  if (year < 2010) {
    rlog::log_error("Anos menores que 2010 não possuem formato compatível.")
    return()
  }
  if (year == 2022) {
    return(censo_2022())
  }
  col_names <- c(
    "codigo_municipio",
    "municipio",
    "populacao_total_anterior",
    "populacao_masculina",
    "populacao_feminina",
    "populacao_urbana",
    "populacao_rural",
    "populacao_total"
  )
  col_types <- c(
    "numeric",
    "text",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric"
  )

  url <- paste0(censo_url, sprintf(censo_suffix, year))
  files <- list_files_from_ftp(url)
  files <- files[grepl(censo_file, files)]
  df_censo <- dplyr::data_frame()

  for (file in files) {
    url_to_download <- paste0(url, file)
    destfile <- tempfile()
    tmp_dir <- tempdir()
    curl::curl_download(url_to_download, destfile)
    utils::unzip(destfile, exdir = tmp_dir)
    base::unlink(destfile)
    xls_filename <- file.path(
      tmp_dir,
      gsub("\\.zip$", ".xls", file)
    )
    df <- readxl::read_xls(
      xls_filename,
      skip = 1,
      col_names = col_names,
      col_types = col_types
    )
    df_censo <- dplyr::bind_rows(df_censo, df)
    base::unlink(xls_filename)
  }
  # Remove NA (rodape do xls)
  df_censo <- df_censo[complete.cases(df_censo), ]
  # transforma codigo_municipio to char
  df_censo <- transform(
    df_censo,
    codigo_municipio = as.character(codigo_municipio)
  )
  return(df_censo)
}


#' Lista de anos disponíveis de estimativas de população por amostra do IBGE
#'
#' @return Lista de anos disponíveis
#' @export
#'
#' @examples
#' load_estimativa_years()
load_estimativa_years <- function() {
  files <- list_files_from_ftp(estimativa_pop_url)
  output <- c()
  for (file in files) {
    if (grepl("^Estimativas_.*", file)) {
      only_number <- gsub("Estimativas_", "", file)
      year <- strtoi(only_number)
      if (year >= 2019) {
        output <- c(output, year)
      }
    }
  }
  return(output)
}

#' Cria conjunto de dados de estimativas populacionais por município do IBGE
#'
#' @param year ano que deseja baixar
#'
#' @return um `data.frame` com a estimativa populacional
#' @export
#'
#' @examples
#' download_estimativa_populacao()
download_estimativa_populacao <- function(year) {
  col_names <-
    c("UF", "codigo_UF", "codigo", "municipio", "populacao_str")
  col_types <- c("text", "text", "text", "text", "text")
  ibge_populacao <- list()

  rlog::log_info(sprintf("baixando estimativa população %s", year))
  url <- paste0(
    estimativa_pop_url,
    estimativa_pop_suffix,
    estimativa_pop_file
  )
  url <- sprintf(url, year, year)
  destfile <- tempfile()
  rlog::log_info(sprintf("url %s", url))
  curl::curl_download(url, destfile)
  if (file.info(destfile)$size <= 0) {
    rlog::log_warn(sprintf("could not download file for %s", year))
    return(NULL)
  }
  populacao_estimada <- readxl::read_xls(
    destfile,
    skip = 2,
    sheet = 2,
    col_names = col_names,
    col_types = col_types
  )
  base::unlink(destfile)
  # Arruma populacoes com notas de rodapé e remove separador de milhar
  populacao_estimada$populacao_total <- as.numeric(
    gsub("\\.", "", gsub("\\([^)]*\\)", "", populacao_estimada$populacao_str))
  )
  # Remove NA (rodape do xls)
  populacao_estimada <- populacao_estimada[
    complete.cases(populacao_estimada),
  ]
  # Arruma Codigo Municipio
  populacao_estimada$codigo_municipio <- paste0(
    populacao_estimada$codigo_UF,
    populacao_estimada$codigo
  )
  populacao_estimada <- subset(
    populacao_estimada,
    select = -c(codigo, populacao_str)
  )
  return(populacao_estimada)
}

#' Cria armazenamento local de dados populacionais
#'
#' @return nada
#' @export
#'
#' @examples
#' \dontrun{
#' create_populacao()
#' }
create_populacao <- function() {
  data("ibge_populacao", package = "rsan")
  save(ibge_populacao, file = get_data_path(ibge_tag))
  rlog::log_info("Populacao criada")
}

#' Limpa armazenamento de dados populacionais
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' clean_populacao()
#' }
clean_populacao <- function() {
  files <- list.files(get_data_dir())
  for (file in files) {
    if (grepl("^populacao.*", file)) {
      base::unlink(file.path(get_data_dir(), file))
    }
  }
}

#' Verifica a integridade do armazenamento local de dados de população
#'
#' @return boleano indicando integridade ok ou não
#' @export
#'
#' @examples
#' integrity_populacao()
integrity_populacao <- function() {
  pop <- load_data("populacao")
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

#' Retorna os rótulos dos dados do IBGE disponíveis
#'
#' @return um vetor com os dados disponiveis
#' @export
#'
#' @examples
#' \dontrun{
#' df <- get_sinapi_data(sinapi)
#' }
get_populacao_labels <- function() {
  return(names(load_data(ibge_tag)))
}

#' Retorna os rótulos dos dados do IBGE disponíveis (somente censo)
#'
#' @return um vetor com os dados disponiveis
#' @export
#'
#' @examples
#' \dontrun{
#' df <- get_sinapi_data(sinapi)
#' }
get_censo_labels <- function() {
  output <- names(load_data(ibge_tag))
  return(output[grepl("censo.*", output)])
}

#' Transforma id IBGE em nome legível
#'
#' @return um `character` com o nome
#' @export
ibge_id_to_name <- function(id) {
  split <- strsplit(id, "_")
  name <- split[[1]][1]
  year <- split[[1]][2]
  first <- toupper(substr(name, 1, 1))
  remainder <- substr(name, 2, nchar(id))
  name <- paste0(first, remainder)
  return(paste(year, name, sep = " - "))
}

#' Carrega conjunto de dados do IBGE
#
#' @param id um `character` com o id
#'
#' @return o conjunto de dados do SINAPI
#' @export
load_ibge <- function(id) {
  load_data(ibge_tag)[[id]]
}

#' Atualiza dados do censo IBGE
#'
#' @param year é um `number` com o ano desejado
#'
#' @return um `logical` dizendo se a atualização ocorreu com sucesso
#' @export
update_ibge_censo <- function(ano) {
  id <- paste0("censo_", ano)
  ibge_populacao <- load_data(ibge_tag)
  try({
    ibge_populacao[[id]] <- download_censo_raw(ano)
    save(ibge_populacao, file = get_data_path(ibge_tag))
  })
  return(!is.null(ibge_populacao[[id]]))
}

#' Atualiza dados de estimativa populacional IBGE
#'
#' @param year é um `number` com o ano desejado
#'
#' @return um `logical` dizendo se a atualização ocorreu com sucesso
#' @export
update_ibge_estimativa <- function(ano) {
  id <- paste0("estimativa_", ano)
  ibge_populacao <- load_data(ibge_tag)
  try({
    ibge_populacao[[id]] <- download_estimativa_populacao(ano)
    save(ibge_populacao, file = get_data_path(ibge_tag))
  })
  return(!is.null(ibge_populacao[[id]]))
}
