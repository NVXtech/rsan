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
  path <- file.path(getwd(),"dados")
  if (!dir.exists(path)){
    dir.create(path)
  }
  subdirs <- c("brutos", "processados", "resultados")
  for (subdir in subdirs){
    subpath <- file.path(path, subdir)
    if (!dir.exists(subpath)){
      dir.create(subpath)
    }
  }
  return(path)
}

raw_data_dir <- function() {
  return(file.path(workspace_dir(), "brutos"))
}


populacao_municipio <- function(tabela){
  tabela <- dplyr::select(tabela, codigo_municipio, municipio, situacao, populacao)
  tabela <- dplyr::mutate(tabela, situacao = dplyr::if_else(is.na(situacao), "Rural", situacao))
  tabela <- dplyr::group_by(tabela, codigo_municipio, municipio, situacao)
  tabela <- dplyr::summarise(tabela, populacao = sum(populacao, na.rm=TRUE))
  tabela <- dplyr::ungroup(tabela)
  tabela <- tidyr::pivot_wider(tabela, names_from = situacao, values_from = populacao)
  tabela <- dplyr::mutate(tabela, Rural = dplyr::if_else(is.na(Rural), 0, Rural))
  tabela <- dplyr::mutate(tabela, Urbana = dplyr::if_else(is.na(Urbana), 0, Urbana))
  tabela <- dplyr::mutate(tabela, populacao_total = Rural + Urbana, codigo_municipio = as.character(codigo_municipio))
  names(tabela) <- c("codigo_municipio", "municipio", "populacao_rural", "populacao_urbana", "populacao_total")
  return(tabela)
}

censo_2022 <- function() {
  url <- "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios/Agregados_por_Setor_xlsx/Agregados_por_setores_basico_BR.zip"
  dest <- file.path(raw_data_dir(), "ibge", "censo", "censo_2022.xlsx")
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
  tabela <- readxl::read_xlsx(dest)

  tabela <- dplyr::select(tabela, CD_MUN, NM_MUN, SITUACAO, v0001)
  labels <- c("codigo_municipio", "municipio", "situacao", "populacao")
  names(tabela) <- labels
  tabela <- populacao_municipio(tabela)
  return(tabela)
}



#' Cria conjunto de dados do censo IBGE
#'
#' @param year ano que deseja baixar
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
  rlog::log_info("Carregando estimativa populacao IBGE 2010")
  data("populacao_estimada_2021", package = "rsan")
  estimativa2021 <- get("populacao_estimada")

  rlog::log_info("Carregando censo IBGE 2010")
  data("populacao_censo_2010", package = "rsan")
  censo2010 <- get("df_censo")
  rlog::log_info("Carregando censo IBGE 2022")
  censo2022 <- censo_2022()

  rlog::log_info("juntando estimativas populacional")
  ibge_populacao <- list(
    censo_2010 = censo2010,
    censo_2022 = censo2022,
    estimativa_2021 = estimativa2021
  )
  save(ibge_populacao, file = rsan::get_data_path(ibge_tag))
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
  files <- list.files(rsan::get_data_dir())
  for (file in files) {
    if (grepl("^populacao.*", file)) {
      base::unlink(file.path(rsan::get_data_dir(), file))
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
  pop <- rsan::load_data("populacao")
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
  return(names(rsan::load_data(ibge_tag)))
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
  output <- names(rsan::load_data(ibge_tag))
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
  rsan::load_data(ibge_tag)[[id]]
}

#' Atualiza dados do censo IBGE
#'
#' @param year é um `number` com o ano desejado
#'
#' @return um `logical` dizendo se a atualização ocorreu com sucesso
#' @export
update_ibge_censo <- function(ano) {
  id <- paste0("censo_", ano)
  ibge_populacao <- rsan::load_data(ibge_tag)
  try({
    ibge_populacao[[id]] <- download_censo_raw(ano)
    save(ibge_populacao, file = rsan::get_data_path(ibge_tag))
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
  ibge_populacao <- rsan::load_data(ibge_tag)
  try({
    ibge_populacao[[id]] <- download_estimativa_populacao(ano)
    save(ibge_populacao, file = rsan::get_data_path(ibge_tag))
  })
  return(!is.null(ibge_populacao[[id]]))
}
