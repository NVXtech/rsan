#
# Funções auxiliarias para criar os datasets do IBGE
#

# CONSTANTS --------------------------------------------------------------------

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

#' List of files/folders in a FTP folder
#'
#' @param url
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


#' Lista de anos de população estimada pelo Censo
#' que estão disponíveis para download no ftp do IBGE
#'
#' @param url
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
      output <- c(output, year)
    }
  }
  return(output)
}


#' Cria conjunto de dados do censo IBGE
#'
#' @return Dataframe com informações dos conjuntos de dados baixados
#' @export
#'
#' @examples
#' download_censo_raw()
download_censo_raw <- function() {
  years <- get_censo_years()
  # Anos anteriores a 2010 não são suportados
  years <- years[years >= 2010]
  col_names <-
    c(
      "codigo_municipio",
      "municipio",
      "populacao_total_anterior",
      "populacao_masculina",
      "populacao_feminina",
      "populacao_urbana",
      "populacao_rural",
      "populacao_total"
    )
  col_types <-
    c(
      "numeric",
      "text",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric"
    )
  nome <- c()
  ano <- c()
  caminho <- c()
  tipo <- c()
  for (year in years) {
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
    filename_out <- file.path(
      get_data_dir(),
      sprintf("populacao_censo_%s.rda", year)
    )
    save(df_censo, file = filename_out)
    ano <- c(ano, year)
    caminho <- c(caminho, sprintf("populacao_censo_%s", year))
    tipo <- c(tipo, "CENSO")
    nome <- c(nome, sprintf("%s - Censo", year))
  }
  return(data.frame(nome, tipo, ano, caminho))
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
#' @return nada
#' @export
#'
#' @examples
#' download_estimativa_populacao()
download_estimativa_populacao <- function() {
  col_names <-
    c("UF", "codigo_UF", "codigo", "municipio", "populacao_str")
  col_types <- c("text", "text", "text", "text", "text")
  nome <- c()
  ano <- c()
  caminho <- c()
  tipo <- c()
  for (year in c(2021)) {
    rlog::log_info(sprintf("baixando estimativa população %s", year))
    # for (year in load_estimativa_years()) {
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
      next
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
    filename_out <- file.path(
      get_data_dir(),
      sprintf("populacao_estimada_%s.rda", year)
    )
    rlog::log_info(sprintf("salvando em %s", filename_out))
    save(populacao_estimada, file = filename_out)
    ano <- c(ano, year)
    caminho <- c(caminho, sprintf("populacao_estimada_%s", year))
    tipo <- c(tipo, "ESTIMATIVA")
    nome <- c(nome, sprintf("%s - Estimativa", year))
  }
  return(data.frame(nome, tipo, ano, caminho))
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
  rlog::log_info("baixando estimativa populacao ibge")
  df_estimativa <- download_estimativa_populacao()
  rlog::log_info("baixando censo ibge")
  df_censo <- download_censo_raw()
  rlog::log_info("juntando estimativas")
  populacao <- rbind(df_censo, df_estimativa)
  rlog::log_info("salvando")
  save(populacao, file = file.path(rsan::get_data_dir(), "populacao.rda"))
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
