#' Lê arquivo SNIS-RS unidades
#'
#' @param ano é um `character` com o ano desejado
#' @param dir é um `character` com o diretorio do arquivo de unidades
#'
#' @return um `data.frame` contendo os dados de snis para unidades
#' @export
read_planilha_unidades <- function(year, dir) {
  filenames <- c(
    "Planilha_Unidades_Fluxos_RS_%s.xlsx",
    "Planilha_Unidades_Lixoes_Aterros_RS_%s.xlsx"
  )
  skips <- c(11, 11)
  for (i in 1:length(skips)) {
    if (year == 2021) {
      file_name <- file.path(dir, sprintf("Planilhas_RS%s", year), sprintf(filenames[i], year))
    } else {
      file_name <- file.path(dir, sprintf(filenames[i], year))
    }
    tabela <- readxl::read_xlsx(file_name, skip = skips[i])
    if (i == 1) {
      if (year == 2022) {
        names(tabela)[names(tabela) == "-...1"] <- "Código"

        names(tabela)[names(tabela) == "-...3"] <- "Nome"
        names(tabela)[names(tabela) == "-...4"] <- "UF"
      }
      tabela <- dplyr::select(tabela, -c(2, 5, 6, 7))
      unidades <- tabela
    } else {
      if (year == 2022) {
        names(tabela)[names(tabela) == "-...1"] <- "Código"
        names(tabela)[names(tabela) == "-...3"] <- "Nome"
        names(tabela)[names(tabela) == "-...4"] <- "UF"
        names(tabela)[names(tabela) == "-...8"] <- "Ano"
      }
      tabela <- dplyr::select(tabela, -c(2, 5, 6, 7))
      unidades <- dplyr::full_join(unidades, tabela)
    }
  }
  unidades <- dplyr::mutate(unidades, Código = as.character(Código), Ano = as.character(Ano))
  return(unidades)
}

#' Retorna dados do SNIS-RS
#'
#' @param year ano desejado
#'
#' @return um `data.frame` contendo os dados do SNIS
#' @export
download_snis_rs <- function(year) {
  if (year == 2022) {
    download_url <- "https://www.gov.br/cidades/pt-br/acesso-a-informacao/acoes-e-programas/saneamento/snis/produtos-do-snis/diagnosticos/Planilha_RS_2022_atualizado_29112024.zip"
  } else {
    snis_base_url <- "https://www.gov.br/cidades/pt-br/acesso-a-informacao/acoes-e-programas/saneamento/snis/produtos-do-snis/diagnosticos/"
    download_url <- paste0(
      snis_base_url, sprintf("Planilhas_RS%s.zip", year)
    )
  }
  snis_rs <- list()
  destfile <- tempfile(fileext = ".zip")
  tmp_dir <- tempdir(check = TRUE)
  curl::curl_download(download_url, destfile)
  if (file.exists(destfile)) {
    try(files_zip <- utils::unzip(destfile, exdir = tmp_dir))
    if (is.null(files_zip)) {
      rlog::log_warn(sprintf("Error extracting SNIS zip for %s", year))
      return(NULL)
    }
    snis_rs <- read_planilha_unidades(year, tmp_dir)
    unlink(files_zip)
  } else {
    rlog::log_warn(sprintf("Error downloading SNIS for %s", year))
  }
  unlink(destfile)
  return(snis_rs)
}


#' Atualiza dados do SNIS-RS
#'
#' @param ano é um `number` com o ano desejado
#'
#' @return um `logical` dizendo se a atualização ocorreu com sucesso
#' @export
update_snis_rs <- function(ano) {
  id <- paste0("ano", ano)
  snis_rs <- load_data("snis_rs")
  try({
    snis_rs[[id]] <- download_snis_rs(ano)
    save(snis_rs, file = get_data_path("snis_rs"))
  })
  return(!is.null(snis_rs[[id]]))
}

#' Carrega conjunto de dados do SNIS-RS
#
#' @param id um `character` com o id
#'
#' @return o conjunto de dados do SNIS
#' @export
load_snis_rs <- function(id) {
  load_data("snis_rs")[[id]]
}

#' Cria armazenamento local dos dados do SNIS-RS
#'
#' @return NULL
#' @export
create_snis_rs <- function() {
  data("snis_rs", package = "rsan")
  snis_rs <- get("snis_rs")
  save(snis_rs, file = get_data_path("snis_rs"))
}

#' Verifica a integridade dos dados do SNIS-AP
#'
#' @return um `logical` sendo `TRUE` integridade OK.
#' @export
integrity_snis_rs <- function() {
  check_data_exists("snis_rs")
}
