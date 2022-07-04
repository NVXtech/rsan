# SNIS - Dados brutos por prestador
snis_base_url <- "http://www.snis.gov.br/downloads/diagnosticos/"
segmentos <- c(
  "ae", # Água e esgoto
  "ap", # Águas pluviais
  "rs" # Resíduos sólidos
)

# SNIS-RS Resíduos sólidos -----------------------------------------------------
# Os dados anteriores a 2020 serão carregados via planilha
# Assume-se que a partir de 2020 vão manter o mesmo padrão de dados
# e assim a função download snis vai tentar baixar todos dados a partir de 2020
read_planilha_informacoes <- function(year, dir) {
  filenames <- c(
    "Tabela de informações_AP%s.xlsx"
  )
  skips <- c(11)
  for (i in 1:length(skips)) {
    file_name <- file.path(dir, sprintf(filenames[i], year))
    tabela <- readxl::read_xlsx(file_name, skip = skips[i])
    if (i == 1) {
      unidades <- tabela
    } else {
      unidades <- dplyr::full_join(unidades, tabela)
    }
  }
  return(unidades)
}

download_snis_ap <- function(since_year = 2017) {
  current_year <- as.integer(format(Sys.Date(), format = "%Y"))
  years <- since_year:current_year
  snis_ap <- list()
  for (year in years) {
    download_url <- paste0(
      snis_base_url,
      sprintf("ap/%s/Planilhas_AP%s.zip", year, year)
    )
    temp_zipfile <- tempfile(fileext = ".zip")
    tmp_dir <- tempdir()
    curl::curl_download(download_url, temp_zipfile)
    if (file.exists(temp_zipfile)) {
      try(
        files_zip <- utils::unzip(
          temp_zipfile,
          exdir = tmp_dir, unzip = "unzip"
        )
      )
      xls_fname <- dir(path = tmp_dir, pattern = ".*informa.*")
      if (length(xls_fname) == 0) {
        rlog::log_warn(sprintf("Could not extract snis ap for %s", year))
        next
      }
      file.rename(file.path(tmp_dir, xls_fname), file.path(tmp_dir, "planilha"))
      list_name <- paste0("ano", year)
      snis_ap[[list_name]] <- readxl::read_excel(
        file.path(tmp_dir, "planilha"),
        skip = 11
      )
      colnames(snis_ap[[list_name]])[1:5] <- c(
        "codigo_municipio", "municipio", "estado", "regiao", "capital"
      )
      snis_ap[[list_name]]$codigo_municipio <- as.character(
        snis_ap[[list_name]]$codigo_municipio
      )
      unlink(tmp_dir)
    } else {
      rlog::log_warn(sprintf("Error downloading SNIS for %s", year))
    }
    unlink(temp_zipfile)
  }
  return(snis_ap)
}

snis_ap <- download_snis_ap()
usethis::use_data(snis_ap, overwrite = TRUE)
