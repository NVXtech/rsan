# SNIS - Dados brutos por prestador
snis_base_url <- "http://www.snis.gov.br/downloads/diagnosticos/"
segmentos <- c(
  "ae", # Água e esgoto
  "ap", # Águas pluviais
  "rs"  # Resíduos sólidos
)

# SNIS-RS Resíduos sólidos -----------------------------------------------------
# Os dados anteriores a 2020 serão carregados via planilha
# Assume-se que a partir de 2020 vão manter o mesmo padrão de dados
# e assim a função download snis vai tentar baixar todos dados a partir de 2020
read_planilha_unidades <- function(year, dir){
  filenames <- c(
    "Planilha_Unidades_Fluxos_RS_%s.xlsx",
    "Planilha_Unidades_Lixoes_Aterros_RS_%s.xlsx"
  )
  skips <- c(11, 11)
  for (i in 1:length(skips)){
    file_name <- file.path(dir, sprintf(filenames[i], year))
    tabela <- readxl::read_xlsx(file_name, skip=skips[i])
    if (i == 1)
      unidades <- tabela
    else
      unidades <- dplyr::full_join(unidades, tabela)
  }
  return(unidades)
}

download_snis_rs <- function(){
  current_year <- as.integer(format(Sys.Date(), format = "%Y"))
  years <- 2020:current_year
  snis_rs <- list()
  for (year in years){
    download_url <- paste0(snis_base_url, sprintf("rs/%s/Planilhas_RS%s.zip", year, year))
    destfile <- tempfile(fileext=".zip")
    tmp_dir <- tempdir(check=TRUE)
    curl::curl_download(download_url, destfile)
    if (file.exists(destfile) ){
      try(files_zip <- utils::unzip(destfile, exdir = tmp_dir))
      if (is.null(files_zip)){
        rlog::log_warn(sprintf("Error extracting SNIS zip for %s", year))
        next
      }
      list_name <- paste0("ano", year)
      snis_rs[[list_name]] <- read_planilha_unidades(year, tmp_dir)
      unlink(files_zip)
    } else {
      rlog::log_warn(sprintf("Error downloading SNIS for %s", year))
    }
    unlink(destfile)
  }
  return(snis_rs)
}

snis_rs <- download_snis_rs()
usethis::use_data(snis_rs, overwrite = TRUE)
