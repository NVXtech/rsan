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
read_planilha_informacoes <- function(year, dir){
  filenames <- c(
    "Tabela de informações_AP%s.xlsx"
  )
  skips <- c(11)
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

download_snis_ap <- function(){
  current_year <- as.integer(format(Sys.Date(), format = "%Y"))
  years <- 2020:current_year
  snis_ap <- list()
  for (year in years){
    download_url <- paste0(snis_base_url, sprintf("ap/%s/Planilhas_AP%s.zip", year, year))
    destfile <- tempfile(fileext=".zip")
    tmp_dir <- tempfile()
    curl::curl_download(download_url, destfile)
    if (file.exists(destfile) ){
      try(files_zip <- utils::unzip(destfile, exdir = tmp_dir, unzip="unzip"))
      xls_fname <- dir(path=tmp_dir, pattern=".*informa.*")
      if (length(xls_fname) == 0){
        rlog::log_warn(sprintf("Could not extract snis ap for %s", year))
        next
      }
      list_name <- paste0("ano", year)
      snis_ap[[list_name]] <-  readxl::read_xlsx(file.path(tmp_dir, xls_fname), skip=11)
      colnames(snis_ap[[list_name]])[1:5] <- c("codigo_municipio", "municipio", "estado", "regiao", "capital")
      snis_ap[[list_name]]$codigo_municipio <- as.character(snis_ap[[list_name]]$codigo_municipio)
      unlink(tmp_dir)
    } else {
      rlog::log_warn(sprintf("Error downloading SNIS for %s", year))
    }
    unlink(destfile)
  }
  return(snis_ap)
}

snis_ap <- download_snis_ap()
usethis::use_data(snis_ap, overwrite = TRUE)
