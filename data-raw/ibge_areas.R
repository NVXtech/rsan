areas_url <-
  "geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/areas_territoriais/"


get_anos_ibge_area <- function() {
  listFilesFromFTP(areas_url)
}

get_ultimo_ano_ibge_area <- function() {
  anos <- listFilesFromFTP(areas_url)
  max(anos)
}

load_ultimo_ibge_area <- function() {
  col_names <- c("id", "codigo_uf", "estado", "estado_sigla", "codigo_municipio", "municipio", "area")
  col_types <- c("numeric", "text", "text", "text", "text", "text", "numeric")
  ano <- get_ultimo_ano_ibge_area()
  files <- listFilesFromFTP(paste0(areas_url, ano, "/"))
  for (file in files) {
    if (grepl(".*xls", file)) {
      url_to_download <- paste0(areas_url, ano, "/", file)
      sheet <- sprintf("AR_BR_MUN_%s", ano)
      destfile <- tempfile()
      curl::curl_download(url_to_download, destfile)
      area_municipio <- readxl::read_xls(
        destfile,
        sheet = sheet,
        skip = 1,
        col_names = col_names,
        col_types = col_types
      )
      unlink(destfile)
      area_municipio <- area_municipio[stats::complete.cases(area_municipio), ]
      area_municipio <- dplyr::select(area_municipio, codigo_municipio, area)
      usethis::use_data(area_municipio, overwrite = TRUE)
    }
  }
}

load_area_urbana <- function() {
  col_names <- c("codigo_municipio", "area_urbana")
  col_types <- c("text", "numeric")
  file_name <- file.path("data-raw", "area_urbana_2015.xlsx")
  area_urbana_municipio <- readxl::read_xlsx(
    file_name,
    sheet = "AreaUrbana",
    skip = 1,
    col_names = col_names,
    col_types = col_types
  )
  usethis::use_data(area_urbana_municipio, overwrite = TRUE)
}

load_ultimo_ibge_area()
load_area_urbana()
