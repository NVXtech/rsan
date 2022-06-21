file_path <- file.path("data-raw", "CPRM_precipitacao.xlsx")

pluviometria <- readxl::read_xlsx(file_path)
pluviometria$codigo_municipio <- as.character(pluviometria$codigo_municipio)
usethis::use_data(pluviometria, overwrite=TRUE)
