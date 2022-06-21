file_path <- file.path("data-raw", "investimentos_plano_drenagem.xlsx")

plano_drenagem <- readxl::read_xlsx(file_path)
plano_drenagem$codigo_municipio <- as.character(plano_drenagem$codigo_municipio)
usethis::use_data(plano_drenagem, overwrite=TRUE)
