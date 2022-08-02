file_path <- file.path("data-raw", "investimentos_plano_drenagem.xlsx")

plano_drenagem <- readxl::read_xlsx(file_path, sheet = 1)
plano_drenagem$codigo_municipio <- as.character(plano_drenagem$codigo_municipio)
usethis::use_data(plano_drenagem, overwrite = TRUE)

investimento_existente <- readxl::read_xlsx(file_path, sheet = 2)
investimento_existente$codigo_municipio <- as.character(investimento_existente$codigo_municipio)
usethis::use_data(investimento_existente, overwrite = TRUE)

area_urbana <- readxl::read_xlsx(file_path, sheet = 2)
area_urbana$codigo_municipio <- as.character(area_urbana$codigo_municipio)
usethis::use_data(area_urbana, overwrite = TRUE)
