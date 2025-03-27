file_path <- file.path("data-raw", "drenagem_municipios_criticos.xlsx")

drenagem_municipios_criticos <- readxl::read_xlsx(file_path)
usethis::use_data(drenagem_municipios_criticos, overwrite = TRUE)
