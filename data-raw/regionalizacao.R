file_path <- file.path("data-raw", "potencial_regionalizacao.xlsx")

potencial_regionalizacao <- readxl::read_xlsx(file_path)
usethis::use_data(potencial_regionalizacao, overwrite = TRUE)
