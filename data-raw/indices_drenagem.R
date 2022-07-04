file_path <- file.path("data-raw", "indices_drenagem.xlsx")

indices_drenagem <- readxl::read_xlsx(file_path)
indices_drenagem$codigo_municipio <- as.character(
    indices_drenagem$codigo_municipio
)
usethis::use_data(indices_drenagem, overwrite = TRUE)
