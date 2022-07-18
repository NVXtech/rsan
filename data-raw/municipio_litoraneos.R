file_path <- file.path("data-raw", "municipio_litoraneos.xlsx")

municipio_litoraneos <- readxl::read_xlsx(file_path, col_types = c("text", "text"))
usethis::use_data(municipio_litoraneos, overwrite = TRUE)
