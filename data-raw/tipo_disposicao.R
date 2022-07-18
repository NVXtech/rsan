file_path <- file.path("data-raw", "tipo_disposicao.xlsx")

tipo_disposicao <- readxl::read_xlsx(file_path, col_types = c("text", "text"))
usethis::use_data(tipo_disposicao, overwrite = TRUE)
