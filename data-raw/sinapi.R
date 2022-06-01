file_path <- file.path("data-raw", "SINAPI_202112.xlsx")

sinapi202112 <- readxl::read_xlsx(file_path)
usethis::use_data(sinapi202112, overwrite=TRUE)
