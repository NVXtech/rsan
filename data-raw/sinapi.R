file_path <- file.path("data-raw", "SINAPI_202112.xlsx")

sinapi_202112 <- readxl::read_xlsx(file_path)
usethis::use_data(sinapi_202112, overwrite = TRUE)

sinapi <- list(dt202112 = sinapi_202112)
usethis::use_data(sinapi, overwrite = TRUE)
