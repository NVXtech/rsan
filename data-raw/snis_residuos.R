download_all_snis_rs <- function() {
  current_year <- as.integer(format(Sys.Date(), format = "%Y"))
  years <- 2020:current_year
  snis_rs <- list()
  for (year in years) {
      list_name <- paste0("ano", year)
      try(snis_rs[[list_name]] <- download_snis_rs(year))
  }
  return(snis_rs)
}

snis_rs <- download_all_snis_rs()
usethis::use_data(snis_rs, overwrite = TRUE)
