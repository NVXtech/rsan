download_all_snis_ap <- function(since_year = 2017) {
  current_year <- as.integer(format(Sys.Date(), format = "%Y"))
  years <- since_year:current_year
  snis_ap <- load_data("snis_ap")
  for (year in years) {
      print(year)
      list_name <- paste0("ano", year)
      try(snis_ap[[list_name]] <- download_snis_ap(year))
  }
  return(snis_ap)
}

snis_ap <- download_all_snis_ap()
usethis::use_data(snis_ap, overwrite = TRUE)
