#' Corrige CSV baixado do SNIS
#' 1. Converte para UTF-8
#' 2. Remove separador (;) no final da linha
#' 3. Remove a linha de TOTAL
#' @param filename
#' @param newfilename
#'
#' @return
#' @export
#'
#' @examples
#' fix_snis_csv("snis.csv", "snis_corrigido.csv")
fix_snis_csv <- function(filename, newfilename) {
  conn <- file(file_path, open = "r", encoding = "UTF-16LE")
  connout <- file(newfilename, open = "w")
  linn <- readLines(conn)
  for (i in 1:length(linn)) {
    if (grepl("^TOTAL", linn[i])) {
      next
    }
    writeLines(gsub(";$", "", linn[i]), con = connout)
  }
  close(conn)
  close(connout)
}

#' Repara nomes das colunas do SNIS
#' deixa somente os códigos
#'
#' @param data
#'
#' @return Um tibble(). Contendo os dados do SNIS
#' @export
#'
#' @examples
fix_snis_colnames <- function(data) {
  names <- colnames(data)
  new_names <- c()
  for (name in names) {
    new_name <- stringr::str_trim(strsplit(name, "-")[[1]][1])
    new_names <- c(new_names, new_name)
  }
  new_names[match(new_names, "Código do IBGE")] <-
    "codigo_municipio"
  colnames(data) <- new_names
  data <-
    dplyr::mutate(data, dplyr::across(codigo_municipio, \(x) as.character(x)))
  return(data)
}

# SNIS - Série histórica AE+RS consolidado por municipio -----------------------
file_path <-
  file.path("data-raw", "SNISConsolidadoMunicipio2020.csv")
file_path_out <-
  file.path("data-raw", "SNISConsolidadoMunicipio2020_corrigido.csv")
fix_snis_csv(file_path, file_path_out)
snis2020 <- readr::read_csv2(file_path_out)
snis2020 <- fix_snis_colnames(snis2020)
usethis::use_data(snis2020, overwrite = TRUE)

file_path <-
  file.path("data-raw", "SNISConsolidadoMunicipio2021.csv")
file_path_out <-
  file.path("data-raw", "SNISConsolidadoMunicipio2021_corrigido.csv")
fix_snis_csv(file_path, file_path_out)
snis2021 <- readr::read_csv2(file_path_out)
snis2021 <- fix_snis_colnames(snis2021)
usethis::use_data(snis2021, overwrite = TRUE)

file_path <-
  file.path("data-raw", "SNISConsolidadoMunicipio2022.csv")
file_path_out <-
  file.path("data-raw", "SNISConsolidadoMunicipio2022_corrigido.csv")
fix_snis_csv(file_path, file_path_out)
snis2022 <- readr::read_csv2(file_path_out)
snis2022 <- fix_snis_colnames(snis2022)
usethis::use_data(snis2022, overwrite = TRUE)
