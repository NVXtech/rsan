#' Salva todos os arquivos de dados/base_calculo em um único arquivo .rda em data/
#'
#' Esta função lê todos os arquivos CSV do diretório 'dados/base_calculo/' e salva um arquivo RDA chamado 'base_calculo.rda' no diretório 'data/'. Cada arquivo será um data.frame em uma lista nomeada pelo nome do arquivo (sem extensão).
#' @export
save_base_calculo_data <- function() {
  base_dir <- file.path("dados", "base_calculo")
  files <- list.files(base_dir, pattern = "\\.csv$", full.names = TRUE)
  base_calculo <- list()
  for (f in files) {
    name <- tools::file_path_sans_ext(basename(f))
    base_calculo[[name]] <- readr::read_delim(
      f,
      delim = ";",
      locale = readr::locale(encoding = "UTF-8", decimal_mark = ",", grouping_mark = ".", date_names = "pt"),
      show_col_types = FALSE
    )
  }
  save(base_calculo, file = file.path("data", "base_calculo.rda"))
  return(NULL)
}

#' Carrega a base_calculo salva no pacote
#'
#' Esta função carrega o arquivo 'base_calculo.rda' do diretório 'data/' e retorna uma lista de data.frames.
#' @return Lista nomeada de data.frames, cada um correspondente a um arquivo CSV original de dados/base_calculo.
#' @export
load_base_calculo_data <- function(output_dir = file.path("dados", "base_calculo"), overwrite = FALSE) {
  env <- new.env()
  data("base_calculo", package = "rsan", envir = env)
  base_calculo <- env$base_calculo
  if (!dir.exists(output_dir)) {
    rlog::log_info(sprintf("Creating output directory: %s", output_dir))
    dir.create(output_dir, recursive = TRUE)
  }
  for (name in names(base_calculo)) {
    out_path <- file.path(output_dir, paste0(name, ".csv"))
    if (!file.exists(out_path) || overwrite) {
      readr::write_csv2(base_calculo[[name]], out_path)
    } else {
      rlog::log_info(sprintf("File %s already exists, skipping.", out_path))
    }
  }
  return(NULL)
}
