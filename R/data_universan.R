dir_base_calculo <- file.path("dados", "base_calculo")

#' Salva todos os arquivos de dados/base_calculo em um único arquivo .rda em data/
#'
#' Esta função lê todos os arquivos CSV do diretório 'dados/base_calculo/' e salva um arquivo RDA chamado 'base_calculo.rda' no diretório 'data/'. Cada arquivo será um data.frame em uma lista nomeada pelo nome do arquivo (sem extensão).
#'
#' @return NULL
#' @export
salva_dados_base_calculo <- function() {
  files <- list.files(dir_base_calculo, pattern = "\\.csv$", full.names = TRUE)
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

#' Verifica a integridade e cria amarzenamento de conjunto de dados
#'
#' @return NULL
#' @export
check_and_create_datasets <- function() {
  rlog::log_info("Checking and creating datasets..")
  load_base_calculo_data()
  # TODO: verificar integridade dos dados
}
#' Verifica a integridade da base de cálculo
#'
#'
#' @param df Data frame a ser validado
#' @param componente Componente da base de cálculo
#'
#' @return `TRUE` se a base de cálculo for válida, `FALSE` caso contrário
#' @export
valida_base_calculo <- function(df, componente) {
  if (componente == "agua") {
    cols <- c(
      "codigo_municipio",
      "atendimento_tot_agua_hab",
      "atendimento_urb_agua_hab",
      "atendimento_rural_agua_hab",
      "volume_agua_produzido_dam3_ano",
      "volume_agua_consumido_dam3_ano",
      "extensao_rede_agua_km"
    )
  } else if (componente == "esgoto") {
    cols <- c(
      "codigo_municipio",
      "atendimento_urb_esgoto_hab",
      "atendimento_rural_esgoto_hab",
      "atendimento_tot_esgoto_hab",
      "extensao_rede_esgoto_km",
      "volume_esgoto_tratado_dam3_ano"
    )
  } else if (componente == "residuos") {
    cols <- c(
      "codigo_municipio",
      "atendimento_coleta_indiferenciada_hab",
      "atendimento_coleta_seletiva_hab",
      "tem_pesagem",
      "residuo_coletado_ton_ano",
      "residuo_recuperado_ton_ano",
      "residuo_compostagem_ton_ano",
      "tem_coleta_seletiva",
      "quantidade_caminhoes_compactadores",
      "quantidade_caminhoes_basculantes"
    )
  } else if (componente == "drenagem") {
    cols <- c("codigo_municipio", "tem_cadastro_tecnico")
  } else {
    rlog::log_error(sprintf("Componente %s não faz parte da base de cálculo", componente))
    return(FALSE)
  }
  missing_cols <- setdiff(cols, names(df))
  rlog::log_info(sprintf("Colunas faltando: %s", paste(missing_cols, collapse = ", ")))
  return(all(cols %in% names(df)))
}

#' Salva a base de cálculo da Necessidade de Investimento em Saneamento
#'
#' @param df Data frame com os dados a serem salvos
#' @param componente Componente da base de cálculo
#' @param fonte Fonte dos dados
#' @param ano Ano da base de cálculo
#'
#' @return Nenhum valor retornado
#' @export
salva_base_calculo <- function(df, componente, fonte, ano) {
  if (!valida_base_calculo(df, componente)) {
    stop(sprintf("Base de cálculo inválida para o componente %s", componente))
  }
  if (!dir.exists(dir_base_calculo)) {
    dir.create(dir_base_calculo, recursive = TRUE)
  }
  file_name <- paste0(componente, "_", fonte, "_", ano, ".csv")
  rlog::log_info(sprintf("Base de Calculo - Salvando  %s", file_name))
  path_out <- file.path(dir_base_calculo, file_name)
  # sort by codigo_municipio
  df <- dplyr::arrange(df, codigo_municipio)
  readr::write_excel_csv2(df, path_out, quote = "needed", append = FALSE)
  rlog::log_info(sprintf("Base de Calculo salva em %s", path_out))
}

#' Carrega a base de cálculo da Necessidade de Investimento em Saneamento
#'
#' @param componente Componente da base de cálculo
#' @param fonte Fonte dos dados
#' @param ano Ano da base de cálculo
#'
#' @return Data frame com os dados da base de cálculo
#' @export
carrega_base_calculo <- function(componente, fonte, ano) {
  if (!componente %in% c("agua", "esgoto", "residuos", "drenagem")) {
    stop(sprintf("Base de cálculo inválida para o componente %s", componente))
  }
  file_name <- paste0(componente, "_", fonte, "_", ano, ".csv")
  rlog::log_info(sprintf("Base de Calculo - Carregando %s", file_name))
  df <- readr::read_csv2(
    file.path(dir_base_calculo, file_name),
    locale = readr::locale(decimal_mark = ",", grouping_mark = "."),
    col_types = "c"
  )
  rlog::log_info("Base de Calculo carregada com sucesso.")
  if (valida_base_calculo(df, componente)) {
    rlog::log_info("Base de Calculo - Validação OK")
  } else {
    rlog::log_error("Base de Calculo - Validação Falhou")
    stop("Base de Calculo - Validação Falhou")
  }
  return(df)
}

#' Carrega um dado auxiliar da pasta dados/base_calculo
#'
#' Esta função lê um arquivo CSV auxiliar localizado em 'dados/base_calculo' e retorna um data frame.
#' Caso a coluna 'codigo_municipio' exista, ela será convertida para caractere.
#'
#' @param nome Nome do arquivo (sem extensão .csv) a ser carregado.
#'
#' @return Um data frame com os dados do arquivo auxiliar, ou NULL se o arquivo não for encontrado.
#' @export
carrega_dado_auxiliar <- function(nome) {
  path <- file.path("dados", "base_calculo", paste0(nome, ".csv"))
  if (!file.exists(path)) {
    rlog::log_error(sprintf("Arquivo auxiliar %s não encontrado", nome))
    return(NULL)
  }
  df <- readr::read_csv2(
    path,
    locale = readr::locale(decimal_mark = ",", grouping_mark = "."),
    show_col_types = FALSE
  )
  if ("codigo_municipio" %in% names(df)) {
    df$codigo_municipio <- as.character(df$codigo_municipio)
  }
  return(df)
}

#' Carrega dados populacionais de um arquivo auxiliar
#'
#' Esta função lê um arquivo CSV de dados populacionais localizado em 'dados/base_calculo' e retorna um data frame
#' contendo apenas as colunas permitidas: codigo_municipio, municipio, populacao_rural, populacao_urbana, populacao_total.
#' Caso a coluna 'codigo_municipio' exista, ela será convertida para caractere.
#'
#' @param nome Nome do arquivo (sem extensão .csv) a ser carregado.
#'
#' @return Um data frame com os dados populacionais filtrados, ou NULL se o arquivo não for encontrado.
#' @export
carrega_dado_populacao <- function(nome) {
  path <- file.path("dados", "base_calculo", paste0(nome, ".csv"))
  if (!file.exists(path)) {
    rlog::log_error(sprintf("Dados %s não encontrado", nome))
    return(NULL)
  }
  df <- readr::read_csv2(
    path,
    locale = readr::locale(decimal_mark = ",", grouping_mark = "."),
    show_col_types = FALSE
  )
  if ("codigo_municipio" %in% names(df)) {
    df$codigo_municipio <- as.character(df$codigo_municipio)
  }
  allowed_cols <- c("codigo_municipio", "municipio", "populacao_rural", "populacao_urbana", "populacao_total")
  current_cols <- names(df)
  cols_to_keep <- intersect(allowed_cols, current_cols)
  df <- df[cols_to_keep]
  return(df)
}


#' Salva resultados intermediários do calculo da necessidade de investimento
#'
#' @param df Data frame com os dados a serem salvos
#' @param nome Nome do arquivo a ser salvo
#' @param cenario Cenário de cálculo
#'
#' @return Nenhum valor retornado
#' @export
salva_resultado_intermediario <- function(df, nome, cenario = "base") {
  dir_resultado <- file.path("dados", "resultados", cenario)
  if (!dir.exists(dir_resultado)) {
    dir.create(dir_resultado, recursive = TRUE)
  }
  file_name <- paste0(nome, ".csv")
  path_out <- file.path(dir_resultado, file_name)
  readr::write_excel_csv2(df, path_out, quote = "needed", append = FALSE)
  rlog::log_info(paste0("Salvando resultado intermediário: ", path_out))
}


#' Retorna lista de dados sinapi disponíveis
#'
#' Esta função retorna uma lista nomeada com os arquivos disponíveis na pasta 'dados/base_calculo' que
#' começam com o prefixo "sinapi_". Os nomes dos arquivos são retornados sem o caminho e sem a extensão.
#'
#' @return Uma lista nomeada com os nomes dos arquivos sinapi disponíveis.
#' @export
get_sinapi_labels <- function() {
  # get list of files in base_calculo directory that start with "sinapi_"
  files <- list.files(file.path("dados", "base_calculo"), pattern = "^sinapi_", full.names = TRUE)
  # get only the file names without the path and extension
  file_names <- tools::file_path_sans_ext(basename(files))
  # return a named list with file names as labels
  labels <- setNames(file_names, file_names)
  return(labels)
}

#' Retorna os rótulos dos arquivos de censo disponíveis na base de cálculo
#'
#' Esta função lista os arquivos na pasta 'dados/base_calculo' que contêm o padrão "censo" no nome.
#'
#' @return Um vetor com os nomes dos arquivos de censo disponíveis (sem extensão).
#' @export
get_censo_labels <- function() {
  # Lista arquivos que contenham 'censo' seguido de 4 dígitos e extensão .csv (case-insensitive)
  files <- list.files(
    file.path("dados", "base_calculo"),
    pattern = "censo_[0-9]{4}\\.csv$",
    full.names = FALSE
  )
  file_names <- tools::file_path_sans_ext(files)
  return(file_names)
}

#' Retorna os anos dos censos disponíveis na base de cálculo
#'
#' Esta função lista os arquivos na pasta 'dados/base_calculo' que contêm o padrão "censo" no nome
#' e extrai os anos correspondentes.
#' @return Um vetor com os anos dos censos disponíveis.
#' @export
anos_censo_base_calculo <- function() {
  # Lista arquivos que contenham 'censo' seguido de 4 dígitos e extensão .csv
  files <- list.files(
    file.path("dados", "base_calculo"),
    pattern = "censo_[0-9]{4}\\.csv$",
    full.names = FALSE
  )
  anos <- gsub("censo_|\\.csv", "", files)
  return(as.integer(anos))
}


#' Retorna os anos dos censos disponíveis na base de cálculo por setor
#'
#' Esta função lista os arquivos na pasta 'dados/base_calculo' que contêm o padrão "censo_YYYY_setor" no nome
#'
#' @return Um vetor com os anos dos censos disponíveis por setor.
#' @export
anos_censo_setor_base_calculo <- function() {
  files <- list.files(
    file.path("dados", "base_calculo"),
    pattern = "censo_[0-9]{4}_setor.csv$",
    full.names = FALSE
  )
  anos <- gsub("censo_|_setor.csv", "", files)
  return(as.integer(anos))
}

#' Retorna os rótulos dos dados do FIBGE disponíveis
#'
#' @return um vetor com os dados disponiveis
#' @export
#'
#' @examples
#' \dontrun{
#' df <- get_sinapi_data(sinapi)
#' }
get_populacao_labels <- function() {
  # Lista arquivos que contenham 'estimativa_' seguido de 4 dígitos e extensão .csv
  files <- list.files(
    file.path("dados", "base_calculo"),
    pattern = "estimativa_[0-9]{4}\\.csv$",
    full.names = FALSE
  )
  file_names <- tools::file_path_sans_ext(files)
  censo_files <- get_censo_labels()
  return(c(file_names, censo_files))
}
