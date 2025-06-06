dir_base_calculo <- file.path("dados", "base_calculo")

#' Verifica a integridade da base de cálculo
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
  } else if (componente == "aguas_pluviais") {
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
  if (!componente %in% c("agua", "esgoto", "residuos", "aguas_pluviais")) {
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
get_sinapi_labels <- function() {
  # get list of files in base_calculo directory that start with "sinapi_"
  files <- list.files(file.path("dados", "base_calculo"), pattern = "^sinapi_", full.names = TRUE)
  # get only the file names without the path and extension
  file_names <- tools::file_path_sans_ext(basename(files))
  # return a named list with file names as labels
  labels <- setNames(file_names, file_names)
  return(labels)
}
