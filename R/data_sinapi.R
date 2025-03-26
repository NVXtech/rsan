#' Último ano e mês
#'
#' @return uma `list` com o último ano e mes
#' @export
get_last_month_and_year <- function() {
  last_date <- lubridate::floor_date(as.Date(Sys.Date()), "month") - months(1)
  return(list(
    year = substr(last_date, 1, 4),
    month = substr(last_date, 6, 7)
  ))
}

#' Baixa dados do SINAPI
#'
#' Baixa dados do SINAPI para um determinado ano e mês

#' @param year é um `number` com o ano desejado
#' @param month é um `number` com o mês desejado
#'
#' @return um `data.frame` com os dados do SINAPI
#' @export
download_sinapi <- function(year, month) {
  month <- sprintf("%02.f", as.integer(month))
  type <- "NaoDesonerado"
  z <- 0
  for (state in rsan::states_acronym()) {
    url <- paste0(
      "https://www.caixa.gov.br/Downloads/sinapi-a-partir-jul-2009-",
      tolower(state), "/SINAPI_ref_Insumos_Composicoes_",
      state, "_", month, year, "_", type, ".zip"
    )
    rlog::log_info(sprintf("Baixando SINAPI do ESTADO %s", state))
    rlog::log_info(sprintf("SINAPI URL: %s", url))
    tmp_file <- tempfile(fileext = ".zip")
    tmp_dir <- tempdir()
    curl::curl_download(url, tmp_file)
    lista <- utils::unzip(tmp_file, list = TRUE)
    coluna_preco <- paste0("PRECO_", state)
    consolidada <- data.frame()
    for (arquivo in lista$Name) {
      if (grepl("Sintetico.*NaoDesonerado(.XLS|.xls)", arquivo)) {
        colunas <- c(
          "CODIGO  DA COMPOSICAO", "DESCRICAO DA COMPOSICAO",
          "UNIDADE", "CUSTO TOTAL"
        )
        nomes <- c("CODIGO", "DESCRICAO", "UNIDADE", coluna_preco)
        skip <- 4
        tipo <- "COMPOSICAO"
      } else if (grepl("Insumos.*NaoDesonerado(.XLS|.xls)", arquivo)) {
        colunas <- c(
          "CODIGO", "DESCRICAO DO INSUMO",
          "UNIDADE DE MEDIDA", "PRECO MEDIANO R$"
        )
        yearmonth <- paste0(year, month)
        if (as.integer(yearmonth) >= 202206) {
          colunas[3] <- "UNIDADE"
        }
        nomes <- c("CODIGO", "DESCRICAO", "UNIDADE", coluna_preco)
        skip <- 6
        tipo <- "INSUMO"
      } else {
        next
      }
      utils::unzip(tmp_file,
        exdir = tmp_dir,
        files = c(arquivo), unzip = "unzip"
      )
      caminho <- file.path(tmp_dir, arquivo)
      tabela <- readxl::read_excel(
        caminho,
        sheet = 1, skip = skip, col_types = "text"
      )
      tabela <- dplyr::select(tabela, dplyr::all_of(colunas))
      rlog::log_info("Removing NA")
      tabela <- tabela[complete.cases(tabela), ] # REMOVE LINHAS COM NA
      names(tabela) <- nomes
      tabela[["TIPO"]] <- tipo
      tabela[[coluna_preco]] <- as.double(
        gsub(
          pattern = ",", replacement = ".",
          gsub(
            pattern = "\\.", replacement = "",
            tabela[[coluna_preco]]
          )
        )
      )
      rlog::log_info("Consolidando Insumos e Composicoes")
      consolidada <- dplyr::bind_rows(consolidada, tabela)
      unlink(arquivo)
    }
    unlink(tmp_file)
    if (z == 0) {
      output <- consolidada
    } else {
      rlog::log_info("Juntando estados")
      vars <- c("CODIGO", coluna_preco)
      consolidada <- dplyr::select(consolidada, dplyr::all_of(vars))
      output <- dplyr::left_join(output, consolidada, by = "CODIGO")
    }
    z <- z + 1
  }
  return(output)
}


#' Armazena dados do SINAPI
#'
#' @return NULL
#' @export
create_sinapi <- function() {
  data("sinapi_202112", package = "rsan")
  sinapi_202112 <- get("sinapi_202112")
  sinapi <- list(
    dt202112 = sinapi_202112
  )
  save(sinapi, file = rsan::get_data_path("sinapi"))
}

#' Armazena dados sinapi
#'
#' @return um `logical` sendo `TRUE` integridade OK.
#' @export
integrity_sinapi <- function() {
  pop <- rsan::load_data("sinapi")
  for (caminho in pop$caminho) {
    if (!file.exists(get_data_path(caminho))) {
      rlog::log_info(sprintf("%s dataset not found", caminho))
      return(FALSE)
    } else {
      rlog::log_info(sprintf("%s dataset is OK", caminho))
    }
  }
  return(TRUE)
}

#' Atualiza dados do SINAPI
#'
#' @param year é um `number` com o ano desejado
#' @param month é um `number` com o mês desejado
#'
#' @return um `logical` dizendo se a atualização ocorreu com sucesso
#' @export
update_sinapi <- function(ano, mes) {
  id <- paste0("dt", ano, mes)
  sinapi <- rsan::load_data("sinapi")
  try({
    sinapi[[id]] <- download_sinapi(ano, mes)
    save(sinapi, file = rsan::get_data_path("sinapi"))
  })
  return(!is.null(sinapi[[id]]))
}

#' Retorna os rótulos dos dados do SINAPI disponívei
#'
#' @return um vetor com os dados disponiveis
#' @export
#'
#' @examples
#' \dontrun{
#' df <- get_sinapi_data(sinapi)
#' }
get_sinapi_labels <- function() {
  return(names(rsan::load_data("sinapi")))
}

#' Transforma id SINAPI em nome legível
#'
#' @return um `character` com o nome
#' @export
sinapi_id_to_name <- function(id) {
  ano <- substr(id, 3, 6)
  mes <- substr(id, 7, 8)
  return(paste0("SINAPI - ", ano, "-", mes))
}


#' Carrega conjunto de dados do SINAPI
#'
#' @param id um `character` com o id
#'
#' @return o conjunto de dados do SINAPI
#' @export
load_sinapi <- function(id) {
  rsan::load_data("sinapi")[[id]]
}
