sinapi_base_path <- file.path("dados", "brutos", "sinapi")
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

#' Baixa dados do SINAPI para um determinado ano e mês
#'
#' @param year é um `number` com o ano desejado
#' @param month é um `number` com o mês desejado
#'
#' @return um `data.frame` com os dados do SINAPI
#' @export
download_sinapi <- function(year, month) {
  month <- sprintf("%02.f", as.integer(month))
  type <- "NaoDesonerado"
  z <- 0
  h <- curl::new_handle()
  caixa_url <- "https://www.caixa.gov.br/site/Paginas/downloads.aspx"
  req <- curl::curl_fetch_memory(caixa_url, handle = h)
  curl::handle_cookies(h)
  for (state in states_acronym()) {
    if (year <= 2023) {
      url <- paste0(
        "https://www.caixa.gov.br/Downloads/sinapi-a-partir-jul-2009-",
        tolower(state), "/SINAPI_ref_Insumos_Composicoes_",
        state, "_", month, year, "_", type, ".zip"
      )
    } else {
      url <- paste0(
        "https://www.caixa.gov.br/Downloads/sinapi-a-partir-jul-2009-",
        tolower(state), "/SINAPI_ref_Insumos_Composicoes_",
        state, "_", year, month, "_", type, ".zip"
      )
    }
    rlog::log_info(sprintf("Baixando SINAPI do ESTADO %s", state))
    rlog::log_info(sprintf("SINAPI URL: %s", url))
    tmp_file <- paste0("sinapi", year, month, state, ".zip")
    path_out <- file.path(sinapi_base_path, tmp_file)
    tmp_dir <- tempdir()
    if (!dir.exists(sinapi_base_path)) {
      dir.create(sinapi_base_path, recursive = TRUE)
    }
    if (file.exists(path_out)) {
      rlog::log_info(sprintf("Arquivo %s já existe", path_out))
    } else {
      req <- curl::curl_fetch_disk(url, path_out, handle = h)
      if (req$status_code != 200) {
        rlog::log_info(sprintf("Erro ao baixar o arquivo %s", url))
        # first try to download retification trying to download the retification file
        # remove the .zip from the url
        url <- gsub(".zip", "_Retificacao01.zip", url)
        req <- curl::curl_fetch_disk(url, path_out, handle = h)
        if (req$status_code != 200) {
          rlog::log_info(sprintf("Erro ao baixar o arquivo %s", url))
          stop("Erro ao baixar o arquivo")
        }
      }
    }
    lista <- utils::unzip(path_out, list = TRUE)
    coluna_preco <- paste0("PRECO_", state)
    consolidada <- data.frame()
    for (arquivo in lista$Name) {
      if (grepl("Sintetico.*NaoDesonerado(.XLS|.xls|.xlsx)", arquivo)) {
        colunas <- c(
          "CODIGO  DA COMPOSICAO", "DESCRICAO DA COMPOSICAO",
          "UNIDADE", "CUSTO TOTAL"
        )
        nomes <- c("CODIGO", "DESCRICAO", "UNIDADE", coluna_preco)
        skip <- 4
        tipo <- "COMPOSICAO"
      } else if (grepl("Insumos.*NaoDesonerado(.XLS|.xls|.xlsx)", arquivo)) {
        colunas <- c(
          "CODIGO", "DESCRICAO DO INSUMO",
          "UNIDADE DE MEDIDA", "PRECO MEDIANO R$"
        )
        yearmonth <- paste0(year, month)
        # if (as.integer(yearmonth) >= 202206) {
        #  colunas[3] <- "UNIDADE"
        # }
        nomes <- c("CODIGO", "DESCRICAO", "UNIDADE", coluna_preco)
        skip <- 6
        tipo <- "INSUMO"
      } else {
        next
      }
      utils::unzip(path_out,
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
  save(sinapi, file = get_data_path("sinapi"))
}

#' Armazena dados sinapi
#'
#' @return um `logical` sendo `TRUE` integridade OK.
#' @export
integrity_sinapi <- function() {
  pop <- load_data("sinapi")
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
  sinapi <- load_data("sinapi")
  try({
    sinapi[[id]] <- download_sinapi(ano, mes)
    save(sinapi, file = get_data_path("sinapi"))
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
  return(names(load_data("sinapi")))
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
  load_data("sinapi")[[id]]
}
