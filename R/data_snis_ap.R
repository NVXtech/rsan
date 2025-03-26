#' Retorna dados do SNIS-AP
#'
#' @param year ano desejado
#'
#' @return um `data.frame` contendo os dados do SNIS
#' @export
download_snis_ap <- function(year) {
    if (year>=2020){
      snis_base_url <- "https://www.gov.br/mdr/pt-br/assuntos/saneamento/snis/produtos-do-snis/diagnosticos/"
      download_url <- paste0(
        snis_base_url,
        sprintf("Planilhas_AP%s.zip", year)
      )
    } else {
      snis_base_url <- "https://www.gov.br/mdr/pt-br/assuntos/saneamento/snis/diagnosticos-anteriores-do-snis/aguas-pluviais/"
      download_url <- paste0(
        snis_base_url,
        sprintf("%s/Planilhas_AP%s.zip", year, year)
      )
    }
    temp_zipfile <- tempfile(fileext = ".zip")
    tmp_dir <- tempdir(check = TRUE)
    curl::curl_download(download_url, temp_zipfile)
    if (file.exists(temp_zipfile)) {
        try(
            files_zip <- utils::unzip(
                temp_zipfile,
                exdir = tmp_dir, unzip = "unzip"
            )
        )
        print(tmp_dir)
        xls_fname <- dir(path = tmp_dir, pattern = ".*nforma.*")
        if (length(xls_fname) == 0) {
            rlog::log_warn(sprintf("Could not extract snis ap for %s!", year))
            return(NULL)
        }
        file.rename(
            file.path(tmp_dir, xls_fname), file.path(tmp_dir, "planilha")
        )
        snis_ap <- readxl::read_excel(
            file.path(tmp_dir, "planilha"),
            skip = 11
        )
        colnames(snis_ap)[1:5] <- c(
            "codigo_municipio", "municipio", "estado", "regiao", "capital"
        )
        snis_ap$codigo_municipio <- as.character(
            snis_ap$codigo_municipio
        )
        unlink(tmp_dir)
    } else {
        rlog::log_warn(sprintf("Error downloading SNIS for %s", year))
    }
    unlink(temp_zipfile)
    return(snis_ap)
}

#' Atualiza dados do SNIS-AP
#'
#' @param ano é um `number` com o ano desejado
#'
#' @return um `logical` dizendo se a atualização ocorreu com sucesso
#' @export
update_snis_ap <- function(ano) {
    id <- paste0("ano", ano)
    snis_ap <- rsan::load_data("snis_ap")
    try({
        snis_ap[[id]] <- download_snis_ap(ano)
        save(snis_ap, file = rsan::get_data_path("snis_ap"))
    })
    return(!is.null(snis_ap[[id]]))
}


#' Retorna lista dos anos de SNIS-AP
#'
#' @return um `list` contendo os anos para tentar baixar
#' @export
get_snis_ap_list <- function() {
    ids <- names(rsan::load_data("snis_ap"))
    output <- list()
    for (id in ids) {
        year <- substr(id, 4, 7)
        name <- paste0("SNIS-AP ", year)
        output[[name]] <- id
    }
    return(output)
}

#' Carrega conjunto de dados do SNIS-AP
#
#' @param id um `character` com o id
#'
#' @return o conjunto de dados do SNIS
#' @export
load_snis_ap <- function(id) {
    rsan::load_data("snis_ap")[[id]]
}

#' Cria armazenamento local dos dados do SNIS-AP
#'
#' @return NULL
#' @export
create_snis_ap <- function() {
    data("snis_ap", package = "rsan")
    snis_ap <- get("snis_ap")
    save(snis_ap, file = rsan::get_data_path("snis_ap"))
}

#' Verifica a integridade dos dados do SNIS-AP
#'
#' @return um `logical` sendo `TRUE` integridade OK.
#' @export
integrity_snis_ap <- function() {
    rsan::check_data_exists("snis_ap")
}
