# Libraries ----
library(readxl)
library(tidyverse)


# Working Directory ----
setwd("data-raw")


# Data Import ----

## Store Date String
if (substr(Sys.time(), 6, 7) == "01") {
  year <- as.character(as.numeric(substr(Sys.time(), 1, 4)) - 1)
  month <- "12"
} else {
  year <- substr(Sys.time(), 1, 4)
  if (nchar(as.numeric(substr(Sys.time(), 6, 7))) == 1) {
    month <- paste0("0", as.character(as.numeric(substr(Sys.time(), 6, 7)) - 1))
  } else {
    month <- as.character(as.numeric(substr(Sys.time(), 6, 7)) - 1)
  }
}

states <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE",
            "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO")

type <- c("Desonerado", "NaoDesonerado")

## Download Zipped Folders
for (i in type) {
  if (length(list.files(path = paste0(i, "/Zipped"))) == 27) {
    unlink(paste0(i, "/Zipped/*"))
  }
  for (j in states) {
    url <- paste0("https://www.caixa.gov.br/Downloads/sinapi-a-partir-jul-2009-", tolower(j),
                  "/SINAPI_ref_Insumos_Composicoes_", j, "_", month, year, "_", i, ".zip")
    destfile <- paste0(i, "/Zipped/SINAPI_ref_Insumos_Composicoes_", j, "_", month, year, "_", i, ".zip")
    if (!file.exists(destfile)) {
      curl::curl_download(url, destfile)
    }
  }
}

## Unzipping All Folders
for (i in type) {
  for (j in list.files(path = paste0(i, "/Zipped"))) {
    unzip(paste0(i, "/Zipped/", j),
          exdir = sub(pattern = ".zip$", replacement = "", paste0(i, "/Unzipped/", j)))
  }
}

## Clearing Environment
rm(list = ls())


# Data Manipulation ----

## Inputs Data
path <- "NaoDesonerado/Unzipped"

for (i in list.files(path = path, pattern = "Insumos.*\\.xls$", recursive = TRUE)) {
  if (!exists("inputs")) {
    inputs <- cbind("ESTADO" = substr(i, 32, 33),
                    head(read_excel(paste0(path, "/", i),
                                    col_types = c("text", "text", "text", "text", "text"),
                                    skip = 6),
                         n = -2)
                    )[ , -5]
  } else {
    temp <- cbind("ESTADO" = substr(i, 32, 33),
                  head(read_excel(paste0(path, "/", i),
                                  col_types = c("text", "text", "text", "text", "text"),
                                  skip = 6),
                       n = -2)
                  )[ , -5]
    inputs <- rbind(inputs, temp)
  }
}
rm(temp)

inputs$`PRECO MEDIANO R$` <- as.numeric(gsub(pattern = ",", replacement = ".",
                                             gsub(pattern = "\\.", replacement = "",
                                                  inputs$`PRECO MEDIANO R$`)
                                             )
                                        )

new.names <- c("ESTADO", "CODIGO", "DESCRICAO", "UNIDADE", "PRECO")
for (i in 1:length(names(inputs))) {
  names(inputs)[names(inputs) == names(inputs)[i]] <- new.names[i]
}
rm(new.names, i)


## Compositions Data
for (i in list.files(path = path, pattern = "Composicoes_Sintetico.*\\.xls$", recursive = TRUE)) {
  if (!exists("compositions")) {
    compositions <- cbind("ESTADO" = substr(i, 32, 33),
                          read_excel(paste0(path, "/", i), skip = 4)
                          )[-1, c(1, 8, 9, 10, 12)]
  } else {
    temp <- cbind("ESTADO" = substr(i, 32, 33),
                  read_excel(paste0(path, "/", i), skip = 4)
                  )[-1, c(1, 8, 9, 10, 12)]
    compositions <- rbind(compositions, temp)
  }
}
rm(temp)

compositions$`CUSTO TOTAL` <- as.numeric(gsub(pattern = ",", replacement = ".",
                                              gsub(pattern = "\\.", replacement = "",
                                                   compositions$`CUSTO TOTAL`)
                                              )
                                         )

new.names <- c("ESTADO", "CODIGO", "DESCRICAO", "UNIDADE", "PRECO")
for (i in 1:length(names(compositions))) {
  names(compositions)[names(compositions) == names(compositions)[i]] <- new.names[i]
}
rm(new.names, i)

## Merge Two Data Frames
inputs <- cbind("TIPO" = "INSUMO", inputs)
compositions <- cbind("TIPO" = "COMPOSICAO", compositions)
SINAPI <- rbind(inputs, compositions)

usethis::use_data(SINAPI)
# Storing Results ----
write.csv(SINAPI, file = "SINAPI.csv", row.names = FALSE)
