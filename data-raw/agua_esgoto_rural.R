file_path <- file.path("data-raw", "agua_esgoto_rural.xlsx")

agua_esgoto_rural <- list()
planilhas <- c(
    "censo", "deficit_pnad", "seguranca_hidrica",
    "custo_producao", "custo_distribuicao",
    "custo_coleta", "custo_tratamento"
)

for (planilha in planilhas) {
    agua_esgoto_rural[[planilha]] <- readxl::read_xlsx(
        file_path,
        sheet = planilha
    )
}

agua_esgoto_rural$censo$codigo_setor <- as.character(agua_esgoto_rural$censo$codigo_setor)
agua_esgoto_rural$seguranca_hidrica$codigo_municipio <- as.character(agua_esgoto_rural$seguranca_hidrica$codigo_municipio)

usethis::use_data(agua_esgoto_rural, overwrite = TRUE)
