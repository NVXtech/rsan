file_path <- file.path("data-raw", "projeto_tipo.xlsx")

projeto_ditribuicao_agua <- readxl::read_xlsx(file_path, sheet="DistribuicaoAgua")
usethis::use_data(projeto_ditribuicao_agua, overwrite=TRUE)

projeto_coleta_esgoto <- readxl::read_xlsx(file_path, sheet="ColetaEsgoto")
usethis::use_data(projeto_coleta_esgoto, overwrite=TRUE)

projeto_producao_agua <- readxl::read_xlsx(file_path, sheet="ProducaoAgua")
usethis::use_data(projeto_producao_agua, overwrite=TRUE)

projeto_tratamento_esgoto <- readxl::read_xlsx(file_path, sheet="TratamentoEsgoto")
usethis::use_data(projeto_tratamento_esgoto, overwrite=TRUE)
