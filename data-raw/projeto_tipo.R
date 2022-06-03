file_path <- file.path("data-raw", "projeto_tipo.xlsx")

projeto_distribuicao_agua <- readxl::read_xlsx(file_path, sheet="DistribuicaoAgua")
usethis::use_data(projeto_distribuicao_agua, overwrite=TRUE)

projeto_coleta_esgoto <- readxl::read_xlsx(file_path, sheet="ColetaEsgoto")
usethis::use_data(projeto_coleta_esgoto, overwrite=TRUE)

projeto_producao_agua_unidades <- readxl::read_xlsx(file_path, sheet="UnidadesProducaoAgua")
usethis::use_data(projeto_producao_agua_unidades, overwrite=TRUE)

projeto_producao_agua <- readxl::read_xlsx(file_path, sheet="ProjetoProducao")
usethis::use_data(projeto_producao_agua, overwrite=TRUE)

projeto_tratamento_esgoto_unidades <- readxl::read_xlsx(file_path, sheet="UnidadesTratamentoEsgoto")
usethis::use_data(projeto_tratamento_esgoto_unidades, overwrite=TRUE)

projeto_tratamento_esgoto <- readxl::read_xlsx(file_path, sheet="ProjetoTratamento")
usethis::use_data(projeto_tratamento_esgoto, overwrite=TRUE)

projeto_predominancia_tipo_producao <- readxl::read_xlsx(file_path, sheet="PredominanciaTipoProducao")
usethis::use_data(projeto_predominancia_tipo_producao, overwrite=TRUE)
