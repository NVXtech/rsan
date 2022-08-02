test_that("calculos estão rodando", {
    setwd(file.path("../../"))
    app_state <- list(input = get_default_input())
    state <- rodar_modelo(app_state)
    testthat::expect_true(!is.null(state))

    valores <- dplyr::group_by(state$necessidade, componente)
    valores <- dplyr::summarise(
        valores,
        total = sum(necessidade_investimento, na.rm = TRUE)
    )
    componente <- c("agua", "drenagem", "esgoto", "residuos")
    total <- c(240238483806., 478254161220., 334163032430., 122893122849.)
    expected <- dplyr::tibble(componente, total)
    testthat::expect_equal(valores, expected)
    valores <- dplyr::filter(state$necessidade, componente == "residuos", destino == "expansao")
    valores <- dplyr::group_by(valores, regiao, etapa)
    valores <- dplyr::summarise(
        valores,
        total = sum(necessidade_investimento, na.rm = TRUE)
    )
    valores$total <- format(
        valores$total,
        big.mark = ".",
        decimal.mark = ",",
        scientific = FALSE
    )
    print(valores)
})
