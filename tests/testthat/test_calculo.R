test_that("calculos estão rodando", {
    setwd(file.path("../../"))
    app_state <- list(input = get_default_input())
    state <- rodar_modelo(app_state)
    expect_true(!is.null(state))
})
