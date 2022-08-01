test_that("populacao IBGE é criado", {
    unlink(get_data_path("ibge_populacao"))
    create_populacao()
    ibge_populacao <- load_data("ibge_populacao")
    unlink(get_data_path("ibge_populacao"))

    expect_false(is.null(ibge_populacao))
})

test_that("retorno lista de rotulos de populacao", {
    unlink(get_data_path("ibge_populacao"))
    create_populacao()
    labels <- get_populacao_labels()
    unlink(get_data_path("ibge_populacao"))
    expected <- c("censo_2010", "estimativa_2021")
    expect_equal(labels, expected)
})

test_that("retorno lista de rotulos de censo", {
    unlink(get_data_path("ibge_populacao"))
    create_populacao()
    labels <- get_censo_labels()
    unlink(get_data_path("ibge_populacao"))
    expected <- c("censo_2010")
    expect_equal(labels, expected)
})

test_that("coversao id para nome", {
    id <- "censo_2010"
    id <- ibge_id_to_name(id)
    expected <- "2010 - Censo"
    expect_equal(id, expected)

    id <- "estimativa_2021"
    id <- ibge_id_to_name(id)
    expected <- "2021 - Estimativa"
    expect_equal(id, expected)
})
