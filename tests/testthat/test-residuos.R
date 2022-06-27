test_that("limit_classification", {
  limites <- as.integer(c(0, 10e3, 30e3, 100e3, 250e3, 1e6, 4e6))
  POP_TOT <- as.integer(c(0, 10e3, 30e3, 100e3, 250e3, 1e6, 4e6, 5e6))
  faixa <- as.integer(c(1, 1, 2, 3, 4, 5, 6, 7))
  input <- dplyr::tibble(POP_TOT)
  output <- classifica_faixa_populacional(input, limites)
  expected <- input
  expected$faixa <- faixa
  expect_true(dplyr::all_equal(expected, output))
})


test_that("soma_por_estado_faixa is working", {
  Estado <- c(rep("AC", 4), rep("AL", 4))
  faixa <- c(rep(1, 2), rep(3, 2), rep(2, 2), rep(4, 2))
  valor <- c(1, 1, 2, 2, 3, 3, 4, 4)

  input <- dplyr::tibble(Estado, faixa, valor)
  output <- soma_por_estado_faixa(input)

  Estado <- c(rep("AC", 2), rep("AL", 2))
  faixa <- c(1, 3, 2, 4)
  valor <- c(2, 4, 6, 8)
  expected <- dplyr::tibble(Estado, faixa, valor)
  expect_true(dplyr::all_equal(expected, output))
})


test_that("conta_municipios_por_estado_faixa", {
  Estado <- c(rep("AC", 4), rep("AL", 4))
  faixa <- c(rep(1, 2), rep(3, 2), rep(2, 2), rep(4, 2))
  valor <- c(1, 1, 2, 2, 3, 3, 4, 4)

  input <- dplyr::tibble(Estado, faixa, valor)
  output <- conta_municipios_por_estado_faixa(input)

  Estado <- c(rep("AC", 2), rep("AL", 2))
  faixa <- c(1, 3, 2, 4)
  numero_municipios <- as.integer(c(2, 2, 2, 2))
  expected <- dplyr::tibble(Estado, faixa, numero_municipios)
  test_result <- dplyr::all_equal(expected, output)
  expect_true(test_result)
})

test_that("soma_por_estado_faixa keep character cols", {
  Estado <- c(rep("AC", 4), rep("AL", 4))
  regiao <- c(rep("Norte", 4), rep("Nordeste", 4))
  codigo <- as.character(1:8)
  faixa <- c(rep(1, 2), rep(3, 2), rep(2, 2), rep(4, 2))
  valor <- c(1, 1, 2, 2, 3, 3, 4, 4)

  input <- dplyr::tibble(Estado, faixa, valor, regiao, codigo)
  output <- soma_por_estado_faixa(input)

  Estado <- c(rep("AC", 2), rep("AL", 2))
  regiao <- c(rep("Norte", 2), rep("Nordeste", 2))
  faixa <- c(1, 3, 2, 4)
  valor <- c(2, 4, 6, 8)
  expected <- dplyr::tibble(Estado, faixa, valor, regiao)
  expect_true(dplyr::all_equal(expected, output))
})

test_that("soma_por_faixa is working", {
  faixa <- c(rep(1, 2), rep(3, 2), rep(2, 2), rep(4, 2))
  valor <- c(1, 1, 2, 2, 3, 3, 4, 4)

  input <- dplyr::tibble(faixa, valor)
  output <- soma_por_faixa(input)

  faixa <- c(1, 3, 2, 4)
  valor <- c(2, 4, 6, 8)
  expected <- dplyr::tibble(faixa, valor)
  expect_true(dplyr::all_equal(expected, output))
})

test_that("adiciona preco unidade", {
  input <- list()
  precos <- c(1, 2, 3, 4, 5, 6, 7)
  output <- adiciona_preco_unidade_residuos(input, "aterro", precos)
  print(output)
  expected <- list(
    aterro_faixa1 = 1,
    aterro_faixa2 = 2,
    aterro_faixa3 = 3,
    aterro_faixa4 = 4,
    aterro_faixa5 = 5,
    aterro_faixa6 = 6,
    aterro_faixa7 = 7
  )
  expect_equal(output, expected)
})
