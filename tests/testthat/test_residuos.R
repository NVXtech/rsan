test_that("limit_classification", {
  limites <- as.integer(c(0, 10e3, 30e3, 100e3, 250e3, 1e6, 4e6))
  POP_TOT <- as.integer(c(0, 10e3, 30e3, 100e3, 250e3, 1e6, 4e6, 5e6))
  faixa <- as.integer(c(1, 1, 2, 3, 4, 5, 6, 7))
  input <- dplyr::tibble(POP_TOT)
  output <- classifica_faixa_populacional(input, limites)
  expected <- input
  expected$faixa <- faixa
  expect_true(all.equal(expected, output))
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
  expect_true(all.equal(expected, output))
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
  test_result <- all.equal(expected, output)
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
  expect_true(all.equal(expected, output))
})

test_that("soma_por_faixa is working", {
  faixa <- c(rep(1, 2), rep(3, 2), rep(2, 2), rep(4, 2))
  valor <- c(1, 1, 2, 2, 3, 3, 4, 4)

  input <- dplyr::tibble(faixa, valor)
  output <- soma_por_faixa(input)

  faixa <- c(1, 3, 2, 4)
  valor <- c(2, 4, 6, 8)
  expected <- dplyr::tibble(faixa, valor)
  # sort by faixa to compare
  output <- output[order(output$faixa), ]
  expected <- expected[order(expected$faixa), ]
  expect_true(all.equal(expected, output))
})

test_that("adiciona preco unidade", {
  input <- list()
  precos <- c(1, 2, 3, 4, 5, 6, 7)
  output <- adiciona_preco_unidade_residuos(input, "aterro", precos)
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

test_that("regionaliza faixa 1 tem resultados consistentes", {
  # Entrada
  demanda_triagem <- c(rep(1000, 7), rep(500, 7))
  faixa <- c(seq(1, 7), seq(1, 7))
  regiao <- c(rep("Norte", 7), rep("Sul", 7))
  estado <- c(rep("AC", 7), rep("RS", 7))
  tabela <- dplyr::tibble(demanda_triagem, faixa, regiao, estado)

  # Saida experada
  expected_demanda <- demanda_triagem
  expected_demanda[1] <- 800
  expected_demanda[2] <- 1200
  expected_demanda[8] <- 250
  expected_demanda[9] <- 750
  expected <- dplyr::tibble(
    estado,
    demanda_triagem = expected_demanda, faixa, regiao
  )
  # Potencial de regionalização
  potencial_regionalizacao <- c(0.2, 0.5)
  regiao <- c("Norte", "Sul")
  potencial <- dplyr::tibble(potencial_regionalizacao, regiao)

  # Testing
  output <- regionaliza_faixa1(tabela, potencial, "demanda_triagem", "potencial_regionalizacao")
  expect_equal(output, expected)
})

test_that("regionalizacao 100% tem resultados consistentes", {
  # Entrada
  demanda <- c(rep(1000, 7), rep(500, 7))
  faixa <- c(seq(1, 7), seq(1, 7))
  regiao <- c(rep("Norte", 7), rep("Sul", 7))
  estado <- c(rep("AC", 7), rep("RS", 7))
  tabela <- dplyr::tibble(demanda, faixa, regiao, estado)

  # Saida experada
  expected_demanda <- rep(0, 14)
  expected_demanda[5] <- 4000
  expected_demanda[6] <- 2000
  expected_demanda[7] <- 1000
  expected_demanda[12] <- 2000
  expected_demanda[13] <- 1000
  expected_demanda[14] <- 500
  expected <- dplyr::tibble(
    estado,
    demanda = expected_demanda, faixa, regiao
  )

  # Testing
  output <- regionaliza100(tabela, "demanda")
  expect_equal(output, expected)
})

test_that("regionaliza faixa 2 e 6 tem resultados consistentes", {
  # Entrada
  demanda_triagem <- c(rep(1000, 7), rep(500, 7))
  faixa <- c(seq(1, 7), seq(1, 7))
  regiao <- c(rep("Norte", 7), rep("Sul", 7))
  estado <- c(rep("AC", 7), rep("RS", 7))
  tabela <- dplyr::tibble(demanda_triagem, faixa, regiao, estado)

  # Saida experada
  expected_demanda <- demanda_triagem
  expected_demanda[1] <- 800
  expected_demanda[2] <- 1200
  expected_demanda[5] <- 800
  expected_demanda[6] <- 1200
  expected_demanda[8] <- 250
  expected_demanda[9] <- 750
  expected_demanda[12] <- 250
  expected_demanda[13] <- 750
  expected <- dplyr::tibble(
    estado,
    demanda_triagem = expected_demanda, faixa, regiao
  )
  # Potencial de regionalização
  potencial_regionalizacao <- c(0.2, 0.5)
  regiao <- c("Norte", "Sul")
  potencial <- dplyr::tibble(potencial_regionalizacao, regiao)

  # Testing
  output <- regionaliza_faixa2e6(
    tabela, potencial, "demanda_triagem", "potencial_regionalizacao"
  )
  expect_equal(output, expected)
})

test_that("regionaliza faixa 5 e 6 tem resultados consistentes", {
  # Entrada
  demanda_triagem <- c(rep(1000, 7), rep(500, 7))
  faixa <- c(seq(1, 7), seq(1, 7))
  regiao <- c(rep("Norte", 7), rep("Sul", 7))
  estado <- c(rep("AC", 7), rep("RS", 7))
  tabela <- dplyr::tibble(demanda_triagem, faixa, regiao, estado)

  # Saida experada
  expected_demanda <- demanda_triagem
  expected_demanda[1] <- 800
  expected_demanda[2] <- 800
  expected_demanda[3] <- 800
  expected_demanda[4] <- 800
  expected_demanda[5] <- 1600
  expected_demanda[6] <- 1200
  expected_demanda[7] <- 1000

  expected_demanda[8] <- 250
  expected_demanda[9] <- 250
  expected_demanda[10] <- 250
  expected_demanda[11] <- 250
  expected_demanda[12] <- 1250
  expected_demanda[13] <- 750
  expected_demanda[14] <- 500
  expected <- dplyr::tibble(
    estado,
    demanda_triagem = expected_demanda, faixa, regiao
  )
  # Potencial de regionalização
  potencial_regionalizacao <- c(0.2, 0.5)
  regiao <- c("Norte", "Sul")
  potencial <- dplyr::tibble(potencial_regionalizacao, regiao)

  # Testing
  output <- regionaliza_faixa5e6(tabela, potencial, "demanda_triagem", "potencial_regionalizacao")
  expect_equal(output, expected)
})

test_that("preenche faixa vazia", {
  # Entrada
  demanda <- c(rep(1000, 5), rep(500, 5))
  teste <- c(rep(1000, 5), rep(500, 5))
  faixa <- c(seq(1, 5), seq(1, 5))
  regiao <- c(rep("Norte", 5), rep("Sul", 5))
  estado <- c(rep("AC", 5), rep("RS", 5))
  tabela <- dplyr::tibble(demanda, teste, faixa, regiao, estado)

  demanda <- c(rep(1000, 5), rep(NA, 2), rep(500, 5), rep(NA, 2))
  teste <- c(rep(1000, 5), rep(NA, 2), rep(500, 5), rep(NA, 2))
  faixa <- c(seq(1, 7), seq(1, 7))
  regiao <- c(rep("Norte", 7), rep("Sul", 7))
  estado <- c(rep("AC", 7), rep("RS", 7))
  expected <- dplyr::tibble(estado, demanda, teste, faixa, regiao)

  # Testing
  output <- cria_faixas_vazias(tabela)
  expect_equal(output, expected)
})
