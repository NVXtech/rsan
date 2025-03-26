test_that("classificacão de municipio esta correta", {
  classificacao <- c("07a", "07b", "08a", "08b", "09a", "09b")
  cenario <- c("07", "07", "08", "08", "09", "09")
  populacao_total <- c(3000, 3000, 300000, 300000, 600000, 600000)
  populacao_urbana <- c(2999, 1, 299999, 1, 599999, 1)
  populacao_rural <- c(1, 2999, 1, 299999, 1, 599999)

  df_in <- dplyr::tibble(populacao_total, populacao_urbana, populacao_rural)
  df_out <- dplyr::tibble(
    populacao_total, populacao_urbana, populacao_rural, cenario,classificacao,
  )

  output <- classifica_municipio(df_in)
  expect_true(all.equal(output, df_out))
})

test_that("adiciona_estados adiciona coluna com estado corretamente", {
  codigo_municipio <- c("1200013", "1200054", "1200104", "1200138")
  estado <- rep("AC", 4)

  input <- dplyr::tibble(codigo_municipio)
  expected_output <- dplyr::tibble(codigo_municipio, estado)

  df_out <- adiciona_estado(input)
  expect_true(all.equal(expected_output, df_out))
})

test_that("adiciona_regiao adiciona coluna com regiao corretamente", {
  codigo_municipio <- c("1200013", "1200054", "1200104", "1200138")
  regiao <- rep("Norte", 4)

  input <- dplyr::tibble(codigo_municipio)
  expected_output <- dplyr::tibble(codigo_municipio, regiao)


  df_out <- adiciona_regiao(input)
  expect_true(all.equal(expected_output, df_out))
})

test_that("adiciona_pais adiciona coluna pais preenchido com a constante Brasil", {
  codigo_municipio <- c("1200013", "1200054", "1200104", "1200138")
  pais <- rep("Brasil", 4)

  input <- dplyr::tibble(codigo_municipio)
  expected_output <- dplyr::tibble(codigo_municipio, pais)


  df_out <- adiciona_pais(input)
  expect_true(all.equal(expected_output, df_out))
})

test_that("testa calculo de reposicao/depreciacao total", {
  ano_inicial <- 2021
  ano_final <- 2033
  vida_util <- 30
  investimento <- c(1, 2)
  capacidade <- c(0, 1.2)
  reposicao <- c(0.2, 0.92)

  df_in <- dplyr::tibble(capacidade, investimento)
  expected <- dplyr::tibble(capacidade, investimento, reposicao)
  df_out <- calcula_reposicao_total(
    df_in,
    "capacidade",
    "investimento",
    "reposicao",
    ano_inicial,
    ano_final,
    vida_util
  )
  expect_true(all.equal(expected, df_out))
})

test_that("testa calculo de reposicao/depreciacao parcial", {
  ano_inicial <- 2021
  ano_final <- 2033
  ano_reposicao <- 2022
  vida_util <- 30
  investimento <- c(1.0, 3.0)
  capacidade <- c(0.0, 1.0)
  reposicao <- c(0.2, 1.0)

  df_in <- dplyr::tibble(capacidade, investimento)
  expected <- dplyr::tibble(capacidade, investimento, reposicao)
  df_out <- calcula_reposicao_parcial(
    df_in,
    "capacidade",
    "investimento",
    "reposicao",
    ano_inicial,
    ano_final,
    ano_reposicao,
    vida_util
  )
  expect_true(all.equal(expected, df_out))
})


test_that("se o custo relativo dos projeto tipo producao de água está correto", {
  estado <- c("AC", "AC", "AC", "AL", "AL", "AL")
  unidade <- c("ETA200", "EEA200", "POÇO40", "ETA200", "EEA200", "POÇO40")
  preco <- c(3.055, 0.129, 0.69, 2.88, 0.12, 0.66)
  preco_unidade <- dplyr::tibble(estado, unidade, preco)

  cenario <- c("08", "08", "08", "08", "09", "09", "09", "09")
  unidade <- c("ETA200", "EEA200", "POÇO40", "EEA200", "ETA200", "EEA200", "POÇO40", "EEA200")
  tipo <- c(
    "superficial", "superficial", "subterranea", "subterranea",
    "superficial", "superficial", "subterranea", "subterranea"
  )
  quantidade <- c(0.854106686284393, 1.3, 1, 1, 0.549559117416313, 1.3, 1, 1)
  projeto_producao <- dplyr::tibble(cenario, unidade, tipo, quantidade)

  estado <- c("AC", "AC", "AL", "AL")
  cenario <- c("08", "09", "08", "09")
  custo_relativo <- c(2.62, 1.76, 1.87, 1.35)
  expected_output <- dplyr::tibble(estado, cenario, custo_relativo)

  df_out <- calcula_custo_relativo_producao(preco_unidade, projeto_producao)
  df_out <- dplyr::mutate(df_out, custo_relativo = round(custo_relativo, 2))
  df_out <- dplyr::as_tibble(df_out)
  expect_true(all.equal(expected_output, df_out))
})

test_that("Se o custo relativo dos projeto tipo de tratamento de esgoto está correto", {
  estado <- c("AC", "AC", "AC", "AC", "AL", "AL", "AL", "AL")
  unidade <- c(
    "LAGOA125", "REATORANA180", "EE85", "LODOBAT400",
    "LAGOA125", "REATORANA180", "EE85", "LODOBAT400"
  )
  preco <- c(2.476, 0.762, 0.209, 2.31, 2.051, 0.4282, 0.2008, 2.2114)
  preco_unidade <- dplyr::tibble(estado, unidade, preco)

  cenario <- c("07", "07", "07", "09", "09")
  unidade <- c("LAGOA125", "REATORANA180", "EE85", "LODOBAT400", "EE85")
  quantidade <- c(1.2, 1, 1, 2.05 * 1.71361608179778, 1)
  projeto_tratamento <- dplyr::tibble(cenario, unidade, quantidade)

  estado <- c("AC", "AC", "AL", "AL")
  cenario <- c("07", "09", "07", "09")
  custo_relativo <- c(3.94, 8.32, 3.09, 7.97)
  expected_output <- dplyr::tibble(estado, cenario, custo_relativo)

  df_out <- calcula_custo_relativo_tratamento(
    preco_unidade, projeto_tratamento
  )
  df_out <- dplyr::mutate(df_out, custo_relativo = round(custo_relativo, 2))
  df_out <- dplyr::as_tibble(df_out)
  expect_true(all.equal(expected_output, df_out))
})

test_that("Se o fator de perda esta correto", {
  fator <- fator_perda_agua(25)
  expect_equal(4 / 3, fator)
})


test_that("Testa density filling", {
  Estado <- c(rep("RR", 3), rep("SP", 3), rep("PR", 3))
  POP_TOT <- 1e5 * c(1, 2, 3, 4, 5, 6, 7, 8, 9)
  densidade_esperada <- c(2, 3, 6, 6, 10, 12, 14, 16, 28)
  densidade <- c(2, NA, 6, NA, 10, 12, 14, 16, NA)
  df_in <- dplyr::tibble(Estado, POP_TOT, densidade)
  expected <- dplyr::tibble(Estado, POP_TOT, densidade = densidade_esperada)

  df_out <- fill_missing_density(df_in, c("densidade"))
  df_out <- dplyr::mutate(df_out, densidade = round(densidade, 0))
  expect_true(all.equal(expected, df_out))
})
