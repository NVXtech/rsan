test_that("testa classificacao de municipio", {
  classificacao <- c("07a", "07b", "08a", "08b", "09a", "09b")
  populacao_total <-  c(3000, 3000, 300000, 300000, 600000, 600000)
  populacao_urbana <- c(2999,    1, 299999,      1, 599999,      1)
  populacao_rural <-  c(   1, 2999,      1, 299999,      1, 599999)

  df_in <- dplyr::tibble(populacao_total, populacao_urbana, populacao_rural)
  df_out <- dplyr::tibble(populacao_total, populacao_urbana, populacao_rural, classificacao)

  output <- classifica_municipio(df_in)
  expect_true(dplyr::all_equal(output, df_out))
})

test_that("testa consolidação de populacoes com snis", {
 codigo_municipio <- c("1200013","1200054", "1200104", "1200138")
 estado <- rep("AC", 4)

 input <- dplyr::tibble(codigo_municipio)
 expected_output <- dplyr::tibble(codigo_municipio, estado)

 df_out <- adiciona_estado(input)
 expect_true(dplyr::all_equal(expected_output, df_out))
})

