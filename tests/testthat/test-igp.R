test_that("correcao pelo igp é calculada corretamente", {
  data(igp)
  data_inicial <- as.Date("2016-01-10")
  data_final <- as.Date("2022-01-05")
  value <- get_taxa_igp(igp, data_inicial, data_final)
  expected <- 1.819
  expect_equal(value, expected, tolerance=0.001)
})
