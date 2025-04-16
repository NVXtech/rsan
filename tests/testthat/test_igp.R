test_that("correcao pelo igp é calculada corretamente", {
  data_inicial <- as.Date("2016-01-10")
  data_final <- as.Date("2022-01-05")
  value <- get_taxa_igp(data_inicial, data_final)
  expected <- 1.4929
  expect_equal(value, expected, tolerance = 0.001)
})

test_that("correcao pelo igp é calculada pra listas", {
  data_inicial <- c("2016-01-10", "2016-01-10")
  data_final <- c("2022-01-05", "2022-02-05")

  value <- get_taxa_igp(as.Date(data_inicial), as.Date(data_final))
  expected <- c(1.4929, 1.4986)
  expect_equal(value, expected, tolerance = 0.001)
})
