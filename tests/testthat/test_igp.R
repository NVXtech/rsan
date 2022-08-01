test_that("correcao pelo igp é calculada corretamente", {
  data(igp, package = "rsan")
  data_inicial <- as.Date("2016-01-10")
  data_final <- as.Date("2022-01-05")
  value <- get_taxa_igp(igp, data_inicial, data_final)
  expected <- 1.819
  expect_equal(value, expected, tolerance = 0.001)
})

test_that("correcao pelo igp é calculada pra listas", {
  data(igp, package = "rsan")
  data_inicial <- c("2016-01-10", "2016-01-10")
  data_final <- c("2022-01-05", "2022-02-05")

  value <- get_taxa_igp(igp, as.Date(data_inicial), as.Date(data_final))
  expected <- c(1.819, 1.847)
  expect_equal(value, expected, tolerance = 0.001)
})
