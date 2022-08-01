test_that("testa conversao depreciacao para vida útil", {
  value <- depreciacao_para_vida_util(2)
  expected <- 50
  expect_equal(value, expected, tolerance=0.001)
})
