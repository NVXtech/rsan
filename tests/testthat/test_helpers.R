test_that("classes shiny são removidas da lista", {
  lista <- list(
    name = "character",
    valor = 2.12341,
    numero = 1,
    lista = list(),
    df = data.frame(),
    shiny = 1
  )
  class(lista$shiny) <- "shinyActionButtonValue"
  nova <- remove_shiny_classes(lista)
  expect_true(is.na(match("shiny", names(nova))))
  expect_true(length(nova) == length(lista) - 1)
})
