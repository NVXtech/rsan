test_that("calculos estão rodando", {
  setwd(file.path("../../"))
  app_state <- list(input = get_default_input())
  state <- rodar_modelo(app_state)
  testthat::expect_true(!is.null(state))

  valores <- dplyr::group_by(state$necessidade, componente)
  valores <- dplyr::summarise(
    valores,
    total = sum(necessidade_investimento, na.rm = TRUE)
  )
  componente <- c("agua", "drenagem", "esgoto", "residuos")
  total <- c(240238483806., 478254161220., 334163032430., 122893122849.)
  expected <- dplyr::tibble(componente, total)
  # testthat::expect_equal(valores, expected)
  # Verifica totais por componente, situacao e destino
  valores <- dplyr::group_by(state$necessidade, componente, situacao, destino)
  valores <- dplyr::summarise(
    valores,
    total = sum(necessidade_investimento, na.rm = TRUE)
  )
  # valores$total <- format(
  #     valores$total,
  #     big.mark = ".",
  #     decimal.mark = ",",
  #     scientific = FALSE
  # )
  # writexl::write_xlsx(valores, "valores.xlsx")
  print(valores)
  componente <- c(
    rep("agua", 4),
    rep("drenagem", 3),
    rep("esgoto", 4),
    rep("residuos", 2)
  )
  situacao <- c(
    rep("rural", 2),
    rep("urbana", 5),
    rep("rural", 2),
    rep("urbana", 4)
  )
  padrao <- c("expansao", "reposicao")
  destino <- c(
    padrao,
    padrao,
    "cadastro",
    rep(padrao, 4)
  )
  # valores rsan 2023
  total <- c(
    29859415051., # agua rural exp
    35103461658., # agua rural rep
    59395545723., # agua urbana exp
    117668258473., # agua urbana rep
    180399640., # drenagem cadastro
    444239646456., # drenagem exp
    52509217245., # drenagem rep
    65755082810., # esgoto rural exp
    14632907004., # esgoto rural rep
    145858541196., # esgoto urbano exp
    107492713114., # esgoto urbano rep
    27239923848., # residuos exp
    60402903253. # residuos rep
  )
  expected <- dplyr::tibble(componente, situacao, destino, total)
  relativo <- dplyr::left_join(
    valores,
    expected,
    by = c("componente", "situacao", "destino"),
  )
  relativo <- dplyr::mutate(relativo, fator = abs(total.x - total.y) / total.x * 100)
  print(relativo, n = 13)
  testthat::expect_equal(tibble::as_tibble(valores), expected, tolerance = 1.0)
})
