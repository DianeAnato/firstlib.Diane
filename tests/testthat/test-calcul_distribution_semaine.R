test_that("calcul_distribution_semaine calcule bien les trajets par jour", {
  df_test <- tibble::tibble(
    `Jour de la semaine` = c("1", "1", "2", "2", "2"),
    Total = c(10, 15, 20, 5, 5)
  )

  res <- calcul_distribution_semaine(df_test)

  res <- dplyr::arrange(res, `Jour de la semaine`)

  expect_equal(nrow(res), 2)
  expect_equal(res$trajets, c(25, 30))
})

test_that("calcul_distribution_semaine crée bien la colonne trajets", {
  df_test <- tibble::tibble(
    `Jour de la semaine` = c("1", "2"),
    Total = c(10, 20)
  )

  res <- calcul_distribution_semaine(df_test)

  expect_true("trajets" %in% names(res))
})
