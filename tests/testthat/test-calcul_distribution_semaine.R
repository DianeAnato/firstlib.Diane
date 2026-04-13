test_that("calcul_distribution_semaine ne filtre pas si filtre = FALSE", {
  df_test <- tibble::tibble(
    `Jour de la semaine` = c("1", "1", "2"),
    Total = c(10, 15, 20),
    `Probabilité de présence d'anomalies` = c(NA, 0.4, NA)
  )

  res <- calcul_distribution_semaine(df_test, filtre = FALSE)
  res <- dplyr::arrange(res, `Jour de la semaine`)

  expect_equal(nrow(res), 2)
  expect_equal(res$trajets, c(25, 20))
})

test_that("calcul_distribution_semaine filtre les anomalies si filtre = TRUE", {
  df_test <- tibble::tibble(
    `Jour de la semaine` = c("1", "1", "2"),
    Total = c(10, 15, 20),
    `Probabilité de présence d'anomalies` = c(NA, 0.4, NA)
  )

  res <- calcul_distribution_semaine(df_test, filtre = TRUE)
  res <- dplyr::arrange(res, `Jour de la semaine`)

  expect_equal(nrow(res), 2)
  expect_equal(res$trajets, c(10, 20))
})
