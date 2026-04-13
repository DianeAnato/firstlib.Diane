test_that("plot_distribution_semaine retourne un objet ggplot", {
  df_test <- tibble::tibble(
    `Jour de la semaine` = c("1", "2", "3"),
    Total = c(10, 20, 30),
    `Probabilité de présence d'anomalies` = c(NA, NA, NA)
  )

  res <- plot_distribution_semaine(df_test)

  expect_s3_class(res, "ggplot")
})

test_that("plot_distribution_semaine fonctionne avec des anomalies non retenues", {
  df_test <- tibble::tibble(
    `Jour de la semaine` = c("1", "1", "2"),
    Total = c(10, 15, 20),
    `Probabilité de présence d'anomalies` = c(NA, 0.3, NA)
  )

  res <- plot_distribution_semaine(df_test)

  expect_s3_class(res, "ggplot")
})
