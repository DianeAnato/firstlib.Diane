library(tibble)

test_that("filtre_anomalie garde seulement les NA", {

  df_test <- tibble::tibble(
    `Probabilité de présence d'anomalies` = c(NA, 0.5, NA, 0.2)
  )

  res <- filtre_anomalie(df_test)

  expect_equal(nrow(res), 2)
  expect_true(all(is.na(res$`Probabilité de présence d'anomalies`)))
})


test_that("filtre_anomalie retourne vide si pas de NA", {

  df_test <- tibble::tibble(
    `Probabilité de présence d'anomalies` = c(0.1, 0.5)
  )

  res <- filtre_anomalie(df_test)

  expect_equal(nrow(res), 0)
})
