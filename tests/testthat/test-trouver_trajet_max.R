test_that("trouver_trajet_max retourne la ligne avec le plus grand total", {
  df_test <- tibble::tibble(
    `Boucle de comptage` = c("A", "B", "C"),
    Jour = c("2025-10-20", "2025-10-21", "2025-10-22"),
    Total = c(100, 250, 180)
  )

  res <- trouver_trajet_max(df_test)

  expect_equal(nrow(res), 1)
  expect_equal(res$Total, 250)
  expect_equal(res$`Boucle de comptage`, "B")
})

test_that("trouver_trajet_max garde les colonnes attendues", {
  df_test <- tibble::tibble(
    `Boucle de comptage` = c("A", "B"),
    Jour = c("2025-10-20", "2025-10-21"),
    Total = c(100, 250)
  )

  res <- trouver_trajet_max(df_test)

  expect_named(res, c("Boucle de comptage", "Jour", "Total"))
})
