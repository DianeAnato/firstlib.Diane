test_that("compter_nombre_boucle compte bien les boucles distinctes", {
  df_test <- tibble::tibble(
    `Numéro de boucle` = c("880", "881", "880", "882")
  )

  res <- compter_nombre_boucle(df_test)

  expect_equal(res, 3)
})

test_that("compter_nombre_boucle retourne 1 si une seule boucle est présente", {
  df_test <- tibble::tibble(
    `Numéro de boucle` = c("880", "880", "880")
  )

  res <- compter_nombre_boucle(df_test)

  expect_equal(res, 1)
})
