test_that("filtrer_trajet retourne le dataset complet si boucle est NULL", {

  df_test <- tibble::tibble(
    Boucle = c("880", "881", "882"),
    valeur = c(10, 20, 30)
  )

  res <- filtrer_trajet(df_test, boucle = NULL)

  expect_equal(res, df_test)
})
