test_that("compter_nombre_trajets calcule bien la somme des trajets", {
  df_test <- tibble::tibble(
    Total = c(10, 20, 30)
  )

  res <- compter_nombre_trajets(df_test)

  expect_equal(res, 60)
})

test_that("compter_nombre_trajets retourne 0 si tous les trajets sont nuls", {
  df_test <- tibble::tibble(
    Total = c(0, 0, 0)
  )

  res <- compter_nombre_trajets(df_test)

  expect_equal(res, 0)
})
