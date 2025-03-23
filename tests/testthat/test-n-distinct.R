test_that("n_distinct() basic", {
  withr::local_envvar(DUCKPLYR_FORCE = TRUE)

  df <- duckdb_tibble(
    x = c(1, 1, 2, 2, 2), 
    y = c(3, 3, NA, 3, 3)
  )

  out <- df |>
    summarise( n_distinct_x = n_distinct(x),
               n_distinct_x_na_rm = n_distinct(x, na.rm = TRUE),
               n_distinct_y = n_distinct(y, na.rm = FALSE),
               n_distinct_y_na_rm = n_distinct(y, na.rm = TRUE)
    )

  expect_equal(out$n_distinct_x, 2)
  expect_equal(out$n_distinct_x_na_rm, 2)
  expect_equal(out$n_distinct_y, 2)
  expect_equal(out$n_distinct_y_na_rm, 1)
})


test_that("n_distinct() counts NA correctly", {
  withr::local_envvar(DUCKPLYR_FORCE = TRUE)

  df <- duckdb_tibble(
    x = c(1, NA, 1, NA, 2, NA, 2), 
    y = c(3, 3, NA, 3, NA, 4, 5)
  )

  out <- df |>
    summarise( n_distinct_x = n_distinct(x),
               n_distinct_x_na_rm = n_distinct(x, na.rm = TRUE),
               n_distinct_y = n_distinct(y, na.rm = FALSE),
               n_distinct_y_na_rm = n_distinct(y, na.rm = TRUE)
    )

  expect_equal(out$n_distinct_x, 3)
  expect_equal(out$n_distinct_x_na_rm, 2)
  expect_equal(out$n_distinct_y, 4)
  expect_equal(out$n_distinct_y_na_rm, 3)
})
