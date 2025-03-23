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

  out <- df |>
    mutate( n_distinct_x = n_distinct(x),
            n_distinct_x_na_rm = n_distinct(x, na.rm = TRUE),
            n_distinct_y = n_distinct(y, na.rm = FALSE),
            n_distinct_y_na_rm = n_distinct(y, na.rm = TRUE)
    )

  expect_equal(out$n_distinct_x, c(2, 2, 2, 2, 2))
  expect_equal(out$n_distinct_x_na_rm, c(2, 2, 2, 2, 2))
  expect_equal(out$n_distinct_y, c(2, 2, 2, 2, 2))
  expect_equal(out$n_distinct_y_na_rm, c(1, 1, 1, 1, 1))
})


test_that("n_distinct() counts empty inputs", {
  withr::local_envvar(DUCKPLYR_FORCE = TRUE)

  df <- duckdb_tibble(
    a = integer(), 
    b = double(), 
    c = logical(), 
    d = character()
  )

  out <- df |>
    summarise( n_distinct_a = n_distinct(a),
               n_distinct_b = n_distinct(b),
               n_distinct_c = n_distinct(c),
               n_distinct_d = n_distinct(d),
    )

  expect_equal(out$n_distinct_a, 0)
  expect_equal(out$n_distinct_b, 0)
  expect_equal(out$n_distinct_c, 0)
  expect_equal(out$n_distinct_d, 0)

  out <- df |>
    mutate( n_distinct_a = n_distinct(a),
            n_distinct_b = n_distinct(b),
            n_distinct_c = n_distinct(c),
            n_distinct_d = n_distinct(d),
    )

  expect_equal(out$n_distinct_a, 0)
  expect_equal(out$n_distinct_b, 0)
  expect_equal(out$n_distinct_c, 0)
  expect_equal(out$n_distinct_d, 0)
})


test_that("n_distinct() counts unique values in simple vectors", {
  withr::local_envvar(DUCKPLYR_FORCE = TRUE)

  df <- duckdb_tibble(
    a = c(TRUE, FALSE, NA), 
    b = c(1, 2, NA), 
    c = c(1L, 2L, NA), 
    d = c("x", "y", NA)
  )

  out <- df |>
    summarise( n_distinct_a = n_distinct(a),
               n_distinct_b = n_distinct(b),
               n_distinct_c = n_distinct(c),
               n_distinct_d = n_distinct(d),
    )

  expect_equal(out$n_distinct_a, 3)
  expect_equal(out$n_distinct_b, 3)
  expect_equal(out$n_distinct_c, 3)
  expect_equal(out$n_distinct_d, 3)

  out <- df |>
    mutate( n_distinct_a = n_distinct(a),
            n_distinct_b = n_distinct(b),
            n_distinct_c = n_distinct(c),
            n_distinct_d = n_distinct(d),
    )

  expect_equal(out$n_distinct_a, 3)
  expect_equal(out$n_distinct_b, 3)
  expect_equal(out$n_distinct_c, 3)
  expect_equal(out$n_distinct_d, 3)

})


test_that("n_distinct() can drop missing values", {
  withr::local_envvar(DUCKPLYR_FORCE = TRUE)

  df <- duckdb_tibble(
    a = c(NA), 
    b = c(NA, 0), 
  )

  out <- df |>
    summarise( n_distinct_a = n_distinct(a, na.rm = TRUE),
               n_distinct_b = n_distinct(b, na.rm = TRUE),
    )

  expect_equal(out$n_distinct_a, 0)
  expect_equal(out$n_distinct_b, 1)

  out <- df |>
    mutate( n_distinct_a = n_distinct(a, na.rm = TRUE),
            n_distinct_b = n_distinct(b, na.rm = TRUE),
    )

  expect_equal(out$n_distinct_a, c(0, 0))
  expect_equal(out$n_distinct_b, c(1, 1))

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

  out <- df |>
    mutate( n_distinct_x = n_distinct(x),
            n_distinct_x_na_rm = n_distinct(x, na.rm = TRUE),
            n_distinct_y = n_distinct(y, na.rm = FALSE),
            n_distinct_y_na_rm = n_distinct(y, na.rm = TRUE)
    )

  expect_equal(out$n_distinct_x, c(3, 3, 3, 3, 3, 3, 3))
  expect_equal(out$n_distinct_x_na_rm, c(2, 2, 2, 2, 2, 2, 2))
  expect_equal(out$n_distinct_y, c(4, 4, 4, 4, 4, 4, 4))
  expect_equal(out$n_distinct_y_na_rm, c(3, 3, 3, 3, 3, 3, 3))
})


test_that("n_distinct() error with more than one argument", {
  withr::local_envvar(DUCKPLYR_FORCE = TRUE)

  df <- duckdb_tibble(
    x = c(1, 1, 2, 2, 2), 
    y = c(3, 3, NA, 3, 3)
  )

  expect_error(df |> summarise( dummy = n_distinct(x, y) ))
  expect_error(df |> mutate( dummy = n_distinct(x, y) ))
})

