# duckdb n_distinct() error with more than one argument

    Code
      summarise(df, dummy = n_distinct(a, b))
    Condition
      Error in `summarise()`:
      ! `n_distinct()` needs exactly one argument besides the optional `na.rm`

# duckdb n_distinct() error with mutate

    Code
      mutate(df, dummy = n_distinct(a))
    Condition
      Error in `mutate()`:
      ! `n_distinct()` not supported in window functions

