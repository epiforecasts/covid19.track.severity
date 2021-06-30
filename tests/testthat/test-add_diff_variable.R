dt <- data.frame(target = "test", var = 1:100, var2 = c(1, 2),
                 strat = c("a", "b"))

test_that("Simple use case works as expected", {
  out <- add_diff_variable(dt, "test", "new", vars = "var",
                           shift_var = "var2", by = "strat")
  old <- out[target %in% "test"][strat %in% "a"]
  new <- out[target %in% "new"][strat %in% "a"]
  expect_s3_class(out, "data.table")
  expect_equal(old$var - 1, new$var)
})

test_that("Arguments are optional", {
  test_fn <- purrr::partial(add_diff_variable,
    dt = dt, variable = "test",
    label = "new", shift_var = "var2")
  expect_error(test_fn(vars = "var"), NA)
  expect_error(test_fn(partial_vars = "var"), NA)
})
