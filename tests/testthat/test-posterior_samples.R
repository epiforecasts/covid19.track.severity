test_that("works as expected", {
  out <- posterior_samples(example_fit)
  expect_s3_class(out, "data.table")
  expect_equal(colnames(out), c("parameter", "time", "date", "sample", "value"))
  expect_true(class(out$date) == "Date")
  expect_true(class(out$parameter) == "character")
  expect_true(is.numeric(out$sample))
  expect_true(is.numeric(out$value))
})