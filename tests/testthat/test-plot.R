out <- posterior_samples(example_fit)

test_that("works as expected", {
  expect_error(plot_trace(draws[parameter %in% "sim_secondary"],
                          example_obs[, .(date, value = secondary)],
                          y_label = "Admissions", samples = 100),
               NA)
  expect_error(plot_trace(draws[parameter %in% "sim_secondary"],
                          y_label = "Admissions", samples = 100),
               NA)
})