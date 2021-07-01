
# load packages used
library(EpiNow2)
library(data.table)

# set cores
options(mc.cores = ifelse(interactive(), 4 ,1))


obs <- uk_notifications
obs <- obs[region %in% "England"]
obs <- obs[date >= "2020-09-01"][date < "2021-06-01"]
obs <- obs[, `:=`(primary = cases, secondary = deaths)][1:60]

fit <- estimate_secondary(reports = obs, chains = 2,
                         obs = obs_opts(scale = list(mean = 0.2, sd = 0.2)))

test_that("works as expected", {
  out <- posterior_samples(fit)
  expect_s3_class(out, "data.table")
  expect_equal(colnames(out), c("parameter", "time", "date", "sample", "value"))
  expect_true(class(out$date) == "Date")
  expect_true(class(out$parameter) == "character")
  expect_true(is.numeric(out$sample))
  expect_true(is.numeric(out$value))
})