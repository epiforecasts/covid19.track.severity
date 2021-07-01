
# load packages used
library(EpiNow2)
library(data.table)

# set cores
options(mc.cores = ifelse(interactive(), 4 ,1))

obs <- uk_notifications
obs <- obs[region %in% "England"]
obs <- obs[date >= "2020-09-01"][date < "2021-06-01"]
obs <- obs[, `:=`(primary = cases, secondary = deaths)]

fit <- estimate_secondary(reports = obs, chains = 2,
                         obs = obs_opts(scale = list(mean = 0.2, sd = 0.2)))
example_fit <- fit
usethis::use_data(example_fit, overwrite = TRUE)

example_obs <- obs
usethis::use_data(example_obs, overwrite = TRUE)
