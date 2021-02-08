# Packages ----------------------------------------------------------------
library(covid19.track.severity)
library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(future)
library(future.apply)

# set number of parallel cores
no_cores <- availableCores()
options(mc.cores = 4)
# min date
min_date <- as.Date("2020-10-01")

# Get data ----------------------------------------------------------------
levels <- tibble(level = c("DA", "NHS region"))

ratios <- tibble(
  ratio = c("cfr", "chr", "hfr"),
  obs_pairs = list(c("cases", "deaths"), c("cases", "admissions"),
                   c("admissions", "deaths"))
)

obs <- levels %>% 
  mutate(ratios = list(ratios)) %>% 
  unnest(cols = "ratios") %>% 
  mutate(data = map2(level, obs_pairs, 
                     ~ get_uk_notificiatons(.y[1], .y[2], level = .x, 
                                            date_from = min_date))
         )

model <- brm_convolution(secondary ~ 1 + (1 + time | loc), 
                         conv_varying = "loc",
                         prior = prior("normal(-4, 0.5)", class = "Intercept"),
                         data = obs %>% 
                           filter(level %in% "DA") %>% 
                           filter(ratio %in% "cfr") %>% 
                           pull(data) %>% 
                           .[[1]], save_pars = save_pars(manual = "mu"))


# Define model ------------------------------------------------------------
models <- tibble(
  model = c(),
  formula = c()
)
models <- list()
models[["intercept"]] <- as.formula(secondary ~ 1 + s(time))
models[["time"]] <- as.formula(secondary ~ 1 + s(time))
models[["loc"]] <- as.formula(secondary ~ (1 | loc) + s(time))
models[["all"]] <- as.formula(secondary ~ s(time, by = loc))

# Fit models --------------------------------------------------------------
# set up parallel
## core usage
if (no_cores <= 4) {
  stan_cores <- no_cores
  mc_cores <- 1
} else {
  stan_cores <- 4
  mc_cores <- ceiling(no_cores / 4)
}
plan("multisession", workers = mc_cores, earlySignal = TRUE)

#define context specific args
fit_brm_convolution <- function(formula, ...) {
  brm_convolution(formula, control = list(adapt_delta = 0.9, max_treedepth = 12),
                  iter = 2000, cores = stan_cores, ...)
}
# set context specific priors (based on mean in data)
priors <- list()
priors[["cfr"]] <- c(prior("normal(-4, 0.5)", class = "Intercept"))
priors[["chr"]] <- c(prior("normal(-2.5, 0.5)", class = "Intercept"))
priors[["hfr"]] <- c(prior("normal(-1, 0.5)", class = "Intercept"))

# model grid
fit_targets <- expand_grid(loc = c("region"), 
                           conv = c("fixed", "loc"), 
                           target = c("cfr", "chr", "hfr"))

# drop currently not tractable combinations
fit_targets <- fit_targets %>% 
  filter(!(loc %in% "utla" & conv %in% "loc"))

# fit models in parallel
fits <- future_lapply(1:nrow(fit_targets), function(i) {
  ft <- fit_targets[i, ]
  message("Fitting ", ft$target, " at the ", ft$loc, " level using following convolution: ", ft$conv)
  out <- list()
  fits <- suppressMessages(lapply(models, fit_brm_convolution,
                data = df[[ft$loc]][[ft$target]],
                prior = priors[[ft$target]],
                conv_varying = ft$conv))
  ft$models <- list(names(models))
  ft$fit <- list(fits)
  ft <- unnest(ft, cols = c("models", "fit"))
  return(ft)},
  future.scheduling = Inf, future.seed = TRUE)

fits <- reduce(fits, bind_rows)

# Save output -------------------------------------------------------------
saveRDS(fits, here("output", "track-ratios.rds"))