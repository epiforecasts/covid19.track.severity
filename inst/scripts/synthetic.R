# load data.table for manipulation
library(data.table, quietly = TRUE, warn.conflicts = FALSE)
library(purrr, quietly = TRUE, warn.conflicts = FALSE)
library(EpiNow2, quietly = TRUE, warn.conflicts = FALSE)
library(rstan, quietly = TRUE, warn.conflicts = FALSE)
library(future, quietly = TRUE, warn.conflicts = FALSE)
library(here, quietly = TRUE, warn.conflicts = FALSE)
library(lubridate, quietly = TRUE, warn.conflicts = FALSE)
library(ggplot2, quietly = TRUE, warn.conflicts = FALSE)
library(cowplot, quietly = TRUE, warn.conflicts = FALSE)
library(scales, quietly = TRUE, warn.conflicts = FALSE)
library(forcats, quietly = TRUE, warn.conflicts = FALSE)
library(purrr, quietly = TRUE, warn.conflicts = FALSE)
library(patchwork, quietly = TRUE, warn.conflicts = FALSE)

# set number of cores to use
options(mc.cores = 4)

# load simulation function and iterator
source(here("regional-secondary/simulate-secondary.R"))
source(here("regional-secondary/secondary-posterior.R"))
source(here("regional-secondary/regional-secondary.R"))

# load hospital admissions
hosp <- setDT(readRDS(here("data/hospital_admissions.rds")))

# limit to England  only, filter and rename
hosp <- hosp[region == "England", .(date, primary = cases)]
hosp <- hosp[date <= as.Date("2021-06-01")]
hosp <- hosp[date >= as.Date("2020-07-01")]

# add assumed baseline distributions and scalings
hosp <- hosp[,
  `:=`(scaling = round(rnorm(.N, 0.3, 0.3 * 0.05), 3),
       meanlog = round(rnorm(.N, 1.8, 1 * 0.05), 3),
       sdlog = round(rnorm(.N, 0.5, 0.05), 3))]

# scenario helpers
linear <- function(baseline, scale, time) {
  y <- baseline + scale * (1:time) / time
  return(y)
}
# add scenarios
hosp <- hosp[, `:=`(scaling_mod = 1, meanlog_mod = 1, sdlog_mod = 1)]

# scaling scenario
hosp[date > "2020-09-01" & date <= "2020-11-01",
  `:=`(scaling_mod = linear(1, 0.5, .N))]
hosp[date > "2020-11-01" & date <= "2020-12-01",
  `:=`(scaling_mod = linear(1.5, -0.75, .N))]
hosp[date > "2020-12-01" & date <= "2021-02-01",
  `:=`(scaling_mod = linear(0.75, 1.25, .N))]
hosp[date > "2021-02-01" & date < "2021-04-01",
  `:=`(scaling_mod = linear(2, -1, .N))]
hosp[date > "2021-04-01",
  `:=`(scaling_mod = linear(1, -0.5, .N))]

# distribution scenarios
hosp[date > "2020-10-01" & date <= "2021-02-01",
  `:=`(meanlog_mod = 1.25, sdlog_mod = 1.25)]
hosp[date > "2021-02-01" & date <= "2021-04-01",
  `:=`(meanlog_mod = 1.5,
       sdlog_mod = 0.75)]
hosp[date > "2021-04-01",
  `:=`(meanlog_mod = 1,
       sdlog_mod = 1.25)]

# apply scenarios
hosp <- hosp[, `:=`(scaling = scaling * scaling_mod,
                    meanlog = meanlog * meanlog_mod,
                    sdlog = sdlog * sdlog_mod)]

# summarise scenarios
summarised_scenarios <- summarise_scenario(hosp)

fwrite(
  summarised_scenarios,
  here("format-forecast", "data", "synthetic", "simulation-parameters.csv")
)

# simulate targets
deaths <- simulate_secondary(hosp, type = "incidence")
beds <- simulate_secondary(hosp, type = "prevalence")

obs <- rbindlist(list("Fatalities" = deaths, "Hospital bed occupancy" = beds),
                 idcol = "target")
obs_long <- rbind(
  hosp[, .(target = "Hospitalisations", date, value = primary)],
  obs[, .(target, date, value = secondary)]
)


# define shared observation and delay arguments
shared_args <- list(
  obs = obs_opts(
    scale = list(mean = 0.2, sd = 0.1),
    week_effect = FALSE, family = "poisson",
    gp = gp_opts(order = "1", basis_prop = 0.2, step = 7,
                 ls_mean = 30, ls_sd = 14, alpha_sd = 0.1,
                 ls_min = 14, ls_max = 180)),
  delays = delay_opts(list(
    mean = 2, mean_sd = 0.5,
    sd = 0.75, sd_sd = 0.25, max = 30),
    mean = list(gp_opts(order = "0", basis_prop = 0.2, step = 28,
                        ls_min = 60, ls_mean = 180, ls_sd = 14,
                        alpha_sd = 0.1, ls_max = 360)),
    sd = list(gp_opts(order = "0", basis_prop = 0.2, step = 28,
                      ls_min = 60, ls_max = 360, ls_mean = 180,
                      ls_sd = 14, alpha_sd = 0.1)),
    cache = TRUE),
  control = list(adapt_delta = 0.95, max_treedepth = 15),
  CrIs = c(0.3, 0.6, 0.9),
  warmup = 250, iter = 1250
)

# fit
fits <- list()
fits[["Fatalities"]] <- do.call(estimate_secondary,
  c(list(
    reports = deaths,
    secondary = secondary_opts(type = "incidence")
    ),
    shared_args
  ))

fits[["Hospital bed occupancy"]] <- do.call(estimate_secondary,
  c(list(
    reports = beds,
    secondary = secondary_opts(type = "prevalence")
    ),
    shared_args
  ))

# Extract posterior samples and summarise

draws <- map2(fits, list(deaths, beds), posterior_draws)
draws <- rbindlist(draws, idcol = "target")
summarised_draws <- calc_summary_measures(
  draws, summarise_by = c("target", "parameter", "date"),
  CrIs = c(seq(0.1, 0.9, 0.1), 0.95)
)
num_col <- which(sapply(summarised_draws, is.numeric))
summarised_draws[, (num_col) := lapply(.SD, signif, digits = 3),
                   .SDcols = num_col]
# in built plots
posterior_plots <- map(fits, plot, primary = TRUE)
parameter_plots <- map(fits, ~ plot(.$fit))

# plot trace
scaling <- plot_trace(
  draws[parameter %in% "frac_obs"], scale = "percent",
  scale_label = "Ratio of admissions to fatalities/bed occupancy",
  data = copy(obs)[, value := scaling],
  samples = 250
)

ggsave(
  here("format-forecast", "figures", "synthetic", "scaling.pdf"),
  scaling,
  dpi = 330, height = 9, width = 9
)

meanlog <- plot_trace(
  draws[parameter %in% "delay_mean"],
  scale_label = "Log mean of the convolution distributon",
  data = copy(obs)[, value := meanlog],
  samples = 250
)

ggsave(
  here("format-forecast", "figures", "synthetic", "meanlog.pdf"),
  meanlog,
  dpi = 330, height = 9, width = 9
)
sdlog <- meanlog <- plot_trace(
  draws[parameter %in% "delay_sd"],
  scale_label = "Log standard deviation of the convolution distributon",
  data = copy(obs)[, value := sdlog]
)
ggsave(
  here("format-forecast", "figures", "synthetic", "sdlog.pdf"),
  sdlog,
  dpi = 330, height = 9, width = 9
)

# summarise and plot simulations and posteriors of simulated values
posterior_predictions <- draws[parameter %in% "sim_secondary"]

# add change in hospital bed occupancy
posterior_predictions <- add_diff_variable(
  posterior_predictions,
  variable = "Hospital bed occupancy",
  label = "Change in hospital bed occupancy",
  by = c("sample"),
  fill = NA
)

# make observations long format
obs_long <- add_diff_variable(
  obs_long,
  variable = "Hospital bed occupancy",
  label = "Change in hospital bed occupancy"
)

# plot model fits vs data
preds_plot <-
  plot_trace(posterior_predictions[target %in% "Hospitalisations"],
             obs_long[target %in% "Hospitalisations"],
             scale_label = "Notifications",
             x_axis = FALSE, scale = "log") +
  plot_trace(posterior_predictions[target %in% "Fatalities"],
             obs_long[target %in% "Fatalities"],
             scale_label = "Notifications",
             x_axis = FALSE, scale = "log") +
  plot_trace(posterior_predictions[target %in% "Hospital bed occupancy"],
             obs_long[target %in% "Hospital bed occupancy"],
             scale_label = "Hospital bed occupancy",
             x_axis = FALSE, scale = "log") +
  plot_trace(
    posterior_predictions[target %in% "Change in hospital bed occupancy"],
    obs_long[target %in% "Change in hospital bed occupancy"],
             scale_label = "Change in hospital bed occupancy",
             x_axis = TRUE, scale = "continuous") +
  plot_layout(ncol = 1) & facet_wrap(~target)

ggsave(
  here("format-forecast", "figures", "synthetic", "posterior-predictions.pdf"),
  preds_plot,
  dpi = 330, height = 12, width = 9
)