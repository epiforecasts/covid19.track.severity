
# summarise simulated scenarios
summarise_scenario <- function(hosp, window = 1, dates) {
  summarised_scenarios <- copy(hosp)[, .(date, scaling, meanlog, sdlog)]
  summarised_scenarios <- melt(summarised_scenarios, id.vars = "date")
  cris <- function(index, window, x) {
    x <- data.table(value = x[max(1, index - window + 1):index], type = "temp")
    cris <-
      EpiNow2::calc_summary_measures(x, CrIs = c(seq(0.1, 0.9, 0.1), 0.95))
    return(cris)
  }
  summarised_scenarios <- summarised_scenarios[,
    .(date = date, summary = map(1:.N, ~ cris(index = ., window, x = value))),
      by = c("variable")]
  summarised_scenarios <- summarised_scenarios[,
    rbindlist(summary), by = c("date", "variable")][, type := NULL]
  if (!missing(dates)) {
    summarised_scenarios <-
    summarised_scenarios[as.character(date) %in% as.character(dates)]
  }

  setnames(summarised_scenarios, "date", "target_date")
  return(summarised_scenarios)
}

# join multiple simulations together
join_simulations <- function(simulations, labels, to_week = FALSE) {
  simulations <- map2(simulations, labels, ~ .x[, target := .y])
  simulations <- rbindlist(simulations, fill = TRUE, use.names = TRUE)

  if (to_week) {
    simulations <- simulations[,
      date := floor_date(date, "week", week_start = 1)]
    simulations <- simulations[, lapply(.SD, sum),
                                by = c("date", "target"),
                                .SDcols = c("secondary"), ]
  }
  simulations <- simulations[, target : as.factor(target)]
  return(simulations)
}