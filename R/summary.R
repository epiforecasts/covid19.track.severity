#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param hosp DESCRIPTION.
#' @param window DESCRIPTION.
#' @param dates DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
summary.convolution_simulation <- function(hosp, window = 1, dates) {
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