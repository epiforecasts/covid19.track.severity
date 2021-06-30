
#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param simulations DESCRIPTION.
#' @param labels DESCRIPTION.
#' @param to_week DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
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