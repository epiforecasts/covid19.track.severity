#' Add the lagged difference between multiple variables to a data frame
#'
#' @description Differences multiple variables (either exactly or partially
#' defined) by another variable for a specified lag and stratification.
#' @param dt A `data.table` containing at a `target` variable and
#' the variables identified in the other arguments.
#' @param variable Character string identifying the variable in
#' target to calculate the lagged difference for.
#' @param label Character string giving the name for the new
#' lagged variable.
#' @param by An optional character vector identifying the variables to
#' stratify by.
#' @param shift_var Character string, indicating the variable to use to
#' calculate the difference.
#' @param vars Character vector giving the exact names of variables to
#' calculate the difference for.
#' @param partial_vars Character vector giving the partial names of variables to
#' calculate the difference for.
#' @inheritParams data.table::shift
#' @importFrom purrr map
#' @return The input `data.table` combined with the new differenced
#' variable.
#' @importFrom data.table := .SD
#' @examples
#' dt <- data.frame(target = "test", var = 1:100, var2 = c(1, 2),
#'                  strat = c("a", "b"))
#' add_diff_variable(dt, "test", "new", vars = "var", shift_col = "var2",
#'                   by = "strat")
add_diff_variable <- function(dt, variable, label, by, shift_var = "value",
                              vars = c("median", "mean", "secondary", "value"),
                              partial_vars = c("lower_", "upper_"),
                              fill = 1, n = 1L, type = "lag") {
  across <- target <- NULL
  dt <- as.data.table(dt)
  dt <- copy(dt)
  dt_alt <- dt[target %in% variable]

  cols <- colnames(dt_alt)
  target_cols <- c()
  if (!missing(vars)) {
    target_cols <- c(target_cols, intersect(cols, vars))
  }
  if (!missing(partial_vars)) {
      target_cols <- c(target_cols,
      grep(paste(partial_vars, collapse = "|"),
           cols, fixed = FALSE, value = TRUE))
  }
  if (missing(by)) {
    by <- "across"
    dt_alt[, across := 1]
  }

  dt_alt <- dt_alt[,
    (target_cols) := map(.SD, ~ . - shift(get(shift_var), fill = fill,
                                          n = n, type = type)),
    .SDcols = target_cols, by = by]
  dt_alt <- dt_alt[, target := label]
  dt_alt <- suppressWarnings(dt_alt[, across := NULL])
  dt <- rbind(dt, dt_alt)
  return(dt)
}
