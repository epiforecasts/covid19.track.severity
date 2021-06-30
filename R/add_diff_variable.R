add_diff_variable <- function(dt, variable, label, by, fill = 0,
                              shift_col = "value",
                              exact_cols = c("median", "mean", "secondary",
                                         "value"),
                              partial_cols = c("lower_", "upper_")) {
  dt <- copy(dt)
  dt_alt <- dt[target %in% variable]

  cols <- colnames(dt_alt)
  target_cols <- intersect(cols, exact_cols)
  target_cols <- c(target_cols, grep(
    paste(partial_cols, collapse = "|"),
    cols, fixed = FALSE, value = TRUE
  ))

  if (missing(by)) {
    by <- "across"
    dt_alt[, across := 1]
  }

  dt_alt <- dt_alt[,
    (target_cols) := map(.SD, ~ . - shift(get(shift_col), fill = fill)),
    .SDcols = target_cols, by = by]
  dt_alt <- dt_alt[, target := label]
  dt_alt <- suppressWarnings(dt_alt[, across := NULL])
  dt <- rbind(dt, dt_alt)
  return(dt)
}
