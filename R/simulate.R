#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param object DESCRIPTION.
#' @param type DESCRIPTION.
#' @param obs_model DESCRIPTION.
#' @param delay_max DESCRIPTION.
#' @param ... DESCRIPTION.
#' @method simulate convolution_scenario
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
simulate.convolution_scenario <- function(object, type = "incidence",
                                          obs_model = "poisson",
                                          delay_max = 30, ...) {
  type <- match.arg(type, choices = c("incidence", "prevalence"))
  obs_model <- match.arg(obs_model, choices = c("none", "poisson", "negbin"))
  data <- as.data.table(object)
  data <- copy(data)
  data <- data[, index := 1:.N]
  # apply scaling
  data <- data[, scaled := scaling * primary]
  # add convolution
  data <- data[,
    conv := pmap_dbl(list(i = index, m = meanlog, s = sdlog),
     function(i, m, s) {
       weight_discrete_pmf(scaled[max(1, i - delay_max):i],
                  meanlog = m, sdlog = s)
     })]
  # build model
  if (type == "incidence") {
    data <- data[, secondary := conv]
  }else if (type == "prevalence") {
    data <- data[1, secondary := scaled]
    for (i in 2:nrow(data)) {
      index <-
        data[c(i - 1, i)][, secondary := shift(secondary, 1) - conv]
      index <- index[secondary < 0, secondary := 0]
      data[i, ] <- index[2][, secondary := secondary + scaled]
    }
  }
  # check secondary is greater that zero
  data <- data[secondary < 0, secondary := 0]
  data <- data[!is.na(secondary)]
  # apply observation model
  if (obs_model == "poisson") {
    data <- data[, secondary := map_dbl(secondary, ~ rpois(1, .))]
  }else if (obs_model == "negbin") {
    data <- data[, secondary := map_dbl(secondary, ~ rnbinom(1, mu = .), ...)]
  }
  data <- data[, secondary := as.integer(secondary)]
  return(data)
}
