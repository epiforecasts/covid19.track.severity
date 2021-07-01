#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param x DESCRIPTION.
#' @param meanlog DESCRIPTION.
#' @param sdlog DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
weight_discrete_pmf <- function(x, meanlog, sdlog) {
  pmf <- cumsum(dlnorm(1:length(x), meanlog, sdlog)) -
    cumsum(dlnorm(0:(length(x) - 1), meanlog, sdlog))
  pmf <- pmf / plnorm(length(x), meanlog, sdlog)
  conv <- sum(x * rev(mf), na.rm = TRUE)
  return(conv)
}
