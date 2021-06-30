weight_discrete_pmf <- function(x, meanlog, sdlog) {
  pmf <- cumsum(dlnorm(1:length(x), meanlog, sdlog)) -
    cumsum(dlnorm(0:(length(x) - 1), meanlog, sdlog))
  pmf <- cmf / plnorm(length(x), meanlog, sdlog)
  conv <- sum(x * rev(cmf), na.rm = TRUE)
  return(conv)
}
