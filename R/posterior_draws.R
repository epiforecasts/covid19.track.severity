#' Posterior samples from estimate_secondary
#'
#' @param x A `estimate_secondary` class model.
#'
#' @return Returns a data frame of posterior samples. If the optional
#' `vpars` argument is used then returns samples joined with dates.
#' @importFrom purrr map2
#' @importFrom rstan extract
#' @examples
posterior_samples.estimate_secondary <- function(x
  vpars = c("sim_secondary", "frac_obs", "delay_mean", "delay_sd")) {
  draws <- extract(fit$fit)
  dates <- c(list(obs$date[(fit$data$burn_in + 1):nrow(obs)]),
                rep(list(obs$date), 3))
  draws <- map2(vpars, dates,
                  ~ EpiNow2:::extract_parameter(.x, draws, .y))
  draws <- rbindlist(draws)
  return(draws)
}

#' @rdname posterior_samples.estimate_secondary
#' @export 
posterior_samples <- function(x, ...) {
  UseMethod("posterior_samples")
}
