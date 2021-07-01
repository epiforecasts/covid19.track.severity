#' Posterior samples from estimate_secondary
#'
#' @param x A `estimate_secondary` class model.
#'
#' @return Returns a data frame of posterior samples for time-varying parameters.
#' @importFrom purrr map2
#' @importFrom rstan extract
#' @method posterior_samples estimate_secondary
#' @examples
#' \dontrun{
#' # load packages used
#' library(EpiNow2)
#'
#' # set cores
#' options(mc.cores = ifelse(interactive(), 4 ,1))
#'
#' # fit using just data in the UK for cases and deaths
#' fit <- estimate_secondary(reports = example_obs, chains = 2,
#'                           obs = obs_opts(scale = list(mean = 0.2, sd = 0.2)))
#'
#' posterior_samples(fit)
#' }
posterior_samples.estimate_secondary <- function(x) {
  draws <- extract(fit$fit)
  vpars <- c("sim_secondary", "frac_obs", "delay_mean", "delay_sd")
  dates <- fit$predictions$date
  dates <- c(list(dates[!is.na(fit$predictions$mean)]),
                rep(list(dates), 3))
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
