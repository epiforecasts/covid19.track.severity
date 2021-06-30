#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param fit DESCRIPTION.
#' @param obs DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
posterior_draws <- function(fit, obs) {
  draws <- extract(fit$fit)
  temporal_params <- c("sim_secondary", "frac_obs", "delay_mean", "delay_sd")
  dates <- c(list(obs$date[(fit$data$burn_in + 1):nrow(obs)]),
                rep(list(obs$date), 3))
  temporal_draws <- map2(temporal_params, dates,
                         ~ EpiNow2:::extract_parameter(.x, draws, .y))
  temporal_draws <- rbindlist(temporal_draws)
  return(temporal_draws)
}