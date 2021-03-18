#' Get a grid of observations for each severity measure of interest and
#' spatial scale
#'
#' @inheritParams get_uk_notifications
#' @return A nested tibble containing paired observation data for each spatial
#' level and severity measure of interest.
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr mutate select
#' @importFrom purrr map2
#' @importFrom tidyr unnest
#' @importFrom rlang .data
get_uk_obs_grid <- function(date_from, date_to) {
  if (missing(date_from)) {
    date_from <- NULL
  }
  if (missing(date_to)) {
    date_to <- NULL
  }
  levels <- tibble(
    level = c("DA", "NHS region", "UTLA")
  )

  ratios <- tibble(
    ratio = c("cfr", "chr", "hfr"),
    obs_pairs = list(
      c("cases", "deaths"),
      c("cases", "admissions"),
      c("admissions", "deaths")
    )
  )

  obs <- levels %>%
    mutate(ratios = list(ratios)) %>%
    unnest(cols = "ratios") %>%
    mutate(data = map2(
      .data$level, .data$obs_pairs,
      ~ get_uk_notificiatons(.y[1], .y[2],
        level = .x,
        date_from = date_from,
        date_to = date_to
      )
    )) %>%
    select(-.data$obs_pairs)
  return(obs)
}
