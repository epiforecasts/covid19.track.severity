#' Plot posterior samples over time
#'
#' @param draws A data frame as produced by `posterior_samples()`
#' @param data A data frame of data raw data with date and value variables.
#' @param samples Integer, defaults to 100. Number of samples to plot.
#' @param alpha Numeric, fill intensity of the trace. Defaults to 0.01.
#' @param obs_alpha Numeric, fill intensity of the observed data. Defaults to
#' 0.8.
#' @param y_scale Character string indicating the scale to use on the y axis.
#' Options are "continuous", "log", or "percent". Default is "continuous".
#' @param x_axis Logical, should the x axis labelling be included. Default is
#' `TRUE`.
#' @param y_label Character string indicating the y axis label to use.
#' @importFrom scales percent comma
#' @importFrom cowplot theme_cowplot
#' @return A `ggplot2` object.
#' @examples
#' \dontrun{
#' # load packages used
#' library(EpiNow2)
#'
#' # set cores
#' options(mc.cores = ifelse(interactive(), 4 ,1))
#'
#' #fit using data in the UK for cases and deaths
#' fit <- estimate_secondary(reports = example_obs, chains = 2,
#'                           obs = obs_opts(scale = list(mean = 0.2, sd = 0.2)))
#'
#' draws <- posterior_samples(fit)
#' plot_trace(draws[parameter %in% "sim_secondary"],
#'            obs[, .(date, value = secondary)],
#'            y_label = "Admissions", samples = 100)
#' }
plot_trace <- function(draws, data = NULL,
                       samples = 100,
                       alpha = 0.01, obs_alpha = 0.8,
                       y_scale = "continuous", x_axis = TRUE,
                       y_label = "scaling") {
  y_scale <- match.arg(y_scale, choice = c("continuous", "log", "percent"))
  draws <- as.data.table(draws)[, Source := "Model"]
  draws <- draws[sample <= samples]
  if (!is.null(data)) {
    data <- as.data.table(data)[, Source := "Simulation"]
  }

  plot <- ggplot(draws) +
    aes(x = date, y = value, group = sample) +
    geom_line(size = 1.1, alpha = alpha) +
    theme_minimal() +
    labs(x = "Date", y = y_label) +
    guides(size = NULL) +
    theme_cowplot() +
    scale_x_date(date_breaks = "2 week", date_labels = "%b %d") +
    theme(axis.text.x = element_text(angle = 90)) +
    theme(legend.position = "bottom") +
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2")

  if (!is.null(data)) {
    plot <- plot +
      geom_point(aes(group = NULL), alpha = obs_alpha, size = 1.2,
                 data = data, colour =  "black")
  }

  if (y_scale %in% "log") {
    plot <- plot +
      scale_y_continuous(labels = comma, trans = log_trans()) +
      labs(x = "Date", y = paste0(y_label, "(log scale)"))
  }else if (y_scale %in% "continuous") {
    plot <- plot +
      scale_y_continuous(labels = comma) +
      labs(x = "Date", y = y_label)
  }else if (y_scale %in%  "percent") {
        plot <- plot +
      scale_y_continuous(labels = percent) +
      labs(x = "Date", y = y_label)
  }

  if (!x_axis) {
    plot <- plot +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank())
  }
  return(plot)
}