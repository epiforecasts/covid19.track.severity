plot_trace <- function(draws, data = NULL,
                       samples = 100,
                       alpha = 0.01, obs_alpha = 0.8,
                       scale = "continuous", x_axis = TRUE,
                       scale_label = "scaling") {
  scale <- match.arg(scale, choice = c("continuous", "log", "percent"))
  draws <- as.data.table(draws)[, Source := "Model"]
  draws <- draws[sample <= samples]
  if (!is.null(data)) {
    data <- data[, Source := "Simulation"]
  }

  plot <- ggplot(draws) +
    aes(x = date, y = value, group = sample) +
    geom_line(size = 1.1, alpha = alpha) +
    theme_minimal() +
    labs(x = "Date", y = scale_label) +
    guides(size = NULL) +
    theme_cowplot() +
    scale_x_date(date_breaks = "2 week", date_labels = "%b %d") + 
    theme(axis.text.x = ggplot2::element_text(angle = 90)) + 
    theme(legend.position = "bottom") +
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    facet_wrap(~target, ncol = 1, scales = "free_y")

  if (!is.null(data)) {
    plot <- plot +
      geom_point(aes(group = NULL), alpha = obs_alpha, size = 1.2,
                 data = data, colour =  "black")
  }

  if (scale %in% "log") {
    plot <- plot + 
      scale_y_continuous(labels = comma, trans = log_trans()) +
      labs(x = "Date", y = paste0(scale_label, "(log scale)"))
  }else if (scale %in% "continuous") {
    plot <- plot +
      scale_y_continuous(labels = comma) +
      labs(x = "Date", y = scale_label)
  }else if (scale %in%  "percent") {
        plot <- plot +
      scale_y_continuous(labels = percent) +
      labs(x = "Date", y = scale_label)
  }

  if (!x_axis) {
    plot <- plot +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank())
  }

  return(plot)
}