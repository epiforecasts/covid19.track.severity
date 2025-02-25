% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot_trace}
\alias{plot_trace}
\title{Plot posterior samples over time}
\usage{
plot_trace(
  draws,
  data = NULL,
  samples = 100,
  alpha = 0.01,
  obs_alpha = 0.8,
  y_scale = "continuous",
  x_axis = TRUE,
  y_label = "scaling"
)
}
\arguments{
\item{draws}{A data frame as produced by \code{posterior_samples()}}

\item{data}{A data frame of data raw data with date and value variables.}

\item{samples}{Integer, defaults to 100. Number of samples to plot.}

\item{alpha}{Numeric, fill intensity of the trace. Defaults to 0.01.}

\item{obs_alpha}{Numeric, fill intensity of the observed data. Defaults to
0.8.}

\item{y_scale}{Character string indicating the scale to use on the y axis.
Options are "continuous", "log", or "percent". Default is "continuous".}

\item{x_axis}{Logical, should the x axis labelling be included. Default is
\code{TRUE}.}

\item{y_label}{Character string indicating the y axis label to use.}
}
\value{
A \code{ggplot2} object.
}
\description{
Plot posterior samples over time
}
\examples{
\dontrun{
# load packages used
library(EpiNow2)

# set cores
options(mc.cores = ifelse(interactive(), 4 ,1))

#fit using data in the UK for cases and deaths
fit <- estimate_secondary(reports = example_obs, chains = 2,
                          obs = obs_opts(scale = list(mean = 0.2, sd = 0.2)))

draws <- posterior_samples(fit)
plot_trace(draws[parameter \%in\% "sim_secondary"],
           obs[, .(date, value = secondary)],
           y_label = "Admissions", samples = 100)
}
}
