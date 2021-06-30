  # load observed data
  observations <- merge(primary, secondary, by = c("date", "region"))
  observations <- observations[date <= forecast_date]
  observations <- observations[!is.na(primary)][!is.na(secondary)]

  if (!is.null(obs_weeks)) {
    observations <- observations[,
      .SD[date >= (max(date) - lubridate::weeks(obs_weeks))],
      by = region
    ]
  }
  setorder(observations, date)
