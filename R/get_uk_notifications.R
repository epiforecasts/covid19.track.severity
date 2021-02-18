#' Get a combined data set of two epidemiological indicators by date of notification
#'
#' @param from "cases", "admissions" or "deaths"
#' @param to "cases", "admissions" or "deaths"
#' @param level Aggregation level of the data. Options are devolved authority
#' (DA), NHS region, and upper-tier local authority (UTLA).
#' @param date_from Date to filter from
#' @param date_to Date to filter to
#' @return A data frame containing primary and secondary notifications alongside
#' the proportion of cases that were SGTF positive.
#' @export
#' @importFrom dplyr select mutate filter group_by slice rename summarise inner_join
#' @importFrom tidyr drop_na
#' @importFrom vroom vroom
#' @importFrom lubridate floor_date weeks
#' @author Sam Abbott
get_uk_notificiatons <- function(from = c("cases", "admissions", "deaths"),
                                 to = c("cases", "admissions", "deaths"),
                                 level = c("UTLA", "NHS region", "DA"),
                                 date_from, date_to){

  ## Arguments ---------------------------------------------------------------
  from <- match.arg(from, choices = c("cases", "admissions", "deaths"))
  to <- match.arg(to, choices = c("cases", "admissions", "deaths"))
  level <- match.arg(level, choices = c("UTLA", "NHS region", "DA"))

  ## Data --------------------------------------------------------------------
  if (level %in% c("NHS region", "DA")) {
    base_url <-
      paste0("https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/",
             "master/subnational/united-kingdom/")
  }else{
    base_url <-
      paste0("https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/",
             "master/subnational/united-kingdom-local/")
  }

  data_path <- "/summary/reported_cases.csv"
  primary <- suppressMessages(
    vroom(paste0(base_url, from, data_path)) %>%
    select(location = region, date, primary = confirm)
    )

  secondary <- suppressMessages(
    vroom(paste0(base_url, to, data_path)) %>%
    select(location = region, date, secondary = confirm)
    )

  # link datasets
  reports <- primary %>%
    inner_join(secondary, by = c("location", "date")) %>%
      select(location, date, primary, secondary) %>%
      mutate(week_specimen = floor_date(date, "week", week_start = 1))

  das <- c("England", "Wales", "Scotland", "Northern Ireland")
  if (level %in% "NHS region") {
    reports <- reports %>%
      filter(!(location %in% das))
  }else if (level %in% "DA") {
    reports <- reports %>%
      filter(location %in% das)
  }

  reports <- reports %>%
    filter(!(location %in% "United Kingdom"))

  # make normalised predictors
  reports <- reports %>%
    mutate(normalised_primary = (primary - mean(primary)) / sd(primary),
           time = as.numeric(date),
           time = time - min(time),
           time = (time - mean(time)) / sd(time))

  if (!missing(date_from)) {
    if(!is.null(date_from)) {
      reports <- reports %>%
        filter(date >= date_from)
    }
  }

  if (!missing(date_to)) {
    if (!is.null(date_to)) {
      reports <- reports %>%
        filter(date <= date_to)
    }
  }
  return(reports)
}
