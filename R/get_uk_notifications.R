#' Get a combined data set of two epidemiological indicators by date of notification
#'
#' @param from "cases", "admissions" or "deaths"
#' @param to "cases", "admissions" or "deaths"
#' @param level Aggregation level of the data
#' @param date_from Date to filter from
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
                                   level = c("utla", "NHS region", "DA"), date_from){
  
  ## Arguments ---------------------------------------------------------------
  from <- match.arg(from, choices = c("cases", "admissions", "deaths"))
  to <- match.arg(to, choices = c("cases", "admissions", "deaths"))
  level <- match.arg(level, choices = c("utla", "NHS region", "DA"))
  
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
    select(loc = region, date, primary = confirm)
    )
  
  secondary <- suppressMessages(
    vroom(paste0(base_url, to, data_path)) %>% 
    select(loc = region, date, secondary = confirm)
    )

  # link datasets 
  reports <- primary %>% 
    inner_join(secondary, by = c("loc", "date")) %>% 
      select(loc, date, primary, secondary) %>% 
      mutate(week_specimen = floor_date(date, "week", week_start = 1))
  
  das <- c("England", "Wales", "Scotland", "Northern Ireland")
  if (level %in% "NHS region") {
    reports <- reports %>% 
      filter(!(loc %in% das))
  }else if (level %in% "DA") {
    reports <- reports %>% 
      filter(loc %in% das) 
  }
  
  reports <- reports %>% 
    filter(!(loc %in% "United Kingdom"))
  
  # make normalised predictors
  reports <- reports %>% 
    mutate(normalised_primary = (primary - mean(primary)) / sd(primary),
           time = as.numeric(date),
           time = time - min(time),
           time = (time - mean(time)) / sd(time))
  
  if (!missing(from)) {
    reports <- reports %>% 
      filter(date >= date_from)
  }
  return(reports)
}