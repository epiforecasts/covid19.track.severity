#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


#' @import data.table
#' @import ggplot2
NULL

linear <- function(baseline, scale, time) {
  y <- baseline + scale * (1:time) / time
  return(y)
}