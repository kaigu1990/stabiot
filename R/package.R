#' `stabiot` Package
#'
#' `stabiot` Common Statistical Analysis for Clinical Trials in Biotech.
#'
"_PACKAGE"

#' @import checkmate
#' @importFrom lifecycle deprecated
#' @importFrom stats pbeta rbinom confint as.formula
#' @importFrom dplyr count add_count group_by
#' @importFrom rlang sym := .data
NULL

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.stabiot <- list(
    stabiot.precision.default = tibble::tribble(
      ~stat,     ~extra,
      "N",       0,
      "MEAN",    1,
      "SD",      2,
      "MEDIAN",  1,
      "MAX",     0,
      "MIN",     0,
      "Q1",      1,
      "Q3",      1,
      "EST",     1,
      "SE",      2,
      "DF",      0,
      "CI",      1,
      "RATIO",   1
    )
  )
  toset <- !(names(op.stabiot) %in% names(op))
  if (any(toset)) {
    options(op.stabiot[toset])
  }

  invisible()
}
