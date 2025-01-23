#' `stabiot` Package
#'
#' `stabiot` Common Statistical Analysis for Clinical Trials in Biotech.
#'
"_PACKAGE"

#' @import checkmate
#' @importFrom magrittr set_colnames set_rownames set_names
#' @importFrom lifecycle deprecated
#' @importFrom stats pbeta rbinom confint as.formula setNames coef quantile
#' @importFrom dplyr add_count arrange bind_rows case_when count distinct filter
#'  full_join left_join group_by mutate row_number rowwise select summarise ungroup
#'  everything any_of all_of between desc bind_cols bind_rows
#' @importFrom tidyr pivot_wider starts_with
#' @importFrom tibble tibble tribble enframe
#' @importFrom forcats fct fct_drop fct_reorder
#' @importFrom rlang sym syms := .data
#' @importFrom rtables analyze basic_table build_table in_rows rcell non_ref_rcell
#'  split_cols_by
#' @importFrom stringr str_c str_detect str_replace_all str_remove str_remove_all
#' @importFrom survival coxph strata
#' @importFrom survminer pairwise_survdiff
#' @importFrom lubridate ymd days
#' @importFrom utils combn
#' @importFrom formatters format_value
NULL

#' @importFrom survival Surv
#' @export
survival::Surv

utils::globalVariables(c(
  "ADT", "ADT.x", "ADT.y", "AVAL", "AVALC", "AVALC.x", "AVALC.y", ".", "label"
))

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.stabiot <- list(
    stabiot.aval.mapping = tibble::tribble(
      ~avalc_temp, ~aval_temp,
      "CR", 1,
      "PR", 2,
      "SD", 3,
      # "NON-CR/NON-PD",  4,
      "PD", 4,
      "NE", 5
    ),
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
