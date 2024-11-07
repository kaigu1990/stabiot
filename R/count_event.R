# s_count_event ----

#' Count the Number of Events for Specific Variable
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This function counts the number of events occur for specific variable with
#' multiple conditions if needed, such as counting the different types of AE
#' occurrence rates.
#'
#' @param data (`data.frame`)\cr a data frame as input.
#' @param var (`string`)\cr target variable name for counting.
#' @param by (`string`)\cr an optional variable to group by. If null, use the whole data.
#' @param cond (`string`)\cr a listing contains multiple types of filtering conditions
#'  that only support the equation such as `"TRTEMFL" = "Y"`. And the left side of the
#'  equation is filtering id while the right should be a flag, string or numerical value.
#' @param label (`string`)\cr an optional string vector for labeling each filtering
#'  conditions. Default is the name of `cond` variable.
#' @param denom (`string` or `data.frame`)\cr denominator for proportion can be a
#'  numeric vector of denominators or a data frame where we can count the `var` inside.
#'
#' @return
#' * `s_count_event` returns an object of class `count_evt` that is a data frame
#'  contains percentages for each group for different conditions.
#'
#' @note
#' - The order of `cond` names should be one-to-one respect to `label` you define.
#' - The order of `denom` names should be one-to-one respect to `by` variable levels.
#'
#' @export
#'
#' @examples
#' data("rand_adsl")
#' data("rand_adae")
#'
#' # by TRTA groups
#' s_count_event(
#'   data = rand_adae, var = "SUBJID", by = "ARMCD",
#'   cond = list(
#'     "TEAEs" = c("TRTEMFL" = "Y"),
#'     "TRAEs" = c("TRTEMFL" = "Y", "AEREL" = "Y"),
#'     "SAE" = c("AESER" = "Y"),
#'     "TRSAE" = c("AESER" = "Y", "AEREL" = "Y")
#'   ),
#'   label = c(
#'     "Any TEAEs", "Any treatment-related TEAEs",
#'     "Any serious TEAEs", "Any serious treatment-related TEAEs"
#'   ),
#'   denom = rand_adsl
#' )
#'
#' # specify the denominator for each groups
#' s_count_event(
#'   data = rand_adae, var = "SUBJID", by = "ARMCD",
#'   cond = list("TEAEs" = c("TRTEMFL" = "Y")),
#'   label = c("Any TEAEs"),
#'   denom = c(100, 100, 100)
#' )
#'
#' # no grouping
#' s_count_event(
#'   data = rand_adae, var = "SUBJID",
#'   cond = list(
#'     "TEAEs" = c("TRTEMFL" = "Y"),
#'     "TRAEs" = c("TRTEMFL" = "Y", "AEREL" = "Y")
#'   ),
#'   label = c("Any TEAEs", "Any treatment-related TEAEs"),
#'   denom = 200
#' )
s_count_event <- function(data,
                          var,
                          by = NULL,
                          cond,
                          label = names(cond),
                          denom) {
  assert_class(data, "data.frame")
  assert_subset(var, names(data), empty.ok = FALSE)
  assert_subset(by, names(data))
  assert_true(length(names(cond)) == length(label))
  assert_multi_class(denom, c("numeric", "data.frame"))
  if (is.null(by) & is.data.frame(denom)) {
    stop("denom should be numeric vector if by is defined as NULL.")
  }

  cond_labels <- split(label, names(cond))
  if (is.null(by)) {
    by <- "Total"
    data[["Total"]] <- factor(by, levels = by)
  } else {
    if (!is.factor(data[[by]])) {
      data[[by]] <- factor(data[[by]], levels = unique(data[[by]]))
    }
  }
  denom_vec <- if (is.data.frame(denom)) {
    if (!is.null(by)) {
      cnt <- denom %>% count(!!sym(by))
      set_names(cnt[, "n", drop = TRUE], cnt[, 1, drop = TRUE])
    } else {
      set_names(length(denom[[var]]), levels(data[[by]]))
    }
  } else {
    set_names(denom, levels(data[[by]]))
  }

  assert_numeric(denom_vec)
  assert_true(all(names(denom_vec) == levels(data[[by]])))

  cnt_tb <- cond %>%
    purrr::imap(function(x, idx) {
      filter_expr <- if (is.character(x)) {
        paste0(names(x), " == '", x, "'", collapse = " & ")
      } else if (is.numeric(x)) {
        paste0(names(x), " == ", x, collapse = " & ")
      } else {
        stop("Each element of cond variable should be numeric or characteric.")
      }
      df <- data %>%
        filter(!!rlang::parse_quo(filter_expr, env = rlang::global_env()))
      attributes(df)$label <- NULL
      res <- if (!is.null(by)) {
        df %>%
          distinct(!!sym(by), !!sym(var), .keep_all = TRUE) %>%
          count(!!sym(by))
      } else {
        df %>%
          distinct(!!sym(var), .keep_all = TRUE) %>%
          count()
      }
      res %>%
        mutate(
          group = droplevels(!!sym(by)),
          N = as.vector(denom_vec),
          perc = .data$n / .data$N,
          label_ = idx,
          label = cond_labels[[idx]]
        ) %>%
        ungroup() %>%
        select(c("group", "n", "N", "perc", "label_", "label"))
    }) %>%
    purrr::list_rbind()

  structure(
    list(
      data = data,
      cnt = cnt_tb,
      params = list(
        var = var,
        by = by,
        cond = cond,
        label = label,
        denom = denom_vec
      )
    ),
    class = "count_evt"
  )
}
