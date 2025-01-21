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
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

# h_prep_prop ----

#' @describeIn prop_odds_ratio Helper Function for Pre-processing Proportion Data.
#'
#' @export
#'
h_prep_prop <- function(data,
                        var,
                        by,
                        by.level,
                        resp) {
  if (is.null(by)) {
    by <- "Total"
    data[[by]] <- "Total"
  } else {
    if (!is.null(by.level)) {
      assert_set_equal(by.level, as.character(unique(data[[by]])))
      data[[by]] <- factor(data[[by]], levels = by.level)
    } else {
      if (is.factor(data[[by]])) {
        data[[by]] <- droplevels(data[[by]])
        by.level <- levels(data[[by]])
      } else {
        data[[by]] <- factor(data[[by]], levels = unique(data[[by]]))
        by.level <- levels(data[[by]])
      }
    }
  }

  if (is.null(resp)) {
    resp <- if (is.numeric(data[[var]])) {
      max(unique(data[[var]])[unique(data[[var]]) > 0])
    } else if (is.character(resp)) {
      unique(data[[var]])[1]
    } else if (is.factor(data[[var]])) {
      levels(data[[var]])[1]
    }
  }

  list(
    data = data,
    by = by,
    by.level = by.level,
    resp = resp
  )
}

#' @describeIn s_get_survfit a modified version of [survminer::pairwise_survdiff()]
#' that can calculates the pairwise comparisons of survival curves regardless of
#' whether stratification is given.
#'
#' @export
h_pairwise_survdiff <- function(formula,
                                data,
                                strata = NULL,
                                rho = 0) {
  group_var <- attr(stats::terms(formula), "term.labels")
  formula <- if (!is.null(strata)) {
    as.formula(
      paste0(format(formula), " + strata(", paste(strata, collapse = ", "), ")")
    )
  } else {
    formula
  }

  method <- if (rho == 0) {
    "Log-Rank"
  } else if (rho == 1) {
    "Peto & Peto"
  }

  vardf <- unlist(data[, group_var])
  group <- if (!is.factor(vardf)) {
    droplevels(as.factor(vardf))
  } else {
    droplevels(vardf)
  }

  compare.levels <- function(i, j) {
    subdf <- data %>% filter(!!sym(group_var) %in% c(levels(group)[c(i, j)]))
    sdif <- survival::survdiff(formula, data = subdf, rho = rho)
    stats::pchisq(sdif$chisq, length(sdif$n) - 1, lower.tail = FALSE)
  }

  level.names <- levels(group)
  ix <- setNames(seq_along(level.names), level.names)
  pval <- outer(
    ix[-1L], ix[-length(ix)],
    function(ivec, jvec) {
      vapply(seq_along(ivec), function(k) {
        i <- ivec[k]
        j <- jvec[k]
        if (i > j) {
          compare.levels(i, j)
        } else {
          NA_real_
        }
      }, FUN.VALUE = 0.05)
    }
  )

  list(
    method = ifelse(!is.null(strata), paste("Stratified", method), method),
    p.value = pval
  )
}

#' Format Count and Percent
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Help function to format the count and percent into one string.
#'
#' @param cnt (`numeric`)\cr numeric vector for count.
#' @param perc (`numeric`)\cr numeric vector for percent, if Null only format count.
#' @param format (`string`)\cr formatting string from `formatters::list_valid_format_labels()`
#'  for `formatters::format_value()` function.
#' @param ... other arguments to be passed to [formatters::format_value].
#'
#' @return A character vector of formatted counts and percents.
#' @export
#'
#' @examples
#' h_fmt_count_perc(cnt = c(5, 9, 12, 110, 0), format = "xx")
#' h_fmt_count_perc(
#'   cnt = c(5, 9, 12, 110, 0),
#'   perc = c(0.0368, 0.0662, 0.0882, 0.8088, 0),
#'   format = "xx (xx.x%)"
#' )
h_fmt_count_perc <- function(cnt, perc = NULL, format, ...) {
  if (is.null(perc)) {
    assert_choice(format, formatters::list_valid_format_labels()$`1d`)
    assert_numeric(cnt)
    num_str <- sapply(cnt, function(x) {
      format_value(x, format = format, ...)
    })
  }

  if (!is.null(perc)) {
    assert_choice(format, formatters::list_valid_format_labels()$`2d`)
    assert_numeric(cnt)
    assert_numeric(perc)
    num_str <- mapply(function(x, y) {
      format_value(c(x, y), format = format, ...)
    }, x = cnt, y = perc)
  }

  num_str
}

#' @describeIn sfreq This helper function is designed to generate a summarized data
#' table with counts and percentages based on specified grouping variables using
#' [dplyr::count()]. It can also handle hierarchical (nested) variables and apply
#' various formatting and filtering options.
#'
#' @return The `h_count_add_tot` function returns a data frame with counts and
#' percentages for the specified variables, grouped by the group_by_var. The output
#' is in a wide format, with additional columns for counts and percentages, and
#' optionally includes a total column.
#'
h_count_add_tot <- function(input_data,
                            var,
                            sub_var = NULL,
                            nested_vars = NULL,
                            by,
                            fmt,
                            distinct_vars = NULL,
                            cutoff = 0,
                            tot_df,
                            dtype,
                            fctdrop = FALSE,
                            col_tot = TRUE,
                            nested_row = TRUE) {
  # Combine all relevant variables
  vars <- c(nested_vars, var, sub_var, by)
  vars_2 <- c(var, nested_vars, sub_var)
  var0 <- c(var, nested_vars)[1]


  # Handle distinct repeated counts
  dat <- input_data
  if (!is.null(distinct_vars)) {
    if (is.null(sub_var)) {
      dat <- distinct(input_data, !!!syms(c(distinct_vars, var0)), .keep_all = TRUE)
    } else {
      dat <- input_data %>%
        arrange(!!!syms(c(distinct_vars, var0)), desc(!!sym(sub_var))) %>%
        distinct(!!!syms(c(distinct_vars, var0)), .keep_all = TRUE)
    }
  }

  # Calculate counts and percentages
  rst1 <- left_join(
    dat %>% count(!!!syms(vars_2), .drop = fctdrop) %>% .[, vars_2],
    dat %>% count(!!!syms(vars), .drop = FALSE),
    by = vars_2,
    relationship = "many-to-many"
  ) %>%
    left_join(tot_df, by = by) %>%
    mutate(
      label := !!sym(var0),
      # Remove the factor levels from tot_df in case of error occurs in sort process.
      !!sym(by) := fct_drop(!!sym(by)),
      dtype = dtype,
      # special case, only occur in shiny calculation.
      perc = dplyr::coalesce(.data$n / .data$tot, 0),
    ) %>%
    select(label, !!!syms(vars), dtype, everything())

  # Add total column if required
  rst2 <- NULL
  if (col_tot) {
    rst2 <- dat %>%
      count(!!!syms(vars_2), .drop = fctdrop) %>%
      mutate(
        label := !!sym(var0),
        dtype = dtype,
        tot = sum(tot_df$tot),
        perc = dplyr::coalesce(.data$n / .data$tot, 0),
        !!sym(by) := "Total"
      ) %>%
      select(label, !!!syms(vars), dtype, everything())
  }

  # Combine the results
  df <- rbind(rst1, rst2)

  # Format percentage using `formatters` package
  fmt_lst <- formatters::list_valid_format_labels()
  df$con <- if (fmt %in% fmt_lst$`1d`) {
    h_fmt_count_perc(df$n, format = fmt)
  } else if (fmt %in% fmt_lst$`2d`) {
    h_fmt_count_perc(df$n, perc = df$perc, format = fmt)
  } else {
    NA
  }

  # Apply cutoff filter if required
  df <- if (length(cutoff) == 1) {
    group_by(df, !!sym(var0)) %>%
      filter(any(perc >= cutoff / 100)) %>%
      ungroup()
  } else if (length(cutoff) == 2) {
    group_by(df, !!sym(var0)) %>%
      filter(any(between(perc, cutoff[1] / 100, cutoff[2] / 100))) %>%
      ungroup()
  }

  # Add level number for sortings
  levels_df <- data.frame(1:nlevels(df[[by]]), levels(df[[by]]), stringsAsFactors = FALSE)
  names(levels_df) <- c("level_n", by)
  df <- left_join(df, levels_df, by = by) %>%
    arrange(match(!!sym(by), levels(dat[[by]]))) %>%
    arrange(!!sym(by) == "Total")

  # Pivot the data to wide format
  df %>%
    tidyr::pivot_wider(
      id_cols = -all_of(by),
      names_from = "level_n",
      names_sep = "",
      values_from = c("n", "perc", "tot", "con")
    )
}

#' @describeIn sfreq This helper function is designed to sort a data frame based
#' on one or two sorting variables. It handles both factor and non-factor variables
#' and allows for custom ordering expressions. Additionally, it can manage total
#' rows and nested rows within the sorting process.
#'
#' @return The `h_count_sort` function returns a data frame with an additional
#' `var_row_ord` indicating the order of the rows based on the specified sorting criteria.
h_count_sort <- function(data,
                         df,
                         sort_var,
                         sort_var2 = NULL,
                         row_tot,
                         nested_row = FALSE,
                         .order = NULL) {
  sorted_res <- if (is.null(sort_var2)) {
    if (is.null(.order)) {
      if (length(row_tot) > 0 & !nested_row) {
        df %>% arrange(
          match(
            .data[[sort_var]],
            c(row_tot, levels(data[[sort_var]])[which(levels(data[[sort_var]]) != row_tot)])
          ),
          !!sym(sort_var)
        )
      } else if (is.factor(data[[sort_var]])) {
        df %>% arrange(match(.data[[sort_var]], levels(data[[sort_var]])))
      } else {
        df %>% arrange(!!sym(sort_var))
      }
    } else {
      df %>% arrange(!!rlang::parse_expr(.order))
    }
  } else {
    if (is.null(.order)) {
      if (is.factor(data[[sort_var]])) {
        df %>% arrange(
          match(.data[[sort_var]], levels(data[[sort_var]])),
          !!sym(sort_var2)
        )
      } else {
        df %>% arrange(!!sym(sort_var), !!sym(sort_var2))
      }
    } else {
      ord_var <- str_remove_all(.order, pattern = "desc|\\(|\\)")
      var_max_df <- df %>%
        group_by(!!sym(sort_var)) %>%
        mutate(var_max = max(!!sym(ord_var)))
      if (str_detect(.order, "desc")) {
        var_max_df %>%
          arrange(desc(.data$var_max), !!sym(sort_var2)) %>%
          select(-"var_max")
      } else {
        var_max_df %>%
          arrange(.data$var_max, !!sym(sort_var2)) %>%
          select(-"var_max")
      }
    }
  }
  
  sorted_res %>%
    ungroup() %>%
    mutate(!!sym(sort_var) := factor(!!sym(sort_var), levels = unique(.data[[sort_var]]))) %>%
    group_by(!!sym(sort_var)) %>%
    mutate(var_row_ord = dplyr::cur_group_id()) %>%
    ungroup()
}
