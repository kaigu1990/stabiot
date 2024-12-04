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

#' @describeIn sfreq Helper Function for counting by groups using [dplyr::count()].
#'
#' @export
#'
h_count_by_add_tot <- function(data,
                               var = NULL,
                               nested_vars = NULL,
                               by,
                               fmt,
                               tot_df,
                               dtype,
                               na_str,
                               sort = FALSE,
                               fctdrop = FALSE,
                               col_tot = TRUE,
                               nested_row = TRUE) {
  vars <- c(nested_vars, var, by)
  var0 <- c(var, nested_vars)[1]
  rst1 <- data %>%
    dplyr::count(!!!syms(vars), sort = sort, .drop = fctdrop) %>%
    left_join(tot_df, by = by) %>%
    dplyr::mutate(
      label := !!sym(var0),
      dtype = dtype,
      perc = .data$n / .data$tot
    ) %>%
    dplyr::select(label, !!!syms(vars), dtype, dplyr::everything())

  rst2 <- if (col_tot) {
    rst2 <- data %>%
      dplyr::count(!!!syms(vars[-length(vars)]), sort = sort, .drop = fctdrop) %>%
      dplyr::mutate(
        label := !!sym(var0),
        dtype = dtype,
        tot = colSums(tot_df[, "tot"]),
        perc = .data$n / tot,
        !!sym(by) := "Total"
      ) %>%
      dplyr::select(label, !!!syms(vars), dtype, dplyr::everything())
  } else {
    NULL
  }

  df <- rbind(rst1, rst2)
  
  fmt_lst <- formatters::list_valid_format_labels()
  df$con <- if (fmt %in% fmt_lst$`1d`) {
    h_fmt_count_perc(df$n, format = fmt)
  } else if (fmt %in% fmt_lst$`2d`) {
    h_fmt_count_perc(df$n, perc = df$perc, format = fmt)
  } else {
    NA
  }
  list_levels <- split(1:nlevels(df[[by]]), levels(df[[by]]))
  df$level_n <- sapply(df[[by]], function(x) {
    list_levels[[x]]
  })
  
  df %>%
    tidyr::pivot_wider(
      id_cols = -c(tidyselect::all_of(by)),
      names_from = "level_n",
      names_sep = "",
      values_from = c("n", "perc", "tot", "con"),
      values_fill = na_str
    )
}

#' @describeIn sfreq Helper Function for sorting the rows.
#'
#' @export
#'
h_count_sort <- function(data,
                         df,
                         sort_var,
                         row_tot,
                         nested_row = FALSE,
                         .order = NULL) {
  sorted_res <- if (is.null(.order)) {
    if (is.factor(data[[sort_var]])) {
      df %>% arrange(match(.data[[sort_var]], levels(droplevels(data[[sort_var]]))))
    } else {
      df %>% arrange(!!sym(sort_var))
    }
  } else {
    df %>% arrange(!!rlang::parse_expr(.order))
  }
  if (length(row_tot) > 0 & !nested_row) {
    sorted_res <- df %>% dplyr::arrange(
      match(
        .data[[sort_var]],
        c(row_tot, .data[[sort_var]][which(.data[[sort_var]] != row_tot)])
      ),
      sort_col
    )
  }
  
  sorted_res %>% ungroup() %>%
    group_by(!!sym(sort_var)) %>%
    mutate(var_row_ord = dplyr::cur_group_rows())
}
