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
