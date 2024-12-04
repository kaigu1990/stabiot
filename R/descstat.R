#' Summarize Frequency Counts and Percentages
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Create a summary table for one variable or nested variable by one group, as 
#' well as a total column if necessary.
#'
#' @param data (`data.frame`)\cr a data frame that contains the variables to be
#'  summarized and grouped.
#' @param var (`string`)\cr a character variable to be summarized within `data`.
#' @param nested_vars (`string`)\cr a character variable as the nested level to 
#'  be summarized along with `var`.
#' @param by (`string`)\cr a character variable for grouping within `data`.
#' @param fmt (`string`)\cr formatting string from `formatters::list_valid_format_labels()`
#'  for frequency counts and percentages.
#' @param denom (`numeric` or `data.frame`)\cr denominator for proportion can be a
#'  numeric vector of denominators or a data frame where we can count the `var` inside.
#' @param distinct (`string`)\cr a character variable to determine which level should 
#'  be kept only unique rows from `data`. Default is 'USUBJID' for normal counting, but 
#'  it can be set as 'STUDYID' for AE events if necessary.
#' @param fctdrop (`logical`)\cr whether to include the levels of the variables
#'  but with no records.
#' @param col_tot (`logical`)\cr whether to add total column in the output or not.
#' @param row_tot (`string`)\cr Default set as NULL for no total row, but if set as 
#'  'n' or other words that will be defined as a label for total row.
#' @param nested_row (`logical`)\cr whether to add nested variable in the label row. 
#'  Set to TRUE for AE counting.
#' @param na_str (`string`)\cr a string to replace `NA` in the output if no records
#'  will be counted for any category, but not used.
#'
#' @note By default, the each category is sorted based on the corresponding factor
#'  level of `var` variable. If the variable is not a factor, that will be sorted
#'  alphabetically.
#'
#' @return A object `Desc` contains an intermediate data with long form for
#'  post-processing and final data with wide form for presentation.
#' @export
#'
#' @examples
#' data(rand_adsl)
#' data(rand_adae)
#'
#' # Count the sex by treatment with 'xx (xx.x%)' format
#' rand_adsl %>%
#'   sfreq(
#'     var = "SEX",
#'     by = "TRT01P",
#'     fmt = "xx (xx.x%)"
#'   )
#'
#' # Count the race by treatment with 'xx (xx.xx)' format and replace NA with '0'
#' rand_adsl %>%
#'   sfreq(
#'     var = "RACE",
#'     by = "TRT01P",
#'     fmt = "xx (xx.xx%)"
#'   )
#'  
#' # Count the race by treatment for sex group
#' rand_adsl %>%
#'   sfreq(
#'     var = "RACE",
#'     nested_vars = "SEX",
#'     by = "TRT01P",
#'     fmt = "xx (xx.xx%)",
#'     nested_row = TRUE
#'   )
#'
#' # Count the AE subjects by treatment for AEDECOD and AEBODSYS that sorted by total column
#' rand_adae %>%
#'   sfreq(
#'     var = "AEDECOD",
#'     nested_vars = "AEBODSYS",
#'     by = "TRT01P",
#'     fmt = "xx (xx.x%)",
#'     denom = rand_adsl,
#'     row_tot = NULL,
#'     fctdrop = TRUE,
#'     nested_row = TRUE,
#'     .order = "desc(perc4)"
#'   )
sfreq <- function(data,
                  var,
                  nested_vars = NULL,
                  by,
                  fmt,
                  denom = NULL,
                  distinct = "USUBJID",
                  fctdrop = FALSE,
                  col_tot = TRUE,
                  row_tot = NULL,
                  nested_row = FALSE,
                  na_str = NULL,
                  .order = NULL) {
  assert_data_frame(data)
  assert_subset(var, choices = names(data), empty.ok = FALSE)
  assert_subset(nested_vars, choices = names(data), empty.ok = TRUE)
  assert_subset(by, choices = names(data), empty.ok = FALSE)
  assert_multi_class(denom, c("numeric", "data.frame"))
  assert_subset(distinct, choices = names(data), empty.ok = FALSE)
  assert_logical(fctdrop)
  assert_logical(col_tot)
  assert_string(row_tot, null.ok = TRUE)
  assert_string(na_str, null.ok = TRUE)

  # Convert by variable to factor class.
  if (!is.factor(data[[by]])) {
    data[[by]] <- factor(data[[by]], unique(data[[by]]))
    message(paste0("Group Var [", by, "] was not factor, converting to facor by first appearance."))
  }

  denom_vec <- if (is.data.frame(denom)) {
    if (!is.null(by)) {
      assert_subset(by, choices = names(denom), empty.ok = FALSE)
      denom %>%
        dplyr::count(!!sym(by)) %>%
        dplyr::mutate(tot = n) %>%
        dplyr::select(-n)
    }
  } else if (is.numeric(denom)) {
    data.frame(tot = denom, stringsAsFactors = FALSE) %>%
      dplyr::mutate(!!sym(by) := levels(data[[by]]))
  } else if (is.null(denom)) {
    data %>%
      dplyr::count(!!sym(by)) %>%
      dplyr::mutate(tot = n) %>%
      dplyr::select(-n)
  }

  dat_0 <- if (!is.null(distinct)) {
    dplyr::distinct(data, !!!syms(c(nested_vars, var, distinct)), .keep_all = TRUE)
  } else {
    data
  }
  dat_1 <- if (length(row_tot) > 0) {
    rbind(dat_0 %>% dplyr::mutate(!!sym(var) := row_tot), dat_0)
  } else {
    dat_0
  }

  cnt_num <- h_count_by_add_tot(
    data = dat_1, var = var, nested_vars = nested_vars,
    by = by, fmt = fmt, tot_df = denom_vec, dtype = "VAR",
    fctdrop = fctdrop, col_tot = col_tot, na_str = na_str,
    nested_row = nested_row
  )

  # Arrange the rows by specific conditions.
  grp_num <- cnt_num %>% dplyr::select(starts_with("perc")) %>% ncol()
  assert_subset(.order,
    choices = c(
      var, paste0("desc(", var, ")"),
      paste0("n", 1:grp_num),
      paste0("desc(", paste0("n", 1:grp_num), ")"),
      paste0("perc", 1:grp_num),
      paste0("desc(", paste0("perc", 1:grp_num), ")")
    ),
    empty.ok = TRUE
  )
  cnt_num_sorted <- h_count_sort(
    data = data,
    df = cnt_num,
    sort_var = var,
    row_tot = row_tot,
    .order = .order
  )

  cnt_num_rst <- if (nested_row) {
    nested_df <- h_count_by_add_tot(
      data = dat_1, nested_vars = nested_vars,
      by = by, fmt = fmt, tot_df = denom_vec, dtype = "NESTED_VARS",
      fctdrop = fctdrop, col_tot = col_tot, na_str = na_str,
      nested_row = nested_row
    )
    nested_df_sorted <- h_count_sort(
      data = data,
      df = nested_df,
      sort_var = nested_vars,
      row_tot = row_tot,
      .order = .order
    )

    cnt_num_sorted %>%
      left_join(nested_df_sorted %>% dplyr::select(nested_vars, nested_row_ord = var_row_ord),
        by = nested_vars
      ) %>%
      dplyr::bind_rows(nested_df_sorted) %>%
      mutate(
        nested_row_ord = dplyr::case_when(
          stringr::str_detect(dtype, "NESTED_VARS") ~ var_row_ord,
          TRUE ~ nested_row_ord
        ),
        var_row_ord = dplyr::case_when(
          stringr::str_detect(dtype, "NESTED_VARS") ~ 0,
          TRUE ~ var_row_ord
        )
      ) %>%
      dplyr::arrange(.data$nested_row_ord, .data$var_row_ord)
  } else {
    cnt_num_sorted
  }

  cnt_tb <- cnt_num_rst %>%
    ungroup() %>%
    dplyr::select("label", dplyr::starts_with("con"), "dtype") %>%
    rlang::set_names(c("Label", levels(data[[by]]), "Total", "DTYPE"))

  structure(
    list(
      data = data,
      cnt_tb = cnt_tb,
      cnt_num = cnt_num_rst,
      params = list(
        var = var,
        nested_vars = nested_vars,
        by = by,
        fmt = fmt,
        denom = denom_vec,
        distinct = distinct
      )
    ),
    class = "sfreq"
  )
}
