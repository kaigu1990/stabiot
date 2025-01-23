#' Summarize Frequency Counts and Percentages
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Create a summary frequency table for a given data frame, including counts and
#' percentage for specified variables. It handles nested variables such as AE summary,
#' and formatting options, and can incorporate a denominator for proportion calculation.
#'
#' @param data (`data.frame`)\cr a input data frame containing the variables to be summarized.
#' @param var (`string`)\cr a primary variable to be summarized.
#' @param sub_var (`string`)\cr an optional sub-variable to be summarized, such as
#'  AE summary by toxicity grade.
#' @param nested_vars (`string`)\cr an optional nested variable for hierarchical
#'  summary along with `var`, such as AE summary by SOC and PT.
#' @param by (`string`)\cr a variable used for grouping the data.
#' @param fmt (`string`)\cr format type from `formatters::list_valid_format_labels()`
#'  for displaying counts and percentages, the default is "xx (xx.x%)".
#' @param denom (`numeric` or `data.frame`)\cr denominator for calculating proportion,
#'  than can be a data frame where we can count the `var` inside or a specified numeric vector.
#' @param cutoff (`numeric`)\cr a numeric cutoff for filtering results based on percentage,
#'  default is 0. If it's a numeric vector of length 1, the result with percentage greater
#'  than the value is retained. If the length is 2, the result with percentage between the
#'  two values is retained.
#' @param distinct_vars (`string`)\cr a  variable used to ensure distinct counts,  default
#'  is 'USUBJID' for normal counting, but for AE events it can be set as Null or
#'  another variable that can represent the unique AE events.
#' @param fctdrop (`logical`)\cr whether to drop unused factor levels, default is FALSE.
#' @param col_tot (`logical`)\cr whether to include a total column in the output.
#' @param row_tot (`string`)\cr An optional label for the total row, default set as NULL
#'  for no total row, but if set as 'n' or other words that will be defined as a label
#'  for the total row.
#' @param nested_row (`logical`)\cr whether to include nested rows in the output.
#'  Suggestion is set to TRUE for AE counting if the `nested_vars` is defined.
#' @param .order (`string`)\cr An optional ordering expression, to sort the output
#'  based on the specified variable. The default is NULL, but can be set as 'desc(perc4)'
#'  to descending sort the output based on the percentage of the fourth group.
#' @param .rm (`string`)\cr An optional string to remove from the `var` variable,
#'  default is NULL.
#'
#' @note By default, the each category is sorted based on the corresponding factor
#'  level of `var` variable. If the variable is not a factor, that will be sorted
#'  alphabetically. If you would like to sort by specified column, you can use the
#'  `.order` parameter to specify the column name. If the order is descending, you
#'  can add `desc()` before the column name, like `.order = "desc(perc4)"`.
#'
#'  If you want to get the nested rows for the specified variables, you can use
#'  `nested_vars` to specify the nested variables, and set `nested_row = TRUE`.
#'  This is useful for AE summary tables when you want to get the
#'  summary for AE by SOC and PT.
#'
#' @return A list with the class `sfreq` containing the following components:
#' - `cnt_tb`, a final data frame for presentation with necessary columns.
#' - `cnt_num`, an intermediate data frame with the counts and percentages for each group.
#'  It's easy to extract the data for further analysis.
#' - `params`, a list of parameters used in the function.
#'
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
#' # Count the race by treatment for sex group with n(%) row total
#' rand_adsl %>%
#'   sfreq(
#'     var = "RACE",
#'     by = "TRT01P",
#'     fmt = "xx (xx.xx%)",
#'     row_tot = "n (%)"
#'   )
#'
#' # Count the AE subjects by treatment for AEDECOD sorted by total column (column 4)
#' rand_adae %>%
#'   sfreq(
#'     var = "AEDECOD",
#'     by = "TRT01P",
#'     fmt = "xx (xx.x%)",
#'     denom = rand_adsl,
#'     fctdrop = TRUE,
#'     .order = "desc(perc4)"
#'   )
#'
#' # Count the AE subjects by treatment for AEDECOD by AE toxicity grade
#' rand_adae %>%
#'   sfreq(
#'     var = "AEDECOD",
#'     sub_var = "AETOXGR",
#'     by = "TRT01P",
#'     fmt = "xx (xx.x%)",
#'     denom = rand_adsl,
#'     fctdrop = TRUE,
#'     .order = "desc(perc4)"
#'   )
#'
#' # Count the AE subjects by treatment for AEDECOD and AEBODSYS
#' rand_adae %>%
#'   sfreq(
#'     var = "AEDECOD",
#'     nested_vars = "AEBODSYS",
#'     by = "TRT01P",
#'     fmt = "xx (xx.x%)",
#'     denom = rand_adsl,
#'     fctdrop = TRUE,
#'     nested_row = TRUE,
#'     .order = "desc(perc4)"
#'   )
#'
#' # Count the AE subjects by treatment for AEDECOD and AEBODSYS by AE toxicity grade
#' rand_adae %>%
#'   sfreq(
#'     var = "AEDECOD",
#'     sub_var = "AETOXGR",
#'     nested_vars = "AEBODSYS",
#'     by = "TRT01P",
#'     fmt = "xx (xx.x%)",
#'     denom = rand_adsl,
#'     fctdrop = TRUE,
#'     nested_row = TRUE,
#'     .order = "desc(perc4)"
#'   )
#'
sfreq <- function(data,
                  var,
                  sub_var = NULL,
                  nested_vars = NULL,
                  by = NULL,
                  fmt = "xx (xx.x%)",
                  denom = NULL,
                  cutoff = 0,
                  distinct_vars = "USUBJID",
                  fctdrop = FALSE,
                  col_tot = TRUE,
                  row_tot = NULL,
                  nested_row = FALSE,
                  .order = NULL,
                  .rm = NULL) {
  assert_data_frame(data)
  assert_subset(var, choices = names(data), empty.ok = FALSE)
  assert_subset(sub_var, choices = names(data), empty.ok = TRUE)
  assert_subset(nested_vars, choices = names(data), empty.ok = TRUE)
  assert_subset(by, choices = names(data), empty.ok = TRUE)
  assert_multi_class(denom, c("numeric", "data.frame"), null.ok = TRUE)
  assert_subset(distinct_vars, choices = names(data), empty.ok = TRUE)
  assert_logical(fctdrop)
  assert_logical(col_tot)
  assert_string(row_tot, null.ok = TRUE)
  assert_logical(nested_row)
  assert_string(.order, null.ok = TRUE)
  assert_string(.rm, null.ok = TRUE)

  if (nested_row & is.null(nested_vars)) {
    stop("The 'nested_vars' is required when the 'nested_row' is TRUE.")
  }

  # Convert `by` and `var` to factors if not already.
  if (!is.factor(data[[by]])) {
    data[[by]] <- fct(data[[by]])
    message(paste0("Group Var [", by, "] of data was not factor, converting to factor by first appearance."))
  }
  if (!is.factor(data[[var]])) {
    data[[var]] <- factor(data[[var]])
    # message(paste0("Group Var [", var, "] of data was not factor, converting to factor by default levels."))
  }
  if (!is.factor(denom[[by]]) & !is.null(denom)) {
    denom[[by]] <- fct(denom[[by]])
    message(paste0("Group Var [", by, "] of denom was not factor, converting to factor by first appearance."))
  }

  # Remove NA and special characters in `var` column
  if (!is.null(.rm)) {
    if (is.na(.rm)) {
      data <- data[!is.na(data[[var]]), ]
    } else {
      data <- data[data[[var]] != .rm, ]
      data[[var]] <- droplevels(data[[var]])
    }
  }

  # Calculation denominator
  denom_vec <- if (is.data.frame(denom)) {
    if (!is.null(by)) {
      assert_subset(by, choices = names(denom), empty.ok = FALSE)
      denom %>%
        filter(!!sym(by) %in% levels(data[[by]])) %>%
        count(!!sym(by), .drop = TRUE) %>%
        select(all_of(by), tot = .data$n)
    } else {
      stop("The 'by' variable is required when the 'denom' is a data frame.")
    }
  } else if (is.numeric(denom)) {
    data.frame(tot = denom, stringsAsFactors = FALSE) %>% mutate(!!sym(by) := levels(data[[by]]))
  } else if (is.null(denom)) {
    data %>%
      count(!!sym(by), .drop = FALSE) %>%
      mutate(tot = .data$n) %>%
      select(-"n")
  }

  # add row_tot if needed, and prepare data for counting
  dat_1 <- if (length(row_tot) > 0) {
    rbind(data %>% mutate(!!sym(var) := row_tot), data)
  } else {
    data
  }

  # Calculate the counts and percentages
  cnt_num <- h_count_add_tot(
    input_data = dat_1, var = var, nested_vars = nested_vars, sub_var = sub_var, by = by,
    fmt = fmt, distinct_vars = distinct_vars, cutoff = cutoff, tot_df = denom_vec,
    dtype = "VAR", fctdrop = fctdrop, col_tot = col_tot, nested_row = nested_row
  )

  # Determine sorting order
  grp_num <- cnt_num %>%
    select(starts_with("perc")) %>%
    colnames() %>%
    str_remove("perc")
  if (length(grp_num) > 0) {
    assert_subset(.order,
      choices = c(
        var, paste0("desc(", var, ")"),
        paste0("n", grp_num),
        paste0("desc(", paste0("n", grp_num), ")"),
        paste0("perc", grp_num),
        paste0("desc(", paste0("perc", grp_num), ")")
      ),
      empty.ok = TRUE
    )
  } else {
    .order <- NULL
  }

  # sort data
  cnt_num_sorted <- h_count_sort(
    data = data,
    input_data = cnt_num,
    sort_var = var,
    sort_var2 = sub_var,
    row_tot = row_tot,
    .order = .order
  )

  # Handle nested rows if specified
  cnt_num_rst <- if (nested_row) {
    nested_df <- h_count_add_tot(
      input_data = dat_1, var = NULL, nested_vars = nested_vars, sub_var = sub_var, by = by,
      fmt = fmt, distinct_vars = distinct_vars, tot_df = denom_vec, dtype = "NESTED_VARS",
      fctdrop = fctdrop, col_tot = col_tot, nested_row = nested_row
    )
    nested_df_sorted <- h_count_sort(
      data = data,
      input_data = nested_df,
      sort_var = nested_vars,
      sort_var2 = sub_var,
      row_tot = row_tot,
      .order = .order
    )

    cnt_num_sorted %>%
      left_join(
        nested_df_sorted %>%
          select(any_of(c(nested_vars, sub_var)), "nested_row_ord" = "var_row_ord"),
        by = c(nested_vars, sub_var)
      ) %>%
      bind_rows(
        # keep the nested_vars when the percentage of var within the cutoff
        nested_df_sorted %>%
          filter(!!sym(nested_vars) %in% cnt_num_sorted[[nested_vars]])
      ) %>%
      mutate(
        nested_row_ord = case_when(
          str_detect(dtype, "NESTED_VARS") ~ .data$var_row_ord,
          TRUE ~ .data$nested_row_ord
        ),
        var_row_ord = case_when(
          str_detect(dtype, "NESTED_VARS") ~ 0,
          TRUE ~ .data$var_row_ord
        ),
        varname = case_when(
          str_detect(dtype, "NESTED_VARS") ~ nested_vars[1],
          TRUE ~ var
        )
      ) %>%
      arrange(.data$nested_row_ord, .data$var_row_ord) %>%
      ungroup()
  } else {
    cnt_num_sorted %>%
      mutate(varname = var) %>%
      arrange(.data$var_row_ord)
  }

  # Generate final summary table
  cnt_tb <- if (nrow(cnt_num_rst) == 0) {
    cnt_num_rst %>%
      select(
        "Label" = "label", "DTYPE" = "dtype", "VARNAME" = "varname",
        !!!syms(c(var, nested_vars, sub_var))
      )
  } else {
    cnt_num_rst %>%
      select(
        "label", starts_with("con"), "dtype", "varname",
        !!!syms(c(var, nested_vars, sub_var))
      ) %>%
      set_names(
        c(
          "Label", levels(data[[by]]),
          switch(col_tot,
            "Total",
            NULL
          ),
          "DTYPE", "VARNAME", var, nested_vars, sub_var
        )
      )
  }

  # Generate mapping for grouping variable
  if (length(grp_num) > 0) {
    if (col_tot) {
      bymap <- set_names(data.frame(c(levels(data[[by]]), "Total"), grp_num), c(by, "number"))
    } else {
      bymap <- set_names(data.frame(levels(data[[by]]), grp_num), c(by, "number")) %>%
        distinct(!!sym(by), .data$number) %>%
        mutate(!!sym(by) := fct_reorder(!!sym(by), .data$number))
    }
  } else {
    NULL
  }


  structure(
    list(
      cnt_tb = cnt_tb,
      cnt_num = cnt_num_rst,
      params = list(
        var = var,
        sub_var = sub_var,
        nested_vars = nested_vars,
        by = by,
        fmt = fmt,
        denom = denom_vec,
        distinct_vars = distinct_vars,
        bymap = bymap
      )
    ),
    class = "sfreq"
  )
}
