# derive_bor ----

#' Derivation of Best Overall Response per RECIST
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Derive best overall response (confirmed or not confirmed BOR) according to
#' RECIST 1.1 from time-point overall responses of CR, PR, SD, PD and NE.
#'
#' @param data (`data.frame`)\cr a data frame as input, preferably ADRS of the ADaM
#' standard with few necessary variables, see notes and examples.
#' @param unique_id (`string`)\cr string specifying which column as the unique subject id.
#' @param aval_map (`tbl` or `data.frame`)\cr a mapping list between AVALC and AVAL,
#' default are CR=1, PR=2, SD=3, PD=4 and NE=5. See `getOption("stabiot.aval.mapping")`
#' for more details.
#' @param spec_date (`string`)\cr a date is used to determine whether or not the
#' responses should be filtered after that specific date. Such as filtering all
#' responses after the first PD date (pd_date). If NULL, no filtering procedure.
#' @param ref_date (`string`)\cr a date is used to determine the during from the
#' specific date. Usually it's the treatment start date or randomization date.
#' @param ref_start_window (`integer`)\cr an integer days is used to determine how
#' many days are required following the reference day (`ref_date`) when the responses
#' occur.
#' @param ref_interval (`integer`)\cr an integer days is used to determine interval
#' days are required between two assessments.
#' @param max_ne (`integer`)\cr an integer of maximum NE is acceptable between two
#' assessments.
#' @param confirm (`logical`)\cr a logical indicating of whether the BOR is required
#' for confirmation.
#'
#' @details
#' The detailed rule of computing the best overall response (BOR) as shown below.
#'
#' If the `spec_date = "pd_date"` option is selected, only the assessments up
#' to first PD will be considered to calculate the BOR.
#'
#' No confirmation is required for BOR:
#' 1. Set to complete response (CR) if there exists an assessment of CR.
#' 2. Set to partial response (PR) if there exists an assessment of PR.
#' 3. Set to stable disease (SD) if there exists an assessment of SD that is
#' at least `ref_start_window` days after `ref_date`.
#' 4. Set to progressive disease (PD) if there exists an assessment of PD.
#' 5. Set to not estimable (NE) if there exists an assessment of NE or the assessment
#' occurring is less than `ref_start_window` days after `ref_date`.
#'
#' Confirmation is required for BOR:
#' 1. Set to complete response (CR) if:
#'    - there is one confirmatory assessment of CR at least `ref_interval` days later,
#'    - all assessments between two assessment are "CR" or "NE", and
#'    - there are at most `max_ne` NE between two assessments.
#' 2. Set to partial response (PR) if:
#'    - there is one confirmatory assessment of CR or PR at least `ref_interval` days later,
#'    - all assessments between two assessment are "CR", "PR" or "NE", and
#'    - there are at most `max_ne` NE between two PR/CR assessments.
#' 3. Set to stable disease (SD) if:
#'    - there is one CR, PR or SD that is at least `ref_start_window` days after `ref_date`.
#' 4. Set to progressive disease (PD) if:
#'    - there is one PD.
#' 5. Set to not estimable (NE) if:
#'    - there is at least one CR, PR, SD, NE.
#'
#' After executing the previous steps, select the best one using the order 'CR>PR>SD>PD>NE'
#' for each subject. If the BOR is not unique, the first one (based on ADT) is selected.
#'
#' @note
#' - The `ref_start_window` refers to assessment date (ADT) minus `ref_date`. For
#' example, `ref_start_window = 28` that means ADT minus TRTSDT should be equal to
#' or greater than 28 days. And this argument is only used in non-confirmatory/confirmatory
#' of SD.
#' - The `ref_interval` refers to confirmatory assessment date minus assessment date.
#' For example, `ref_interval = 28` that means subsequent CR date minus prior CR
#' should be equal to or greater than 28 days. And this argument is used in confirmatory
#' of CR/PR.
#'
#' @return
#' A data frame with a new parameter for confirmed or not confirmed best overall response.
#'
#' @export
#'
#' @examples
#' # This example is referred from `admiral::event_joined`.
#' adrs <- tibble::tribble(
#'   ~USUBJID, ~TRTSDTC,     ~ADTC,        ~AVALC,
#'   "1",      "2020-01-01", "2020-01-01", "PR",
#'   "1",      "2020-01-01", "2020-02-01", "CR",
#'   "1",      "2020-01-01", "2020-02-16", "NE",
#'   "1",      "2020-01-01", "2020-03-01", "CR",
#'   "1",      "2020-01-01", "2020-04-01", "SD",
#'   "2",      "2019-12-12", "2020-01-01", "SD",
#'   "2",      "2019-12-12", "2020-02-01", "PR",
#'   "2",      "2019-12-12", "2020-03-01", "SD",
#'   "2",      "2019-12-12", "2020-03-13", "CR",
#'   "4",      "2019-12-30", "2020-01-01", "PR",
#'   "4",      "2019-12-30", "2020-03-01", "NE",
#'   "4",      "2019-12-30", "2020-04-01", "NE",
#'   "4",      "2019-12-30", "2020-05-01", "PR",
#'   "5",      "2020-01-01", "2020-01-01", "PR",
#'   "5",      "2020-01-01", "2020-01-10", "PR",
#'   "5",      "2020-01-01", "2020-01-20", "PR",
#'   "6",      "2020-02-02", "2020-02-06", "PR",
#'   "6",      "2020-02-02", "2020-02-16", "CR",
#'   "6",      "2020-02-02", "2020-03-30", "PR",
#'   "7",      "2020-02-02", "2020-02-06", "PR",
#'   "7",      "2020-02-02", "2020-02-16", "CR",
#'   "7",      "2020-02-02", "2020-04-01", "NE",
#'   "8",      "2020-02-01", "2020-02-16", "PD"
#' ) |>
#'   dplyr::mutate(
#'     ADT = lubridate::ymd(ADTC),
#'     TRTSDT = lubridate::ymd(TRTSDTC),
#'     PARAMCD = "OVR",
#'     PARAM = "Overall Response by Investigator"
#'   ) |>
#'   dplyr::select(-TRTSDTC)
#'
#' # Derive BOR without confirmation.
#' derive_bor(data = adrs)
#'
#' # Derive confirmed BOR.
#' derive_bor(data = adrs, confirm = TRUE)
#'
#' # Derive confirmed BOR with no NE is acceptable between two assessments.
#' derive_bor(data = adrs, max_ne = 0, confirm = TRUE)
derive_bor <- function(data,
                       unique_id = "USUBJID",
                       aval_map = getOption("stabiot.aval.mapping"),
                       spec_date = "pd_date",
                       ref_date = "TRTSDT",
                       ref_start_window = 28,
                       ref_interval = 28,
                       max_ne = 1,
                       confirm = FALSE) {
  assert_class(data, "data.frame")
  assert_subset(unique_id, names(data), empty.ok = FALSE)
  assert_class(aval_map, "data.frame")
  assert_subset(names(aval_map), c("avalc_temp", "aval_temp"))
  assert_string(spec_date, null.ok = TRUE)
  assert_date(data[[ref_date]])
  assert_number(ref_start_window, lower = 0)
  assert_number(ref_interval, lower = 0)
  assert_number(max_ne, lower = 0)
  assert_logical(confirm)

  data <- data %>%
    arrange(!!sym(unique_id), ADT)

  rs <- if (spec_date == "pd_date") {
    data %>%
      group_by(!!sym(unique_id)) %>%
      filter(row_number() <= match("PD", AVALC) | all(AVALC != "PD")) %>%
      ungroup()
  } else {
    data
  }

  bor_res <- if (!confirm) {
    rs %>%
      mutate(
        # set to 'NE' if the SD is occurred less than 28 days after treatment start.
        avalc_temp = case_when(
          AVALC %in% c("SD") &
            ADT < !!sym(ref_date) + days(ref_start_window) ~ "NE",
          TRUE ~ AVALC
        )
      ) %>%
      left_join(aval_map, by = c("AVALC" = "avalc_temp")) %>%
      mutate(AVAL = .data$aval_temp) %>%
      arrange(!!sym(unique_id), AVAL, ADT) %>%
      # select the best and first one as the best overall response.
      distinct(!!sym(unique_id), .keep_all = TRUE) %>%
      mutate(
        PARAMCD = "BOR",
        PARAM = "Best Overall Response"
      ) %>%
      select(-c("avalc_temp", "aval_temp"))
  } else {
    conf_cr <- rs %>%
      left_join(
        select(rs, !!sym(unique_id), "ADT", "AVALC"),
        by = unique_id, relationship = "many-to-many"
      ) %>%
      # keep the joined observations are after the original observations (e.g. AVALC.x)
      filter(AVALC.x == "CR" & ADT.y > ADT.x) %>%
      # find out the first observation that fulfills the restriction, like subsequent CR at least 28 days later.
      mutate(
        flag = AVALC.y %in% c("CR") & ADT.y >= ADT.x + days(ref_interval)
      ) %>%
      group_by(!!sym(unique_id)) %>%
      filter(
        row_number() <= min(match(TRUE, .data$flag))
      ) %>%
      # define how many NE and which responses can be acceptable between these two assessments.
      filter(sum(AVALC.y == "NE") <= max_ne & all(AVALC.y %in% c("CR", "NE"))) %>%
      ungroup() %>%
      distinct(!!sym(unique_id), .keep_all = TRUE) %>%
      mutate(
        AVALC = "CR",
        ADT = ADT.x
      ) %>%
      select(names(rs))

    conf_pr <- rs %>%
      left_join(
        select(rs, !!sym(unique_id), "ADT", "AVALC"),
        by = unique_id, relationship = "many-to-many"
      ) %>%
      filter(AVALC.x == "PR" & ADT.y > ADT.x) %>%
      mutate(
        flag = AVALC.y %in% c("CR", "PR") & ADT.y >= ADT.x + days(ref_interval)
      ) %>%
      group_by(!!sym(unique_id)) %>%
      filter(
        row_number() <= min(match(TRUE, .data$flag))
      ) %>%
      filter(sum(AVALC.y == "NE") <= max_ne & all(AVALC.y %in% c("CR", "PR", "NE"))) %>%
      ungroup() %>%
      distinct(!!sym(unique_id), .keep_all = TRUE) %>%
      mutate(
        AVALC = "PR",
        ADT = ADT.x
      ) %>%
      select(names(rs))

    sd <- rs %>%
      # CR, PR and SD can be considered as SD that occurs at least 28 days after treatment start.
      filter(AVALC %in% c("CR", "PR", "SD") &
        ADT >= !!sym(ref_date) + days(ref_start_window)) %>%
      distinct(!!sym(unique_id), .keep_all = TRUE) %>%
      mutate(AVALC = "SD") %>%
      select(names(rs))

    pd <- rs %>%
      filter(AVALC == "PD") %>%
      distinct(!!sym(unique_id), .keep_all = TRUE) %>%
      mutate(AVALC = "PD") %>%
      select(names(rs))

    ne <- rs %>%
      filter(AVALC %in% c("CR", "PR", "SD", "NE")) %>%
      distinct(!!sym(unique_id), .keep_all = TRUE) %>%
      mutate(AVALC = "NE") %>%
      select(names(rs))

    purrr::list_rbind(
      list(conf_cr, conf_pr, sd, pd, ne)
    ) %>%
      left_join(aval_map, by = c("AVALC" = "avalc_temp")) %>%
      mutate(
        AVAL = .data$aval_temp,
        PARAMCD = "CBOR",
        PARAM = "Confirmed Best Overall Response"
      ) %>%
      arrange(!!sym(unique_id), AVAL, ADT) %>%
      distinct(!!sym(unique_id), .keep_all = TRUE) %>%
      select(-"aval_temp")
  }

  bor_res
}
