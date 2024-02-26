library(random.cdisc.data)

# adsl_vars <- c(
#   "STUDYID", "USUBJID", "SUBJID", "SITEID", "AGE", "AGEU", "SEX", "RACE", "ETHNIC",
#   "COUNTRY", "DTHFL", "ARM", "ARMCD", "ACTARM", "ACTARMCD", "TRT01P", "TRT01A",
#   "REGION1", "STRATA1", "STRATA2", "BMRKR1", "BMRKR2", "ITTFL", "SAFFL", "BMEASIFL",
#   "BEP01FL", "AEWITHFL", "RANDDT", "TRTSDTM", "TRTEDTM"
# )

drop_vars <- c("INVID", "INVNAM", "LDDTHELD", "LDDTHGR1", "ADTHAUT")

adsl <- radsl(N = 50, seed = 12306, study_duration = 2, with_trt02 = FALSE) %>%
  dplyr::select(-drop_vars)

adrs <- radrs(adsl, seed = 123)
a <- select(adrs, -c(5:10)) %>%
  arrange(USUBJID, PARAM, AVISITN)

data(cadrs)
a <- select(cadrs, -c(5:20))
