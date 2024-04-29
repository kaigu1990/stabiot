rand_adsl <- random.cdisc.data::radsl(
  N = 100, study_duration = 2,
  with_trt02 = FALSE,
  seed = 2
)
rand_adae <- random.cdisc.data::radae(adsl, seed = 2)

usethis::use_data(rand_adsl, overwrite = TRUE)
usethis::use_data(rand_adae, overwrite = TRUE)
