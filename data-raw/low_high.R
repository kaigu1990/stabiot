low1 <- haven::read_sas("./data-raw/low1.sas7bdat")
usethis::use_data(low1, overwrite = TRUE)

high1 <- haven::read_sas("./data-raw/high1.sas7bdat")
usethis::use_data(high1, overwrite = TRUE)
