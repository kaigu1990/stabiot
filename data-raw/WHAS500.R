whas500 <- haven::read_sas("./data-raw/whas500.sas7bdat") %>%
  labelled::set_variable_labels(
    ID = "Identification Code",
    AGE = "Age at Hospital Admission",
    GENDER = "Gender (0 = Male, 1 = Female)",
    HR = "Initial Heart Rate (Beats per minute)",
    SYSBP = "Initial Systolic Blood Pressure (mmHg)",
    DIASBP = "Initial Diastolic Blood Pressure (mmHg)",
    BMI = "Body Mass Index (kg/m^2)",
    CVD = "History of Cardiovascular Disease",
    AFB = "Atrial Fibrillation ((0 = No, 1 = Yes))",
    SHO = "Cardiogenic Shock",
    CHF = "Congestive Heart Complications",
    AV3 = "Complete Heart Block",
    MIORD = "MI Order",
    MITYPE = "MI Type",
    YEAR = "Cohort Year",
    LOS = "Length of Hospital Stay",
    DSTAT = "Discharge Status from Hospital",
    LENFOL = "Total Length of Follow-up (months)",
    FSTAT = "Vital Status at Last Follow-up (0 = Alive 1 = Dead)"
  ) %>%
  mutate(
    LENFOL = LENFOL / 30.4375
  )

usethis::use_data(whas500, overwrite = TRUE)
