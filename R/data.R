#' Worcester Heart Attack Study Data
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This is a data of 500 subjects of the Worcester Heart Attack study (also called WHAS500)
#' that demonstrates several factors, such as age, gender and BMI, that may influence
#' survival time after heart attack. Follow up time for all participants begins at
#' the time of hospital admission after heart attack and ends with death or loss
#' to follow up (censoring).
#'
#' @format This data set contains 500 subjects and 19 variables.
#' - ID:     Identification Code               (1 - 500)
#' - AGE:    Age at Hospital Admission         (Years)
#' - GENDER: Gender                            (0 = Male, 1 = Female)
#' - HR:     Initial Heart Rate                (Beats per minute)
#' - SYSBP:  Initial Systolic Blood Pressure   (mmHg)
#' - DIASBP: Initial Diastolic Blood Pressure  (mmHg)
#' - BMI:    Body Mass Index                   (kg/m^2)
#' - CVD:    History of Cardiovascular Disease (0 = No, 1 = Yes)
#' - AFB:    Atrial Fibrillation               (0 = No, 1 = Yes)
#' - SHO:    Cardiogenic Shock                 (0 = No, 1 = Yes)
#' - CHF:    Congestive Heart Complications    (0 = No, 1 = Yes)
#' - AV3:    Complete Heart Block              (0 = No, 1 = Yes)
#' - MIORD:  MI Order                          (0 = First, 1 = Recurrent)
#' - MITYPE: MI Type                           (0 = non Q-wave, 1 = Q-wave)
#' - YEAR:   Cohort Year                       (1 = 1997, 2 = 1999, 3 = 2001)
#' - LOS:    Length of Hospital Stay           (Days between Hospital Discharge and Hospital Admission)
#' - DSTAT:  Discharge Status from Hospital    (0 = Alive, 1 = Dead)
#' - LENFOL: Total Length of Follow-up         (Days between Date of Last Follow-up and Hospital Admission Date)
#' - FSTAT:  Vital Status at Last Follow-up    (0 = Alive 1 = Dead)
#'
#' @references Hosmer, D.W. and Lemeshow, S. and May, S. (2008) Applied Survival
#' Analysis: Regression Modeling of Time to Event Data: Second Edition,
#' John Wiley and Sons Inc., New York, NY.
#'
"whas500"
