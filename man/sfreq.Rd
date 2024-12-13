% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/descstat.R, R/utils.R
\name{sfreq}
\alias{sfreq}
\alias{h_count_by_add_tot}
\alias{h_count_sort}
\title{Summarize Frequency Counts and Percentages}
\usage{
sfreq(
  data,
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
  .order = NULL
)

h_count_by_add_tot(
  data,
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
  nested_row = TRUE
)

h_count_sort(data, df, sort_var, row_tot, nested_row = FALSE, .order = NULL)
}
\arguments{
\item{data}{(\code{data.frame})\cr a data frame that contains the variables to be
summarized and grouped.}

\item{var}{(\code{string})\cr a character variable to be summarized within \code{data}.}

\item{nested_vars}{(\code{string})\cr a character variable as the nested level to
be summarized along with \code{var}.}

\item{by}{(\code{string})\cr a character variable for grouping within \code{data}.}

\item{fmt}{(\code{string})\cr formatting string from \code{formatters::list_valid_format_labels()}
for frequency counts and percentages.}

\item{denom}{(\code{numeric} or \code{data.frame})\cr denominator for proportion can be a
numeric vector of denominators or a data frame where we can count the \code{var} inside.}

\item{distinct}{(\code{string})\cr a character variable to determine which level should
be kept only unique rows from \code{data}. Default is 'USUBJID' for normal counting, but
it can be set as 'STUDYID' for AE events if necessary.}

\item{fctdrop}{(\code{logical})\cr whether to include the levels of the variables
but with no records.}

\item{col_tot}{(\code{logical})\cr whether to add total column in the output or not.}

\item{row_tot}{(\code{string})\cr Default set as NULL for no total row, but if set as
'n' or other words that will be defined as a label for total row.}

\item{nested_row}{(\code{logical})\cr whether to add nested variable in the label row.
Set to TRUE for AE counting.}

\item{na_str}{(\code{string})\cr a string to replace \code{NA} in the output if no records
will be counted for any category, but not used.}
}
\value{
A object \code{Desc} contains an intermediate data with long form for
post-processing and final data with wide form for presentation.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

Create a summary table for one variable or nested variable by one group, as
well as a total column if necessary.
}
\section{Functions}{
\itemize{
\item \code{h_count_by_add_tot()}: Helper Function for counting by groups using \code{\link[dplyr:count]{dplyr::count()}}.

\item \code{h_count_sort()}: Helper Function for sorting the rows.

}}
\note{
By default, the each category is sorted based on the corresponding factor
level of \code{var} variable. If the variable is not a factor, that will be sorted
alphabetically.
}
\examples{
data(rand_adsl)
data(rand_adae)

# Count the sex by treatment with 'xx (xx.x\%)' format
rand_adsl \%>\%
  sfreq(
    var = "SEX",
    by = "TRT01P",
    fmt = "xx (xx.x\%)"
  )

# Count the race by treatment with 'xx (xx.xx)' format and replace NA with '0'
rand_adsl \%>\%
  sfreq(
    var = "RACE",
    by = "TRT01P",
    fmt = "xx (xx.xx\%)"
  )
 
# Count the race by treatment for sex group
rand_adsl \%>\%
  sfreq(
    var = "RACE",
    nested_vars = "SEX",
    by = "TRT01P",
    fmt = "xx (xx.xx\%)",
    nested_row = TRUE
  )

# Count the AE subjects by treatment for AEDECOD and AEBODSYS that sorted by total column
rand_adae \%>\%
  sfreq(
    var = "AEDECOD",
    nested_vars = "AEBODSYS",
    by = "TRT01P",
    fmt = "xx (xx.x\%)",
    denom = rand_adsl,
    row_tot = NULL,
    fctdrop = TRUE,
    nested_row = TRUE,
    .order = "desc(perc4)"
  )
}
