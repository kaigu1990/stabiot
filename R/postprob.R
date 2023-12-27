#' Simulation of Sample Size Determination by Bayesian
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Simulation to obtain trial operational characteristic
#'
#' @param nsample (`integer`)\cr integer value of specified sample size.
#' @param cutoff (`numeric`)\cr numeric cutoff of response rate.
#' @param p (`numeric`)\cr numeric vector of true response rate.
#' @param a,b (`numeric`)\cr parameters of Bayesian priors, defaults are a=1 and
#'  b=1 which is a weekly informative prior.
#' @param succs (`numeric`)\cr a threshold value to define pass for each simulation.
#' @param nrep (`integer`)\cr times of simulation.
#'
#' @details
#' The Prior is P(p)~Beta(1,1), likelihood is P(k|p)~Binomial(n,p), and posterior
#' can be P(p|k)~Bate(k+1, n-k+1).
#'
#' @return a vector of probability for each true response rate.
#' @export
#'
#' @examples
#' # Given that the operational characteristics are based on at least 70% posterior
#' # confidence that the true rate exceeds a threshold of interest, what is the
#' # chance of confirming a true response of at least 75% given the assumptions
#' # of various sample sizes and true response rates(0.75, 0.8, 0.85, 0.9)?
#' # It should be noted that operational characteristics are based on a Bayesian
#' # model without considering baseline stratification factors.
#' rrPostProb(nsample = 66, cutoff = 0.75, p = c(0.75, 0.8, 0.85, 0.9), succs = 0.7)
rrPostProb <- function(nsample,
                       cutoff,
                       p,
                       a = 1,
                       b = 1,
                       succs = NULL,
                       nrep = 100000) {
  perc_resp <- rep(0, length(p))
  for (i in 1:length(p)) {
    effective <- c()
    for (j in 1:nrep) {
      # number of patients response
      nresp <- rbinom(n = 1, size = nsample, prob = p[i])
      # posterior probability
      pp <- pbeta(cutoff, nresp + a, nsample - nresp + b, lower.tail = FALSE)
      status <- ifelse(pp > succs, 1, 0)
      effective <- sum(status, effective)
    }
    perc_resp[i] <- round(effective / nrep, 3) * 100
  }
  perc_resp
}
