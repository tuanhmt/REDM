
##########################################################################################################
# Copyright (C) 2018
# Based on REDM project (http://www.educationaldatamining.org/). Power by R language.
#
# Tuan Hoang - MSC Student
# Email: tuanhcmup@gmail.com, tuan.hmt@outlook.com
# University of Science, HCM City, Vietnam
##########################################################################################################

###################################
##
## an customize of the Bayesian Knowledge Tracing
##

#' root mean squared error for a BKT model
#'
#' Calculates the root mean squared error for the BKT model
#' with the given parameters on the given data.
#' @param opps the opportunity table of observed student actions
#' @param params a BKT parameter object
#' @return the root mean squared error for the BKT model's predictions on this data
#' @export
bkt.rmse <- function(opps, params, testset.ratio = 1, testfit = TRUE) {
  if (is.vector(opps) && !is.list(opps))
    bkt.rmse.seq(opps, params, testset.ratio, testfit)
  else if (is.list(opps))
    sum(unlist(lapply(opps, bkt.rmse.seq, params = params, testset.ratio = testset.ratio, testfit = testfit))) / length(opps)
}

#' root mean squared error of the BKT model
#'
#' Calculates a root mean squared error for the BKT model
#' with the given parameters on the given data.
#' @param seq a vector of observed student actions
#' as occurs in a single row of an opportunity table
#' @param params a BKT parameter object
#' @return the Residual sum of squared errors for the BKT model's predictions on this seq
#' @export
#' @keywords internal
bkt.rmse.seq <- function(seq, params, testset.ratio = 1, testfit = TRUE) {
  opps = seq[!is.na(seq)]
  diff = opps - pcorrect.seq(opps, params)
  if (testfit) {
    testlength = length(opps) - as.integer(length(opps) / testset.ratio) * (testset.ratio - 1)
    diff = tail(diff, testlength)
    return (sqrt(as.double(diff %*% diff) / testlength))
  } else {
    diff = head(diff, length(opps) / testset.ratio)
    return (sqrt(as.double(diff %*% diff) / (length(opps) / testset.ratio)))
  }
}
