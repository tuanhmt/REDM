
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
## an implementation of the Bayesian Knowledge Tracing
##

#' coerces various data types to a standard format for BKT parameters
#'
#' @param p the object to be coerced
#' @param round.params whether we want to round off the parameter values to 2 decimal-places.
#' @return a data.frame with four named columns:
#' \itemize{
#'   \item init
#'   \item learn
#'   \item guess
#'   \item slip
#' }
#' @export
as.bkt.params <- function(p, round.params = T)
{
  if(is.list(p))
    kt.params = as.data.table(p)
  else if("init" %in% names(p)){
    kt.params = data.table(init=p["init"], learn=p["learn"], guess=p["guess"], slip=p["slip"])
    rownames(kt.params) = NULL
  } else
    kt.params = data.table(init=p[1], learn=p[2], guess=p[3], slip=p[4])
  if(round.params)
    kt.params = round(kt.params, 3)
  kt.params
}

#' generate random BKT parameters
#'
#' @return a BKT parameter object with parameters chosen uniformly at random in [0,1] for
#' the init and learn parameters, and in [0,0.5] for the guess and slip parameters
#'@export
bkt.random.params <- function()
{
  as.bkt.params(list(init=runif(1), learn=runif(1), guess=runif(1,0,0.5), slip=runif(1,0,0.5)))
}

#' distance between two BKT parameter vectors
#'
#' Calculates the Euclidean distance between two sets of BKT parameters
#' @param p1 a BKT parameter object, or a matrix of same
#' @param p2 another BKT parameter object, or a matrix of same
#' @return the euclidean distance between each pair of BKT parameter vectors in p1, p2
#' @export
bkt.param.dist <- function(p1, p2)
{
  se = (p1 - p2)^2
  if(is.vector(se))
    return( sqrt(sum(se)))
  sqrt(apply(se, 1, sum))
}

#' root mean squared error for a BKT model
#'
#' Calculates the root mean squared error for the BKT model
#' with the given parameters on the given data.
#' @param opps the opportunity table of observed student actions
#' @param params a BKT parameter object
#' @return the root mean squared error for the BKT model's predictions on this data
#' @export
bkt.rmse <- function(opps, params)
{
  sqrt(bkt.sse(opps, params) / sum(!is.na(unlist(opps))))
}

#' sum of squared errors of the BKT model
#'
#' Calculates a sum of squared errors for the BKT model
#' with the given parameters on the given data.
#'
#' @param opps the opportunity table of observed student actions with row names are student ids
#' @param params a list BKT parameter object for all student
#'
#' @return the sum of squared errors for the BKT model's predictions on this data
#' @export
bkt.sse <- function(opps, params)
{
  sum(unlist(lapply(opps, bkt.sse.seq, params = params)))
}

#' #' sum of squared errors of the BKT model
#'
#' Calculates a sum of squared errors for the BKT model
#' with the given parameters on the given data.
#' @param seq a vector of observed student actions
#' as occurs in a single row of an opportunity table
#' @param params a BKT parameter object
#' @return the sum of squared errors for the BKT model's predictions on this data
#' @export
#' @keywords internal
bkt.sse.seq <- function(seq, params)
{
  opps = seq[!is.na(seq)]
  diff = opps - bkt.pcorrect.seq(opps, params)
  as.double(diff %*% diff)
}

#' probability of correct responses predicted by BKT
#'
#' Calculates the probability of correct responses predicted by BKT
#' given the observed actions in a table of student opportunities
#' @param opps the opportunity table of observed student actions
#' @param params a BKT parameter object
#' @return a matrix with the same dimensions as \code{opps}, giving the running estimates of the
#' probability of a correct response predicted by the BKT model with parameters \code{params}
#' for the student represented by the corresponding row immediately prior to
#' corresponding column in \code{opps}. That is, the if the \code{(i,j)} entry in the matrix
#' had a value of 0.7, this means that the BKT algorithm with the given parameters predicts that
#' the i-th student has a 70 percent chance of producing a correct response on the j-th entry, given
#' the j-1 previously observed actions.
#' @export
bkt.pcorrect <- function(opps, params)
{
  if(is.vector(opps))
    return( bkt.pcorrect.seq(opps, params) )
  ### t(apply(opps, 1, function(stu.seq) { bkt.pcorrect.seq(stu.seq, params)}))
}

#' probability of correct responses predicted by BKT
#'
#' Calculates the probability of correct responses predicted by BKT
#' given the observed actions in a sequence of student opportunities
#' @param opps a vector of observed student actions
#' as occurs in a single row of an opportunity table
#' @param params a BKT parameter object
#' @return a vector with the same length as \code{opps}, giving running estimates of the
#' probability of a correct response predicted by the BKT model with parameters \code{params}
#' for the student given the prior actions in \code{opps}.
#' @export
#' @keywords internal
bkt.pcorrect.seq <- function(opps, params)
{
  params = as.list(params)
  n.opps = sum(!is.na(opps))
  pk = bkt.pknown.seq(opps, params)
  pc = as.double(rep(NA, length(opps)))
  pc[1:n.opps] = (pk[1:n.opps] * (1.0 - params$slip)) + ((1.0 - pk[1:n.opps]) * params$guess)
  pc
}

#' probability of student knowledge predicted by BKT
#'
#' Calculates the probability of a student being in the known state,
#' predicted by BKT given the observed actions in a table of student opportunities
#' @param opps the opportunity table of observed student actions
#' @param params a BKT parameter object
#' @return a matrix with the same rows as \code{opps}, and \code{ncol(opps)+1} columns,
#' giving running estimates of the probability of student knowledge predicted by the BKT model
#' with parameters \code{params} for the student represented by the corresponding row immediately prior to
#' corresponding column in \code{opps}. That is, the if the \code{(i,j)} entry in the matrix
#' had a value of 0.7, this means that the BKT algorithm with the given parameters predicts that
#' the i-th student has a 70 percent chance of being in the  on the j-th entry, given
#' the j-1 previously observed actions.
#' @export
bkt.pknown <- function(opps, params)
{
  if(is.vector(opps))
    return(bkt.pknown.seq(opps, params))
  ### t(apply(opps, 1, function(stu.seq) { bkt.pknown.seq(stu.seq, params)}))
}

#' probability of student knowledge predicted by BKT
#'
#' Calculates the probability of a student being in the known state,
#' predicted by BKT given the observed actions in a sequence of student opportunities
#' @param opps a vector of observed student actions
#' as occurs in a single row of an opportunity table
#' @param params a BKT parameter object
#' @return a vector with the same length as \code{opps}, giving running estimates of the
#' probability of student knowledge predicted by the BKT model with parameters \code{params}
#' for the student given the prior actions in \code{opps}.
#' @export
#' @keywords internal
bkt.pknown.seq <- function(opps, params)
{
  params = as.list(params)
  n.opps = sum(!is.na(opps))
  pk = as.double(rep(NA,length(opps) + 1))
  pk[1] = params$init
  if(n.opps > 0){
    for(i in 1:n.opps)
      pk[i + 1] = bkt.posterior.pknown(opps[i], params, pk[i])
  }
  pk
}

#' updates a BKT estimate of student knowledge
#'
#' applies the BKT algorithm to update the estimate of student knowledge given a single observed action
#' @param result an observed student action
#' @param params a BKT parameter object
#' @param prior.pknown a prior on student knowledge, defaults to the init parameter
#' @return a posterior probability on student knowledge, given the parameters and observed data
#' @keywords internal
#' @export
bkt.posterior.pknown <- function(result, params, prior.pknown=params$init)
{
  params = as.list(params)
  correct = (result == 1)
  # calculate the posterior given observed result
  posterior = double(length(result))
  posterior[correct] = (prior.pknown[correct] * (1.0 - params$slip)) / ((prior.pknown[correct] * (1.0 - params$slip)) + ((1.0 - prior.pknown[correct]) * params$guess))
  posterior[!correct] = (prior.pknown[!correct] * params$slip) / ((prior.pknown[!correct] * params$slip) + ((1.0 - prior.pknown[!correct]) * (1.0 - params$guess)))
  # posterior with learning
  (posterior + (1.0 - posterior) * params$learn)
}
