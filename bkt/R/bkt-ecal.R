#' Title
#'
#' @param data.stu
#' @param threshold
#' @param result
#'
#' @return
#' @export
#'
#' @examples
bkt.care.stu <- function(stu.data, threshold = 3, result = 0.5) {
  result = lapply(stu.data, function(x) lapply(x, function(x) bkt.care.obs(x)))
  result = lapply(result, unlist)
  result = lapply(result, mean)

  return(result)
}

bkt.ecal.stu <- function(stu.data, threshold = 3) {
  result = lapply(stu.data, function(x) lapply(x, function(x) bkt.ecal.obs(x, threshold)))
  result = lapply(result, function(x) rbindlist(x))
  # calculate learn rate
  tmp = lapply(result, function(x) min(x$learn[!is.na(x$learn)]))
  min = min(unlist(tmp))
  tmp = lapply(result, function(x) max(x$learn[!is.na(x$learn)]))
  max = max(unlist(tmp))
  nStu = length(result)
  for (i in 1:nStu) {
    result[[i]]$learn = sapply(result[[i]]$learn, function(x) {ifelse(!is.na(x), (x - min) / (max - min), x)})
  }
  # mean value for each student
  result = lapply(result, function(x) x[, .(learn=mean(learn[!is.na(learn)]), slip=mean(slip[!is.na(slip)]), guess=mean(guess[!is.na(guess)]))])
  result
}

#' Calculate the carefulness of a student according to the given obs sequence
#'
#' @param obs an obs vector
#' @param threshold required number of consecutive correct responses which each of students needs to master a skill
#' @param result default carefulness
#'
#' @return the carefulness of student
#' @export
#'
#' @examples bkt.carefulness.obs(c(0,1,1,1,0,1)) -> 0.5
bkt.care.obs <- function(obs.seq, threshold = 3, result = 0.5) {
  obs.seq = obs.seq[!is.na(obs.seq)]
  obs.seq = paste(obs.seq, collapse = "")
  n.obs = nchar(obs.seq)
  # the given obs.seq must be longer than matched string
  if (n.obs > threshold) {
    m.seq = paste(rep('1', threshold), collapse = "")
    pos = regexpr(m.seq, obs.seq)[1]
    if (pos != -1) {
      obs.seq = substr(obs.seq, pos, n.obs)
      total = nchar(obs.seq) - threshold
      if (total > 0) {
        wrong.pos = unlist(gregexpr("0", obs.seq))
        freq = 0
        if (wrong.pos[1] != -1)
          freq = length(wrong.pos)
        result = 1 - freq / total
      }
    }
  }
  return(result)
}

bkt.ecal.obs <- function(obs.seq, threshold = 3) {
  obs.seq = obs.seq[!is.na(obs.seq)]
  obs.seq = paste(obs.seq, collapse = "")
  n.obs = nchar(obs.seq)
  result = data.frame(learn = NA, slip = NA, guess = NA)
  # the given obs.seq must be longer than matched string
  if (n.obs > threshold) {
    m.seq = paste(rep('1', threshold), collapse = "")
    pos = regexpr(m.seq, obs.seq)[1]
    if (pos != -1) {
      if (pos + threshold - 1 > 0)
        result$learn = pos + threshold - 1
      obs.seq.tail = substr(obs.seq, pos, n.obs)
      obs.seq.head = substr(obs.seq, 1, pos - 1)
      # calculate the slip param
      total = nchar(obs.seq.tail) - threshold
      if (total > 0) {
        wrong.pos = unlist(gregexpr("0", obs.seq.tail))
        freq = 0
        if (wrong.pos[1] != -1)
          freq = length(wrong.pos)
        if (freq / total > 0 && freq / total < 1)
          result$slip = freq / total
      }
      # calculate the guess param
      total = nchar(obs.seq.head)
      if (total > 0) {
        right.pos = unlist(gregexpr("1", obs.seq.head))
        freq = 0
        if (right.pos[1] != -1)
          freq = length(right.pos)
        if (freq / total > 0 && freq / total < 1)
          result$guess = freq / total
      }
    }
  }
  return(result)
}



bkt.reduce.grid <- function(grid.params, sec) {
  mean.slip = 1 - (mean(unlist(sec)))
  mean.slip
}

bkt.as.stu.mean.params <- function(bkt, stu.care) {
  if (!is.null(stu.care))
    bkt$slip = mean(bkt$slip, (1 - stu.care))
  bkt
}

bkt.as.stu.regression.params <- function(bkt, stu.care) {
  if (!is.null(stu.care))
    bkt$slip = (bkt$slip * (1 - stu.care)) / (bkt$slip * (1 - stu.care) + (1 - bkt$slip))
  bkt
}

bkt.as.stu.replace.params <- function(bkt, stu.care) {
  if (!is.null(stu.care))
    bkt$slip = (1 - stu.care)
  bkt
}
