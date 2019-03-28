bkt.ecal.stu <- function(stu.data, k = 2) {
  result = lapply(stu.data, function(x) lapply(x, function(x) bkt.ecal.obs(x, k)))
  result = lapply(result, function(x) rbindlist(x))
  # calculate learn rate
  # tmp = lapply(result, function(x) min(x$learn[!is.na(x$learn)]))
  # min = min(unlist(tmp))
  # tmp = lapply(result, function(x) max(x$learn[!is.na(x$learn)]))
  # max = max(unlist(tmp))
  # nStu = length(result)
  # for (i in 1:nStu) {
  #   result[[i]]$learn = sapply(result[[i]]$learn, function(x) {ifelse(!is.na(x), (x - min) / (max - min), x)})
  # }
  # mean value for each student
  result = lapply(result, function(x) x[, .(slip=mean(slip[!is.na(slip)]), guess=mean(guess[!is.na(guess)]))])
  result
}

bkt.ecal.obs <- function(stu.seq, k = 2) {
  stu.seq = stu.seq[!is.na(stu.seq)]
  stu.seq = paste(stu.seq, collapse = "")
  n.obs = nchar(stu.seq)
  result = data.frame(slip = NA, guess = NA)
  # the given stu.seq must be longer than matched string
  if (n.obs > k) {
    m.seq = paste(rep('1', k), collapse = "")
    pos = regexpr(m.seq, stu.seq)[1]
    if (pos != -1) {
      stu.seq.tail = substr(stu.seq, pos, n.obs)
      stu.seq.head = substr(stu.seq, 1, pos - 1)
      # calculate the slip param
      total = nchar(stu.seq.tail) - k
      if (total > 0) {
        wrong.pos = unlist(gregexpr("0", stu.seq.tail))
        freq = 0
        if (wrong.pos[1] != -1)
          freq = length(wrong.pos)
        if (freq / total > 0 && freq / total < 1)
          result$slip = freq / total
      }
      # calculate the guess param
      total = nchar(stu.seq.head)
      if (total > 0) {
        right.pos = unlist(gregexpr("1", stu.seq.head))
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
