bkt.evaluation.rmse <- function(data, params, id = FALSE, testset.ratio = 1, mean = FALSE, testfit = TRUE) {
  nKc = length(params)
  kc.names = names(data)
  result = params
  for (i in 1:nKc) {
    kc = data[[i]]
    if (id) {
      nStu = length(params[[i]])
      stu.names = names(params[[i]])
      for (j in 1:nStu) {
        result[[i]][[j]]$data = kc[[j]]
        result[[i]][[j]]$rmse = bkt.rmse(kc[[j]], params[[i]][[j]][["bkt.params"]], testset.ratio)
      }
    } else {
      # non-individual evaluation per kc
      result[[i]]$data = kc
      result[[i]]$rmse = bkt.rmse(kc, params[[i]][["bkt.params"]], testset.ratio)
    }
  }

  if (id) {
    if (mean) {
      tmp = lapply(result, function(x) lapply(x, function(y) y$rmse))
      tmp = lapply(tmp, function(x) unlist(x))
      tmp = lapply(tmp, function(x) x[!is.na(x)])
      result = lapply(tmp, mean)
    }
  } else {
    if (mean) {
      result = lapply(result, function(x) x$rmse)
    }
  }
  result
}
