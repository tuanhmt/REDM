#' HMM experiments
#'
#' @param hmm
#' @param data
#'
#' @return
#' @export
#'
#' @examples
bkt.ex.hmm <- function(hmm = bkt.to.hmm(bkt.random.params()), data) {
  n = length(data)
  kc.names = names(data)
  bf.grid = list()
  for (i in 1:n) {
    cat(paste("Training kc:", kc.names[i]), "...")
    bf.grid[[i]] = list(bf = bkt.hmm.fit(hmm, data[[i]]))
    cat("done \n")
  }
  names(bf.grid) = kc.names
  bf.grid
}

bkt.ex.hmm.ecal <- function(kc.datas, stu.params, kc.params, stu.ecals) {
  # experiments with some options:
  # bkt, idw-bkt, idg-bkt, ids-bkt, idl-bkt, id3-bkt
  # 1. bkt
  kc.names = names(kc.datas)
  nKc = length(kc.datas)
  result = list()
  for (i in 1:nKc) {
    result[[i]] = list()
    cat(paste("Evaluating kc[", i, "]:..."))
    kc.data = kc.datas[[i]]
    stu.names = names(kc.data)
    kc.name = kc.names[i]
    kc.param = kc.params[[i]]
    result[[i]]$bkt = bkt.rmse(kc.data, kc.param)
    result[[i]]$"idw-bkt" = mean(unlist(lapply(names(kc.data), function(x) bkt.rmse(kc.data[[x]], bkt.combined.params(stu.params[[x]], kc.param)))))
    result[[i]]$"idg-bkt" = mean(unlist(lapply(names(kc.data), function(x) bkt.rmse(kc.data[[x]], bkt.combined.params(stu.ecals[[x]], kc.param, c("guess"))))))
    result[[i]]$"ids-bkt" = mean(unlist(lapply(names(kc.data), function(x) bkt.rmse(kc.data[[x]], bkt.combined.params(stu.ecals[[x]], kc.param, c("slip"))))))
    result[[i]]$"idl-bkt" = mean(unlist(lapply(names(kc.data), function(x) bkt.rmse(kc.data[[x]], bkt.combined.params(stu.ecals[[x]], kc.param, c("learn"))))))
    result[[i]]$"id3-bkt" = mean(unlist(lapply(names(kc.data), function(x) bkt.rmse(kc.data[[x]], bkt.combined.params(stu.ecals[[x]], kc.param, c("guess", "slip", "learn"))))))
    result[[i]]$"rid3-bkt" = mean(unlist(lapply(names(kc.data), function(x) bkt.rmse(kc.data[[x]], bkt.replaced.params(stu.ecals[[x]], kc.param)))))
    result[[i]]$"ridg-bkt" = mean(unlist(lapply(names(kc.data), function(x) bkt.rmse(kc.data[[x]], bkt.replaced.params(stu.ecals[[x]], kc.param, c("guess"))))))
    result[[i]]$"rids-bkt" = mean(unlist(lapply(names(kc.data), function(x) bkt.rmse(kc.data[[x]], bkt.replaced.params(stu.ecals[[x]], kc.param, c("slip"))))))
    result[[i]]$"ridl-bkt" = mean(unlist(lapply(names(kc.data), function(x) bkt.rmse(kc.data[[x]], bkt.replaced.params(stu.ecals[[x]], kc.param, c("learn"))))))
    result[[i]]$"mid3-bkt" = mean(unlist(lapply(names(kc.data), function(x) bkt.rmse(kc.data[[x]], bkt.mean.params(stu.ecals[[x]], kc.param)))))
    result[[i]]$"midg-bkt" = mean(unlist(lapply(names(kc.data), function(x) bkt.rmse(kc.data[[x]], bkt.mean.params(stu.ecals[[x]], kc.param, c("guess"))))))
    result[[i]]$"mids-bkt" = mean(unlist(lapply(names(kc.data), function(x) bkt.rmse(kc.data[[x]], bkt.mean.params(stu.ecals[[x]], kc.param, c("slip"))))))
    result[[i]]$"midl-bkt" = mean(unlist(lapply(names(kc.data), function(x) bkt.rmse(kc.data[[x]], bkt.mean.params(stu.ecals[[x]], kc.param, c("learn"))))))
    cat("done \n")
  }
  names(result) = kc.names
  result
}

bkt.ex.hmm.ecal.scale <- function(kc.datas, stu.datas, kc.fits, stu.fits, minThreshold = 4, maxThreshold = 10) {
  result = list()
  for (i in minThreshold:maxThreshold) {
    cat("threshold = ", i, '\n')
    stu.ecals = bkt.ecal.stu(stu.datas, threshold = i)
    result[[i]] = bkt.ex.hmm.ecal(kc.datas, stu.fits, kc.fits, stu.ecals)
  }
  result
}

bkt.evaluation <- function(rmse, c = "bkt") {
  cols = names(rmse)
  nKc = nrow(rmse)
  result = list()
  for (i in 1:length(cols)) {
    col = cols[i]
    if (col != c) {
      result[[col]] = paste(length(which(rmse[, rmse[[c]] > rmse[[col]]])), nKc, sep = "/")
    } else {
      result[[col]] = NA
    }
  }
  as.data.frame(result)
}


bkt.combined.params <- function(stu.params, kc.params, cparams = c('init', 'learn', 'guess', 'slip')) {
  result = kc.params
  if (!is.null(stu.params)) {
    if (is.vector(cparams)) {
      for (i in 1:length(cparams)) {
        name = cparams[i]
        if (bkt.param.isValid(kc.params[[name]]) && bkt.param.isValid(stu.params[[name]]))
          result[[name]] = (kc.params[[name]] * stu.params[[name]]) / ((kc.params[[name]] * stu.params[[name]]) + (1 - kc.params[[name]]))
        else if (bkt.param.isValid(stu.params[[name]]))
          result[[name]] = stu.params[[name]]
      }
    }
  }
  result
}

bkt.replaced.params <- function(stu.params, kc.params, cparams = c('learn', 'guess', 'slip')) {
  result = kc.params
  if (!is.null(stu.params)) {
    if (is.vector(cparams)) {
      for (i in 1:length(cparams)) {
        name = cparams[i]
        if (bkt.param.isValid(stu.params[[name]]))
          result[[name]] = stu.params[[name]]
      }
    }
  }
  result
}

bkt.mean.params <- function(stu.params, kc.params, cparams = c('learn', 'guess', 'slip')) {
  result = kc.params
  if (!is.null(stu.params)) {
    if (is.vector(cparams)) {
      for (i in 1:length(cparams)) {
        name = cparams[i]
        if (bkt.param.isValid(kc.params[[name]]) && bkt.param.isValid(stu.params[[name]]))
          result[[name]] = mean(c(kc.params[[name]], stu.params[[name]]))
        else if (bkt.param.isValid(stu.params[[name]]))
          result[[name]] = stu.params[[name]]
      }
    }
  }
  result
}

bkt.param.isValid <- function(number) {
  if (is.na(number) || is.null(number) || !is.numeric(number) || number == 0)
    return(FALSE)
  return(TRUE)
}

