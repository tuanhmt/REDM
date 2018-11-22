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
    cat("done \n")
  }
  names(result) = kc.names
  result
}


# main experiments
bkt.ex.hmm.ecal.scale <- function(data.params, minThreshold = 3, maxThreshold = 10) {
  result = list()
  kc.data.params = data.params
  stu.data.params = data.params
  kc.data.params$data.info$base = 'kc'
  stu.data.params$data.info$base = 'stu'
  kc.data.prep = bkt.data.prep(kc.data.params$data.info)
  stu.data.prep = bkt.data.prep(stu.data.params$data.info)
  index = 1
  for (i in minThreshold:maxThreshold) {
    # i is threshold
    result[[index]] = list()
    result[[index]]$obsLength = i;
    result[[index]]$kc.datas = bkt.data(kc.data.prep, kc.data.params$data.info, kc.data.params$filter)
    result[[index]]$stu.datas = bkt.data(stu.data.prep, stu.data.params$data.info, stu.data.params$filter)
    index = index + 1;
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

bkt.evaluation.kc <- function(rmse, c = "bkt") {
  cols = names(rmse)
  nKc = nrow(rmse)
  result = list()
  for (i in 1:length(cols)) {
    col = cols[i]
    if (col != c) {
      result[[col]] = which(rmse[, rmse[[c]] > rmse[[col]]])
    } else {
      result[[col]] = NA
    }
  }
  result
}


bkt.combined.params <- function(stu.params, kc.params, cparams = c('init', 'learn', 'guess', 'slip')) {
  result = kc.params
  if (!is.null(stu.params)) {
    if (is.vector(cparams)) {
      for (i in 1:length(cparams)) {
        name = cparams[i]
        if (bkt.param.isValid(kc.params[[name]]) && bkt.param.isValid(stu.params[[name]])) {
          # result[[name]] = (kc.params[[name]] * stu.params[[name]]) / ((kc.params[[name]] * stu.params[[name]]) + (1 - kc.params[[name]]))
          result[[name]] = log(kc.params[[name]]/(1 - kc.params[[name]])) + log(stu.params[[name]]/(1 - stu.params[[name]]))
          result[[name]] = 1/(1 + exp(-result[[name]]))
        }
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

bkt.individual.params <- function(kc.fits, stu.fits, cparams = c('init', 'learn', 'guess', 'slip')) {
  result = lapply(kc.fits, function(x) lapply(stu.fits, function(y) bkt.combined.params(y, x, cparams)))
  result = lapply(result, function(x) do.call(rbind.data.frame, x))
  result = do.call(rbind.data.frame, result)
  result
}

bkt.check.perform <- function(params) {
  nP = nrow(params)
  result = list()
  result$theory_md = length(which(params$guess > 0.5 | params$slip > 0.5)) / nP
  result
}

bkt.params.to.table <- function(params.list) {
  return (do.call(rbind.data.frame, params.list))
}
