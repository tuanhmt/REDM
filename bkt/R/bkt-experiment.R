#' HMM experiments
#'
#' @param hmm
#' @param data
#'
#' @return
#' @export
#'
#' @examples
bkt.ex.hmm <- function(hmm = bkt.to.hmm(), data) {
  n = length(data)
  kc.names = names(data)
  bf.grid = list()
  for (i in 1:n) {
    cat(paste("Training kc:", kc.names[i]), "...")
    train = data[[i]]
    bf.grid[[kc.names[i]]] = bkt.hmm.fit(hmm, data[[i]])
    cat("done \n")
  }
  bf.grid
}

bkt.ex.id.hmm <- function(hmm = bkt.to.hmm(), data) {
  n = length(data)
  kc.names = names(data)
  bf.grid = list()
  for (i in 1:n) {
    cat(paste("Training kc:", kc.names[i]), "...")
    train = data[[i]]
    trained = lapply(train, function(x) bkt.hmm.fit(hmm, x))
    bf.grid[[kc.names[i]]] = trained
    cat("done \n")
  }
  bf.grid
}

bkt.ex.id.ecal.hmm <- function(data, stu.ecals, params = c()) {
  n = length(data)
  kc.names = names(data)
  bf.grid = list()
  for (i in 1:n) {
    cat(paste("Training kc:", kc.names[i]), "...")
    train = data[[i]]
    trained = lapply(names(train), function(stu) bkt.hmm.fit(bkt.to.hmm(bkt.generate.ecalparams(stu.ecals[[stu]], params)), train[[stu]]))
    names(trained) = names(train)
    bf.grid[[kc.names[i]]] = trained
    cat("done \n")
  }
  bf.grid
}

bkt.generate.ecalparams <- function (stu.ecal, params = c()) {
  nparams = length(params)
  result = random.params()
  if (!is.null(stu.ecal) && !is.na(stu.ecal)) {
    if (nparams > 0) {
      for (k in 1:nparams) {
        if (!is.na(stu.ecal[[params[k]]]))
          result[[params[k]]] = stu.ecal[[params[k]]]
      }
    }
  }
  result
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
    # result[[i]]$"idw-bkt" = mean(unlist(lapply(names(kc.data), function(x) bkt.rmse(kc.data[[x]], bkt.combined.params(stu.params[[x]], kc.param)))))
    result[[i]]$"idg-bkt" = mean(unlist(lapply(names(kc.data), function(x) bkt.rmse(kc.data[[x]], bkt.combined.params(stu.ecals[[x]], kc.param, c("guess"))))))
    result[[i]]$"ids-bkt" = mean(unlist(lapply(names(kc.data), function(x) bkt.rmse(kc.data[[x]], bkt.combined.params(stu.ecals[[x]], kc.param, c("slip"))))))
    # result[[i]]$"idl-bkt" = mean(unlist(lapply(names(kc.data), function(x) bkt.rmse(kc.data[[x]], bkt.combined.params(stu.ecals[[x]], kc.param, c("learn"))))))
    result[[i]]$"idgs-bkt" = mean(unlist(lapply(names(kc.data), function(x) bkt.rmse(kc.data[[x]], bkt.combined.params(stu.ecals[[x]], kc.param, c("guess", "slip"))))))
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

bkt.eval.improvement <- function(rmse, c = "bkt") {
  cols = names(rmse)
  nKc = nrow(rmse)
  result = list()
  if (c == 'reverse') {
    for (i in 1:length(cols)) {
      col = cols[i]
      if (col != c) {
        result[[col]] = length(which(rmse[, rmse[[c]] < rmse[[col]]]))
      }
    }
  }
  else {
    for (i in 1:length(cols)) {
      col = cols[i]
      if (col != c) {
        result[[col]] = length(which(rmse[, rmse[[c]] > rmse[[col]]]))
      }
    }
  }

  as.data.table(result)
}

bkt.eval.overfitting <- function(rmse) {
  cols = names(rmse)
  nKc = nrow(rmse)
  result = list()
  for (i in 1:length(cols)) {
    col = cols[i]
    result[[col]] = length(which(rmse[, rmse[[col]] == 0]))
  }
  as.data.table(result)
}

bkt.eval.theoryMD <- function(params, bkt.params) {
  result = lapply(params, function(x) length(which(x[, x$guess > 0.5 | x$slip > 0.5])) / nrow(x))
  result$bkt = length(which(bkt.params[, bkt.params$guess > 0.5 | bkt.params$slip > 0.5])) / nrow(bkt.params)

  as.data.table(result)
}

bkt.eval.all <- function(experiment) {
  result = list()
  result$threshold = experiment$threshold
  # 1. number of improved kcs
  result$improvedKCs = bkt.eval.improvement(experiment$rmse)

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

bkt.combined.params.all <- function(kc.trained, stu.ecals, kc.data, cparams = c('init', 'learn', 'guess', 'slip')) {
  kc.names = names(kc.data)
  nKC = length(kc.names)
  result = list()
  for (i in 1:nKC) {
    result[[kc.names[i]]] = list()
    kc.params = kc.trained[[kc.names[i]]][["bkt.params"]]
    stu.names = names(kc.data[[i]])
    nStu = length(stu.names)
    for (j in 1:nStu) {
      result[[kc.names[i]]][[stu.names[j]]] = list()
      stu.params = stu.ecals[[stu.names[j]]]
      if (is.numeric(stu.params$guess))
        stu.params$guess = round(stu.params$guess, 3)
      if (is.numeric(stu.params$slip))
        stu.params$slip = round(stu.params$slip, 3)
      result[[kc.names[i]]][[stu.names[j]]]$bkt.params = bkt.combined.params(stu.params, kc.params, cparams)
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
  as.data.table(result)
}

bkt.list.to.table <- function(params.list) {
  result = do.call(rbind.data.frame, params.list)
  as.data.table(result)
}

bkt.test.threhold <- function(kc.datas, stu.datas, kc.fits, stu.fits, min = 3, max = 10) {
  result = list()
  for (i in min:max) {
    cat(paste('Step[', i, ']:...', '\n'))
    result[[i]] = list()
    # result[[i]]$threshold = i
    stu.ecals = bkt.ecal.stu(stu.datas, i)
    cat(paste('Calculate RMSE...', '\n'))
    rmse = bkt.ex.hmm.ecal(kc.datas, stu.fits, kc.fits, stu.ecals)
    rmse = bkt.list.to.table(rmse)
    result[[i]]$rmse = rmse
    cat(paste('Create params table...', '\n'))
    result[[i]]$combinedParams = list()
    result[[i]]$combinedParams[['idg-bkt']] = bkt.individual.params(kc.fits, stu.ecals, cparams = c('guess'))
    result[[i]]$combinedParams[['ids-bkt']] = bkt.individual.params(kc.fits, stu.ecals, cparams = c('slip'))
    result[[i]]$combinedParams[['idgs-bkt']] = bkt.individual.params(kc.fits, stu.ecals, cparams = c('guess', 'slip'))
  }
  result
}


bkt.draw.barplot <- function(evalDF, model) {
  df_long = gather(evalDF, key = Model, value = value, sBKT, model)
  p <- ggplot(df_long, aes(k, value))
  p <- p +geom_bar(stat = "identity", aes(fill = Model), position = "dodge") +
    xlab("k") +
    ylab("Number of KCs") +
    ggtitle("Reducing RMSE of Individualized BKT Models") +
    theme_bw() +
    scale_x_continuous(breaks=seq(2, 20, 1)) +
    scale_y_continuous(breaks=seq(0, 300, 50))
  p
}

