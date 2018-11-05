bkt.kfold <- function(k = 10, kc.data = list(), train = bkt.hmm.fit, test = bkt.rmse) {
  folds = rep_len(1:k, length(kc.data))
  # actual cross validation
  rmse = c()
  for(i in 1:k) {
    # actual split of the data
    fold = which(folds == i)
    kc.train = kc.data[-fold]
    kc.test = kc.data[fold]
    bkt = train(observation = kc.train)$fit
    rmse = c(rmse, test(kc.test, bkt))
  }
  return(mean(rmse))
}
