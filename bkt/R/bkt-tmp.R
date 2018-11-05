#' # bforce experiment
#' bkt.ex.bforce <- function(data) {
#'   grid.params = bkt.bforce.search.grid()
#'   kc.names = names(data)
#'   bf.grid = list()
#'   n = length(data)
#'   for (i in 1:n) {
#'     bf.grid[[i]] <- bkt.bforce.search(data[[i]], grid.params)
#'   }
#'   names(bf.grid) = kc.names
#'   bf.grid
#' }
#'
#' bkt.ex.bforce.ecal <- function(data, stu.care) {
#'   # reduce grid space
#'   grid.params = bkt.bforce.search.grid()
#'   grid.params = grid.params[slip < bkt.reduce.grid(grid.params, stu.care)]
#'   kc.names = names(data)
#'   bf.grid = list()
#'   n = length(data)
#'   for (i in 1:n) {
#'     bf.grid[[i]] <- bkt.bforce.search(data[[i]], grid.params)
#'     cat(paste(i,n,sep = "/"))
#'     cat('\n')
#'   }
#'   names(bf.grid) = kc.names
#'   bf.grid
#' }
#'
#' bkt.ex.md.bforce <- function(bf1, bf2, kc.data, threshold = 3) {
#'   bf.best.params = lapply(names(bf1), function(x) bf1[[x]]$best.params)
#'   names(bf.best.params) = names(bf1)
#'   bf.best.params
#'   cbf1 = unlist(lapply(names(bf.best.params), function(kc) length(which(lapply(kc.data[[kc]], function(stu) bkt.em.model.degenerate(stu, bf.best.params[[kc]], threshold)) == TRUE))))
#'   bf.best.params = lapply(names(bf2), function(x) bf2[[x]]$best.params)
#'   names(bf.best.params) = names(bf2)
#'   bf.best.params
#'   cbf2 = unlist(lapply(names(bf.best.params), function(kc) length(which(lapply(kc.data[[kc]], function(stu) bkt.em.model.degenerate(stu, bf.best.params[[kc]], threshold)) == TRUE))))
#'   data.frame(bf1 = cbf1, bf2 = cbf2)
#' }
#'
#' bkt.ex <- function(data, stu.care) {
#'   list(bf1 = bkt.ex.bforce(data), bf2 = bkt.ex.bforce.ecal(data, stu.care))
#' }
#'
#' bkt.ex.bf.mean <- function(bf, fit = "best.fit", cost = "cost") {
#'   list(rmse = mean(unlist(lapply(bf, function(x) x[[fit]]))), time = mean(unlist(lapply(bf, function(x) x[[cost]][["user.self"]]))))
#' }
#'
#'
#'
#' #' HMM experiments
#' #'
#' #' @param hmm
#' #' @param data
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' bkt.ex.hmm <- function(hmm = bkt.to.hmm(bkt.random.params()), data) {
#'   n = length(data)
#'   kc.names = names(data)
#'   bf.grid = list()
#'   for (i in 1:n) {
#'     cat(paste("Training kc:", kc.names[i]), "...")
#'     bf.grid[[i]] = list(bf = bkt.hmm.fit(hmm, data[[i]]))
#'     cat("done \n")
#'   }
#'   names(bf.grid) = kc.names
#'   bf.grid
#' }
#'
#' bkt.ex.chmm.scale <- function(kc.data, stu.data, kc.stu.min = 100, kc.stu.max = 200, stu.kc.min = 0, stu.kc.max = 100, threshold = 3, stu.seq.min = threshold * 2, kc.bf, fc = 'mean') {
#'   if (length(kc.bf) == length(kc.data) && all(names(kc.bf) == names(kc.data))) {
#'     kc.data = Filter(function(x) (length(x) >= kc.stu.min) && (length(x) <= kc.stu.max), kc.data)
#'     kc = lapply(names(kc.data), function(x) list(data = kc.data[[x]], fit = kc.bf[[x]]$fit))
#'     stu.data = lapply(stu.data, function(x) Filter(function(y) length(y) > stu.seq.min, x))
#'     stu.data = Filter(function(x) length(x) > 0, stu.data)
#'     stu.data = Filter(function(x) (length(x) >= stu.kc.min) && (length(x) <= stu.kc.max), stu.data)
#'     names(kc) = names(kc.data)
#'     stu.care = bkt.care.stu(stu.data, threshold = threshold)
#'     kc.rmse1 = list()
#'     kc.rmse2 = list()
#'     for (i in names(kc)) {
#'       dat = kc[[i]]$data
#'       fit = kc[[i]]$fit
#'       if (fc == 'replace') {
#'         kc.rmse1[[i]] = bkt.rmse(dat, fit)
#'         kc.rmse2[[i]] = mean(unlist(lapply(names(dat), function(stu) bkt.rmse(dat[[stu]], bkt.as.stu.replace.params(fit, stu.care[[stu]])))))
#'       } else if (fc == 'mean') {
#'         kc.rmse1[[i]] = bkt.rmse(dat, fit)
#'         kc.rmse2[[i]] = mean(unlist(lapply(names(dat), function(stu) bkt.rmse(dat[[stu]], bkt.as.stu.mean.params(fit, stu.care[[stu]])))))
#'       } else if (fc == 'regression') {
#'         kc.rmse1[[i]] = bkt.rmse(dat, fit)
#'         kc.rmse2[[i]] = mean(unlist(lapply(names(dat), function(stu) bkt.rmse(dat[[stu]], bkt.as.stu.regression.params(fit, stu.care[[stu]])))))
#'       }
#'     }
#'     result = list(nKc = length(kc), rmse1 = mean(unlist(kc.rmse1)), rmse2 = mean(unlist(kc.rmse2)), overfit1 = length(Filter(function(x) x == 0, kc.rmse1)), overfit2 = length(Filter(function(x) x == 0, kc.rmse2)))
#'     return(result)
#'   }
#'   return('No results')
#' }
#'
#' bkt.grid.scale <- function(kc.step = 200, stu.step = 50, seq.dis = 3) {
#'   result = expand.grid(
#'               kc.stu.min = seq(0, 2800, kc.step),
#'               kc.stu.max = seq(0, 3000, kc.step),
#'               stu.kc.min = seq(0, 300, stu.step),
#'               stu.kc.max = seq(0, 350, stu.step),
#'               threshold = c(10),
#'               stu.seq.min = c(13),
#'               fc = c('regression')
#'             )
#'   result = result[(result$kc.stu.max - result$kc.stu.min) == kc.step,]
#'   result = result[(result$stu.kc.max - result$stu.kc.min) == stu.step,]
#'   result = result[(result$stu.seq.min - result$threshold) >= seq.dis,]
#'   return(result)
#' }
#'
#' bkt.ex.scale <- function(grid.plan = bkt.grid.scale(), kc.data, stu.data, kc.bf = list()) {
#'   n = nrow(grid.plan)
#'   nKc = c()
#'   rmse1 = c()
#'   rmse2 = c()
#'   overfit1 = c()
#'   overfit2 = c()
#'   for (i in 1:n) {
#'     row = grid.plan[i,]
#'     result = bkt.ex.chmm.scale(
#'       kc.data,
#'       stu.data,
#'       row['kc.stu.min'],
#'       row['kc.stu.max'],
#'       row['stu.kc.min'],
#'       row['stu.kc.max'],
#'       row['threshold'],
#'       row['stu.seq.min'],
#'       kc.bf,
#'       row['fc']
#'     )
#'     nKc = c(nKc, result$nKc)
#'     rmse1 = c(rmse1, result$rmse1)
#'     rmse2 = c(rmse2, result$rmse2)
#'     overfit1 = c(overfit1, result$overfit1)
#'     overfit2 = c(overfit2, result$overfit2)
#'     cat(paste(i, n, sep = '/'))
#'     cat('\n')
#'   }
#'   grid.plan$nKc = nKc
#'   grid.plan$rmse1 = rmse1
#'   grid.plan$rmse2 = rmse2
#'   grid.plan$overfit1 = overfit1
#'   grid.plan$overfit2 = overfit2
#'   grid.plan
#' }
#'
#' bkt.em.model.degenerate <- function(obs, param, threshold = 3) {
#'   obs.seq = obs
#'   obs.seq = obs.seq[!is.na(obs.seq)]
#'   obs.seq = paste(obs.seq, collapse = "")
#'   n.obs = nchar(obs.seq)
#'   # the given obs.seq must be longer than matched string
#'   if (is.list(threshold)) threshold = threshold[[1]]
#'   if (n.obs > threshold) {
#'     m.seq = paste(rep('1', threshold), collapse = "")
#'     pos = regexpr(m.seq, obs.seq)[1]
#'     if (pos != -1) {
#'       pknow.seq = bkt.pknown.seq(obs, param)
#'       if (!is.na(pknow.seq[pos + threshold - 1]) && pknow.seq[pos + threshold - 1] < 0.95) {
#'         return(TRUE)
#'       }
#'     }
#'   }
#'   return(FALSE)
#' }
#'
#' bkt.ex.chmm.md.scale <- function(kc.data, stu.data, kc.stu.min = 100, kc.stu.max = 200, stu.kc.min = 0, stu.kc.max = 100, threshold = 3, stu.seq.min = threshold * 2, kc.bf, fc = 'mean') {
#'   if (length(kc.bf) == length(kc.data) && all(names(kc.bf) == names(kc.data))) {
#'     kc.data = Filter(function(x) (length(x) >= kc.stu.min) && (length(x) <= kc.stu.max), kc.data)
#'     kc = lapply(names(kc.data), function(x) list(data = kc.data[[x]], fit = kc.bf[[x]]$fit))
#'     stu.data = lapply(stu.data, function(x) Filter(function(y) length(y) > stu.seq.min, x))
#'     stu.data = Filter(function(x) length(x) > 0, stu.data)
#'     stu.data = Filter(function(x) (length(x) >= stu.kc.min) && (length(x) <= stu.kc.max), stu.data)
#'     names(kc) = names(kc.data)
#'     stu.care = bkt.care.stu(stu.data, threshold = threshold)
#'     kc.emd1 = list()
#'     kc.emd2 = list()
#'     obs_num = 0
#'     for (i in names(kc)) {
#'       dat = kc[[i]]$data
#'       obs_num = obs_num + length(dat)
#'       fit = kc[[i]]$fit
#'       if (fc == 'replace') {
#'         kc.emd1[[i]] = length(which(lapply(names(dat), function(stu) bkt.em.model.degenerate(dat[[stu]], fit, threshold)) == TRUE))
#'         kc.emd2[[i]] = length(which(lapply(names(dat), function(stu) bkt.em.model.degenerate(dat[[stu]], bkt.as.stu.replace.params(fit, stu.care[[stu]]), threshold)) == TRUE))
#'       } else {
#'         kc.emd1[[i]] = length(which(lapply(names(dat), function(stu) bkt.em.model.degenerate(dat[[stu]], fit, threshold)) == TRUE))
#'         kc.emd2[[i]] = length(which(lapply(names(dat), function(stu) bkt.em.model.degenerate(dat[[stu]], bkt.as.stu.mean.params(fit, stu.care[[stu]]), threshold)) == TRUE))
#'       }
#'     }
#'     result = list(emd1 = sum(unlist(kc.emd1)), emd2 = sum(unlist(kc.emd2)), obs_num = obs_num)
#'     return(result)
#'   }
#'   return('No results')
#' }
#'
#'
#' bkt.ex.scale.md <- function(grid.plan = bkt.grid.scale(), kc.data, stu.data, kc.bf = list()) {
#'   n = nrow(grid.plan)
#'   emd1 = c()
#'   emd2 = c()
#'   obs_num = c()
#'   for (i in 1:n) {
#'     row = grid.plan[i,]
#'     result = bkt.ex.chmm.md.scale(
#'       kc.data,
#'       stu.data,
#'       row['kc.stu.min'],
#'       row['kc.stu.max'],
#'       row['stu.kc.min'],
#'       row['stu.kc.max'],
#'       row['threshold'],
#'       row['stu.seq.min'],
#'       kc.bf,
#'       row['fc']
#'     )
#'     emd1 = c(emd1, result$emd1)
#'     emd2 = c(emd2, result$emd2)
#'     obs_num = c(obs_num, result$obs_num)
#'     cat(paste(i, n, sep = '/'))
#'     cat('\n')
#'   }
#'
#'   grid.plan$emd1 = emd1
#'   grid.plan$emd2 = emd2
#'   grid.plan$obs_num = obs_num
#'   grid.plan
#' }
