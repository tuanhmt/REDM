##########################################################################################################
# ITS Data Preprocessing Tool. Copyright (C) 2018
# Based on REDM project (http://www.educationaldatamining.org/). Power by R language.
#
# Tuan Hoang - Junior Researcher
# Email: tuanhcmup@gmail.com, tuan.hmt@outlook.com
# University of Science, HCM City, Vietnam
##########################################################################################################


bkt.cols.params <- function(cols, kc.oppr.cols, stu.obs.cols, base = 'kc') {
  list(cols = cols, kc.oppr.cols = kc.oppr.cols, stu.obs.cols = stu.obs.cols, base = base)
}

bkt.file.params <- function(url, sep = '~~') {
  list(url = url, sep = sep)
}

bkt.filter.params <- function(minStu = 100, minKc = 2, minObs = 4) {
  list(minStu = minStu, minKc = minKc, minObs = minObs)
}

#' as.its.data preprocessing function
#'
#' @param file.params
#' @param cols.params
#'
#' @return mrt.data tidy table data
#' @export
#'
#' @examples
bkt.data.prep <- function(file.params, cols.params) {
  # load data from KDD data txt
  cat("[1] Loading ...\n")
  mrt.data = fread(file.params$url, sep = "\t", header = TRUE, na.strings = "")
  cat("File is loaded success !! ...\n")
  Sys.sleep(0.01)

  # just keep some columns
  cat("[2] Selecting columns ...\n")
  mrt.data = mrt.data[, .SD, .SDcols = cols.params$cols]
  Sys.sleep(0.01)
  #
  # remove NA value for main columns
  cat("[3] Cleaning rows with na value ...\n")
  mrt.data = na.omit(mrt.data, cols = cols.params$kc.oppr.cols)
  # if (is.train == TRUE)
  mrt.data = na.omit(mrt.data, cols = cols.params$stu.obs.cols)
  # else
  #   mrt.data = na.omit(mrt.data, cols = stu.obs.cols[1])
  Sys.sleep(0.01)

  # Separate the multiple values KC & Opprs columns
  cat("[4] Separating rows ...\n")
  mrt.data = separate_rows(mrt.data, cols.params$kc.oppr.cols, sep = file.params$sep)
  Sys.sleep(0.01)

  # Rename and order mains columns
  cat("[5] Renaming columns ...\n")
  names(mrt.data)[cols.params$stu.obs.cols[1]] <- "stu"
  names(mrt.data)[cols.params$stu.obs.cols[2]] <- "obs"
  names(mrt.data)[cols.params$kc.oppr.cols[1]] <- "kc"
  names(mrt.data)[cols.params$kc.oppr.cols[2]] <- "oppr"
  Sys.sleep(0.01)

  # convert obs column to numeric
  cat("[5] Transforming obs to numeric ...\n")
  mrt.data = transform(mrt.data, oppr = as.numeric(oppr))
  Sys.sleep(0.01)

  # ordering obs by kc, stu & oppr
  cat("[6] Ordering columns ...")
  if (cols.params$base == 'kc')
    mrt.data = mrt.data[order(kc, stu, oppr)]
  else
    mrt.data = setorderv(mrt.data, c(cols.params$base, 'kc', 'oppr'))
  Sys.sleep(0.01)

  # Return tidy data
  cat("Done !!! ... \n")
  return(mrt.data)
}

#' as.redm.data.kc convert data to REDM format
#'
#' @param data data returned from as.its.data
#' @param minStu skills have number of student lower than this will be removed
#' @param minOppr students have number of opprs associate with a skill lower than this will be removed
#' @param first
#'
#' @return perfect data
#' @export
#'
#' @examples
bkt.data <- function(file.params, cols.params, filter.params) {
  # convert data to list skills
  data = bkt.data.prep(file.params, cols.params)
  if (cols.params$base == 'kc') {
    mrt.data = split(data, by = c('kc', 'stu'), flatten = FALSE, keep.by = FALSE)
    # remove skills which have number of students is lower than minStu
    mrt.data = Filter(function(x) length(x) > filter.params$minStu, mrt.data)
    # remove students that have number of opprs is lower than minObs
    mrt.data = lapply(mrt.data, function(x) Filter(function(y) length(y$obs) > filter.params$minObs, x))
    # remove skills again
    mrt.data = Filter(function(x) length(x) > filter.params$minStu, mrt.data)
  } else {
    mrt.data = split(data, by = c(cols.params$base, 'kc'), flatten = FALSE, keep.by = FALSE)
    # remove skills which have number of students is lower than minStu
    mrt.data = Filter(function(x) length(x) > filter.params$minKc, mrt.data)
    # remove students that have number of opprs is lower than minObs
    mrt.data = lapply(mrt.data, function(x) Filter(function(y) length(y$obs) > filter.params$minObs, x))
    # remove skills again
    mrt.data = Filter(function(x) length(x) > filter.params$minKc, mrt.data)
  }

  # just keep only obs col
  mrt.data = lapply(mrt.data, function(x) lapply(x, function(y) y$obs))
  # mrt.data = lapply(mrt.data, function(x) lapply(x, t))
  # mrt.data = lapply(mrt.data, function(x) lapply(x, as.data.table))
  # mrt.data = lapply(mrt.data, function(x) list(rbindlist(x, fill = TRUE), names(x)))
  # mrt.data = lapply(mrt.data, function(x) {tmp = as.data.frame(x[[1]]); row.names(tmp) = x[[2]]; tmp})
  # mrt.data = lapply(mrt.data, function(x) lapply(names(x), as.data.table))

  return(mrt.data)
}
