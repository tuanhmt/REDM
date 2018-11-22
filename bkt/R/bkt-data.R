##########################################################################################################
# ITS Data Preprocessing Tool. Copyright (C) 2018
# Based on REDM project (http://www.educationaldatamining.org/). Power by R language.
#
# Tuan Hoang - Kid Researcher
# Email: tuanhcmup@gmail.com, tuan.hmt@outlook.com
# University of Science, HCM City, Vietnam
##########################################################################################################


bkt.data.info.params <- function(name = "KDD",
                                 year = "2008-2009",
                                 url = "../../BKT/Data/algebra_2008_2009/algebra_2008_2009_train.txt",
                                 sep = "\t",
                                 kc.sep = '~~',
                                 cols = c(2,4,6,11,14,15,16,20,21),
                                 kc.oppr.cols = c(8,9),
                                 stu.obs.cols = c(1,5),
                                 base = 'kc') {
  list(name = name, year = year, url = url, sep = sep, kc.sep = kc.sep, cols = cols, kc.oppr.cols = kc.oppr.cols, stu.obs.cols = stu.obs.cols, base = base)
}

bkt.filter.params <- function(minStu = 100, minKc = 2, minObs = 4) {
  list(minStu = minStu, minKc = minKc, minObs = minObs)
}

#' as.its.data preprocessing function
#'
#' @param data.info
#' @param data.info
#'
#' @return mrt.data tidy table data
#' @export
#'
#' @examples
bkt.data.prep <- function(data.info = bkt.data.info.params()) {
  # load data from KDD data txt
  cat("[1] Loading ...")
  mrt.data = fread(data.info$url, sep = data.info$sep, header = TRUE, na.strings = "")
  cat("File is successfully loaded  !! ...\n")
  Sys.sleep(0.01)

  # just keep some columns
  cat("[2] Selecting columns ...\n")
  mrt.data = mrt.data[, .SD, .SDcols = data.info$cols]
  Sys.sleep(0.01)
  #
  # remove NA value for main columns
  cat("[3] Cleaning rows with na value ...\n")
  mrt.data = na.omit(mrt.data, cols = data.info$kc.oppr.cols)
  # if (is.train == TRUE)
  mrt.data = na.omit(mrt.data, cols = data.info$stu.obs.cols)
  # else
  #   mrt.data = na.omit(mrt.data, cols = stu.obs.cols[1])
  Sys.sleep(0.01)

  # Separate the multiple values KC & Opprs columns
  cat("[4] Separating rows ...\n")
  mrt.data = separate_rows(mrt.data, data.info$kc.oppr.cols, sep = data.info$kc.sep)
  Sys.sleep(0.01)

  # Rename and order mains columns
  cat("[5] Renaming columns ...\n")
  names(mrt.data)[data.info$stu.obs.cols[1]] <- "stu"
  names(mrt.data)[data.info$stu.obs.cols[2]] <- "obs"
  names(mrt.data)[data.info$kc.oppr.cols[1]] <- "kc"
  names(mrt.data)[data.info$kc.oppr.cols[2]] <- "oppr"
  Sys.sleep(0.01)

  # convert obs column to numeric
  cat("[6] Transforming obs to numeric ...\n")
  mrt.data = transform(mrt.data, oppr = as.numeric(oppr))
  Sys.sleep(0.01)

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
bkt.data <- function(data.prep, data.info = bkt.data.info.params(), filter.params = bkt.filter.params()) {
  mrt.data = data.prep
  # ordering obs by kc, stu & oppr
  cat("[7] Ordering columns ... \n")
  if (data.info$base == 'kc')
    data = mrt.data[order(kc, stu, oppr)]
  else
    data = setorderv(mrt.data, c(data.info$base, 'kc', 'oppr'))
  Sys.sleep(0.01)

  # Return tidy data
  cat("[8] Converting to list !!! ... \n")
  if (data.info$base == 'kc') {
    mrt.data = split(data, by = c('kc', 'stu'), flatten = FALSE, keep.by = FALSE)
    # remove skills which have number of students is lower than minStu
    mrt.data = Filter(function(x) length(x) > filter.params$minStu, mrt.data)
    # remove students that have number of opprs is lower than minObs
    mrt.data = lapply(mrt.data, function(x) Filter(function(y) length(y$obs) > filter.params$minObs, x))
    # remove skills again
    mrt.data = Filter(function(x) length(x) > filter.params$minStu, mrt.data)
  } else {
    mrt.data = split(data, by = c(data.info$base, 'kc'), flatten = FALSE, keep.by = FALSE)
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
  # Return tidy data
  cat("Done !!! ... \n")
  return(mrt.data)
}


#' Example KDD Data Params
#'
#' @param data.info
#' @param filter.params
#'
#' @return
#' @export
#'
#' @examples
bkt.kdd.params <- function(data.info = bkt.data.info.params(), filter.params = bkt.filter.params()) {
  data.params = list(filter = filter.params)
  if (data.info$year == '2004-2005') {
    data.info$cols = c(2,4,6,11,14,15,16,18,19)
  }
  data.params$data.info = data.info
  data.params
}
