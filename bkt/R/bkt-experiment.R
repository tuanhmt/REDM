bkt.ex.bforce <- function(data) {
  grid.params = bkt.bforce.search.grid()
  kc.names = names(data)
  bf.grid = list()
  n = length(data)
  for (i in 1:n) {
    bf.grid[[i]] <- bkt.bforce.search(data[[i]], grid.params)
  }
  names(bf.grid) = kc.names
  bf.grid
}

bkt.ex.bforce.ecal <- function(data, stu.care) {
  # reduce grid space
  grid.params = bkt.bforce.search.grid()
  grid.params = grid.params[slip < bkt.reduce.grid(grid.params, stu.care)]
  kc.names = names(data)
  bf.grid = list()
  n = length(data)
  for (i in 1:n) {
    bf.grid[[i]] <- bkt.bforce.search(data[[i]], grid.params)
  }
  names(bf.grid) = kc.names
  bf.grid
}

bkt.ex <- function(data, stu.care) {
  list(bf1 = bkt.ex.bforce(data), bf2 = bkt.ex.bforce.ecal(data, stu.care))
}

bkt.ex.bf.mean <- function(bf) {
  list(rmse = mean(unlist(lapply(bf, function(x) x$best.fit))), time = mean(unlist(lapply(bf, function(x) x$cost[["user.self"]]))))
}

bkt.kdd.params <- function(data.info = bkt.data.info.params(), filter.params = bkt.filter.params()) {
  data.params = list(filter = filter.params)
  if (data.info$year == '2004-2005') {
    data.info$cols = c(2,4,6,11,14,15,16,18,19)
  }
  data.params$data.info = data.info
  data.params
}
