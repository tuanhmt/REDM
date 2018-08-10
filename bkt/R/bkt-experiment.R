ex.bforce <- function(data) {
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
