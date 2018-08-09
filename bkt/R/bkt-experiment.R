ex.bforce <- function(data) {
  grid.params = bkt.bforce.search.grid();
  bf.grid = list(c(0));
  for (i in 1:data.length) {
    bf.grid[i] <- bkt.bforce.search(data[[i]], grid.params)
  }
}
