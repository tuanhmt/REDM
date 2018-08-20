bkt.initHMM <- function (params) {
  if( ! is.vector(params) )
    params = as.bkt.params( params )
  initHMM( c("unmastered","mastered"),
           c("1","2"),
           c( 1.0 - params$init, params$init ),
           rbind( c( 1.0 - params$learn, params$learn ),
                  c( NA, 1.0 ) ),
           rbind( c( 1.0 - params$guess, params$guess ),
                  c( params$slip, 1.0 - params$slip ) ) )
}


# convert seq from 0,1 to 1,2 for easy calculate log
bkt.convert.seq <- function (seq) {
  seq = replace(seq, seq == 1, 2)
  seq = replace(seq, seq == 0, 1)
  seq
}
