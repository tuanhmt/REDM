# convert hmm to bkt params
bkt.hmm.to.bkt <- function (hmm) {
  bkt.as.params(c(init=as.numeric(hmm$startProbs[2]),
                    learn=as.numeric(hmm$transProbs[1,2]),
                    guess=as.numeric(hmm$emissionProbs[1,2]),
                    slip=as.numeric(hmm$emissionProbs[2,1])))
}

# convert bkt to hmm params
# assumption that the knowlegde is unforgetable so mastered to unmasterd is zero
bkt.to.hmm <- function(params=bkt.random.params())
{
  if(!is.vector(params))
    params = bkt.as.params(params)
  bkt.init.hmm(c("unmastered","mastered"),
           c("1","2"),
           c(1.0 - params$init, params$init),
           rbind(c(1.0 - params$learn, params$learn),
                  c(0, 1.0)),
           rbind(c(1.0 - params$guess, params$guess),
                  c(params$slip, 1.0 - params$slip)))
}


bkt.convert.seq <- function(seq) {
  seq = replace(seq, seq == 1, 2)
  seq = replace(seq, seq == 0, 1)
  seq
}

# create a hmm params
bkt.init.hmm <- function(States, Symbols, startProbs=NULL, transProbs=NULL, emissionProbs=NULL) {
  nStates    = length(States)
  nSymbols   = length(Symbols)
  S          = rep(1/nStates,nStates)
  T          = 0.5*diag(nStates) + array(0.5/(nStates),c(nStates,nStates))
  E          = array(1/(nSymbols),c(nStates,nSymbols))
  names(S)   = States
  dimnames(T)= list(from=States,to=States)
  dimnames(E)= list(states=States,symbols=Symbols)
  if(!is.null(startProbs)){S[]  = startProbs[]}
  if(!is.null(transProbs)){T[,] = transProbs[,]}
  if(!is.null(emissionProbs)){E[,] = emissionProbs[,]}
  return(list(States=States,Symbols=Symbols,startProbs=S,transProbs=T,
              emissionProbs=E))
}

forward <- function(hmm, observation) {
  hmm$transProbs[is.na(hmm$transProbs)]       = 0
  hmm$emissionProbs[is.na(hmm$emissionProbs)] = 0
  nObservations  = length(observation)
  nStates    = length(hmm$States)
  f          = array(NA,c(nStates,nObservations))
  dimnames(f)= list(states=hmm$States,index=1:nObservations)
  # Init
  for (state in hmm$States) {
    f[state,1] = log(hmm$startProbs[state] * hmm$emissionProbs[state,observation[1]])
  }
  # Iteration
  for (k in 2:nObservations) {
    for (state in hmm$States) {
      logsum = -Inf
      for (previousState in hmm$States) {
        temp   = f[previousState,k-1] + log(hmm$transProbs[previousState,state])
        logsum = logadd(temp, logsum)
      }
      f[state,k] = log(hmm$emissionProbs[state,observation[k]]) + logsum
    }
  }
  return(f)
}

backward <- function(hmm, observation) {
  hmm$transProbs[is.na(hmm$transProbs)]       = 0
  hmm$emissionProbs[is.na(hmm$emissionProbs)] = 0
  nObservations  = length(observation)
  nStates    = length(hmm$States)
  b          = array(NA,c(nStates,nObservations))
  dimnames(b)= list(states=hmm$States,index=1:nObservations)
  # Init
  for (state in hmm$States) {
    b[state,nObservations] = log(1)
  }
  # Iteration
  for (k in (nObservations-1):1) {
    for (state in hmm$States) {
      logsum = -Inf
      for (nextState in hmm$States) {
        temp   = b[nextState,k+1] + log(hmm$transProbs[state,nextState]*hmm$emissionProbs[nextState,observation[k+1]])
        logsum = logadd(temp, logsum)
      }
      b[state,k] = logsum
    }
  }
  return(b)
}

bkt.hmm.fit <- function(hmm = bkt.to.hmm(), observation, maxIterations=100, delta=1E-9) {
  bw = baumWelch(hmm, observation, maxIterations, delta)
  return(bkt.hmm.to.bkt(bw$hmm))
}

#log likelihood calculating with each of observation
bkt.hmm.ll <- function(hmm, observation)
{
  seq = as.character(observation[! is.na(observation)])
  n_obs = length(seq)
  n_states = length(hmm$States)
  hmm$transProbs[is.na(hmm$transProbs)]       = 0
  hmm$emissionProbs[is.na(hmm$emissionProbs)] = 0
  # the log-likelihood is the sum over all states of the last forward coefficient
  f = forward(hmm, seq)
  ll = f[1,n_obs]
  if( n_states > 1 )
    for( i in 2:n_states )
      ll = logadd( ll, f[i,n_obs] )
  ll
}

baumWelch = function(hmm, observation, maxIterations=100, delta=1E-9 ) {
  tempHmm = hmm
  na.trans = is.na(hmm$transProbs)
  tempHmm$transProbs[ na.trans ]       = 0
  na.emit = is.na(hmm$emissionProbs)
  tempHmm$emissionProbs[ na.emit ] = 0
  na.start = is.na(hmm$startProbs)
  tempHmm$startProbs[ na.start ] = 0
  diff = c()
  for(i in 1:maxIterations)
  {
    #print(paste("iteration",i))
    # Expectation Step (Calculate expected Transitions and Emissions)
    newHmm = bwEx( tempHmm, observation )
    # check the amount of change in the parameters
    d = sqrt(sum((tempHmm$transProbs - newHmm$transProbs)^2)) +
      sqrt(sum((tempHmm$emissionProbs - newHmm$emissionProbs)^2)) +
      sqrt(sum((tempHmm$startProbs - newHmm$startProbs)^2))
    diff = c(diff, d)
    # print(paste("iteration:",i,"diff:",d))
    tempHmm = newHmm
    if(d < delta)
    {
      break
    }
  }
  tempHmm$transProbs[ na.trans ] = NA
  tempHmm$emissionProbs[ na.emit ] = NA
  tempHmm$startProbs[ na.start ] = NA
  return(list(hmm=tempHmm,difference=diff))
}

new.bw <- function( n_states, n_syms )
{
  list(
    # the numerator & denominator of the transition matrix
    logTransNumer = matrix( data=-Inf, nrow=n_states, ncol=n_states ),
    logTransDenom = matrix( data=-Inf, nrow=n_states, ncol=n_states ),
    # the numerator & denominator for emission matrix
    logEmitNumer = matrix( data=-Inf, nrow=n_states, ncol=n_syms ),
    logEmitDenom = matrix( data=-Inf, nrow=n_states, ncol=n_syms ),
    # the initial probabilities matrix
    logStart    = rep(-Inf,n_states)
  )
}

bw.add <- function( bw1, bw2, logweight )
{
  sum.bw = list()
  for( n in names(bw1) ){
    sum.bw[[n]] = mlogadd( bw1[[n]], ( logweight + bw2[[n]] ) )
  }
  sum.bw
}

bw.to.hmm <- function ( bw, hmm, logStartNormalizer = 0.0 )
{
  bkt.init.hmm( hmm$States,
           hmm$Symbols,
           startProbs=exp( bw$logStart - logStartNormalizer ),
           transProbs=exp( bw$logTransNumer - bw$logTransDenom ),
           emissionProbs=exp( bw$logEmitNumer - bw$logEmitDenom ) )
}

bwEx = function( hmm, observation )
{
  # switch to list data type
  if (is.list(observation)) {
    if (length(observation) < 2)
      return( bw.to.hmm( bwIter( hmm, as.character(observation[[1]][!is.na(observation[[1]])]) ), hmm))
    else
      n_seq = length(observation)
  } else if(is.vector(observation) || dim(observation)[1] < 2 || length(dim(observation)) < 2)
    return( bw.to.hmm( bwIter( hmm, as.character(observation[!is.na(observation)]) ), hmm))
  else
    n_seq = dim(observation)[1]
  bw = new.bw( length(hmm$States), length(hmm$Symbols) )
  logweight = 0.0 # = log(1.0)
  logStartNormalizer = -Inf
  for( j in 1:n_seq ){
    seq = bkt.convert.seq(observation[[j]])
    seq = as.character(seq[!is.na(seq)])
    if( length(seq) < 2 )
      next
    seq.bw = bwIter( hmm, seq )
    #logweight = bkt.hmm.ll( bw.to.hmm( seq.bw, hmm ), seq )
    bw = bw.add( bw, seq.bw, logweight )
    logStartNormalizer = logadd( logStartNormalizer, logweight )
  }
  bw.to.hmm( bw, hmm, logStartNormalizer )
}

bwIter = function( hmm, observation )
{
  n_obs = length(observation)
  n_states = length(hmm$States)
  n_syms = length(hmm$Symbols)
  f = forward(hmm,  observation)
  b = backward(hmm, observation)
  # build the gamma matrix
  gamma = f + b
  for( t in 1:n_obs ){
    gamma_denom = -Inf
    for( x in hmm$States ){
      gamma_denom = logadd( gamma_denom, gamma[x,t] )
    }
    gamma[,t] = gamma[,t] - gamma_denom
  }
  # build the xi matrix
  xi = array( data=0.0, dim=c( n_states, n_states, n_obs-1 ) )
  for( t in 1:(n_obs - 1) ){
    xi_denom = -Inf
    for( i in 1:n_states ){
      for( j in 1:n_states ){
        xi[i,j,t] = f[i,t] + log(hmm$transProbs[i,j]) +
          log(hmm$emissionProbs[j,observation[t+1]]) + b[j,t+1]
        xi_denom = logadd( xi_denom, xi[i,j,t] )
      }
    }
    xi[,,t] = xi[,,t] - xi_denom
  }
  # initialize the bw structure
  bw = new.bw( n_states, n_syms )
  # the numerator & denominator of the transition matrix
  for( i in 1:n_states ){
    for( j in 1:n_states ){
      for( t in 1:(n_obs-1) ){
        bw$logTransNumer[i,j] = logadd( bw$logTransNumer[i,j], xi[i,j,t] )
        bw$logTransDenom[i,j] = logadd( bw$logTransDenom[i,j], gamma[i,t] )
      }
    }
  }
  # the numerator & denominator for emission matrix
  for( i in 1:n_states ){
    for( j in 1:n_syms ){
      for( t in 1:n_obs ){
        if( observation[t] == hmm$Symbols[j] )
          bw$logEmitNumer[i,j] = logadd( bw$logEmitNumer[i,j], gamma[i,t] )
        bw$logEmitDenom[i,j] = logadd( bw$logEmitDenom[i,j], gamma[i,t] )
      }
    }
  }
  # initial state probabilities
  bw$logStart = gamma[,1]
  bw
}
