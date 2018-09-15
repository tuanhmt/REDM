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
                  c(NA, 1.0)),
           rbind(c(1.0 - params$guess, params$guess),
                  c(params$slip, 1.0 - params$slip)))
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


baumWelch = function(hmm, observation, maxIterations=100, delta=1E-9, pseudoCount=0) {
  tempHmm = hmm
  tempHmm$transProbs[is.na(hmm$transProbs)]       = 0
  tempHmm$emissionProbs[is.na(hmm$emissionProbs)] = 0
  diff = c()
  for (i in 1:maxIterations) {
    # Expectation Step (Calculate expected Transitions and Emissions)
    bw = baumWelchRecursion(tempHmm, observation)
    T  = bw$TransitionMatrix
    E  = bw$EmissionMatrix
    # Pseudocounts
    T[!is.na(hmm$transProbs)]    = T[!is.na(hmm$transProbs)]    + pseudoCount
    E[!is.na(hmm$emissionProbs)] = E[!is.na(hmm$emissionProbs)] + pseudoCount
    # Maximization Step (Maximise Log-Likelihood for Transitions and Emissions-Probabilities)
    T = (T/apply(T,1,sum))
    E = (E/apply(E,1,sum))
    d = sqrt(sum((tempHmm$transProbs-T)^2)) + sqrt(sum((tempHmm$emissionProbs-E)^2))
    diff = c(diff, d)
    tempHmm$transProbs    = T
    tempHmm$emissionProbs = E
    if (d < delta) {
      break
    }
  }
  tempHmm$transProbs[is.na(hmm$transProbs)]       = NA
  tempHmm$emissionProbs[is.na(hmm$emissionProbs)] = NA
  return(list(hmm=tempHmm,difference=diff))
}

baumWelchRecursion = function(hmm, observation) {
  TransitionMatrix    = hmm$transProbs
  TransitionMatrix[,] = 0
  EmissionMatrix      = hmm$emissionProbs
  EmissionMatrix[,]   = 0
  f = forward(hmm,  observation)
  b = backward(hmm, observation)
  probObservations = f[1,length(observation)]
  for (i in 2:length(hmm$States)) {
    j = f[i,length(observation)]
    if (j > - Inf) {
      probObservations = j + log(1+exp(probObservations-j))
    }
  }
  for (x in hmm$States) {
    for (y in hmm$States) {
      temp = f[x,1] + log(hmm$transProbs[x,y]) +
        log(hmm$emissionProbs[y,observation[1+1]]) + b[y,1+1]
      for (i in 2:(length(observation)-1)) {
        j = f[x,i] + log(hmm$transProbs[x,y]) +
          log(hmm$emissionProbs[y,observation[i+1]]) + b[y,i+1]
        if (j > - Inf) {
          temp = j + log(1+exp(temp-j))
        }
      }
      temp = exp(temp - probObservations)
      TransitionMatrix[x,y] = temp
    }
  }
  for (x in hmm$States) {
    for (s in hmm$Symbols) {
      temp = -Inf
      for (i in 1:length(observation)) {
        if (s == observation[i]) {
          j = f[x,i] + b[x,i]
          if (j > - Inf) {
            temp = j + log(1+exp(temp-j))
          }
        }
      }
      temp = exp(temp - probObservations)
      EmissionMatrix[x,s] = temp
    }
  }
  return(list(TransitionMatrix=TransitionMatrix,EmissionMatrix=EmissionMatrix))
}
