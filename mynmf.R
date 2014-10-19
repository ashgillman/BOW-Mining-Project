mynmf <- function(V, r, maxIts=1000, precision=0.1) {
  # get dimensions
  n <- dim(V)[1]
  m <- dim(V)[2]

  # initilise W and H (randomly)
  W <- matrix(runif(n*r), ncol=r)
  H <- matrix(runif(r*m), ncol=m)

  # progress bar
  pb <- txtProgressBar(min=0, max=maxIts)

  # iterate (multiplicative update, Euclidean cost fn)
  for (i in 1:maxIts) {
    W <- W * (V %*% t(H)) / (W %*% H %*% t(H))
    H <- H * (t(W) %*% V) / (t(W) %*% W %*% H)
    setTxtProgressBar(pb, i)
    cost <- (V - W %*% H) ^ 2
    if (max(cost) < precision) {
      break
    }
  }
  close(pb)

  return(list(W, H))
}
list <- structure(NA,class="result")
"[<-.result" <- function(x,...,value) {
  args <- as.list(match.call())
  args <- args[-c(1:2,length(args))]
  length(value) <- length(args)
  for(i in seq(along=args)) {
    a <- args[[i]]
    if(!missing(a)) eval.parent(substitute(a <- v,list(a=a,v=value[[i]])))
  }
  x
}

V <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow=3, byrow=TRUE)
nb <- 2

list[W, H] <- mynmf(V, nb)

print(W %*% H)