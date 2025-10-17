# Loss functions
Norm2 <- function(u){sqrt(sum(u^2))}
LossGM <- function(m, x){as.vector(sum(sapply(1:nrow(x), function(i){Norm2(x[i, ] - m)})))}
GradLossGM <- function(m, x){rowSums(sapply(1:nrow(x), function(i){(x[i, ] - m) / Norm2(x[i, ] - m)}))}
HessLossGM <- function(m, x){
  hess <- matrix(0, ncol(x), ncol(x))
  Id <- diag(ncol(x))
  for(i in 1: nrow(x)){
    norm2 <- Norm2(x[i, ] - m)
    hess <- hess + (norm2^2 * Id - (x[i, ] - m)%o%(x[i, ] - m)) / norm2^3
  }
  as.matrix(hess)
}

# Algo Newton
NewtonGM <- function(x, m=rep(0, ncol(x)), iterMax=1e2, tol=1e-6){
  iter <- 1; diff <- 2*tol
  ossPath <- rep(NA, iterMax); lossPath <- LossGM(m, x=x)
  mPath <- matrix(NA, iterMax, d); mPath[iter, ] <- m
  while((iter < iterMax) & (diff > tol)){
    iter <- iter+1
    hess <- HessLossGM(m, x=x)
    step <- solve(hess)%*%GradLossGM(m, x=x)
    mTmp <- as.vector(m + step)
    while(LossGM(mTmp, x=x) > lossPath[iter-1]){
      step <- .5*step; mTmp <- as.vector(m + step)
    }
    diff <- Norm2(mTmp-m)
    m <- mTmp
    mPath[iter, ] <- m; lossPath[iter] <- LossGM(m, x=x)
  }
  lossPath <- lossPath[1:iter]; mPath <- mPath[1:iter, ]
  return(list(m=m, loss=lossPath[iter], iter=iter, mPath=mPath, lossPath=lossPath))
}

# Algo Quasi-Newton
QNewtonGM <- function(x, m=rep(0, ncol(x)), iterMax=1e2, tol=1e-6){
  iter <- 1; diff <- 2*tol
  lossPath <- rep(NA, iterMax); lossPath <- LossGM(m, x=x)
  mPath <- matrix(NA, iterMax, d); mPath[iter, ] <- m
  while((iter < iterMax) & (diff > tol)){
    iter <- iter+1
    hess <- HessLossGM(m, x=x)
    step <- GradLossGM(m, x=x) / diag(hess)
    mTmp <- as.vector(m + step)
    while(LossGM(mTmp, x=x) > lossPath[iter-1]){
      step <- .5*step; mTmp <- as.vector(m + step)
    }
    diff <- Norm2(mTmp-m)
    m <- mTmp
    mPath[iter, ] <- m; lossPath[iter] <- LossGM(m, x=x)
  }
  lossPath <- lossPath[1:iter]; mPath <- mPath[1:iter, ]
  return(list(m=m, loss=lossPath[iter], iter=iter, mPath=mPath, lossPath=lossPath))
}
