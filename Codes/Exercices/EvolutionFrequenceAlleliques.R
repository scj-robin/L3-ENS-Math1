m <- 5

DrawA <- function(p){A <- rgamma(m, m, 1)*matrix(runif(m^2), m, m); A <- A + t(A); A / max(A)}

Viabilite <- function(x){x <- x/sum(x); (t(x)%*%A%*%x)[1, 1]}

A <- DrawA(p)
while(sum(eigen(A)$values > 0)!=1){A <- DrawA(p)}

tMax <- 1000
pMat <- matrix(NA, tMax, m)
pMat[1, ] <- rep(1/m, m)
for(t in 2:tMax){
  pMat[t, ] <- pMat[t-1, ] * (A %*% pMat[t-1, ])
  pMat[t, ] <- pMat[t, ] / sum(pMat[t, ])
}
matplot(pMat, ylim=c(0, 1))
V <- eigen(A)$vectors
lambda <- eigen(A)$values
v1 <- V[, 1] / sum(V[, 1])
abline(h=v1, col=1:m, lty=2)

Viabilite <- function(x){x <- x/sum(x); (t(x)%*%A%*%x)[1, 1]}

opt <- optim(par=rep(1/m, m), fn=Viabilite, control=list(fnscale=-1))$par
opt <- opt/sum(opt)

Viabilite(opt)
Viabilite(pMat[tMax, ])
Viabilite(v1)
