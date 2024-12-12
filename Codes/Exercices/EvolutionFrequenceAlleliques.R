rm(list=ls()); palette('R3')

m <- 5

DrawA <- function(p){A <- rgamma(m, m, 1)*matrix(runif(m^2), m, m); A <- A + t(A); A / max(A)}

Viabilite <- function(x){x <- x/sum(x); (t(x)%*%A%*%x)[1, 1]}

A <- DrawA(p)
while(sum(eigen(A)$values > 0)!=1){A <- DrawA(p)}
V <- eigen(A)$vectors
lambda <- eigen(A)$values
v1 <- V[, 1] / sum(V[, 1])

tMax <- 50
pPath <- matrix(NA, tMax, m)
pPath[1, ] <- rep(1/m, m)
for(t in 2:tMax){
  pPath[t, ] <- pPath[t-1, ] * (A %*% pPath[t-1, ])
  pPath[t, ] <- pPath[t, ] / sum(pPath[t, ])
}
pMax <- pPath[tMax, ]
VPath <- sapply(1:tMax, function(t){Viabilite(pPath[t, ])})

par(mfrow=c(2, 1))
matplot(pPath, ylim=c(0, 1), type='l', lwd=2, lty=1)
abline(h=v1, col=1:m, lty=2)
plot(VPath, type='l', lwd=2)
abline(h=Viabilite(v1), lty=2)

Viabilite(pMax)
sum(lambda*((t(V)%*%pMax)^2))
Viabilite(v1)
lambda[1]*sum(v1^2)
t(V)%*%pMax
