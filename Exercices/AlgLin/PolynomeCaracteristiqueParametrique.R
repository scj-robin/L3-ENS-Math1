rm(list=ls())
library(fields); library(Matrix); palette('R3')
par(mfrow=c(2, 1), lwd=2)

A <- function(a, b){matrix(c(-1, 3, 1, 0, 2, 1, a, b, 2), 3, 3, byrow=TRUE)}
A(1, 4)
eigen(A(1, -1))
eigen(A(4, 4))

aGrid <- bGrid <- seq(-1, 10, by=1)

nbEig <- t(sapply(aGrid, function(a){sapply(bGrid, function(b){
  # rankMatrix(A(a, b))
  eigA <- eigen(A(a, b))
  eigA$vectors <- round(eigA$vectors, 6)
  min(length(which(abs(Im(eigA$values)) < 1e-6)), length(which(as.vector(dist(eigA$vectors)) > 1e-6)))
})}))
image.plot(aGrid, bGrid, nbEig, xlabel='a', ylabel='b')
abline(9, -1, col=7, lwd=2); abline(0, -1, , col=7, lwd=2); abline(h=0, v=0, lwd=1)

imEig <- t(sapply(aGrid, function(a){sapply(bGrid, function(b){
  eigA <- eigen(A(a, b))
  sum(abs(Im(eigA$vectors)))
})}))
image.plot(aGrid, bGrid, imEig, xlabel='a', ylabel='b')
abline(9, -1, col=7, lwd=2); abline(0, -1, , col=7, lwd=2); abline(h=0, v=0, lwd=1)
