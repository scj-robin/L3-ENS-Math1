# Compute geometric median

rm(list=ls()); par(pch=20); palette('R3')
seed <- 1; set.seed(seed)
source('MedianeGeometrique-Fonctions.R')
library(mvtnorm); library(RGMM)
par(mfcol=c(3, 2))
figDir <- '../../Figures/'
exoName <- 'MedianeGeometrique'
exportFig <- TRUE

# # Data
# n <- 1e2; d <- 2
# muTrue <- rep(1, d)
# sigma <- sqrt(rgamma(d, 2, 1))
# corr <- exp(-as.matrix(dist(matrix(rnorm(2*d), d, 2))))
# rho <- .99; corr <- toeplitz(rho^(0:(d-1)))
# sigmaTrue <- diag(sigma)%*%corr%*%diag(sigma)
# x <- exp(rmvnorm(n, mean=muTrue, sigma=sigmaTrue))

# Data
n <- 1e2
u <- rexp(n, rate=1/5)
x <- cbind(u + rnorm(n), u + rnorm(n))
# x <- cbind(exp((1:n)/n), exp(-(1:n)/n))
d <- ncol(x)

# Fit
robust <- RobVar(x)
newton <- NewtonGM(x=x)
quasi <- QNewtonGM(x=x)
par(mfcol=c(2, 1)); plot(newton$lossPath, type='b'); plot(quasi$lossPath, type='b')

# # Try
# if(exists('muTrue')){
#   rbind(muTrue, robust$median, newton$m, quasi$m)
#   plot(robust$median, newton$m, col=2)
#   points(robust$median, quasi$m, col=4)
#   abline(0, 1, lty=2)
#   # rbind(muTrue, NewtonGM(x=x)$m)
# }

# Plot
par(mfrow=c(1, 1))
if(exportFig){png(paste0(figDir, exoName, '.png'))}
if(d==2){
  plot(x, xlab='', ylab='', pch=20)
  abline(v=newton$m[1], h=newton$m[2], col=2, lwd=2)
  abline(v=colMeans(x)[1], h=colMeans(x)[2], col=4, lwd=2)
  abline(v=median(x[, 1]), h=median(x[, 2]), col=3, lwd=2)
}
if(exportFig){dev.off()}

