# Exemple Galton-Watson

figDir <- '../Figures/'
dataName <- 'GaltonWatson'
exportFig <- TRUE
palette('R3')

# Fonction
fX <- function(s){exp(lambda*(s-1))}
# Pgf
lambdaList <- c(.5, 1, 1.5)
if(exportFig){png(paste0(figDir, dataName, '-pgf.png'))}
lambda <- lambdaList[1]
curve(fX, ylim=c(0, 1), col=2, lwd=3, xlab='s', ylab='fX(s)')
for(i in 2:length(lambdaList)){
   lambda <- lambdaList[i]
   curve(fX, ylim=c(0, 1), col=1+i, lwd=3, xlab='s', ylab='fX(s)', add=TRUE)
}
abline(0, 1, col=1, lwd=3)
abline(h=c(0, 1), v=c(0, 1), col=8)
if(exportFig){dev.off()}

# Point fixe
lambda <- 1.5
sGrid <- seq(0, .99, length.out=1e3)
fGrid <- fX(sGrid)
sStar <- sGrid[min(which(abs(fGrid-sGrid) < 1e-3))]

# Iteration de la fonction génératrice
Z0 <- 1; nMax <- 100
q0 <- .75
qVec <- rep(0, 1+nMax); qVec[1] <- q0
if(exportFig){png(paste0(figDir, dataName, '-fixPoint-q0', round(100*q0), '.png'))}
for(n in 1:nMax){qVec[n+1] <- fX(qVec[n])}
curve(fX, ylim=c(0, 1), col=4, lwd=3, xlab='s', ylab='fX(s)')
abline(0, 1, col=1, lwd=3)
abline(v=qVec[nMax], col=4, lty=2, lwd=3)
abline(h=c(0, 1), v=c(0, 1), col=8)
lines(c(q0, qVec[-(nMax+1)]), c(0, qVec[-1]), type='s', col=2, lwd=3)
if(exportFig){dev.off()}

# # Cas géométrique
# fX <- function(s){(1-a)/(1-a*s)}
# a <- 2/3
# curve(fX, ylim=c(0, 1), col=2, lwd=2, xlab='s', ylab='fX(s)')
# abline(0, 1, col=1, lwd=2)
# abline(v = (1-a)/a, lwd=2, col=4)
# abline(h=c(0, 1), v=c(0, 1), col=8)

# Cas Poisson
fX <- function(s){exp(lambda*(s-1))}
gX <- function(s){fX(s)-s}
lambda <- 4/3
curve(fX, ylim=c(0, 1), col=2, lwd=2, xlab='s', ylab='fX(s)')
abline(0, 1, col=1, lwd=2)
abline(v = uniroot(gX, c(1e-4, 1-1e-4))$root, lwd=2, col=4)
abline(h=c(0, 1), v=c(0, 1), col=8)

gXlambda <- function(s, lambda){exp(lambda*(s-1))-s}
qPoisson <- function(lambda){uniroot(gXlambda, c(1e-4, 1-1e-4), lambda=lambda)$root}
qPoisson(1+1e-4)
lambdaGrid <- seq(1+1e-4, 5, length.out=1e2)
plot(lambdaGrid, sapply(lambdaGrid, function(lambda){qPoisson(lambda)}), 
     type='l', col=2, lwd=2, xlab='lambda', ylab='q')
abline(h=.5, v=2*log(2), col=4, lty=2, lwd=2)

qGrid <- seq(1e-4, 1-1e-4, length.out=1000)
plot(qGrid, -log(qGrid)/(1-qGrid), type='l', col=2, lwd=2, xlab='q', ylab='lambda')
abline(h=-log(0.001)/(1-0.001), v=0.001, col=4, lty=2, lwd=2)

