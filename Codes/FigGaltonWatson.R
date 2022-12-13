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
curve(fX, ylim=c(0, 1), col=2, lwd=2, xlab='s', ylab='fX(s)')
for(i in 2:length(lambdaList)){
   lambda <- lambdaList[i]
   curve(fX, ylim=c(0, 1), col=1+i, lwd=2, xlab='s', ylab='fX(s)', add=TRUE)
}
abline(0, 1, col=1, lwd=2)
if(exportFig){dev.off()}

# Point fixe
lambda <- 1.5
sGrid <- seq(0, .99, length.out=1e3)
fGrid <- fX(sGrid)
sStar <- sGrid[min(which(abs(fGrid-sGrid) < 1e-3))]

# Iteration de la fonction génératrice
Z0 <- 1; nMax <- 100
q0 <- .00
qVec <- rep(0, 1+nMax); qVec[1] <- q0
if(exportFig){png(paste0(figDir, dataName, '-fixPoint-q0', round(100*q0), '.png'))}
for(n in 1:nMax){qVec[n+1] <- fX(qVec[n])}
curve(fX, ylim=c(0, 1), col=4, lwd=2, xlab='s', ylab='fX(s)')
abline(0, 1, col=8, lwd=2)
abline(v=qVec[nMax], col=4, lty=2, lwd=2)
abline(h=c(0, 1), v=c(0, 1))
lines(c(q0, qVec[-(nMax+1)]), c(0, qVec[-1]), type='s', col=2, lwd=2)
if(exportFig){dev.off()}

# # Cas géométrique
# fX <- function(s){(1-a)/(1-a*s)}
# a <- 2/3
# curve(fX, ylim=c(0, 1), col=2, lwd=2, xlab='s', ylab='fX(s)')
# abline(0, 1, col=1, lwd=2)
# abline(v = (1-a)/a, lwd=2, col=4)
