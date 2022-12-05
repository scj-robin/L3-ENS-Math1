# Figures du chapitre EDO / sytèmes dynamiques

rm(list=ls()); par(pch=20)
figDir <- '../Figures/'
exportFig <- TRUE
par(pch=20, lwd=1); lwd=4

################################################################################
# Exo L2 bio SU
exName <- 'TD-SUbioL3-TD2Exo2'
yGrid <- seq(-1, 5, by=.01)
f <- function(y){-y^3 + 7*y^2 - 14*y + 8}
if(exportFig){png(paste0(figDir, exName, '.png'))}
plot(yGrid, f(yGrid), type='l', lwd=lwd, xlab='y', ylab='F(y)', ylim=c(-1, 5))
abline(h=0, v=0)
abline(v=c(1, 4), col=4, lty=2, lwd=lwd)
abline(v=2, col=2, lty=2, lwd=lwd)
if(exportFig){dev.off()}

################################################################################
# Exo activation réciproque / compétition intra-spécifique
exName <- 'ActivationReciproque'
library(plotrix); library(lattice); library(deSolve)
a <- 1/2
EDO <- function(y, a=1){c(a*y[2] - y[1]^2, a*y[1] - y[2]^2)}

# Gradient field
xGrid <- yGrid <- seq(0, 2*a, length.out=20); nGrid <- length(xGrid)
xyGrid <- as.matrix(expand.grid(xGrid, yGrid))
xyGrid <- rbind(xyGrid, cbind(xGrid, xGrid^2/a), cbind(xGrid, sqrt(a*xGrid)))
grad <- t(sapply(1:nrow(xyGrid), function(i){EDO(xyGrid[i, ], a=a)}))

# Solutions
parms <- c(a=a); 
model <- function(t, y, parms){return(list(EDO(y, a=parms[1])))}

y0List <- list(c(a/2, a/2), c(a/2, 3*a/2), c(3*a/2, a/2), c(3*a/2, 3*a/2))
solList <- list()
for(i in 1:length(y0List)){
  solList[[i]] <- ode.2D(y0List[[i]], times=seq(0, 10, by=.001), func=model, 
                         nspec=2, parms=parms, dimens=c(1, 1), method='ode45')
}

# Figure
if(exportFig){png(paste0(figDir, exName, '.png'))}
plot(xGrid, yGrid, col=0, xlab='x', ylab='y')
abline(h = 0, v = 0, col=1)
vectorField(grad[, 1], grad[, 2], as.vector(xyGrid[, 1]), as.vector(xyGrid[, 2]), col=8)
abline(h = a, v = a, col=1, lwd=2)
curve(sqrt(a*x), from=min(xGrid), to=max(xGrid), add=TRUE, col=8)
curve(x^2/a, from=min(xGrid), to=max(xGrid), add=TRUE, col=8)
for(i in 1:length(y0List)){
  points(y0List[[i]][1], y0List[[i]][2], col=1+i, pch=20)
  lines(solList[[i]][, 2], solList[[i]][, 3], col=1+i, lwd=2)
}
if(exportFig){dev.off()}

if(exportFig){png(paste0(figDir, exName, '-zoom.png'))}
plot(xGrid, yGrid, col=0, xlab='x', ylab='y', xlim=c(0.9*a, 1.1*a), ylim=c(0.9*a, 1.1*a))
abline(h = 0, v = 0, col=1)
vectorField(grad[, 1], grad[, 2], as.vector(xyGrid[, 1]), as.vector(xyGrid[, 2]), col=8)
abline(h = a, v = a, col=1, lwd=2)
curve(sqrt(a*x), from=min(xGrid), to=max(xGrid), add=TRUE, col=8)
curve(x^2/a, from=min(xGrid), to=max(xGrid), add=TRUE, col=8)
for(i in 1:length(y0List)){
  points(y0List[[i]][1], y0List[[i]][2], col=1+i, pch=20)
  lines(solList[[i]][, 2], solList[[i]][, 3], col=1+i, lwd=2)
}
if(exportFig){dev.off()}
