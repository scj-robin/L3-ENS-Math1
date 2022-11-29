# Figures du chapitre EDO / sytèmes dynamiques

rm(list=ls())
figDir <- '../Figures/'
exportFig <- TRUE
par(pch=20, lwd=1); lwd=4

################################################################################
# Bifurcation
exName <- 'StabiliteSystemeDynamique-Bifurcation'
muMax <- 3; muGrid <- seq(-muMax, muMax, by=.01)
if(exportFig){png(paste0(figDir, exName, '.png'))}
plot(muGrid, sqrt(muGrid), type='l', col=4, lwd=lwd, ylim=sqrt(muMax)*c(-1, 1), 
     xlab='mu', ylab='points stationnaires')
lines(muGrid, -sqrt(muGrid), col=4, lwd=lwd, )
lines(muGrid[which(muGrid <= 0)], rep(0,sum(muGrid <= 0)), col=4, lwd=lwd, )
lines(muGrid[which(muGrid > 0)], rep(0, sum(muGrid > 0)), col=2, lwd=lwd, )
if(exportFig){dev.off()}

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
