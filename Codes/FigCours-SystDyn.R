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

