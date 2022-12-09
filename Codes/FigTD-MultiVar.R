# Figures du chapitre fonction de plusieurs variables

rm(list=ls()); par(pch=20)
figDir <- '../Figures/'
exportFig <- TRUE
par(pch=20, lwd=1); lwd=4
source('FonctionsSystDyn.R')

################################################################################
# Coubre parametre
exName <- 'CourbeParametreeDim2'
times <- seq(-1.1, 1.1, length.out=1001)
Ft <- function(t){c(t - t^3, t^2 - t^4)}
path <- t(sapply(times, function(t){Ft(t)}))

DFt <- function(t, parm=NULL){c(1 - 3*t^2, 2*t - 4*t^3)}
tGrid <- seq(-1.1, 1.1, length.out=11); 
tGrid <- sort(unique(c(tGrid, -1/sqrt(3), -1/sqrt(2), 0, 1/sqrt(2), 1/sqrt(3))))
grad <- t(sapply(tGrid, function(t){DFt(t)}))


if(exportFig){png(paste0(figDir, exName, '.png'))}
plot(path, type='l', lwd=2, xlab='x', ylab='y', col=8)
points(Ft(0)[1], Ft(0)[2], cex=2, pch=20)
points(rbind(Ft(1/sqrt(2)), Ft(-1/sqrt(2))), col=2, cex=2, pch=20)
points(rbind(Ft(1/sqrt(3)), Ft(-1/sqrt(3))), col=4, cex=2, pch=20)
abline(h=0, v=0)
vectorField(grad[, 1], grad[, 2], 
            sapply(tGrid, function(t){Ft(t)[1]}), 
            sapply(tGrid, function(t){Ft(t)[2]}), col=1)
if(exportFig){dev.off()}
