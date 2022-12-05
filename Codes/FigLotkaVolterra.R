# Exemple pour Lotka-Volterra
# \dot N = r N - c N P, \dot P = b N P - m P
# N = proies, P = prédateurs

rm(list=ls()); library(deSolve)
figDir <- '../Figures/'
dataName <- 'LotkaVolterra'
exportFig <- TRUE

# Paramétrisation en x = b N /m, y = c P /r
r <- 2; m <- 1; parms <- c(r=r, m=m)
y0List <- list(c(.1, 0), c(0, .1), c(.1, .1), c(.25, .25), c(.5, .5), c(.75, .75)); y0Nb <- length(y0List)
tMax <- 15; tNb <- 1e3; tGrid <- seq(0, tMax, length.out=tNb)

###############################################################################
# Functions
model <- function(t, y, parms){return(list(c(parms[1]*y[1]*(1-y[2]), -parms[2]*y[2]*(1-y[1]))))}
model(0, y0List[[1]], parms)

pathList <- list()
for(i in 1:y0Nb){
   pathList[[i]] <- ode.2D(y=y0List[[i]], times=tGrid, func=model, parms=parms, dimens=c(1, 1), method='ode45')
}
preyMax <- 8; predMax <- 5

par(mfrow=c(2, 2), mex=.6)
if(exportFig){png(paste0(figDir, dataName, '-preys.png'))}
plot(tGrid, rep(preyMax, tNb), col=0, ylim=c(0, preyMax), xlab='time', ylab='preys', cex.lab=2)
abline(h=1, lty=2, col=8, lwd=3)
for(i in 1:y0Nb){lines(pathList[[i]][, 1], pathList[[i]][, 2], lwd=3, col=i)}
if(exportFig){dev.off()}
if(exportFig){png(paste0(figDir, dataName, '-predators.png'))}
plot(tGrid, rep(predMax, tNb), col=0, ylim=c(0, predMax), xlab='time', ylab='predators', cex.lab=2)
abline(h=1, lty=2, col=8, lwd=3)
for(i in 1:y0Nb){lines(pathList[[i]][, 1], pathList[[i]][, 3], lwd=3, col=i)}
if(exportFig){dev.off()}
if(exportFig){png(paste0(figDir, dataName, '-preysPredators.png'))}
plot(rep(preyMax, tNb), rep(predMax, tNb), col=0, xlim=c(0, preyMax), ylim=c(0, predMax), xlab='preys', ylab='predators', cex.lab=2)
abline(h=1, v=1, lty=2, col=8, lwd=3)
for(i in 1:y0Nb){
   lines(pathList[[i]][, 2], pathList[[i]][, 3], lwd=3, col=i)
   points(y0List[[i]][1], y0List[[i]][2], pch=20, col=i)
   }
if(exportFig){dev.off()}

par(mfrow=c(2, 2), mex=.6)
logMin <- .5e-2
if(exportFig){png(paste0(figDir, dataName, '-preysLog.png'))}
plot(tGrid, rep(preyMax, tNb), col=0, ylim=c(logMin, preyMax), xlab='time', ylab='preys', log='y', cex.lab=2)
abline(h=1, lty=2, col=8, lwd=3)
for(i in 1:y0Nb){lines(pathList[[i]][, 1], pathList[[i]][, 2], lwd=3, col=i)}
if(exportFig){dev.off()}
if(exportFig){png(paste0(figDir, dataName, '-predatorsLog.png'))}
plot(tGrid, rep(predMax, tNb), col=0, ylim=c(logMin, predMax), xlab='time', ylab='predators', log='y', cex.lab=2)
abline(h=1, lty=2, col=8, lwd=3)
for(i in 1:y0Nb){lines(pathList[[i]][, 1], pathList[[i]][, 3], lwd=3, col=i)}
if(exportFig){dev.off()}
if(exportFig){png(paste0(figDir, dataName, '-preysPredatorsLog.png'))}
plot(rep(preyMax, tNb), rep(predMax, tNb), col=0, xlim=c(logMin, preyMax), ylim=c(logMin, predMax), xlab='preys', ylab='predators', log='xy', cex.lab=2)
abline(h=1, v=1, lty=2, col=8, lwd=3)
for(i in 1:y0Nb){
   lines(pathList[[i]][, 2], pathList[[i]][, 3], lwd=3, col=i)
   points(y0List[[i]][1], y0List[[i]][2], pch=20, col=i)
}
if(exportFig){dev.off()}
