# Exemple pour Lotka-Volterra
# \dot N = r N - c N P, \dot P = b N P - m P
# N = proies, P = prédateurs

rm(list=ls()); library(deSolve)
figDir <- '../Figures/'
dataName <- 'SIR'
exportFig <- TRUE

# Paramétrisation en rho = a/r, x = rho*S, y = I
r <- 1; a <- 2; parms <- c(r=r, a=a)
y0List <- list(c(.5, .1), c(1, .1), c(1.5, .1), 
               c(.5, .5), c(1, .5), c(1.5, .5), 
               c(.5, 1), c(1, 1), c(1.5, 1)); y0Nb <- length(y0List)
tMax <- 5; tNb <- 1e3; tGrid <- seq(0, tMax, length.out=tNb)

###############################################################################
# Functions
model <- function(t, y, parms){return(list(c(-parms[1]*y[1]*y[2], parms[2]*y[2]*(y[1]-1))))}
model(0, y0List[[1]], parms)

pathList <- list()
for(i in 1:y0Nb){
   pathList[[i]] <- ode.2D(y=y0List[[i]], times=tGrid, func=model, parms=parms, dimens=c(1, 1), method='ode45')
}
xMax <- 2; yMax <- 2

par(mfcol=c(3, 2), mex=.6)
if(exportFig){png(paste0(figDir, dataName, '-x.png'))}
plot(tGrid, rep(xMax, tNb), col=0, ylim=c(0, xMax), xlab='time', ylab='x(t)', cex.lab=2)
abline(h=0, lty=2, col=8, lwd=3)
for(i in 1:y0Nb){lines(pathList[[i]][, 1], pathList[[i]][, 2], lwd=3, col=i)}
if(exportFig){dev.off()}
if(exportFig){png(paste0(figDir, dataName, '-y.png'))}
plot(tGrid, rep(yMax, tNb), col=0, ylim=c(0, yMax), xlab='time', ylab='y(t)', cex.lab=2)
abline(h=0, lty=2, col=8, lwd=3)
for(i in 1:y0Nb){lines(pathList[[i]][, 1], pathList[[i]][, 3], lwd=3, col=i)}
if(exportFig){dev.off()}
if(exportFig){png(paste0(figDir, dataName, '-xy.png'))}
plot(rep(xMax, tNb), rep(yMax, tNb), col=0, xlim=c(0, xMax), ylim=c(0, yMax), xlab='x(t)', ylab='y(t)', cex.lab=2)
abline(h=0, v=1, lty=2, col=8, lwd=3)
for(i in 1:y0Nb){
   lines(pathList[[i]][, 2], pathList[[i]][, 3], lwd=3, col=i)
   points(y0List[[i]][1], y0List[[i]][2], pch=20, col=i)
   }
if(exportFig){dev.off()}


iRef <- 1
rho <- r/a; S0 <- y0List[[iRef]][1]/rho; N <- S0 + y0List[[iRef]][2]
if(exportFig){png(paste0(figDir, dataName, '-finalState.png'))}
f <- function(x){x*exp(-x/rho)}
xGrid <- seq(0, 2.5, by=.01)
plot(xGrid, f(xGrid), type='l', lwd=3, xlab='S*', ylab='', col=4)
abline(v = rho, col=8, lwd=2, lty=2)
abline(h = S0*exp(-N/rho), lwd=2, lty=2, col=iRef)
abline(v=xGrid[min(which(abs(f(xGrid) - S0*exp(-N/rho)) < 1e-3))], col=iRef, lty=2, lwd=2)
if(exportFig){dev.off()}

