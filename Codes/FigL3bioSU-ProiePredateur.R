# Modèle proie - prédateur Examen L2bio-SU, exercice 2

rm(list=ls()); par(pch=20, mfrow=c(1, 1));

figDir <- '../Figures/'
dataName <- 'L3bioSU-ProiePredateur'
exportFig <- FALSE
palette('R3')


library(deSolve)

# Model
model <- function(t, m, parms=NA){
  return(list(c(m[1]*(1 - m[1]/2) - m[1]*m[2]/(1+m[1]), (m[1]-1)/(m[1]+1)*m[2])))
  }

# Parms
tMax <- 60; n <- 1e4
times <- seq(0, tMax, length.out=n)

# Stationary points
mList <- list(c(0, 0), c(2, 0), c(1, 1))

# Paths
initList <- list(c(0, .5), c(.05, 2.5), c(2, .05), c(3, .5), c(3, 1.5))
solList <- list()
for(i in 1:length(initList)){
  solList[[i]] <- ode.2D(y=initList[[i]], times=times, func=model, dimens=c(1, 1), method='ode45', parms=NA)
} 

# Plots
if(exportFig){png(paste0(figDir, dataName, '-paths.png'))}
plot(0, 0, col=0, xlim=c(0, 3), ylim=c(0, 2.5), xlab='proies', ylab='prédateurs')
abline(h=c(0, 1), v=c(0, 1), col=8)
for(i in 1:length(initList)){
  lines(solList[[i]][, 2], solList[[i]][, 3], col=1+i, lwd=2, lty=2)
  points(initList[[i]][1], initList[[i]][2], col=1+i, cex=1.5)
} 
sapply(mList, function(m){points(m[1], m[2], col=1, pch=23, cex=1.5, bg=1)})
if(exportFig){dev.off()}

if(exportFig){png(paste0(figDir, dataName, '-proies.png'))}
plot(0, 0, col=0, xlim=range(times), ylim=c(0, 3), xlab='temps', ylab='proies')
abline(h=c(0, 1), col=8)
for(i in 1:length(initList)){
  lines(solList[[i]][, 1], solList[[i]][, 2], col=1+i, lwd=2, lty=2)
  points(0, initList[[i]][1], col=1+i, cex=1.5)
} 
if(exportFig){dev.off()}

if(exportFig){png(paste0(figDir, dataName, '-predateurs.png'))}
plot(0, 0, col=0, xlim=range(times), ylim=c(0, 3), xlab='temps', ylab='prédateurs')
abline(h=c(0, 1), col=8)
for(i in 1:length(initList)){
  lines(solList[[i]][, 1], solList[[i]][, 3], col=1+i, lwd=2, lty=2)
  points(0, initList[[i]][2], col=1+i, cex=1.5)
} 
if(exportFig){dev.off()}
