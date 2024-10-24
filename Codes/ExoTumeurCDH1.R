# Exercice pour tumeur CDH1

rm(list=ls())
par(mfrow=c(2, 1)); palette('R3')
dirFig <- '../Figures/'
exportFig <- TRUE
exoName <- 'TumeurCDH1'

# Parms 
lambda <- 1; tMax <- 10
muList <- 2^((-3):0); muNb <- length(muList)
tNb <- 1e2; tGrid <- seq(0, tMax, length.out=tNb)

# 1 - proba d'apparition d'une tumeur de type II
if(exportFig){png(paste0(dirFig, exoName, '-ProbaApparition.png'), height=360)}
plot(tGrid, rep(0, tNb), ylim=c(0, 1), type='l', col=0, xlab='temps', ylab='', cex.lab=1.5)
for(m in 1:muNb){
  mu <- muList[m]
  proba <- (1 - exp(-mu * tGrid)) / (mu * tGrid)
  lines(tGrid, proba, col=1+m, lwd=3)
}
if(exportFig){dev.off()}

# Proba d'absence de tumeur de type I et II
if(exportFig){png(paste0(dirFig, exoName, '-ProbaAbsence.png'), height=360)}
plot(tGrid, exp(-lambda*tGrid), type='l', col=1, xlab='temps', ylab='', lwd=3, cex.lab=1.5)
for(m in 1:muNb){
  mu <- muList[m]
  proba <- (1 - exp(-mu * tGrid)) / (mu * tGrid)
  lines(tGrid, exp(-lambda*tGrid*(1 - proba)), col=1+m, lwd=3)
}
if(exportFig){dev.off()}

# Proba d'apparition d'une tumeur de type II = 1/2
muGrid <- seq(min(muList), max(muList), length.out=1e2)
timeHalf <- sapply(muGrid, function(mu){
  uniroot(f=function(t){exp(-mu*t) -1 - mu*(log(2)/lambda - t)}, lower=0, upper=10*tMax)$root
})
plot(muGrid, timeHalf, , type='l', col=1, xlab=expression(mu), ylab='', lwd=3, cex.lab=1.5)
