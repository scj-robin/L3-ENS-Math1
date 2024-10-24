# Figures du chapitre fonctions de plusieurs variables

rm(list=ls())
figDir <- '../Figures/'
exportFig <- TRUE
par(pch=20, lwd=1); lwd=4
palette("R3")

################################################################################
# Equivalence des normes
exName <- 'EquivalenceNorme-R2'
Boule1 <- function(r, col=1, lwd=3, lty=1){
  lines(c(r, 0, -r, 0, r), c(0, r, 0, -r, 0), col=col, lwd=lwd, lty=lty)
}
Boule2 <- function(r, col=1, lwd=3, lty=1){
  theta <- seq(0, 2*acos(-1), length.out=1000)
  lines(r*cos(theta), r*sin(theta), col=col, lwd=lwd, lty=lty)
}
BouleInf <- function(r, col=1, lwd=3, lty=1){
  lines(c(r, r, -r, -r, r, r), c(-r, r, r, -r, -r, r), col=col, lwd=lwd, lty=lty)
}
par(mfrow=c(2, 2), mex=.6); xlim <- 1.2
if(exportFig){png(paste0(figDir, exName, '.png'))}
plot(0, col=0, xlim=2.1*c(-1, 1), ylim=2.1*c(-1, 1), xlab='x', ylab='y')
abline(h = 0, v = 0, col=8, lwd=2)
Boule1(1, 2); Boule1(2, 2); Boule1(sqrt(2), 2); 
Boule2(1, 3); Boule2(sqrt(2), 3); 
BouleInf(1, 4); BouleInf(sqrt(2), 4); 
if(exportFig){dev.off()}

if(exportFig){png(paste0(figDir, exName, '-1inf.png'))}
plot(0, col=0, xlim=xlim*c(-1, 1), ylim=xlim*c(-1, 1), xlab='x', ylab='y')
abline(h = 0, v = 0, col=8, lwd=2)
Boule1(1, 2) 
BouleInf(1, 4); BouleInf(1/2, 4); 
if(exportFig){dev.off()}

if(exportFig){png(paste0(figDir, exName, '-2inf.png'))}
plot(0, col=0, xlim=xlim*c(-1, 1), ylim=xlim*c(-1, 1), xlab='x', ylab='y')
abline(h = 0, v = 0, col=8, lwd=2)
Boule2(1, 3) 
BouleInf(1, 4); BouleInf(1/sqrt(2), 4); 
if(exportFig){dev.off()}

if(exportFig){png(paste0(figDir, exName, '-12.png'))}
plot(0, col=0, xlim=xlim*c(-1, 1), ylim=xlim*c(-1, 1), xlab='x', ylab='y')
abline(h = 0, v = 0, col=8, lwd=2)
Boule1(1, 2) 
Boule2(1, 3); Boule2(1/sqrt(2), 3); 
if(exportFig){dev.off()}
