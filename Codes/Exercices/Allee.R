# Figure exercice modèle de Allee

rm(list=ls()); palette('R3')
library(deSolve)
exportFig <- FALSE
exoName <- 'Allee'
figDir <- '../../Figures/'
lwd <- 3

# Model
f <- function(t, x, parm=NULL){list(-x^3 + 3*x^2 - 2*x)}
df <- function(t, x, parm=NULL){-3*x^2 + 6*x - 2}

# # Solution
# u <- function(t){1 + 3*exp(-2*t)}
# x <- function(t, a=0){1 + 1/sqrt(u(t))}

# Plot
tMax <- 5; tStep=.01; tGrid <- seq(0, tMax, by=tStep)
statList <- (0:2)
x0List <- c(0.5, 0.95, 1.05, 1.55, 2.5, 3)
if(exportFig){png(paste0(figDir, exoName, '-solutions.png'))}
plot(0, col=0, xlim=c(0, tMax), ylim=c(0, 3), xlab=expression(t), ylab=expression(x(t)))
for(x0 in x0List){
  sol <- ode.1D(y=x0, times=tGrid, func=f, dimens=c(1), parm=NULL, method='ode45')
  lines(tGrid, sol[, 2], lwd=lwd)
}
abline(h=statList, lty=2, col=2+statList, lwd=lwd)
text(0.9*tMax, 0.15, label=expression(x[1]*"*"), cex=1.5)
text(0.9*tMax, 1.15, label=expression(x[2]*"*"), cex=1.5)
text(0.9*tMax, 2.15, label=expression(x[3]*"*"), cex=1.5)
if(exportFig){dev.off()}
