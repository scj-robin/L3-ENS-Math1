# Exercice syst dynamique en y^5
# \dot y = F(y); F(y) = mu y + 2 y^3 - y^5 = y (mu + 2 y^2 - y^4)

rm(list=ls())
figDir <- '../Figures/'
figName <- 'ExoSystDynY5'
exportFig <- TRUE

Fy <- function(y){y*(mu + 2*y^2 - y^4)}
Fpy <- function(y){mu + 6*y^2 - 5*y^4}
gz <- function(z){(mu + 2*z - z^2)}
hz <- function(z){mu + 6*z - 5*z^2}

mu <- -.5; from <- -(1+abs(mu)); to <- -from
zStar <- 1 + sqrt(mu+1)*c(-1, 1); yStar <- c(-sqrt(zStar), sqrt(zStar))
cat(Fy(yStar), '/', gz(zStar), '\n')

par(mfcol=c(2, 2)); ylim <- c(-mu, 3*mu)
curve(Fy(x), from=from, to=to, n=1001, type='l', lwd=2, ylim=ylim) 
curve(Fpy(x), from=from, to=to, n=1001, add=TRUE, lwd=2, col=2)
abline(h=0, v=c(0, yStar), lty=2)

curve(gz(x), from=from, to=to^2, n=1001, type='l', lwd=2, , ylim=ylim)
curve(hz(x), from=from, to=to^2, n=1001, add=TRUE, lwd=2, col=2)
abline(h=0, v=c(0, zStar), lty=2)

if(exportFig){png(paste0(figDir, figName, '-yStar.png'))}
plot(curve(0*x, from=-2, to=2, n=1001), 
     type='l', lwd=2, ylim=c(-3, 3), ylab=expression(y[star]), xlab=expression(mu))
curve(sqrt(1+sqrt(1+x)), from=-2, to=2, n=1001, lwd=2, add=TRUE)
curve(sqrt(1-sqrt(1+x)), from=-2, to=2, n=1001, lwd=2, add=TRUE)
curve(-sqrt(1+sqrt(1+x)), from=-2, to=2, n=1001, lwd=2, add=TRUE)
curve(-sqrt(1-sqrt(1+x)), from=-2, to=2, n=1001, lwd=2, add=TRUE)
abline(v=c(-1, 0), lty=2)
if(exportFig){dev.off()}
