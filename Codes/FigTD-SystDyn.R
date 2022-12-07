# Figures du chapitre EDO2 / sytèmes dynamiques

rm(list=ls()); par(pch=20)
figDir <- '../Figures/'
exportFig <- TRUE
par(pch=20, lwd=1); lwd=4
source('FonctionsSystDyn.R')

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

################################################################################
# Exo activation réciproque / compétition intra-spécifique
exName <- 'ActivationReciproque'
parm <- list(a=1/2)
EDO2 <- function(y, parm){c(parm$a*y[2] - y[1]^2, parm$a*y[1] - y[2]^2)}
# Grid for gradient field
xGrid <- yGrid <- seq(0, 2*parm$a, length.out=20); nGrid <- length(xGrid)
xyGrid <- rbind(as.matrix(expand.grid(xGrid, yGrid)), cbind(xGrid, xGrid^2/parm$a), cbind(xGrid, sqrt(parm$a*xGrid)))

# Init
y0List <- list(c(parm$a/2, parm$a/2), c(parm$a/2, 3*parm$a/2), 
               c(3*parm$a/2, parm$a/2), c(3*parm$a/2, 3*parm$a/2))
times <- seq(0, 10, by=.001)

# Figure
if(exportFig){png(paste0(figDir, exName, '.png'))}
PlotSystDyn2D(edo=EDO2, y0List=y0List, xGrid=xGrid, yGrid=yGrid, xyGrid=xyGrid, 
              parm=parm, times=times, href=parm$a, vref=parm$a)
curve(sqrt(parm$a*x), from=min(xGrid), to=max(xGrid), add=TRUE, col=8)
curve(x^2/parm$a, from=min(xGrid), to=max(xGrid), add=TRUE, col=8)
if(exportFig){dev.off()}

if(exportFig){png(paste0(figDir, exName, '-zoom.png'))}
PlotSystDyn2D(edo=EDO2, y0List=y0List, xGrid=xGrid, yGrid=yGrid, xyGrid=xyGrid, 
              xlim=parm$a*c(0.9, 1.1), ylim=parm$a*c(0.9, 1.1),
              parm=parm, times=times, href=parm$a, vref=parm$a)
curve(sqrt(parm$a*x), from=min(xGrid), to=max(xGrid), add=TRUE, col=8)
curve(x^2/parm$a, from=min(xGrid), to=max(xGrid), add=TRUE, col=8)
if(exportFig){dev.off()}

################################################################################
# Dyn 3 pop (mâles, femelles, couples)
exName <- 'DynPopMaleFemelleCouple'
parm <- list(a=1, c=1, r=1, S=.5)
EDO3 <- function(y, parm){c(-parm$a*(y[1]^2 + parm$S*y[1]) + parm$r*y[2],  
                                          parm$a*(y[1]^2 + parm$S*y[1]) - parm$c*y[2]^2)}
Delta <- parm$S^2 + 4*parm$r^2/parm$a/parm$c; 
zStar <- parm$r/parm$c; yStar <- (sqrt(Delta) - parm$S)/2
# Grid
yGrid <- zGrid <- seq(0, 2*max(yStar, zStar), length.out=25); nGrid <- length(yGrid)
# Init
y0List <- list(c(0, 0), c(yStar/2, zStar/2), c(yStar/2, 3*zStar/2), 
               c(3*yStar/2, zStar/2), c(3*yStar/2, 3*zStar/2))
# Figure
if(exportFig){png(paste0(figDir, exName, '.png'))}
PlotSystDyn2D(edo=EDO3, y0List=y0List, xGrid=yGrid, yGrid=zGrid, 
              parm=parm, href=zStar, vref=yStar, xlab='y', ylab='z')
curve(sqrt(parm$a/parm$c*(x^2 + parm$S*x)), from=min(yGrid), to=max(yGrid), n=1001, add=TRUE, col=8)
curve(parm$a/parm$r*(x^2 + parm$S*x), from=min(yGrid), to=max(yGrid), n=1001, add=TRUE, col=8)
if(exportFig){dev.off()}
