# Exercice chemostat simplifie
# https://umr5558-shiny.univ-lyon1.fr/web/

rm(list=ls()); palette('R3'); par(pch=20, mfrow=c(1, 1))
library(deSolve)
source('../Fonctions/FonctionsSystDyn.R')
figDir <- '../../Figures/'
exportFig <- TRUE
exoName <- 'ChemostatSimplifie'

################################################################################
# Système 2D
parm <- list(a=3, b=1)
Chemostat <- function(y, parm){
  c(parm$a*y[1]*y[2] - y[1], 
    -y[1]*y[2] - y[2] + parm$b)
}
Jacob <- function(y, parm){
  matrix(c(parm$a*y[2]-1, parm$a*y[1], 
           -y[2], -y[1]-1), 
         2, 2, byrow=TRUE)  
}

# Points stationnaires
statPoint <- matrix(c(0, parm$b, 
                       parm$a*parm$b - 1, 1/parm$a),
                    2, 2, byrow=TRUE)
statNb <- nrow(statPoint)
sapply(1:statNb, function(i){Chemostat(y=statPoint[i, ], parm=parm)})

# Stabilité
sapply(1:statNb, function(i){Jacob(y=statPoint[i, ], parm=parm)})
sapply(1:statNb, function(i){eigen(Jacob(y=statPoint[i, ], parm=parm))$values})
sapply(1:statNb, function(i){eigen(Jacob(y=statPoint[i, ], parm=parm))$vectors})

# Champ de gradient
tMax <- 50; tStep <- 1e-3; tGrid <- seq(0, tMax, by=tStep); tNb <- length(tGrid)
xGrid <- seq(0, 2*max(statPoint[, 1]), length.out=20); 
yGrid <- seq(0, 2*max(statPoint[, 2]), length.out=20)
xyGrid <- rbind(as.matrix(expand.grid(xGrid, yGrid)))
if(exportFig){png(paste0(figDir, exoName, '-gradient.png'))}
PlotSystDyn2D(edo=Chemostat, y0List=NULL, xGrid=xGrid, yGrid=yGrid, xyGrid=xyGrid,
              parm=parm, times=tGrid, 
              vref=statPoint[2, 1], href=statPoint[2, 2])
# xxGrid <- xGrid + 1 - parm$b; ddGrid <- xxGrid^2 + 4*parm$b; yyGrid <- (-xxGrid + sqrt(ddGrid))/2
# lines(xGrid, yyGrid, col=1, lty=2, lwd=2)
yyGrid <- parm$b / (xGrid + 1)
lines(xGrid, yyGrid, col=1, lty=2, lwd=2)
points(statPoint[, 1], statPoint[, 2], pch=20, cex=2)
if(exportFig){dev.off()}

# Trajectoires
y0List <- list(c(0, 0), 
               c(.1*statPoint[2, 1], 0), 
               c(.1*statPoint[2, 1], statPoint[1, 2]), 
               c(.5*statPoint[2, 1], 2*statPoint[1, 2]), 
               c(2*statPoint[2, 1], statPoint[2, 2]))
if(exportFig){png(paste0(figDir, exoName, '-path.png'))}
PlotSystDyn2D(edo=Chemostat, y0List=y0List, xGrid=xGrid, yGrid=yGrid, xyGrid=xyGrid,
              parm=parm, times=tGrid, 
              vref=statPoint[2, 1], href=statPoint[2, 2])
lines(xGrid, yyGrid, col=1, lty=2, lwd=2)
points(statPoint[, 1], statPoint[, 2], pch=20, cex=2)
if(exportFig){dev.off()}

# # Avec deSolve
# traj2 <- ode.2D(y=y0List[[2]], times=tGrid, func=function(t, y, parm){list(Chemostat(y, parm))}, 
#                 parms=parm, dimens=c(1, 1), method='ode45')
# lines2D(x=traj2[, 2], y=traj2[, 3], col=2)
