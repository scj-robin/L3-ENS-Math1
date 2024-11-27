# Exercice chemostat
# https://umr5558-shiny.univ-lyon1.fr/web/

rm(list=ls()); palette('R3'); par(pch=20, mfrow=c(1, 1))
library(deSolve); library(plot3D)

################################################################################
# Système 3D
parm <- list(a=3, b=1)
Chemostat <- function(t, y, parm){
  list(c(parm$a*y[1]*y[2]/(1+y[2]) - y[1], 
         -y[1]*y[2]/(1+y[2]) - y[2] + parm$b))
}
Jacob <- function(t, y, parm){
  matrix(c(parm$a*y[2]/(1+y[2])-1, parm$a*y[1]/(1+y[2])^2, 
           -y[2]/(1+y[2]), -y[1]/(1+y[2])^2-1), 
         2, 2, byrow=TRUE)  
}
# Points stationnaires
statPoint <- matrix(c(0, parm$b, 
                       parm$a*parm$b - parm$a/(parm$a-1), 1/(parm$a-1)),
                    2, 2, byrow=TRUE)
statNb <- nrow(statPoint)
sapply(1:statNb, function(i){Chemostat(t=NULL, y=statPoint[i, ], parm=parm)})
# Stabilité
sapply(1:statNb, function(i){Jacob(t=NULL, y=statPoint[i, ], parm=parm)})
# sapply(1:statNb, function(i){eigen(Jacob(t=NULL, y=statPoint[i, ], parm=parm))$values})
# sapply(1:statNb, function(i){eigen(Jacob(t=NULL, y=statPoint[i, ], parm=parm))$vectors})
# # Trajectoire
# y0 <- c(1, 3)
# # tMax <- 30; tStep <- 1e-3; tGrid <- seq(0, tMax, by=tStep); tNb <- length(tGrid)
# traj2 <- ode.2D(y=y0, times=tGrid, func=Chemostat, parm=parm, dimens=c(1, 1), method='ode45')
# lines2D(x=traj2[, 2], y=traj2[, 3], col=2)
# # for(i in 1:floor(sqrt(tNb))){lines3D(x=traj2[1:(i*floor(sqrt(tNb))), 2], 
# #                                      y=traj2[1:(i*floor(sqrt(tNb))), 3])}
# scatter2D(x=statPoint[, 1], y=statPoint[, 2], pch=20, col=1, add=TRUE)
