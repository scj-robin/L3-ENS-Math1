# Exercice système de Lorenz
# https://fr.wikipedia.org/wiki/Attracteur_de_Lorenz

rm(list=ls()); palette('R3'); par(pch=20, mfrow=c(1, 1))
library(deSolve); library(plot3D)

################################################################################
# Système 3D
parm3 <- list(sigma=10, rho=28, beta=8/3)
Lorenz3 <- function(t, y, parm){
  list(c(parm$sigma*(y[2] - y[1]), 
         parm$rho*y[1] - y[2] - y[1]*y[3], 
         y[1]*y[2] - parm$beta*y[3]))
}
JacobODE3 <- function(t, y, parm){
  matrix(c(-parm$sigma, parm$sigma, 0, 
           parm$rho-y[3], -1, -y[1], 
           y[2], y[1], -parm$beta), 3, 3, byrow=TRUE)  
}
# Points stationnaires
statPoint3 <- matrix(c(0, 0, 0, 
                      sqrt(parm3$beta*(parm3$rho-1)), sqrt(parm3$beta*(parm3$rho-1)), parm3$rho-1,
                      -sqrt(parm3$beta*(parm3$rho-1)), -sqrt(parm3$beta*(parm3$rho-1)), parm3$rho-1), 
                    3, 3, byrow=TRUE)
statNb <- nrow(statPoint3)
sapply(1:statNb, function(i){Lorenz3(t=NULL, y=statPoint3[i, ], parm=parm3)})
# Stabilité
sapply(1:statNb, function(i){JacobODE3(t=NULL, y=statPoint3[i, ], parm=parm3)})
sapply(1:statNb, function(i){eigen(JacobODE3(t=NULL, y=statPoint3[i, ], parm=parm3))$values})
sapply(1:statNb, function(i){eigen(JacobODE3(t=NULL, y=statPoint3[i, ], parm=parm3))$vectors})
# Trajectoire
y0 <- c(1, 2, 3)
tMax <- 30; tStep <- 1e-3; tGrid <- seq(0, tMax, by=tStep); tNb <- length(tGrid)
traj3 <- ode.3D(y=y0, times=tGrid, func=Lorenz3, parm=parm3, dimens=c(1, 1, 1), method='ode45')
lines3D(x=traj3[, 2], y=traj3[, 3], z=traj3[, 4])
# for(i in 1:floor(sqrt(tNb))){lines3D(x=traj3[1:(i*floor(sqrt(tNb))), 2], 
#                                      y=traj3[1:(i*floor(sqrt(tNb))), 3], 
#                                      z=traj3[1:(i*floor(sqrt(tNb))), 4])}
scatter3D(x=statPoint3[, 1], y=statPoint3[, 2], z=statPoint3[, 3], pch=20, col=1, add=TRUE)

################################################################################
# Système 2D
parm2 <- list(a=27, b=8/3)
Lorenz2 <- function(t, y, parm){
  list(c(parm$a*y[1] - y[1]*y[2], 
         y[1]^2 - parm$b*y[2]))
}
JacobODE2 <- function(t, y, parm){
  matrix(c(parm$a-y[2], -y[1], 
           2*y[1], -parm$b), 2, 2, byrow=TRUE)  
}
# Points stationnaires
statPoint2 <- matrix(c(0, 0, 
                      sqrt(parm2$a*parm2$b), parm2$a,
                      -sqrt(parm2$a*parm2$b), parm2$a),
                    3, 2, byrow=TRUE)
statNb <- nrow(statPoint2)
sapply(1:statNb, function(i){Lorenz2(t=NULL, y=statPoint2[i, ], parm=parm2)})
# Stabilité
sapply(1:statNb, function(i){JacobODE2(t=NULL, y=statPoint2[i, ], parm=parm2)})
sapply(1:statNb, function(i){eigen(JacobODE2(t=NULL, y=statPoint2[i, ], parm=parm2))$values})
sapply(1:statNb, function(i){eigen(JacobODE2(t=NULL, y=statPoint2[i, ], parm=parm2))$vectors})
# Trajectoire
y0 <- c(1, 3)
# tMax <- 30; tStep <- 1e-3; tGrid <- seq(0, tMax, by=tStep); tNb <- length(tGrid)
traj2 <- ode.2D(y=y0, times=tGrid, func=Lorenz2, parm=parm2, dimens=c(1, 1), method='ode45')
lines2D(x=traj2[, 2], y=traj2[, 3], col=2)
# for(i in 1:floor(sqrt(tNb))){lines3D(x=traj2[1:(i*floor(sqrt(tNb))), 2], 
#                                      y=traj2[1:(i*floor(sqrt(tNb))), 3])}
scatter2D(x=statPoint2[, 1], y=statPoint2[, 2], pch=20, col=1, add=TRUE)
