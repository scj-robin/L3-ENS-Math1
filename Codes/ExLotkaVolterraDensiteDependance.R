# Lotka-Volterra model with density-dependence

rm(list=ls()); par(mfrow=c(1, 1), lwd=2)
library(deSolve)
exportFig <- TRUE
dirFig <- '../Figures/'
figName <- 'LotkaVolterraDD'

# Parms
tMax <- 15; tNb <- 1e3; tGrid <- seq(0, tMax, by=.05)
y0List <- list(c(2, 1.5), c(.1, 3), c(.1, .1), c(.25, .25), c(.5, .5), c(.75, .75)); 
y0Nb <- length(y0List)

################################################################################
# Regular Lotka-Volterra (r = 1)
# xp = x ( 1 - y); yp = - m y (1 - x)
m <- 1; parms1 <- c(m=m)
Model1 <- function(t, y, parms){return(list(c(y[1]*(1-y[2]), -parms[1]*y[2]*(1-y[1]))))}
Model1(0, y0List[[1]], parms1)
path1List <- list()
for(i in 1:y0Nb){
  path1List[[i]] <- ode.2D(y=y0List[[i]], times=tGrid, func=Model1, parms=parms1, dimens=c(1, 1), method='ode45')
}
xMax <- 1.1*max(sapply(1:y0Nb, function(i){max(path1List[[i]][, 2])}))
yMax <- 1.1*max(sapply(1:y0Nb, function(i){max(path1List[[i]][, 3])}))
xMin <- 0.9*min(sapply(1:y0Nb, function(i){min(path1List[[i]][, 2])}))
yMin <- 0.9*min(sapply(1:y0Nb, function(i){min(path1List[[i]][, 3])}))
plot(path1List[[1]][, 2], path1List[[1]][, 3], xlab='preys', ylab='predators', 
     log='xy', xlim=c(xMin, xMax), ylim=c(yMin, yMax), type='l', lty=2)
abline(h=1, v=1, lty=2, col=8)
for(i in 1:y0Nb){
  lines(path1List[[i]][, 2], path1List[[i]][, 3], col=i, lty=2)
  points(y0List[[i]][1], y0List[[i]][2], pch=20, col=i)
}

################################################################################
# Lotka-Volterra with density-dependence
# xp = x ( 1 - y); yp = - m y (1 - x)
m <- 0.5; a <- 1.5; parms2 <- c(m=m, a=a)
a1 <- 2*(sqrt(m*(m+1)) - m)
tMax <- 100; tNb <- 1e3; tGrid <- seq(0, tMax, by=.05)
Model2 <- function(t, y, parms){return(list(c(y[1]*(1-y[2]-parms[2]*y[1]), -parms[1]*y[2]*(1-y[1]))))}
Stat <- function(parms){c(1, 1-parms[2])}
JacobStat <- function(parms){matrix(c(-parms[2], -1, parms[1]*(1-parms[2]), 0), byrow=TRUE, 2, 2)}
Carac <- function(lambda, J){det(lambda*diag(2) - J)}
PolStat <- function(lambda, parms){lambda^2 + parms[2]*lambda + parms[1]*(1 - parms[2])}
Model2(0, y0List[[1]], parms2)
path2List <- list()
for(i in 1:y0Nb){
  path2List[[i]] <- ode.2D(y=y0List[[i]], times=tGrid, func=Model2, parms=parms2, dimens=c(1, 1), method='ode45')
}
xMax <- 1.1*max(sapply(1:y0Nb, function(i){max(path2List[[i]][, 2])}))
yMax <- 1.1*max(sapply(1:y0Nb, function(i){max(path2List[[i]][, 3])}))
xMin <- 0.9*min(sapply(1:y0Nb, function(i){min(path2List[[i]][, 2])}))
yMin <- 0.9*min(sapply(1:y0Nb, function(i){min(path2List[[i]][, 3])}))
plot(path2List[[1]][, 2], path2List[[1]][, 3], xlab='preys', ylab='predators', 
     log='xy', xlim=c(xMin, xMax), ylim=c(yMin, yMax), type='l', lty=2)
abline(h=1, v=1, lty=2, col=8)
for(i in 1:y0Nb){
  lines(path2List[[i]][, 2], path2List[[i]][, 3], col=i)
  points(y0List[[i]][1], y0List[[i]][2], pch=20, col=i)
}
abline(h=Stat(parms2)[2], lty=2, col=8)

stat <- Stat(parms2)
Model2(0, stat, parms2)
Jstat <- JacobStat(parms2)
print(Jstat)
print(matrix(c(-a, -1, m*(1-a), 0), byrow=TRUE, 2, 2))
print(c(det(Jstat), m*(1-a)))
lambda <- rnorm(1); print(c(Carac(lambda, Jstat), PolStat(lambda, parms2)))
delta <- a^2 - 4*m*(1 - a)
print(eigen(Jstat)$values)
print(c((-a+sqrt(as.complex(delta)))/2, (-a-sqrt(as.complex(delta)))/2))

################################################################################
# Different parms
par(mfrow=c(1, 1), lwd=1)
tMax <- 100; tNb <- 1e3; tGrid <- seq(0, tMax, by=.01)
y0Num <- 1; y0 <- y0List[[y0Num]]
m <- 0.5; a1 <- 2*(sqrt(m*(m+1)) - m)
# aList <- c(0.25, 0.5, 0.9, 1, 1.1, 1.5); parmsNb <- length(aList)
aList <- c(0.5, 0.9, 1.5); parmsNb <- length(aList)
parmsList <- list(); for(i in 1:parmsNb){parmsList[[i]] <- c(m=m, a=aList[i])}
path3List <- list()
for(i in 1:parmsNb){
  path3List[[i]] <- ode.2D(y=y0, times=tGrid, func=Model2, parms=parmsList[[i]], dimens=c(1, 1), method='ode45')
}
xMax <- 1.1*max(sapply(1:parmsNb, function(i){max(path3List[[i]][, 2])}))
yMax <- 1.1*max(sapply(1:parmsNb, function(i){max(path3List[[i]][, 3])}))
xMin <- 0; #0.9*min(sapply(1:parmsNb, function(i){min(path3List[[i]][, 2])}))
yMin <- 0; #0.9*min(sapply(1:parmsNb, function(i){min(path3List[[i]][, 3])}))
plot(path3List[[1]][, 2], path3List[[1]][, 3], xlab='preys', ylab='predators', 
      xlim=c(xMin, xMax), ylim=c(yMin, yMax), type='l')
points(y0[1], y0[2], pch=20)
abline(v=Stat(parmsList[[1]])[1], lty=2, col=1)
abline(h=Stat(parmsList[[1]])[2], lty=2, col=1)
for(i in 1:parmsNb){
  lines(path3List[[i]][, 2], path3List[[i]][, 3], col=i)
  abline(h=Stat(parmsList[[i]])[2], lty=2, col=i)
}
abline(h=0, v=0)

# Plots
par(mfrow=c(2, 2))
yMax <- max(yMax, max(path1List[[y0Num]][, 3]))
for(i in 1:parmsNb){
  if(exportFig){png(paste0(dirFig, figName, '-m', m, '-a', aList[i], '.png'))}
  plot(path3List[[i]][, 2], path3List[[i]][, 3], xlab='x', ylab='y', 
       xlim=c(xMin, xMax), ylim=c(yMin, yMax), type='l', lwd=3, cex.axis=1.5, cex.lab=2)
  lines(path1List[[y0Num]][, 2], path1List[[y0Num]][, 3], col=8)
  points(y0[1], y0[2], pch=20, cex=3)
  abline(h=1, v=1, lty=2, col=8)
  abline(h=0, v=0)
  if(exportFig){dev.off()}
  # if(aList[i]>1){abline(v=1/aList[i], lty=2)}
}

# Stationary state and Jacobian
a1 <- 2*(sqrt(m*(m+1)) - m)
for(parms in parmsList){
  cat('parms:', parms, '\n')
  Jstat <- JacobStat(parms); 
  # print(Jstat)
  cat(eigen(Jstat)$values, '\n')
  cat(eigen(Jstat)$vectors[, 1], ' / ', eigen(Jstat)$vectors[, 2], '\n\n')
}

aList <- seq(0, 3*m, by=.01); parmsNb <- length(aList)
parmsList <- list(); for(i in 1:parmsNb){parmsList[[i]] <- c(m, aList[i])}
eigenVal <- matrix(NA, parmsNb, 2)
for(i in 1:parmsNb){
  ev <- eigen(JacobStat(parmsList[[i]]))$values
  eigenVal[i, ] <- ev[order(Re(ev))]
}

par(mfrow=c(2, 1), lwd=1)
if(exportFig){png(paste0(dirFig, figName, '-m', m, '-eigenValueReal.png'), width=900, height=250)}
par(mar=c(5, 5, 4, 2)+.1)
plot(aList, Re(eigenVal[, 1]), type='l', ylim=range(Re(eigenVal), na.rm=TRUE), lwd=2, 
     xlab='a', ylab=expression(Re(lambda)), cex.axis=1, cex.lab=1.5)
lines(aList, Re(eigenVal[, 2]), lwd=2)
abline(h=0, v=c(a1, 1), lty=2)
if(exportFig){dev.off()}
# delta <- aList^2 - 4*m*(1 - aList)
# lambda <- cbind((-aList+sqrt(as.complex(delta)))/2, (-aList-sqrt(as.complex(delta)))/2)
# lines(aList, Re(lambda[, 2]), col='pink', lty=2)
# lines(aList, Re(lambda[, 1]), col='cyan', lty=2)

if(exportFig){png(paste0(dirFig, figName, '-m', m, '-eigenValueImaginary.png'), width=900, height=250)}
par(mar=c(5, 5, 4, 2)+.1)
plot(aList, Im(eigenVal[, 1]), type='l', ylim=range(Im(eigenVal), na.rm=TRUE), lwd=2, 
     xlab='a', ylab=expression(Im(lambda)), cex.axis=1, cex.lab=1.5)
lines(aList, Im(eigenVal[, 2]), lwd=2)
abline(h=0, v=c(a1, 1), lty=2)
if(exportFig){dev.off()}

