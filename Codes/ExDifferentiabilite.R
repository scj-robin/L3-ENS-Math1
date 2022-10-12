# Exemple pour différentiabilité

rm(list=ls()); library(lattice)

###############################################################################
# Exemple : f(x, y) = -|x|*|y|
# Dérivable en x et y en (0, 0), mais pas différentiable
rhoGrid <- seq(0, 1, length.out=50); thetaGrid <- 2*acos(-1)*seq(0, 1, length.out=50);
f <- expand.grid(rho=rhoGrid, theta=thetaGrid); f$x <- f$rho*cos(f$theta); f$y <- f$rho*sin(f$theta)
nGrid <- 51; xGrid <- yGrid <- seq(-1, 1, length.out=nGrid)
f <- expand.grid(x=xGrid, y=yGrid)
f$z <- -abs(f$x)*abs(f$y)
wireframe(z ~ x * y, data=f, shade=TRUE, light.source = c(3,3,3))

nGrid <- 501; xGrid <- yGrid <- seq(-1, 1, length.out=nGrid)
zMat <- matrix(0, nGrid, nGrid)
for(i in 1:nGrid){for(j in 1:nGrid){
   # zMat[i, j] <- - abs(xGrid[i]+yGrid[j])*abs(xGrid[i]-yGrid[j])
   zMat[i, j] <- - abs(xGrid[i])*abs(yGrid[j])
}}
par(mfrow=c(2, 2))
image(xGrid, yGrid, zMat); abline(h=0, v=0, col=4, lwd=2)
plot(xGrid, zMat[, which(yGrid==0)], type='l', lwd=2)
plot(yGrid, zMat[which(xGrid==0), ], type='l', lwd=2)

###############################################################################
# Spirale
# Fonctions rho(theta), g, f
rho <- function(t){t}; drho <- function(t){1}
rho <- function(t){t^2}; drho <- function(t){2*t}
rho <- function(t){exp(t/10)}; drho <- function(t){exp(t/10)/10} # spiral logarithmique
# rho <- function(t){3}; drho <- function(t){0}
# rho <- function(t){log(t)}; drho <- function(t){1/t}
x <- function(r, t){r*cos(t)}; y <- function(r, t){r*sin(t)}
g <- function(t){c(t, rho(t))}; f <- function(r, t){c(x(r, t), y(r, t))}
# Différentielles
Jg <- function(t){c(drho(t), 1)}; Jf <- function(r, t){matrix(c(cos(t), sin(t), -r*sin(t), r*cos(t)), 2, 2)}
Jfog <- function(t){as.vector(Jf(rho(t), t)%*%Jg(t))}
# Point particulier
theta0 <- 13.3; rho0 <- rho(theta0)
xy0 <- f(rho0, theta0); x0 <- xy0[1]; y0 <- xy0[2]
Jfog0 <- Jfog(theta0)
# Courbes
theta <- 6*acos(-1)*seq(0, 1, length.out=1e3); 
par(mfrow=c(2, 2))
# x(theta)
plot(theta, x(rho(theta), theta), type='l', lwd=2, xlab='theta', ylab='x'); 
abline(v=0, h=0, col=8); abline(v=theta0, col=4, lwd=2)
points(theta0, x0, pch=20, col=2); abline(x0-Jfog0[1]*theta0, Jfog0[1], col=2, lwd=2)
# y(theta)
plot(theta, y(rho(theta), theta), type='l', lwd=2, xlab='theta', ylab='y'); 
abline(v=0, h=0, col=8); abline(v=theta0, col=4, lwd=2)
points(theta0, y0, pch=20, col=2); abline(y0-Jfog0[2]*theta0, Jfog0[2], col=2, lwd=2)
# rho(theta)
plot(x(rho(theta), theta), y(rho(theta), theta), type='l', lwd=2, xlab='x', ylab='y'); abline(v=0, h=0, col=8)
lines(c(0, 2*x(rho0, theta0)), c(0, 2*y(rho0, theta0)), col=4, lwd=2)
points(xy0[1], xy0[2], col=2, pch=20)
slope <- Jfog0[2] / Jfog0[1]
abline(y0-slope*x0, slope, col=2, lwd=2)
