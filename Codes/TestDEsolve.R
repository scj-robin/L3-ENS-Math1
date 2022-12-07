library(deSolve)

a = acos(.5)
P <- matrix(c(cos(a), -sin(a), sin(a), cos(a)), 2, 2, byrow=TRUE)
eigen(P)

y0 <- c(.1, 0)
de <- function(t, y, parms){return(list(yDot = parms$P%*%y))}
parms <- list(P=P)
de(0, y0, parms)

tMax <- 10; step <- .001
sol <- ode(y=y0, times=seq(0, tMax, by=step), func=de, method='ode45', parms=parms)
head(sol)
par(mfrow=c(2, 2), cex=.6)
plot(sol[, 1], sol[, 2], type='l', col=1); abline(h=0, v=0)
plot(sol[, 1], sol[, 3], type='l', col=1); abline(h=0, v=0)
plot(sol[, 2:3], type='l', col=1); abline(h=0, v=0)
plot(sol[, 2:3], type='l', col=1, xlim=c(-2, 2), ylim=c(-2, 2)); abline(h=0, v=0)
