# Exercice pour tumeur CDH1

rm(list=ls())
par(mfrow=c(2, 1))

# Parms 
lambda <- 1; tMax <- 5
muList <- 2^((-2):0); muNb <- length(muList)
tNb <- 1e2; tGrid <- seq(0, tMax, length.out=tNb)

# Proba d'apparition d'une tumeur de type II
plot(tGrid, rep(0, tNb), ylim=c(0, 1), type='l', col=0, xlab='temps', ylab='', cex.lab=1.5)
for(m in 1:muNb){
  mu <- muList[m]
  proba <- (1 - exp(-mu * tGrid)) / (mu * tGrid)
  lines(tGrid, 1 - proba, col=1+m, lwd=3)
}

# Proba d'absence de tumeur de type I et II
plot(tGrid, exp(-lambda*tGrid), type='l', col=1, xlab='temps', ylab='', lwd=3, cex.lab=1.5)
for(m in 1:muNb){
  mu <- muList[m]
  proba <- (1 - exp(-mu * tGrid)) / (mu * tGrid)
  lines(tGrid, exp(-lambda*tGrid*(1 - proba)), col=1+m, lwd=3)
}
