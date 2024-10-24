# Figure marche aléatoire

figDir <- '../Figures/'
dataName <- 'MarcheAleatoire'
exportFig <- TRUE
palette('R3')

# Parms
p <- 1/3; X0 <- 0

# Moments
nMax <- 1000; nGrid <- 0:nMax;
espXn <- nGrid*(2*p - 1)
varXn <- 4*nGrid*p*(1-p)

# Trajectoires
B <- 100; 
traj <- t(sapply(1:B, function(b){c(0, cumsum(2*rbinom(nMax, 1, p)-1))}))

# Figures
if(exportFig){png(paste0(figDir, dataName, '-p', round(100*p), '.png'))}
plot(0, 0, xlim=range(nGrid), ylim=range(traj), col=0, xlab='n', ylab='Xn')
abline(h=0, v=0)
sapply(1:B, function(b){lines(nGrid, traj[b, ], col=8)})
lines(nGrid, espXn, col=2, lwd=2)
lines(nGrid, espXn+qnorm(.975)*sqrt(varXn), col=2, lwd=2, lty=2)
lines(nGrid, espXn+qnorm(.025)*sqrt(varXn), col=2, lwd=2, lty=2)
abline(h=-50, col=4, lwd=2, lty=4)
if(exportFig){dev.off()}
