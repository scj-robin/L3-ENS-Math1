# Figure spirale

figDir <- '../Figures/'
exName <- 'Spirale'
exportFig <- TRUE


theta <- seq(0, 15, length.out=1e3)
rho <- theta
x <- rho*cos(theta)
y <- rho*sin(theta)

par(mfrow=c(2, 2), mex=0.6)
plot(theta, rho, type='l', lwd=4); abline(h=0, v=0, col=8)

if(exportFig){png(paste0(figDir, exName, '-XY.png'))}
plot(x, y, type='l', lwd=4); abline(h=0, v=0, col=8)
if(exportFig){dev.off()}

if(exportFig){png(paste0(figDir, exName, '-thetaX.png'))}
plot(theta, x, type='l', lwd=4); abline(h=0, v=0, col=8)
if(exportFig){dev.off()}

if(exportFig){png(paste0(figDir, exName, '-thetaY.png'))}
plot(theta, y, type='l', lwd=3); abline(h=0, v=0, col=8)
if(exportFig){dev.off()}
