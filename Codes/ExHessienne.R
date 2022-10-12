# Caractérisation des optimum par étude de la hessienne

rm(list=ls()); library(lattice)
figDir <- '../Figures/'
dataName <- 'ExempleOptimum'
exportFig <- TRUE

###############################################################################
# Exemple : f(x, y) = x^3 +  y^3 - 3xy
f <- function(x, y){x^3 +  y^3 - 3*x*y}
f1 <- function(x){2*x^3 - 3*x^2} # 1ère bissectrice : x = y
f2 <- function(x){3*x^2} # 1ère bissectrice : x = -y

grid <- seq(-2, 2, length.out=100)
xy <- expand.grid(x=grid, y=grid)
xy$z <- sapply(1:nrow(xy), function(i){f(xy$x[i], xy$y[i])})
if(exportFig){png(paste0(figDir, dataName, '-surface.png'))}
wireframe(z ~ x * y, data=xy, shade=TRUE, light.source = c(3, 3, 3), 
          screen = list(z=60, x=-70), default.scales=list(arrows=FALSE), 
          xlim=c(-1, 2), ylim=c(-1, 2), zlim=c(-2, 10))
if(exportFig){dev.off()}

if(exportFig){png(paste0(figDir, dataName, '-1ereBissectrice.png'))}
plot(grid, f1(grid), type='l', lwd=2, xlab='x=y', ylab='f(x, y)', ylim=c(-5, 5)); abline(h=0, v=0)
if(exportFig){dev.off()}

if(exportFig){png(paste0(figDir, dataName, '-2emeBissectrice.png'))}
plot(grid, f2(grid), type='l', lwd=2, xlab='x=-y', ylab='f(x, y)'); abline(h=0, v=0)
if(exportFig){dev.off()}
