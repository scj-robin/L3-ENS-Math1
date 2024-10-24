# Caractérisation des optimum par étude de la hessienne

rm(list=ls()); library(lattice)
figDir <- '../Figures/'
dataName <- 'Verhulst'
exportFig <- TRUE

###############################################################################
# Modele & parms
tMax <- 10
r <- 1; c <- 1/10; K = r/c
f <- function(t){K*a0*exp(r*t)/(1 + a0 * exp(r*t))}

# 3 scénarios
tGrid <- seq(-tMax/2, tMax, length.out=1e2)
tList <- y0List <- c(1, K+1, -1)
yGridList <- list()
for(s in 1:length(y0List)){
   a0 <- y0List[[s]] / (K-y0List[[s]]); 
   yGridList[[s]] <- f(tGrid); 
   tList[[s]] <- -log(y0List[[s]]/(y0List[[s]]-K))/r
   yGridList[[s]][which(abs(yGridList[[s]])>10*K)] <- NA
}
yGridList[[2]][which(yGridList[[2]] < 0)] <- NA
yGridList[[3]][which(yGridList[[3]] > K)] <- NA

# Figures
if(exportFig){png(paste0(figDir, dataName, '-solutions.png'))}
plot(tGrid, yGridList[[1]], type='l', lwd=2, col=1, ylim=c(-.25*K, 1.25*K), xlab='t', ylab='y')
abline(h=0, v=0)
abline(h=K, col=1, lty=2, lwd=2)
for(s in 1:length(y0List)){
   lines(tGrid, yGridList[[s]], lwd=2, col=s); 
   abline(v=tList[s], col=s, lty=2, lwd=2)
}
if(exportFig){dev.off()}

