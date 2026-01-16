library(plotrix); library(lattice); library(deSolve)

PlotSystDyn2D <- function(edo, y0List=NULL, xGrid, yGrid, xyGrid=NULL,
                          parm, times=seq(0, 10, by=.001), xlim=NULL, ylim=NULL,
                          href=NULL, vref=NULL, xlab='x', ylab='y'){
  if(is.null(xyGrid)){xyGrid <- as.matrix(expand.grid(xGrid, yGrid))}
  # Gradients
  grad <- t(sapply(1:nrow(xyGrid), function(i){edo(xyGrid[i, ], parm=parm)}))
  # Solutions
  model <- function(t, y, parm){return(list(edo(y, parm=parm)))}
  for(i in 1:length(y0List)){
  }
  # Figure
  plot(xGrid, yGrid, col=0, xlab='x', ylab='y', xlim=xlim, ylim=ylim)
  abline(h = 0, v = 0, col=1)
  vectorField(grad[, 1], grad[, 2], as.vector(xyGrid[, 1]), as.vector(xyGrid[, 2]), col=8)
  abline(h = href, v = vref, col=1, lwd=2, lty=2)
  if(!is.null(y0List)){
    solList <- list()
    for(i in 1:length(y0List)){
      points(y0List[[i]][1], y0List[[i]][2], col=1+i, pch=20, cex=1.5)
      solList[[i]] <- ode.2D(y0List[[i]], times=times, func=model, 
                             nspec=2, parm=parm, dimens=c(1, 1), method='ode45')
      lines(solList[[i]][, 2], solList[[i]][, 3], col=1+i, lwd=3)
    }
  }else{solList <- NULL}
  return(solList=solList)
}