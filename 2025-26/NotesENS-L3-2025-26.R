# Notes ENS-L3-2025-26

rm(list=ls()); par(mfrow=c(1, 1), pch=20)
library(fields)

# Notes
notes <- read.csv(file='NotesENS-L3-2025-26.csv', sep=';', header=TRUE)
rownames(notes) <- notes[, 2]; notes <- as.matrix(notes[, c(3:5)])
head(notes)
plot(as.data.frame(notes))

# Coefs
coefs <- c(4/4, 4/3, 10/13)

# Grille
gridNb <- 101
c2grid <- seq(1, 6/3, length.out=gridNb)
c3grid <- seq(6/13, 1, length.out=gridNb)
moyenne <- rattrap <- nb20 <- matrix(NA, gridNb, gridNb)
for(i in 1:gridNb){for(j in 1:gridNb){
  tmp <- ceiling(notes %*% c(coefs[1], c2grid[i], c3grid[j]))
  tmp[which(tmp > 20)] <- 20
  moyenne[i, j] <- mean(tmp, na.rm=TRUE)
  rattrap[i, j] <- sum(tmp<10, na.rm=TRUE)
  nb20[i, j] <- sum(tmp>=20, na.rm=TRUE)
}}
par(mfrow=c(2, 2))
image.plot(c2grid, c3grid, moyenne, main='moyenne'); abline(v=coefs[2], h=coefs[3], lwd=2)
image.plot(c2grid, c3grid, rattrap, main='rattrap'); abline(v=coefs[2], h=coefs[3], lwd=2)
image.plot(c2grid, c3grid, nb20, main='nb20'); abline(v=coefs[2], h=coefs[3], lwd=2)

# Ligne
tGrid <- seq(0, 1, length.out=gridNb)
moyenne <- rattrap <- nb20 <- rep(NA, gridNb)
for(i in 1:gridNb){
  c2 <- min(c2grid) + tGrid[i] * diff(range(c2grid))
  c3 <- min(c3grid) + tGrid[i] * diff(range(c3grid))
  tmp <- ceiling(notes %*% c(coefs[1], c2, c3))
  tmp[which(tmp > 20)] <- 20
  moyenne[i] <- mean(tmp, na.rm=TRUE)
  rattrap[i] <- sum(tmp<10, na.rm=TRUE)
  nb20[i] <- sum(tmp>=20, na.rm=TRUE)
}
par(mfrow=c(2, 1))
plot(tGrid, moyenne, type='l', lwd=2)
plot(tGrid, rattrap, type='l', lwd=2, ylim=range(c(rattrap, nb20), na.rm=TRUE))
lines(tGrid, nb20, type='l', lwd=2, col=2)

# Notes finales
# t <- 0.3
# coefs <- c(coefs[1], min(c2grid)+t*diff(range(c2grid)), min(c3grid)+t*diff(range(c3grid)))
coefs*c(8, 6, 13)
finale <- ceiling(notes %*% coefs)
finale[which(finale > 20)] <- 20
print(c(mean(finale, na.rm=TRUE), median(finale, na.rm=TRUE), sum(finale>=20, na.rm=TRUE), sum(finale<10, na.rm=TRUE)))
