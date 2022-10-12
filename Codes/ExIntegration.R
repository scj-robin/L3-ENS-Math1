# Exemple pour intégration multiple

rm(list=ls()); library(lattice)

###############################################################################
# Exemple : f(x, y) = (x^2 - y^2) / (x^2 + y^2)^2
# Change de signe => ordre d'intgration compte
grid <- seq(0, 1, length.out=5e2)
f <- expand.grid(x=grid, y=grid)
f$z <- (f$x^2 - f$y^2) / (f$x^2 + f$y^2)^2
wireframe(z ~ x * y, data=f, shade=TRUE, light.source = c(3,3,3), zlim=c(-5, 5))
