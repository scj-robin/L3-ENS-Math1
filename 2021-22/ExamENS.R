library(Matrix)

a <- -2; b <- 2
c <- 1/sqrt(2)
v <- c(7/2, -1, 3); v <- v / sqrt(sum(v^2))
P <- matrix(c(-c, -c, 0, c, c, 0, v[1], v[2], v[3]), 3, 3)


a <- -2; b <- 2
B = matrix(c(-1, 3, 1, 0, 2, 1, a, b, 2), byrow=TRUE, 3, 3)
curve(-(1+x)*(x^2-4*x+4-a-b), from=-2, to=+5); abline(h=0, v=c(-1, 2))
B
rankMatrix(B)
eigen(B)
