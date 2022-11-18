alpha <- a <- 3
A <- function(alpha){matrix(c(1, 4, 2, 0, alpha, 0, 1, 1, 0), 3, 3, byrow=TRUE)}

eigen(A(2))

aList <- seq(-5, 5, by=.01)
scalProd <- sapply(aList, function(a){
  C <- crossprod(eigen(A(a))$vectors)
  max(abs(C[upper.tri(C)]))
  })
plot(aList, scalProd, type='l')

################################################################################
g2 <- 5; s2 <- 2; n <- 4
S <- g2*matrix(1, n, n) + s2*diag(n)
eigen(S)
(n*g2 + s2)


################################################################################
seed <- 1; set.seed(seed)
n <- 5; 
A <- matrix(rbinom(n^2, 1, 2/n)*runif(n^2), n, n)
A <- A + t(A); A <- A / sqrt(sum(A^2))
A
eigen(A)
R <- t(eigen(A)$vectors)
q <- rowSums(R)

# stationnaire
p <- as.vector(solve(A)%*%rep(1, n)); p <- p / sum(p)
# p <- rep(1/n, n)
# p <- eigen(A)$vectors[, 1]; p <- p / sum(p)
for(i in 1:10){p <- as.vector(p * (A%*%p)); p <- p / sum(p)}
p

cor(p, t(R))
