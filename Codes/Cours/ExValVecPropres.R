# Exemple pour valeurs / vecteurs propres

# Exemple : action de A selon les directions propres
A <- matrix(c(1, 3, 1, -1), 2, 2)
A; eig <- eigen(A)
eig$values; eig$vectors[, 1] * sqrt(2); eig$vectors[, 2] * sqrt(10)

# Exemple : matrice circulante (non diagonalisable)
A <- matrix(c(0, 0, 1, 1, 0, 0, 0, 1, 0), 3, 3)
A; eig <- eigen(A)
eig$values; 2*eig$vectors[, 1]; eig$vectors[, 2]; -eig$vectors[, 3]*sqrt(3)

# Exemple : matrice de covariance
# sigma <- sqrt(c(1, 2, 3))
# R <- matrix(0, 3, 3); R[upper.tri(R)] <- c(.2, .5, .8); R <- R + t(R); diag(R) <- 1
# Sigma <- diag(sigma) %*% R %*% diag(sigma)
# Sigma <- matrix(c(1, .3, .9, .3, 2, 2, .9, 2, 3), 3, 3)
# sigma <- sqrt(c(1, 4))
# R <- matrix(0, 2, 2); R[upper.tri(R)] <- sqrt(7)/4; R <- R + t(R); diag(R) <- 1
# Sigma <- diag(sigma) %*% R %*% diag(sigma)
Sigma <- matrix(c(1, sqrt(7)/2, sqrt(7)/2, 4), 2, 2)
Sigma
eigen(Sigma)

