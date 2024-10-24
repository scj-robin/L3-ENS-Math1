# Exemple pour matrices non diagonalisable

# Matice de rotation
a <- pi/3
A <- matrix(c(cos(a), -sin(a), sin(a), cos(a)), 2, 2, byrow=TRUE)

eigen(A)
# eigen(A)$vector %*% diag(eigen(A)$values) %*% t(eigen(A)$vector)
