# Exercice pour valeurs / vecteurs propres

# Polynôme caractéristique : A1 et A2
A <- matrix(c(2, -1, 3, 2, -1, 6, 1, 0 , 2), 3, 3, byrow=TRUE)
A <- matrix(c(1, 1, 0, -5, -2, 5, -1, 0, 2), 3, 3, byrow=TRUE)
A; eig <- eigen(A)
eig$values; 
eig$vectors
