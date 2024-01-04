# Corrigé de l'exercice 1 de l'examen 2021-22

rm(list=ls())

# Question 1
B <- function(a, b){
  matrix(c(-1, 3, 1, 0, 2, 1, a, b, 2), byrow=TRUE, 3, 3)
}
a <- 2; b <- 2; B(a, b); eigen(B(a, b))
a <- rpois(1, 5); b <- rpois(1, 1); B(a, b); eigen(B(a, b))$values
print(c(-1, 2+sqrt(a+b), 2-sqrt(a+b)))

# Question 3
# a + b = 0
a <- rpois(1, 5); b <- -a; B(a, b); eigen(B(a, b))
a <- rpois(1, 5); b <- 9-a; B(a, b); eigen(B(a, b))
