# Marginal MC not an MC

rm(list=ls()); par(pch=20)
source('/home/robin/PgmR/Markov/FunctionsMarkov.R')

# Parms
states <- c('00', '01', '10', '11')
k <- length(states)
statesX <- substr(states, 1, 1)
statesY <- substr(states, 2, 2)
a <- 1/50; b <- 2/5
a <- rbeta(1, 1, 10); b <- rbeta(1, 1, 1)
trans <- rbind(c((1-a)*(1-b), (1-a)*b, a*(1-b), a*b), 
               c((1-a)*b, (1-a)*(1-b), a*b, a*(1-b)), 
               c(a, 0, (1-a), 0), 
               c(0, a, 0, (1-a)))
rownames(trans) <- colnames(trans) <- states
trans

# Sattionary distribution
statDist <- rep(1/k, k)
names(statDist) <- states
statDist%*%trans

# Path
xy <- F_SimMC(nu=c(1, rep(0, k-1)), Pi=trans, n=2e2)
x <- as.numeric(statesX[xy])
y <- as.numeric(statesY[xy])
plot(y, ylim=c(0, k-1), type='b', ylab='', xlab='')
points(2+x, col=2, type='b')

# Check Yt not a MC
y <- yy <- yyy <- '1'
# P{Y(t+2)=1 | Y(t+1)=1}
p1 <- sum((statesY==yy)*statDist)
pp1 <- 1/2
p11 <- sum((((statesY==yy)*statDist)%*%trans)[which(statesY==yyy)])
pp11 <- (2-b)/4
q1 <- p11/p1
# P{Y(t+2)=1 | Y(t+1)=1, Y(t) = 1}
p111 <- sum((((((statesY==y)*statDist)%*%trans)*(statesY==yy))%*%trans)[which(statesY==yyy)])
pp111 <- (2 - 2*b + b^2 - a*b^2)/4
q2 <- p111/p11
cat(a, b, '\n')
cat(p1, pp1, '/', p11, pp11, '/', p111, pp111, '\n')
cat(q1, q2, '\n')
