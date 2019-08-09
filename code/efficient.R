# implementing efficient grid computation

load('rbffit.Rdata')
mdt <- subset(mdt, mdt$phase == 'G1')
n <- dim(mdt)[1]
m <- 18
x <- seq(0,1, length.out = m)
#mu <- gpsmooth(x, fet$G1)

Hyper <- function(dtf) {
  
  marlik <- function(theta) {
    
    n <- dim(dtf)[1]
    m <- dim(dtf)[2]
    x <- seq(0,1, length.out = m)
    theta <- theta^2
    
    k1ker <- rbfdot(sigma = 1/theta[1])
    k1 <- theta[2] * kernelMatrix(k1ker, x = x) 
    k1 <- k1 + .0000001 * diag(m)
    k2ker <- rbfdot(sigma = 1/theta[3])
    k2 <- theta[4] * kernelMatrix(k2ker, x = x) 
    
    k1inv <- chol2inv(chol(k1))
    G <- k2 + theta[5] * diag(m)
    D <- chol2inv(chol(G))
    C <- chol2inv(chol(k1inv + n * D))
    ySum <- as.matrix(apply(dtf, 2, sum), ncol = 1)
    P <- D %*% ySum
    YD <- sum(apply(dtf, 1, function(x) { x <- matrix(x, ncol = 1); t(x) %*% D %*% x}))
    logl <- YD - (t(P) %*% C %*% P) + determinant((k1inv + n * D), logarithm = T)$modulus + determinant(k1, logarithm = T)$modulus + 
      n * determinant(G, logarithm = T)$modulus
    
    return(logl)
  }
  
  hyp <- optim(par=rep(1, 5), fn = marlik, method = 'Nelder-Mead',
               control=list(maxit = 10000))
  print(hyp)
  return(hyp$par^2)
  
}

#hyper <- fet$G1$hyper

hyper <- Hyper(mdt[,2:19])

k1ker <- rbfdot(sigma = 1/hyper[1])
k1 <- hyper[2] * kernelMatrix(k1ker, x = x) 
k1 <- k1 + .0000001 * diag(m)

k2ker <- rbfdot(sigma = 1/hyper[3])
k2 <- hyper[4] * kernelMatrix(k2ker, x = x) 


k1inv <- chol2inv(chol(k1))
D <- chol2inv(chol(k2 + hyper[5] * diag(18)))
C <- chol2inv(chol(k1inv + n * D))

ySum <- as.matrix(apply(mdt[,2:19], 2, sum), ncol = 1)

mustar <- k1 %*% ( diag(m) - n * D %*% C) %*% (D %*% ySum)


plot(fet$G1$trainx, fet$G1$trainy, pch = 16, col = 1, 
     cex=.5, xlab='t', ylab='y')
lines(x, mustar)
#lines(x, mu, col = 2)
