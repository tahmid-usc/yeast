
source("code/functions.R")
source("code/multistart.R")

source("code/RBF.R")
source("code/laplace.R")
source("code/matern52.R")
source("code/matern32.R")

fet <- lapply(train, feature) #estimate hyperparameter

save(fet, file = 'rbffit.Rdata')

# Plot fitted
ageseq <- seq(0,1, length = 100)

plot(fet$G1$trainx, fet$G1$trainy, pch = 16, col = 1, 
     cex=.5, xlab='t', ylab='y')
points(fet$G1$trainx, fet$G1$trainy, pch = 16, col = 2, cex=.5)
lines(ageseq, gpsmooth(ageseq, fet$G1), type='l', lwd = 2, col = 1)
lines(ageseq, gpsmooth(ageseq, fet$NonG1), type='l', lwd = 2, col = 2)
legend('bottomright', c('G1', 'Non-G1'), col = 1:2, lty=1, bty = 'n')


#------------------------------


log.prob <- c()

for(i in unique(test$idnum)) {
  
  fit1 <- fit.gp(fet$fem, testx = test[test$idnum == i, 3], testy = test[test$idnum == i, 5])
  fit2 <- fit.gp(fet$mal, testx = test[test$idnum == i, 3], testy = test[test$idnum == i, 5])
  log.prob <- rbind(log.prob, c(fit1, fit2))
  
}

y <- c()
for (i in unique(test$idnum)) {
  y <- rbind(y, as.character((test[test$idnum == i, 4])[1]))
}

y.pred <- log.prob[,1] > log.prob[,2]
pred <- ifelse(y.pred == 0, 'mal', 'fem')
table(pred, y)
err <- mean(pred != y)
err


#Training error
#Full training
#laplace : 0.2678571, RBF : 0.2857143, matern52: 0.2785714. matern32: 0.2857143
