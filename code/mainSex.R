
source("code/functions.R")
source("code/multistart.R")

source("code/RBF.R")
source("code/laplace.R")
source("code/matern52.R")
source("code/matern32.R")

fet <- lapply(train, feature) #estimate hyperparameter

save(fet, file = 'matern52fit.Rdata')

# Plot fitted
ageseq <- seq(0,1, length = 100)

plot(fet$G1$trainx, fet$G1$trainy, pch = 16, col = 1, 
     cex=.5, xlab='t', ylab='y')
points(fet$G1$trainx, fet$G1$trainy, pch = 16, col = 2, cex=.5)
lines(ageseq, gpsmooth(ageseq, fet$G1), type='l', lwd = 2, col = 1)
lines(ageseq, gpsmooth(ageseq, fet$NonG1), type='l', lwd = 2, col = 2)
legend('bottomright', c('G1', 'Non-G1'), col = 1:2, lty=1, bty = 'n')


#------------------------------
#blogprob <- log.prob

log.prob <- c()

for(i in unique(test$idnum)) {
#for(i in 1:10) {
  
  fit1 <- fit.gp(fet$G1, testx = test[test$idnum == i, 2], testy = test[test$idnum == i, 4])
  fit2 <- fit.gp(fet$NonG1, testx = test[test$idnum == i, 2], testy = test[test$idnum == i, 4])
  log.prob <- rbind(log.prob, c(fit1, fit2))
  
}

y <- c()
for (i in unique(test$idnum)) {
#for (i in 1:10) {
  y <- rbind(y, as.character((test[test$idnum == i, 3])[1]))
}

log.prob <- log.prob + cbind(rep(log(0.3627451),612), rep(log(0.6372549),612))

y.pred <- log.prob[,1] > log.prob[,2]
pred <- ifelse(y.pred == 1, 'G1', 'NonG1')
table(pred, y)
err <- mean(pred != y)
err


