library(fdapace)
library(fda.usc)

fdt <- fdata(mdt[,2:19])
cdt <- data.frame(mdt$phase)
dat <- list('df' = cdt, 'x' = fdt)
fglm <- classif.glm(mdt.phase~x, data = dat, family = 'binomial')
summary(fglm)
