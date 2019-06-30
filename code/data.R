rm(list=ls())
library(readr)
library(MASS)
library(kernlab)
library(mvtnorm)
library(Matrix)
library(optimx)
library(readxl)

yeast <- read.delim("data/yeast.txt")
gene <- read_excel("data/gene.xlsx")
names(gene)[1] <- "X"
mdt <- merge(yeast, gene, by = "X")
phase <- ifelse(mdt$Peak == 'G1', 'G1', 'NonG1')
mdt <- cbind(mdt, phase)
mdt <- mdt[,-(2:7)]
mdt <- mdt[, -(20:78)]
mdt <- na.omit(mdt)

sel <- sample(1:dim(mdt)[1], 300)

#mdt <- mdt[sel,]

n <- length(unique(mdt$X))
idnum <- rep(1:n, each = 18)
alpha <- mdt[,2:19]
alpha <- matrix(t(alpha), ncol = 1)
mdt$X <- as.character(mdt$X)
gene <- rep(mdt$X, each = 18)
phase <- rep(mdt$phase, each = 18)
time <- seq(0,1, length.out = 18)
time <- rep(time, times = dim(mdt)[1])

ydt <- data.frame(gene, time, phase, alpha, idnum)
names(ydt) <- c('gene', 'time', 'phase', 'alpha', 'idnum')


plot(ydt[ydt$phase == 'G1',]$time, ydt[ydt$phase == 'G1',]$alpha)
plot(ydt[ydt$phase == 'NonG1',]$time, ydt[ydt$phase == 'NonG1',]$alpha)


train <- split(ydt, ydt$phase, drop = T)
train$G1$gene <- as.character(train$G1$gene) 
train$NonG1$gene <- as.character(train$NonG1$gene)


test <- ydt
#table(train$G1$gene)
