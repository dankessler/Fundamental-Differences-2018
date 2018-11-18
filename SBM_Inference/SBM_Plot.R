rm(list=ls())                           #just be sure
library(ggplot2)
library(R.matlab)

a <- 0.005
b <- 0.995

## real data
results = R.matlab::readMat('SBM_Inference.mat')

sbm <- data.frame(
    pval=results$sbm['pval',1,1],
    log.lik.true=results$sbm['log.lik.true',1,1],
    log.lik.null.median=results$sbm['log.lik.null.median',1,1])

full.null <- results$sbm['log.lik.null.full',1,1]$log.lik.null.full

get.quantiles <- function(dat,a,b){
    ## obtain the a'th and b'th
    ## quantiles from the empirical data
    cdf <- ecdf(dat)
    return(c(quantile(cdf,a),quantile(cdf,b)))
}

tiles <- t(apply(full.null,1,get.quantiles,a=a,b=b))

sbm$CompNum = 1:nrow(sbm)
sbm$LogLikGap = sbm$log.lik.true - sbm$log.lik.null.median
sbm$LeftQuantile <- tiles[,1]
sbm$RightQuantile <- tiles[,2]


png('SBM_Likelihood.png',units='in',width=5,height=5,res=300)

ggplot(data=sbm) +
    geom_line(aes(x = CompNum, y=log.lik.true,color='b',linetype='s'),show.legend=FALSE) +
    geom_line(aes(x = CompNum, y=log.lik.null.median,color='r',linetype='s'),show.legend=FALSE) +
    geom_line(aes(x = CompNum, y=LeftQuantile,color='k',linetype='d'),show.legend=FALSE) +
    geom_line(aes(x = CompNum, y=RightQuantile,color='k',linetype='d'),show.legend=FALSE) +
    scale_color_manual(values=c(b='blue',r='red',k='black')) +
    scale_linetype_manual(values=c(s='solid',d='dashed')) +
    xlab('Component Number') +
    ylab('Profile Log Likelihood')

dev.off()


## do an analysis of the "fake" data
rm(list=ls())                           # just be sure
library(ggplot2)
library(R.matlab)

a <- 0.005
b <- 0.995

results = R.matlab::readMat('SBM_Fake_Inference.mat')

fake <- data.frame(
    pval=results$fake['pval',1,1],
    log.lik.true=results$fake['log.lik.true',1,1],
    log.lik.null.median=results$fake['log.lik.null.median',1,1])

full.null <- results$fake['log.lik.null.full',1,1]$log.lik.null.full

get.quantiles <- function(dat,a,b){
    ## obtain the a'th and b'th
    ## quantiles from the empirical data
    cdf <- ecdf(dat)
    return(c(quantile(cdf,a),quantile(cdf,b)))
}

tiles <- t(apply(full.null,1,get.quantiles,a=a,b=b))

fake$CompNum = 1:nrow(fake)
fake$LogLikGap = fake$log.lik.true - fake$log.lik.null.median
fake$LeftQuantile <- tiles[,1]
fake$RightQuantile <- tiles[,2]


png('FAKE_Likelihood.png',units='in',width=5,height=5,res=300)

ggplot(data=fake) +
    geom_line(aes(x = CompNum, y=log.lik.true,color='b',linetype='s'),show.legend=FALSE) +
    geom_line(aes(x = CompNum, y=log.lik.null.median,color='r',linetype='s'),show.legend=FALSE) +
    geom_line(aes(x = CompNum, y=LeftQuantile,color='k',linetype='d'),show.legend=FALSE) +
    geom_line(aes(x = CompNum, y=RightQuantile,color='k',linetype='d'),show.legend=FALSE) +
    scale_color_manual(values=c(b='blue',r='red',k='black')) +
    scale_linetype_manual(values=c(s='solid',d='dashed')) +
    xlab('Component Number') +
    ylab('Profile Log Likelihood')

dev.off()
