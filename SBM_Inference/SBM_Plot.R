library(ggplot2)
library(R.matlab)

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
    geom_line(aes(x = CompNum, y=log.lik.true,color='blue'),show.legend=FALSE) +
    geom_line(aes(x = CompNum, y=log.lik.null.median,color='red'),show.legend=FALSE) +
    xlab('Component Number') +
    ylab('Profile Log Likelihood')
dev.off()

png('SBM_Likelihood_Diff.png',units='in',width=5,height=5,res=300)
ggplot(data=sbm) +
    geom_line(aes(x = CompNum, y=LogLikGap,color='yellow'),show.legend=FALSE) +
    xlab('Component Number') +
    ylab('Difference in Profile Log Likelihood')
dev.off()
