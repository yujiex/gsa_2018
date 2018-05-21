## some theory
## https://stats.idre.ucla.edu/r/dae/power-analysis-for-paired-sample-t-test/
## One is to calculate the necessary sample size for a specified power
## The other aspect is to calculate the power when given a specific sample size

## adapted from: https://www.statmethods.net/stats/power.html

library(pwr)

# power values
p <- seq(.4,.9,.1)
np <- length(p)

## guess these
null_mean = 0
alternative_mean = 20

## use sample to estimate
stdev = 10
pwr::pwr.t.test(d=(null_mean - alternative_mean)/stdev,power=0.8,sig.level=0.05,type="paired",alternative="two.sided")

## # obtain sample sizes
## samsize <- array(numeric(1*np), dim=c(1,np))

## effect_size = 0.5

## for (i in 1:np){
##     ## result <- pwr.r.test(n = NULL, r = r[j],
##     result <- pwr:: pwr.t.test(n = NULL, d = effect_size,
##                          sig.level = .05, power = p[i],
##                          type = "two.sample")
##     samsize[i] <- ceiling(result$n)
## }

## # set up graph
## yrange <- round(range(samsize))
## colors <- rainbow(length(p))
## plot(xrange, yrange, type="n",
##   xlab="Correlation Coefficient (r)",
##   ylab="Sample Size (n)" )

## # add power curves
## for (i in 1:np){
##   lines(r, samsize[,i], type="l", lwd=2, col=colors[i])
## }

## # add annotation (grid lines, title, legend)
## abline(v=0, h=seq(0,yrange[2],50), lty=2, col="grey89")
## abline(h=0, v=seq(xrange[1],xrange[2],.02), lty=2,
##    col="grey89")
## title("Sample Size Estimation for Correlation Studies\n
##   Sig=0.05 (Two-tailed)")
## legend("topright", title="Power", as.character(p),
##    fill=colors)
