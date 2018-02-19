#Say you are given a correlation matrix of 5 different scales along their means and sd's

#What might the variance of a composite variable of 2 or more scales be equal to?

#How might two composites be correlated with each other?

#First creating correlation matrix
sig <- diag(rep(1,5))

#setting upper triangle of correlation matrix
sig[upper.tri(sig)] <- c(.23,.65,.33,.15,.42,.28,-.08,-.12,-.05,-.1)

#function to reflect Upper Tri across diagonal
f = function(m) {
  m[lower.tri(m)] <- t(m)[lower.tri(m)]
  m
}

#final correlation matrix
sig <- f(sig)

##Creating covariance matrix

#Vector of standard deviations
stdevs <- c(11,8.5,12.25,15.3,1.93)

#Cov = correlation(x,y) * sd(x) * sd(y)
#This gives a matrix of sd(x) * sd(y)
b <- stdevs %*% t(stdevs)  

#Final covariance matrix
sig_covar <- b * sig  

#Simulating data from mean vector and covariance matrix
jerry = mvrnorm(n = 1000000,empirical = TRUE, mu = c(32,27,42.5,34.15,7.4),Sigma = sig_covar)

#Variance ot a composite
var(jerry[,2]+jerry[,4])

#correlation between two composites
comp_1 = jerry[,2] + jerry[,4]
comp_2 = jerry[,1] + jerry[,3]

cor(comp_1,comp_2,method = "pearson")

