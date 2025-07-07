## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
library(ISLR)
library(LaplacesDemon)

Auto$horse.std <- (Auto$horsepower-mean(Auto$horsepower))/sd(Auto$horsepower)
# Quadratic fit
Fit1 <- lm( mpg ~ poly(horse.std, degree = 2, raw = TRUE), data = Auto)
summary(Fit1)
plot(mpg ~ horse.std, data = Auto)
where <- seq(min(Auto$horse.std), max(Auto$horse.std), length = 100)
lines(where, predict(Fit1, data.frame(horse.std=where)), col = 2, lwd = 2)

## -----------------------------------------------------------------------------
X <- cbind(rep(1, dim(Auto)[1]), Auto$horse.std, Auto$horse.std^2)
y <- matrix(Auto$mpg, ncol = 1)

betahat <- matrix(Fit1$coefficients, ncol = 1)
RSS <- sum(residuals(Fit1)^2)

# P(sigma^2|y)
Psigma2<-function(sigma2){
  dinvgamma(sigma2, 10+nrow(Auto)/2, 
            300+(RSS+t(betahat)%*%solve(5*diag(3)+solve(t(X)%*%X))%*%betahat)/2)
}

# P(beta|sigma^2,y)
PBetaGivenSigma2<-function(beta,sigma2){
  dmvn(beta, as.vector(solve(diag(3)/5+t(X)%*%X)%*%(t(X)%*%y)),
       sigma2*solve(diag(3)/5+t(X)%*%X))
}

# P(beta,sigma^2|y)  
JointPosterior<-function(beta,sigma2){
  P1 <- Psigma2(sigma2)
  P2 <- PBetaGivenSigma2(beta, sigma2)
  P1*P2
}

## -----------------------------------------------------------------------------
plot(seq(10, 30, by = 0.1), Psigma2(seq(10, 30, by=0.1)), type = "l", 
     xlab = "sigma2", ylab = "density", main = "posterior marginal density of sigma2")

## -----------------------------------------------------------------------------
# Sample from pi(sigma^2|y)
sigma2.sample <- rinvgamma(10000, 10+nrow(Auto)/2, 
                           300+(RSS+t(betahat)%*%(diag(3)+solve(t(X)%*%X))%*%betahat)/2)

# Sample from pi(beta|y, sigma^2)
beta.sample<-t(sapply(sigma2.sample,function(sigma2){
  rmvn(1, as.vector(solve(diag(3)+t(X)%*%X)%*%(t(X)%*%y)),
       sigma2*round(solve(diag(3)+t(X)%*%X),5))
}))
# Sample from pi(beta, sigma^2| y)
posterior.sample <- cbind(beta.sample, sigma2.sample)

## -----------------------------------------------------------------------------
# Posterior mean of each parameter:
apply(posterior.sample, 2, mean)
# Posterior standard deviation of each parameter:
apply(posterior.sample, 2, sd)
# Posterior probability of being higher than 0
apply(posterior.sample>0, 2, mean)

## -----------------------------------------------------------------------------
plot(mpg ~ horse.std, data = Auto)

# Posterior mean of the curve
where <- seq(min(Auto$horse.std), max(Auto$horse.std), length = 100)
posterior.mean.curve <- as.vector(cbind(rep(1,length(where)), where, where^2)%*%
                                    matrix(apply(posterior.sample[,1:3], 2, mean),ncol=1))
lines(where, posterior.mean.curve, col = 2, lwd = 2)

# 95% credible band for the curve
posterior.curves <- t(cbind(rep(1,length(where)), where, where^2) 
                     %*%t(posterior.sample[,1:3]))
Band <- apply(posterior.curves, 2, function(x){quantile(x, c(0.025,0.975))})
lines(where, Band[1,], col = 3, lwd = 1.5, lty = 2)
lines(where, Band[2,], col = 3, lwd = 1.5, lty=2)

## -----------------------------------------------------------------------------
SampleY <- function(at){
  mu <- beta.sample%*%matrix(c(1, at, at^2), ncol = 1)
  rnorm(nrow(mu), mu, sqrt(sigma2.sample))
}
samples <- sapply(where, SampleY)

## -----------------------------------------------------------------------------
quantiles <- apply(samples, 2, quantile, c(0.025,0.975))
plot(mpg ~ horse.std, data = Auto)
lines(where, quantiles[1,], col = 4, lty = 3)
lines(where, quantiles[2,], col = 4, lty = 3)

