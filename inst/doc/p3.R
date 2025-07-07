## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----packages-----------------------------------------------------------------
## Load some required libraries
library(extraDistr)    # functions related with the inverse-Gamma distribution
library(LaplacesDemon) # functions related with the multivariate Student dist.

## -----------------------------------------------------------------------------
data("Auto", package = "ISLR")
plot(mpg ~ horsepower, data = Auto)

## -----------------------------------------------------------------------------
# Standardization of horsepower
Auto$horse.std <- (Auto$horsepower - mean(Auto$horsepower))/sd(Auto$horsepower)
# Quadratic fit
Fit1 <- lm(mpg ~ poly(horse.std, degree = 2, raw = TRUE), data = Auto)
summary(Fit1)
# Plot of the fitted curve
plot(mpg ~ horse.std, data = Auto)
where <- seq(min(Auto$horse.std), max(Auto$horse.std), length = 100)
lines(where, predict(Fit1, data.frame(horse.std = where)), col = 2, lwd = 2)

## -----------------------------------------------------------------------------
plot(seq(13, 25, by = 0.1), 
     dinvgamma(seq(13, 25, by = 0.1), (nrow(Auto)-3)/2,sum(residuals(Fit1)^2)/2), 
     type = "l", ylab = "Posterior density", xlab = "sigma2")

## -----------------------------------------------------------------------------
# posterior mean
sum(residuals(Fit1)^2)/2/((nrow(Auto)-3)/2-1)
# posterior mode
sum(residuals(Fit1)^2)/2/((nrow(Auto)-3)/2+1)
# Frequentist point estimate of sigma2
sum(Fit1$residuals^2)/(nrow(Auto)-3)

## -----------------------------------------------------------------------------
X <- cbind(rep(1, dim(Auto)[1]), Auto$horse.std, Auto$horse.std^2)
VarCov <- solve(t(X)%*%X)*sum(residuals(Fit1)^2)/(nrow(Auto)-3)
VarCov
# Correlation between coefficients
cov2cor(VarCov)

## -----------------------------------------------------------------------------
# Joint posterior distribution of the coefficients for the intercept and the 
# linear term
xlims <- c(Fit1$coefficients[1]-3*sqrt(VarCov[1,1]),
            Fit1$coefficients[1]+3*sqrt(VarCov[1,1]))
ylims <- c(Fit1$coefficients[2]-3*sqrt(VarCov[2,2]),
            Fit1$coefficients[2]+3*sqrt(VarCov[2,2]))
gridPoints <- expand.grid(seq(xlims[1],xlims[2],by=0.01),
                        seq(ylims[1],ylims[2],by=0.01))
resul <- matrix(dmvt(as.matrix(gridPoints), mu = Fit1$coefficients[1:2],
                     S = round(VarCov[1:2,1:2],5), df = nrow(Auto)-3),
                nrow = length(seq(xlims[1], xlims[2], by = 0.01)))
image(x = seq(xlims[1], xlims[2], by = 0.01), y = seq(ylims[1], ylims[2], by = 0.01), 
      z = resul, xlab = "Intercept", ylab = "beta, linear term", 
      main = "Bivariate posterior density")

## -----------------------------------------------------------------------------
# Plot the linear and quadratic component
xlims <- c(Fit1$coefficients[2]-3*sqrt(VarCov[2,2]),
            Fit1$coefficients[2]+3*sqrt(VarCov[2,2]))
ylims <- c(Fit1$coefficients[3]-3*sqrt(VarCov[3,3]),
            Fit1$coefficients[3]+3*sqrt(VarCov[3,3]))
gridPoints <- expand.grid(seq(xlims[1],xlims[2],by=0.01),
                        seq(ylims[1],ylims[2],by=0.01))
resul <- matrix(dmvt(as.matrix(gridPoints), mu = Fit1$coefficients[c(2,3)],
                     S = round(VarCov[c(2,3),c(2,3)], 5), df = nrow(Auto)-3), 
                nrow = length(seq(xlims[1], xlims[2], by = 0.01)))
image(x = seq(xlims[1], xlims[2], by = 0.01), y = seq(ylims[1], ylims[2], by = 0.01), 
      z = resul, xlab = "beta, linear term", 
      ylab = "beta, quadratic term", 
      main = "Bivariate posterior density")

## -----------------------------------------------------------------------------
Probs <- vector()
for(i in 1:3){
  Probs[i] <- pst(0, mu = Fit1$coefficients[i], sigma = sqrt(VarCov[i,i]),
                  nu = nrow(Auto)-3)
}
Probs

