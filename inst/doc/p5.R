## ----eval = TRUE--------------------------------------------------------------
GoTdata <- data.frame(Us = c(25, 29, 27, 27, 25, 27, 22, 26, 27, 29, 23,
                          28, 25, 24, 22, 25, 23, 29, 23, 28, 21, 29,
                          28, 23, 28))
y <- GoTdata$Us

## -----------------------------------------------------------------------------
n_simulations <- 10000
set.seed(1)
lambda_sim <- rlnorm(n_simulations, 3, 0.5)

## -----------------------------------------------------------------------------
# Log-Likelihood (for each value of lambda_sim)
loglik_pois <- sapply(lambda_sim, function(LAMBDA) {
  sum(dpois(GoTdata$Us, LAMBDA, log = TRUE))
})

# Log-weights: log-lik + log-prior - log-proposal_distribution
log_ww <- loglik_pois + dgamma(lambda_sim, 0.01, 0.01, log = TRUE) - dlnorm(lambda_sim, 3, 0.5, log = TRUE)

# Re-scale weights to sum up to one
log_ww <- log_ww - max(log_ww)
ww <- exp(log_ww)
ww <- ww / sum(ww)


## ----fig = TRUE---------------------------------------------------------------
hist(ww, xlab = "Importance weights")

## -----------------------------------------------------------------------------
# Posterior mean
(post_mean <- sum(lambda_sim * ww))

# Posterior variance
(post_var <- sum(lambda_sim^2 * ww)- post_mean^2)

## ----fig = TRUE---------------------------------------------------------------
plot(density(lambda_sim, weights = ww, bw = 0.5) , main = "Posterior density", xlim = c(10,40))

## -----------------------------------------------------------------------------
post_lambda_sim <- sample(lambda_sim, prob = ww, replace = TRUE)
hist(post_lambda_sim, freq = FALSE)

## -----------------------------------------------------------------------------
# Proposal distribution: sampling
rq <- function(lambda) {
  rlnorm(1, meanlog = log(lambda), sdlog = sqrt(1 / 100))
}

# Proposal distribution: log-density
logdq <- function(new.lambda, lambda) {
  dlnorm(new.lambda, meanlog = log(lambda), sdlog = sqrt(1 / 100), log = TRUE)
}

# Prior distribution: Ga(0.01, 0.01)
logprior <- function(lambda) {
  dgamma(lambda, 0.01, 0.01, log = TRUE)
}

# LogLikelihood
loglik <- function(y, lambda) {
   res <- sum(dpois(y, lambda, log = TRUE)) 
}

## -----------------------------------------------------------------------------
# Number of iterations
n.iter <- 40500

# Simulations of the parameter
lambda <- rep(NA, n.iter)

# Initial value
lambda[1] <- 30

for(i in 2:n.iter) {
  new.lambda <- rq(lambda[i - 1])
  
  # Log-Acceptance probability
  logacc.prob <- loglik(y, new.lambda) + logprior(new.lambda) +
    logdq(lambda[i - 1], new.lambda)
  logacc.prob <- logacc.prob - loglik(y, lambda[i - 1]) - logprior(lambda[i - 1]) - 
    logdq(new.lambda, lambda[i - 1])
  logacc.prob <- min(0, logacc.prob)#0 = log(1)
  
  if(log(runif(1)) < logacc.prob) {
    # Accept
    lambda[i] <- new.lambda
  } else {
    # Reject
    lambda[i] <- lambda[i - 1]
  }
}

## -----------------------------------------------------------------------------
# Remove burn-in
lambda <- lambda[-c(1:500)]

# Thinning
lambda <- lambda[seq(1, length(lambda), by = 10)]

# Summary statistics
summary(lambda)

oldpar <- par(mfrow = c(1, 2))
plot(lambda, type = "l", main = "MCMC samples", ylab = expression(lambda))
plot(density(lambda), main = "Posterior density", xlab = expression(lambda))
par(oldpar)

## -----------------------------------------------------------------------------
ESS <- function(ww){
  (sum(ww)^2)/sum(ww^2)
}
ESS(ww)
n_simulations

## -----------------------------------------------------------------------------
n_simulations <- 10000
set.seed(12)
lambda_sim <- rgamma(n_simulations,5,0.1)
loglik_pois <- sapply(lambda_sim, function(LAMBDA) {
  sum(dpois(GoTdata$Us, LAMBDA, log = TRUE))
})
log_ww <- loglik_pois + dgamma(lambda_sim, 0.01, 0.01, log = TRUE) - dgamma(lambda_sim, 5, 0.1, log=TRUE)
log_ww <- log_ww - max(log_ww)
ww <- exp(log_ww)
ww <- ww / sum(ww)

## ----fig = TRUE---------------------------------------------------------------
hist(ww, xlab = "Importance weights")

## -----------------------------------------------------------------------------
post_mean <- sum(lambda_sim * ww)
post_mean
post_var <- sum(lambda_sim^2 * ww)- post_mean^2
post_var

## ----fig = TRUE---------------------------------------------------------------
plot(density(lambda_sim, weights = ww, bw = 0.5), main = "Posterior density", xlim = c(10,40))

## -----------------------------------------------------------------------------
ESS(ww)
n_simulations

## -----------------------------------------------------------------------------
# Prior distribution: Ga(1.0, 1.0)
logprior <- function(lambda) {
  dgamma(lambda, 1.0, 1.0, log = TRUE)
}
# Number of iterations
n.iter <- 40500

# Simulations of the parameter
lambda <- rep(NA, n.iter)

# Initial value
lambda[1] <- 30

for(i in 2:n.iter) {
  new.lambda <- rq(lambda[i - 1])
  
  # Log-Acceptance probability
  logacc.prob <- loglik(y, new.lambda) + logprior(new.lambda) +
    logdq(lambda[i - 1], new.lambda)
  logacc.prob <- logacc.prob - loglik(y, lambda[i - 1]) - logprior(lambda[i - 1]) - 
    logdq(new.lambda, lambda[i - 1])
  logacc.prob <- min(0, logacc.prob)#0 = log(1)
  
  if(log(runif(1)) < logacc.prob) {
    # Accept
    lambda[i] <- new.lambda
  } else {
    # Reject
    lambda[i] <- lambda[i - 1]
  }
}
# Remove burn-in
lambda <- lambda[-c(1:500)]

# Thinning
lambda <- lambda[seq(1, length(lambda), by = 10)]

# Summary statistics
summary(lambda)

oldpar <- par(mfrow = c(1, 2))
plot(lambda, type = "l", main = "MCMC samples", ylab = expression(lambda))
plot(density(lambda), main = "Posterior density", xlab = expression(lambda))
par(oldpar)

## -----------------------------------------------------------------------------
# Read in data
BOD <- c(200,180,135,120,110,120,95,168,180,195,158,145,140,145,165,187,
         190,157,90,235,200,55,87,97,95)
mayfly.length <- c(20,21,22,23,21,20,19,16,15,14,21,21,21,20,19,18,17,19,21,13,
            16,25,24,23,22)
# Create data frame for the analysis
Data <- data.frame(BOD=BOD,mayfly.length=mayfly.length)

## ----echo = FALSE-------------------------------------------------------------
suppressPackageStartupMessages(
  library(MCMCpack)
)

## ----eval = FALSE-------------------------------------------------------------
# library(MCMCpack)

## ----eval = FALSE-------------------------------------------------------------
# ?MCMCregress

## ----fig = TRUE---------------------------------------------------------------
# Scatterplot
plot(BOD,mayfly.length)
# Correlation with hypothesis test
cor.test(BOD,mayfly.length)

## -----------------------------------------------------------------------------
# Linear Regression using lm()
linreg <- lm(mayfly.length ~ BOD, data = Data)
summary(linreg)

## ----fig = TRUE---------------------------------------------------------------
# Bayesian Linear Regression using a Gibbs Sampler

# Set the size of the burn-in, the number of iterations of the Gibbs Sampler
# and the level of thinning
burnin <- 5000
mcmc <- 10000
thin <- 10

# Obtain the samples
results1  <- MCMCregress(mayfly.length~BOD,
                         b0=c(0.0,0.0), B0 = c(0.0001,0.0001),
                         c0 = 2, d0 = 2, # Because the prior is Ga(c0/2,d0/2),
                         beta.start = c(1,1),
                         burnin=burnin, mcmc=mcmc, thin=thin,
                         data=Data, verbose=1000)
summary(results1)


## ----fig = TRUE---------------------------------------------------------------
oldpar <- par(mfrow = c(2, 2))
traceplot(results1)
par(oldpar)

## ----fig = TRUE---------------------------------------------------------------
oldpar <- par(mfrow = c(2, 2))
densplot(results1)
par(oldpar)

## -----------------------------------------------------------------------------
crosscorr(results1)

## ----fig = TRUE---------------------------------------------------------------
# Mean-centre the x covariate
DataC <- Data
meanBOD <- mean(DataC$BOD)
DataC$BOD <- DataC$BOD - meanBOD

# Set the size of the burn-in, the number of iterations of the Gibbs Sampler
# and the level of thinning
burnin <- 50000
mcmc <- 10000
thin <- 10

# Obtain the samples
results2  <- MCMCregress(mayfly.length~BOD,
                         b0=c(0.0,0.0), B0 = c(0.0001,0.0001),
                         c0 = 2, d0 = 2, # Because the prior is Ga(c0/2,d0/2),
                         beta.start = c(1,1),
                         burnin=burnin, mcmc=mcmc, thin=thin,
                         data=DataC, verbose=1000)
summary(results2)

# Correct the effect of the mean-centering on the intercept, using the
# full set of simulated outputs
results2.simulations <- as.data.frame(results2)
results2.beta.0 <- results2.simulations[,"(Intercept)"] - meanBOD * results2.simulations$BOD
summary(results2.beta.0)
var(results2.beta.0)
sd(results2.beta.0)


## ----fig = TRUE---------------------------------------------------------------
oldpar <- par(mfrow = c(2, 2))
traceplot(results2)
par(oldpar)

## ----fig = TRUE---------------------------------------------------------------
oldpar <- par(mfrow = c(2, 2))
densplot(results2)
# Need to use the Base R kernel density function to look at the corrected 
# Intercept
plot(density(results2.beta.0, bw = 0.3404), xlim=c(22,34),
     main = "Density, corrected Intercept")
par(oldpar)

## ----fig = TRUE---------------------------------------------------------------
crosscorr(results2)

