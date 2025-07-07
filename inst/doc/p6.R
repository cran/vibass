## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
fakenews <- bayesrules::fake_news[, c("type", "title_has_excl", "title_words", "negative")]

## -----------------------------------------------------------------------------
library(R2BayesX)

## ----eval=FALSE---------------------------------------------------------------
# model1 <- bayesx(
#   formula = y ~ x1 + x2 + x3,
#   data = data.set,
#   family = "binomial"
# )

## -----------------------------------------------------------------------------
fakenews$titlehasexcl <- as.factor(fakenews$title_has_excl)

## -----------------------------------------------------------------------------
fakenews$typeFAKE <- fakenews$type == "fake"

## ----fig = TRUE---------------------------------------------------------------
# Is there a link between the fakeness and whether the title has an exclamation mark?
table(fakenews$title_has_excl, fakenews$typeFAKE)
# For the quantitative variables, look at boxplots on fake vs real
boxplot(fakenews$title_words ~ fakenews$typeFAKE)
boxplot(fakenews$negative ~ fakenews$typeFAKE)


## ----fig = TRUE---------------------------------------------------------------
# Produce the BayesX output
bayesx.output <- bayesx(formula = typeFAKE ~ titlehasexcl + title_words + negative,
                        data = fakenews,
                        family = "binomial",
                        method = "MCMC",
                        iter = 15000,
                        burnin = 5000)
summary(bayesx.output)
confint(bayesx.output)

## ----fig = TRUE, fig.width = 5, fig.height = 10-------------------------------
# Traces can be obtained separately
plot(bayesx.output,which = "coef-samples")

## ----fig = TRUE---------------------------------------------------------------
# And the density plots one-by-one
oldpar <- par(mfrow = c(2, 2))
plot(density(samples(bayesx.output)[,"titlehasexclTRUE"]),main="Title Has Excl")
plot(density(samples(bayesx.output)[,"title_words"]),main="Title Words")
plot(density(samples(bayesx.output)[,"negative"]),main="Negative")
par(oldpar)

## ----fig = TRUE---------------------------------------------------------------
# Fit model - note similarity with bayesx syntax
glm.output <- glm(formula = typeFAKE ~ titlehasexcl + title_words + negative,
                  data = fakenews,
                  family = "binomial")
# Summarise output
summary(glm.output)
# Perform ANOVA on each variable in turn
drop1(glm.output,test="Chisq")

## -----------------------------------------------------------------------------
esdcomp <- faraway::esdcomp

## ----echo=FALSE---------------------------------------------------------------
library(R2BayesX)

## ----eval=FALSE---------------------------------------------------------------
# model1 <- bayesx(formula = y ~ x1 + x2 + x3 + offset(w),
#                  data = data.set,
#                  family="poisson")

## -----------------------------------------------------------------------------
esdcomp$logvisits <- log(esdcomp$visits)

## ----fig = TRUE---------------------------------------------------------------
# Compute the ratio
esdcomp$ratio <- esdcomp$complaints / esdcomp$visits
# Plot the link with revenue
plot(esdcomp$revenue,esdcomp$ratio)
# Use boxplots against residency and gender
boxplot(esdcomp$ratio ~ esdcomp$residency)
boxplot(esdcomp$ratio ~ esdcomp$gender)

## ----fig = TRUE---------------------------------------------------------------
# Fit model - note similarity with glm syntax
esdcomp$logvisits <- log(esdcomp$visits)
bayesx.output <- bayesx(formula = complaints ~ residency + gender + revenue,
                        offset = logvisits,
                        data = esdcomp,
                        family = "poisson")
# Summarise output
summary(bayesx.output)

## ----fig = TRUE, fig.width = 5, fig.height = 10-------------------------------
# An overall plot of sample traces and density estimates
#  plot(samples(bayesx.output))
# Traces can be obtained separately
plot(bayesx.output,which = "coef-samples")

## ----fig = TRUE---------------------------------------------------------------
# And the density plots one-by-one
oldpar <- par(mfrow = c(2, 2))
plot(density(samples(bayesx.output)[, "residencyY"]), main = "Residency")
plot(density(samples(bayesx.output)[, "genderM"]), main = "Gender")
plot(density(samples(bayesx.output)[, "revenue"]), main = "Revenue")
par(oldpar)

## ----fig = TRUE---------------------------------------------------------------
# Fit model - note similarity with bayesx syntax
esdcomp$log.visits <- log(esdcomp$visits)
glm.output <- glm(formula = complaints ~ residency + gender + revenue,
                  offset = logvisits,
                  data = esdcomp,
                  family = "poisson")
# Summarise output
summary(glm.output)
# Perform ANOVA on each variable in turn
drop1(glm.output, test = "Chisq")

