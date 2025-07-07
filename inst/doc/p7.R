## -----------------------------------------------------------------------------
library("MASS")
data("nlschools")
summary(nlschools)

## ----message = FALSE, warning = FALSE-----------------------------------------
library("R2BayesX")
m1 <- bayesx(lang ~ IQ + GS +  SES + COMB, data = nlschools)

summary(m1)

## ----fig = TRUE, fig.width = 15, fig.height = 5-------------------------------
boxplot(lang ~ class, data = nlschools, las = 2)

## -----------------------------------------------------------------------------
m2 <- bayesx(
  lang ~ IQ + GS +  SES + COMB + sx(class, bs = "re"),
  data = nlschools
)

summary(m2)

## -----------------------------------------------------------------------------
library(lme4)
m2lmer <- lmer(
  lang ~ IQ + GS +  SES + COMB + (1 | class), # Fit a separate intercept for each level of class
  data = nlschools
)

summary(m2lmer)

## ----message = FALSE, warning = FALSE-----------------------------------------
library(spData)
data(nc.sids)
summary(nc.sids)

## -----------------------------------------------------------------------------
# Overall mortality rate
r74 <- sum(nc.sids$SID74) / sum(nc.sids$BIR74)
# Expected cases
nc.sids$EXP74 <- r74 * nc.sids$BIR74

## -----------------------------------------------------------------------------
nc.sids$SMR74 <- nc.sids$SID74 / nc.sids$EXP74

## ----fig = TRUE---------------------------------------------------------------
hist(nc.sids$SMR, xlab = "SMR")

## -----------------------------------------------------------------------------
nc.sids$NWPROP74 <- nc.sids$NWBIR74/ nc.sids$BIR74

## ----fig = TRUE---------------------------------------------------------------
plot(nc.sids$NWPROP74, nc.sids$SMR74)

# Correlation
cor(nc.sids$NWPROP74, nc.sids$SMR74)

## -----------------------------------------------------------------------------
m1nc <- bayesx(
  SID74 ~ 1 + NWPROP74,
  family = "poisson",
  offset = log(nc.sids$EXP74),
  data = nc.sids
)
summary(m1nc)

## -----------------------------------------------------------------------------
# Index for random effects
nc.sids$ID <- seq_len(nrow(nc.sids))

# Model WITH covariate
m2nc <- bayesx(
  SID74 ~  1 + NWPROP74 + sx(ID, bs = "re"),
  family = "poisson",
  offset = log(nc.sids$EXP74),
  data = as.data.frame(nc.sids)
)

summary(m2nc)

## ----fig = TRUE---------------------------------------------------------------
x.predict <- seq(0,1,length=1000)
y.predict <- exp(coef(m2nc)["(Intercept)","Mean"]+coef(m2nc)["NWPROP74","Mean"]*x.predict)
oldpar <- par(mfrow = c(1, 1))
plot(nc.sids$NWPROP74, nc.sids$SMR74)
lines(x.predict,y.predict)
par(oldpar)

## -----------------------------------------------------------------------------
# Model WITHOUT covariate
m3nc <- bayesx(
  SID74 ~  1 + sx(ID, bs = "re"),
  family = "poisson",
  offset = log(nc.sids$EXP74),
  data = as.data.frame(nc.sids)
)

summary(m3nc)

## ----fig = TRUE---------------------------------------------------------------
oldpar <- par(mfrow = c(1, 2))
boxplot(m2nc$effects$`sx(ID):re`$Mean, ylim = c(-1, 1), main = "With NWPROP74")
boxplot(m3nc$effects$`sx(ID):re`$Mean, ylim = c(-1, 1), main = "Without NWPROP74")
par(oldpar)

