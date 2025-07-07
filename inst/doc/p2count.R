## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
udata <- data.frame(Us = c(25, 29, 27, 27, 25, 27, 22, 26, 27, 29, 23, 28, 25, 24, 22, 
25, 23, 29, 23, 28, 21, 29, 28, 23, 28))

## -----------------------------------------------------------------------------
(table.udata <- table(udata))

## ----fig.align='center', out.width="70%"--------------------------------------
barplot(table.udata, col="gray89", xlab="Number of letters u in a page")

## ----fig.align='center', out.width="60%"--------------------------------------
curve(
  dgamma(x, 105.8, 4.6), 
  col="dodgerblue", lwd=4,
  xlim=c(15,35), ylim=c(0,0.5),
  xlab = expression(paste("Expected number of u's in a page ", lambda)),
  ylab = 'Prior 2 (Joan)'
  )

## -----------------------------------------------------------------------------
setNames(nm = c(20, 23, 25, 30, 35)) |> pgamma(105.8, 4.6) |> round(3)

## ----fig.align='center', out.width="60%"--------------------------------------
sum_y <- sum(udata$Us)
n <- nrow(udata)
scale_y <- sum(log(factorial(udata)))
curve(
  exp(sum_y * log(x) - n * x - scale_y),
  col = "darkorange", lwd = 4,
  xlim = c(15, 35),
  xlab = expression(paste("Expected number of u's in a page ", lambda)),
  ylab = 'likelihood'
)

## ----fig.align='center', out.width="60%"--------------------------------------
curve(
  dgamma(x, 748.8, 29.6),
  col = "darkgreen", lwd = 4,
  xlim = c(15, 35),
  xlab = expression(paste("Expected number of u's in a page ", lambda)),
  ylab = 'prior and posteriors'
)
curve(
  dgamma(x, 643.5, 25),
  col = "green3", lwd = 4,
  add = TRUE
)
curve(
  dgamma(x, 105.8, 4.6),
  col = "dodgerblue", lwd = 4,
  add = TRUE
)


## -----------------------------------------------------------------------------
setNames(nm = c(0.025, 0.975)) |> qgamma(643.5, 25) |> round(1)
setNames(nm = c(0.025, 0.975)) |> qgamma(748.8, 29.6) |> round(1)

## -----------------------------------------------------------------------------
round(pgamma(26, 643.5, 25) - pgamma(23, 643.5, 25), 3)
round(pgamma(26, 748.8, 29.6) - pgamma(23, 748.8, 29.6), 3)

## ----fig.align='center', out.width="55%"--------------------------------------
library(extraDistr)
x <- c(10:40)
pred1 <- dgpois(10:40, 643.5, 25)
pred2 <- dgpois(10:40, 748.8, 29.6)
plot(
  x, pred1,
  type = "h", lwd = 2, col = "purple", 
  xlim = c(10, 40), ylim = c(0, 0.1), 
  xlab = "number of letters u in a new page", 
  ylab = "probability"
)
plot(
  x, pred2,
  type = "h", lwd = 2, col = "purple",
  xlim = c(10, 40), ylim = c(0, 0.1),
  xlab = "number of u's in a new page",
  ylab = "probability"
)

