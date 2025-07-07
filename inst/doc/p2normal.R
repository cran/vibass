## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  collapse = TRUE,
  comment = "#>"
)
pacman::p_load(
  knitr,
  plotly,
  vibass
)


## ----fig.cap = cap, echo = FALSE, fig.align='center'--------------------------
cap <- "Some VIBASS' participants."
include_graphics("vibass_participants.jpg")

## ----eval=TRUE----------------------------------------------------------------
hwomen <- data.frame(height = c(1.73, 1.65, 1.65, 1.76, 1.65, 1.63, 1.70, 1.58, 1.57, 1.65, 1.74, 1.68, 1.67, 1.58, 1.66))

## ----summary-table-women-height-----------------------------------------------
summary_table(
  mean = mean(hwomen$height),
  var = var(hwomen$height),
  quant = quantile(hwomen$height, probs = c(0, 0.25, 0.5, 0.75, 1)),
  label = "Women height",
  digits = 3
)

## ----fig.align='center', out.width="65%"--------------------------------------
hist(hwomen$height,
     main = NULL, xlab="Height in metres",
     col="gray89", xlim=c(1.50, 1.85)
)

## -----------------------------------------------------------------------------
qnorm(c(0.005, 0.995), 1.70, 0.07)
pnorm(c(1.50, 1.60, 1.70, 1.80, 1.90), 1.70, 0.07)

## ----fig.align='center', out.width="60%", fig.cap = cap-----------------------
cap <- "Prior distribution for the mean height."
m0 <- 1.7; s0 <- 0.07
curve(
  dnorm(x, m0, s0),
  xlab = expression(paste(mu)), ylab = "prior",
  xlim = c(1.40, 2), lwd = 4, col = "dodgerblue", yaxt = "n"
)

## ----fig.align='center', out.width="60%", fig.cap = cap-----------------------
cap <- "Likelihood function."
Lnorm <- function(mu, y, sigma) {
  # Essentially:
  # prod(dnorm(y, mean = mu, sd = sigma))
  # but we make it numerically more stable working on the log scale
  # and we vectorise over mu
  vapply(
    mu,
    function(.) exp(sum(dnorm(y, mean = ., sd = sigma, log = TRUE))),
    1
  )
}
curve(
  Lnorm(x, hwomen$height, 0.1),
  xlab = expression(paste(mu)), ylab = "likelihood", 
  xlim = c(1.4, 2), col = "darkorange", lwd = 4, yaxt = "n"
)

## ----fig.align='center', out.width="60%", fig.cap = cap, echo = -1------------
cap <- "Prior (blue) and posterior (green) distributions for the mean height."
m0 <- 1.7; s0 <- 0.07
m1 <- 1.6648; s1 <- 0.0242
curve(
  dnorm(x, m1, s1),
  xlab = expression(paste(mu)), ylab = "density",
  xlim = c(1.40, 2), lwd = 4, col = "darkgreen", yaxt = "n"
)
curve(
  dnorm(x, m0, s0),
  lwd = 4, col = "dodgerblue", add = TRUE
)


## -----------------------------------------------------------------------------
qnorm(c(0.005, 0.995), m1, s1)
pnorm(c(1.50, 1.60, 1.70, 1.80, 1.90), m1, s1)

## ----posterior-predictive, fig.align='center', out.width="60%", fig.cap = cap, echo = -1----
cap <- "Predictive (purple) and posterior (green) distributions for the mean height."
mp <- m1; sp <- sqrt(s1^2 + 0.1^2)
curve(
  dnorm(x, m1, s1),
  xlab = expression(paste(mu)), ylab = "density",
  xlim = c(1.40, 2), lwd = 4, col = "darkgreen", yaxt = "n"
)
curve(
  dnorm(x, mp, sp),
  lwd = 4, col = "purple", add = TRUE
)


## -----------------------------------------------------------------------------
qnorm(c(0.025, 0.975), 1.6642, 0.1029)

## ----likelihood, echo = FALSE-------------------------------------------------
## Note: plotly breaks LaTeX rendering in rmarkdown
## Need to enclose output in a iframe. 
## https://github.com/plotly/plotly.R/blob/master/inst/examples/rmd/MathJax/index.Rmd

cap <- "Bi-variate likelihood function"

lik_norm <- function(m, s) {
  exp(-length(hwomen$height) * log(s) - sum((hwomen$height - m) ** 2) / 2 / s**2)
}

mu_vals <- seq(1.60, 1.75, by = 0.002)
s_vals <- seq(0.03, 0.10, by = 0.002)

surf_lik <- expand.grid(
  mu = mu_vals,
  s = s_vals
)
surf_lik$z <- mapply(lik_norm, surf_lik$mu, surf_lik$s)

## ----likelihood-3d, fig.cap = cap, echo = FALSE, eval = identical(Sys.getenv("_R_CHECK_CRAN_INCOMING_"), "")----
p <- plot_ly() %>% 
  add_surface(
    x = ~ mu_vals,
    y = ~ s_vals,
    z = ~ t(matrix(surf_lik$z, length(mu_vals), length(s_vals))),
    showscale = FALSE
  ) %>% 
  layout(
    scene = list(
      xaxis = list(title = "mu"),
      yaxis = list(title = "s2"),
      zaxis = list(visible = FALSE)
    )
  )

htmlwidgets::saveWidget(p, "like_surf.html")


## ----iframe, results = 'asis', echo = FALSE, eval = identical(Sys.getenv("_R_CHECK_CRAN_INCOMING_"), "")----
cat('<iframe src="like_surf.html" width="100%" height="400" id="igraph" scrolling="no" seamless="seamless" frameBorder="0"> </iframe>')


## ----likelihood-3d-flat, fig.cap = cap, echo = FALSE, eval = !identical(Sys.getenv("_R_CHECK_CRAN_INCOMING_"), "")----
# surf_lik |>
#   ggplot() +
#   aes(mu, s, fill = z) +
#   geom_tile(show.legend = FALSE) +
#   scale_fill_viridis_c() +
#   coord_fixed() +
#   labs(
#     x = "μ",
#     y = "σ²"
#   ) +
#   theme_minimal() +
#   theme(
#     plot.margin = margin(0, 0, 0, 0)
#   )
# 

## -----------------------------------------------------------------------------
ny <- length(hwomen$height)
ybar <- mean(hwomen$height)
s2 <- sum((hwomen$height - ybar) ** 2) / (ny - 1)

post_mu <- list(
  mean = ybar,
  scale = sqrt(s2 / ny)
)
post_mu$scale * qt(c(0.005, 0.995), ny - 1) + post_mu$mean
pt((c(1.50, 1.60, 1.70, 1.80, 1.90) - post_mu$mean) / post_mu$scale, ny - 1)

## ----posterior-bivariate, fig.align='center', out.width="60%", fig.cap = cap, echo = -1----
cap <- "Posterior (green) distribution for the mean height."
curve(
  dt((x - post_mu$mean) / post_mu$scale, ny - 1) / post_mu$scale,
  xlab = expression(paste(mu)), ylab = "density",
  xlim = c(1.40, 2), lwd = 4, col = "darkgreen", yaxt = "n"
)

## ----fig.align='center', out.width="70%"--------------------------------------
y <- seq(0, 40, 0.001)
simuchi <- rchisq(y, ny - 1)
simu.sigma <- 0.045598 / simuchi 
hist(
  simu.sigma,
  breaks = 300, freq = FALSE, col = "gray99",
  xlim = c(0, 0.02), ylim = c(0, 400),
  main = NULL, ylab = "density", xlab = expression(paste(sigma2))
)

summary(simu.sigma)
var(simu.sigma)
sqrt(var(simu.sigma))
quantile(simu.sigma, probs = c(0.005,0.995))

## ----eval=TRUE----------------------------------------------------------------
hmen <- data.frame(height = c(1.92, 1.82, 1.69, 1.75, 1.72, 1.71, 1.73, 1.69, 1.70, 1.78, 1.88, 1.82, 1.86, 1.65))

