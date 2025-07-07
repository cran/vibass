## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  collapse = TRUE,
  comment = "#>"
)
pacman::p_load(
  colorspace,
  cowplot,
  dplyr,
  extraDistr,
  ggplot2,
  hrbrthemes,
  knitr,
  magrittr,
  png,
  tibble,
  tidyr,
  waffle
)


## ----fig.cap = cap, echo = FALSE, fig.align='center'--------------------------
cap <- "A pile of peanut M&M's candies. Photo by Victor Roda."
include_graphics("mm_photo.jpg")

## ----sampling-process, fig.cap = cap, echo = FALSE, fig.width=9, fig.height = 4----
cap <- "Representation of the sampling experiment."
## Generates a bunch of M&Ms
# set.seed(20210519)
# tibble(
#   x = runif(1e3),
#   y = runif(1e3)
# ) %>% 
#   ggplot(aes(x, y)) +
#   geom_point(
#     colour = sample(vibass:::mm_cols, 1e3, replace = TRUE)
#   ) +
#   coord_fixed() +
#   theme_void()
## Manually edit the image with GIMP for blur effects on wiggly boundaries. Save into mm_bag.png.
mm_bag <- png::readPNG("mm_bag.png")

sample_dat <- 
  expand_grid(
    x = 1:5,
    y = 1:4
  ) %>% 
  mutate(
    red = x == 1
  )


p <-
  sample_dat %>% 
  ggplot(aes(x, y)) +
  geom_point(
    aes(colour = !red),
    size = 20,
    show.legend = FALSE
  ) +
  geom_curve(
    data = tribble(
      ~x, ~y, ~xend, ~yend,
      -2, 2.5, 0, 2.5,
      0, 1.5, .4, 2
    ),
    aes(xend = xend, yend = yend),
    colour = "grey75",
    linewidth = .5,
    curvature = -.1,
    arrow = arrow(length = unit(0.01, "npc"), type = "closed")
  ) +
  annotate(
    "text",
    x = 0, y = 1.3,
    label = "Red M&Ms: 4",
    hjust = .8,
    colour = vibass:::mm_cols["red"]
  ) +
  geom_curve(
    data = tribble(
      ~x, ~y, ~xend, ~yend,
      .7, 4.5, 5.3, 4.5
    ),
    aes(xend = xend, yend = yend),
    colour = "grey75",
    linewidth = .5,
    curvature = -.1
  ) +
  annotate(
    "text",
    x = 3, y = 5,
    label = "Sample size: 20"
  ) +
  scale_colour_discrete(type = c(vibass:::mm_cols["red"], "grey")) +
  coord_fixed(xlim = c(-6, 6), ylim = c(0, 5)) +
  theme_void()

ggdraw() +
  draw_image(mm_bag, x = -.3, y = 0) +
  draw_plot(p) +
  annotate(
    "text",
    x = .2, y = .1,
    label = "Infinite population of M&Ms"
  ) +
  annotate(
    "text",
    x = .75, y = .1,
    label = "Observed sample"
  )

## -----------------------------------------------------------------------------
mm_sample <- data.frame(n = 20, r = 4) 

## ----binomial-pdfs, fig.cap = cap, fig.width = 5, fig.height = 3, echo = FALSE----
cap <- "Binomial probability function for two values of $\\theta$ and $n=20$ observations."
theta_labels <- 
  tibble(
    theta = c("0.5", "0.2"),
    r = c(12, 6),
    prob = c(.18, .18),
    label = c("theta==0.5", "theta==0.2"),
    hjust = 0
  )

tibble(
  r = 0:20
) %>% 
  mutate(
    theta_0.5 = dbinom(r, 20, 0.5),
    theta_0.2 = dbinom(r, 20, 0.2)
  ) %>% 
  pivot_longer(
    cols = starts_with("theta_"),
    names_to = "theta",
    names_prefix = "theta_",
    # names_transform = list(theta = as.numeric),
    values_to = "prob"
  ) %>% 
  ggplot(aes(r, prob, fill = theta)) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    width = .5,
    show.legend = FALSE
  ) +
  geom_text(
    data = theta_labels,
    aes(label = label, hjust = hjust, color = theta),
    parse = TRUE,
    show.legend = FALSE
  ) +
  labs(
    x = "Number of red M&Ms in the sample (r)",
    y = "Probability"
  ) +
  scale_fill_discrete_qualitative() +
  scale_color_discrete_qualitative() +
  theme_ipsum(grid = "Y")

## ----fig.align='center', out.width="60%"--------------------------------------
curve(
  dbeta(x, 0.5, 0.5),
  col = "dodgerblue", lwd = 4,
  ylim = c(0, 4),
  xlab = expression(paste("Proportion of red candies ", theta)),
  ylab = 'prior'
)

## ----fig.align='center', out.width="60%"--------------------------------------
r <- mm_sample$r
n <- mm_sample$n
curve(
  choose(n, r) * x^r * (1 - x)^(n - r),
  col = "darkorange", lwd = 4,
  xlab = expression(paste("Proportion of red candies ", theta)),
  ylab = 'likelihood'
)

## ----fig.align='center', out.width="60%"--------------------------------------
curve(
  dbeta(x, mm_sample$r + 0.5, mm_sample$n - mm_sample$r + 0.5),
  col = "darkgreen", lwd = 4,
  xlim = c(0, 1), ylim = c(0, 5), 
  xlab = expression(paste("Proportion of red candies ", theta)),
  ylab = 'posterior'
)

## -----------------------------------------------------------------------------
qbeta(c(0.025, 0.975), 4.5, 16.5)

## -----------------------------------------------------------------------------
pbeta(0.3, 4.5, 16.5) - pbeta(0.1, 4.5, 16.5)

## ----posterior-summaries, fig.cap = cap, fig.width = 5, fig.height = 3, echo = FALSE----
cap <- "Posterior summaries for $\\theta$."
inner_prob <- round(100*(pbeta(0.3, 4.5, 16.5) - pbeta(0.1, 4.5, 16.5)))
post_labels <- 
  tribble(
    ~x, ~y, ~label, ~hjust, ~col,
    .2, 2, paste("About", inner_prob, "%\nprobability\nbetween 0.1\nand 0.3"), .5, "white",
    .4, 2, paste("Central 95 % probability\nbetween", paste(round(qbeta(c(0.025, 0.975), 4.5, 16.5), 2), collapse = " and ")), 0, "grey35",
    .27, 4, paste("Posterior mean:", round(4.5/21, 2)), 0, "grey35"
  )

tibble(
  x = seq(0, .75, length = 101)
) %>% 
  mutate(
    y = dbeta(x, 4.5, 16.5)
  ) %>% 
  ggplot(aes(x, y)) +
  geom_area(
    ## 95 % CrI
    data = ~ filter(
      .x,
      between(x, qbeta(0.025, 4.5, 16.5) , qbeta(0.975, 4.5, 16.5))
    ),
    fill = "darkgreen",
    alpha = .2
  ) +
  geom_area(
    ## 95 % CrI
    data = ~ filter(
      .x,
      between(x, .1, .3)
    ),
    fill = "darkgreen",
    alpha = .4
  ) +
  geom_vline(
    xintercept = 4.5/21,
    lwd = 1,
    colour = "darkgreen",
    alpha = .6
  ) +
  geom_line(
    colour = "darkgreen",
    lwd = 1
  ) +
  geom_text(
    data = post_labels,
    aes(label = label, hjust = hjust),
    colour = post_labels$col,
    size = 8/.pt # 14pt font
  ) +
  geom_curve(
    data = tribble(
      ~x, ~y, ~xend, ~yend,
      .39, 2, .35, .8,
      .26, 4, .22, 4.5
    ),
    aes(xend = xend, yend = yend),
    colour = "grey75",
    linewidth = .5,
    curvature = .1,
    arrow = arrow(length = unit(0.01, "npc"), type = "closed")
  ) +
  labs(
    x = expression(theta),
    y = NULL,
    parse = TRUE
  ) +
  theme_ipsum(grid = "xX") +
  theme(
    axis.text.y = element_blank()
  )

## ----expectation-beta-binomial, echo = FALSE----------------------------------
expbbinom <- function(n, a, b) {
  n * a / (a + b)
}

## ----variance-beta-binomial, echo = FALSE-------------------------------------
varbbinom <- function(n, a, b) {
  n * a * b * (a + b + n) / (a + b)**2 / (a + b +1)
}

## ----fig.align='center', out.width="60%"--------------------------------------
library(extraDistr)
curve(
  dbbinom(x, size = 10, mm_sample$r + 0.5, mm_sample$n - mm_sample$r + 0.5),
  col = "purple", lwd = 4, type = 'h', n = 11,
  xlim = c(0, 10), ylim = c(0, 0.4), 
  xlab = expression(paste("Number of red candies")),
  ylab = 'probability'
)

