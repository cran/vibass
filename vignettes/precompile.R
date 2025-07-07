# Pre-compiled vignette that depends on INLA, which is difficult to check
# in CRAN. https://ropensci.org/blog/2019/12/08/precompute-vignettes/

knitr::knit("vignettes/p8.Rmd.orig", "vignettes/p8.Rmd")

# Move image files from vibass/figure to vibass/vignettes/img after knit.
# Need to change the dirname to img/ to prevent a R CMD check note about
# knitr residual files.
system("mv figure vignettes/img")
system("sed -i 's/(figure\\//(img\\//g' vignettes/p8.Rmd")
