.libPaths(c("L:/library", .libPaths()))
library(knitr)
library(rmarkdown)

rmarkdown::render("weekly_data_review.rmd")
#commenting to test