.libPaths(c("L:/library", .libPaths()))
library(knitr)
library(rmarkdown)

rmarkdown::render("weekly_data_review2.3.6.rmd")
