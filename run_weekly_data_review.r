.libPaths(c("L:/newlib", .libPaths()))

source("helpers/StartMeUp.R")

rmarkdown::render("weekly_data_review.rmd")
