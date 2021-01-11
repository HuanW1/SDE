.libPaths(c("L:/library", .libPaths()))
#.libPaths(rev(.libPaths()[2]))
library(knitr)
library(rmarkdown)

rmarkdown::render("markdown_report_autopull_0.68.6.rmd")
