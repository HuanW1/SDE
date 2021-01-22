if(!dir.exists("L:/")) message("You need to have L drive mapped")
.libPaths(c("L:/library", .libPaths()))
library(knitr)
library(rmarkdown)

rmarkdown::render("0.68.8_toSQL_allgasnobrakes.rmd")
