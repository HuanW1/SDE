if(!dir.exists("L:/")) message("You need to have L drive mapped")
.libPaths(c("L:/newlib", .libPaths()))
library(knitr)
library(rmarkdown)

rmarkdown::render("SQL_allgasnobrakes.rmd")
