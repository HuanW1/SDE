if(!dir.exists("L:/")) message("You need to have L drive mapped")
source("helpers/StartMeUp.R")
.libPaths(c("L:/newlib", .libPaths()))
# library(knitr)
# library(rmarkdown)


rmarkdown::render("SQL_allgasnobrakes.rmd")
