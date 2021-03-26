if(!dir.exists("L:/")) message("You need to have L drive mapped")

source("helpers/StartMeUp.R")

.libPaths("L:/newlib")

source("GoodMorning.R")

rmarkdown::render("SQL_allgasnobrakes.rmd")
