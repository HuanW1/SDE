if(!dir.exists("L:/")) message("You need to have L drive mapped")

DPH_packages <- c( "tidyverse", "lubridate", "stringr",
                   "DBI", "odbc", "formatR", "knitr", "MMWRweek","stringdist")

quiet_load <- function(x) {
  suppressPackageStartupMessages(library(x,
                                         lib.loc = "l:/newlib/",
                                         logical.return = TRUE,
                                         character.only = TRUE,
                                         warn.conflicts = FALSE,
                                         quietly = TRUE,
                                         attach.required = TRUE))
}

sapply(DPH_packages, quiet_load)
source(file = "cong_setting_part1.R")