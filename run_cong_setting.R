if(!dir.exists("L:/")) message("You need to have L drive mapped")

.libPaths("L:/newlib")

DPH_packages <- c( "tidyverse", "lubridate", "stringr",
                   "DBI", "odbc", "formatR", "knitr", "MMWRweek", "stringdist",
                   "mgsub", "data.table")

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
source(file = "congregate_setting/cong_setting_part1.R")
