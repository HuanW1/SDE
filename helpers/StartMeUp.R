if(!dir.exists("L:/")) message("You need to have L drive mapped")

DPH_packages <- c("rmarkdown", "kableExtra", "tidyverse", "lubridate", "sf",  
                   "DBI", "odbc", "formatR", "knitr", "MMWRweek",
                  "scales", "english", "flextable")

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

message("All set! The usual packages are loaded")
