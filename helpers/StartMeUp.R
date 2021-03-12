if(!dir.exists("L:/")) message("You need to have L drive mapped")

DPH_packages <- c("rmarkdown", "kableExtra", "tidyverse", "lubridate", "sf",  
                   "DBI", "odbc", "formatR", "knitr", "MMWRweek",
                  "scales", "english", "flextable")

sapply(DPH_packages, library, 
       logical.return = TRUE, 
       character.only = TRUE, 
       warn.conflicts = FALSE, 
       quietly = TRUE)

message("All set! The usual packages are loaded")
