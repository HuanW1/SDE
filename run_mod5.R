#run module 5
source("helpers/StartMeUp.R")
thursday <- lubridate::wday(lubridate::today()) == 5
source("module_5/mod5_setup.R")
source("module_5/gary_odp_outputs.R")
#source("module_5/misc_requested_outputs.R")
if(thursday){
  source("module_5/misc_thursday_requested_outputs.R")
}
rm(list = ls())
gc(verbose = FALSE)