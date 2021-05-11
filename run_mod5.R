#run module 5
message("Welcome to module 5, the tour will take approximately 5 minutes")
message("Over here on our right here we have our start up helper")
source("helpers/StartMeUp.R")

csv_write <- TRUE
SQL_write <- TRUE

thursday <- lubridate::wday(lubridate::today()) == 5
message("and over here on the left we have our module 5 setup and ODP outputs")
source("module_5/mod5_setup.R")
source("module_5/gary_odp_outputs.R")
#source("module_5/misc_requested_outputs.R")
if(thursday){
message("and over there everything the light touches is the COVID-19 Report team's domain, except that shadowy place... that is Thursday, you must never go there")  
  source("module_5/misc_thursday_requested_outputs.R")
}
message("thus concludes our tour of module 5")