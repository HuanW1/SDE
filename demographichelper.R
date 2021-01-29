
#load packages
library(knitr)
library(markdown)
library(rmarkdown)


  ###################################   FILL IN CORRECT INFO HERE   #################################
#put in name of town (must be official city)
town <- "Norwich"
#put in first day (Sunday) of week of first increase 
startdate<-mdy("08-23-2020")
#update to latest geocode results
monthday <- "NOV_05"

townlist<-tibble(town=town, startdate=startdate, monthday=monthday)


for(town in townlist){
  rmarkdown::render('DetailedTownDemographics.Rmd',
                    output_file=paste0(town, "_report_", Sys.Date()),
                    output_dir = paste0("csv/", Sys.Date()))}
