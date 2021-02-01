if(!dir.exists("L:/")) message("You need to have L drive mapped")
.libPaths(c("L:/library", .libPaths()))

#### required packages are on L: ####
require(tidyverse)
require(sf)
require(odbc)
require(lubridate)
require(formatR)
require(knitr)
require(MMWRweek)
require(scales)
require(DBI)


####0. set this epiweek and year####
thisweek <-lubridate::epiweek(Sys.Date())
thisyear <-lubridate::epiyear(Sys.Date())

####1. Read in latest case data####
latestcasesdate <- list.files('L:/daily_reporting_figures_rdp/csv')[length(list.files('L:/daily_reporting_figures_rdp/csv'))]
casenew <- data.table::fread(paste0('L:/daily_reporting_figures_rdp/csv/', latestcasesdate,"/",latestcasesdate,"cases_wphi.csv"))
rm(latestcasesdate)

####2. Filter to last 2 complete mmwr weeks####
#selects folks not labeled cong already
beginofcurmmwr <- MMWRweek2Date(MMWRyear = thisyear, MMWRweek = thisweek, MMWRday = 1)

cases_14 <- cases_14 %>%
  mutate(
         date = ifelse(disease_status == "Confirmed" & !is.na(spec_col_date), spec_col_date, event_date),
         date = ymd(date), 
         week = epiweek(date), 
         year = epiyear(date)
         ) %>%
  filter(
    date >= beginofcurmmwr-14 
    & date < beginofcurmmwr 
    & cong_yn == "No") 

#4. Create list to check for addresses associated with congregate settings among cases not marked as congregate setting cases for past 7 days
#read in last file checked (you will have to set the date to last Monday/Wednesday)



####FILL IN THIS DATE#####
lastdate<- ymd("2021-01-25")
##########################

lasttime<- read_csv(paste0("L:/daily_reporting_figures_rdp/csv/", lastdate ,"/", lastdate, "cases_wphi.csv"))
#only keep cases not in last file
check<-cases_14 %>% 
  filter(! eventid %in% lasttime$eventid)
write_csv(check, paste0("L:/daily_reporting_figures_rdp/csv/", Sys.Date(), "/", Sys.Date(), "checkCongregatesetting.csv"))

#use geocoding to help?

#####CORRECT DAY FOR LATEST GEOCODED DATASET
monthday <- format(ymd("2021-01-25"), "%b_%d")
###############################################

con <- DBI::dbConnect(odbc::odbc(), "epicenter")
statement <- paste0("SELECT [r].[case_id]
                              ,[r].[X]
                              ,[r].[Y]
                              ,[r].[geoid10]
                              ,[r].[Name]
                              ,[r].[License__]
                              ,[r].[DBA]
                              ,[r].[type]
                    FROM [DPH_COVID_IMPORT].[dbo].[", monthday,"_Covid_Geo_Tract] as [r]")
alltestgeo <-  DBI::dbGetQuery(conn = con , statement = statement)
odbc::dbDisconnect(con)

alltestgeo<- alltestgeo %>% 
  mutate(eventid = as.numeric(case_id))

check2<-check %>% 
  left_join(alltestgeo, by="eventid") %>% 
  mutate(Name=ifelse(Name=="" | Name=="NULL", NA, Name),
         License__=ifelse(License__=="" | License__=="NULL", NA, License__),
         DBA=ifelse(DBA==""| DBA=="NULL", NA, DBA))

maybecong<-check2 %>% 
  filter(!is.na(Name) | !is.na(License__) | !is.na(DBA))

write_csv(maybecong, paste0("L:/daily_reporting_figures_rdp/csv/", Sys.Date(), "/", Sys.Date(), "checkCongregatesetting_GEOCODE.csv"))

#4 anything in toms will beflagged in the 'new master'

newmaybecong <- check %>% 
  mutate(
    intoms = ifelse(eventid %in%  maybecong$case_id, 1,0)
    )

write_csv(newmaybecong, paste0("L:/daily_reporting_figures_rdp/csv/", Sys.Date(), "/", Sys.Date(), "NEWcheckCongregatesetting_GEOCODE.csv"))
