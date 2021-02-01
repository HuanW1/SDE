# options(scipen = 999)
# mypak <- function(pkg){
#   new.pkg <- pkg[!(pkg %in% installed.packages(lib.loc = .libPaths()[1])[, "Package"])]
#   if (length(new.pkg)) 
#     install.packages(new.pkg, dependencies = TRUE, repos = "https://cran.rstudio.com/")
#   sapply(pkg, require, character.only = TRUE)
# }
# my_packages <-  c("ggspatial","data.table","sf","odbc","tm","viridis",
#                   "kableExtra","webshot", "geosphere", "pracma", "tidyverse", "tidytext", "ggplot2", "ggraph",
#                   "igraph", "widyr", "stringr", "lubridate", "readr", "leaflet", "RColorBrewer", "ggmap", "rgdal",
#                   "tigris", "acs", "tidycensus", "DT", "rgeos", "formatR", "knitr", "MMWRweek", "sf",
#                   "RODBC", "plotly", 'rgeos', "scales", "english", 'flextable', 'slider', "transformr", "gganimate")
# mypak(my_packages)
# dir <- getwd()

if(!dir.exists("L:/")) message("You need to have L drive mapped")
.libPaths(c("L:/library", .libPaths()))

#### required packages are on L: ####
# require(rmarkdown)
require(tidyverse)
require(sf)
require(odbc)
require(kableExtra)
require(lubridate)
require(formatR)
require(knitr)
require(MMWRweek)
require(scales)
require(english)
require(flextable)
require(DBI)


#0. set this epiweek
thisweek <-epiweek(Sys.Date())

#1. Read in case data (cases w/ phi)
casenew <- read_csv(paste0("L:/daily_reporting_figures_rdp/csv/", Sys.Date(), "/", Sys.Date(), "cases_wphi.csv"),
                    col_types = cols(spec_col_date = col_character(), 
                                     event_date = col_character()))

#2. create new variables for reside in congregate setting (y/n), date for case (1st positive specimen collection or eventdate),
#and mmwr week for that case date
casenew <- casenew %>%
  mutate(cong_yn = if_else((str_detect(cong_exposure_type, "Reside")) &
                             (str_detect(cong_setting, "Jail / Prison") | str_detect(cong_setting, "Long term care facility") | str_detect(cong_setting, "Assisted Living Facility")),
                           "Yes", "No", missing = "No"),
         date = ifelse(disease_status == "Confirmed" & !is.na(spec_col_date), spec_col_date, event_date),
         date = ymd(date), 
         week = epiweek(date), 
         year = epiyear(date)
  ) 
count(casenew, cong_yn)

#3. create dataset of cases with date in past 2 mmwr weeks & apparently congregate setting = No
cases_14 <- casenew %>%
  filter(week==thisweek-1 | week==thisweek-2) %>%
  filter(cong_yn == "No")

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
