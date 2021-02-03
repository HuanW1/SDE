if(!dir.exists("L:/")) message("You need to have L drive mapped")
.libPaths(c("L:/library", .libPaths()))

#### required packages are on L: ####
require(tidyverse)
require(sf)
require(odbc)
require(lubridate)
require(formatR)
require(MMWRweek)
require(DBI)
require(stringr)
con <- DBI::dbConnect(odbc::odbc(), "epicenter")

####0. set this epiweek and year####
thisweek <-lubridate::epiweek(Sys.Date())
thisyear <-lubridate::epiyear(Sys.Date())

####1. Read in latest case data####
latestcasesdate <- list.files('L:/daily_reporting_figures_rdp/csv')[length(list.files('L:/daily_reporting_figures_rdp/csv'))]
cases_14 <- data.table::fread(paste0('L:/daily_reporting_figures_rdp/csv/', latestcasesdate,"/",latestcasesdate,"cases_wphi.csv"), data.table = F)
rm(latestcasesdate)

####2. Filter to last 2 complete mmwr weeks####
#selects folks not labeled cong already
beginofcurmmwr <- MMWRweek2Date(MMWRyear = thisyear, MMWRweek = thisweek, MMWRday = 1)
cases_14 <- cases_14 %>%
  mutate(
         date = ifelse(disease_status == "Confirmed" & !is.na(spec_col_date), spec_col_date, event_date), #account for probs?
         date = lubridate::ymd(date),   
         week = epiweek(date), 
         year = epiyear(date)
         ) %>%
  filter(
    date >= beginofcurmmwr-14 
    & date < beginofcurmmwr 
    & cong_yn == "No") 

####3. Create list addresses associated with congregate settings among cases not marked as congregate setting cases for past 7 days####
#read in last file checked (you will have to set the date to last Monday/Wednesday)


####3a Last ran#####
statement <- paste0("SELECT * FROM DPH_COVID_IMPORT.dbo.CONG_DATERAN")
lastdate <-  DBI::dbGetQuery(conn = con , statement = statement) %>% 
  as_tibble() %>% 
  mutate(DateRan = lubridate::ymd(DateRan)) %>% 
  arrange(desc(DateRan)) %>% 
  slice(1L) %>% 
  pull(DateRan)
rm(statement)
 #this needs automation and someone to explain this and the above to me, is variable, SQL FLAG

lasttime <- data.table::fread(paste0("L:/daily_reporting_figures_rdp/csv/", lastdate ,"/", lastdate, "cases_wphi.csv"), data.table = F) %>% 
  select(eventid) %>% 
  unique()
#only keep cases not in last file
check <- cases_14 %>% 
  filter(!eventid %in% lasttime$eventid)
#write_csv(check, paste0("L:/daily_reporting_figures_rdp/csv/", Sys.Date(), "/", Sys.Date(), "checkCongregatesetting.csv")) #should place in SQL or just leave in global environment for other code. Right?  folks aren't checking these anymore? remove- check future code?
rm(lasttime)

####4 pulling lastest geocoded data ####
statement <- paste0("SELECT * FROM DPH_COVID_IMPORT.INFORMATION_SCHEMA.TABLES WHERE TABLE_TYPE = 'BASE TABLE'")
tabs <-  DBI::dbGetQuery(conn = con , statement = statement)
tabs <- tibble::as_tibble(tabs) %>% 
  filter(stringr::str_detect(string = TABLE_NAME, pattern = "Geo_Tract")) %>% 
  mutate(date = stringr::str_extract(string = TABLE_NAME, pattern = "\\w\\w\\w\\_\\d\\d?"),
         date = paste0(date,"_",year(Sys.Date())),
         date = lubridate::mdy(date)
         ) %>% 
  arrange(desc(date)) %>% 
  slice(1L) %>% 
  select(TABLE_NAME)

statement <- paste0("SELECT [r].[case_id]
                              ,[r].[X]
                              ,[r].[Y]
                              ,[r].[geoid10]
                              ,[r].[Name]
                              ,[r].[License__]
                              ,[r].[DBA]
                              ,[r].[type]
                    FROM [DPH_COVID_IMPORT].[dbo].[", tabs,"] as [r]")
maybecong <-  DBI::dbGetQuery(conn = con , statement = statement)%>% 
  mutate(eventid = as.numeric(case_id))%>% 
  right_join(check, by="eventid") %>% 
  mutate_if(is.character, list(~na_if(., ""))) %>% 
  mutate_if(is.character, list(~na_if(., "NULL"))) %>%
  filter(!is.na(Name) | !is.na(License__) | !is.na(DBA))
rm(statement)

data.table::fwrite(maybecong, paste0("L:/daily_reporting_figures_rdp/csv/", Sys.Date(), "/", Sys.Date(), "checkCongregatesetting_GEOCODE.csv"))#basis of rest of code?

#4 anything in toms will beflagged in the 'new master'

check<- check %>% 
  mutate(
    intoms = ifelse(eventid %in%  maybecong$case_id, 1,0)
                                        )
                                                                                                        #misleading name
data.table::fwrite(check, paste0("L:/daily_reporting_figures_rdp/csv/", Sys.Date(), "/", Sys.Date(), "NEWcheckCongregatesetting_GEOCODE.csv"))#ones that don't code in, probably not needed with molly's code, leave in for now                           # name change to past14daysdelta?  new cases compared to when this was run last from past 2 complete mmwr weeks
  


#5 send the new date ran up
justran <-tibble("DateRan" = Sys.Date()) 
DBI::dbWriteTable(conn = con, value = justran, name = SQL("DPH_COVID_IMPORT.dbo.CONG_DATERAN"), overwrite = FALSE, append = TRUE)
odbc::dbDisconnect(con)
