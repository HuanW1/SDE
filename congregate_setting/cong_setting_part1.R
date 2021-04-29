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
  mutate( date = if_else((disease_status == "Confirmed" |disease_status == "Probable") & !is.na(spec_col_date), spec_col_date, event_date),
         date = lubridate::ymd(date),
         week = epiweek(date),
         year = epiyear(date)
         ) %>%
  filter(
    date >= beginofcurmmwr-14
    & date < beginofcurmmwr
    & cong_yn == "No")

####3. Last Ran ####

statement <- paste0("SELECT * FROM DPH_COVID_IMPORT.dbo.CONG_DATERAN")
lastdate <-  DBI::dbGetQuery(conn = con , statement = statement) %>%
  as_tibble() %>%
  mutate(DateRan = lubridate::ymd(DateRan)) %>%
  arrange(desc(DateRan)) %>%
  slice(1L) %>%
  pull(DateRan)
rm(statement)

lasttime <- data.table::fread(paste0("L:/daily_reporting_figures_rdp/csv/", lastdate ,"/", lastdate, "cases_wphi.csv"), data.table = F) %>%
  select(eventid) %>%
  unique()
#only keep cases not in last file
check <- cases_14 %>%
  filter(!eventid %in% lasttime$eventid)
rm(lasttime)

####4 pulling lastest geocoded data ####
statement <- paste0("SELECT [r].[case_id]
                              ,[r].[Name]
                              ,[r].[License__]
                              ,[r].[DBA]
                              ,[r].[type]
                    FROM [DPH_COVID_IMPORT].[dbo].[CTEDSS_GEOCODED_RECORDS] as [r]")
maybecong <-  DBI::dbGetQuery(conn = con , statement = statement)%>%
  mutate(eventid = as.numeric(case_id))%>%
  right_join(check, by="eventid") %>%
  mutate_if(is.character, list(~na_if(., ""))) %>%
  mutate_if(is.character, list(~na_if(., "NULL"))) %>%
  filter(!is.na(Name) | !is.na(License__) | !is.na(DBA)) %>%
  rename(name = Name, license = License__, dba = DBA) %>%
  mutate(intoms = 1) %>%
  select(eventid, name, license, dba, type,intoms) %>%
  full_join(check, by = "eventid") %>%
  replace_na(list(intoms = 0)) %>%
  mutate(KEEP = NA) %>%
  rename(geo_name = name,
         geo_dba = dba,
         geo_license = license,
         geo_lof = type
  )
rm(statement)

# cleaning/standardizing match vars #
maybecong <- maybecong %>%
#standardizing LOF
  mutate(
    geo_lof = if_else(geo_lof == "CCNH", "LTCF", geo_lof),
    geo_lof = if_else(geo_lof == "CCRH", "LTCF", geo_lof),
    geo_lof = if_else(geo_lof == "RHNS", "LTCF", geo_lof),
    geo_lof = if_else(geo_lof == "Assisted Living", "ALF", geo_lof),
    geo_lof = if_else(geo_lof == "Residential Care Facilities", "ALF", geo_lof),
    geo_lof = na_if(geo_lof, "Other")
  ) %>%
#standardizing names
 mutate(
   geo_cname = geo_dba, #initialize the var
   geo_cname = if_else(geo_dba == geo_name, geo_dba, geo_cname),
   geo_cname = if_else(is.na(geo_dba) & !is.na(geo_name), geo_name, geo_cname),
   geo_cname = if_else(!is.na(geo_dba) & is.na(geo_name), geo_dba, geo_cname )
 ) %>%
  select(-c(geo_name, geo_dba)) %>%
  select(eventid, fname, lname, age, dob, gender, race, hisp, street, city, county, state, geo_cname, geo_license, geo_lof, intoms)


####5 send the new date ran up####
justran <-tibble("DateRan" = Sys.Date())
DBI::dbWriteTable(conn = con, value = justran, name = SQL("DPH_COVID_IMPORT.dbo.CONG_DATERAN"), overwrite = FALSE, append = TRUE)
odbc::dbDisconnect(con)

message("Part 1/4 finished- you're off to a good start!")
source("congregate_setting/cong_setting_part2.R")
