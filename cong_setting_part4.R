####0 loading libraries ####
if(!dir.exists("L:/")) message("You need to have L drive mapped")
.libPaths(c("L:/library", .libPaths()))
# load libraries
library(readxl)			# read_excel
library(dplyr)			# filter
library(stringr)		# str_to_title
library(tidyr)
library(tidyverse)
library(lubridate)
library(stringdist) 		# amatch, stringdist
con <- DBI::dbConnect(odbc::odbc(), "epicenter")


####1 data and dependencies read-in ####
if(nrow(newGEO)<1){
  stop("No data to review.")
}
final <- newGEO

####pulling in look ups to join facility name to ctedss 'nickname'
###nursing
statement <- paste0("SELECT * FROM [DPH_COVID_IMPORT].[dbo].[CONG_NURSING_FACILITIES]")
nursing_sub <-  DBI::dbGetQuery(conn = con , statement = statement) %>% 
  select(`Facility Name`, `CTEDSS Entry Name`)%>% 
  rename(nickname = `CTEDSS Entry Name`)
###prisons
statement <- paste0("SELECT * FROM [DPH_COVID_IMPORT].[dbo].[CONG_PRISON_FACILITIES]")
prison_sub <-  DBI::dbGetQuery(conn = con , statement = statement)%>% 
  select(`Facility Name`, `CTEDSS Entry Name`)%>% 
  rename(nickname = `CTEDSS Entry Name`)

nicknames <- bind_rows(prison_sub, nursing_sub)
#clear trash
rm(statement, nursing_sub, prison_sub)

####2 Roster Creation #####
Match_wLOF <- final %>%
  filter(!is.na(geo_lof) | Roster_Match == TRUE)

DOC <- final %>%
  filter(!eventid %in% Match_wLOF$eventid & (intoms ==1 | disposition == "Yes") & match_LOF == "DOC")

No_DOC_Old <-  final %>%
  filter(!eventid %in% Match_wLOF$eventid & (intoms ==1 | disposition == "Yes") & match_LOF != "DOC" & age >=65)

No_DOC_young <-  final %>%
  filter(!eventid %in% Match_wLOF$eventid & (intoms ==1 | disposition == "Yes") & match_LOF != "DOC" & age <65)

#write_csv(sub1, "roster_FLIS_match.csv")
write_csv(DOC, paste0(Sys.Date(),"nancy_doc.csv"))
write_csv(No_DOC_Old, paste0(Sys.Date(),"loc_determination.csv"))
write_csv(No_DOC_young, paste0(Sys.Date(),"manual_review.csv"))
write_csv(final, paste0(Sys.Date(),"BIGcong_review.csv"))


Match_wLOF_roster <- Match_wLOF %>% 
  select(eventid, race, hisp, gender, geo_lof, match_name) %>% 
  mutate(`Facility Number` = NA,
         State = "CT",
         `Congregate Setting` = "YES",
         Product = "CORONA",
         `Type of CoV` = "2019_NCOV",
         NMI = "YES",
         hisp = str_to_title(hisp),
         race = na_if(race, "Unknown")
         ) %>%
  left_join(nicknames, by = c("match_name" = "Facility Name")) %>% 
  select(eventid, `Facility Number`, race, hisp, gender, nickname, State, `Congregate Setting`, geo_lof, Product, `Type of CoV`, NMI) %>% 
  rename(`Type of congregate setting` = geo_lof,
         `Event ID` =  eventid,
         Race = race,
         Gender = gender,
         Hispanic = hisp,
         `Facility Name` = nickname
         ) 

write_csv(Match_wLOF_roster, paste0(Sys.Date(),"roster_FLIS_match.csv"))