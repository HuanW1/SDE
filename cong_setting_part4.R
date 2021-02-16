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
write_csv(DOC, "nancy_doc.csv")
write_csv(No_DOC_Old, "loc_determination.csv")
write_csv(No_DOC_young, "manual_review.csv")
write_csv(final, "BIGcong_review.csv")


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

write_csv(Match_wLOF_roster, "roster_FLIS_match.csv")











# 
# 
# 
# ### filter on DOC and ALF
# RosterDOCALF<- newGEO %>% filter(geo_lof == "DOC" | geo_lof =="Assisted Living") %>% 
#   select(eventid, age, lname, fname, dob, gender, race, hisp, dba, geo_lof, name)
# 
# ### filter not DOC and ALF
# RosterFLISck<- newGEO %>% filter(geo_lof != "DOC" & geo_lof !="Assisted Living") %>% 
#   select(eventid, age, dba, name, Street, City, gender, race, hisp,  geo_lof,lname, fname, dob) %>% 
#   mutate(dob = lubridate::ymd(dob))
# 
# 
# #######Need to recode hispanic from H or NH to Yes, No)
# #####match the records not DOC and ALF to the FLIS list
# 
# ### import the new running geocode
# #hopefully change this to a SQL pull
# 
# 
# ################################
# TrueFLISmatch=subset(RosterFLISck, MATCH=="TRUE") %>% 
#   select(eventid, age, lname, fname, dob, gender, race, hisp, dba, geo_lof, name)
# 
# # make geo_lof say LTCF
# TrueFLISmatch$geo_lof <-"LTCF"
# 
# #make temp of DOCALF
# temp_DOCALF=RosterDOCALF
# 
# # rename dba
# TrueFLISmatch = rename(TrueFLISmatch, 'Facility Name' ="dba")
# TrueFLISmatch = rename(TrueFLISmatch, 'Race' = "race")
# TrueFLISmatch = rename(TrueFLISmatch, 'Hispanic' ="hisp")
# TrueFLISmatch = rename(TrueFLISmatch, 'Gender' ="gender")
# TrueFLISmatch = rename(TrueFLISmatch, 'Type of congregate setting' = geo_lof)
# TrueFLISmatch = rename(TrueFLISmatch, 'Event ID' ="eventid")
# temp_DOCALF = rename(temp_DOCALF, 'Facility Name' ="dba")
# temp_DOCALF = rename(temp_DOCALF, 'Type of congregate setting' = geo_lof)
# temp_DOCALF = rename(temp_DOCALF, 'Race' = "race")
# temp_DOCALF = rename(temp_DOCALF, 'Hispanic' ="hisp")
# temp_DOCALF = rename(temp_DOCALF, 'Gender' ="gender")
# temp_DOCALF = rename(temp_DOCALF, 'Event ID' ="eventid")
# 
# # stack Rosters together
# FinalRosterMerge <- rbind(TrueFLISmatch, temp_DOCALF)
# 
# # change lower case to all capitals for Roster
# FinalRosterMerge$`Facility Name` = str_to_title(FinalRosterMerge$`Facility Name`)
# 
# # for remaining blank elements in Facility, enforce as Name variable info
# empt_inds=which(is.na(FinalRosterMerge$`Facility Name`))
# if (length(empt_inds)>0){FinalRosterMerge$`Facility Name`[empt_inds]=FinalRosterMerge$name[empt_inds]}
# 
# # if type of cong setting includes 'AL', then it should be 'ALF'
# doc_inds=grep("Assisted Living",FinalRosterMerge$`Type of congregate setting`,ignore.case=TRUE)
# if (length(doc_inds)>0){FinalRosterMerge$`Type of congregate setting`[doc_inds]="ALF"}
# 
# ##### add new variables that repeat in roster
# 
# ## add new columns to the roster
# FinalRosterMerge$'Congregate Setting'<-"YES"
# FinalRosterMerge$'Facility Number'<-NA
# FinalRosterMerge$Product <-"CORONA"
# FinalRosterMerge$'Type of CoV' <- "2019_NCOV"
# FinalRosterMerge$NMI<-"YES"
# FinalRosterMerge$State<-"CT"
# 
# # put the variables in this order
# 
# #FinalRosterMergeforCTEDSS <- FinalRosterMerge[ ,c(1,13,7,8,6,9,17,12,10,14,15,16)]
# FinalRosterMergeforCTEDSS <- FinalRosterMerge %>% 
#   select(`Event ID`, `Facility Number`, Race, Hispanic, Gender, `Facility Name`, State, `Congregate Setting`, `Type of congregate setting`, Product, `Type of CoV`, NMI)
# 
# write.table(FinalRosterMergeforCTEDSS,"FinalRosterMergeforCTEDSS.csv",na="",
#             row.names=FALSE,col.names=TRUE,sep=",")
# 
# 
# ##########################
# 
# # filter people over 65 years old
# RosterUnder65=subset(RosterFLISck, (age<65) & (MATCH==FALSE))
# # filter people 64 and under and FALSE MATCHES = manually review to find location
# RosterOver65=subset(RosterFLISck, (age>64) & (MATCH == FALSE))
# 
# ####################################  create the NEW file for staff manual review rostering into CTEDSS
# 
# 
# ###Create the file for upload to CTEDSS
# write.table(RosterUnder65,"RosterUnder65.csv",na="",
#             row.names=FALSE,col.names=TRUE,sep=",")
# 
# 
# #################################### 
# 
# 
# #####For the Falses on the RosterOver65 I need to check which congregate type they belong to ALF or LTCF
# 
# 
# 




# 
# 
# write.table(RosterOver65,"RosterOver65.csv",na="",
#             row.names=FALSE,col.names=TRUE,sep=",")
# 
# #################################### 









##########Need to reorder and complete the roster file fields



#########################################






