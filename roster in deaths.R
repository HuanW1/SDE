#jan 2021 originated by AK

#pointing to L drive for libraries
if(!dir.exists("L:/")) message("You need to have L drive mapped")

source("helpers/StartMeUp.R")

.libPaths("L:/newlib")

# # loading the required libraries
# library(readxl)			# read_excel
# library(dplyr)			# filter
# library(stringr)			# str_to_title
# library(tidyr)
# library(tidyverse)
# library(stringdist)
# library(lubridate)


###########################
## importing the old OCME list, subseting to covid deaths
oldocme <-  read.csv("L:/daily_reporting_figures_rdp/DeathRostering/old.csv") %>% 
  subset(COVID.Death == "Yes")

### importing the new OCME list
newocme <-  read.csv("L:/daily_reporting_figures_rdp/DeathRostering/new.csv") %>% 
  subset(COVID.Death == "Yes")

#### link on ocme id and only keep the new people for the day
OCME_newdeaths <- anti_join(newocme, oldocme, by=c("OCME."))


Roster <- OCME_newdeaths  %>% 
  separate(col = Name, into = c("lname", "fname"), sep = ",") %>% 
  separate(col = Race.Ethnicity, into = c("Race", "Ethnicity"), sep = ",") %>% 
  select(OCME., DOB, DOD, lname, fname, DateApproved, Residence, Sex, Race, 
         Ethnicity, PronouncedBy) 

# fix for any race
# if Race includes 'HISPANIC', then Ethnicity should be 'YES'
race_inds = grep("HISPANIC",Roster$Race,ignore.case=TRUE)
if (length(race_inds)>0){Roster$Ethnicity[race_inds]="YES"}

# if Race includes 'yes', then Ethnicity should be 'hispanic'
race_inds2 = grep("YES",Roster$Ethnicity,ignore.case=TRUE)
if (length(race_inds2)>0){Roster$Race[race_inds2]="WHITE"}

# for Black in Roster$Race, enforce as "Black or African American"
black_inds = grep("Black",Roster$Race,ignore.case=TRUE)
if (length(black_inds)>0){
  Roster$Race[black_inds]="Black or African American"
}


# SQL connection
statement <-
  paste0("SELECT [event_id]
, [fname]
, [lname]
, [dob]
, [ocme_num]
, [disease_status] FROM [DPH_COVID_IMPORT].[dbo].[CTEDSS_DAILY_REPORT_ALL_Cases]")


con2 <- DBI::dbConnect(odbc::odbc(), "epicenter")
lookup <- DBI::dbGetQuery(conn = con2 , statement = statement)
odbc::dbDisconnect(con2)
glimpse(lookup)

AllCases <- lookup

table(AllCases$disease_status)

################################################################################
# step 2 match names and dob
# 1a. 
Roster$name <-  paste(Roster$fname, Roster$lname)

AllCases$name <-  paste(AllCases$fname, AllCases$lname)

# 1b. find nearest match in name
match_inds <-  amatch(Roster$name, AllCases$name, maxDist=100)

# 1c. construct a results matrix
results <-  data.frame(Roster$name,AllCases$name[match_inds],Roster$DOB, AllCases$dob[match_inds], AllCases$disease_status[match_inds])
names(results) <-  c("Raw_Name","Match_Name","Raw_DOB","Match_DOB","MatchStatus")

# flag people with multiple

# 1d. enforce classes
results$Raw_DOB <-  as.Date(results$Raw_DOB,format="%m/%d/%Y")
results$Match_DOB <-  as.Date(results$Match_DOB,format="%m/%d/%Y")

# 1d. add string distance
results$Name_Dist <-  stringdist(results$Raw_Name,results$Match_Name)
# 1e. add date distance
results$DOB_Dist <-  as.numeric(abs(results$Raw_DOB-results$Match_DOB))

year(results$Raw_DOB)
month(results$Raw_DOB)
day(results$Raw_DOB)

results$BDay_Match <-  month(results$Raw_DOB)==month(results$Match_DOB)
results$BDay_Match <-  results$BDay_Match+(day(results$Raw_DOB)==day(results$Match_DOB))
results$BDay_Match <-  results$BDay_Match+(year(results$Raw_DOB)==year(results$Match_DOB))

results$EventID = AllCases$event_id[match_inds]

# initialize results
results$dobMatch_name = ""
results$dobMatch_dist = -1

# # for each record in the roster
# for (i in 1:nrow(Roster)){
#   # extract the ith date of birth and name and disease
#   temp_dob=Roster$DOB[i]
#   temp_name=Roster$name[i]
#   # find all records with same dob
#   match_dobs=which(as.Date(AllCases$DOB)==as.Date(temp_dob,format="%m/%d/%Y"))
#   # find all names with same dob
#   match_names=AllCases$name[match_dobs]
#   # find best-match name to this dob
#   match_idx=amatch(temp_name,match_names,maxDist=100)
#   match_name=match_names[match_idx]
#   # determine string distance of dob-matched name
#   match_dist=stringdist(temp_name,match_name)
#   # take note of results
#   results$dobMatch_name[i]=match_name
#   results$dobMatch_dist[i]=match_dist
# } # end-for (iteration over roster)

# check the names and put the match in the tables
OCME_newdeaths$MATCH = results$BDay_Match>1 & results$Name_Dist<10
Roster$MATCH = results$BDay_Match>1 & results$Name_Dist<10
Roster$BDay_MATCH = results$BDay_Match
Roster$Name_Distance = results$Name_Dist
Roster$DiseaseStatus = results$MatchStatus

####################################
# create the unique table/file for Rostering into CTEDSS


##rename variables for Roster format to match
Roster = rename(Roster, OCME_ID ="OCME.")
Roster = rename(Roster, Fname ="fname")
Roster = rename(Roster, Lname ="lname")
#Roster = rename(Roster, Deathdate ="DOD")
Roster = rename(Roster, DOB ="DOB")

## add new columns to the roster
Roster$OCME_RPT <-"YES"
Roster$SFNa <-NA


Roster$Outcome <-"DIED"
Roster$DieWith <- "YES"
Roster$Minitial <- NA

Roster$HospitalAccess <- NA
Roster$Suffix <- NA

# Enter the EventIDs into Roster and comparison file
Roster$EventID = results$EventID
OCME_newdeaths$EventID = results$EventID

Roster$DiseaseStatus = results$MatchStatus

# change lower case to all capitals for Roster
Roster$Sex = str_to_upper(Roster$Sex)
Roster$Race = str_to_upper(Roster$Race)
Roster$Ethnicity = str_to_upper(Roster$Ethnicity)
Roster$DiseaseStatus = str_to_upper(Roster$DiseaseStatus)

##############################################################################
################ update the hospitals with the ID#############################

# extract hospital codes/key from hospital reference file
hosp_table = read.csv("L:/daily_reporting_figures_rdp/DeathRostering/hosp_file.csv")

# subset to code match
hosp_list = hosp_table$name_hosp

# subset to raw hosp
hosp_raw = Roster$PronouncedBy

# match raw cities to approved list
match_inds = amatch(hosp_raw,hosp_list,maxDist=8)
# extract matches
hosp_match = hosp_list[match_inds]

# extract keys
key_match = hosp_table$key_hosp[match_inds]
Roster$HospitalAccess = key_match

# delete any false event IDs because they do not MATCH
false_ind = grep("FALSE",Roster$MATCH)
if (length(false_ind)>0){
  Roster$EventID[false_ind]=NA}

# put in the correct order of columns
Roster2 <- Roster[ ,c(24,2,17,1,18,3,19,20,5,4,21,23,9,10,8,16,22,7,13,11,6)]
######################################################################################
# save the file in the correct order and save it as csv for rostering

###save the file for upload to CTEDSS

dir.create(paste0('L:/daily_reporting_figures_rdp/DeathRostering/', Sys.Date()))

write_csv(Roster2,
          paste0("L:/daily_reporting_figures_rdp/DeathRostering/", Sys.Date(), "/", Sys.Date(),"Roster.csv"), na="")

