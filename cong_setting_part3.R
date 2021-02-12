
#####created jan 9 2021 AK
#####modified Jan 14 2021 PG
#####modified Jan 20 2021 AK, PG
######FLIS Filter added Jan  2021 PG

####Working script for congregate setting data review and updates

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



######################## _02-10-2021_FLIS_Extract.csv


#### declare data file for reading ####
#if(exists("data")){
  newGEO <- data #%>%  filter(KEEP==1)
# } else{
#   statement <- paste0("SELECT * FROM DPH_COVID_IMPORT.dbo.CONG_DATERAN")
#   lastdate <-  DBI::dbGetQuery(conn = con , statement = statement) %>% 
#     as_tibble() %>% 
#     mutate(DateRan = lubridate::ymd(DateRan)) %>% 
#     arrange(desc(DateRan)) %>% 
#     slice(1L) %>% 
#     pull(DateRan)
#   newGEO <- data.table::fread(paste0(lastdate,"CONG_past14daysdelta.csv"), data.table = FALSE) #%>% 
#     #filter(KEEP==1)
# }

if(nrow(newGEO)<1){
  stop("No data to review. Check past14daysdelta file.")
}

#FLIS roster readin
FLISfiles <- list.files("L:/FLIS")
maxflisdate <- format(max(lubridate::mdy(str_sub(FLISfiles, 2, 11)), na.rm = TRUE), "%m-%d-%Y")
FLIS <-read_csv(paste0("L:/FLIS/_", maxflisdate, "_FLIS_EXTRACT.csv")) %>% 
  mutate(name = paste(FirstName, LastName))


####Race and Ethnicity recoding ####
newGEO <- newGEO %>% 
  mutate(
    race = if_else(race == "Black", "Black or African American", race),
    race = if_else(race == "Multiracial" , "Other", race),
    hisp = if_else(hisp == "NH", "NO", "YES")
  ) %>% 
  mutate(name = paste(fname, lname))
##############################################################

#####
# we need logic here over what lof to use, we also need to shore it up and collapse it
#probably create a computed LOF + other vars that we feed into, and if rows are a KEEP then these computed vars are filled in automatically.
#####



#######FILTER so only the records in the FLIS list where "Transferred = No" are included in the below matching
#FLIS=subset(FLIS_Raw, (Transferred=="No")) no transferred var in extract for some reason now?





#############################################
#

# step 1: match names
# 1a. load library

#RosterFLISck$name = paste(RosterFLISck$fname, RosterFLISck$lname)


#FLIS$name = paste(FLIS$First.Name, FLIS$Last.Name)

# 1b. find nearest match in name
match_inds=amatch(newGEO$name,FLIS$name,maxDist=100)

# 1c. construct a results dataframe
results <- tibble(
  eventid = newGEO$eventid,
  Raw_Name = newGEO$name,
  Roster_Name = FLIS$name[match_inds],
  Raw_DOB = newGEO$dob, 
  Roster_DOB = FLIS$DOB[match_inds]
) %>% 
  mutate(
    Roster_DOB = lubridate::mdy(Roster_DOB),
    Raw_DOB = lubridate::ymd(Raw_DOB),
# add string distance
    Name_Dist = stringdist(Raw_Name,Roster_Name),
# add date distance
    DOB_Dist = as.numeric(abs(Raw_DOB-Roster_DOB)),
    BDay_Check = month(Raw_DOB) == month(Roster_DOB),
    BDay_Check = BDay_Check + (day(Raw_DOB)==day(Roster_DOB)),
    BDay_Check = BDay_Check + (year(Raw_DOB)==year(Roster_DOB)),
    dobRoster_name = "",
    dobRoster_dist = -1
  )

#results$EventID=AllCases$eventid[match_inds]
# initialize results
# results$dobMatch_name=""
# results$dobMatch_dist=-1
# for each record in the roster
for (i in 1:nrow(newGEO)){
  # extract the ith date of birth and name
  temp_dob=newGEO$dob[i]
  temp_name=newGEO$name[i]
  # find all records with same dob
  match_dobs=which(lubridate::mdy(FLIS$DOB)==temp_dob)
  # find all names with same dob
  match_names=FLIS$name[match_dobs]
  # find best-match name to this dob
  match_idx=amatch(temp_name,match_names,maxDist=100)
  match_name=match_names[match_idx]
  # determine string distance of dob-matched name
  match_dist=stringdist(temp_name,match_name)
  # take note of results
  results$dobRoster_name[i]=match_name
  results$dobRoster_dist[i]=match_dist
} # end-for (iteration over roster)
results <- results %>% 
  mutate(Roster_Match = DOB_Dist<365 & BDay_Check>1 & Name_Dist<5) %>% 
  select(eventid, Roster_Match, BDay_Check, Roster_Name)

# check the names and add matching variables to FLIS check list
#or statement code
newGEO <- newGEO %>% 
  left_join(results, by = "eventid") %>% 
  select(c(1:21, name, Roster_Name, BDay_Check, intoms, disposition, Roster_Match, KEEP))

#

# join on eventid
# newGEO <- newGEO %>% 
#   mutate(
#     Roster_Match = results$DOB_Dist<365 & results$BDay_Check>1 & results$Name_Dist<5,
#     BDay_Check = results$BDay_Check
#   )

# newGEO$Roster_Match=results$DOB_Dist<365 & results$BDay_Check>1 & results$Name_Dist<5
# #and statement code
# newGEO$BDay_Match=results$BDay_Match
# newGEO$Match_Name=results$Match_Name





odbc::dbDisconnect(con)


source("cong_setting_part4.csv")
