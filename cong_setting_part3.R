
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
if(exists("data")){
  newGEO <- data #%>%  filter(KEEP==1)
} else{
  statement <- paste0("SELECT * FROM DPH_COVID_IMPORT.dbo.CONG_DATERAN")
  lastdate <-  DBI::dbGetQuery(conn = con , statement = statement) %>% 
    as_tibble() %>% 
    mutate(DateRan = lubridate::ymd(DateRan)) %>% 
    arrange(desc(DateRan)) %>% 
    slice(1L) %>% 
    pull(DateRan)
  newGEO <- data.table::fread(paste0(lastdate,"CONG_past14daysdelta.csv"), data.table = FALSE) #%>% 
    #filter(KEEP==1)
}

if(nrow(newGEO)<1){
  stop("No data to review. Check past14daysdelta file.")
}

#FLIS roster readin
FLISfiles <- list.files("L:/FLIS")
maxflisdate <- format(max(lubridate::mdy(str_sub(FLISfiles, 2, 11))), "%m-%d-%Y")
FLIS <-read_csv(paste0("L:/FLIS/_", maxflisdate, "_FLIS_EXTRACT.csv"))


####Race and Ethnicity recoding ####
newGEO <- newGEO %>% 
  mutate(
    race = if_else(race == "Black", "Black or African American", race),
    race = if_else(race == "Multiracial" , "Other", race),
    hisp = if_else(hisp == "NH", "NO", "YES")
  )
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
newGEO <- newGEO %>% 
  mutate(name = paste(fname, lname))
FLIS <- FLIS %>% 
  mutate(name = paste(FirstName, LastName))
#FLIS$name = paste(FLIS$First.Name, FLIS$Last.Name)

# 1b. find nearest match in name
match_inds=amatch(newGEO$name,FLIS$name,maxDist=100)

# 1c. construct a results matrix
results <- tibble(
  eventid = newGEO$eventid,
  Raw_Name = newGEO$name,
  Match_Name = FLIS$name[match_inds],
  Raw_DOB = newGEO$dob, 
  Match_DOB = FLIS$DOB[match_inds]
) %>% 
  mutate(
    Match_DOB = lubridate::ymd(Match_DOB),
    Raw_DOB = lubridate::ymd(Raw_DOB),
# add string distance
    Name_Dist = stringdist(Raw_Name,Match_Name),
# add date distance
    DOB_Dist = as.numeric(abs(Raw_DOB-Match_DOB)),
    BDay_Match = month(Raw_DOB) == month(Match_DOB),
    BDay_Match = BDay_Match + (day(Raw_DOB)==day(Match_DOB)),
    BDay_Match = BDay_Match+(year(Raw_DOB)==year(Match_DOB)),
    dobMatch_name = "",
    dobMatch_dist = -1
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
  match_dobs=which(lubridate::ymd(FLIS$DOB)==temp_dob)
  # find all names with same dob
  match_names=FLIS$name[match_dobs]
  # find best-match name to this dob
  match_idx=amatch(temp_name,match_names,maxDist=100)
  match_name=match_names[match_idx]
  # determine string distance of dob-matched name
  match_dist=stringdist(temp_name,match_name)
  # take note of results
  results$dobMatch_name[i]=match_name
  results$dobMatch_dist[i]=match_dist
} # end-for (iteration over roster)

#view the matches in the table
results = results

# check the names and add matching variables to FLIS check list
#or statement code
newGEO$MATCH=results$DOB_Dist<365 & results$BDay_Match>1 & results$Name_Dist<5
#and statement code
newGEO$BDay_Match=results$BDay_Match
newGEO$Match_Name=results$Match_Name





odbc::dbDisconnect(con)



