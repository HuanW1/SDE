####0 loading libraries ####
if(!dir.exists("L:/")) message("You need to have L drive mapped")

DPH_packages <- c( "tidyverse", "lubridate", "stringr",
                   "DBI", "odbc", "formatR", "knitr", "MMWRweek","stringdist")

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

####1 data read-in####
newGEO <- data 

if(nrow(newGEO)<1){
  stop("No data to review.")
}

#FLIS roster read-in
FLISfiles <- list.files("L:/FLIS")
maxflisdate <- format(max(lubridate::mdy(str_sub(FLISfiles, 2, 11)), na.rm = TRUE), "%m-%d-%Y")
FLIS <-read_csv(paste0("L:/FLIS/_", maxflisdate, "_FLIS_EXTRACT.csv")) %>% 
  mutate(name = paste(FirstName, LastName))
# FLIS <-  read_csv("L:/FLIS/testing/_02-12-2021_FLIS_Extract.csv")%>%
# mutate(name = paste(FirstName, LastName))

####2 Race and Ethnicity recoding ####
newGEO <- newGEO %>% 
  mutate(
    race = if_else(race == "Black", "Black or African American", race),
    race = if_else(race == "Multiracial" , "Other", race),
    hisp = if_else(hisp == "NH", "NO", "YES")
  ) %>% 
  mutate(name = paste(fname, lname))

#####3 Matching ####

#  find nearest match in name
match_inds=amatch(newGEO$name,FLIS$name,maxDist=100)

#first part matches on name then double checks their dob
# construct a results dataframe
results <- tibble(
  eventid = newGEO$eventid,
  Raw_Name = newGEO$name,
  Roster_Name = FLIS$name[match_inds],
  Raw_DOB = newGEO$dob, 
  Roster_DOB = FLIS$DOB[match_inds]
) %>% 
  mutate(
    Roster_DOB = lubridate::ymd(Roster_DOB),
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

#this part matches on DOB then checks the matches names with the raw name + dob combo
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


odbc::dbDisconnect(con)

message("Part 3/4 finished- almost to the finish line!")
source("cong_setting_part4.R")