
#####jan 6, now 16 2021 AK
#####jan 23 completed mostly with MW help

rm(list=ls())

# load libraries
library(readxl)			# read_excel
library(dplyr)			# filter
library(stringr)		# str_to_title
library(tidyr)

# set working directory
setwd("R:/CT DPH/Death Rostering")

###########################
## import the old OCME list
read_file="All DPH 2.4.csv"

# read-in old ocme data from file
oldOCME=read.csv(read_file)

### import the new OCME list
read_file="All DPH 2.5.csv"

# read-in new ocme data from file
newOCME=read.csv(read_file)

#subset on covid.death = yes
newOCME=subset(newOCME, subset=COVID.Death == "Yes")
oldOCME=subset(oldOCME, subset=COVID.Death == "Yes")

#### link on ocme id and only keep the new people for the day

OCME_new <-   
  anti_join(newOCME, oldOCME, by=c("OCME."))

#OCME_name <-   
#  anti_join(newOCME, oldOCME, by=c("Name"))

#OCME_new<-rbind(OCME_ident, OCME_name, by= c('OCME.','Name')) %>% 
#  group_by(OCME.) %>% 
#  summarise_all(funs(max))

#split Name into 2 variables
OCME_new <- separate(OCME_new, col = Name, into = c("lname", "fname"), sep = ",")

#split Race.Ethnicity into 2 variables
OCME_new <- separate(OCME_new, col = Race.Ethnicity, into = c("Race", "Ethnicity"), sep = ",")

# add more variables to the roster
Roster<- OCME_new %>% select(OCME., DOB, DOD, lname, fname, DateApproved, Residence, Sex, Race, Ethnicity, PronouncedBy)

# delete rows with labels or no data
elim_inds=which(Roster$DateApproved=="Name")
keep_inds=setdiff(1:nrow(Roster),elim_inds)
Roster=Roster[keep_inds,]

# delete rows with labels or no data
#elim_inds=which(results$Raw_Name=="NA OCME.")
#keep_inds=setdiff(1:nrow(results),elim_inds)
#results=results[keep_inds,]

# delete rows with labels or no data
elim_inds=which(OCME_new$OCME.=="Name")
keep_inds=setdiff(1:nrow(OCME_new),elim_inds)
OCME_new=OCME_new[keep_inds,]

# fix for any race
# if Race includes 'Hispanic', then Ethnicity should be 'YES'
race_inds=grep("Hispanic",Roster$Race,ignore.case=TRUE)
if (length(race_inds)>0){Roster$Ethnicity[race_inds]="YES"}

# if Ethnicity includes any other text, put it in Race
race_inds2=grep("YES",Roster$Ethnicity,ignore.case=TRUE)
if (length(race_inds2)>0){Roster$Race[race_inds2]="White"}

# for remaining blank elements in Roster$Ethnicity, enforce as "NO"
#empt_inds=which(is.na(Roster$Ethnicity))
#if (length(empt_inds)>0){Roster$Ethnicity[empt_inds]="NO"}

# for Black in Roster$Race, enforce as "Black or African American"
black_inds=grep("Black",Roster$Race,ignore.case=TRUE)
if (length(black_inds)>0){Roster$Race[black_inds]="Black or African American"}

#stop
#################################################################################
# step 1 is look at the the cases w PHI table
# might want to get the latest cases w PHI file

### import the cases file from CTEDSS
read_file="2021-02-04cases_wphi.csv"

# read-in data from file
AllCases=read.csv(read_file)

#rename the dob so you can link on that variable
AllCases = rename(AllCases, DOB ="dob")

# step 2 match names and dob
# 1a. load library
library(stringdist) 	
Roster$name = paste(Roster$fname, Roster$lname)
AllCases$name=paste(AllCases$fname, AllCases$lname)

# 1b. find nearest match in name
match_inds=amatch(Roster$name,AllCases$name,maxDist=100)

# 1c. construct a results matrix
results=data.frame(Roster$name,AllCases$name[match_inds],Roster$DOB, AllCases$DOB[match_inds], AllCases$disease_status[match_inds])
names(results)=c("Raw_Name","Match_Name","Raw_DOB","Match_DOB","MatchStatus")

# flag people with multiple

# 1d. enforce classes
results$Raw_DOB=as.Date(results$Raw_DOB,format="%m/%d/%Y")
results$Match_DOB=as.Date(results$Match_DOB,format="%Y-%m-%d")

# 1d. add string distance
results$Name_Dist=stringdist(results$Raw_Name,results$Match_Name)
# 1e. add date distance
results$DOB_Dist=as.numeric(abs(results$Raw_DOB-results$Match_DOB))

library(lubridate)
year(results$Raw_DOB)
month(results$Raw_DOB)
day(results$Raw_DOB)

results$BDay_Match=month(results$Raw_DOB)==month(results$Match_DOB)
results$BDay_Match=results$BDay_Match+(day(results$Raw_DOB)==day(results$Match_DOB))
results$BDay_Match=results$BDay_Match+(year(results$Raw_DOB)==year(results$Match_DOB))

results$EventID=AllCases$eventid[match_inds]

# initialize results
results$dobMatch_name=""
results$dobMatch_dist=-1

# for each record in the roster
for (i in 1:nrow(Roster)){
  # extract the ith date of birth and name and disease
  temp_dob=Roster$DOB[i]
  temp_name=Roster$name[i]
  # find all records with same dob
  match_dobs=which(as.Date(AllCases$DOB)==as.Date(temp_dob,format="%m/%d/%Y"))
  # find all names with same dob
  match_names=AllCases$name[match_dobs]
  # find best-match name to this dob
  match_idx=amatch(temp_name,match_names,maxDist=100)
  match_name=match_names[match_idx]
  # determine string distance of dob-matched name
  match_dist=stringdist(temp_name,match_name)
  # take note of results
  results$dobMatch_name[i]=match_name
  results$dobMatch_dist[i]=match_dist
} # end-for (iteration over roster)


# check the names and put the match in the tables
OCME_new$MATCH=results$BDay_Match>1 & results$Name_Dist<10
Roster$MATCH=results$BDay_Match>1 & results$Name_Dist<10
Roster$BDay_MATCH=results$BDay_Match
Roster$Name_Distance=results$Name_Dist
Roster$DiseaseStatus=results$MatchStatus

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
Roster$SFNa<-NA
Roster$Outcome <-"DIED"
Roster$DieWith <- "YES"
Roster$Minitial<-NA
Roster$HospitalAccess<-NA
Roster$Suffix<-NA

# Enter the EventIDs into Roster and comparison file
Roster$EventID=results$EventID
OCME_new$EventID=results$EventID
Roster$DiseaseStatus=results$MatchStatus

# change lower case to all capitals for Roster
Roster$Sex = str_to_upper(Roster$Sex)
Roster$Race = str_to_upper(Roster$Race)
Roster$Ethnicity = str_to_upper(Roster$Ethnicity)
Roster$DiseaseStatus=str_to_upper(Roster$DiseaseStatus)

##############################################################################
################ update the hospitals with the ID#############################

# declare ref files up-top
hosp_file="R:/CT DPH/Death Rostering/hosp_file.csv"

# extract hospital codes/key from hospital reference file
hosp_table=read.csv(hosp_file)

# subset to code match
hosp_list=hosp_table$name_hosp

# subset to raw hosp
hosp_raw=Roster$PronouncedBy

# match raw cities to approved list
match_inds=amatch(hosp_raw,hosp_list,maxDist=8)
# extract matches
hosp_match=hosp_list[match_inds]

# extract keys
key_match=hosp_table$key_hosp[match_inds]
Roster$HospitalAccess=key_match

# stop
########################################################################################
# Below here is where we want to fix the MATCH = FALSE data by looking in Master list
####################################################################################
# run the search again looking for only the FALSE = MATCH data
# step 1 is look at the the cases w PHI table
#read the file in (1.9 records)
read_file="Alison_Cases_1_13_2021.csv"

# read in the lookup.csv
con=file(read_file,open="r")
data=readLines(con)
close(con)
# cleans data of spurious slash characters
data=gsub("\"","",data)
# split on commas
data=str_split(data,",",simplify=TRUE)
# eliminate spurious first row
data=data[-1,]
# convert to data-frame; declare headers
data=as.data.frame(data)
names(data)=c("eventid","lname","fname","dob","extra")
# convert data (if you like...)
data$eventid=as.numeric(data$eventid)

#add extra code here to read in
AllCases=data

# subset the Roster file for only FALSE ones
RosterF = Roster %>% subset(MATCH==FALSE)

# step 2 match names and dob
# 1a. load library
library(stringdist) 	
RosterF$name = paste(RosterF$Fname, RosterF$Lname)
AllCases$name=paste(AllCases$fname, AllCases$lname)

# 1b. find nearest match in name
match_indsF=amatch(RosterF$name,AllCases$name,maxDist=100)

# 1c. construct a results matrix
resultsF=data.frame(RosterF$name,AllCases$name[match_indsF],RosterF$DOB, AllCases$dob[match_indsF])
names(resultsF)=c("Raw_Name","Match_Name","Raw_DOB","Match_DOB")

# flag people with multiple
# 1d. enforce classes
resultsF$Raw_DOB=as.Date(resultsF$Raw_DOB,format="%m/%d/%Y")
resultsF$Match_DOB=as.Date(resultsF$Match_DOB,format="%m/%d/%Y")
#resultsF$Match_DOB=as.Date(resultsF$Match_DOB,format="%Y-%m-%d")

# 1d. add string distance
resultsF$Name_Dist=stringdist(resultsF$Raw_Name,resultsF$Match_Name)
# 1e. add date distance
resultsF$DOB_Dist=as.numeric(abs(resultsF$Raw_DOB-resultsF$Match_DOB))

library(lubridate)
year(resultsF$Raw_DOB)
month(resultsF$Raw_DOB)
day(resultsF$Raw_DOB)

resultsF$BDay_Match=month(resultsF$Raw_DOB)==month(resultsF$Match_DOB)
resultsF$BDay_Match=resultsF$BDay_Match+(day(resultsF$Raw_DOB)==day(resultsF$Match_DOB))
resultsF$BDay_Match=resultsF$BDay_Match+(year(resultsF$Raw_DOB)==year(resultsF$Match_DOB))

#pass in the event IDS
resultsF$eventid=AllCases$eventid[match_indsF]
RosterF$EventID=resultsF$eventid

# check the names and put the match in the tables
#OCME_new$MATCH=resultsF$BDay_Match>1 & resultsF$Name_Dist<10
RosterF$MATCH=resultsF$BDay_Match>1 & resultsF$Name_Dist<10
RosterF$BDay_MATCH=resultsF$BDay_Match
RosterF$Name_Distance=resultsF$Name_Dist


#stop
################################################################################
# subset the Roster FALSE file for only NEW TRUE ones
RosterNewTrues = RosterF %>% subset(MATCH==TRUE)
RosterTrues = Roster %>% subset(MATCH==TRUE | is.na(MATCH))
RosterFalses = RosterF %>% subset(MATCH==FALSE | is.na(MATCH))

# remove the EventIDs from RosterFalses
RosterFalses$EventID=NA


# bind together true and false
RosterALL <- rbind(RosterTrues, RosterFalses, RosterNewTrues)

# put in the correct order of columns
Roster2 <- RosterALL[ ,c(24,2,17,1,18,3,19,20,5,4,21,23,9,10,8,16,22,7,13,11,6)]
######################################################################################
# save the file in the correct order and save it as csv for rostering

###save the file for upload to CTEDSS
write.table(Roster2,"Roster2.csv",na="",
            row.names=FALSE,col.names=TRUE,sep=",")


