
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
con <- DBI::dbConnect(odbc::odbc(), "epicenter")

# declare data file for reading
if(exists("data")){
  newGEO <- data %>%  filter(KEEP==1)
} else{
  statement <- paste0("SELECT * FROM DPH_COVID_IMPORT.dbo.CONG_DATERAN")
  lastdate <-  DBI::dbGetQuery(conn = con , statement = statement) %>% 
    as_tibble() %>% 
    mutate(DateRan = lubridate::ymd(DateRan)) %>% 
    arrange(desc(DateRan)) %>% 
    slice(1L) %>% 
    pull(DateRan)
  newGEO <- data.table::fread(paste0("L:/daily_reporting_figures_rdp/csv/",lastdate, "/",lastdate,"CONG_past14daysdelta.csv"), data.table = FALSE) %>% 
    filter(KEEP==1)
}

if(nrow(newGEO)<1){
  stop("No data to review. Check past14daysdelta file.")
}

#########Need to recode race from Black to "Black or African American" / Multiracial to "Other" / and Unknowns to a blank/missing

# for Black in Roster$Race, enforce as "Black or African American"
black_inds=grep("Black",newGEO$race,ignore.case=TRUE)
if (length(black_inds)>0){newGEO$race[black_inds]="Black or African American"}

# for Black in Roster$Race, enforce as "Black or African American"
multi_inds=grep("Multi+",newGEO$race,ignore.case=TRUE)
if (length(multi_inds)>0){newGEO$race[multi_inds]="Other"}


# if hisp includes 'NH', then Ethnicity should be 'No'
newGEO$hisp=gsub("NH","NO",newGEO$hisp)

# if hisp includes 'H', then Ethnicity should be 'YES'
newGEO$hisp=gsub("H","YES",newGEO$hisp)

##############################################################


### filter on DOC and ALF
RosterDOCALF<- newGEO %>% filter(type == "DOC" | type =="Assisted Living") %>% 
  select(eventid, age, lname, fname, dob, gender, race, hisp, dba, type, name)

### filter not DOC and ALF
RosterFLISck<- newGEO %>% filter(type != "DOC" & type !="Assisted Living") %>% 
  select(eventid, age, dba, Name, street, city, gender, race, hisp,  type,lname, fname, dob)


#######Need to recode hispanic from H or NH to Yes, No)
#####match the records not DOC and ALF to the FLIS list

### import the new running geocode
#hopefully change this to a SQL pull
read_file="LTCFDailySubmission_Residents Jan25_2021.csv"
# read-in neFLIS list
FLIS_Raw=read.csv(read_file)



#######FILTER so only the records in the FLIS list where "Transferred = No" are included in the below matching
FLIS=subset(FLIS_Raw, (Transferred=="No"))





#############################################
#

# step 1: match names
# 1a. load library
library(stringdist) 		# amatch, stringdist
RosterFLISck$name = paste(RosterFLISck$fname, RosterFLISck$lname)
FLIS$name = paste(FLIS$First.Name, FLIS$Last.Name)

# 1b. find nearest match in name
match_inds=amatch(RosterFLISck$name,FLIS$name,maxDist=100)

# 1c. construct a results matrix
results=data.frame(RosterFLISck$name,FLIS$name[match_inds],RosterFLISck$dob, FLIS$DOB[match_inds])
names(results)=c("Raw_Name","Match_Name","Raw_DOB","Match_DOB")

# flag people with multiple

# 1d. enforce classes
results$Raw_DOB=as.Date(results$Raw_DOB,format="%m/%d/%Y")
results$Match_DOB=as.Date(results$Match_DOB,format="%m/%d/%Y")

# 1d. add string distance
results$Name_Dist=stringdist(results$Raw_Name,results$Match_Name)
# 1e. add date distance
results$DOB_Dist=as.numeric(abs(results$Raw_DOB-results$Match_DOB))


year(results$Raw_DOB)
month(results$Raw_DOB)
day(results$Raw_DOB)

results$BDay_Match=month(results$Raw_DOB)==month(results$Match_DOB)
results$BDay_Match=results$BDay_Match+(day(results$Raw_DOB)==day(results$Match_DOB))
results$BDay_Match=results$BDay_Match+(year(results$Raw_DOB)==year(results$Match_DOB))

#results$EventID=AllCases$eventid[match_inds]
# initialize results
results$dobMatch_name=""
results$dobMatch_dist=-1
# for each record in the roster
for (i in 1:nrow(RosterFLISck)){
  # extract the ith date of birth and name
  temp_dob=RosterFLISck$DOB[i]
  temp_name=RosterFLISck$name[i]
  # find all records with same dob
  match_dobs=which(as.Date(FLIS$DOB)==as.Date(temp_dob,format="%m/%d/%Y"))
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
RosterFLISck$MATCH=results$DOB_Dist<3652 & results$BDay_Match>1 & results$Name_Dist<5
#and statement code
RosterFLISck$BDay_Match=results$BDay_Match
RosterFLISck$Match_Name=results$Match_Name


################################
TrueFLISmatch=subset(RosterFLISck, MATCH=="TRUE") %>% 
  select(eventid, age, lname, fname, dob, gender, race, hisp, DBA, type, Name)

# make type say LTCF
TrueFLISmatch$type <-"LTCF"

#make temp of DOCALF
temp_DOCALF=RosterDOCALF

# rename DBA
TrueFLISmatch = rename(TrueFLISmatch, 'Facility Name' ="DBA")
TrueFLISmatch = rename(TrueFLISmatch, 'Race' = "race")
TrueFLISmatch = rename(TrueFLISmatch, 'Hispanic' ="hisp")
TrueFLISmatch = rename(TrueFLISmatch, 'Gender' ="gender")
TrueFLISmatch = rename(TrueFLISmatch, 'Type of congregate setting' ="type")
TrueFLISmatch = rename(TrueFLISmatch, 'Event ID' ="eventid")
temp_DOCALF = rename(temp_DOCALF, 'Facility Name' ="DBA")
temp_DOCALF = rename(temp_DOCALF, 'Type of congregate setting' ="type")
temp_DOCALF = rename(temp_DOCALF, 'Race' = "race")
temp_DOCALF = rename(temp_DOCALF, 'Hispanic' ="hisp")
temp_DOCALF = rename(temp_DOCALF, 'Gender' ="gender")
temp_DOCALF = rename(temp_DOCALF, 'Event ID' ="eventid")

# stack Rosters together
FinalRosterMerge <- rbind(TrueFLISmatch, temp_DOCALF)

# change lower case to all capitals for Roster
FinalRosterMerge$'Facility Name' = str_to_title(FinalRosterMerge$'Facility Name')

# for remaining blank elements in Facility, enforce as Name variable info
empt_inds=which(is.na(FinalRosterMerge$`Facility Name`))
if (length(empt_inds)>0){FinalRosterMerge$'Facility Name'[empt_inds]=FinalRosterMerge$Name[empt_inds]}

# if type of cong setting includes 'AL', then it should be 'ALF'
doc_inds=grep("Assisted Living",FinalRosterMerge$`Type of congregate setting`,ignore.case=TRUE)
if (length(doc_inds)>0){FinalRosterMerge$`Type of congregate setting`[doc_inds]="ALF"}

##### add new variables that repeat in roster

## add new columns to the roster
FinalRosterMerge$'Congregate Setting'<-"YES"
FinalRosterMerge$'Facility Number'<-NA
FinalRosterMerge$Product <-"CORONA"
FinalRosterMerge$'Type of CoV' <- "2019_NCOV"
FinalRosterMerge$NMI<-"YES"
FinalRosterMerge$State<-"CT"

# put the variables in this order

FinalRosterMergeforCTEDSS <- FinalRosterMerge[ ,c(1,13,7,8,6,9,17,12,10,14,15,16)]


write.table(FinalRosterMergeforCTEDSS,"FinalRosterMergeforCTEDSS.csv",na="",
            row.names=FALSE,col.names=TRUE,sep=",")


##########################

# filter people over 65 years old
RosterUnder65=subset(RosterFLISck, (age<65) & (MATCH==FALSE))
# filter people 64 and under and FALSE MATCHES = manually review to find location
RosterOver65=subset(RosterFLISck, (age>64) & (MATCH == FALSE))

####################################  create the NEW file for staff manual review rostering into CTEDSS


###Create the file for upload to CTEDSS
write.table(RosterUnder65,"1_19_RosterUnder65.csv",na="",
            row.names=FALSE,col.names=TRUE,sep=",")


#################################### 


#####For the Falses on the RosterOver65 I need to check which congregate type they belong to ALF or LTCF









write.table(RosterOver65,"1_19_RosterOVer65.csv",na="",
            row.names=FALSE,col.names=TRUE,sep=",")

#################################### 









##########Need to reorder and complete the roster file fields



#########################################









odbc::dbDisconnect(con)



