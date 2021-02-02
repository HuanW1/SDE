.libPaths(c("L:/library", .libPaths()))

#GOAL: Create a dataset that Paula C. at Yale EIP can use to assess the timeliness and completeness of data flowing into ContaCT
#based on original ask from Leyla/Michelle Gillman to improve data timeliness and quality for ContaCT

#last updated 1/22/2021 by Chuck

require(tidyverse)
require(lubridate)
require(data.table)

#Set start date for dataset (beginning Sunday of the last complete week)
start_date <- ymd("2021-01-19")

#Read in data imported into ContaCT
readmycsv <- function(files) {
  read_csv(files, col_type = cols(
    Email = col_character(),
    `Address 1: Street 2` = col_character(),
    `Is the patient a healthcare worker?` = col_character(),
    `Name of congregate setting facility town` = col_character(),
    `CTEDSS ID` = col_character()
  ))
}



CTfiles <- list.files("W:/DosApps/EPI/EPISTAFF/2019-nCoV/ContaCT Data Quality/ContaCT_import_here/", ".csv")
# The files are "dirty" you'll get lots of warnings
CTfile.list <- sapply(paste0("W:/DosApps/EPI/EPISTAFF/2019-nCoV/ContaCT Data Quality/ContaCT_import_here/",
                             CTfiles),
                      readmycsv,
                      simplify=FALSE)

# importdt is messed up
CTimport <- rbindlist(CTfile.list, idcol="importdt", fill=TRUE)[, importdt:= substr(importdt, 104, 111)] %>%
  rename( street1 = `Address 1: Street 1`,
          street2 = `Address 1: Street 2`,
          city = `Address 1: City`,
          state = `Address 1: State/Province`,
          zip = `Address 1: ZIP/Postal Code`,
          eventid = `CTEDSS ID`) %>%
  mutate(spec_col_date = mdy(`Positive Test date`),
         Birthday = mdy(Birthday)) %>%
  select(-c("Positive Test date"))



#combine imports with missing datafile
ContaCT <- CTimport %>%
  #left_join(MissImport, by="eventid") %>%
  mutate (
    #`Mobile Phone` = if_else(!is.na(lookedup), Phone, `Mobile Phone`),
        #  street1 = if_else(!is.na(lookedup), Street1, street1),
        # city = if_else(!is.na(lookedup), City, city),
        #  state = if_else(!is.na(lookedup), State, state),
        #  zip = if_else(!is.na(lookedup), ZIP, zip),
          EXPORT_DATETIME = mdy(importdt),
          bigID = str_to_lower(paste0(`First Name`, `Last Name`, Birthday)))
 # select(-c("Phone", "Street1", "City", "State", "ZIP")) #, "importdt"))


#elrline list from daily report
elrfiles <-
  list.files("W:/DosApps/EPI/EPISTAFF/2019-nCoV/ContaCT Data Quality/lab_data_here/",
             ".csv")

# > 5 millions rows go get coffee
# This needs a good hard look lots and lots of errors here
elr4paula <- read_csv(paste0("W:/DosApps/EPI/EPISTAFF/2019-nCoV/ContaCT Data Quality/lab_data_here/", elrfiles)) %>%
  mutate(bigID = str_to_lower(paste0(fname, lname, dob)),
         result2=ifelse(result=="detected", "Positive",
                        ifelse(result=="not detected", "Negative", "Indeterminate")))


#### Alison need accurate column names
# there is no create_date column
# select desired variables
elr4paula2 <- elr4paula %>%
  filter(result2 == "Positive") %>%
  select(eventid, bigID, case_create_date, new_elr_result, event_date, age, gender,
         lab_result_create_date,  lab_result_mod_date, mrn_elr, lab_facility, lab_name, spec_rec_date,
         spec_col_date, date_tested, date_reported_dph, spec_num, source, test_method, result, auth_facility, ordering_provider_name) %>%
  mutate(disease_status = "Confirmed",
         eventid = as.character(eventid),
         elr = 1) %>%
  rename(spec_source = source,
         Gender = gender,
         EventAgeYears = age,
         `Event Date`=event_date)

#match by eventid & specimen collection date
contactpull2 <- ContaCT %>%
  inner_join(elr4paula2, by = c("eventid", "spec_col_date")) %>%
  select(-c("bigID.y")) %>%
  rename(bigID = bigID.x)

#match by event ID without spec collection date (because some are missing)
elr4paula3 <- elr4paula2 %>%
  group_by(eventid) %>%
  arrange(desc(spec_col_date)) %>%
  slice(1L)

contactpull3 <- ContaCT %>%
  filter(is.na(spec_col_date) & !eventid %in% contactpull2$eventid) %>%
  inner_join(elr4paula3, by = "eventid") %>%
  select(-c("bigID.y")) %>%
  rename(bigID = bigID.x) %>%
  mutate(elr=2)

#rematch based on name + DOB in case event ID has changed
elr4paula4 <- elr4paula2 %>%
  filter(! eventid %in% contactpull2$eventid) %>%
  mutate(elr=3)

contactpull4 <- ContaCT %>%
  filter(!eventid %in% contactpull2$eventid & !eventid %in% contactpull3$eventid) %>%
  select(-c("eventid")) %>%
  inner_join(elr4paula4, by = c("bigID", "spec_col_date"))

#pull together records matched on event ID and name + DOB
contactpull5 <- bind_rows(contactpull2, contactpull3, contactpull4)
count(contactpull5, elr)

#check not many that end up in the dataset twice - should be only in twice if 2 legit specimens on same date & both positive
cts <- count(contactpull5, eventid) %>%
  filter(n > 1)

#select cases imported for most recent 7 days
contactpull6 <- filter(contactpull5, EXPORT_DATETIME >= start_date)
#look at how many matched by event id (1) and name + DOB (2)
count(contactpull5, elr)
cts2 <- count(contactpull6, eventid) %>%
  filter(n > 1)

#drop variables not needed for Paula
contactpull7 <- contactpull6 %>%
  mutate(spec_col_date = if_else(is.na(spec_col_date), ymd(spec_col_date.y), spec_col_date)) %>%
  select(-c("elr", "importdt", "X20", "bigID", "Email", "Is the patient a healthcare worker?", "Name of congregate setting facility",
            "Name of congregate setting facility Town", "Type of Congregate Setting", "Outcome", "What was their exposure to the congregate setting?", 'spec_col_date.x', 'spec_col_date.y'))
# write_csv(contactpull7, paste0("data_4_paula/", Sys.Date(), "ContaCTdata.csv"))
write_csv(contactpull7, paste0("ContaCTdata.csv"))

#cases not in matched dataset that should be (if <100 records, ignore)
ContaCT2 <- filter(ContaCT, EXPORT_DATETIME >= start_date) %>%
  filter(!bigID %in% contactpull5$bigID & !eventid %in% contactpull5$eventid)


#how many imported into contaCT during period of interest
count(ContaCT2)





