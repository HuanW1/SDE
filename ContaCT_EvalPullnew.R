.libPaths(c("L:/newlib", .libPaths()))

# GOAL: Create a dataset that Paula C. at Yale EIP can use to assess the
# timeliness and completeness of data flowing into ContaCT
# based on original ask from Leyla/Michelle Gillman to improve data
# timeliness and quality for ContaCT

#last updated 3/5/2021 by Chuck

require(tidyverse)
require(lubridate)
# require(data.table)

Sys.time()
# Set start date for dataset (beginning Sunday of the last complete week)
# start_date <- ymd("2021-01-19")

#### better option
prev.days <- seq(today() - 20, today(), by = 'day')
start_date <- prev.days[weekdays(prev.days) == 'Sunday'][1]

magic_directory <- "W:/DosApps/EPI/EPISTAFF/2019-nCoV/ContaCT Data Quality/ContaCT_import_here/"

# Get a list of all csv files in specified directory
all_possible_files <-
  list.files(magic_directory,
             ".csv")

# Extract just the files after start_date
# Use separate() to grab the two dates in the filename
# apply filter() to get dates we want
# paste0 the full path back on
# use deframe() to get a named list of files with the date as a name

relevant_files <-
  all_possible_files %>%
  as_tibble %>%
  tidyr::separate(col = value,
                  into = c(NA, NA, NA, NA, "Date1", NA, "Date2", NA, NA, NA),
                  sep = "_|\\.", remove = FALSE, fill = "right") %>%
  mutate(across(starts_with("Date"), mdy)) %>%
  arrange(desc(Date1)) %>%
  filter(Date1 > start_date) %>%
  select(Date2, value) %>%
  mutate(value = paste0(magic_directory, value)) %>%
  deframe()

# Take our list of relevant files and build a dataframe
# Make all columns type char
# Since the list is named add a column called importdt with the date
# Reduce it down with map_df() to one big tibble
CTimport <-
  purrr::map_df(relevant_files,
                ~ read_csv(.x,
                           col_types = list(.default = col_character())),
                .id = "importdt")

# Rename some stuff, make spec_col_date & Birthday real dates
ContaCT <-
  CTimport %>%
  rename( street1 = `Address 1: Street 1`,
          street2 = `Address 1: Street 2`,
          city = `Address 1: City`,
          state = `Address 1: State/Province`,
          zip = `Address 1: ZIP/Postal Code`,
          eventid = `CTEDSS ID`) %>%
  mutate(spec_col_date = mdy(`Positive Test date`),
         Birthday = mdy(Birthday),
         EXPORT_DATETIME = ymd(importdt),
         bigID = str_to_lower(paste0(`First Name`,
                                     `Last Name`,
                                     Birthday))) %>%
  select(-c("Positive Test date"))

# View(ContaCT)

# Make a list of possible elr_linelists to choose from
possibilities <-
  list.files(path = "l:/recent_rdata/",
             pattern = "elr_linelist_",
             full.names = TRUE)

# Figure out which is latest
which_one <-
  as.data.frame(possibilities) %>%
  cbind(as.data.frame(str_split(possibilities,
                                pattern = "_|\\.",
                                simplify = TRUE))) %>%
  select(-V1, -V2, -V3, justdate = V4, -V5) %>%
  mutate(justdate = as_datetime(justdate)) %>%
  filter(justdate == max(justdate)) %>%
  pull(possibilities)

# load the latest
load(which_one)

# filter the elr down to just result=="detected"
# select needed columns goes from ~7M to ~350k rows
elr4paula <-
  elr_linelist %>%
  filter(result=="detected") %>%
  select(eventid, bigID, case_create_date, new_elr_result, event_date, age, gender,
         lab_result_create_date,  lab_result_mod_date, mrn_elr, lab_facility, lab_name, spec_rec_date,
         spec_col_date, date_tested, date_reported_dph, spec_num, source, test_method,
         result, auth_facility, ordering_provider_name) %>%
  mutate(disease_status = "Confirmed",
         eventid = as.character(eventid),
         elr = 1) %>%
  rename(spec_source = source,
         Gender = gender,
         EventAgeYears = age,
         `Event Date` = event_date)

# throw away original linelist recover memory
rm(elr_linelist)
gc()

#### From here on it's pretty much untouched ####
### not at all sure the comments are accurate ###
contactpull2 <- ContaCT %>%
  inner_join(elr4paula, by = c("eventid", "spec_col_date")) %>%
  select(-c("bigID.y")) %>%
  rename(bigID = bigID.x)

#match by event ID without spec collection date (because some are missing)
elr4paula3 <- elr4paula %>%
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
elr4paula4 <- elr4paula %>%
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

#select cases imported for most recent 7 days ????
contactpull6 <- filter(contactpull5, EXPORT_DATETIME >= start_date)

#look at how many matched by event id (1) and name + DOB (2)
count(contactpull5, elr)
cts2 <- count(contactpull6, eventid) %>%
  filter(n > 1)

#drop variables not needed for Paula
contactpull7 <-
  contactpull6 %>%
  mutate(spec_col_date = if_else(is.na(spec_col_date), ymd(spec_col_date.y), spec_col_date)) %>%
  select(-c("elr", "importdt", "bigID", "Email", "Is the patient a healthcare worker?",
            "Name of congregate setting facility",
            "Name of congregate setting facility Town", "Type of Congregate Setting", "Outcome",
            "What was their exposure to the congregate setting?", 'spec_col_date.x', 'spec_col_date.y'))

write_csv(contactpull7, paste0("W:/DosApps/EPI/EPISTAFF/2019-nCoV/ContaCT Data Quality/data_4_paula/", Sys.Date(), "ContaCTdata.csv"))
#write_csv(contactpull7, paste0("W:/DosApps/EPI/EPISTAFF/2019-nCoV/ContaCT Data Quality/data_4_paula/"))

#cases not in matched dataset that should be (if <100 records, ignore)
ContaCT2 <- filter(ContaCT, EXPORT_DATETIME >= start_date) %>%
  filter(!bigID %in% contactpull5$bigID & !eventid %in% contactpull5$eventid)


#how many imported into contaCT during period of interest
count(ContaCT2)
Sys.time()



