#### Module 5 Setup ####
#This script will pull common files needed for all module 5 scripts
#this will be slotted in and run by run_mod5 script and replace the top of mod5 scripts when ready
message("Module 5 Setup process will now begin")
message("Common files used across all outputs will be loaded, be sure to clear up your RAM")

###0 libraries, connections and data, oh my ####
message("CSV and SQL writes are OFF")
csv_write <- FALSE
SQL_write <- FALSE

#grab relevant test names, cases data, test data, and CHA data
source("helpers/testtypes.R")
source("helpers/Fetch_case.R")
source("helpers/Fetch_ELR.R")
source("helpers/Fetch_cha.R")

####1 Load Lookups and Dependancies ####
#vectors
age_labels <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", ">=80")
gary_age_labels <- c("cases_age0_9", "cases_age10_19", "cases_age20_29", "cases_age30_39", "cases_age40_49", "cases_age50_59", "cases_age60_69", "cases_age70_79", "cases_age80_Older" )
counties <-c("Fairfield County", "Hartford County", "Litchfield County", "Middlesex County", "New Haven County", "New London County", "Tolland County", "Windham County")

#city lookup
city_file <- tibble(table_to_df("RPT_TOWN_CODES"))

#commonly used cha vector
HospitalizedCases <- tibble(HospitalizedCases = cha_c$today[9])

#sets up rea tables and percents
source("race_ethnicity/race_ethnicity_setup.R")

#state denoms
ct_rea_denoms <- county_rea_denoms %>%
  group_by(hisp_race) %>% 
  summarize(pop = sum(pop))

#date used for graphs and DateUpdated type of variables
graphdate <- Sys.Date() - 1

#only keep what we need from cases and elr
case <- case %>% 
  select(c(bigID,eventid,disease_status,age,gender,street, city,county, state,zip_code, hisp_race,outcome, spec_col_date,date, 
           age_group,death_date, cong_yn)) %>% 
  mutate(outcome = ifelse(is.na(outcome), "Survived", outcome)) 

elr_linelist <- elr_linelist %>% 
  select(c(eventid, bigID, city,county, test_method, result,spec_col_date, pcrag, spec_rec_date)) %>% 
  mutate(eventid = as.numeric(eventid))

#quick easy death count used throughout
dec <- case %>% filter(outcome == "Died") %>%  nrow()

message("Module 5 setup is now complete")