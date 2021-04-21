#### Module 5 / misc_requested_outputs ####
#This script will generate the COVID-19 reporting outputs needed for historical use cases and other stakeholders
message("Miscellaneous output process will now begin.  This usually takes X minutes")
#replaces chunk at line 3451 and chunk at linbe 3488

####0 libraries, connections and data, oh my ####
source("helpers/StartMeUp.R")
gary_con <- DBI::dbConnect(odbc::odbc(), "epicenter")
csv_write <- FALSE
SQL_write <- FALSE

#grab relevant test names, cases data, test data, and CHA data
source("helpers/testtypes.R")
source("helpers/Fetch_case.R")
source("helpers/Fetch_cha_c.R")

####1 Load Lookups and Dependancies ####
age_labels <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", ">=80")

gary_age_labels <- c("cases_age0_9", "cases_age10_19", "cases_age20_29", "cases_age30_39", "cases_age40_49", "cases_age50_59", "cases_age60_69", "cases_age70_79", "cases_age80_Older" )

statement <- paste0("SELECT * FROM [DPH_COVID_IMPORT].[dbo].[RPT_TOWN_CODES]")
city_file <-  DBI::dbGetQuery(conn = gary_con , statement = statement)

HospitalizedCases <- tibble(HospitalizedCases = cha_c$today[9])
dec <- case %>% filter(outcome == "Died") %>%  nrow()
counties <-c("Fairfield County", "Hartford County", "Litchfield County", "Middlesex County", "New Haven County", "New London County", "Tolland County", "Windham County")

#sets up rea tables and percents
#source("race_ethnicity/race_ethnicity_setup.R")

#state denoms
ct_rea_denoms <- county_rea_denoms %>%
  group_by(hisp_race) %>% 
  summarize(pop = sum(pop))

graphdate <- Sys.Date() - 1

#only keep what we need
case <- case %>% 
  select(c(bigID,eventid,disease_status,age,gender,street, city,county, state,zip_code, hisp_race,outcome, spec_col_date,date, age_group,death_date, cong_yn)) %>% 
  mutate(outcome = ifelse(is.na(outcome), "Survived", outcome)) 


#clear trash for race_ethnicity_setup
odbc::dbDisconnect(gary_con)
#rm(adj_tbl_long, adj_tbl_long_dec, agg_table, agg_table_dec, more_ages, spop2000)


####2 county7days.csv ####

county7days <-  case %>% 
  filter(date %in% seq.Date(Sys.Date()-7, Sys.Date(), by = "day")) 

if(nrow(county7days)>0){
  county7days <- case %>% 
    filter(!is.na(county) & county %in% counties) %>% 
    filter(date %in% seq.Date(Sys.Date()-7, Sys.Date(), by = "day")) %>% 
    group_by(county) %>% 
    tally(name = 'Cases in last 7 days') %>% 
    inner_join(case %>% filter(!is.na(county) & county %in% counties) %>% 
                 group_by(county) %>% tally(name = 'Total cases to date'),
               by = "county") %>% 
    rename(County = county)
}else{
  county7days<- tibble(
    County = counties,
    `Cases in last 7 days` = 0,
    `Total cases to date` = 0
  )  
}

if(csv_write){
  write_csv(county7days, paste0("L:/daily_reporting_figures_rdp/csv/", Sys.Date(), "/", "county7days.csv"))
}

####3 mastereventidlist.csv ####
TOI <- city_file$TOWN_LC
range_start <- floor_date(Sys.Date() - 12, unit = "week")
range_end <- range_start + 13

if(csv_write){
dir.create(paste0("L:/daily_reporting_figures_rdp/town_case_eventids/", Sys.Date()))
}

mastereventidlist <- case %>% 
  filter(date >= range_start & date <= range_end & !is.na(city) & city %in% city_file$TOWN_LC) %>%
  filter(cong_yn == "No") %>% 
  select(eventid, city) %>% 
  left_join(city_file %>% select(TOWN_LC, lhd), 
            by = c("city" = "TOWN_LC")) %>% 
  rename(`Health Department Name` = lhd) %>% 
  arrange(`Health Department Name`, city)

if(csv_write){
  write_csv(mastereventidlist, 
            path = paste0("L:/daily_reporting_figures_rdp/town_case_eventids/", 
                          Sys.Date(), "/", "mastereventidlist.csv"))
}
if(csv_write) {
  for(i in TOI){
    mastereventidlisti <- mastereventidlist %>%  
      filter(city == i) %>% 
      select(eventid)
    
    dir.create(paste0("L:/daily_reporting_figures_rdp/town_case_eventids/", 
                      Sys.Date(), "/", 
                      city_file$lhd[city_file$TOWN_LC == i]))  
    write_csv(mastereventidlisti, 
              path = paste0("L:/daily_reporting_figures_rdp/town_case_eventids/", 
                            Sys.Date(), "/", 
                            city_file$lhd[city_file$TOWN_LC == i],
                            "/", i, "last2week.csv"))  
  }
}
