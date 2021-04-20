#### Module 5 / gary_odp_outputs ####
#This script will generate the COVID-19 reporting outputs needed for ODP and other stakeholders
message("Gary's ODP output process will now begin.  This usually takes 3.5 minutes")

####0 libraries, connections and data, oh my ####
source("helpers/StartMeUp.R")
gary_con <- DBI::dbConnect(odbc::odbc(), "epicenter")
csv_write <- FALSE
SQL_write <- FALSE

#grab relevant test names, cases data, test data, and CHA data
source("helpers/testtypes.R")
source("helpers/Fetch_case.R")
source("helpers/Fetch_ELR.R")
source("helpers/Fetch_cha_c.R")

####1 Load Lookups and Dependancies ####
age_labels <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", ">=80")

gary_age_labels <- c("cases_age0_9", "cases_age10_19", "cases_age20_29", "cases_age30_39", "cases_age40_49", "cases_age50_59", "cases_age60_69", "cases_age70_79", "cases_age80_Older" )

statement <- paste0("SELECT * FROM [DPH_COVID_IMPORT].[dbo].[RPT_TOWN_CODES]")
city_file <-  DBI::dbGetQuery(conn = gary_con , statement = statement)

HospitalizedCases <- tibble(HospitalizedCases = cha_c$today[9])
dec <- case %>% filter(outcome == "Died") %>%  nrow()

#sets up rea tables and percents
source("race_ethnicity/race_ethnicity_setup.R")

#state denoms
ct_rea_denoms <- county_rea_denoms %>%
  group_by(hisp_race) %>% 
  summarize(pop = sum(pop))

graphdate <- Sys.Date() - 1

#only keep what we need
case <- case %>% 
  select(c(bigID,eventid,disease_status,age,gender,street, city,county, state,zip_code, hisp_race,outcome, spec_col_date,date, age_group,death_date)) %>% 
  mutate(outcome = ifelse(is.na(outcome), "Survived", outcome)) 

elr_linelist <- elr_linelist %>% 
  select(c(eventid, bigID, city,county, test_method, result,spec_col_date))

#clear trash
odbc::dbDisconnect(gary_con)
rm(adj_tbl_long, adj_tbl_long_dec, agg_table, agg_table_dec, more_ages, spop2000)

####2 state_Result.csv ####
#these require case and dec and cha_c
state_Result <- case %>% 
  group_by(disease_status,outcome, age_group) %>% 
  tally() %>% 
  mutate(outcome = ifelse(is.na(outcome), "Survived", outcome)) 

state_Result <- tibble(`State` = "CONNECTICUT",
                       `LastUppdateDate` = graphdate,
                       TotalCases = sum(state_Result$n),
                       ConfirmedCases = sum(state_Result %>% filter(disease_status == "Confirmed") %>%  pull(n)),
                       ProbableCases = sum(state_Result %>% filter(disease_status == "Probable") %>%  pull(n)),
                       HospitalizedCases = HospitalizedCases,
                       TotalDeaths = sum(state_Result %>% filter(outcome == "Died") %>%  pull(n)),
                       ConfirmedDeaths = sum(state_Result %>% filter(disease_status == "Confirmed" & outcome == "Died") %>%  pull(n)),
                       ProbableDeaths = sum(state_Result %>% filter(disease_status == "Probable" & outcome == "Died") %>%  pull(n)),
                       ) %>% 
  bind_cols(case %>%
              filter(!is.na(age_group)) %>%
              group_by(age_group) %>%
              tally() %>%
              pivot_wider(names_from = age_group, values_from = n))

#printing
if (csv_write) {
  write_csv(state_Result, paste0("L:/daily_reporting_figures_rdp/gary_csv/", Sys.Date(), "/state_Result.csv"))
}
if (SQL_write) {
  df_to_table(state_Result, "ODP_state_Result", overwrite = TRUE, append = FALSE)
}
message("Table 1/11 complete, printed and pushed to SQL")
#clear trash
rm(state_Result)

####3 town_Result.csv ####

  town_cases <- case %>% 
    filter(!is.na(city) & city != "Not_available") %>% 
    group_by(city, disease_status,outcome) %>% 
    tally() %>%
    rename(Town = city) %>% 
    ungroup() %>% 
    complete(Town = city_file$TOWN_LC, outcome = c("Died", "Survived"),disease_status = c("Confirmed", "Probable"), fill = list(n = 0)) %>% 
    left_join(city_file %>% select(TOWNNO, TOWN_LC,pop_2019),
                by = c("Town" = "TOWN_LC")) %>% 
    group_by(Town) %>% 
    mutate(TownTotalCases = sum(n),
           TownCaseRate = round((TownTotalCases/pop_2019)*100000)) %>% 
    ungroup() %>% 
    pivot_wider(id_cols = c(Town, outcome, disease_status,TownTotalCases, TownCaseRate, TOWNNO ), names_from = c(outcome, disease_status), values_from = n) %>% 
    mutate(TownTotalDeaths = Died_Confirmed + Died_Probable,
           LastUpdateDate = graphdate,
           TownConfirmedCases = Died_Confirmed + Survived_Confirmed,
           TownProbableCases = Died_Probable + Survived_Probable) %>% 
  rename(Town_No = TOWNNO,
         TownConfirmedDeaths = Died_Confirmed,
         TownProbableDeaths = Died_Probable) %>% 
  select(Town_No, Town, LastUpdateDate, TownTotalCases, TownConfirmedCases, TownProbableCases, TownTotalDeaths, TownConfirmedDeaths, TownProbableDeaths, TownCaseRate)



#creating test counts by town and cleaning up names
town_tests <- elr_linelist
town_tests$result[town_tests$result == "detected"] <- "Positive"
town_tests$result[town_tests$result == "not detected"] <- "Negative"
town_tests$result[town_tests$result == "indeterminate"] <- "Indeterminate"

#grouping and tallying test by town then pivoting it to the expected wide format
town_tests <- town_tests %>% 
  filter(!is.na(city) & !city %in% c("Not_available", "Pending validation")) %>% 
  group_by(city, result) %>%
  tally() %>%
  pivot_wider(id_cols = city, names_from = result, values_from = n) %>%
  replace_na(replace = list(Positive = 0, Negative= 0, Indeterminate = 0)) %>%
  mutate(NumberofTests=sum(Positive, Negative, Indeterminate, na.rm=TRUE)) %>%
  rename(NumberofPositives = Positive,
         NumberofNegatives = Negative,
         NumberofIndeterminates = Indeterminate,
         Town = city)

#Number of people tested by town
town_people<- elr_linelist %>% 
  filter(!city %in% c("Not_available", "Pending validation") & !is.na(city) & !is.na(spec_col_date) & city %in% city_file$TOWN_LC) %>%
  select(bigID, result, spec_col_date, city) %>% 
  mutate(simple_result = ifelse(result == "detected", 1, 2)) %>%
  group_by(bigID) %>%
  arrange(simple_result,spec_col_date) %>%
  slice(1L) %>%
  ungroup() %>% 
  rename(Town = city, SpecimenCollectionDate = spec_col_date) %>% 
  group_by(Town) %>% 
  tally(name = "PeopleTested") %>% 
  mutate(Town = str_to_title(Town)) %>% 
  left_join(city_file %>% select(TOWN_LC,pop_2019),
            by = c("Town" = "TOWN_LC")) %>% 
  mutate(RateTested100k = round(PeopleTested/pop_2019 * 100000)) %>% 
  select(-pop_2019)

#binding all the columns into gary_town_file
town_Result <- town_cases %>% 
  inner_join(town_tests, by = "Town") %>% 
  inner_join(town_people, by = "Town")

#printing
 if (csv_write) {
   write_csv(town_Result, paste0("L:/daily_reporting_figures_rdp/gary_csv/", Sys.Date(), "/town_Result.csv"))
 }
 if (SQL_write) {
   df_to_table(town_Result, "ODP_town_Result", overwrite = TRUE, append = FALSE)
 }
message("Table 2/11 complete, printed and pushed to SQL")
#clear trash
rm(town_total_cases,town_confirmed_cases,town_probable_cases, town_total_deaths, town_confirmed_deaths, town_probable_deaths, TownCaseRate, peopletestbytown, town_tests)#,town_Result

####4 ConProbByDate.csv ####
#but first, epicurve
epicurve <- case %>% filter(date >= "2020-03-01" & date < Sys.Date()) %>% 
  group_by(disease_status, date) %>% 
  tally() %>% 
  filter(!is.na(date)) %>% 
  rename(type = disease_status) %>% 
  mutate(type = factor(type, levels = c("Probable", "Confirmed"), labels = c("Probable", "Confirmed"))) %>% 
  arrange(date)

# Confirmed and Probable cases by date
ConProbByDate <- epicurve %>%
  filter(!is.na(date) & date >= "2020-03-05") %>%
  rename(Date = date, CaseCount = n ) %>% 
  pivot_wider(id_cols = Date, names_from = type, values_from = CaseCount) 
ConProbByDate[is.na(ConProbByDate)] <- 0
ConProbByDate <- ConProbByDate %>% 
  mutate(Total =  Confirmed + Probable)

#printing
if (csv_write) {
  write_csv(ConProbByDate, paste0("L:/daily_reporting_figures_rdp/gary_csv/", Sys.Date(), "/ConProbByDate.csv"))
}
if (SQL_write) {
  df_to_table(ConProbByDate, "ODP_ConProbByDate", overwrite = TRUE, append = FALSE)
}
message("Table 3/11 complete, printed and pushed to SQL")
####5 GenderSummary.csv####
#set up the lookup to be gender only
gender_tots <- county_rea_denoms %>% 
  group_by(gender) %>% 
  summarize(n=sum(pop)) %>% 
  mutate(gender = ifelse(gender == "M","Male","Female"))

GenderSummary <- case %>%
  filter(!is.na(gender) & !gender %in% c("Unknown", "Other")) %>% 
  group_by(gender, disease_status,outcome) %>% 
  tally() %>% 
  ungroup() %>%
  pivot_wider(id_cols = c(gender, disease_status, outcome), names_from = c(disease_status, outcome), values_from = n) %>% 
  mutate(ConfirmedCases = Confirmed_Survived + Confirmed_Died,
         ProbableCases = Probable_Survived + Probable_Died,
         TotalDeaths = Confirmed_Died + Probable_Died,
         TotalCases = ConfirmedCases + ProbableCases,
         DateUpdated = graphdate) %>% 
  rename(Gender = gender, ProbableDeaths = Probable_Died, 
         ConfirmedDeaths = Confirmed_Died) %>% 
  left_join(gender_tots, by= c("Gender" = "gender")) %>% 
  mutate(TotalCaseRate = round((TotalCases/n)*100000)) %>% 
  select(Gender, TotalCases, ConfirmedCases, ProbableCases,TotalDeaths, ConfirmedDeaths, ProbableDeaths, TotalCaseRate, DateUpdated) 
  
#printing
if (csv_write) {
write_csv(GenderSummary, paste0("L:/daily_reporting_figures_rdp/gary_csv/", Sys.Date(), "/GenderSummary.csv"))
}
if(SQL_write){
  df_to_table(GenderSummary, "ODP_GenderSummary", overwrite = TRUE, append = FALSE)
}
message("Table 4/11 complete, printed and pushed to SQL")
#clear trash
rm(gstotalcase, gsconfirmedcase, gsprobcase, gstotaldeaths, gsconfdeaths, gsprobdeaths, gstotalcaserate, gender_tots)#,GenderSummary


####6 StateSummary.csv ####
# hospitalized_totals <-  cha_c %>%  filter(State== "TOTAL") %>% select(today)
ncases <- nrow(case)
ntests <- nrow(elr_linelist)

mock_table <-
  tibble(`Overall Summary` = c("COVID-19 Cases (confirmed and probable)",
                               "COVID-19 Tests Reported (molecular and antigen)",
                               "Daily Test Positivity*",
                               "Patients Currently Hospitalized with COVID-19",
                               "COVID-19-Associated Deaths"),
         `Total**` = c("", "", "", "", ""),
         `Change Since Yesterday` = c("", "", "", "", ""))

tbl_total_col <-
  c(as.character(ncases),
    as.character(ntests),
    "",
    as.character(HospitalizedCases$HospitalizedCases),
    as.character(dec))

mock_table$`Total**` <- tbl_total_col

### Temporary logic to get last report
when_was_last_report <-
  case_when(weekdays(today()) %in% c("Monday", "Sunday") ~ "Friday",
            TRUE ~ weekdays(today() - 1))

last_rpt_data <-
  table_to_df("RPT_summary_bydate") %>%
  filter(dow_report_date == when_was_last_report) %>%
  filter(report_date == max(report_date))

Positivity <- (ncases - last_rpt_data$Cases)/(ntests - last_rpt_data$Tests)

yesterday_col <-
  c(
    paste0("+", ncases - last_rpt_data$Cases),
    paste0("+", ntests - last_rpt_data$Tests),
    paste0(round(Positivity * 100, 2), "%"),
    ifelse(HospitalizedCases$HospitalizedCases - last_rpt_data$Hospitalized > 0,
           paste0("+", HospitalizedCases$HospitalizedCases - last_rpt_data$Hospitalized),
           paste0(HospitalizedCases$HospitalizedCases - last_rpt_data$Hospitalized)),
    ifelse(dec - last_rpt_data$Deaths > 0,
           paste0("+", dec - last_rpt_data$Deaths),
           paste0(dec - last_rpt_data$Deaths))
  )

mock_table$`Change Since Yesterday` <- yesterday_col

tableforgary <- mock_table

StateSummary <- tableforgary %>%
  rename(
    Measure ='Overall Summary',
    Total = "Total**",
    Change = 'Change Since Yesterday'
  ) %>% 
  mutate(ChangeDirection = str_extract(Change, pattern = "\\+|\\-"),
         DateUpdated = graphdate,
         Change = str_remove(Change,pattern = "\\+|\\-|%")
  ) %>% 
  select(Measure, Total, ChangeDirection, Change, DateUpdated)%>% 
  replace_na(replace = list(ChangeDirection = ""))

#printing
if (csv_write) {
write_csv(StateSummary, paste0("L:/daily_reporting_figures_rdp/gary_csv/", Sys.Date(), "/StateSummary.csv"))
}
if(SQL_write){
  df_to_table(StateSummary, "ODP_StateSummary", overwrite = FALSE, append = TRUE)
}
message("Table 5/11 complete, printed and pushed to SQL")
#clear trash
rm(tableforgary, mock_table)#,StateSummary

####7 AgeGroupSummary.csv####
#setting up the lookup
pop <- county_rea_denoms %>% 
  group_by(age_group) %>% 
  summarise(pop = sum(pop))

AgeGroupSummary <- case %>% 
  filter(!is.na(age_group)) %>% 
  mutate(outcome = ifelse(is.na(outcome), "Survived", outcome)) %>% 
  group_by(age_group, disease_status,outcome) %>% 
  tally() %>% 
  ungroup() %>%
  complete(age_group = age_labels, disease_status = c("Confirmed", "Probable"), outcome = c("Died", "Survived"), fill = list(n = 0)) %>% 
  pivot_wider(id_cols = c(age_group, disease_status, outcome), names_from = c(disease_status, outcome), values_from = n) %>% 
  mutate(ConfirmedCases = Confirmed_Survived + Confirmed_Died,
         ProbableCases = Probable_Survived + Probable_Died,
         TotalDeaths = Confirmed_Died + Probable_Died,
         TotalCases = ConfirmedCases + ProbableCases,
         DateUpdated = graphdate) %>% 
  left_join(pop, by = c("age_group" = "age_group")) %>% 
  mutate(TotalCaseRate = round((TotalCases/pop)*100000),
         DateUpdated = graphdate,
         age_group = factor(age_group, levels = age_labels, labels = age_labels)) %>% 
  rename(AgeGroups = age_group, ProbableDeaths = Probable_Died, 
         ConfirmedDeaths = Confirmed_Died) %>% 
  select(AgeGroups, TotalCases, ConfirmedCases, ProbableCases,TotalDeaths, ConfirmedDeaths, ProbableDeaths, TotalCaseRate, DateUpdated) %>% 
  arrange(AgeGroups) %>% 
  mutate(AgeGroups = as.character(AgeGroups),
         AgeGroups = ifelse(AgeGroups == ">=80", "80 and older", AgeGroups))

#printing
if (csv_write) {
  write_csv(AgeGroupSummary, paste0("L:/daily_reporting_figures_rdp/gary_csv/", Sys.Date(), "/AgeGroupSummary.csv"))
}
if(SQL_write){
  df_to_table(AgeGroupSummary, "ODP_AgeGroupSummary", overwrite = TRUE, append = FALSE)
}
message("Table 6/11 complete, printed and pushed to SQL")
#clear trash
rm(pop, agstotalcases, agsconfcases, agsprobcases, agstotaldeaths, agsprobdeaths, agstotalrate)#,AgeGroupSummary


####8 CountySummary.csv####
#needs cha_c, tbl_cty_sum from county_lab_cases_deaths_table ~line 800ish in allgasnobrakes
hospsum <- cha_c %>% filter(!is.na(NAME)) %>%  pull(today)
counties <- tibble::tibble(County = forcats::as_factor(c("Fairfield County", "Hartford County", "Litchfield County", "Middlesex County", "New Haven County", "New London County", "Tolland County", "Windham County"))) %>% pull(County)

CountySummary<- case %>%
  rename(ds = disease_status, c = county, o = outcome) %>% 
  group_by(ds,c,o) %>% 
  tally() %>% 
  ungroup() %>% 
  replace_na(replace = list(o = "Survived")) %>%
  filter(!is.na(c)) %>% 
  complete(c = counties, o = c("Died", "Survived"), ds = c("Confirmed", "Probable"),
           fill = list(n = 0)) %>% 
  mutate(c = factor(c, level = counties, labels = counties))

CountySummary <- tibble(
  'County' = counties,
  'ConfirmedCases' = CountySummary %>% filter(ds == "Confirmed") %>% group_by(c) %>%  summarize(n =sum(n)) %>%  pull(n),
  'ProbableCases' = CountySummary %>% filter(ds == "Probable") %>% group_by(c) %>%  summarize(n =sum(n)) %>%  pull(n),
  'ConfirmedDeaths' = CountySummary %>% filter(ds == "Confirmed" & o == "Died") %>% group_by(c) %>%  summarize(n =sum(n)) %>%  pull(n),
  'ProbableDeaths' = CountySummary %>% filter(ds == "Probable" & o == "Died") %>% group_by(c) %>%  summarize(n =sum(n)) %>%  pull(n)
) %>% 
  filter(County != "Pending address validation") %>% 
  left_join(county_rea_denoms %>% group_by(county) %>% summarize(pop = sum(pop)) %>% mutate(county = paste0(county, " County")),
            by = c("County" = "county")) %>% 
  mutate(
    TotalCases = ConfirmedCases + ProbableCases,
    TotalDeaths = ConfirmedDeaths + ProbableDeaths,
    CNTY_COD = factor(County, levels = , labels = c(1,2,3,4,5,6,7,8 )),
    Hospitalization =hospsum,
    DateUpdated = graphdate,
    TotalCaseRate = round(TotalCases/pop*100000)
  ) %>% 
  select(CNTY_COD, County, TotalCases, ConfirmedCases,ProbableCases,TotalCaseRate,TotalDeaths,ConfirmedDeaths,ProbableDeaths,Hospitalization,DateUpdated) 

if(csv_write){
write_csv(CountySummary, paste0("L:/daily_reporting_figures_rdp/gary_csv/", Sys.Date(), "/CountySummary.csv"))
}

if(SQL_write){
  df_to_table(CountySummary, "ODP_CountySummary", overwrite = TRUE, append = FALSE)
}
message("Table 7/11 complete, printed and pushed to SQL")
#clear trash
rm(hospsum, counties)#,Countysummary

####9 DodSummary.csv####
#deaths by date
totaldeathsdate <- case %>% 
  filter(outcome == "Died") %>% 
  filter(!is.na(death_date) & death_date >="2020-01-01") %>% 
  group_by(death_date) %>% 
  tally(name = "TotalDeathCount") %>% 
  rename(dateofDeath = death_date) %>% 
  complete(dateofDeath = seq.Date(as.Date("2020-01-01"), Sys.Date()-1, by = "day"), fill = list(TotalDeathCount = 0))
confirmeddeathsdate <- case %>% 
  filter(outcome == "Died" & disease_status == "Confirmed") %>% 
  filter(!is.na(death_date) & death_date >="2020-01-01") %>% 
  group_by(death_date) %>% 
  tally(name = "ConfDeathCount") %>% 
  rename(dateofDeath = death_date) %>% 
  complete(dateofDeath = seq.Date(as.Date("2020-01-01"), Sys.Date()-1, by = "day"), fill = list(ConfDeathCount = 0))
probsdeathdate <- case %>% 
  filter(outcome == "Died" & disease_status == "Probable") %>% 
  filter(!is.na(death_date) & death_date >="2020-01-01") %>% 
  group_by(death_date) %>% 
  tally(name = "ProbDeathCount") %>% 
  rename(dateofDeath = death_date) %>% 
  complete(dateofDeath = seq.Date(as.Date("2020-01-01"), Sys.Date()-1, by = "day"), fill = list(ProbDeathCount = 0))

DodSummary <-tibble(
  dateofDeath = totaldeathsdate$dateofDeath,
  TotalDeaths = totaldeathsdate$TotalDeathCount,
  ConfirmedDeaths = confirmeddeathsdate$ConfDeathCount,
  ProbableDeaths = probsdeathdate$ProbDeathCount
)

#printing
if(csv_write){
  write_csv(DodSummary, paste0("L:/daily_reporting_figures_rdp/gary_csv/", Sys.Date(), "/DodSummary.csv"))
}
if(SQL_write){
  df_to_table(DodSummary, "ODP_DodSummary", overwrite = TRUE, append = FALSE)
}
message("Table 8/11 complete, printed and pushed to SQL")
#clear trash
rm(totaldeathsdate, confirmeddeathsdate, probsdeathdate)#,DodSummary

####10 TestCounty.csv ####
#count positive, negative, indeterminate, total tests by spec date and county for ODP
anti_tests <- elr_linelist %>% 
  filter(test_method %in% agtests)
anti_tests$result[anti_tests$result == "detected"] <- "Positive"
anti_tests$result[anti_tests$result == "not detected"] <- "Negative"
anti_tests$result[anti_tests$result == "indeterminate"] <- "Indeterminate"

anti_tests <- anti_tests %>%
  group_by(county, spec_col_date, result) %>% 
  tally() %>% 
  replace_na(replace = list(county= "Pending address validation")) %>%
  complete(result = c("Positive", "Negative", "Indeterminate"),fill =  list (n = 0)) %>% 
  pivot_wider(id_cols = c(county, spec_col_date), names_from = result, values_from = n ) %>%
  replace_na(replace = list('Positive' = 0,'Negative'= 0, 'Indeterminate' = 0)) %>%
  mutate(state="CT",
         number_of_ag_tests=sum(Positive, Negative, Indeterminate, na.rm=TRUE)) %>%
  rename(date = spec_col_date,
         number_of_ag_positives = Positive,
         number_of_ag_negatives = Negative,
         number_of_ag_indeterminates = Indeterminate)%>%
  select(county,date,number_of_ag_tests,number_of_ag_positives,number_of_ag_negatives,number_of_ag_indeterminates)

molec_tests <- elr_linelist %>% 
  filter(test_method %in% pcrtests)
molec_tests$result[molec_tests$result == "detected"] <- "Positive"
molec_tests$result[molec_tests$result == "not detected"] <- "Negative"
molec_tests$result[molec_tests$result == "indeterminate"] <- "Indeterminate"

molec_tests <- molec_tests %>%
  group_by(county, spec_col_date, result) %>% 
  tally() %>% 
  replace_na(replace = list(county= "Pending address validation")) %>%
  complete(result = c("Positive", "Negative", "Indeterminate"),fill =  list (n = 0)) %>% 
  pivot_wider(id_cols = c(county, spec_col_date), names_from = result, values_from = n ) %>%
  replace_na(replace = list('Positive' = 0,'Negative'= 0, 'Indeterminate' = 0)) %>%
  mutate(state="CT",
         number_of_pcr_tests=sum(Positive, Negative, Indeterminate, na.rm=TRUE)) %>%
  rename(date = spec_col_date,
         number_of_pcr_positives = Positive,
         number_of_pcr_negatives = Negative,
         number_of_pcr_indeterminates = Indeterminate)%>%
  select(county,date,number_of_pcr_tests,number_of_pcr_positives,number_of_pcr_negatives,number_of_pcr_indeterminates)

TestCounty <- molec_tests %>%
  left_join(anti_tests, by = c('county', 'date')) %>% 
  filter(!is.na(date) & !is.na(county)) 
TestCounty[is.na(TestCounty)] <- 0

#printing
if(csv_write){
  write_csv(TestCounty, paste0("L:/daily_reporting_figures_rdp/gary_csv/", Sys.Date(), "/TestCounty.csv"))
}
if(SQL_write){
  df_to_table(TestCounty, "ODP_TestCounty", overwrite = TRUE, append = FALSE)
}
message("Table 9/11 complete, printed and pushed to SQL")

#clear trash
rm(anti_tests, molec_tests)#,TestCounty

####11 REStateSummary.csv ####
#summary by race/ethnicity
REStateSummary <- race_eth_comb %>% 
  left_join(adj_table) %>% 
  rename(
    CrudeCaseRate = Crude,
    CaseAgeAdjusted = 'Age adjusted',     
  ) %>% 
  mutate(
    CrudeCaseRate =round(CrudeCaseRate),
    CaseAgeAdjusted = round(CaseAgeAdjusted)
  ) %>% 
  left_join(adj_table_dec) %>% 
  rename(
    CrudeDeathRate = Crude,
    DeathAgeAdjusted = 'Age adjusted',     
  ) %>% 
  mutate(
    CrudeDeathRate =round(CrudeDeathRate ),
    DeathAgeAdjusted= round(DeathAgeAdjusted),
    DateUpdated = graphdate
  ) 
#REStateSummary$tot[REStateSummary$hisp_race == "NH Multiracial"] <- NA

#printing
if(csv_write){
  write_csv(REStateSummary , paste0("L:/daily_reporting_figures_rdp/gary_csv/", 
                                    Sys.Date(), "/REStateSummary.csv"), na = "")
}
if(SQL_write){
  df_to_table(REStateSummary, "ODP_REStateSummary", overwrite = TRUE, append = FALSE)
}
message("Table 10/11 complete, printed and pushed to SQL")

#clear trash
rm()#race_eth_comb


####12 CountySummarybyDate.csv####
#cases by date by county
CountySummarybyDate <- case %>% 
  group_by(date, county) %>% 
  tally(name = "Count") %>% 
  mutate(UpdateDate = Sys.Date()) %>% 
  rename(
    County = county,
    Date = date
  ) %>% 
  ungroup() %>% 
  filter(!is.na(County) & !is.na(Date))

#printing
if (csv_write) {
  write_csv(CountySummarybyDate, paste0("L:/daily_reporting_figures_rdp/gary_csv/", 
                                    Sys.Date(), "/CountySummarybyDate.csv"))
}
if(SQL_write){
  df_to_table(CountySummarybyDate, "ODP_CountySummarybyDate", overwrite = TRUE, append = FALSE)
}
message("Table 11/11 complete, printed and pushed to SQL")

#clear trash
rm()#CountySummarybyDate
message("Gary's ODP Output process complete")