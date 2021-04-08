#### Module 5 ####
#This script will generate the COVID-19 reporting outputs needed for ODP and other stakeholders

####0 libraries and connections ####
source("helpers/StartMeUp.R")
con <- DBI::dbConnect(odbc::odbc(), "epicenter")
csv_write <- FALSE
SQL_write <- FALSE
####1 Load Lookups and Dependancies ####
age_labels <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", ">=80")

gary_age_labels <- c("cases_age0_9", "cases_age10_19", "cases_age20_29", "cases_age30_39", "cases_age40_49", "cases_age50_59", "cases_age60_69", "cases_age70_79", "cases_age80_Older" )

statement <- paste0("SELECT * FROM [DPH_COVID_IMPORT].[dbo].[RPT_TOWN_CODES]")
city_file <-  DBI::dbGetQuery(conn = con , statement = statement)

statement <- paste0("SELECT cty_label AS county
                    ,agegp18_label AS age_group
                    ,sex_label AS gender
                    ,race6_label AS race
                    ,hisp2 AS hisp
                    ,POP AS pop
                    FROM DPH_COVID_IMPORT.dbo.RPT_COUNTY_REA_DENOMS
                    WHERE YEAR = 2019")
county_rea_denoms <- DBI::dbGetQuery(conn = con , statement = statement) %>% 
  mutate(hisp_race = ifelse(hisp == 1, "Hispanic", paste0("NH ", race))) 
county_rea_denoms$hisp_race[county_rea_denoms$hisp_race %in% c("NH Asian", "NH Native Hawaiian and Other Pacific Islander")] <- "NH Asian or Pacific Islander"
county_rea_denoms$hisp_race[county_rea_denoms$hisp_race %in% c("NH Two or more races")] <- "Multiple Races" 
county_rea_denoms$hisp_race[county_rea_denoms$hisp_race %in% c("NH Black or African American")] <- "NH Black"

county_rea_denoms <- county_rea_denoms %>% 
  mutate(age_group = case_when(
    age_group %in% c("0-4 yrs", "5-9 yrs") ~ "0-9",
    age_group %in% c("10-14 yrs", "15-19 yrs") ~ "10-19",
    age_group %in% c("20-24 yrs", "25-29 yrs") ~ "20-29",
    age_group %in% c("30-34 yrs" , "35-39 yrs") ~ "30-39",
    age_group %in% c("40-44 yrs", "45-49 yrs") ~ "40-49",
    age_group %in% c("50-54 yrs", "55-59 yrs") ~ "50-59",
    age_group %in% c("60-64 yrs", "65-69 yrs") ~ "60-69",
    age_group %in% c("70-74 yrs", "75-79 yrs") ~ "70-79",
    age_group %in% c("80-84 yrs", "85+ yrs") ~ ">=80"
  )) %>% 
  group_by(county, age_group, gender, hisp_race) %>% 
  summarize(pop = sum(pop)) %>% 
  ungroup()


ct_rea_denoms <- county_rea_denoms %>%
  group_by(hisp_race) %>% 
  summarize(pop = sum(pop))

graphdate <- Sys.Date() - 1

#clear trash
rm(statement)

####2 state_Result.csv ####
#these require case and dec and cha_c
State <- tibble("State" = "CONNECTICUT")
LastUpdateDate <- tibble("LastUppdateDate" = graphdate)#tibble("LastUppdateDate" = max(as.Date(mdy(case$`Event Date`))))
TotalCases <- tibble('TotalCases'= nrow(case))
ConfirmedCases <- tibble("ConfirmedCases" = nrow(case %>% filter(disease_status == "Confirmed")))
ProbableCases <- tibble("ProbableCases" = nrow(case %>% filter(disease_status == "Probable")))
HospitalizedCases <- tibble(HospitalizedCases = cha_c$today[9])
dec <- case %>% filter(outcome == "Died") %>%  nrow()
TotalDeaths <- tibble("Deaths" = dec)
ConfirmedDeaths <- tibble("ConfirmedDeaths" = nrow(case %>%  filter(outcome == "Died" & disease_status == "Confirmed")))
ProbableDeaths <-  tibble("ProbableDeaths" = nrow(case %>%  filter(outcome == "Died" & disease_status == "Probable")))
gary_ages <- case %>%
  group_by(age_group) %>%
  tally() %>%
  filter(!is.na(age_group)) %>%
  pivot_wider(names_from = age_group, values_from = n)
state_Result <-  bind_cols(State, LastUpdateDate, TotalCases, ConfirmedCases,ProbableCases, HospitalizedCases, TotalDeaths, ConfirmedDeaths, ProbableDeaths,gary_ages)

#printing
if (csv_write) {
  write_csv(state_Result, paste0("L:/daily_reporting_figures_rdp/gary_csv/", Sys.Date(), "/state_Result.csv"))
}
if (SQL_write) {
  df_to_table(state_Result, "ODP_state_Result", overwrite = TRUE, append = FALSE)
}

#clear trash
rm(State, LastUpdateDate, TotalCases, ConfirmedCases, ProbableCases, HospitalizedCases, dec, TotalDeaths, ConfirmedDeaths,ProbableDeaths, gary_ages)#,state_Result

####3 town_Result.csv ####
#probably some good candidates for a custom function here

#creating the various columns for people related counts by town
town_total_cases <- case %>% 
  filter(city != "Not_available") %>% 
  group_by(city) %>% 
  tally() %>%
  rename(CITY = city) %>% 
  complete(CITY = city_file$TOWN_LC, fill = list(n = 0))
town_confirmed_cases <- case %>% 
  filter(disease_status == "Confirmed") %>% 
  filter(city != "Not_available") %>% 
  group_by(city) %>% 
  tally() %>%
  rename(CITY = city) %>% 
  complete(CITY = city_file$TOWN_LC, fill = list(n = 0))
town_probable_cases <- case %>% 
  filter(disease_status == "Probable") %>% 
  filter(city != "Not_available") %>% 
  group_by(city) %>% 
  tally() %>%
  rename(CITY = city) %>% 
  complete(CITY = city_file$TOWN_LC, fill = list(n = 0))
town_total_deaths <- case %>%
  filter(outcome == "Died")%>%
  filter(city != "Not_available") %>% 
  group_by(city) %>%
  tally(name = "Deaths") %>% 
  rename(CITY = city)%>% 
  complete(CITY = city_file$TOWN_LC, fill = list(n = 0))
town_confirmed_deaths <- case %>%
  filter(outcome == "Died" & disease_status == "Confirmed")%>%
  filter(city != "Not_available") %>% 
  group_by(city) %>%
  tally(name = "Deaths") %>% 
  rename(CITY = city)%>% 
  complete(CITY = city_file$TOWN_LC, fill = list(n = 0))
town_probable_deaths <- case %>%
  filter(outcome == "Died" & disease_status == "Probable")%>%
  filter(city != "Not_available") %>% 
  group_by(city) %>%
  tally(name = "Deaths") %>% 
  rename(CITY = city) %>% 
  complete(CITY = city_file$TOWN_LC, fill = list(n = 0))
TownCaseRate <- case %>%
  filter(!is.na(city) & city != "Not_available") %>% 
  group_by(city) %>% 
  tally(name ='case_n') %>% 
  complete(city = city_file$TOWN_LC, fill = list(case_n = 0)) %>% 
  left_join(city_file %>% select(TOWN_LC,pop_2019),
            by = c("city" = "TOWN_LC")) %>% 
  mutate(CaseRate = round((case_n/pop_2019)*100000),
         CITY = paste0(city, " town")
  ) %>% 
  select(-c(case_n, pop_2019, city)) 

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
  mutate(state="CT",
         number_of_tests=sum(Positive, Negative, Indeterminate, na.rm=TRUE)) %>%
  rename(number_of_positives = Positive,
         number_of_negatives = Negative,
         number_of_indeterminates = Indeterminate,
         town = city)

#Number of people tested by town
peopletestbytown<- elr_linelist %>% 
  mutate(simple_result = ifelse(result == "detected", 1, 2)) %>%
  group_by(bigID) %>%
  arrange(simple_result,spec_col_date) %>%
  slice(1L) %>%
  ungroup() %>% 
  rename(Town = city, SpecimenCollectionDate = spec_col_date) %>% 
  group_by(Town) %>% 
  tally(name = "PeopleTested") %>% 
  filter(Town != "Not_available") %>% 
  mutate(Town = str_to_title(Town)) %>% 
  left_join(city_file %>% select(TOWN_LC,pop_2019),
            by = c("Town" = "TOWN_LC")) %>% 
  mutate(RateTested100k = round(PeopleTested/pop_2019 * 100000)) %>% 
  select(-pop_2019)

#binding all the columns into gary_town_file
town_Result <- tibble(
  CITY = town_total_cases$CITY,
  LastUpdateDate = graphdate,
  TownTotalCases = town_total_cases$n,
  TownConfirmedCases = town_confirmed_cases$n,
  TownProbableCases = town_probable_cases$n,
  TownTotalDeaths = town_total_deaths$Deaths,
  TownConfirmedDeaths = town_confirmed_deaths$Deaths,
  TownProbableDeaths = town_probable_deaths$Deaths,
  TownCaseRate = TownCaseRate$CaseRate,
  PeopleTested = peopletestbytown$PeopleTested,
  NumberofTests = town_tests$number_of_tests,
  NumberofPositives = town_tests$number_of_positives,
  NumberofNegatives = town_tests$number_of_negatives,
  NumberofIndeterminates = town_tests$number_of_indeterminates,
  RateTested100k = peopletestbytown$RateTested100k
) %>%
  left_join(city_file, by = c("CITY" = "TOWN_LC"))%>%
  rename(Town_No = TOWNNO,
         Town = CITY) %>%
  select(Town_No, everything()) %>%
  filter(!is.na(Town_No)) 
town_Result[is.na(town_Result)] <- 0

#printing
 if (csv_write) {
   write_csv(town_Result, paste0("L:/daily_reporting_figures_rdp/gary_csv/", Sys.Date(), "/town_Result.csv"))
 }
 if (SQL_write) {
   df_to_table(town_Result, "ODP_town_Result", overwrite = TRUE, append = FALSE)
 }

#clear trash
rm(town_total_cases,town_confirmed_cases,town_probable_cases, town_total_deaths, town_confirmed_deaths, town_probable_deaths, TownCaseRate, peopletestbytown, town_tests)#,town_Result

####4 ConProbByDate.csv ####
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

####5 GenderSummary.csv####
#set up the lookup to be gender only
gender_tots <- county_rea_denoms %>% 
  group_by(gender) %>% 
  summarize(n=sum(pop)) %>% 
  mutate(gender = ifelse(gender == "M",
                         "Male",
                         "Female"
  ))
#creating the columns for this table 
gstotalcase <- case %>% 
  group_by(gender) %>% 
  tally(name = "Cases") %>% 
  filter(!is.na(gender) & !gender %in% c("Unknown", "Other"))
gsconfirmedcase <- case %>% 
  filter(disease_status == "Confirmed") %>% 
  group_by(gender) %>% 
  tally(name = "Cases") %>% 
  filter(!is.na(gender) & !gender %in% c("Unknown", "Other"))
gsprobcase <- case %>% 
  filter(disease_status == "Probable") %>% 
  group_by(gender) %>% 
  tally(name = "Cases") %>% 
  filter(!is.na(gender) & !gender %in% c("Unknown", "Other"))  
gstotaldeaths <- case %>% 
  filter(outcome =="Died") %>% 
  group_by(gender) %>% 
  tally(name = "Deaths")  %>% 
  filter(!is.na(gender) & !gender %in% c("Unknown", "Other"))
gstotaldeaths <- case %>% 
  filter(outcome =="Died") %>% 
  group_by(gender) %>% 
  tally(name = "Deaths")  %>% 
  filter(!is.na(gender) & !gender %in% c("Unknown", "Other"))
gsconfdeaths <- case %>% 
  filter(disease_status == "Confirmed" & outcome =="Died") %>% 
  group_by(gender) %>% 
  tally(name = "Deaths")  %>% 
  filter(!is.na(gender) & !gender %in% c("Unknown", "Other"))
gsprobdeaths <- case %>% 
  filter(disease_status == "Probable" & outcome =="Died") %>% 
  group_by(gender) %>% 
  tally(name = "Deaths")  %>% 
  filter(!is.na(gender) & !gender %in% c("Unknown", "Other"))
gstotalcaserate <- gstotalcase %>% 
  left_join(gender_tots, by= c("gender" = "gender")) %>% 
  mutate(TotalCaseRate = round((Cases/n)*100000)) %>% 
  select(TotalCaseRate)

#bind all the columns together
GenderSummary <- tibble(
  Gender = gstotalcase$gender,
  TotalCases = gstotalcase$Cases,
  ConfirmedCases = gsconfirmedcase$Cases,
  ProbableCases = gsprobcase$Cases,
  TotalDeaths = gstotaldeaths$Deaths,
  ConfirmedDeaths = gsconfdeaths$Deaths,
  ProbableDeaths = gsprobdeaths$Deaths,
  TotalCaseRate = gstotalcaserate$TotalCaseRate,
  DateUpdated = format(graphdate, "%m/%d/%Y")
) 

#printing
if (csv_write) {
write_csv(GenderSummary, paste0("L:/daily_reporting_figures_rdp/gary_csv/", Sys.Date(), "/GenderSummary.csv"))
}

#clear trash
rm(gstotalcase, gsconfirmedcase, gsprobcase, gstotaldeaths, gsconfdeaths, gsprobdeaths, gstotalcaserate, gender_tots)#,GenderSummary


####6 StateSummary.csv ####
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

#clear trash
rm(tableforgary)#,StateSummary

####7 AgeGroupSummary.csv####
#setting up the lookup
pop <- county_rea_denoms %>% 
  group_by(age_group) %>% 
  summarise(pop = sum(pop))

#creating the columns of this table
agstotalcases <- case %>% 
  filter(!is.na(age_group)) %>% 
  group_by(age_group) %>% 
  tally(name = "TotalCases")
agsconfcases <- case %>% 
  filter(!is.na(age_group) & disease_status == "Confirmed") %>% 
  group_by(age_group) %>% 
  tally(name = "ConfCases")

agsprobcases<- case %>% 
  filter(!is.na(age_group) & disease_status == "Probable") %>% 
  group_by(age_group) %>% 
  tally(name = "ProbCases")
agstotaldeaths <- case %>% 
  filter(!is.na(age_group)  & outcome == "Died") %>% 
  group_by(age_group) %>% 
  tally(name = "TotalDeaths") %>% 
  complete(age_group = unique(case$age_group), fill = list("TotalDeaths" = 0))

agsconfdeaths <- case %>% 
  filter(!is.na(age_group) & disease_status == "Confirmed" & outcome == "Died") %>% 
  group_by(age_group) %>% 
  tally(name = "ConfDeaths")%>% 
  complete(age_group = unique(case$age_group), fill = list("ConfDeaths" = 0))

agsprobdeaths <- case %>% 
  filter(!is.na(age_group) & disease_status == "Probable" & outcome == "Died") %>% 
  group_by(age_group) %>% 
  tally(name = "ProbDeaths") %>% 
  complete(age_group = unique(case$age_group), fill = list(ProbDeaths = 0))

agstotalrate <- case %>% 
  filter(!is.na(age_group)) %>% 
  group_by(age_group) %>% 
  tally(name = "TotalCases") %>% 
  left_join(pop, by = c("age_group" = "age_group")) %>% 
  mutate(TotalCaseRate = round((TotalCases/pop)*100000))

#binding the columns together
AgeGroupSummary <- tibble(
  AgeGroups = agstotalcases$age_group,
  TotalCases =agstotalcases$TotalCases,
  ConfirmedCases = agsconfcases$ConfCases,
  ProbableCases = agsprobcases$ProbCases,
  TotalDeaths = agstotaldeaths$TotalDeaths,
  ConfirmedDeaths = agsconfdeaths$ConfDeaths,
  ProbableDeaths = agsprobdeaths$ProbDeaths,
  TotalCaseRate = agstotalrate$TotalCaseRate,
  DateUpdated = graphdate
) %>% 
  mutate(AgeGroups = as.character(AgeGroups))
AgeGroupSummary$AgeGroups[AgeGroupSummary$AgeGroups == ">=80"] <- "80 and older"

#printing
if (csv_write) {
  write_csv(AgeGroupSummary, paste0("L:/daily_reporting_figures_rdp/gary_csv/", Sys.Date(), "/AgeGroupSummary.csv"))
}

#clear trash
rm(pop, agstotalcases, agsconfcases, agsprobcases, agstotaldeaths, agsprobdeaths, agstotalrate)#,AgeGroupSummary


####8 CountySummary.csv####
#needs cha_c, tbl_cty_sum from county_lab_cases_deaths_table ~line 800ish in allgasnobrakes
hospsum <- cha_c %>% filter(!is.na(NAME)) %>%  select(today)
county_pop <- county_rea_denoms %>% 
  group_by(county) %>% 
  summarize(pop = sum(pop))
CountySummary <- tbl_cty_sum %>% 
  rename(
    ConfirmedCases = cases.confirmed,
    ProbableCases = cases.probable,
    ConfirmedDeaths = dec.confirmed,
    ProbableDeaths = dec.probable
  ) %>% 
  filter(County != "Pending address validation") %>% 
  bind_cols(county_pop %>% select(-county)) %>%
  mutate(
    TotalCases = ConfirmedCases + ProbableCases,
    TotalDeaths = ConfirmedDeaths + ProbableDeaths,
    CNTY_COD = factor(County, levels =c(
      'Fairfield County','Hartford County','Litchfield County','Middlesex County','New Haven County','New London County', 'Tolland County', 'Windham County'), labels = c(1,2,3,4,5,6,7,8 )),
    CNTY_FIPS = factor(County, levels=c(
      'Fairfield County','Hartford County','Litchfield County','Middlesex County','New Haven County','New London County', 'Tolland County', 'Windham County'), labels=c("'001", "'003", "'005", "'007", "'009", "'011", "'013", "'015")),
    Hospitalization =hospsum$today,
    DateUpdated = graphdate,
    TotalCaseRate = round(TotalCases/pop*100000)
  ) %>% 
  select(CNTY_COD, County, TotalCases, ConfirmedCases,ProbableCases,TotalCaseRate,TotalDeaths,ConfirmedDeaths,ProbableDeaths,Hospitalization,DateUpdated) #CNTY_FIPS removed as of 11/9 per request

if(csv_write){
write_csv(CountySummary, paste0("L:/daily_reporting_figures_rdp/gary_csv/", Sys.Date(), "/CountySummary.csv"))
}

#clear trash
rm(hospsum, county_pop, tbl_cty_sum)#,Countysummary

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
  write_csv(death_by_deathdate, paste0("L:/daily_reporting_figures_rdp/gary_csv/", Sys.Date(), "/DodSummary.csv"))
}

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

#clear trash
rm(anti_tests, molec_tests)#,TestCounty

####11 REStateSummary.csv ####
#summary by race/ethnicity

REStateSummary <- race_eth_comb %>% 
  select(-c(caserate100k,deathrate100k)) %>% 
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

#printing
if(csv_write){
  write_csv(REStateSummary , paste0("L:/daily_reporting_figures_rdp/gary_csv/", 
                                    Sys.Date(), "/REStateSummary.csv"), na = "")
}

#clear trash
rm()


##notes ##
#dependancies
#case, elr_linelist, dec, cha_c, cc_map (replaced with city_file), town_codes (city_file), epicurve (from thursday), gener_race_eth
#  tableforgary
#connection



#replace gender_race_eth <- read_csv("L:/daily_reporting_figures_rdp/population_data/county_re_gender_age.csv")
#replace race_eth_comb

#new dependancies

# gary_age_labels <- c("cases_age0_9", "cases_age10_19", "cases_age20_29", "cases_age30_39", "cases_age40_49", "cases_age50_59", "cases_age60_69", "cases_age70_79", "cases_age80_Older" )


# race_eth_SHA_lookup <- gender_race_eth %>% 
#   group_by(hisp_race) %>%  
#   summarize(tot=sum(n))
# race_eth_SHA_case <- case %>%
#   group_by(hisp_race) %>% 
#   tally(name = "case_tot")
# 
# race_eth_comb <- race_eth_SHA_case %>% 
#   left_join(race_eth_SHA_lookup) %>% 
#   mutate(rate100k = round((case_tot/tot)*100000))
# 
# ####dec
# race_eth_SHA_case_dec <- case %>%
#   filter(outcome == "Died") %>% 
#   group_by(hisp_race) %>%  
#   tally(name = "deaths")
# 
# 
# race_eth_comb <- race_eth_comb %>% 
#   left_join(race_eth_SHA_case_dec) %>% 
#   replace_na(replace =list(deaths = 0)) %>% 
#   rename(caserate100k=rate100k) %>% 
#   mutate(deathrate100k = round((deaths/tot)*100000))
# race_eth_comb$hisp_race <- factor(race_eth_comb$hisp_race, levels =c("Hispanic", "NH White", "NH Black", "NH American Indian or Alaskan Native", "NH Asian or Pacific Islander", "NH Other", "NH Multiracial", "Unknown") , labels =c("Hispanic", "NH White", "NH Black", "NH American Indian or Alaskan Native", "NH Asian or Pacific Islander", "NH Other", "NH Multiracial", "Unknown"))



















#summary by race/ethnicity
REStateSummary <- race_eth_comb %>% 
  select(-c(caserate100k,deathrate100k)) %>% 
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


#cases by date by county
countycountDate <- case %>% 
  group_by(date, county) %>% 
  tally(name = "Count") %>% 
  mutate(UpdateDate = Sys.Date()) %>% 
  rename(
    County = county,
    Date = date
  ) %>% 
  ungroup() %>% 
  filter(!is.na(County) & !is.na(Date))

####old writes ####

if (csv_write) {
  write_csv(countycountDate, paste0("L:/daily_reporting_figures_rdp/gary_csv/", 
                                    Sys.Date(), "/CountySummarybyDate.csv"))
  

  
  
  
}
if (SQL_write) {
  df_to_table(countycountDate, "ODP_CountySummarybyDate", overwrite = TRUE, append = FALSE)
  df_to_table(death_by_deathdate, "ODP_DodSummary", overwrite = TRUE, append = FALSE)
  df_to_table(GenderSummary, "ODP_GenderSummary", overwrite = TRUE, append = FALSE)
  df_to_table(StateSummary, "ODP_StateSummary", overwrite = FALSE, append = TRUE)
  df_to_table(AgeGroupSummary, "ODP_AgeGroupSummary", overwrite = TRUE, append = FALSE)
  df_to_table(CountySummary, "ODP_CountySummary", overwrite = TRUE, append = FALSE)
  df_to_table(REStateSummary, "ODP_REStateSummary", overwrite = TRUE, append = FALSE)
  df_to_table(agg_test_gary, "ODP_TestCounty", overwrite = TRUE, append = FALSE)
}


#calculate % positive by town for past 2 weeks
pctpos14 <- test14nc %>% 
  group_by(city, result) %>% 
  tally() %>%
  pivot_wider(names_from=result, values_from=n) %>% 
  replace_na(replace=list(detected=0, `not detected`=0, indeterminate=0)) %>%
  mutate(PercentPositive = round(detected/(detected+`not detected`)*100, 1),
         TotalTests=detected+`not detected`+indeterminate) %>% 
  select(city, TotalTests,PercentPositive) %>% 
  filter(city != "Not Available")

#community cases in the past 2 weeks with rates by town, case count for each week
gary_dailyincidence <- 
  cases_14_nc %>% 
  group_by(week, city) %>% 
  tally() %>%  
  pivot_wider(names_from = week,  
              values_from = n, 
              values_fill = list(n = 0)) %>% 
  rename(casesweek1 = 2,
         casesweek2 = 3) %>% 
  right_join(c14nc_count, by=c("city" = "NAME")) %>% 
  replace_na(replace=list(casesweek1 = 0, 
                          casesweek2 = 0, 
                          n = 0)) %>% 
  mutate(RateCategory = ifelse(avgrate =="<5 cases per 100,000 or <5 reported cases", 
                               "1. <5 cases per 100,000 or <5 reported cases",
                               ifelse(avgrate=="5-9 cases per 100,000", 
                                      "2. 5-9 cases per 100,000", 
                                      ifelse(avgrate=="10-14 cases per 100,000", 
                                             "3. 10-14 cases per 100,000",
                                             ifelse(avgrate=="15 or more cases per 100,000", 
                                                    "4. 15 or more cases per 100,000", 
                                                    "other")))),
         UpdateDate = Sys.Date(),
         ReportPeriodStartDate = thursday_range_start,
         ReportPeriodEndDate = thursday_range_end,
         townname=paste0(city, " town")) %>%
  rename(totalcases = n,
         NAME = city) %>% 
  left_join(pctpos14, by = c("NAME" = "city")) %>% 
  left_join(town_codes, by = c("townname" = "ANPSADPI")) %>%
  rename(Town_No = TOWNNO) %>% 
  select(Town_No, NAME, pop, casesweek1, casesweek2, totalcases, CaseRate, 
         RateCategory, TotalTests, PercentPositive, UpdateDate, 
         ReportPeriodStartDate, ReportPeriodEndDate) %>%
  filter(!NAME == "Not Available")

### Why do we write the same thing to two different places?

# if(csv_write) {
#   write_csv(gary_dailyincidence, 
#             paste0("L:/daily_reporting_figures_rdp/gary_csv/CTTown_Alert.csv"))
#   dir.create(paste0("L:/daily_reporting_figures_rdp/yesterday/", Sys.Date(), "/avg_di"))
#   write_csv(gary_dailyincidence, 
#             paste0("L:/daily_reporting_figures_rdp/yesterday/", 
#                    Sys.Date(),"/avg_di/avgdailyincidence.csv"))
# }