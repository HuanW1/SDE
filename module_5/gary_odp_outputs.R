#### Module 5 / gary_odp_outputs ####
#This script will generate the COVID-19 reporting outputs needed for ODP and other stakeholders
message("Gary's ODP output process will now begin")

####1 REStateSummary.csv ####
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

#printing
if(csv_write){
  write_csv(REStateSummary , paste0("L:/Outputs/", 
                                    Sys.Date(), "/ODP_REStateSummary.csv"), na = "")
}
if(SQL_write){
  df_to_table(REStateSummary, "ODP_REStateSummary", overwrite = TRUE, append = FALSE)
}
message("Table 1/11 complete, printed and pushed to SQL")

#clear trash
rm(REStateSummary)

####2 CountySummary.csv####
hospsum <- cha_c %>% filter(!is.na(NAME)) %>%  pull(today)

CountySummary<- case %>%
  filter(!is.na(county)) %>% 
  group_by(county, disease_status,outcome) %>% 
  tally() %>% 
  ungroup() %>%
  complete(county = counties, disease_status = c("Confirmed", "Probable"), outcome = c("Died", "Survived"), fill = list(n = 0)) %>% 
  pivot_wider(id_cols = c(county, disease_status, outcome), names_from = c(disease_status, outcome), values_from = n) %>% 
  mutate(ConfirmedCases = Confirmed_Survived + Confirmed_Died,
         ProbableCases = Probable_Survived + Probable_Died,
         TotalDeaths = Confirmed_Died + Probable_Died,
         TotalCases = ConfirmedCases + ProbableCases,
         DateUpdated = graphdate) %>% 
  left_join(county_rea_denoms %>% group_by(county) %>% summarize(pop = sum(pop)) %>% mutate(county = paste0(county, " County")),
            by = c("county" = "county")) %>% 
  mutate(TotalCaseRate = round((TotalCases/pop)*100000),
         DateUpdated = graphdate,
         county = factor(county, levels = counties, labels = counties),
         Hospitalizations = hospsum,
         CNTY_COD = factor(county, levels =c(
             'Fairfield County','Hartford County','Litchfield County','Middlesex County','New Haven County','New London County', 'Tolland County', 'Windham County'), labels = c(1,2,3,4,5,6,7,8 ))) %>% 
  rename(County = county, ProbableDeaths = Probable_Died, 
         ConfirmedDeaths = Confirmed_Died) %>% 
  select(CNTY_COD, County, TotalCases, ConfirmedCases, ProbableCases,TotalCaseRate, TotalDeaths, ConfirmedDeaths, ProbableDeaths, Hospitalizations,DateUpdated) %>% 
  arrange(CNTY_COD)

if(csv_write){
  write_csv(CountySummary, paste0("L:/Outputs/", Sys.Date(), "/ODP_CountySummary.csv"))
}

if(SQL_write){
  df_to_table(CountySummary, "ODP_CountySummary", overwrite = TRUE, append = FALSE)
}
message("Table 2/11 complete, printed and pushed to SQL")
#clear trash
rm(hospsum,CountySummary)

####3 StateSummary.csv ####
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
    as.character(HospitalizedCases),
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
    ifelse(HospitalizedCases - last_rpt_data$Hospitalized > 0,
           paste0("+", HospitalizedCases - last_rpt_data$Hospitalized),
           paste0(HospitalizedCases - last_rpt_data$Hospitalized)),
    ifelse(dec - last_rpt_data$Deaths > 0,
           paste0("+", dec - last_rpt_data$Deaths),
           paste0(dec - last_rpt_data$Deaths))
  )

mock_table$`Change Since Yesterday` <- yesterday_col

StateSummary <- mock_table %>%
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
  write_csv(StateSummary, paste0("L:/Outputs/", Sys.Date(), "/ODP_StateSummary.csv"))
}
if(SQL_write){
  df_to_table(StateSummary, "ODP_StateSummary", overwrite = FALSE, append = TRUE)
}
message("Table 3/11 complete, printed and pushed to SQL")
#clear trash
rm(mock_table, ncases, ntests, StateSummary, last_rpt_data, yesterday_col, when_was_last_report, Positivity, tbl_total_col)

####4 state_Result.csv ####
state_Result <- case %>% 
  group_by(disease_status,outcome, age_group) %>% 
  tally() %>% 
  mutate(outcome = ifelse(is.na(outcome), "Survived", outcome)) 

state_Result <- tibble(`State` = "CONNECTICUT",
                       DateUpdated = graphdate,
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
  write_csv(state_Result, paste0("L:/Outputs/", Sys.Date(), "/ODP_state_Result.csv"))
}
if (SQL_write) {
  df_to_table(state_Result, "ODP_state_Result", overwrite = TRUE, append = FALSE)
}
message("Table 4/11 complete, printed and pushed to SQL, 5 is a bit of a doozy.")
#clear trash
rm(state_Result)

####5 town_Result.csv ####
  town_cases <- case %>% 
    filter(!is.na(city) & city != "Not_available") %>% 
    group_by(city, disease_status,outcome) %>% 
    tally() %>%
    rename(Town = city) %>% 
    ungroup() %>% 
    complete(Town = city_file$TOWN_LC, outcome = c("Died", "Survived"),
             disease_status = c("Confirmed", "Probable"), fill = list(n = 0)) %>% 
    left_join(city_file %>% select(TOWNNO, TOWN_LC,pop_2019),
                by = c("Town" = "TOWN_LC")) %>% 
    group_by(Town) %>% 
    mutate(TownTotalCases = sum(n),
           TownCaseRate = round((TownTotalCases/pop_2019)*100000)) %>% 
    ungroup() %>% 
    pivot_wider(id_cols = c(Town, outcome, disease_status,TownTotalCases, TownCaseRate, TOWNNO ), names_from = c(outcome, disease_status), values_from = n) %>% 
    mutate(TownTotalDeaths = Died_Confirmed + Died_Probable,
           DateUpdated = graphdate,
           TownConfirmedCases = Died_Confirmed + Survived_Confirmed,
           TownProbableCases = Died_Probable + Survived_Probable) %>% 
  rename(Town_No = TOWNNO,
         TownConfirmedDeaths = Died_Confirmed,
         TownProbableDeaths = Died_Probable) %>% 
  select(Town_No, Town, DateUpdated, TownTotalCases, TownConfirmedCases, TownProbableCases, TownTotalDeaths, TownConfirmedDeaths, TownProbableDeaths, TownCaseRate)

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
  filter(city %in% city_file$TOWN_LC & !is.na(city) 
         & !is.na(spec_col_date) & city %in% city_file$TOWN_LC) %>%
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
  inner_join(city_file %>% select(TOWN_LC,pop_2019),
            by = c("Town" = "TOWN_LC")) %>% 
  mutate(RateTested100k = round(PeopleTested/pop_2019 * 100000)) %>% 
  select(-pop_2019)

#binding all the columns into gary_town_file
town_Result <- town_cases %>% 
  inner_join(town_tests, by = "Town") %>% 
  inner_join(town_people, by = "Town") %>% 
  select(Town_No,Town, DateUpdated, TownTotalCases,	TownConfirmedCases, 
         TownProbableCases,	TownTotalDeaths,	TownConfirmedDeaths,
         TownProbableDeaths,	TownCaseRate,	PeopleTested,	NumberofTests,
         NumberofPositives,	NumberofNegatives,	NumberofIndeterminates,
         RateTested100k)

#printing
 if (csv_write) {
   write_csv(town_Result, paste0("L:/Outputs/", Sys.Date(), "/ODP_town_Result.csv"))
 }
 if (SQL_write) {
   df_to_table(town_Result, "ODP_town_Result", overwrite = TRUE, append = FALSE)
 }
message("Table 5/11 complete, printed and pushed to SQL")
#clear trash
rm(town_Result, town_people, town_tests, town_cases)

####6 ConProbByDate.csv ####
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
  mutate(Total =  Confirmed + Probable,
         DateUpdated = graphdate) %>% 
  select(Date, Confirmed, Probable, Total, DateUpdated)

#printing
if (csv_write) {
  write_csv(ConProbByDate, paste0("L:/Outputs/", Sys.Date(), "/ODP_ConProbByDate.csv"))
}
if (SQL_write) {
  df_to_table(ConProbByDate, "ODP_ConProbByDate", overwrite = TRUE, append = FALSE)
}
message("Table 6/11 complete, printed and pushed to SQL")
rm(epicurve, ConProbByDate)

####7 GenderSummary.csv####
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
write_csv(GenderSummary, paste0("L:/Outputs/", Sys.Date(), "/ODP_GenderSummary.csv"))
}
if(SQL_write){
  df_to_table(GenderSummary, "ODP_GenderSummary", overwrite = TRUE, append = FALSE)
}
message("Table 7/11 complete, printed and pushed to SQL")
#clear trash
rm(gender_tots, GenderSummary)

####8 AgeGroupSummary.csv####
#setting up the lookup
pop <- county_rea_denoms %>% 
  group_by(age_group) %>% 
  summarise(pop = sum(pop))

AgeGroupSummary <- case %>% 
  filter(!is.na(age_group)) %>% 
  group_by(age_group, disease_status,outcome) %>% 
  tally() %>% 
  ungroup() %>%
  complete(age_group = age_labels, disease_status = c("Confirmed", "Probable"),
           outcome = c("Died", "Survived"), fill = list(n = 0)) %>% 
  pivot_wider(id_cols = c(age_group, disease_status, outcome), 
              names_from = c(disease_status, outcome), values_from = n) %>% 
  mutate(ConfirmedCases = Confirmed_Survived + Confirmed_Died, ProbableCases = Probable_Survived + Probable_Died,
         TotalDeaths = Confirmed_Died + Probable_Died, TotalCases = ConfirmedCases + ProbableCases,
         DateUpdated = graphdate) %>% 
  left_join(pop, by = c("age_group" = "age_group")) %>% 
  mutate(TotalCaseRate = round((TotalCases/pop)*100000),
         age_group = factor(age_group, levels = age_labels, labels = age_labels)) %>% 
  rename(AgeGroups = age_group, ProbableDeaths = Probable_Died, 
         ConfirmedDeaths = Confirmed_Died) %>% 
  select(AgeGroups, TotalCases, ConfirmedCases, ProbableCases,TotalDeaths,
         ConfirmedDeaths, ProbableDeaths, TotalCaseRate, DateUpdated) %>% 
  arrange(AgeGroups) %>% 
  mutate(AgeGroups = as.character(AgeGroups),
         AgeGroups = ifelse(AgeGroups == ">=80", "80 and older", AgeGroups))

#printing
if (csv_write) {
  write_csv(AgeGroupSummary, paste0("L:/Outputs/", Sys.Date(), "/ODP_AgeGroupSummary.csv"))
}
if(SQL_write){
  df_to_table(AgeGroupSummary, "ODP_AgeGroupSummary", overwrite = TRUE, append = FALSE)
}
message("Table 8/11 complete, printed and pushed to SQL")
#clear trash
rm(pop, AgeGroupSummary)

####9 DodSummary.csv####
#deaths by date

DodSummary<- case %>% 
  filter(outcome == "Died" & !is.na(death_date) & death_date >="2020-01-01") %>%
  group_by(disease_status, death_date) %>%
  tally() %>% 
  ungroup() %>% 
  complete(death_date = seq.Date(as.Date("2020-01-01"), Sys.Date()-1, by = "day"), 
           disease_status = c("Confirmed", "Probable"), fill = list(n = 0)) %>% 
  pivot_wider(id_cols = c(death_date, disease_status), names_from = c(disease_status), values_from = n) %>%
  mutate(TotalDeaths = Confirmed + Probable,
         DateUpdated = graphdate) %>% 
  rename(dateofDeath = death_date, ConfirmedDeaths = Confirmed,
         ProbableDeaths = Probable) %>% 
  select(dateofDeath, TotalDeaths, ConfirmedDeaths, ProbableDeaths,DateUpdated)

#printing
if(csv_write){
  write_csv(DodSummary, paste0("L:/Outputs/", Sys.Date(), "/ODP_DodSummary.csv"))
}
if(SQL_write){
  df_to_table(DodSummary, "ODP_DodSummary", overwrite = TRUE, append = FALSE)
}
message("Table 9/11 complete, printed and pushed to SQL")
#clear trash
rm(DodSummary)

####10 TestCounty.csv ####
#count positive, negative, indeterminate, total tests by spec date and county for ODP
TestCounty <- elr_linelist %>% 
  filter(city %in% city_file$TOWN_LC & !is.na(spec_col_date)) %>% 
  mutate(result = case_when(result == "detected" ~ "Positive",
                            result == "not detected" ~ "Negative",
                            result == "indeterminate" ~ "Indeterminate"),
         county = ifelse(is.na(county), "Pending address validation", county)) %>% 
  group_by(county, pcrag, spec_col_date, result) %>% 
  tally() %>% 
  ungroup() %>% 
  complete(county = c(counties, 'Pending address validation'), pcrag = c("pcr", "ag"),
           result = c("Positive", "Negative", "Indeterminate"),
           spec_col_date = seq.Date(as.Date("2020-02-20"), Sys.Date()-1, by = "day"), fill = list(n=0)) %>% 
  pivot_wider(id_cols = c(county, pcrag, result, spec_col_date), names_from = c(pcrag, result),
              values_from = n) %>% 
  mutate(number_of_ag_tests = ag_Indeterminate + ag_Negative + ag_Positive,
         number_of_pcr_tests = pcr_Indeterminate + pcr_Negative + pcr_Positive,
         DateUpdated = graphdate) %>% 
  rename(date = spec_col_date, number_of_ag_indeterminates = ag_Indeterminate,
         number_of_ag_positives = ag_Positive, number_of_ag_negatives = ag_Negative,
         number_of_pcr_positives = pcr_Positive, number_of_pcr_negatives = pcr_Negative, 
         number_of_pcr_indeterminates = pcr_Indeterminate) %>% 
  select(county, date, number_of_pcr_tests, number_of_pcr_positives, number_of_pcr_negatives,	number_of_pcr_indeterminates,
         number_of_ag_tests, number_of_ag_positives, number_of_ag_negatives, number_of_ag_indeterminates,DateUpdated)

#printing
if(csv_write){
  write_csv(TestCounty, paste0("L:/Outputs/", Sys.Date(), "/ODP_TestCounty.csv"))
}
if(SQL_write){
  df_to_table(TestCounty, "ODP_TestCounty", overwrite = TRUE, append = FALSE)
}
message("Table 10/11 complete, printed and pushed to SQL")

#clear trash
rm(TestCounty)

####11 CountySummarybyDate.csv####
#cases by date by county
CountySummarybyDate <- case %>% 
  filter(!is.na(county) & !is.na(date)) %>% 
  group_by(date, county) %>% 
  tally(name = "Count") %>% 
  ungroup() %>% 
  complete(date = seq.Date(as.Date("2020-03-02"), Sys.Date()-1, by = "day"), 
           county = counties, fill = list(Count = 0)) %>% 
  mutate(DateUpdated = graphdate) %>% 
  rename(County = county, Date = date) %>% 
  arrange(Date, County)

#printing
if (csv_write) {
  write_csv(CountySummarybyDate, paste0("L:/Outputs/",Sys.Date(), 
                                        "/ODP_CountySummarybyDate.csv"))
}
if(SQL_write){
  df_to_table(CountySummarybyDate, "ODP_CountySummarybyDate", overwrite = TRUE, append = FALSE)
}
message("Table 11/11 complete, printed and pushed to SQL")

#clear trash
rm(CountySummarybyDate)
gc(verbose = FALSE)
message("Gary's ODP Output process complete")