#### Module 5 / misc thursday requested_outputs ####
#This script will generate the COVID-19 reporting thursday-specific outputs needed for historical use cases and other stakeholders
message("Thursday-specific output process will now begin")
#replaces chunk at line 3451 and chunk at line 3488
thursday_range_start <- floor_date(Sys.Date() - 12, unit = "week")
thursday_range_end <- thursday_range_start + 13

#set up by mod5 setup

####1 CT_daily_counts_totals.csv / modelingdata ####
spec_dates <- case %>% 
  select(spec_col_date) %>% 
  rename(spec_date = spec_col_date) %>% 
  filter(!is.na(spec_date) & spec_date >= "2020-03-01" & spec_date <= Sys.Date()) %>%
  group_by(spec_date) %>% 
  tally(name = "speccollected") %>% 
  complete(spec_date = seq.Date(as.Date(ymd("2020-03-01")), Sys.Date(), by ="day"), fill = list(speccollected = 0))

admissions <- longcha %>% 
  complete(admit_date = seq.Date(as.Date(ymd("2020-03-01")), Sys.Date(), by ="day"), fill = list(admissions = 0))

deathsdata <- case%>% 
  rename(dod= death_date) %>% 
  group_by(dod) %>% 
  tally(name = "deaths") %>%
  filter(!is.na(dod)& dod >= "2020-03-01"  & dod <= Sys.Date()) %>% 
  complete(dod = seq.Date(as.Date(ymd("2020-03-01")), Sys.Date(), by ="day"), fill = list(deaths = 0))

labdata <- elr_linelist %>%
  filter(!is.na(spec_col_date) & spec_col_date >= "2020-03-01"  & spec_col_date <= Sys.Date()) %>%
  group_by(spec_col_date) %>%
  tally(name = "tests") %>%
  complete(spec_col_date = seq.Date(as.Date(ymd("2020-03-01")), Sys.Date(), by ="day"), fill = list(tests = 0))

CT_daily_counts_totals <- bind_cols(spec_dates, admissions, deathsdata, labdata) %>% 
  rename(date = spec_date) %>% 
  mutate(
    spec_cum = cumsum(speccollected),
    admit_cum = cumsum(admissions),
    death_cum = cumsum(deaths),
    test_cum = cumsum(tests)
  ) %>% 
  select(-c(admit_date, dod, spec_col_date))

if(csv_write){
  write_csv(CT_daily_counts_totals, 
            paste0("L:/Outputs/",Sys.Date(),"/THR_CT_daily_counts_totals.csv"))
}
#clear trash
rm(spec_dates, admissions, labdata, CT_daily_counts_totals)
message("1/8 thursday tables have finished")

####2 newcases+tests.csv / newcasetable ####
testgeo_con <- DBI::dbConnect(odbc::odbc(), "epicenter")

geocoded_community_tests <- 
  tbl(testgeo_con, sql("SELECT * FROM DPH_COVID_IMPORT.dbo.CTEDSS_GEOCODED_RECORDS")) %>%
  select(case_id,name, DBA) %>%
  mutate(newName = if_else(name %in% c("", "NULL"), NA_character_, name),
         DBA = if_else(DBA %in% c("", "NULL"), NA_character_, DBA),
         facil_name = if_else(is.na(newName), DBA, name),
         cong_test = if_else(!is.na(facil_name), "Yes", "No"),
         eventid = as.numeric(case_id)) %>%
  select(eventid, cong_test) %>%
  collect() %>% 
  filter(!cong_test %in% "Yes") %>% 
  inner_join(elr_linelist, by = "eventid")

newcases_tests <- case %>%
  filter(date >= thursday_range_start & date <= thursday_range_end 
         & city %in% city_file$TOWN_LC & cong_yn == "No") %>%
  group_by(city) %>% 
  tally(name = "Cases") %>% 
  ungroup() %>% 
  complete(city = city_file$TOWN_LC, fill = list('Cases' = 0)) %>% 
  inner_join(city_file %>%  select(TOWN_LC, pop_2019), 
             by = c("city" = "TOWN_LC")) %>% 
  mutate(Rate =  round(((Cases/14)/pop_2019)*100000,1),
         DateUpdated = graphdate)%>% 
  rename(Town = city)

newcases_tests <- geocoded_community_tests %>%
  mutate(date = if_else(!is.na(spec_col_date), spec_col_date, mdy(spec_rec_date)),
         mmwrweek = epiweek(date)) %>% 
  filter(date >= thursday_range_start & date <= thursday_range_end) %>% 
  group_by(city) %>% 
  tally(name = "Tests") %>% 
  right_join(newcases_tests, by = c("city" = "Town")) %>% 
  rename(Town = city, Population = pop_2019) %>% 
  select(Town, Population, Cases, Rate, Tests, DateUpdated)
  
if(csv_write){
  write_csv(newcases_tests, 
            paste0("L:/Outputs/",Sys.Date(),"/THR_newcases_tests.csv"))
}
#clear trash
rm(newcases_tests)
message("2/8 thursday tables have finished")

####3 daily_test_communityonly.csv / forrestdata ####
dailytest_communityonly <- 
  geocoded_community_tests %>% 
  filter(!is.na(spec_col_date)) %>% 
  group_by(spec_col_date, pcrag, result) %>%
  tally() %>%
  ungroup() %>% 
  complete(spec_col_date = seq.Date(as.Date(ymd("2020-02-20")), Sys.Date()-1, by ="day"),
           pcrag = c("pcr", "ag"), result = c("detected", "not detected" , "indeterminate"), 
           fill = list(n = 0)) %>% 
  pivot_wider(names_from=c("pcrag", "result"), values_from=n)%>%
  mutate(pcrtotal = pcr_detected + `pcr_not detected`+ pcr_indeterminate,
         agtotal = ag_detected +`ag_not detected` + ag_indeterminate)

#create dataset for Forrest

if(csv_write){
  write_csv(dailytest_communityonly, 
            paste0("L:/Outputs/",Sys.Date(),"/THR_dailytest_communityonly.csv"))
}
#clear trash
rm(dailytest_communityonly)
message("3/8 thursday tables have finished")

####4 communitytest_county.csv / sdedata ####
#create dataset for sde indicator for county test counts
# added epiyear
communitytest_county <- 
  geocoded_community_tests %>% 
  filter(!is.na(county) & !is.na(spec_col_date)) %>% 
  mutate(week = epiweek(spec_col_date),
         year = epiyear(spec_col_date),
         result = case_when(result == "detected" ~ "Positive",
                            result == "not detected" ~ "Negative",
                            result == "indeterminate" ~ "Indeterminate")) %>% 
  group_by(county, year, week, result) %>%
  tally() %>%
  ungroup() %>% 
  complete(county = counties, result = c("Positive", "Negative", "Indeterminate"), 
           year = unique(.$year), week = unique(.$week), fill = list(n = 0)) %>% 
  pivot_wider(id_cols = c(county, year, week), names_from = result, values_from = n)

if(csv_write){
  write_csv(communitytest_county, 
            paste0("L:/Outputs/",Sys.Date(),"/THR_communitytest_county.csv"))
}
#clear trash
rm(communitytest_county)
message("4/8 thursday tables have finished")

####5 mastereventidlist.csv ####
TOI <- city_file$TOWN_LC

mastereventidlist <- case %>% 
  filter(date >= thursday_range_start & date <= thursday_range_end & !is.na(city) & city %in% city_file$TOWN_LC) %>%
  filter(cong_yn == "No") %>% 
  select(eventid, city) %>% 
  left_join(city_file %>% select(TOWN_LC, lhd), 
            by = c("city" = "TOWN_LC")) %>% 
  rename(`Health Department Name` = lhd) %>% 
  arrange(`Health Department Name`, city)

if(csv_write){
  write_csv(mastereventidlist, 
            paste0("L:/Outputs/", Sys.Date(), 
                          "/eventid_lists/mastereventidlist.csv"))
}
if(csv_write) {
  for(i in TOI){
    mastereventidlisti <- mastereventidlist %>%  
      filter(city == i) %>% 
      select(eventid)

    write_csv(mastereventidlisti, 
              paste0("L:/Outputs/", Sys.Date(),"/eventid_lists/", 
                            city_file$lhd[city_file$TOWN_LC == i],
                            "_", i, "_last2week.csv"))  
  }
  rm(mastereventidlisti)
}
#clear trash
rm(TOI,  mastereventidlist)
message("5ish/8 thursday tables have finished")

####6 CTTown_Alert (for gary)  ####
CTTown_Alert <- 
  geocoded_community_tests %>% 
    mutate(date = if_else(!is.na(spec_col_date), spec_col_date, mdy(spec_rec_date))) %>% 
    filter(date >= thursday_range_start & date <= thursday_range_end & city %in% city_file$TOWN_LC ) %>% 
    group_by(city, result) %>% 
    tally() %>%
    ungroup() %>% 
    complete(city = city_file$TOWN_LC, result = c("detected", "not detected", "indeterminate"),
             fill = list(n=0)) %>% 
    pivot_wider(id_cols = c(city, result),names_from = result, values_from = n) %>% 
    replace_na(replace = list(detected = 0, `not detected` = 0, indeterminate = 0)) %>%
    mutate(PercentPositive = round(detected/(detected +`not detected`)*100, 1),
           TotalTests = detected +`not detected`+ indeterminate) %>% 
    select(city, TotalTests,PercentPositive)

CTTown_Alert  <- 
  case %>%
  filter(date >= thursday_range_start & date <= thursday_range_end 
         & city %in% city_file$TOWN_LC & cong_yn == "No") %>%
  mutate(week = epiweek(date), 
         year = epiyear(date)) %>% 
  group_by(city, week, year) %>% 
  tally() %>%
  ungroup() %>% 
  complete(city = city_file$TOWN_LC, week = unique(.$week), year = unique(.$year),
           fill = list(n = 0)) %>% 
  inner_join(city_file %>%  select(TOWN_LC, pop_2019), 
             by = c("city" = "TOWN_LC")) %>% 
  group_by(city) %>% 
  mutate(CaseRate =  round(((sum(n)/14)/pop_2019)*100000,1))%>%
  arrange(city, year, week) %>% 
  mutate(caseweek = row_number(),
         DateUpdated = graphdate) %>% 
  ungroup() %>% 
  select(-c(year, week)) %>% 
  pivot_wider(id_cols = c(city, pop_2019, CaseRate, DateUpdated, caseweek),names_from = caseweek, values_from = n) %>% 
  rename(caseweek1 = `1`, caseweek2 = `2`) %>% 
  mutate(totalcases = caseweek1 + caseweek2,
         RateCategory = case_when(totalcases < 5 | CaseRate < 5 ~ "1. <5 cases per 100,000 or <5 reported cases",
                                  CaseRate >=5 & CaseRate <10 ~ "2. 5-9 cases per 100,000",
                                  CaseRate >=10 & CaseRate <15 ~ "3. 10-14 cases per 100,000",
                                  CaseRate >=15 ~ "4. 15 or more cases per 100,000"),
         ReportPeriodStartDate = thursday_range_start,
         ReportPeriodEndDate = thursday_range_end
         ) %>% 
    inner_join(city_file %>% select(c(TOWN_LC, TOWNNO)),
               by = c("city" = "TOWN_LC")) %>% 
    inner_join(CTTown_Alert, by = "city") %>% 
  select(TOWNNO, city, pop_2019, caseweek1, caseweek2, totalcases, CaseRate, RateCategory, TotalTests, PercentPositive, DateUpdated, ReportPeriodStartDate, ReportPeriodEndDate) %>% 
  rename(Town_No = TOWNNO, Town = city)
    
if(csv_write) {
  write_csv(CTTown_Alert, 
            paste0("L:/Outputs/",Sys.Date(),"/ODP_CTTown_Alert.csv"))
}

if(SQL_write){
  df_to_table(CTTown_Alert, "ODP_CTTown_Alert", overwrite = FALSE, append = TRUE)
}  
message("6/8 thursday tables have finished and this extra special one has gone up onto SQL022")

####7 TownAlertLevelsTable  (for leadership) ########
lvls <- c("<5 cases per 100,000 or <5 reported cases", "5-9 cases per 100,000", "10-14 cases per 100,000", "15 or more cases per 100,000")
TownAlertLevelsTable<- CTTown_Alert %>%
  mutate(
    RateCategory =str_trim(str_sub(RateCategory, start = 3)),
    RateCategory = factor( RateCategory, labels = lvls, levels = lvls)
  ) %>%
  arrange(desc(RateCategory))

lastdate <- 
  tbl(testgeo_con, sql("SELECT * FROM DPH_COVID_IMPORT.dbo.ODP_CTTown_Alert")) %>%
  select(DateUpdated) %>% 
  collect() %>% 
  unique() %>% 
  arrange(desc(DateUpdated)) %>% 
  mutate(rank = row_number())
rank <- min(max(lastdate$rank), 2) #take rank 2, 2nd most recent date, or rank 1 if there is only 1 rank
lastdate <- lastdate %>%filter(rank ==rank) %>%  pull(DateUpdated)
  
TownAlertLevelsTable <- 
  tbl(testgeo_con, sql("SELECT * FROM DPH_COVID_IMPORT.dbo.ODP_CTTown_Alert")) %>%
  filter(DateUpdated == lastdate) %>% 
  collect() %>%
  mutate(
    RateCategory =str_trim(str_sub(RateCategory, start = 3)),
    RateCategory = factor( RateCategory, labels = lvls, levels = lvls)
  ) %>%
  arrange(desc(RateCategory)) %>%
  select(Town_No, RateCategory,CaseRate,TotalTests, PercentPositive) %>%
  rename(
    `Previous Rate Category`=RateCategory,
    `Previous Case Rate`=CaseRate,
    `Previous Test Total`=TotalTests,
    `Previous Percent_Positivity`=PercentPositive
  ) %>% 
  right_join(TownAlertLevelsTable, by = "Town_No") %>%
  mutate(
    Case_Rate_Difference = CaseRate -`Previous Case Rate`,
    Total_Test_Difference = TotalTests -`Previous Test Total`,
    Percent_Positive_Difference = PercentPositive - `Previous Percent_Positivity`
  ) %>%
  select(-c(Town_No, DateUpdated, ReportPeriodStartDate,ReportPeriodEndDate)) %>%
  arrange(desc(RateCategory), desc(CaseRate))%>% 
  rename(Pop_2019 = pop_2019, Week1Cases = caseweek1, 
         Week2Cases = caseweek2, TotalCases = totalcases) %>% 
  select(Town, Week1Cases, Week2Cases, TotalCases, CaseRate, 
         RateCategory, TotalTests, PercentPositive, everything())

if(csv_write) {
  write_csv(TownAlertLevelsTable, 
            paste0("L:/Outputs/",Sys.Date(),"/THR_TownAlertLevelsTable.csv"))
}
#clear trash
rm(rank, lastdate)
message("7/8 thursday tables have finished")

####8 SummaryAlertLevelsTable ####

SummaryAlertLevelsTable <- TownAlertLevelsTable %>% 
  select(RateCategory) %>% 
  mutate(cat = factor(RateCategory, levels = lvls, labels = c("Grey", "Yellow", "Orange", "Red"))) %>% 
  select(-c(RateCategory)) %>% 
  group_by(cat) %>% 
  tally(name = "catn") %>% 
  inner_join(TownAlertLevelsTable %>% 
               select(`Previous Rate Category`) %>% 
               mutate(cat = factor(`Previous Rate Category`, levels = lvls, labels = c("Grey", "Yellow", "Orange", "Red"))) %>% 
               select(-c(`Previous Rate Category`)) %>% 
               group_by(cat) %>% 
               tally(name = "pcatn"),
             by = "cat") %>% 
  ungroup() %>% 
  mutate(`Change From Last Week` = catn-pcatn) %>% 
  rename(Category = cat, `Towns This Week` = catn,
         `Towns Last Week` = pcatn)
 
if(csv_write){
  write_csv(SummaryAlertLevelsTable, paste0("L:/Outputs/",Sys.Date(),
                                            "/THR_SummaryAlertLevelsTable.csv"))
}

rm(geocoded_community_tests, thursday_range_start, thursday_range_end,
   SummaryAlertLevelsTable,TownAlertLevelsTable)
odbc::dbDisconnect(testgeo_con)
gc(verbose =  FALSE)
message("8/8 thursday tables have finished")
message("Thursday Outputs are done!")