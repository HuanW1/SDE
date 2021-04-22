#### Module 5 / misc thursday requested_outputs ####
#This script will generate the COVID-19 reporting thursday-specific outputs needed for historical use cases and other stakeholders
message("Thursday-specific output process will now begin.  This usually takes X minutes")
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
            paste0("L:/daily_reporting_figures_rdp/gary_csv/CT_daily_counts_totals.csv"))
}
#clear trash
rm(spec_dates, admissions, labdata, CT_daily_counts_totals)

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
            paste0("L:/daily_reporting_figures_rdp/csv/", 
                   Sys.Date(), "/", Sys.Date(), "newcases+tests.csv"))
}
#clear trash
rm(newcases_tests)

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
            paste0("L:/daily_reporting_figures_rdp/csv/", 
                   Sys.Date(), "/", Sys.Date(), "dailytest_communityonly.csv"))
}
#clear trash
rm(dailytest_communityonly)

####4 communitytest_county.csv / sdedata ####
#create dataset for sde indicator for county test counts
# added epiyear
communitytest_county <- 
  geocoded_community_tests %>% 
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
            paste0("L:/daily_reporting_figures_rdp/csv/", 
                   Sys.Date(), "/", Sys.Date(), "communitytest_county.csv"))
}
#clear trash
rm(communitytest_county)

####5 mastereventidlist.csv ####
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
  rm(mastereventidlisti)
}
#clear trash
rm(TOI, range_start, range_end, mastereventidlist)


####6 incidence_town_alerts ####
gary_dailyincidence <- 
  geocoded_community_tests %>% 
    mutate(date = if_else(!is.na(spec_col_date), spec_col_date, mdy(spec_rec_date))) %>% 
    filter(date >= thursday_range_start & date <= thursday_range_end ) %>% 
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


testcases <- case %>%
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
  mutate(Rate =  round(((Cases/14)/pop_2019)*100000,1),
         DateUpdated = graphdate)%>% 
  rename(Town = city)


gary_dailyincidence <- cases_14_nc %>% 
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
  left_join(city_file, by = c("townname" = "ANPSADPI")) %>%  #replace with city list
  rename(Town_No = TOWNNO) %>% 
  select(Town_No, NAME, pop, casesweek1, casesweek2, totalcases, CaseRate, 
         RateCategory, TotalTests, PercentPositive, UpdateDate, 
         ReportPeriodStartDate, ReportPeriodEndDate) %>%
  filter(!NAME == "Not Available")














rm(geocoded_community_tests, thursday_range_start, thursday_range_end)
odbc::dbDisconnect(testgeo_con)
gc(verbose =  FALSE)
message("Thursday Outputs are done!")