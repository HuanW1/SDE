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
rm(spec_dates, admissions, labdata, CT_daily_counts_totals)

####2 newcases+tests.csv / newcasetable ####
# thursday_case <- case %>% 
#   filter(date >= thursday_range_start & date <= thursday_range_end )
# casenew <- thursday_case 
thisweek <- epiweek(Sys.Date())
thisyear <- epiyear(Sys.Date())
beginofcurmmwr <- MMWRweek2Date(MMWRyear = thisyear, MMWRweek = thisweek, MMWRday = 1)

newcasetable <- case %>%
  filter(date>= beginofcurmmwr-14 & date < beginofcurmmwr 
         & city %in% city_file$TOWN_LC & cong_yn == "No") %>%
  group_by(city) %>% 
  tally(name = "Cases") %>% 
  ungroup() %>% 
  complete(city = city_file$TOWN_LC, fill = list('Cases' = 0)) %>% 
  inner_join(city_file %>%  select(TOWN_LC, pop_2019), 
             by = c("city" = "TOWN_LC")) %>% 
  mutate(Rate =  round(((Cases/14)/pop_2019)*100000,1),
         DateUpdated = graphdate)


testgeo_con <- DBI::dbConnect(odbc::odbc(), "epicenter")
ghost_data <- tbl(testgeo_con, sql("SELECT * FROM DPH_COVID_IMPORT.dbo.CTEDSS_GEOCODED_RECORDS"))
testgeo2 <-
  ghost_data %>%
  select(case_id, X, Y, geoid10, name, License__, DBA, type) %>%
  mutate(newName = if_else(name %in% c("", "NULL"), NA_character_, name),
         DBA = if_else(DBA %in% c("", "NULL"), NA_character_, DBA),
         facil_name = if_else(is.na(newName), DBA, name),
         cong_test = if_else(!is.na(facil_name), "Yes", "No"),
         eventid = as.numeric(case_id)) %>%
  rename(GEOID10 = geoid10, Name = name)  %>%
  select(-c("case_id", "newName", "License__", "DBA")) %>%
  arrange(facil_name) %>%
  collect() %>%
  inner_join(elr_linelist %>% filter(pcrag == "pcr"), 
             by = "eventid") %>%
  mutate(
    date = ifelse(!is.na(spec_col_date), spec_col_date, mdy(spec_rec_date)),
    date = as.Date(date, origin = ymd("1970-01-01")),
    mmwrweek = epiweek(date)) %>% 
  filter(date >= thursday_range_start & date <= thursday_range_end) %>% 
  
  
odbc::dbDisconnect(testgeo_con)


#dates for TESTS SHOULD NOT BE SET BY EVENT DATE. UPDATED TO ONLY USE SPEC COL or received DATE 10/15/2020.
# testgeo2 <- elr_linelist %>%
#   mutate(eventid = as.numeric(eventid)) %>% 
#   left_join(testgeo, by = "eventid") %>%
#   mutate(
#     date = ifelse(!is.na(spec_col_date), spec_col_date, mdy(spec_rec_date)),
#     date = as.Date(date, origin = ymd("1970-01-01")),
#     mmwrweek = epiweek(date)
#  )  %>% 
# filter(test_method %in% pcrtests2)

#limit to 14 days thursday range
test14 <- testgeo2 %>%
  filter(date >= thursday_range_start & date <= thursday_range_end )
#limit to community setting tests
test14nc <- test14 %>%
  filter(!cong_test %in% "Yes")

#count by town community only tests (exclude tests in congregate setting based on geocode results)
test14nc_ct <- test14nc %>%
  mutate(NAME = str_to_title(city)) %>% 
  group_by(NAME) %>% 
  tally(name = "Tests")
# #join to geography
# subdat14tnc <- subdat %>% 
#   left_join(
#     test14nc_ct, by = c("NAME" = "NAME")    
#   )




#drop n
#0 is 0 and 1 to 4 is <5

if(csv_write){
  write_csv(newcasetable, 
            paste0("L:/daily_reporting_figures_rdp/csv/", 
                   Sys.Date(), "/", Sys.Date(), "newcases+tests.csv"))
}






####3 daily_test_communityonly.csv / forrestdata ####
forrestdata <- testgeo2 %>%
  filter(cong_test!="Yes") %>%
  group_by(spec_col_date, pcrag, result) %>%
  tally()%>%
  pivot_wider(names_from=c("pcrag", "result"), values_from=n)%>%
  replace_na(list(pcr_detected=0, `pcr_not detected`=0, pcr_indeterminate =0, ag_detected=0, `ag_not detected`=0)) %>%
  mutate(pcrtotal = sum(pcr_detected, `pcr_not detected`, pcr_indeterminate, na.rm=TRUE),
         agtotal = sum(ag_detected, `ag_not detected`, na.rm=TRUE))

#create dataset for Forrest

if(csv_write){
  write_csv(forrestdata, 
            paste0("L:/daily_reporting_figures_rdp/csv/", 
                   Sys.Date(), "/", Sys.Date(), "dailytest_communityonly.csv"))
}

####4 communitytest_county.csv / sdedata ####
#create dataset for sde indicator for county test counts
# added epiyear
sdedata <- testgeo2 %>%
  filter(cong_test !="Yes") %>% 
  mutate(week = epiweek(spec_col_date),
         year = epiyear(spec_col_date),
         result2=ifelse(result=="detected", "Positive",
                        ifelse(result=="not detected", "Negative", "Indeterminate"))) %>% 
  group_by(county, year, week, result2) %>%
  tally() %>% 
  spread(result2, n) %>% 
  replace_na(replace=list(Positive=0, Negative=0, Indeterminate=0)) 

if(csv_write){
  write_csv(sdedata, 
            paste0("L:/daily_reporting_figures_rdp/csv/", 
                   Sys.Date(), "/", Sys.Date(), "communitytest_county.csv"))
}


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
}