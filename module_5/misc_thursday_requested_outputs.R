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

thursday_case <- case %>% 
  filter(date >= thursday_range_start & date <= thursday_range_end )

casenew <- thursday_case 
thisweek <- epiweek(Sys.Date())
thisyear <- epiyear(Sys.Date())
beginofcurmmwr <- MMWRweek2Date(MMWRyear = thisyear, MMWRweek = thisweek, MMWRday = 1)

cases_14 <- casenew %>%
  # filter(week >= thisweek-2 & week <= thisweek-1)
  filter(date>= beginofcurmmwr-14 & date <beginofcurmmwr)
cases_14_nc <- cases_14 %>%
  filter(cong_yn == "No")

num_groups <- c("None", "1 to 5", "6 to 24", "25 to 49", "51 to 100", "101 to 200", "201 to 500", "501 to 1000", "1001 to 5000")
avgratebreaks <-  c(
  "<5 cases per 100,000 or <5 reported cases",
  "5-9 cases per 100,000",
  "10-14 cases per 100,000",
  "15 or more cases per 100,000"
)

c14nc_count <- cases_14_nc %>%
  mutate(NAME = str_to_title(city)) %>% 
  group_by(NAME) %>% 
  tally() %>% 
  complete(NAME = town_pop18$city, fill = list(n = 0)) %>% 
  filter(!NAME == "Not Available")

#c14nc_count$n[is.na(c14nc_count$n)] <- 0
c14nc_count <- c14nc_count %>% 
  full_join(town_pop18, by = c("NAME" = "city")) %>% 
  mutate(CaseRate = round(((n/14)/pop)*100000,1),
         n2 = cut(n, breaks = c(-Inf,0,5,25,50,100,200,500,1000, Inf), labels = num_groups),
         avgrate = ifelse(n < 5 | CaseRate < 5, "<5 cases per 100,000 or <5 reported cases",
                          ifelse(CaseRate >=5 & CaseRate <10, "5-9 cases per 100,000",
                                 ifelse(CaseRate >=10 & CaseRate <15, "10-14 cases per 100,000",
                                        ifelse(CaseRate >=15,  "15 or more cases per 100,000", "Not categorized"))))
  )
c14nc_count$avgrate <- factor(c14nc_count$avgrate, labels = avgratebreaks, levels = avgratebreaks )
subdat14nc <- subdat %>% 
  left_join(
    c14nc_count, by = c("NAME" = "NAME")    
  )
subdat14nc$n2[is.na(subdat14nc$n2)] <- "None"
paletteo<- c("#fcf8f8", "#fff7bc", "#fec44f", "#d95f0e", "#993404", NA) ## fix colors

#paletteo2<- c("#fcf8f8", "#fff7bc", "#fee391", "#fec44f", "#fe9929", "#ec7014", "#cc4c02", "#8c2d04", NA)
paletteo2<- c("#fcf8f8", "#fff7bc", "#fee391", "#fec44f", "#fe9929", "#ec7014", "#cc4c02", "#8c2d04", "#e31a1c", NA)

n14ncperc <- paste0("(",round(nrow(cases_14_nc)/nrow(cases_14)*100), "%)")
com <- nrow(cases_14) - nrow(cases_14_nc)
n14cperc <- paste0("(",round(com/nrow(cases_14)*100), "%)")
```


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
  inner_join(elr_linelist, by = "eventid") %>%
  mutate(
    date = ifelse(!is.na(spec_col_date), spec_col_date, mdy(spec_rec_date)),
    date = as.Date(date, origin = ymd("1970-01-01")),
    mmwrweek = epiweek(date))
  
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
#join to geography
subdat14tnc <- subdat %>% 
  left_join(
    test14nc_ct, by = c("NAME" = "NAME")    
  )


######## FIX
newcasetable <- c14nc_count %>%
  select(NAME, n, pop, CaseRate) %>%
  left_join(test14nc_ct, by = "NAME") %>% 
  mutate(Cases =if_else(is.na(n), 0, n),
         CaseRate = if_else(is.na(CaseRate), 0, CaseRate),
         Tests = as.numeric(Tests),
         Tests = if_else(is.na(Tests), 0, Tests),
         DateUpdated = Sys.Date(),
         #Cases = ifelse(Cases >0 & Cases <5, "<5", Cases)
  ) %>%
  select(-n) %>% 
  #select(-c("n.x", "n.y")) %>%
  filter(NAME != "Not_available"  & NAME != "Not Available" & NAME  %in% cc_map$CITY) %>%
  rename(Town = NAME,
         Population = pop,
         Rate = CaseRate) %>%
  select(c(Town, Population, Cases, Rate, Tests, DateUpdated)) %>% 
  arrange(Town)

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