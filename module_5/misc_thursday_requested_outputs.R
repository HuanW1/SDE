```{r model_data, message=FALSE, warning=FALSE, include=FALSE, eval = thursday}

spec_dates <- case %>% 
  select(spec_col_date) %>% 
  rename(spec_date = spec_col_date) %>% 
  filter(!is.na(spec_date) & spec_date >= "2020-03-01" & spec_date <= Sys.Date()) %>%
  group_by(spec_date) %>% 
  tally(name = "speccollected") %>% 
  complete(spec_date = seq.Date(as.Date(ymd("2020-03-01")), Sys.Date(), by ="day"), fill = list(speccollected = 0))

admissions <- newcha %>% 
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

modelingdata <- bind_cols(spec_dates, admissions, deathsdata, labdata) %>% 
  rename(date = spec_date) %>% 
  mutate(
    spec_cum = cumsum(speccollected),
    admit_cum = cumsum(admissions),
    death_cum = cumsum(deaths),
    test_cum = cumsum(tests)
  ) %>% 
  select(-c(admit_date, dod, spec_col_date))

if(csv_write){
  write_csv(modelingdata, 
            paste0("L:/daily_reporting_figures_rdp/gary_csv/CT_daily_counts_totals.csv"))
}

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

```