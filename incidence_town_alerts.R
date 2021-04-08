
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

if(csv_write) {
  write_csv(gary_dailyincidence, 
            paste0("L:/daily_reporting_figures_rdp/gary_csv/CTTown_Alert.csv"))
  dir.create(paste0("L:/daily_reporting_figures_rdp/yesterday/", Sys.Date(), "/avg_di"))
  write_csv(gary_dailyincidence, 
            paste0("L:/daily_reporting_figures_rdp/yesterday/", 
                   Sys.Date(),"/avg_di/avgdailyincidence.csv"))
}

```

```{r avg_incidence_gary2, eval=wedthurs, message=FALSE, warning=FALSE, include=FALSE}
lvls <- c("<5 cases per 100,000 or <5 reported cases", "5-9 cases per 100,000", "10-14 cases per 100,000", "15 or more cases per 100,000")
gdi <- gary_dailyincidence %>%
  mutate(
    RateCategory =str_trim(str_sub(RateCategory, start = 3)),
    RateCategory = factor( RateCategory, labels = lvls, levels = lvls)
  ) %>%
  arrange(desc(RateCategory))

#change for thanksgiving
# change the -7 to the day you need, then make sure the very last table is in that day's folder, It likely won't be, but just go to the very alst thursday and copy it into there and you should be good.
yestgdi <- read_csv(paste0("L:/daily_reporting_figures_rdp/yesterday/", Sys.Date()-7, "/avg_di/avgdailyincidence.csv"))%>%
  mutate(
    RateCategory =str_trim(str_sub(RateCategory, start = 3)),
    RateCategory = factor( RateCategory, labels = lvls, levels = lvls)
  ) %>%
  select(Town_No, RateCategory,CaseRate,TotalTests, PercentPositive) %>%
  rename(
    `Previous Rate Category`=RateCategory,
    `Previous Case Rate`=CaseRate,
    `Previous Test Total`=TotalTests,
    `Previous Percent_Positivity`=PercentPositive
  )


gdi <- gdi %>%
  left_join(yestgdi, by = "Town_No") %>%
  mutate(
    Case_Rate_Difference = CaseRate-`Previous Case Rate`,
    Total_Test_Difference = TotalTests-`Previous Test Total`,
    Percent_Positive_Difference = PercentPositive- `Previous Percent_Positivity`
  )


gdi2 <- gdi%>%
  select(-c(Town_No, pop, UpdateDate, ReportPeriodStartDate,ReportPeriodEndDate)) %>%
  arrange(desc(RateCategory), desc(CaseRate)) %>% 
  left_join(town_pop18, by = c("NAME" = "city")) %>% 
  mutate(PercentPop = round(pop/3572665*100, 2)) %>% 
  rename(Population=pop)

kable2 <-  gdi2 %>%
  rename(
    Town = NAME,
    Week1Cases = casesweek1,
    Week2Cases = casesweek2,
    TotalCases = totalcases
  ) %>%
  select(Town, Week1Cases, Week2Cases, TotalCases, CaseRate, RateCategory, TotalTests, PercentPositive, everything()) %>%
  select(-PercentPop) %>% 
  filter(Town != "Not_available")

#kable2
#  dir.create("tables")
if(csv_write) {
  dir.create(paste0("L:/daily_reporting_figures_rdp/tables/", Sys.Date()))
  write_csv(kable2, 
            paste0("L:/daily_reporting_figures_rdp/tables/", 
                   Sys.Date(),"/TownAlertLevelsTable.csv"))
}

```

```{r town_alert_summary,  eval=wedthurs, message=FALSE, warning=FALSE, include=FALSE}
towncatthisweek <- gdi2 %>%  group_by(RateCategory) %>%  tally(name = "Towns This Week") %>% 
  mutate(Category= factor(RateCategory, levels = lvls, labels = c("Grey", "Yellow", "Orange", "Red"))) %>% 
  select(-RateCategory)

towncatlastweek <- yestgdi %>%  group_by(`Previous Rate Category`) %>%  tally(name = "Towns Last Week") %>% 
  mutate(Category= factor(`Previous Rate Category`, levels = lvls, labels = c("Grey", "Yellow", "Orange", "Red")))%>% 
  select(-`Previous Rate Category`)
#newtowns
yestred <- yestgdi %>% 
  filter(`Previous Rate Category` == '15 or more cases per 100,000') %>% 
  select(Town_No)
redtownnew <- gdi %>% 
  filter(RateCategory == '15 or more cases per 100,000' & !Town_No %in% yestred$Town_No) %>%
  summarise(
    Category = "Red",
    "New Towns" = nrow(.)
  )

yestorange<- yestgdi %>% 
  filter(`Previous Rate Category` == "10-14 cases per 100,000") %>% 
  select(Town_No)
orangetownnew <- gdi %>% 
  filter(RateCategory == "10-14 cases per 100,000" & !Town_No %in% yestorange$Town_No) %>% 
  summarise(
    Category = "Orange",
    "New Towns" = nrow(.)
  )

yestyellow <- yestgdi %>% 
  filter(`Previous Rate Category` == "5-9 cases per 100,000" ) %>% 
  select(Town_No)
yellowtownnew <- gdi %>% 
  filter(RateCategory == "5-9 cases per 100,000"  & !Town_No %in% yestyellow$Town_No) %>% 
  summarise(
    Category = "Yellow",
    "New Towns" = nrow(.)
  )

yestgrey <- yestgdi %>% 
  filter(`Previous Rate Category` == "<5 cases per 100,000 or <5 reported cases") %>% 
  select(Town_No)
greytownnew <- gdi %>% 
  filter(RateCategory == "<5 cases per 100,000 or <5 reported cases" & !Town_No %in% yestgrey$Town_No) %>% 
  summarise(
    Category = "Grey",
    "New Towns" = nrow(.)
  )
newtowns <- bind_rows(greytownnew, yellowtownnew, orangetownnew, redtownnew)

#lost towns
yestred <- yestgdi %>% 
  filter(`Previous Rate Category` == '15 or more cases per 100,000') %>% 
  select(Town_No)
redtownlost <- gdi %>% 
  filter(RateCategory != '15 or more cases per 100,000' & Town_No %in% yestred$Town_No) %>%
  summarise(
    Category = "Red",
    "Lost Towns" = nrow(.)
  )

yestorange<- yestgdi %>% 
  filter(`Previous Rate Category` == "10-14 cases per 100,000") %>% 
  select(Town_No)
orangetownlost <- gdi %>% 
  filter(RateCategory != "10-14 cases per 100,000" & Town_No %in% yestorange$Town_No) %>% 
  summarise(
    Category = "Orange",
    "Lost Towns" = nrow(.)
  )

yestyellow <- yestgdi %>% 
  filter(`Previous Rate Category` == "5-9 cases per 100,000" ) %>% 
  select(Town_No)
yellowtownlost <- gdi %>% 
  filter(RateCategory != "5-9 cases per 100,000"  & Town_No %in% yestyellow$Town_No) %>% 
  summarise(
    Category = "Yellow",
    "Lost Towns" = nrow(.)
  )

yestgrey <- yestgdi %>% 
  filter(`Previous Rate Category` == "<5 cases per 100,000 or <5 reported cases") %>% 
  select(Town_No)
greytownlost <- gdi %>% 
  filter(RateCategory != "<5 cases per 100,000 or <5 reported cases" & Town_No %in% yestgrey$Town_No) %>% 
  summarise(
    Category = "Grey",
    "Lost Towns" = nrow(.)
  )
losttowns <- bind_rows(greytownlost, yellowtownlost, orangetownlost, redtownlost)


Summary <- towncatthisweek %>% 
  left_join(towncatlastweek) %>% 
  select(Category, `Towns This Week`, `Towns Last Week`, everything()) %>% 
  left_join(newtowns) %>% 
  left_join(losttowns) 

if(csv_write){
  write_csv(Summary, paste0("L:/daily_reporting_figures_rdp/tables/", 
                            Sys.Date(), "/SummaryAlertLevelsTable.csv"))
}