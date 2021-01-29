#### This file just has older PROBABLY unneeded code snippets

# df_admitdates<- df %>% 
#   select(c(eventid, admit_date)) %>%
#   filter(!is.na(admit_date)) %>%
#   filter(str_detect(admit_date, ",") & admit_date != ",") %>% 
#   group_by(eventid)%>%
#   slice(1L) %>%
#   mutate(
#     admitsplit = str_split(admit_date, pattern = ",")
#   ) %>% 
#   unnest(admitsplit) %>%
#   select(-admit_date) %>% 
#   mutate(admitsplit = as.Date(mdy(admitsplit))) %>% 
#   group_by(eventid) %>% 
#   arrange(admitsplit) %>% 
#   slice(1L) %>% 
#   rename(admit_date = admitsplit)
# 
# df_dischargedates<- df %>% 
#   select(c(eventid, discharge_date)) %>%
#   filter(!is.na(discharge_date)) %>%
#   filter(str_detect(discharge_date, ",") & discharge_date != ",") %>% 
#   group_by(eventid) %>%
#   slice(1L) %>%
#   mutate(
#     dischargesplit = str_split(discharge_date, pattern = ",")
#   ) %>% 
#   unnest(dischargesplit) %>%
#   select(-discharge_date) %>% 
#   mutate(dischargesplit = as.Date(mdy(dischargesplit))) %>% 
#   group_by(eventid) %>% 
#   arrange(dischargesplit) %>% 
#   slice(1L) %>% 
#   rename(discharge_date = dischargesplit)


# df <- df %>% 
# select(-c(admit_date, discharge_date)) %>%
#   mutate_at(.vars = vars(contains("_date")), ~as.Date(mdy(.))) %>% 
#   left_join(df_dischargedates, by = c("eventid" = "eventid")) %>% 
#   left_join(df_admitdates, by = c("eventid" = "eventid")) %>% 
#   filter(result != "Test not done"| is.na(result)) 

################  DF DOWNSTREAM EDITS SECTION  #################

#wiping out deaths
# deathchangelist <- c()#add eventids separated by commas here, or something similiar to get a list
# df$covid_death[df$eventid %in% deathchangelist] <- NA
# 
# df$age[df$eventid == 100894768]<- 88
# df$city[df$eventid == 100894768]<- "Trumbull"
# df$disease_status[df$eventid %in% c(101027326, 100906919)] <- "Probable"
# df$covid_death[df$eventid %in% c(102800742)]<- NA
# df$covid_death[df$eventid %in% c(100906919, 100926548, 101027326, 101086633)]<- "Yes"
# 
# deathchangelist <- c(102739821, 103224555, 100961381, 103963349, 103922717, 101518798, 102824455, 102849708, 103910753, 102680157, 101944009, 104384369, 103384146, 104641759) #add eventids separated by commas here, or something similiar to get a list
# 
# df$outcome[df$eventid %in% deathchangelist] <- NA
# 
# #12/28/2020 LE: death date edits#
# df$death_date[df$eventid %in% c(103553124, 103785921, 101111721, 101177512, 102165382)]<- "2020-12-24"
# df$death_date[df$eventid %in% 101000245]<- "2020-05-24"
# df$death_date[df$eventid %in% 101305279]<- "2020-12-23"
# df$death_date[df$eventid %in% 100862191]<- "2020-03-23"
# df$death_date[df$eventid %in% 104384393]<- "2020-12-18"
# df$death_date[df$eventid %in% 101043310]<- "2020-11-20"
# df$death_date[df$eventid %in% c(102567239, 103553130)]<- "2020-12-25"
# df$death_date[df$eventid %in% 102901517]<- "2020-12-21"


#go into text, add to vectors later
# count(test14, city=="Not_available")
# count(test14, result)
# counts<-count(test14nc, city=="Not Available")
# counts<- count(test14nc, result)


# ```{r syndromic_plot_example, eval=FALSE}

#OLD SYNDROMIC TEXT MOVE TITLE AND PARAGRAPH BACK OUT OF THIS CHUNK INTO SPACE ABOVE TO INCLUDE IN REPORT
### Syndromic Surveillance  
#The Department of Public Health EpiCenter hospital emergency department (ED) syndromic surveillance system receives near real-time #information from all 38 licensed, hospital EDs in Connecticut. The percentage of individuals seeking care for "unexplained fever/flu" #syndrome ED visits is monitored to identify individuals that may have symptoms consistent with COVID-19; this differs from the #"fever/flu" syndrome in the weekly influenza report since patients with a known diagnosis (e.g., influenza, strep throat) are not #included. **The syndromic surveillance systems suggest that many more people are still seeking care for respiratory illnesses, including #COVID-19.**  

#sys_file <- list.files("sys_figure_here", pattern = ".png")
#sys_path <- file.path("C:", "Users", "asene", "Desktop", "daily_reporting_figures", "sys_figure_here", sys_file)
#sys_path <- paste0(getwd(),"/sys_figure_here/",sys_file)
#include_graphics(path =sys_path, dpi =300)
# ```

# StateSummary <-  tibble(
#   "Measure" = c("Total COVID-19 Cases","Total COVID-19-Associated Deaths",  "Patients Currently Hospitalized with COVID-19", "COVID-19 Molecular Tests Reported", "COVID-19 Ag Tests Reported"),
#   "Total" = c(nrow(case),dec, cha_c$today[cha_c$State == 'TOTAL'], total_pcr_tests, total_ag_tests),
#   ChangeDirection = chg_yst2$sign,
#   Change =chg_yst2$delta,
#   DateUpdated = c(graphdate,graphdate,graphdate,graphdate,graphdate)
#                         )


#### NEW WIP #####
#agegroup summary by county  
# agstotalcases <- case %>% 
#   filter(!is.na(age_group) & !is.na(county)) %>% 
#   group_by(county, age_group) %>% 
#   tally(name = "TotalCases")
# agsconfcases <- case %>% 
#   filter(!is.na(age_group) & disease_status == "Confirmed") %>% 
#   group_by(age_group) %>% 
#     tally(name = "ConfCases")
# 
# agsprobcases<- case %>% 
#   filter(!is.na(age_group) & !is.na(county) & disease_status == "Probable") %>% 
#   group_by(county, age_group) %>% 
#   tally(name = "ProbCases")
# agstotaldeaths <- case %>% 
#   filter(!is.na(age_group) & !is.na(county) & outcome == "Died") %>% 
#    group_by(county, age_group) %>%  
#   tally(name = "TotalDeaths") %>% 
#   complete(age_group = unique(case$age_group), fill = list("TotalDeaths" = 0, county = unique(case$county)))
# 
# agsconfdeaths <- case %>% 
#   filter(!is.na(age_group) & disease_status == "Confirmed" & outcome == "Died") %>% 
#   group_by(age_group) %>% 
#   tally(name = "ConfDeaths")%>% 
#   complete(age_group = unique(case$age_group), fill = list("ConfDeaths" = 0))
# 
# agsprobdeaths <- case %>% 
#   filter(!is.na(age_group) & disease_status == "Probable" & outcome == "Died") %>% 
#   group_by(age_group) %>% 
#   tally(name = "ProbDeaths") %>% 
#   complete(age_group = unique(case$age_group), fill = list(ProbDeaths = 0))
# 
# 
# agstotalrate <- case %>% 
#   filter(!is.na(age_group)) %>% 
#   group_by(age_group) %>% 
#   tally(name = "TotalCases") %>% 
#   left_join(pop, by = c("age_group" = "age_g")) %>% 
#   mutate(TotalCaseRate = round((TotalCases/total)*100000))
# 
# 
# AgeGroupSummary <- tibble(
#   AgeGroups = agstotalcases$age_group,
#   TotalCases =agstotalcases$TotalCases,
#   ConfirmedCases = agsconfcases$ConfCases,
#   ProbableCases = agsprobcases$ProbCases,
#   TotalDeaths = agstotaldeaths$TotalDeaths,
#   ConfirmedDeaths = agsconfdeaths$ConfDeaths,
#   ProbableDeaths = agsprobdeaths$ProbDeaths,
#   TotalCaseRate = agstotalrate$TotalCaseRate,
#   DateUpdated = graphdate
# ) %>% 
#   mutate(AgeGroups = as.character(AgeGroups))
# AgeGroupSummary$AgeGroups[AgeGroupSummary$AgeGroups == ">=80"] <- "80 and older"
# 



# kable2 <-  gdi2 %>%
#      rename(
#        Town = NAME,
#        Week1Cases = casesweek1,
#        Week2Cases = casesweek2,
#        TotalCases = totalcases
#             ) %>%
#      select(Town, Week1Cases, Week2Cases, TotalCases, CaseRate, RateCategory, TotalTests, PercentPositive, everything()) %>%
#     select(-PercentPop) %>% 
#      kbl() %>%
#      kable_styling(bootstrap_options = c("striped", "condensed", "responsive")) %>%
#      column_spec( column = 1:15,color = "black") %>%
#      column_spec(
#        6,
#        background =    ifelse(gdi2$RateCategory =="<5 cases per 100,000 or <5 reported cases", "#d3d3d3",
#                              ifelse(gdi2$RateCategory =="5-9 cases per 100,000", "#ffffb2",
#                                     ifelse(gdi2$RateCategory =="10-14 cases per 100,000", "#fd8d3c", "#e31a1c")))
# 
#                 ) %>% 
#   kable_classic(full_width = F, html_font = "Cambria")








