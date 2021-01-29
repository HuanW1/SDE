#Compare cases or deaths from today to a previous day.
.libPaths(c("L:/library", .libPaths()))
library(tidyverse, lib.loc = .libPaths()[1])
library(lubridate, lib.loc = .libPaths()[1])
#Compare cases
#read in data from last day - update date if not yesterday

casefiles <-ymd(list.files("L:\\daily_reporting_figures_rdp\\csv"))
oldcasefile <- casefiles[length(casefiles)-1]
newcasefile<- casefiles[length(casefiles)]
oldcases <- read_csv(paste0("L:/daily_reporting_figures_rdp/csv/", oldcasefile, "/", oldcasefile, "cases_wphi.csv"))

#read in data from today to compare
newcases <-read_csv(paste0("L:/daily_reporting_figures_rdp/csv/", newcasefile, "/", newcasefile, "cases_wphi.csv"))

#find cases in the new dataset not in the old dataset
newcasestoday <- newcases %>% 
  filter(!eventid %in% oldcases$eventid)

missingcases <- oldcases %>% 
  filter(!eventid %in% newcases$eventid)

newcaseswk <- newcasestoday %>% 
  group_by(spec_col_date, disease_status) %>% 
  tally()


#Compare deaths
#read in data from last day - update date if not yesterday
olddeaths <- oldcases %>% 
  filter(outcome=="Died")

#read in data from today to compare
newdeaths <- newcases %>% 
  filter(outcome=="Died")

#find cases in the new dataset not in the old dataset
newdeathstoday <- newdeaths %>% 
  filter(!eventid %in% olddeaths$eventid)

missingdeaths <- olddeaths %>% 
  filter(!eventid %in% newdeaths$eventid)

write_csv(newdeathstoday,  "newdeathstoday.csv")
write_csv(missingdeaths,  "missingdeathstoday.csv")

# #Compare lab results
# #read old ELR line list
# oldelr <- read_csv("elr_linelists/elr_linelist2020-10-09.csv")
# 
# olddate <- oldelr %>% 
#   group_by(lab_result_mod_date) %>% 
#   tally() %>% 
#   rename(oldn=n)
# 
# #read new ELR line list 
# newelr <- read_csv("elr_linelists/elr_linelist2020-10-12.csv")
# 
# newdate <-newelr %>% 
#   #group_by(spec_col_date, result2) %>% 
#   mutate(lab_result_mod_date = mdy(lab_result_mod_date)) %>% 
#   group_by(lab_result_mod_date) %>% 
#   tally() %>% 
#   rename(newn=n)
# 
# comparedate <- newdate %>% 
#   left_join (olddate, by="lab_result_mod_date") %>% 
#   mutate(diff=newn-oldn)
# 
# newer <- newelr %>% 
#   filter(mdy(lab_result_mod_date) >= mdy("10/09/2020"))
# 
# 
# newlab <- newelr %>% 
#   group_by(name) %>% 
#   tally()
# 
# oldlab <-oldelr %>% 
#   group_by(name) %>% 
#   tally()
# 
# namecompare <- newlab %>% 
#   left_join(oldlab, by ="name") %>% 
#   mutate(diff=n.x-n.y)
# 
# #plot labs over days
# counttests <- newelr %>% 
#   group_by(spec_col_date, result) %>% 
#   tally() %>% 
#   filter(spec_col_date >=mdy("03-01-2020"))
# 
# ggplot(counttests)+
#   geom_col(aes(x=spec_col_date, y=n, fill=result))+
#   theme_bw()+
#   labs(
#     x="Number of Tests",
#     y="Specimen Collection Date",
#     title="Number of PCR COVID-19 Tests Reported to DPH by Specimen Collection Date",
#     fill="Test result"
#   )
# 
# # #count tests per month
# # testmonth <- elr_linelist %>% 
# #   group_by(month(spec_col_date)) %>% 
# #   tally()
# # 
# # #count tests + % pos in past 7 days
# # testday <- elr_linelist %>% 
# #   filter(spec_col_date >= ymd("2020-09-25")) %>% 
# #   group_by(result2) %>% 
# #   tally()
#   
# 
# #compare auto vs. manual
# # man <- read_csv("csv/2020-09-14/2020-09-14cases_wphi.csv")
# # auto<- read_csv("csv/2020-09-14/2020-09-14cases_wphi_autopull.csv")
# # 
# # notauto <- man %>% 
# #   filter(!eventid %in% auto$eventid)
# # notman <- auto %>% 
# #   filter(!eventid %in% man$eventid)
# # 
# # deadauto<- auto %>% 
# #   filter(outcome=="Died")
# # deadman <- man %>% 
# #   filter(outcome=="Died")
# # 
# # nadead <- deadman %>% 
# #   filter(!eventid %in% auto$eventid)
# # madead <- deadauto %>% 
# #   filter(!eventid %in% man$eventid)
# 
# #new cases by town
# newcasescity <- newcasestoday %>% 
#   group_by(city) %>% 
#   tally()
# newcasescounty <- newcasestoday %>% 
#   group_by(county) %>% 
#   tally()
# count(newcasestoday, disease_status)
# dates<-count(newcasestoday, disease_status, spec_col_date)
# 
# write_csv(newcasestoday, "csv/2020-10-13/2020-10-13_newcases.csv")
