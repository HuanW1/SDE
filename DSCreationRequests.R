#Code to produce custom datasets upon request

###########################################################
#Date 11/12/2020
#Requestor Sydney
#Find percent positive over time for only PCR tests reported by elr

pctpos<-elr_linelist %>% 
  #keep only specimens tested during 2/28/2020-11/9 (because fairly complete reporting for that period)
  filter(spec_col_date >= mdy("02-28-2020") & spec_col_date <= mdy("11-09-2020")) %>% 
  #keep only pcr tests and those reported via elr 
  filter(pcrag=="pcr" & `New Lab Result Received via ELR`=="Yes") %>% 
  #count # of tests by date of collection and result
  group_by(spec_col_date, result) %>% 
  tally() %>%
  #reformat the data to make it wider (1 observation per collection date with # of positive and # of negative tests)
  pivot_wider(names_from=result, values_from=n) %>% 
  #replace missing values with zeros (since missing indicates no tests with that result on that date)
  replace_na(list(detected=0, `not detected`=0)) %>%
  #calculate the % positive (positive tests / positive+negative tests)
  mutate(pctpos = (detected/(detected + `not detected`))*100) 


ggplot (pctpos)+
  #this line says "make a line graph with specimen collection on the x axis, and percent positive on the y axis. make the line a little thicker by setting size =2"
  geom_line(aes(x=spec_col_date, y=pctpos), size=2)+
  #this line says 'use the classic theme, which is like a built in style or format related to the colors in the graph, background, grid lines, etc.'
  theme_classic()+
  #this line says "make the x-axis a date axis with lables at each 1 month interval in the format of "Mon DD" (e.g., Jun 01)."
  scale_x_date(date_breaks="1 month", date_labels="%b %d")+
  #this line says "make the y-axis go from 0 to 50 with breaks every 5 units
  scale_y_continuous(breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50))+
  #this line says "put in a horizontal reference line at 5% positive that is dotted and blue"
  geom_hline(yintercept=5, linetype="dotted", color="blue")+
  #these lines say "give the x and y axes these labels and add a title at the top
  labs(
    x="Specimen Collection Date",
    y="Percent Positive",
    title="Percentage of Positive PCR tests by Date of Specimen Collection \nAmong Tests Reported via ELR"
  )




###########################################################
#Date 11/9/2020
#Requestor Steve
#Are there still cases that are geocoded but missing info on census tract? 

#use alltestgeo dataset from 11/10/2020 that Tom uploaded and was created in running the daily report
#look for cases where there is an x,y coordinate pair assigned (i.e., case was geocoded) but there is no census tract ID assigned
#(i.e., geoid10 is missing)

#look at range of x, y and geoid 10
rangex <- count(alltestgeo, X)
rangey <- count(alltestgeo, Y)
rangegeo <- count(alltestgeo, geoid10)

missingtract<- alltestgeo %>% 
  mutate(Xcat = ifelse(is.na(X), "Missing",
                        ifelse(X==0.0, "Zero", "Coordinate")),
         Ycat = ifelse(is.na(Y), "Missing",
                        ifelse(Y==0.0, "Zero", "Coordinate")),
         Geocat = ifelse(is.na(geoid10), "Missing",
                          ifelse(geoid10=="NULL", "NULL", 
                                  ifelse(geoid10=="", "Blank", "Tract ID"))))

rangexygeo<-count(missingtract, Xcat, Ycat, Geocat)

#1270881 tests in geocoded dataset



###########################################################
#Date 11/9/2020
#Requestor Jess 
#How many new cases in a specific town?

#read in cases with phi
newcases <- read_csv(paste0("csv/", Sys.Date(), "/", Sys.Date(), "cases_wphi.csv"))

#filter to town and weeks of interest
town<-"Bristol"
last2<-c(epiweek(Sys.Date())-1, epiweek(Sys.Date())-2)

#all cases in town in last 2 weeks
townlast2 <- newcases %>% 
  filter(city==town & mmwrweek %in% last2)

#community cases only in last 2 weeks
townlast2community <- townlast2 %>% 
 # mutate(cong_yn = if_else((str_detect(cong_exposure_type, "Reside")) &
 #                            (str_detect(cong_setting, "Jail / Prison") | str_detect(cong_setting, "Long term care facility") | str_detect(cong_setting, "Assisted Living Facility")),
 #                          "Yes", "No", missing = "No")) %>% 
  filter(cong_yn=="No")


###########################################################
#Date 11/5/2020
#Requestor Josh 
#How many of new cases and tests are Ag related? 

#read in data from last day - update date if not yesterday
oldcases <- read_csv(paste0("csv/", Sys.Date()-1, "/", Sys.Date()-1, "cases_wphi.csv"))

#read in data from today to compare
case <- read_csv(paste0("csv/", Sys.Date(), "/", Sys.Date(), "cases_wphi.csv"))

#find cases in the new dataset not in the old dataset
newcasestoday <- newcases %>% 
  filter(!eventid %in% oldcases$eventid)

missingcases <- oldcases %>% 
  filter(!eventid %in% newcases$eventid)
count(newcasestoday, disease_status)
count(missingcases, disease_status)


#deaths
deaths<-newcases %>% 
  filter(outcome=="Died") %>% 
  group_by(death_date) %>% 
  tally()

#how many PCR and how many Ag labs?
labs<- read_csv("elr_linelists/elr_linelist2020-11-05.csv")

pcr<-labs %>% 
  filter(test_method %in% pcrtests2)
count(pcr, `New Lab Result Received via ELR`)

ag<-labs %>% 
  filter(test_method %in% agtests)

tests <- count(labs, test_method)

###########################################################
#Date 11/3/2020
#Requestor John Brady (CHA)
#Count PCR tests by lab type and ordering provider type by month and patient county

elr<- read_csv("elr_linelists/elr_linelist2020-11-03.csv")

labs <- elr %>% 
  group_by(lab_facility, name) %>% 
  tally()
#write_csv(labs, "csv/2020-11-03/2020-11-03lablist.csv")
facils <- elr %>% 
  group_by(auth_facility) %>% 
  tally()
#write_csv(facils, "csv/2020-11-03/2020-11-03facils.csv")

#read in with types
facils<- read_csv("csv/2020-11-03/2020-11-03facils.csv")
labs <- read_csv("csv/2020-11-03/2020-11-03lablist.csv")

#join in with elr 
elr2 <- elr %>% 
  left_join(facils, by="auth_facility") %>% 
  rename(order_type=type) %>%
  left_join(labs, by=c("lab_facility", "name")) %>% 
  rename(lab_type=type) %>% 
  mutate(order_type = if_else(is.na(order_type) | order_type=="unknown" | order_type =="oth", "other", 
                              if_else(order_type=="urgent care", "urgent care/point of care", order_type)),
         lab_type = if_else(is.na(lab_type), "unknown", lab_type),
         month=month(spec_col_date)) %>% 
  group_by(lab_type, order_type, month, county) %>% 
  tally()

write_csv(elr2, "csv/2020-11-03/2020-11-03testbylabandorder.csv")

###########################################################
#Date 11/3/2020
#Requestor Josh Geballe
#What is the commonality in recent probable cases? are these antigen positives? 

cases <- read_csv("csv/2020-11-03/2020-11-03cases_wphi.csv") %>% 
  filter(disease_status=="Probable")

by_week<-count(cases, mmwrweek)
ggplot(by_week)+
  geom_col(aes(x=mmwrweek, y=n))

recentprobables <- cases %>% 
  filter(mmwrweek >=40) %>% 
  filter(! test %in% c("SARS CoV 2 Ag (Quidel Sofia/Lumira)", "SARS CoV 2 Ag rapid IA (BD Veritor/BinaxNOW)"))

tests<-count(recentprobables, test, result)
#among 1367 recent probables, 695 had positive Ag test

#for remaining 672, search line list to see if they ended up there
#run dq code through line 140 to create df. then apply this. 
othertest <- df %>% 
  filter(`Case ID` %in% recentprobables$eventid & 
           Test %in% c("SARS CoV 2 Ag (Quidel Sofia/Lumira)", "SARS CoV 2 Ag rapid IA (BD Veritor/BinaxNOW)")) %>% 
  group_by(`Case ID`) %>% 
  arrange(desc(Result)) %>% 
  slice(1L) %>% 
  ungroup() %>% 
  filter(Result %in% c("Positive", "Detected"))
count(othertest, Result)
#127 more with positive antigen test

recentprobables2 <- recentprobables %>% 
  filter(!eventid %in% othertest$`Case ID`)

#1367 recent probables, 823 with positive Ag result (60%)
#remaining 544 cases:
#close look at first 10: 
  #1. No labs, wizard says negative - why probable??? (102920578)
  #2. negative result reported by AFC - why probable??? (102673277)
  #3. CRF says positive we don't have result from Quinnipiac
  #4. neg lab from OCT; originally created in march
  #5. no lab info; not tested, afc
  #6. no lab info; pos docs
  #7. neg in Aug; pos crf from sept
  #8. no lab info; pos docs
  #9. no lab info; pos prohealth
  #10. no lab info; pos docs
#if 60% of those without pos Ag labs are actually pos, then overall 84% of recent probables have pos ag result


###########################################################
#Date 11/3/2020
#Requestor Stephen Civitelli
#Count of non-congregate cases in Wallingford

cases <- read_csv("csv/2020-11-03/2020-11-03cases_wphi.csv") %>% 
  filter(city=="Wallingford" & mmwrweek %in% c(43, 44)) %>% 
  mutate(cong_yn = if_else((str_detect(cong_exposure_type, "Reside")) &
                             (str_detect(cong_setting, "Jail / Prison") | str_detect(cong_setting, "Long term care facility") | str_detect(cong_setting, "Assisted Living Facility")),
                           "Yes", "No", missing = "No")) %>% 
  filter(cong_yn=="No")
count(cases, mmwrweek, disease_status)
write_csv(cases, "csv/2020-11-03/2020-11-03wallingford.csv")

###########################################################
#Date 10/22/2020
#Requestor Matt
#look for college cases

pcr <- read_csv("elr_linelists/elr_linelist2020-10-29.csv")

recent<-pcr %>% 
  filter(spec_col_date >=mdy("10-01-2020"))

facilt<-count(recent, auth_facility)

college <- recent %>% 
  filter(auth_facility %in% c("Albertus Magnus College", "Connecticut College", "Mitchell College",
                              "Trinity College", "Wesleyan University", "Quinnipiac University",
                              "University of Connecticut", "University of Connecticut (Clinical)",
                              "Sacred Heart University (Physician One)", "Sacred Heart University",
                              "Fairfield University", "University of Hartford", "University of New Haven",
                              "Fairfield University Student Health Center", "University of St Joseph",
                              "Univ Htfd/health Services", "Yale Health Center", "Quinnipiac University/ir-Hlt",
                              "Yale Univ Hlth Serv", "UNIVERSITY OF NEW HAVEN", "Yale University Health Center",
                              "Yale University", "UN.SCSU", "UN.ECSU", "UN.WCSU", "UN.CCSU","UN.FFLDU", 
                              "UN.WCSU.EM", "UN.SCSU.RN", "UN.CCSU.CM", "UN.CCSU.EM", "UN.SCSU.EM", "UN.SCSU.SX",
                              "UN.CCSU.SX", "UN.WCSU.SX", "UN.ECSU.EM"))
#542123 tests
#88902 college tests
#16% (19% if you exclude missing ordering provider)
#MANY MISSING ORDERING PROVIDeR (73615)



###########################################################
#Date 10/22/2020
#Requestor Sydney
#look for cases 70+ in LTCF

old<- read_csv("csv/2020-10-21/2020-10-21cases_wphi.csv")
cases <- read_csv("csv/2020-10-22/2020-10-22cases_wphi.csv") %>% 
  filter(mmwrweek %in% c(41, 42) & age >=70) %>% 
  mutate(cong_yn = if_else((str_detect(cong_exposure_type, "Reside")) &
                      (str_detect(cong_setting, "Jail / Prison") | str_detect(cong_setting, "Long term care facility") | str_detect(cong_setting, "Assisted Living Facility")),
                    "Yes", "No", missing = "No")) %>% 
    filter(cong_yn=="No") %>% 
  filter(!eventid %in% old$eventid)
  


###########################################################
#Date 10/21/2020
#Requestor Dr. Cato Laurencin
#Num of tests by race/ethnicity by month Mar-June, 2020

tests <- read_csv('elr_linelists/elr_linelist2020-10-21.csv') 

tests<-tests %>% 
  filter(spec_col_date >= mdy("03-01-2020") & spec_col_date <= mdy("06-30-2020")) %>% 
  mutate(month=month(spec_col_date)) %>% 
  group_by(hisp_race, month) %>% 
  tally() %>% 
  pivot_wider(names_from=month, values_from=n)

write_csv(tests, "csv/2020-10-21/2020-10-21testsbyre.csv")


###########################################################
#Date 10/21/2020
#Requestor Commissioner
#week over week comparison for town alerts

prior <- read_csv("gary_csv/2020-10-15/AvgDailyIncidence.csv")

new <- read_csv("gary_csv/2020-10-22/AvgDailyIncidence.csv")

all<- new %>% 
  left_join(prior, by="NAME") %>% 
  mutate(change = ifelse(RateCategory.x != RateCategory.y, "Change", "Same"),
         increase = ifelse(change=="Change"& CaseRate.x > CaseRate.y, "Increase", 
                           ifelse(change=="Change" & CaseRate.x < CaseRate.y, "Decrease", "Same"))) %>% 
  select(NAME, totalcases, CaseRate.x, RateCategory.x, n, CaseRate.y, RateCategory.y, change, increase)
write_csv(all, "csv/2020-10-21/2020-10-21change.csv")
count(all, change, increase)

###########################################################
#Date 10/16/2020
#Requestor Patrick
#cases by week for Uncas

cases15<- read_csv("csv/2020-10-22/2020-10-22cases_wphi.csv") %>% 
  filter(city %in% c("Bozrah", "Franklin", "Griswold", "Lebanon", "Lisbon", "Montville", "Norwich", "Preston", "Salem", "Sprague", 'Voluntown')) %>% 
  mutate(year =2020,
    mmwrweek = MMWRweek2Date(MMWRyear = year, MMWRweek = mmwrweek, MMWRday = 7)) %>% 
  group_by(mmwrweek) %>% 
  tally() %>% 
  arrange(mmwrweek) %>% 
  ungroup() %>% 
  mutate(
    date2 = format(mmwrweek, "%B %d, %Y"),
    date2 = factor(date2, levels = date2, labels = date2),
    date = format(mmwrweek, "%b-%d"),
    date = factor(date, levels = date, labels = date)
  )

casesuncas <- read_csv("csv/2020-10-22/2020-10-22cases_wphi.csv") %>% 
  filter(city %in% c("Bozrah", "Franklin", "Griswold", "Lebanon", "Lisbon", "Montville", "Norwich", "Preston", "Salem", "Sprague", 'Voluntown')) %>% 
  filter(mmwrweek %in% c(41, 42)) %>% 
  mutate(cong_yn = if_else((str_detect(cong_exposure_type, "Reside")) &
                             (str_detect(cong_setting, "Jail / Prison") | str_detect(cong_setting, "Long term care facility") | str_detect(cong_setting, "Assisted Living Facility")),
                           "Yes", "No", missing = "No"),
         age_g = if_else(age < 30,  "<30",
                         if_else(age >=30 & age <60, "30-59", ">=60")))
count(casesuncas, cong_yn, gender)

cumuliative <- casesuncas %>% 
  group_by(city) %>% 
  tally()


xaxis <- length(levels(cases15$date))

ggplot(cases15)+
  geom_col(aes(x=date, y=n))+
  geom_text(aes(x=date, y=n, label=n))+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    title = element_text(size = 8.5)
  )+
  labs(
    x="Week of specimen collection or onset",
    y="Number of cases",
    title="COVID-19 Cases among Uncas Health District Residents by Week"
  )


ellington <- cases_14_nc %>% 
  filter(str_to_lower(city)=="ellington") %>% 
  select(eventid)
write_csv(ellington, "csv/2020-10-22/2020-10-22ellington.csv")


###########################################################
#Date 10/16/2020
#Requestor Matt
#cases in Bethany and norwich
bethany <- read_csv("csv/2020-10-16/2020-10-16cases_wphi.csv") %>% 
  filter(city=="Bethany") %>% 
  mutate(date = if_else(disease_status=="Confirmed"& !is.na(spec_col_date), spec_col_date, event_date),
         week=epiweek(date)) %>% 
  filter(week %in% c(40, 41))


#Norwich
norwich <- read_csv("csv/2020-10-16/2020-10-16cases_wphi.csv") 
testgeo2 <- read_csv("csv/2020-10-15/2020-10-15testgeo2.csv")

testgeo3 <- testgeo2 %>% 
  distinct(eventid, GEOID10) %>% 
  select(eventid, GEOID10) 
  
norwich2 <- norwich %>% 
  left_join(testgeo3, by="eventid") %>% 
  filter(city=="Norwich") %>% 
  mutate(category = ifelse(mmwrweek %in% c(34, 35, 36, 37), "baseline", mmwrweek)) %>% 
  filter(category %in% c("baseline", "38", "39", "40", "41", "42")) %>% 
  #select(eventid, category, GEOID10, ) %>% 
  filter(is.na(GEOID10))

testgeocheck <- testgeo2 %>% 
  filter(eventid %in% norwich2$eventid)


install.packages("janitor")
library(janitor)

cts <- norwich2 %>% 
  tabyl(GEOID10, category) %>% 
  adorn_percentages("col") %>% 
  adorn_pct_formatting(rounding="half up", digits=0) %>% 
  adorn_ns(position="front") %>% 
  adorn_title()
write_csv(cts, "csv/counts.csv")

###########################################################
#Date 10/14/2020
#Requestor: Sydney
#Tests by Race/Ethnicity
 testsre <- elr %>% 
  mutate(mmwr_week = epiweek(spec_col_date)) %>% 
  group_by(hisp_race, mmwr_week, result) %>% 
  tally() %>% 
  spread(result, n) %>% 
  replace_na(replace=list(detected=0, `not detected`=0, indeterminate=0)) %>% 
  mutate(pctpos = detected/(detected+`not detected`)*100,
         total=detected+indeterminate+`not detected`) %>% 
  filter(mmwr_week <=41 & mmwr_week >=30)

#make graphs
ggplot(testsre)+
  geom_line(aes(x=mmwr_week, y=total, color=hisp_race), size=2)+
  theme_classic()+
  labs(
    x="Week of Specimen Collection",
    y="Number of PCR tests",
    color="Race/Ethnicity",
    title = "Number of PCR COVID-19 Tests by Race/Ethnicity and Week"
  )

ggplot(testsre)+
  geom_line(aes(x=mmwr_week, y=pctpos, color=hisp_race), size=2)+
  theme_classic()+
  labs(
    x="Week of Specimen Collection",
    y="% Positive Tests",
    color="Race/Ethnicity",
    title = "Percent Positive Tests by Race/Ethnicity and Week"
  )

#look at symptomatic by R/E
cases <- read_csv("csv/2020-10-15/2020-10-15cases_wphi.csv")

casessx <- cases %>% 
  mutate(date = if_else(disease_status=="Confirmed" & !is.na(spec_col_date), spec_col_date, event_date),
         mmwr_week = epiweek(date)) %>% 
  replace_na(replace= list(symptoms="Unknown")) %>%
  group_by(hisp_race, mmwr_week, symptoms) %>% 
  tally() %>% 
  spread(symptoms, n) %>% 
  replace_na(replace= list(Yes=0, No=0, Unknown=0)) %>% 
  mutate(withinfo = (Yes + No) / (Yes + No + Unknown)*100,
         withsx = Yes/(Yes+No)*100) %>% 
  filter(hisp_race %in% c("NH Black", "Hispanic", "NH White", "Unknown") & mmwr_week <=41 & mmwr_week >=10)

ggplot(casessx)+
  geom_line(aes(x=mmwr_week, y=withinfo, color=hisp_race), size=2)+
  theme_classic()+
  labs(
    x="Week of Specimen Collection",
    y="% with info on symptoms",
    color="Race/Ethnicity",
    title = "% of cases with data on symptoms by race/ethnicity and week"
  )

ggplot(casessx)+
  geom_line(aes(x=mmwr_week, y=withsx, color=hisp_race), size=2)+
  theme_classic()+
  labs(
    x="Week of Specimen Collection",
    y="% with symptoms",
    color="Race/Ethnicity",
    title = "% of cases with symptoms (among those data on symptoms) by race/ethnicity and week"
  )




###########################################################
#Date 10/8/2020
#Requestor: Josh
#test counts New London City; New Haven 

newlondon<-test14nc %>% 
  filter(city=="New London")

orders<-count(newlondon, auth_facility)
persons<-count(newlondon, bigID)

#recent deaths
deaths<-case %>% 
  filter(outcome=="Died") %>% 
  filter(death_date > mdy("09-27-2020"))

#where are symptomatic people being tested in norwich?
norwich <- cases_14_nc %>% 
  filter(city=="Norwich"|city=="New London") %>% 
  filter(symptoms=="Yes")

idlist<-norwich$eventid
norwishtest<-elr_linelist %>% 
  filter(eventid %in% idlist)
facils<-count(norwishtest, auth_facility)

#########################################################
#Requestor: Nick P
#Date: 10/16/2020
#in search of missing college test results 
yale <- read_csv("csv/2020-10-23/elr_linelist2020-10-23.csv")
broad <- yale %>% 
  filter(str_detect(str_to_lower(lab_facility),"broad"))   
facil<-count(yale, auth_facility)

broad2<-broad %>% 
  group_by(spec_col_date) %>% 
  tally()

ggplot(broad2)+
  geom_col(aes(x=spec_col_date, y=n))+
  theme_classic()+
  labs(
    x="Specimen Collection Date",
    y="Number of Tests",
    title="PCR Tests Reported by Broad by Date (de-duplicated)"
  )

yale2<- yale %>% 
  filter(auth_facility =="Yale Health Center")%>% 
group_by(lab_facility) %>% 
  #  group_by(spec_col_date) %>% 
#  group_by(lab_facility, spec_col_date) %>% 
  tally()


yaleraw <- df %>% 
  filter(str_detect(`Ordering Provider Facility`, "Yale Health Center")) %>% 
  group_by(`Specimen Date`) %>% 
  tally()

dffacil <-df %>% 
  group_by(`Lab Facility Facility`, `Ordering Provider Facility`, `Ordering Provider Name`) %>% 
  tally()
yalefacil <- yaleraw %>% 
  group_by(`Lab Facility Facility`, `Ordering Provider Facility`, `Ordering Provider Name`) %>% 
  tally()

provider<-yaleraw %>% 
  filter(str_detect(`Ordering Provider Name`, "Christine Wong Chen")|str_detect(`Ordering Provider Name`, "Dorothyann")) %>% 
 # group_by(`Lab Facility Facility`, `Ordering Provider Facility`, `Ordering Provider Name`) %>% 
  tally()

trinity <- broad %>% 
  filter(auth_facility =="Trinity College") %>% 
  group_by(spec_col_date) %>% 
  tally()

wesleyan <- broad %>% 
  filter(auth_facility =="Wesleyan University")%>% 
  group_by(spec_col_date) %>% 
  tally()

fairfield <- broad %>% 
  filter(auth_facility =="Fairfield University")%>% 
  #group_by(spec_col_date) %>% 
  tally()

quinnipiac <- broad %>% 
  filter(auth_facility =="Quinnipiac University")%>% 
#  group_by(spec_col_date) %>% 
  tally()

uconn <- broad %>% 
  filter(auth_facility =="University of Connecticut")%>% 
  group_by(spec_col_date) %>% 
  tally()

uhart <- broad %>% 
  filter(auth_facility =="University of Hartford")%>% 
  group_by(spec_col_date) %>% 
  tally()

###########################################################
#Date 9/28/2020
#requestor: Nancy
#urgent care submissions

labs <- elr_linelist %>% 
  filter(str_detect(str_to_lower(auth_facility), "urgent care|urgentcare|afc|gohealth|physicianone|prohealth uc|physician one|carewell|alliance uc|uc-#")) %>% 
  mutate (auth_facility = str_to_lower(auth_facility),
          urgentcaregroup = ifelse(str_detect(auth_facility, "afc"), "AFC",
                                               ifelse(str_detect(auth_facility, "gohealth"), "GoHealth",
                                                                 ifelse(str_detect(auth_facility, "physicianone|physician one"), "PhysicianOne",
                                                                                   ifelse(str_detect(auth_facility, "carewell"), "Carewell",
                                                                                          ifelse(str_detect(auth_facility, "docs|doc's"), "DOCS",
                                                                                                 ifelse(str_detect(auth_facility, "kathy's|kathys"), "Kathy's",
                                                                                                        ifelse(str_detect(auth_facility, "priority"), "Priority",
                                                                                                               ifelse(str_detect(auth_facility, "stony creek"), "Stony Creek",
                                                                                                                      ifelse(str_detect(auth_facility, "velocity"), "Velocity",
                                                                                                                             ifelse(str_detect(auth_facility,"westport"), "Westport", "Other UC")
                                                                                                                      ))))))))))
                                                                                                                                               

auths <- count(labs, lab_facility, auth_facility)
ucs <- count(labs,  urgentcaregroup)

ucs2 <- count(labs, urgentcaregroup, lab_facility)


###########################################################
#Date 9/28/2020
#requestor: Michelle Gilman
#info on positivity etc. in New Britain

labs <- elr_linelist %>% 
  filter(city=="New Britain" & spec_col_date >= mdy("08-01-2020")) 

labs2 <- labs %>% 
  filter(! auth_facility %in% c("UN.CCSU", "UN.CCSU.CM", "UN.CCSU.EM", "UN.CCSU.SX")) %>% 
  filter(!(auth_facility=="Griffin Hospital" & 
             str_detect(lab_facility,"Jackson") & EventAgeYears >=18 & EventAgeYears <=22)) %>% 
  filter(! str_detect(`Street Address`, "Hall")) %>% 
  filter(! str_detect(`Street Address`, "CCSU")) %>% 
  mutate(week = epiweek(spec_col_date)) %>% 
  group_by(week, result2) %>% 
  tally() %>% 
  spread(result2, n) %>% 
  mutate(pctpos = Positive/(Positive + Negative)*100)



auths<-count(labs2, lab_facility, auth_facility, auth_name)

labs3 <- labs2 %>% 
  filter(str_detect(lab_facility, "Jackson") & is.na(auth_facility))
         

###########################################################
#Date 9/18/2020
#requestor: Steve
#tests from uconn
uconn<- elr_linelist %>% 
  filter(auth_facility %in% c("Uconn Student Health Service", "UCONN Health Center", "University of Connecticut")) %>% 
  group_by(lab_facility, spec_col_date) %>% 
  tally()


###########################################################
#Date 9/18/2020
#requestor: Lynn Sosa
#Danbury Line list

cases <- read_csv("csv/2020-09-23/2020-09-23cases_wphi.csv")
labs <- read_csv("elr_linelists/elr_linelist2020-09-23.csv")

danbury<- cases %>% 
  filter(city=="Danbury" & spec_col_date >= mdy("09/06/2020"))

labsdanbury<- labs %>% 
  filter(eventid %in% danbury$eventid & spec_col_date >=mdy("09/18/2020") & result2=="Positive")
count(labsdanbury, lab_facility)

labsdanbury2 <- labsdanbury %>% 
  filter(str_detect(lab_facility, "Sema4|Quest"))

labsdanbury918 <- labs %>% 
  filter(official_city =="DANBURY" & spec_col_date == mdy("09/18/2020") & result2=="Positive" &
           str_detect(lab_facility, "Quest"))

write_csv(labsdanbury2, "csv/2020-09-23/2020-09-23danburyposspecimen.csv")
write_csv(danbury, "csv/2020-09-18/2020-0918danburycases.csv")

###########################################################
#Date 9/18/2020
#requestor: Jillian Armstrong
#Serology results for repeat positives

repeats <- read_csv("repeatpos.csv") %>% 
  rename(eventid=ID)

cases <- read_csv("csv/2020-09-18/2020-09-18cases_wphi.csv") %>% 
  select(eventid, fname, lname, dob, gender, city)

repeatinfo<- repeats %>% 
  left_join(cases, by="eventid") %>% 
  mutate(bigID=paste0(str_to_lower(lname),str_to_lower(fname),dob),
         bigID2=paste0(str_sub(str_to_lower(lname),1,5), str_sub(str_to_lower(fname),1,5), dob))

library(DBI)
#bring in data that is output from geocoding
con <- DBI::dbConnect(odbc::odbc(), "epicenter")
statement <- paste0("SELECT [r].[PatientLastName]
                              ,[r].[PatientFirstName]
                              ,[r].[PatientMiddleName]
                              ,[r].[PatientGender]
                              ,[r].[PatientDOB]
                              ,[r].[PatientCity]
                              ,[r].[OrderingFacilityName]
                              ,[r].[OrderingFacilityCity]
                              ,[r].[SpecimenID]
                              ,[r].[SpecimenCollectionDate]
                              ,[r].[SpecimenSource]
                              ,[r].[PerformingOrganizationName]
                              ,[r].[DateResultTested]
                              ,[r].[Result]
                              ,[r].[ResultUnits]
                              ,[r].[ResultReferenceRange]
                              ,[r].[ResultStatus]
                              ,[r].[ResultNotes]
                    FROM [DPH_COVID_IMPORT].[dbo].[Serology_TSV_IMPORT] as [r]")
serotest <-  DBI::dbGetQuery(conn = con , statement = statement)
odbc::dbDisconnect(con)

serotest<-serotest %>% 
  mutate(dob=ymd(PatientDOB),
         bigID = paste0(str_to_lower(PatientLastName), str_to_lower(PatientFirstName), dob),
         bigID2 = paste0(str_sub(str_to_lower(PatientLastName),1,5), str_sub(str_to_lower(PatientFirstName),1,5), dob))

repeatinfo2<- repeatinfo %>% 
  left_join(serotest, by="bigID")

repeatinfo3 <- repeatinfo %>% 
  left_join(serotest, by="bigID2")

repeatinfo4 <- repeatinfo3 %>% 
  filter(!is.na(PatientLastName)) %>% 
  select(-c("fname","lname","dob.x","gender","city","bigID.y","bigID.x","bigID2", "dob.y","ResultUnits",
            "ResultReferenceRange","ResultStatus"))

write_csv(repeatinfo4, "csv/2020-09-18/2020-09-18serologyrepeatpos.csv")

###########################################################
#Date 9/15/2020
#requestor: Rep
#count of tests and people tested

peopletested <- read_csv("elr_linelists/elr_linelist2020-09-24.csv") %>%
  #create unique id
  mutate(bigID = paste0(`First Name`, `Middle Name`, `Last Name`, `Birth Date`)) %>%
  #count people tested
  distinct(bigID)

peopeltested2<- read_csv("elr_linelists/elr_linelist2020-09-24.csv") %>%
  #create unique id
  mutate(bigID = paste0(`First Name`, `Last Name`, `Birth Date`)) %>%
  #count people tested
  distinct(bigID)

#look for out of statetests
labs<-count(peopletested, lab_facility)
broad <- peopletested %>% 
  filter(str_detect(lab_facility, "Broad Institute Genomics Services")) %>% 
  group_by(auth_facility) %>% 
tally()

write_csv(broad,"csv/2020-09-15/2020-09-15broad.csv")

###########################################################
#Date 9/15/2020
#requestor: Forrest
#count of tests and cases per week per town excluding tests and cases in congregate settings

#count cases
cases <- read_csv("csv/2020-09-15/2020-09-15cases_wphi.csv")
#filter congregate residents, group by town and week, count
cases_nc<- cases %>% 
  mutate(cong_yn = if_else((str_detect(cong_exposure_type, "Reside")) &
                             (str_detect(cong_setting, "Jail / Prison") | str_detect(cong_setting, "Long term care facility") | str_detect(cong_setting, "Assisted Living Facility")),
                           "Yes", "No", missing = "No")) %>% 
  filter(cong_yn=="No"& disease_status=="Confirmed") %>% 
  group_by(mmwrweek, city) %>% 
  tally() %>% 
  rename(confirmedcases=n)

#count tests 
tests <- read_csv("csv/2020-09-15/2020-09-15testgeo2.csv") 
tests_nc<- tests %>% 
  filter(cong_test=="No") %>% 
  mutate(mmwrweek=epiweek(spec_col_date)) %>% 
  group_by(mmwrweek, city) %>% 
  tally() %>% 
  rename(pcrtests=n)

#join together
cases_tests<- tests_nc %>% 
  left_join(cases_nc, by=c("mmwrweek", "city"))

#output csv
write_csv(cases_tests,"csv/2020-09-15/2020-09-15noncongtestconfirmcaseweek.csv")



###########################################################
#Date 9/14/2020
#requestor: Josh
#probable cases with positive antigen results

cases <- read_csv("csv/2020-09-14/2020-09-14cases_wphi.csv") %>% 
  filter(disease_status=='Probable')

#antigen tests
count(elr, `Tests Performed on Sample/Specimen: Test or Method`)
antigen <- elr %>% 
  filter(`Tests Performed on Sample/Specimen: Test or Method` %in% c("SARS CoV 2 Ag (Quidel Sofia/Lumira)", "SARS CoV 2 Ag rapid IA (BD Veritor)"))

count(antigen, `Tests Performed on Sample/Specimen: Result`)

###########################################################
#Date 9/11/2020
#requestor: Jillian Armstrong
#Lab and clinical info for all cases with 2 positives >90 days apart

linelist <- read_csv("data_requests/repeat_NH_cases_allpos.csv")
cases <- read_csv("csv/2020-09-11/2020-09-11cases_wphi.csv")

cases <- cases %>% 
  select(eventid, hospitalized, admit_date, discharge_date, icu,
         symptoms, symp_onset_date, fever, fatigue, sob, headache,
         cough, myalgia, new_olf_taste, rigors, pneumonia, ards, 
         outcome, death_date, healthcare_worker, cong_setting, 
         cong_exposure_type, cong_facility)

missing <- linelist %>% 
  filter(!eventid %in% cases$eventid)

linelist2 <- linelist %>% 
  left_join(cases, by="eventid")

count(linelist2, cong_setting, cong_exposure_type)

linelistcheck<- linelist2 %>%
  group_by(eventid) %>% 
  slice(1L) %>% 
  ungroup() %>% 
  filter(is.na(cong_setting))
write_csv(linelistcheck, "csv/2020-09-11/repeatposcheck.csv")

count(linelistcheck, cong_setting, cong_exposure_type)

write_csv(linelist2, "data_requests/2020-09-11repeatpositive90d.csv")

###########################################################
#Date 9/10/2020
#requestor: vivian
#Number of cases marked as lTCF residents

cases <- read_csv("csv/2020-09-10/2020-09-10cases_wphi.csv")
congs<-count(cases, cong_setting, cong_exposure_type)

###########################################################
#Date 9/2/2020
#requestor: Alycia
#SPHL lab results with DOC, event ID, spec num, result
labfile <- list.files("lab_data_here", ".csv")
elr <-   labfile %>%
  map(~read_csv(file.path("lab_data_here", .))) %>% 
  reduce(rbind) %>% 
  
elralycia<- elr %>% 
  select(`Event ID`, `Testing Laboratory Information: Lab Facility`,
         `Tests Performed on Sample/Specimen: Result`, `Specimen Information (add a new lab result for each specimen): Specimen Number`,
         `Specimen Information (add a new lab result for each specimen): Specimen Collection Date`) %>% 
  filter(`Testing Laboratory Information: Lab Facility`==	"DPH State Laboratory(Rocky Hill)")

write_csv(elralycia, "csv/2020-09-02/2020-09-02SPHL_tests_alycia.csv")

###########################################################
#Date 9/1/2020
#requestor: Josh Geballe
#where are lab test results coming from

oldelr <- read_csv("csv/2020-08-31/2020-08-31elr_linelist.csv")
newelr <- read_csv("csv/2020-09-01/2020-09-01elr_linelist.csv")

locationsold <- count(oldelr, auth_facility)
locationsnew <- count(newelr, lab_facility)

newelr2 <- newelr %>% 
  filter(lab_result_mod_date == "08/31/2020")

locations <- count(newelr2, str_detect(auth_facility, "univ|Univ|college|College|Uconn Student Health Service|Yale Health-Student Health"))

elr_linelist <- newelr

#compare autopull to manual elr pull
autopull <- read_csv("csv/2020-09-01/2020-09-01autoelr.csv")

autopull2 <- autopull %>% 
  filter(`Investigation Modification Date` == "08/31/2020")
newelr2 <- newelr %>% 
  filter(lab_result_mod_date =="08/31/2020")

autotest<-count(autopull2, test_method)
mantest<-count(newelr2, test_method)

deathsman<- read_csv("csv/2020-09-01/2020-09-01cases_wphi.csv") %>% 
  filter(outcome=="Died")
deathsauto<- read_csv("csv/2020-09-01/2020-09-01cases_wphiauto.csv") %>% 
  filter(outcome=="Died")

deasthnotauto <- deathsman %>% 
  filter(!eventid %in% deathsauto$eventid)

deathsnotman <- deathsauto %>% 
  filter(!eventid %in% deathsman$eventid)

###########################################################
#Date 8/31/2020
#requestor: Alison Kleppinger
#deaths (probable, confirmed, suspect) for comparison with vital records

#read in all data from CTEDSS_data_here using markdown report, then use DF
#sort by disease status and create bigID, keep 1 record per person

df_death <- df %>% 
  filter(Outcome=="Died") %>% 
  group_by(`Event ID`) %>% 
  arrange(`Disease classification status`) %>% 
  slice(1L) %>% 
  ungroup() 
df_death2 <- df_death %>% 
  mutate(bigID = paste0(`First Name`, `Last Name`, `Birth Date`)) %>% 
  group_by(bigID) %>% 
  arrange(`Disease classification status`) %>% 
  slice(1L) %>% 
  ungroup() %>% 
  select(`Event ID`, `First Name`, `Last Name`, `Birth Date`,
         EventAgeYears, Gender, OfficialCityAtTimeOfEvent, County,
         State, Race, `Is case Hispanic or Latino?`, `Disease classification status`,
         Outcome, `Date of death`, `Died with COVID-19`)
write_csv(df_death2, "csv/2020-08-31/2020-08-31alldeaths.csv")  




###########################################################
#Date 8/28/2020
#Requestor: LS
#cases in North Haven and Hartford and waterbury
cases <- read_csv("csv/2020-08-28/2020-08-28cases_wphi.csv") %>% 
  filter(event_date >= mdy("08-16-2020") | spec_col_date >= dmy("08-16-2020"))


cases_northaven <- cases %>% 
  filter(city=="North Haven") %>% 
  mutate(date = if_else(disease_status=="Confirmed" & !is.na(spec_col_date), 
                 spec_col_date, event_date))
write_csv(cases_northaven, "csv/2020-08-28/2020-08-28northhavencases.csv")

cases_northaven_day <- cases_northaven %>% 
  group_by(date) %>% 
  tally() #%>% 
  #filter(date >= mdy("07-01-2020"))

ggplot(cases_northaven_day)+
  geom_col(aes(x=date, y=n), fill="darkgrey")+
  theme_classic()+
  labs(
    title="COVID-19 Cases among Residents of North Haven, CT",
    x="Specimen Collection or Onset Date",
    y="Number of cases"
  )

cases_hartford <- cases %>% 
  filter(city=="Hartford")
write_csv(cases_hartford, "csv/2020-08-28/2020-08-28hartfordcases.csv")

#hartford cases by age and date 
age_labels3 <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-39", "40-64",  "65-79", ">=80")
cases_hartford2 <- cases_hartford %>% 
  filter(!is.na(age)) %>% 
  mutate(date = if_else(disease_status=="Confirmed" & !is.na(spec_col_date), 
                        spec_col_date, event_date),
    age_group = cut(age, breaks = c(-1,4,9,14,19,24,29,39,64,79,Inf),labels = age_labels3),
                  week = epiweek(spec_col_date)) 

cases_hartford_age <- cases_hartford2 %>% 
  group_by(mmwrweek, age_group) %>% 
  tally() %>% 
  filter(mmwrweek >=25)

ggplot(cases_hartford_age)+
  geom_col(aes(x=date, y=n, fill=age_group))+
  theme_classic()+
  labs(
    x="MMWR Week",
    y="Number of cases",
    color="Age group"
  )

#using casenew from NewCases1.2 program
cases_hartford_day <- cases_hartford2 %>% 
  group_by(date) %>% 
  tally() %>% 
  filter(date >= mdy("07-01-2020"))

ggplot(cases_hartford_day)+
  geom_col(aes(x=date, y=n), fill="darkgrey")+
  theme_classic()+
  labs(
    title="COVID-19 Cases among Residents of Hartford, CT",
    x="Specimen Collection or Onset Date",
    y="Number of cases"
  )


#tests in Hartford
hartford_test <- read_csv("csv/2020-08-31/2020-08-31elr_linelist.csv") %>% 
  filter(city=="Hartford")  %>%
  mutate(EventAgeYears = as.numeric(EventAgeYears),
         age_group = cut(EventAgeYears, breaks = c(-1,4,9,14,19,24,29,39,64,79,Inf),labels = age_labels3),
         week = epiweek(spec_col_date)) 

hartford_test_age <- hartford_test %>% 
  filter(!is.na(spec_col_date) & !is.na(EventAgeYears)) %>%
  #group_by(age, week, result2) %>% 
  group_by(spec_col_date, result2) %>% 
  tally()  %>%
  spread(result2, n) %>% 
  replace_na(replace = list(Positive=0, Negative=0, Indeterminate=0)) %>% 
  mutate(Total = sum(Positive, Negative, Indeterminate, na.rm=TRUE),
         #rate = Total/pop*100000,
         pctpos = Positive / (Positive + Negative)*100 #,
         #year = 2020,
         #date = MMWRweek2Date(MMWRyear=year, MMWRweek=week, MMWRday=7)
         ) %>% 
  #filter( week >=20)
  filter(spec_col_date >= mdy("03-01-2020"))
hartford_test$age_group <- factor(hartford_test$age_group, labels = age_labels3, levels = age_labels3 )

ggplot(hartford_test_age)+
  geom_line(aes(x=spec_col_date, y=Total))+
  theme_classic()+
  scale_color_brewer(palette= "Paired")+
  labs(x='Specimen Collection Date',
       y='Tests per 100,000 Population per Week',
       title="Number of Community Tests per Week by Age Group, Danbury, CT",
       color='Age Group (years)')


ggplot(hartford_test_age)+
  geom_line(aes(x=spec_col_date, y=pctpos))+
  theme_classic()+
  scale_color_brewer(palette= "Paired")+
  labs(x='Specimen Collection Date',
       y='% Positive',
       title="Percentage of Postive Tests per Week by Age Group, Danbury, CT",
       color='Age Group (years)')

#percent positivity in recent weeks Hartford
hartford_test_all <- hartford_test %>% 
  filter(!is.na(spec_col_date)) %>% 
  group_by(spec_col_date, result2) %>% 
  tally()  %>%
  spread(result2, n) %>% 
  replace_na(replace = list(Positive=0, Negative=0, Indeterminate=0)) %>% 
  mutate(Total = sum(Positive, Negative, Indeterminate, na.rm=TRUE),
         pctpos = Positive / (Positive + Negative)*100#,
         #year = 2020,
         #date = MMWRweek2Date(MMWRyear=year, MMWRweek=week, MMWRday=7)
  ) %>% 
  #filter( week >=27)
  filter(spec_col_date >=mdy("07-01-2020"))

ggplot(hartford_test_all)+
  geom_col(aes(x=spec_col_date, y=Total), fill="lightgrey")+
  #geom_line(aes(x=spec_col_date, y=pctpos))+
  theme_classic()+
  labs(x='Specimen Collection Date',
       y='Number of tests per Day',
       title="COVID-19 Tests per Day, Hartford, CT"
  )
ggplot(hartford_test_all)+
  geom_line(aes(x=spec_col_date, y=pctpos))+
  geom_hline(yintercept = 5, color="blue", linetype="dotted")+
  theme_classic()+
  scale_color_brewer(palette= "Paired")+
  ylim(min=0, max=75)+
  labs(x='Specimen Collection Date',
       y='% Positive',
       title="Percentage of Postive Tests per Day, Hartford, CT"
  )

#epicurve of Danbury



#hospitalizations in Danbury
hosp_danbury <- read_csv("csv/2020-08-27/2020-08-27cases_wphi.csv") %>% 
  filter(hospitalized =="Yes" & city=="Danbury") %>% 
  group_by(spec_col_date) %>% 
  tally()
ggplot(hosp_danbury)+
  geom_col(aes(x=spec_col_date, y=n))+
  theme_classic()+
  labs(
    title="Hospitalized COVID-19 patients among Danbury Residents",
    x="Specimen collection/onset",
    y="Number of patients"
  )


###########################################################
#Date 8/27/2020
#Requestor: Forrest
#No. cases by age group over time
age_labels3 <- c("0-39", "40-64",  "65+")

cases <- read_csv("csv/2020-09-03/2020-09-03cases.csv") %>% 
  filter(!is.na(age)) %>% 
  mutate(date = if_else(disease_status =="Confirmed" & !is.na(spec_col_date), spec_col_date, event_date),
         age_group = cut(age, breaks = c(-1,39,64,Inf),labels = age_labels3)
         ) %>% 
  group_by(age_group, mmwrweek) %>% 
  tally() %>% 
  filter(mmwrweek >= 10)
ggplot(cases)+
  geom_bar(aes(x=mmwrweek, y=n, fill=age_group), position="dodge", stat="identity")+
  scale_fill_brewer(palette = "Blues")+
  theme_classic()+
  labs(
    title="Number of cases by date and age group",
    y="Cases",
    x="MMWR Week",
    legend="Age Group (years)"
  )

#how many probable vs. confirmed
status <- cases %>% 
  group_by(mmwrweek, disease_status) %>% 
  tally()

#how many cases have symptoms
symp <- cases %>% 
  group_by(mmwrweek, symptoms) %>% 
  tally() %>% 
  spread(symptoms, n) %>% 
  mutate(total=sum(Yes, No, Unknown, `<NA>`, na.rm=TRUE),
         totinfo = sum(Yes, No, na.rm=TRUE),
         pctsymp = Yes/totinfo*100,
         pctunkn = sum(Unknown, `<NA>`, na.rm=TRUE)/total*100)

ggplot(symp)+
  geom_line(aes(x=mmwrweek, y=pctsymp))+
  geom_line(aes(x=mmwrweek, y=pctunkn))

##########################################################
#Date 8/26/2020
#Requestor: Gary, LS
#Goal: Map of tests and positive tests in Danbury
#excludes tests that could not be geocoded and tests associated with cong settings

#danbury reboot map for cases positive in September
testgeo2 <- read_csv("csv/2020-09-17/2020-09-17testgeo2.csv")
danbury_positive <- testgeo2 %>% 
  filter(city=="Danbury" & (!cong_test=="Yes") & !GEOID10=="NULL") %>% 
  select(eventid, EventAgeYears, spec_col_date, result2, X, Y, GEOID10) %>% 
  rename(age=EventAgeYears,
         result=result2) %>% 
  filter(spec_col_date >=ymd("2020-09-01") & result=="Positive") %>% 
  mutate(month=month(spec_col_date))
write_csv(danbury_positive, "csv/2020-09-22/2020-09-22danburypositive.csv")


danbury_tract <- danbury_positive %>% 
  group_by(month, GEOID10) %>% 
  tally() %>% 
  spread(month, n)

danbury_tests<- testgeo2 %>% 
  filter(city=="Danbury" & (is.na(cong_test)|cong_test=="No") & !is.na(X)) %>% 
  select(eventid, EventAgeYears, spec_col_date, result2, X, Y, GEOID10)  %>% 
  rename(age=EventAgeYears,
         result=result2) 

danbury_positive <- danbury_tests %>% 
  filter(spec_col_date >= ymd("2020-08-01") & result=="Positive")

danbury_recentests <- danbury_tests %>% 
  filter(spec_col_date >= ymd("2020-08-16"))
  

write_csv(danbury_positive, "csv/2020-08-26/2020-08-26danburypositive.csv")
write_csv(danbury_recentests, "csv/2020-08-26/2020-08-26danburytests.csv")


testgeo2 <- read_csv("csv/2020-08-27/2020-08-27testgeo2.csv")
hford_tests<- testgeo2 %>% 
  filter(city=="Hartford" & (is.na(cong_test)|cong_test=="No") & !is.na(X)) %>% 
  select(eventid, EventAgeYears, spec_col_date, result2, X, Y, GEOID10)  %>% 
  rename(age=EventAgeYears,
         result=result2) 

hford_positive <- hford_tests %>% 
  filter(spec_col_date >= ymd("2020-08-01") & result=="Positive")

hford_recentests <- hford_tests %>% 
  filter(spec_col_date >= ymd("2020-08-16"))


write_csv(hford_positive, "csv/2020-08-28/2020-08-28hartfordpositive.csv")
write_csv(hford_recentests, "csv/2020-08-28/2020-08-28hartfordtests.csv")


#statewide dataset of tests for Gary for surface modeling
statewide <-testgeo2 %>% 
  mutate(week = epiweek(spec_col_date)) %>%
  filter((is.na(cong_test)|cong_test=="No") & !is.na(X)) %>% 
  select(eventid, week, EventAgeYears, spec_col_date, result2, X, Y, GEOID10) %>% 
  rename(age=EventAgeYears,
       result=result2) %>% 
  filter(spec_col_date >= mdy("08-01-2020") & spec_col_date < mdy("08-31-2020"))
write_csv(statewide, "csv/2020-09-01/2020-09-01statewidetests.csv")
weekly <- elr_linelist %>% 
  mutate(week = epiweek(spec_col_date)) %>% 
  group_by(week) %>% 
  tally()
  
##########################################################
#Date 8/25/2020
#Requestor: Bernie Bova
#Goal: Count of deaths in Stratford

stratford <- read_csv("csv/2020-08-25/2020-08-25cases_wphi.csv") %>% 
  filter(city=="Stratford" & !is.na(death_date))

stratford2 <- stratford %>% 
  filter(is.na(outcome))


##########################################################
#Date 8/21/2020
#Requestor: Rep. Horn
#Goal: Turn around time for northwest corner towns; Charlotte-Hungerford

nwcorner <- elr_linelist %>% 
  filter(county=="Litchfield County") %>% 
  mutate(TAT = (mdy(date_reported_dph)-spec_col_date),
         week=epiweek(spec_col_date)) %>% 
  group_by(spec_col_date) %>% 
  summarise(medtat = median(TAT, na.rm=TRUE), meantat = mean(TAT, na.rm=TRUE), n=n(), 
            lower = quantile(TAT, na.rm=TRUE)[2], upper = quantile(TAT, na.rm=TRUE)[4]) %>% 
  filter(spec_col_date <= mdy("08-18-2020"))

ggplot(nwcorner)+
  geom_line(aes(x=spec_col_date,y=medtat))+
  geom_ribbon(aes(x=spec_col_date, ymin = lower, ymax = upper), fill = "grey70", alpha = 0.5)+
  theme_classic()+
  labs(
    title="Median (IQR) Days from Test Collection to Result Report, \nLitchfield County, CT",
    y="Days from Collection to Report",
    x="Specimen Collection Date"
  )
  


##########################################################
#Date 8/21/2020
#Requestor: Commissioner
#Goal: Danbury tests over time
age_labels3 <- c("0--4", "5--9", "10--14", "15--19", "20--24", "25--29", "30--39", "40--64", "65--79", "80+")  
danbury_pop <- read_csv("population_data/danbury_age.csv") 

danbury_test <- read_csv("csv/2020-09-03/2020-09-03testgeo2.csv") %>% 
  filter(city=="Danbury" & cong_test !="Yes")  %>%
  mutate(EventAgeYears = as.numeric(EventAgeYears),
        age_group = cut(EventAgeYears, breaks = c(-1,4,9,14,19,24,29,39,64,79,Inf),labels = age_labels3),
         week = epiweek(spec_col_date)) %>% 
  filter(!is.na(spec_col_date) & !is.na(EventAgeYears)) %>%
  group_by(age_group, week, result2) %>% 
  tally()  %>%
  spread(result2, n) %>% 
  left_join(danbury_pop, by=c("age_group" ="AGE GROUP")) %>% 
  replace_na(replace = list(Positive=0, Negative=0, Indeterminate=0)) %>% 
  mutate(Total = sum(Positive, Negative, Indeterminate, na.rm=TRUE),
         rate = Total/pop*100000,
         pctpos = Positive / (Positive + Negative)*100,
         year = 2020,
         date = MMWRweek2Date(MMWRyear=year, MMWRweek=week, MMWRday=7)) %>% 
  filter( week >=20)
danbury_test$age_group <- factor(danbury_test$age_group, labels = age_labels3, levels = age_labels3 )

ggplot(danbury_test)+
  geom_line(aes(x=date, y=rate, color=age_group))+
  theme_classic()+
  scale_color_brewer(palette= "Paired")+
  labs(x='Specimen Collection Date',
       y='Tests per 100,000 Population per Week',
       title="Number of Community Tests per Week by Age Group, Danbury, CT",
       color='Age Group (years)')


ggplot(danbury_test)+
  geom_line(aes(x=date, y=pctpos, color=age_group))+
  theme_classic()+
  scale_color_brewer(palette= "Paired")+
  labs(x='Specimen Collection Date',
       y='% Positive',
       title="Percentage of Postive Tests per Week by Age Group, Danbury, CT",
       color='Age Group (years)')
  
#percent positivity in recent weeks Danbury
danbury_test2 <- read_csv("csv/2020-09-03/2020-09-03testgeo2.csv") %>% 
  filter(cong_test !="Yes" & city=="Danbury")  %>%
  mutate(EventAgeYears = as.numeric(EventAgeYears),
         age_group = cut(EventAgeYears, breaks = c(-1,4,9,14,19,24,29,39,64,79,Inf),labels = age_labels3),
         week = epiweek(spec_col_date)) %>% 
  filter(!is.na(spec_col_date)) %>%
  group_by(spec_col_date, result2) %>% 
  tally()  %>%
  spread(result2, n) %>% 
  replace_na(replace = list(Positive=0, Negative=0, Indeterminate=0)) %>% 
  mutate(Total = sum(Positive, Negative, Indeterminate, na.rm=TRUE),
         pctpos = Positive / (Positive + Negative)*100#,
         #year = 2020,
         #date = MMWRweek2Date(MMWRyear=year, MMWRweek=week, MMWRday=7)
         ) %>% 
  #filter( week >=27)
  filter(spec_col_date >=mdy("08-27-2020"))

ggplot(danbury_test2)+
  geom_col(aes(x=spec_col_date, y=Total), fill="lightgrey")+
  #geom_line(aes(x=spec_col_date, y=pctpos))+
  theme_classic()+
  labs(x='Specimen Collection Date',
       y='Number of tests per Day',
       title="COVID-19 Tests per Day, Danbury, CT"
  )
ggplot(danbury_test2)+
  geom_line(aes(x=spec_col_date, y=pctpos))+
  geom_hline(yintercept = 5, color="blue", linetype="dotted")+
  theme_classic()+
  scale_color_brewer(palette= "Paired")+
  ylim(min=0, max=15)+
  labs(x='Specimen Collection Date',
       y='% Positive',
       title="Percentage of Postive Tests per Day, Danbury, CT"
       )

#epicurve of Danbury
#using casenew from NewCases1.2 program
danbury_case <- casenew %>% 
  filter(city=="Danbury") %>% 
  group_by(date) %>% 
  tally() %>% 
  filter(date >= mdy("08-01-2020"))

ggplot(danbury_case)+
  geom_col(aes(x=date, y=n), fill="darkgrey")+
  theme_classic()+
  labs(
    title="COVID-19 Cases among Residents of Danbury, CT",
    x="Specimen Collection or Onset Date",
    y="Number of cases"
  )


#hospitalizations in Danbury
hosp_danbury <- read_csv("csv/2020-08-27/2020-08-27cases_wphi.csv") %>% 
  filter(hospitalized =="Yes" & city=="Danbury") %>% 
  group_by(spec_col_date) %>% 
  tally()
ggplot(hosp_danbury)+
  geom_col(aes(x=spec_col_date, y=n))+
  theme_classic()+
  labs(
    title="Hospitalized COVID-19 patients among Danbury Residents",
    x="Specimen collection/onset",
    y="Number of patients"
  )

##########################################################
#Date 8/19/2020
#Requestor: Cato Laurencin 
#Goal: % positive by race/ethnicity

tests_re <- elr_linelist %>% 
  replace_na(replace=list(`Is case Hispanic or Latino?`="UNKNOWN",
                          Race="Unknown")) %>% 
  mutate(hisp_race = ifelse(
    `Is case Hispanic or Latino?`=="YES",
    "Hispanic",
    if_else(
      Race =="WHITE",
      "NH White",
      if_else(Race =="BLACK_AFRICAN_AMERICAN",
              "NH Black",
              if_else(Race =="AMERICAN_INDIAN_ALASKAN_NATIVE",
                      "NH American Indian or Alaskan Native",
                      if_else(Race=="ASIAN"|Race=="NATIVE_HAWAIIAN_PACIFIC_ISLANDER"|
                               Race== "ASIAN,NATIVE_HAWAIIAN_PACIFIC_ISLANDER"|
                               Race== "NATIVE_HAWAIIAN_PACIFIC_ISLANDER,ASIAN",
                              "NH Asian or Pacific Islander",
                              if_else(Race=="OTHER", 
                                      "Other", 
                                      ifelse(Race=="Unknown"|Race=="REFUSED"|is.na(Race), "Unknown",
                                              "Multirace")))))
    )
  ),
  week=epiweek(spec_col_date),
  hisp_race=factor(hisp_race, 
                   levels=c("Unknown", "NH White", "NH American Indian or Alaskan Native", "NH Asian or Pacific Islander", "NH Black",
                            "Hispanic", "Multirace", "Other" )))


tests_resummary<- tests_re %>% 
  group_by(hisp_race, result2) %>% 
  filter(spec_col_date >= ymd("2020-08-01")) %>% 
  tally() %>% 
  spread(result2, n) %>% 
  mutate(total=Negative+Positive+Indeterminate,
         pctpos=Positive/(Positive+Negative)*100)
write_csv(tests_resummary, "csv/2020-08-26/2020-08-26_summarytestsRace-ethnicity.csv")

tests_re2 <- tests_re %>% 
  group_by(hisp_race, week, result2) %>% 
  tally() %>% 
  spread(result2, n) %>% 
  replace_na(replace=list(Positive=0, Negative=0, Indeterminate=0)) %>% 
  mutate(total=sum(Negative,Positive,Indeterminate),
         pctpos=Positive/(Positive+Negative)*100)

pctpos<- ggplot(tests_re2)+
  geom_point(aes(x=week, y=pctpos, color=hisp_race, size=total))+
  #theme_bw()+
  scale_color_brewer("Race/Ethnicity")+
  labs(
    x="MMWR Week",
    y="% positive tests",
    title = "Percentage positive COVID-19 tests by race/ethnicity",
    legend="Race/ethnicity"
  )

ggplotly(pctpos)

totaltests<- ggplot(tests_re2)+
  geom_point(aes(x=week, y=total, color=hisp_race))+
  #theme_bw()+
  scale_color_brewer("Race/Ethnicity")+
  labs(
    x="MMWR Week",
    y="# tests",
    title = "Number of COVID-19 tests by race/ethnicity",
    legend="Race/ethnicity"
  )

ggplotly(totaltests)

#tests per population
pops<-read_csv("population_data/county_re_gender_age.csv") %>% 
  group_by(hisp_race) %>%  
  summarize(tot=sum(n))

tests_re2 <- tests_re2 %>% 
  left_join(pops, by="hisp_race") %>% 
  mutate(rate = total/tot*100000)
totalrates<- ggplot(tests_re2)+
  geom_point(aes(x=week, y=rate, color=hisp_race))+
  #theme_bw()+
  scale_color_brewer("Race/Ethnicity")+
  labs(
    x="MMWR Week",
    y="Tests per 100,000 population",
    title = "Tests per 100,000 population by race/ethnicity",
    legend="Race/ethnicity"
  )

ggplotly(totalrates)



##########################################################
#Date 8/19/2020
#Requestor: SJ
#Goal: compare new scheduled report cases to today's manual data pull report

automated <- read_csv("csv/2020-08-19/2020-08-19cases_wphi_automated.csv") %>% 
  mutate(inautomated=1)

today <- read_csv("csv/2020-08-19/2020-08-19cases_wphi.csv") %>% 
  mutate(intoday=1)

diff <- automated %>% 
  left_join(today, by="eventid") %>% 
  filter(is.na(intoday))

write_csv(diff, "csv/2020-08-19/2020-08-19diff.csv")

diff2 <- today %>% 
  left_join(automated, by = "eventid") %>% 
  filter(is.na(inautomated))


##########################################################
#Date 8/14/2020
#Requestor: Lou Gonsalves/NCI
#Goal: dataset of COVID-19 cases through June 30, 2020 for matching with cancer registry data

covid_case_phi <- read_csv("csv/2020-08-13/2020-08-13cases_wphi.csv") %>% 
  select(eventid, disease_status, spec_col_date, event_date, fname, lname, dob, gender, `Street address`, city, county, state, race, hisp, hisp_race, outcome, death_date) %>% 
  mutate(outcome = ifelse(outcome =="Died", outcome, NA),
         date = ifelse((disease_status == "Confirmed" & !is.na(spec_col_date)), spec_col_date, event_date), 
         ethnicity = ifelse(race=="Unknown" & hisp_race=="Unknown", "Unknown", hisp)) %>%
  filter(date <= mdy("06-30-2020") & state=="CT") %>% 
  select(-c("event_date", "spec_col_date", "date", "hisp", "hisp_race"))

count(covid_case_phi, outcome)

covid_case_nophi <- read_csv("csv/2020-08-13/2020-08-13cases_wphi.csv") %>%
  filter(eventid %in% covid_case_phi$eventid) %>% 
  select(eventid, disease_status, state, gender, dob, race, hisp, hisp_race, spec_col_date, symptoms, cough, fever, fatigue, 
         sob, headache, pneumonia, hospitalized, icu, outcome, death_date) %>% 
  mutate(outcome = ifelse(outcome =="Died", outcome, NA), 
         dob = paste0(year(dob), "-", month(dob)),
         ethnicity = ifelse(race=="Unknown" & hisp_race=="Unknown", "Unknown", hisp)) %>% 
  select(-c("hisp_race", "hisp"))
count(covid_case_phi, outcome)


write_csv(covid_case_phi, "csv/2020-08-13/2020-08-13covidcasenci_phi.csv")
write_csv(covid_case_nophi, "csv/2020-08-13/2020-08-13covidcasenci_nophi.csv")


##########################################################
#Date 8/12/2020
#Requestor: Forrest
#Goal: dataset showing whether person lived in congregate setting and location of death
#take data that Alison put together from vital records data and OCME (which has location of
#death) with CTEDSS data on deaths with location of whether somone lived in a congregate
#setting or not. Use data from Sue for congregate vs. not deaths. 

deaths <- read_csv("csv/2020-08-26/2020-08-26cases_wphi_missing facility names for deceased cases added_Deaths only.csv")

locations <- read_csv("ocme_data_here/2020-08-28location.csv")

deaths2 <- deaths %>% 
  left_join(locations, by="eventid") %>% 
  filter(outcome.x=="Died") %>% 
  select(eventid, disease_status.x, age.x, death_date.x, Location, `COVID Death`, `OCMENum`, symp_onset_date, hospitalized, admit_date,
         cong_setting.x, cong_exposure_type, cong_facility, FacillityType, Congregate) %>% 
  rename(FacilityType=FacillityType) %>% 
  mutate(Congregate = if_else(Congregate =="new", "yes", Congregate), 
         FacilityType = if_else(FacilityType=="al", "AL", FacilityType))


#the # deaths by type of residence (congregate / non-congregate) 
#and date of death at the state level? The latests data we have is from 07/08.
death_date <- deaths2 %>% 
  mutate(death_date.x = mdy(death_date.x)) %>% 
  group_by(disease_status.x, Congregate, death_date.x) %>% 
  tally() %>% 
  spread(disease_status.x, n)

write_csv(death_date, "csv/2020-08-28/2020-08-28deathsbydate.csv")

#proportion of community (non-congregate) deaths that occur 
#outside of CT hospitals? Is it more or less than 10% of those deaths?
locations2<-count(deaths2, Location, Congregate)


##########################################################
#Date 8/12/2020
#Requestor: Sydney/Gary
#Goal: geocoded cases in Bridgeport for past 2 weeks for heat mapping

bport <- read_csv('csv/2020-08-12/2020-08-12testgeo2.csv')

bport <- bport %>% 
  filter(city=="Bridgeport" & cong_test !="Yes") %>% 
  filter(spec_col_date > mdy("07-18-2020") & !is.na(GEOID10) & result2 == "Positive") %>% 
  select(eventid, `Event Date`, EventAgeYears, spec_col_date, city, county, X, Y, GEOID10, mmwrweek)

write_csv(bport, "csv/2020-08-12/2020-08-12bportcases_3weeks.csv")

##########################################################
#Date 8/11/2020
#requestor: OEC
#goal: cases by date by county

cases <- read_csv("csv/2020-08-11/2020-08-11cases.csv")

cases_date <- cases %>% 
  mutate(date=if_else(disease_status=="Confirmed" & !is.na(spec_col_date),
                     spec_col_date,
                     event_date)) %>% 
  mutate(date=if_else(date < mdy("03-01-2020"), mdy("03-01-2020"), date),
         week=epiweek(date)) %>% 
  group_by(county, week) %>% 
  tally() %>% 
  complete(week = full_seq(10, max(week), 1), fill =list(n = 0))%>%  
  rename(County=county,
         `Total Cases`=n,
         `MMWR week` = week) %>% 
  ungroup() %>% 
  mutate(County = if_else(is.na(County), "Pending Address Validation", County))
# early <- cases_date %>% 
#   filter(date < mdy("03-01-2020"))
    
write_csv(cases_date, "csv/2020-08-11/2020-08-11casebyweekbycounty.csv")


##########################################################
#Date 8/10/2020
#requestor: Steve Bart
#goal: deaths during 3/1-8/7/2020

deaths<- read_csv("csv/2020-08-07/2020-08-07cases_wphi.csv")

deaths <- deaths %>% 
  filter(outcome=="Died")

write_csv(deaths, "csv/2020-08-07/2020-08-07deathsSteve.csv")


##########################################################
#Date 8/10/2020
#requestor: SJ
#goal: town level % positivity for SDE
#goal2: limit to community tests
#testlist<-testgeo2
testlist <- read_csv(paste0("csv/", Sys.Date(), "/", Sys.Date(), "testgeo2.csv"))
test_county <- testlist %>%
  filter(cong_test !="Yes") %>% 
  mutate(week = epiweek(spec_col_date),
         result2=ifelse(result=="detected", "Positive",
                        ifelse(result=="not detected", "Negative", "Indeterminate"))) %>% 
  group_by(county, week, result2) %>%
  tally() %>% 
  spread(result2, n) %>% 
  replace_na(replace=list(Positive=0, Negative=0, Indeterminate=0)) 

test_t <- testlist %>%
  filter(cong_test !="Yes") %>% 
  mutate(week = epiweek(spec_col_date),
         result2=ifelse(result=="detected", "Positive",
                        ifelse(result=="not detected", "Negative", "Indeterminate")))  %>% 
  group_by(city, week, result2) %>%
  tally() %>% 
  spread(result2, n) %>% 
  replace_na(replace=list(Positive=0, Negative=0, Indeterminate=0)) 

write_csv(test_t, paste0("csv/", Sys.Date(), "/", Sys.Date(), "communitytest_town.csv"))
write_csv(test_county, paste0("csv/", Sys.Date(), "/", Sys.Date(), "communitytest_county.csv"))


##########################################################
#Date 8/6/2020
#requestor: Zainab Lasani via Michelle Gillman (approved by Lynn)
#goal: number of tests and community tests in Bridgeport for June-Aug

tests <- read_csv("csv/2020-08-06/2020-08-06testgeo2.csv")

#filter to bridgeport
bridgeport <- tests %>% 
  filter(city=="Bridgeport" & spec_col_date >=mdy("06-01-2020")) %>% 
  mutate(cong_test = ifelse(is.na(cong_test), "No", cong_test))

bridgeportday <- bridgeport %>% 
  group_by(spec_col_date, cong_test) %>% 
  tally() %>% 
  spread(cong_test,n) %>% 
  mutate(`total tests`=sum(No, Yes, na.rm=TRUE)) %>% 
  rename(`community tests`=No,
         `specimen collection date`=spec_col_date) %>% 
  select(-Yes)

bridgeportweek <- bridgeportday %>% 
  mutate(`MMWR week` = epiweek(`specimen collection date`)) %>% 
  group_by(`MMWR week`) %>% 
  summarise(`total tests`=sum(`total tests`), 
            `community tests` = sum(`community tests`)) %>% 
  mutate(year = 2020,
         `week starting` = MMWRweek2Date(MMWRyear=year, MMWRweek=`MMWR week`, MMWRday=1)) %>% 
  select(-year)

write_csv(bridgeportday, "csv/2020-08-06/2020-08-06brideportday.csv")
write_csv(bridgeportweek, "csv/2020-08-06/2020-08-06brideportweek.csv")


##########################################################
#Date 8/5/2020
#requestor: Dr. King via Lou Gonsalves and Lynn
#goal: number of new cases by 1 year age group during Mar 1- May 3, 2020
#censor ages <=18 and >100 to reduce identifiability

cases <- read_csv(paste0("csv/", Sys.Date(), "/", Sys.Date(), "cases_wphi.csv")) #%>%
  #filter(outcome=="Died")

# deaths05052020 <- read_csv("csv/2020-05-05/2020-05-05cases.csv")%>%
#   filter(outcome=="Died")%>%
#   group_by(age)%>%
#   tally()

cases_may3 <- cases %>%
  filter(mmwrweek <= 18 & !is.na(age)) %>%
  mutate(age = ifelse(age <= 18, 18, 
                      ifelse(age >100, 101, age))) %>% 
  group_by(age) %>% 
  tally() %>% 
  complete(age = seq(19, 99, by = 1), fill=list(n=0)) %>%
  mutate(age = ifelse(age <= 18, "<=18", 
                      ifelse(age >100, ">100", age)))

write_csv(cases_may3, "csv/2020-09-09/2020-09-09casesbyage.csv")


##########################################################
#Date 8/5/2020
#requestor: Nancy
#goal: number of new cases reported in past 24 hours by town

oldcase <- read_csv(paste0("csv/", Sys.Date()-1, "/", Sys.Date()-1, "cases_wphi.csv")) %>% 
  filter(outcome=="Died")

newcase <- read_csv(paste0("csv/", Sys.Date(), "/", Sys.Date(), "cases_wphi.csv")) %>% 
  filter(outcome=="Died")

diff <- newcase %>%
  mutate(bigID=paste0(fname, lname, dob)) %>% 
  filter(!eventid %in% oldcase$eventid)

gone <- oldcase %>%
  mutate(bigID = paste0(fname, lname, dob)) %>% 
  filter(!eventid %in% newcase$eventid)

diff2 <- diff %>%
  filter(!bigID %in% gone$bigID) %>%
  group_by(city) %>% 
  tally()

date <- diff %>%
  filter(!bigID %in% gone$bigID) %>%
  group_by(spec_col_date) %>% 
 tally()

gone2 <- gone %>%
  filter(!bigID %in% diff$bigID)

count(diff2, city)

write_csv(diff2, "csv/2020-08-05/2020-08-05newcases.csv")


##########################################################
#Date 8/4/2020; 8/19/2020
#requestor: Bethel HD ; Mansfield HD
#goal: number of cases in Bethel/Mansfield over time

bethel <- read_csv("csv/2020-08-04/2020-08-04cases.csv")
mansfield <- read_csv("csv/2020-08-19/2020-08-19cases_wphi.csv")
bethel2 <- bethel %>% 
  filter(city=="Bethel") %>%
  mutate(date=if_else(disease_status=="Confirmed" & !is.na(spec_col_date),
                      spec_col_date, event_date))%>%
  group_by(date) %>% 
  tally() %>%
  complete(date = seq.Date(min(date), max(date), by = "day"), fill =list(n = 0))
mansfield2 <- mansfield %>% 
  filter(city=="Mansfield") %>% 
  mutate(date=if_else(disease_status=="Confirmed" & !is.na(spec_col_date),
                      spec_col_date, event_date),
         innew=1)%>%
  group_by(date) %>% 
  tally() %>%
  complete(date = seq.Date(min(date), max(date), by = "day"), fill =list(n = 0))
ggplot(mansfield2)+
  geom_line(aes(x=date, y=n))+
  xlim(mdy("03-01-2020", "08-19-2020"))+
  ylim(0,20)+
  theme_bw()+
  labs(
    y="Number of Cases",
    x="Specimen Collection or Onset Date",
    title="COVID-19 Cases among Residents of Mansfield, CT"
  )
  
recentmansfield <- mansfield2 %>% 
  filter(date >= ymd("2020-08-01"))

#compare 8/14 to 8/19 case list
mansfield814 <- read_csv("csv/2020-08-14/2020-08-14cases_wphi.csv") %>% 
  filter(city=="Mansfield") %>% 
  mutate(in814=1)

diff <- mansfield2 %>% 
  left_join(mansfield814, by="eventid") %>% 
  filter(is.na(in814))
diff2 <- mansfield814 %>% 
  left_join(mansfield2, by="eventid") %>% 
  filter(is.na(innew))

##########################################################
#Date 8/4/2020
#requestor Shiwani 
#goal: number of cases during March 1- May 31 by congregate setting vs. not with age >=18
#issue: missing congregate setting data from various overwrites in CTEDSS

#looked at 6/4 NH and AL reports:
  #8517 cases in NH residents
  #1041 cases in AL residents
#looked at 6/1 report from DOC on OPen data portal
  #870 cases in DOC inmates

#look at 8/4/2020 data for total # of cases in CTEDSS
case <- read_csv("csv/2020-08-04/2020-08-04cases.csv")

case2 <- case %>%
  mutate(date=if_else(disease_status=="Confirmed" & !is.na(spec_col_date), 
                      spec_col_date, event_date)) %>% 
  filter(date < mdy("06-01-2020") & age >=18) %>% 
  tally()

##########################################################
#Date 7/31/2020
#requestor OEC and Shiwani
#goal: congregate setting cases vs. not during March 1 - June 1 and all time
#issue: missing congregate setting data from various overwrites in CTEDSS


case730 <- read_csv("csv/2020-07-30/2020-07-30cases_wphi.csv")
case7302 <- case730 %>%
  select(eventid, fname, lname, dob, city, cong_setting, cong_exposure_type, cong_facility, spec_col_date) %>%
  mutate(cong_yn730= if_else((str_detect(cong_setting, "Assisted Living Facility") |
                                 str_detect(cong_setting, "Long term care facility") |
                                 str_detect(cong_setting, "Jail / Prison")) & 
                                cong_exposure_type =="Reside", "Yes", "No"))
count(case7302, cong_yn730)
  
case722 <- read_csv("csv/2020-07-22/2020-07-22cases_wphi.csv")
case7222 <- case722 %>%
  select(eventid, fname, lname, dob, city, cong_setting, cong_exposure_type, cong_facility) %>%
  rename(fname722=fname, lname722=lname, dob722=dob, 
         city722=city, cong_setting722=cong_setting, cong_exposure_type722=cong_exposure_type,
         cong_facility722=cong_facility) %>% 
  mutate(cong_yn722 = if_else((str_detect(cong_setting722, "Assisted Living Facility") |
                             str_detect(cong_setting722, "Long term care facility") |
                               str_detect(cong_setting722, "Jail / Prison")) & 
                               cong_exposure_type722 =="Reside", "Yes", "No"))
count(case7222, cong_yn722)

case621 <- read_csv("csv/2020-06-21/2020-06-21cases.csv")
case6212 <- case621 %>%
  select(eventid, city, cong_setting, cong_exposure_type) %>%
  rename(city621=city, cong_setting621=cong_setting, cong_exposure_type621=cong_exposure_type) %>%
  mutate(cong_yn621 = if_else(
    (str_detect(cong_setting621, "Jail / Prison") | str_detect(cong_setting621, "Long term care facility/assisted living")) &
      str_detect(cong_exposure_type621, "Reside"), "Yes", "No"))
count(case6212, cong_yn621)

caseall <- case7302 %>%
  left_join(case7222, by="eventid") %>% 
  left_join(case6212, by="eventid") %>%
  mutate(congagree = ifelse( (cong_yn730==cong_yn722 & cong_yn730==cong_yn621) |
          (is.na(cong_yn730) & is.na(cong_yn722) & is.na(cong_yn621)), 1, 0))
combos <- count(caseall, cong_yn730, cong_yn722, cong_yn621)
count(caseall,congagree)

#filter to cases with potential issues
caseissue <- caseall %>%
  filter(congagree !=1 | is.na(congagree)) %>%
  mutate(newer621 = ifelse(is.na(city621), 1, 0),
         newer722 = ifelse(is.na(city722), 1, 0),
         newest = ifelse(newer621==1 & newer722==1, 1, 0),
         agree722730 = ifelse(newer621==1 & cong_yn722==cong_yn730, 1, 0))
count(caseissue, newest, agree722730)

#take out cases that are brand new or newer than 621 and agree for 722 and 730
caseissue2 <- caseissue %>%
  filter(newest!=1 | agree722730 == 1)

#find cases that aren't in group of case issues and were in 621 or 722 as cong case
cong621miss <- case6212 %>%
  filter(cong_yn621=="Yes" & ! eventid %in% caseall$eventid)
cong722miss <- case7222 %>%
  filter(cong_yn722=="Yes" & ! eventid %in% caseall$eventid)

congmiss <- cong621miss %>%
  full_join(cong722miss, by="eventid")


##########################################################
#Date 7/30/2020
#requestor: SDE
#goal: CLI + ILI by county by 7-day rolling average 

sys <- read_csv("csv/2020-07-30/roll7c.csv") %>% 
  mutate(date=mdy(date))

counties<-sys$county

for (i in seq_along(counties)) {
  county_sys <- ggplot(sys[sys$county == counties[i],])+
  geom_line(aes(x=date, y=mean7, color=ccdd))
}

county_sys

ggplot(sys[sys$county == "Connecticut",])+
  geom_line(aes(x=date, y=mean7, col=ccdd, group=ccdd))

sys2<-sys %>%
  filter(county=="Connecticut") %>%
  mutate(date=mdy(date))
ggplot(sys2)+
  geom_line(aes(x=date, y=mean7, color=ccdd))
  
#ggsave(county_sys, file=paste0(counties[i], " _cliili.png"), scale = 2, path = path)

##########################################################
#Date 7/28/2020
#requestor: Jim Hadler/Albert Ko
#goal: % of tests reported to DPH with 48 hours of specimen collection by week 
elr_timely <- elr_linelist %>% 
  mutate(tooearly = if_else(mdy(date_reported_dph) < spec_col_date, 1, 0),
         tat = if_else(
           tooearly!=1, days(mdy(date_reported_dph)-spec_col_date), 
           9999), 
         tat = na_if(tat, 9999),
         tooearly2 = if_else(mdy(date_reported_dph)< mdy(date_tested), 1, 0),
         totest = if_else(
           tooearly2!=1, days(mdy(date_tested)-spec_col_date), 
           9999),
         totest = na_if(totest, 9999),
         mmwrweek = ifelse(!is.na(spec_col_date), epiweek(spec_col_date), NA),
         report48 = if_else(tat <= days(2), "under48", "over48"),
         test48 = if_else(totest <= days(2), "under48", "over48")) %>%
  replace_na(list(report48="missing", test48="missing"))

elr_timelywk <- elr_timely %>%
  group_by(mmwrweek, report48) %>%
  tally() %>%
  spread(report48, n) %>%
  filter(!is.na(mmwrweek)) %>%
  mutate(total = sum(under48, over48, na.rm=TRUE),
         pct48 = under48/total*100,
         year = 2020,
         date = MMWRweek2Date(MMWRyear=year, MMWRweek=mmwrweek, MMWRday=1))
  
ggplot(elr_timelywk)+
  geom_line(aes(x=date, y=pct48, color=total), size = 1.5)+
  #geom_col(aes(x=date, y=total))+
  theme_classic()+
  labs(
    title="Percentage of test results reported to DPH \nwithin 48 hours of specimen collection, by week",
    y = "Percentage",
    x = "Specimen Collection Date"
  )
  
  
issues <- elr_timely %>%
  filter(tooearly ==1 )

count(elr_timely,tooearly, tooearly2)
count(elr_timely, report48)

##########################################################
#Date 7/28/2020
#requestor: Josh
#goal: per capita testing by age group
age_labels <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", ">=80")
pop <- read_csv("population_data/2018_dph_asrh/2018_asrh.csv") %>% 
  select(age_g, total) %>% 
  filter(!is.na(age_g))

elr_age <- elr_linelist %>%
  filter(!is.na(spec_col_date)) %>%
  mutate(age = as.numeric(EventAgeYears),
      age_group = cut(age, breaks = c(-1,9,19,29,39,49,59,69,79, Inf),labels = age_labels),
      mmwrweek = epiweek(spec_col_date)) %>%
  group_by(age_group, mmwrweek) %>% 
    tally() %>% 
    filter(!is.na(age_group)) %>% 
    left_join(pop, by=c("age_group" = "age_g")) %>% 
    ungroup() %>% 
    mutate(rate_100k = (n/total)*100000,
           year = 2020,
           date = MMWRweek2Date(MMWRyear=year, MMWRweek=mmwrweek, MMWRday=1)) %>%
  filter(date < mdy("07-20-2020"))

elr_age$age_group <- factor(elr_age$age_group, labels = age_labels, levels = age_labels )

ggplot(elr_age)+
  geom_line(aes(date, rate_100k, col = age_group), size =1)+
  scale_y_continuous(breaks = seq(0, 10000, by =500))+
  scale_x_date(limits=c(ymd("2020-03-01", max(elr_age$date))), date_labels = "%b-%d", date_breaks = "7 days")+
#  scale_color_brewer(palette = "Blues")+
  theme_minimal()+
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x=element_text(angle=45,hjust=1),
    legend.background = element_rect(),
    #legend.justification=c(1,1),
    #legend.position = c(1,1)
  )+
  labs(
    title= "Weekly rates of COVID-19 PCR tests by age group",
    subtitle = "As of 7/26/2020 at 8:30 PM",
    x="",
    y="Tests per 100,000 population",
    legend.position="top"
  )

##########################################################
#Date 7/27/2020
#requestor: Lynn
#goal:number of cases with information on pregnancy

newcases <- read_csv("csv/2020-07-27/2020-07-27cases_wphi.csv")
count(newcases, gender, preg)


##########################################################
#Date 7/27/2020
#requestor: Commissioner
#goal: describe increase in cases age 20-29 and <40 in recent weeks

#read in case dataset
newcases <- read_csv("csv/2020-07-27/2020-07-27cases_wphi.csv")

newcases <- newcases %>%
  mutate(under40 = ifelse(age < 40, "under40", "over40"),
         twenties = ifelse(age >=20 & age <=29, "twenties", "not twenties")) %>%
  replace_na(list(under40="noage", twenties="noage"))
count(newcases, under40, twenties)

weeklypct40 <- newcases %>%
  group_by(mmwrweek, under40)%>%
  tally() %>%
  spread(under40, n) %>%
  mutate(total=sum(under40,over40,noage, na.rm=TRUE),
         pct40 = under40/total*100,
        year = 2020,
        date = MMWRweek2Date(MMWRyear=year, MMWRweek=mmwrweek, MMWRday=1))

weeklypct20 <- newcases %>%
  group_by(mmwrweek, twenties)%>%
  tally() %>%
  spread(twenties, n) %>%
  mutate(total=sum(twenties,`not twenties`,noage, na.rm=TRUE),
         pct20 = twenties/total*100, 
         year = 2020,
         date = MMWRweek2Date(MMWRyear=year, MMWRweek=mmwrweek, MMWRday=1))

#monthly percentage of cases
newcases <- newcases %>%
  mutate(date=if_else(is.na(spec_col_date), event_date, spec_col_date),
         month=month(date))

monthpct40 <- newcases %>%
  group_by(month, under40)%>%
  tally() %>%
  spread(under40, n) %>%
  mutate(total=sum(under40,over40,noage, na.rm=TRUE),
         pct40 = under40/total*100)

monthpct20 <- newcases %>%
  group_by(month, twenties)%>%
  tally() %>%
  spread(twenties, n) %>%
  mutate(total=sum(twenties,`not twenties`,noage, na.rm=TRUE),
         pct20 = twenties/total*100)

#weekly incidence by age group
age_labels <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", ">=80")
pop <- read_csv("population_data/2018_dph_asrh/2018_asrh.csv") %>% 
  select(age_g, total) %>% 
  filter(!is.na(age_g))
agegrp <- newcases %>%
  filter(!is.na(age)) %>% 
  mutate(age_group = 
           cut(age, breaks = c(-1,9,19,29,39,49,59,69,79, Inf),labels = age_labels)) %>% 
  group_by(age_group, mmwrweek) %>% 
  tally() %>% 
  filter(!is.na(age_group)) %>% 
  left_join(pop, by=c("age_group" = "age_g")) %>% 
  ungroup() %>% 
  mutate(rate_100k = (n/total)*100000,
         year = 2020,
         date = MMWRweek2Date(MMWRyear=year, MMWRweek=mmwrweek, MMWRday=1)) #%>% 
 # select(-c(n,total))
agegrp$age_group <- factor(agegrp$age_group, labels = age_labels, levels = age_labels )
ggplot(agegrp)+
  geom_line(aes(date, rate_100k, col = age_group), size =1)+
  scale_y_continuous(breaks = seq(0, 300, by =50))+
  scale_x_date(limits=c(ymd("2020-03-01", max(agegrp$date))), date_labels = "%b-%d", date_breaks = "7 days")+
  scale_color_brewer(palette = "Dark2")+
  theme_minimal()+
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x=element_text(angle=45,hjust=1),
    legend.background = element_rect(),
    #legend.justification=c(1,1),
    #legend.position = c(1,1)
  )+
  labs(
    title= "Weekly rates of COVID-19 cases by age group",
    subtitle = "As of 7/26/2020 at 8:30 PM",
    x="",
    y="Rate per 100,000 population",
    legend.position="top"
  )

write_csv(agegrp, "csv/2020-07-27/2020-07-27agegrp.csv")

##########################################################
#date 7/24/2020
#requestor: Sydney JOnes
#goal: identify change in cases to see how many are old after Enzo update

#read cases from last day
oldcases <- read_csv("csv/2020-09-02/2020-09-02cases_wphi.csv") %>%
  filter(outcome=="Died") %>% 
  mutate(oldcase = 1)

newcases <- read_csv("csv/2020-09-03/2020-09-03cases_wphi.csv") %>% 
  filter(outcome=="Died")

newdeath <- newcases %>% 
  filter(!eventid %in% oldcases$eventid)

missingdeath <- oldcases %>% 
  filter(!eventid %in% newcases$eventid)

newcases2 <- newcases %>%
  left_join(oldcases, by="eventid") %>%
  filter(is.na(oldcase)) %>% 
  mutate(bigID = paste0(fname.x, lname.x, dob.x))

bydate<-newcases2 %>%
  group_by(spec_col_date.x) %>%
  tally()

missing <- oldcases %>%
  filter(!eventid %in% newcases$eventid) %>% 
  mutate(bigID = paste0(fname, lname, dob))

newcases3 <- newcases2 %>%
  filter(! bigID %in% missing$bigID)

missing2 <- missing %>%
  filter(!bigID %in% newcases2$bigID)

write_csv(newcases3, "csv/2020-07-24/2020-07-24_catchupcases.csv")

#write_csv(newdeaths2,"csv/2020-07-14/2020-07-14_ctedssdeaths.csv")



##########################################################
#date 7/16/2020
#requestor Sue
#goal: match up data from 7/14 and 7/15 case files so that correct congregate setting information is included

cases710 <- read_csv("csv/2020-07-10/2020-07-10cases_wphi.csv")
cases714 <- read_csv("csv/2020-07-14/2020-07-14cases_wphi.csv")
cases715 <- read_csv("csv/2020-07-15/2020-07-15cases_wphi.csv")
cases716 <- read_csv("csv/2020-07-16/2020-07-16cases_wphi.csv")
cases717 <- read_csv("csv/2020-07-17/2020-07-17cases_wphi.csv")



cases710 <- cases710 %>%
  rename(cong_setting10 = cong_setting,
         cong_facility10 = cong_facility,
         cong_exposure_type10 = cong_exposure_type)%>%
  mutate(in10=1) %>%
  select(eventid, cong_setting10, cong_facility10, cong_exposure_type10, in10)

cases714 <- cases714 %>%
  rename(cong_setting14 = cong_setting,
         cong_facility14 = cong_facility,
         cong_exposure_type14 = cong_exposure_type) %>%
  select(eventid, cong_setting14, cong_facility14, cong_exposure_type14)

cases715 <- cases715 %>%
  rename(cong_setting15 = cong_setting,
         cong_facility15 = cong_facility,
         cong_exposure_type15 = cong_exposure_type) %>%
  select(eventid, cong_setting15, cong_facility15, cong_exposure_type15)

cases716 <- cases716 %>%
  rename(cong_setting16 = cong_setting,
         cong_facility16 = cong_facility,
         cong_exposure_type16 = cong_exposure_type) %>%
  select(eventid, cong_setting16, cong_facility16, cong_exposure_type16)

cases717 <- cases717 %>%
  left_join(cases710, by="eventid") %>%
  left_join(cases714, by="eventid") %>%
  left_join(cases715, by="eventid") %>%
  left_join(cases716, by="eventid")
  
cases717_2 <- cases717 %>%
  filter(in10==1)%>%
  filter((!is.na(cong_facility10) & is.na(cong_facility)) |
           (cong_facility10 !=cong_facility))

write_csv(cases717_2, "csv/2020-07-17/2020-07-17cases_LTCFcompare.csv")

##########################################################
#date 7/15/2020
#requestor: sydney
#goal: identify probable cases that are not deaths with no symptom info and no tests that might need reclassification

probables <- df %>%
  filter(disease_status == "Probable" &
           (outcome != "Died" | is.na(outcome)) &
           is.na(test) )

sympsdf <- probables %>%
  select(eventid,disease_status,fever, fatigue, sob, headache, cough, chills, myalgia, 
         new_olf_taste,rigors, sore_throat, pneumonia, ards)  %>%
  mutate(fever = if_else(fever %in% c("Yes", "Not taken"), "Yes", "No"))
two_or_more_symps <- sympsdf %>% 
  select(eventid, disease_status, fever, chills, rigors, myalgia, headache, sore_throat, new_olf_taste) %>% 
  pivot_longer(-c(eventid, disease_status), names_to = "symptom", values_to = "symp_pres") %>% 
  filter(symp_pres == "Yes") %>% 
  group_by(eventid, disease_status) %>% 
  tally() %>% 
  filter(disease_status == "Probable"  & n >= 2) %>% 
  select(eventid) %>% 
  unique()
one_of_symps <- sympsdf %>%   
  select(eventid, disease_status, cough, sob, ards, pneumonia) %>% 
  pivot_longer(-c(eventid, disease_status), names_to = "symptom", values_to = "symp_pres") %>% 
  filter(symp_pres == "Yes") %>% 
  group_by(eventid, disease_status) %>% 
  tally() %>% 
  filter(disease_status == "Probable") %>% 
  select(eventid) %>% 
  unique()
nosymp_prob <- probables %>% 
  filter(!eventid %in% one_of_symps$eventid & !eventid %in% two_or_more_symps$eventid) %>% 
  unique() 

rm(one_of_symps)
rm(two_or_more_symps)

#probables with negative test result (not deceased)
probables_but_why <- case %>% 
  filter(str_detect(test, pcrtestlist) 
        & disease_status == "Probable" 
        & str_detect(str_to_lower(result), "not detected|negative") 
        & (is.na(outcome)| outcome != "Died" ))

write_csv(probables_but_why, paste0("csv/", Sys.Date(), "/", Sys.Date(), "probable+negative.csv"))
write_csv(nosymp_prob, paste0("csv/", Sys.Date(), "/", Sys.Date(), "probable+notest+nosymp.csv"))


##########################################################
#date 7/15/2020
#requestor: sydney
#goal : look at ge distribution for people tested in Hartford, New Haven and Fairfield County

countyage <- elr_linelist %>%
  filter(county %in% c("Hartford County", "New Haven County", "Fairfield County")) %>%
  mutate(age = as.numeric(EventAgeYears)) %>%
  group_by(county, age) %>%
  tally()

library(plyr)
mu<- ddply(countyage, "county", summarise, grp.med=median(age, na.rm=TRUE))

ggplot(countyage, aes(x=age, y=n, color=county))+
  geom_area(fill="white", position="identity", alpha=0.5) #+
  #geom_vline(data=mu, aes(xintercept=grp.med, color=county),
   #          linetype="dashed")+
  #geom_density(alpha=.2)
  
install.packages("ggridges")
library(ggridges)

ggplot(countyage, aes(x=age, y=county, group=county, fill=median(age, na.rm=TRUE)))+
  geom_density_ridges(scale=0.9)

ggplot(casenew, aes(x=age, y=county, group=county))+
  geom_density_ridges()


##########################################################
#date 7/13/2020
#requestor: Josh Geballe
#goal: when were the new deaths 

#read deaths from last day
olddeaths <- read_csv("csv/2020-08-31/2020-08-31cases_wphi.csv") %>%
  filter(outcome=="Died") %>%
  mutate(olddeath = 1)

newdeaths <- read_csv("csv/2020-09-01/2020-09-01cases_wphi.csv") %>%
  filter(outcome=="Died")

newdeaths2 <- newdeaths %>%
  left_join(olddeaths, by="eventid") %>%
  filter(is.na(olddeath))
count(newdeaths2, olddeath)

missing <- olddeaths %>%
  filter(!eventid %in% newdeaths$eventid)

write_csv(newdeaths2,"csv/2020-07-14/2020-07-14_ctedssdeaths.csv")

#######################################################
#date 7/8/2020
#requestor: Matt Cartter
#Number of people who had PCR tests in June in CT
#number of poeple who had serology tests in June in CT

mar.aprtest <- elr_linelist %>%
  #filter to tests in June
  filter(spec_col_date >= mdy("03-01-2020") & spec_col_date <= mdy("04-30-2020")) %>% 
  #create unique id
  mutate(bigID = paste0(`First Name`, `Last Name`, `Birth Date`)) %>%
  #count people tested
  distinct(bigID)

maytest <- elr_linelist %>%
  #filter to tests in June
  filter(spec_col_date >= mdy("05-01-2020") & spec_col_date <= mdy("05-31-2020")) %>% 
  #create unique id
  mutate(bigID = paste0(`First Name`, `Last Name`, `Birth Date`)) %>%
  #count people tested
  distinct(bigID)

junetest <- elr_linelist %>%
  #filter to tests in June
  filter(spec_col_date >= mdy("06-01-2020") & spec_col_date <= mdy("06-30-2020")) %>% 
  #create unique id
  mutate(bigID = paste0(`First Name`, `Last Name`, `Birth Date`)) %>%
  #count people tested
  distinct(bigID)
 
prejulytest <- elr_linelist %>%
  #filter to tests in June
  filter(spec_col_date <= mdy("06-30-2020")) %>% 
  #create unique id
  mutate(bigID = paste0(`First Name`, `Last Name`, `Birth Date`)) %>%
  #count people tested
  distinct(bigID)

library(DBI)
#bring in data that is output from geocoding
con <- DBI::dbConnect(odbc::odbc(), "epicenter")
statement <- paste0("SELECT [r].[PatientLastName]
                              ,[r].[PatientFirstName]
                              ,[r].[PatientDOB]
                              ,[r].[SpecimenID]
                              ,[r].[SpecimenCollectionDate]
                              ,[r].[SpecimenSource]
                              ,[r].[Result]
                    FROM [DPH_COVID_IMPORT].[dbo].[Serology_TSV_IMPORT] as [r]")
tsvserology <-  DBI::dbGetQuery(conn = con , statement = statement)
odbc::dbDisconnect(con)

tsvserology <- tsvserology %>% 
  mutate(bigID = str_to_lower(paste0(PatientFirstName, PatientLastName, PatientDOB)),
         SpecimenCollectionDate = ymd(SpecimenCollectionDate))

serologytest <- tsvserology %>%
  distinct(
    PatientFirstName, PatientLastName, PatientDOB, SpecimenCollectionDate, SpecimenID, Result, .keep_all = TRUE
  ) %>%
  distinct(
    PatientFirstName, PatientLastName, PatientDOB, SpecimenCollectionDate, SpecimenSource, Result, .keep_all = TRUE
  )
  
serotestcount <- serologytest %>%
  group_by(month(SpecimenCollectionDate)) %>%
  tally()

serologytestpersonmarapr <-serologytest %>%
  filter(month(SpecimenCollectionDate) ==3 | month(SpecimenCollectionDate)==4) %>%
  distinct(bigID)
serologytestpersonmay <-serologytest %>%
  filter(month(SpecimenCollectionDate) ==5) %>%
  distinct(bigID)
serologytestpersonjun <-serologytest %>%
  filter(month(SpecimenCollectionDate) ==6) %>%
  distinct(bigID)

serolotytestprejuly <- serologytest %>% 
  filter(SpecimenCollectionDate < mdy("07-01-2020")) %>%
  distinct(bigID)


########################################################
#date: 7/6/2020
#requestor: Lynn Sosa for ABC News request
#average age of hospitalized and decedents changed over time?

#hosptializations
hosp <- case %>%
  filter(hospitalized == "Yes") %>% 
  mutate(admit_date = if_else(is.na(admit_date), spec_col_date, admit_date)) %>%
  group_by(month(admit_date)) %>%
  summarise(mage = mean(age, na.rm=TRUE), n = n(), median = median(age, na.rm=TRUE))


#deaths 
death <- case %>%
  filter(outcome == "Died") %>% 
  group_by(month(death_date)) %>% 
  summarise(mage = mean(age, na.rm=TRUE), n = n(), median = median(age, na.rm=TRUE))


caseage <- case %>%
  group_by(month(spec_col_date)) %>%
  summarise(mage = mean(age, na.rm=TRUE), n = n(), median = median(age, na.rm=TRUE))


##################################################
#new cases compared to previous reporting day

oldcases <- read_csv("csv/2020-07-30/2020-07-30cases_wphi.csv")

case <- read_csv("csv/2020-07-31/2020-07-31cases_wphi.csv")
newcases <- case %>%
  filter(! eventid %in% oldcases$eventid)

write_csv(newcases, "csv/2020-07-31/2020-07-31newcases2.csv")

missingcases <- oldcases %>%
  filter(! eventid %in% case$eventid)

dates <- newcases %>%
  group_by(spec_col_date) %>%
  tally()

#cases554 <- read_csv("csv/2020-07-29/2020-07-29cases_wphi554.csv")

#caesdiff<- cases554 %>%
#  filter(! eventid %in% case$eventid)


#casediff2 <- read_csv("csv/2020-07-29/2020-07-29newcases.csv")

casediff2 <- casediff2 %>%
  filter(! eventid %in% newcases$eventid)

##################################################
#count of deaths 3/1 to 5/31 (inclusive) for Harlan and Shawani at Yale
#date: 6/29/2020
#data: case with PHI from 6/25/2020 to be consistent with Sue Petit's classification of congregate setting deaths

#desired age groups
agegrpl <- c("<18", "18-29", "30-44", "45-54", "55-64", "65-74", ">=75") 

#read in data
cases <- read_csv("csv/2020-06-25/2020-06-25cases_wphi.csv") %>%
  mutate(death_date = mdy(death_date), 
         gender = if_else(is.na(gender), "Unknown", gender), 
         agegrp = cut(age, breaks = c(-1, 17, 29, 44, 54, 64, 74, Inf), labels=agegrpl)) %>%
  filter(death_date > mdy("02-28-2020") & death_date < mdy("06-01-2020") & age >= 18)


#create table
case_all <- count(cases, disease_status) %>%
  spread(disease_status, n) %>%
  mutate(charac="Total")
case_sex <- count(cases, disease_status, gender) %>%
  spread(disease_status, n) %>%
  rename(charac=gender)
case_re <- count(cases, disease_status, hisp_race) %>%
  spread(disease_status, n)%>%
  rename(charac=hisp_race)
case_age <- count(cases, disease_status, agegrp) %>%
  spread(disease_status, n) %>%
  rename(charac=agegrp)
case_county <- count(cases, disease_status, county) %>%
  spread(disease_status, n)%>%
  rename(charac=county)
case_city <- count(cases, disease_status, city) %>%
  spread(disease_status, n)%>%
  rename(charac=city)
table1 <- bind_rows(case_all, case_sex, case_re, case_age, case_county, case_city)

write_csv(table1, "csv/2020-06-29/2020-06-29deathdemog.csv")

#find deaths missing gender and r/e
genmiss <- cases %>%
  filter(gender == "Unknown")
racemiss <- cases %>% 
  filter(hisp_race =="Unknown")


#############################################
#date: 6/26/2020
#look at completeness of ordering facility type
facil_type <- elr_linelist %>%
  group_by(str_to_lower(auth_facility)) %>%
  tally()
##2588 different entries
#missing for 66940 tests


order_type <- elr_linelist %>%
  group_by(ordering_lab) %>%
  tally()
#ordering lab is missing for most results --Not helpful.



#####################################
# look for cases without spec col date
nospecdt <- case %>%
  filter(disease_status =="Confirmed" & is.na(spec_col_date))


# look for lab results without spec col date 
nospecdt_t <- elr_linelist %>%
  filter(is.na(spec_col_date)) %>%
  mutate(diff = as.Date(date_tested, "%m/%d/%Y") - as.Date(`Event Date`, "%m/%d/%Y"))
count(nospecdt_t, is.na(date_tested))
days<-count(nospecdt_t, diff)


bylab<-count(nospecdt_t, lab_facility)

#look for onset or collection prior to birth date 
toosoon <- case %>%
  mutate(dob = as.Date(dob, "%m/%d/%Y")) %>%
  filter((ymd(spec_col_date) < dob) | (symp_onset_date < dob))

toosoon2 <- elr_linelist %>%
  mutate(dob = as.Date(`Birth Date`, "%m/%d/%Y")) %>%
  filter((ymd(spec_col_date) < dob) | (is.na(spec_col_date) & (as.Date(date_tested, "%m/%d/%Y") < dob))) %>%
  select(c("eventid", "First Name", "Last Name", "Birth Date", "spec_col_date", "spec_num", "source", "date_tested", "lab_facility", "auth_facility"))
write_csv(toosoon2, "csv/labpriortoDOB.csv")


# ###################################
# Request: cases by race/ethnicity by date
# Request date: 6/25/2020
# Date fulfilled: 6/25/2020
# Requested by: Dr Laurencin
# Sent to: Rachel King

casenew <- read_csv(paste0("csv/", Sys.Date(), "/", Sys.Date(), "cases_wphi.csv"),
                    col_types = cols(spec_col_date = col_character(), 
                                     event_date = col_character())) %>%
  mutate(date = ifelse(!is.na(spec_col_date), spec_col_date, event_date),
         date = mdy(date),
         week = epiweek(date))



#number of cases by race/ethnicity by date
casedate <- casenew %>%
  group_by(date, hisp_race) %>%
  tally() %>%
  spread(hisp_race, n)


caseweek <- casenew %>%
  group_by(week, hisp_race) %>%
  tally() %>%
  spread(hisp_race, n)

write_csv(casedate, "csv/2020-06-25/2020-06-25_casedate.csv")

write_csv(caseweek, "csv/2020-06-25/2020-06-25_caseweek.csv")




# ###################################
# Request: info on cases 5 years old and under
# Request date: 6/22/2020
# Date fulfilled: 6/22/2020
#UPDATED 7/10/2020 - cases 18 and younger
# Requested by: Lynn Sosa
# Sent to: Lexi Edmundson
casenew <- read_csv(paste0("csv/", Sys.Date(), "/", Sys.Date(), "cases_wphi.csv"),
                    col_types = cols(spec_col_date = col_character(), 
                                     event_date = col_character()))
under5 <- casenew %>%
  filter(age <= 5) %>%
  select(-c("phone", "fname", "lname", "Street address", "preg"))
under18 <- casenew %>%
  filter(age <= 18) %>%
  select(-c("phone", "fname", "lname", "Street address", "preg"))

write_csv(under5, "csv/2020-06-26/2020-06-26_5+under.csv")
write_csv(under18, "csv/2020-07-10/2020-07-10_18+under.csv")


#############################################
# Request: info on deaths to reconcile with vital records
  #Needs: Name, DOB, town residence, hospital (+ name), gender, date of death, congregate facility if any, OCME#
# Request date: 6/22/2020
# Date fulfilled: 6/23/2020
# Requested by: Karyn Backus
# Sent to:  Karyn Backus

#input is 'case' from ctedss_ocme_ltcf script which includes all records from ctedss
casedeath <- case %>%
  filter(outcome == "Died") %>%
  select(eventid, disease_status, fname, lname, dob, gender, city, death_date, `Died with COVID-19`, hospitalized, hospital_name, cong_setting, `Name of facility`)

#input is 'all_ocme_match' from ctedss_ocme_ltcf script, which includes all matching of OCME and CTEDSS data
matchrecs <- all_ocme_match %>%
  select(DOD, OCMENum, Name, DOB, Sex, Location, `Town of Death`, `COVID Death`, eventid)

casedeath2 <- casedeath %>%
  left_join(matchrecs, by = "eventid")
write_csv(casedeath2, "csv/2020-06-23/2020-06-23casedeath.csv")
