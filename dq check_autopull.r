.libPaths(c("L:/library", .libPaths()))
library(rmarkdown, lib.loc = .libPaths()[1])
library(tidyverse, lib.loc = .libPaths()[1])
library(ggplot2, lib.loc = .libPaths()[1])
#library(ggspatial, lib.loc = .libPaths()[1])
library(data.table, lib.loc = .libPaths()[1])
library(sf, lib.loc = .libPaths()[1])
library(odbc, lib.loc = .libPaths()[1])
library(tm, lib.loc = .libPaths()[1])
library(viridis, lib.loc = .libPaths()[1])
library(kableExtra, lib.loc = .libPaths()[1])
library(webshot, lib.loc = .libPaths()[1])
#library(geosphere, lib.loc = .libPaths()[1])
library(pracma, lib.loc = .libPaths()[1])
library(tidytext, lib.loc = .libPaths()[1])
library(ggraph, lib.loc = .libPaths()[1])
library(igraph, lib.loc = .libPaths()[1])
library(widyr, lib.loc = .libPaths()[1])
library(stringr, lib.loc = .libPaths()[1])
library(lubridate, lib.loc = .libPaths()[1])
library(readr, lib.loc = .libPaths()[1])
library(leaflet, lib.loc = .libPaths()[1])
library(RColorBrewer, lib.loc = .libPaths()[1])
library(ggmap, lib.loc = .libPaths()[1])
library(rgdal, lib.loc = .libPaths()[1])
library(tigris, lib.loc = .libPaths()[1])
library(acs, lib.loc = .libPaths()[1])
library(tidycensus, lib.loc = .libPaths()[1])
library(DT, lib.loc = .libPaths()[1])
library(formatR, lib.loc = .libPaths()[1])
library(knitr, lib.loc = .libPaths()[1])
library(MMWRweek, lib.loc = .libPaths()[1])
library(plotly, lib.loc = .libPaths()[1])
library(rgeos, lib.loc = .libPaths()[1])
library(scales, lib.loc = .libPaths()[1])
library(english, lib.loc = .libPaths()[1])
library(flextable, lib.loc = .libPaths()[1])
library(DBI, lib.loc = .libPaths()[1])
age_labels <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", ">=80")
dir <- getwd()
#check for new LOINC codes that change test method name


files <- list.files("L:/daily_reporting_figures_rdp/ctedss_data_here_2", pattern = ".csv")
df <-   files %>%
  map(~read_csv(file.path("L:/daily_reporting_figures_rdp/ctedss_data_here_2", .), col_types = cols(`Chills:` = col_character(),
                                                                     `Event Age Years` = col_character(),
                                                                     `Cough:` = col_character(),
                                                                     `Date Hospitalized` = col_character(),
                                                                     `Date Discharged` = col_character(),
                                                                     `Date of Death` = col_character(),
                                                                     `Difficulty breathing/Shortness of breath:` = col_character(),
                                                                     `Fatigue:` = col_character(),
                                                                     `Fever:` = col_character(),
                                                                     `Headache:` = col_character(),
                                                                     `Hospital Name` = col_character(),
                                                                     `Muscle aches/pains (myalgia):` = col_character(),
                                                                     `New olfactory or taste disorders` = col_character(),
                                                                     `Rigors / Shivers:` = col_character(),
                                                                     `Specimen Date` = col_character(),
                                                                     `Died With COVID-19` = col_character(),
                                                                     `Lab Facility Name` = col_character(),
                                                                     `Result` = col_character(),
                                                                     `Test` = col_character(),
                                                                     `Type of Congregate Setting` = col_character(),
                                                                     `Name of facility` = col_character(),
                                                                     `Did Patient Reside or Spend Time in a Congregate Setting` = col_character(),
                                                                     `Pregnant` = col_character(),
                                                                     `What Was Their Exposure to the Congregate Setting` = col_character()))) %>%
  reduce(bind_rows) %>% 
  select_if(!str_detect(names(.), "X[0-9]"))





tests<-count(df, `Test`)
sources<-count(df, str_to_lower(df$`Specimen Source`)) %>%  arrange(desc(n))
results<- count(df, Test, Result)%>%  arrange(desc(n))
#check that this list includes all PCR-based tests based on output from count above
pcrtests <- c(NA, 'SARS CoV 2 ORF1 resp', 
              'SARS CoV 2 RNA nasopharynx', 
              'SARS CoV 2 Rapid RdRp gene (AbbottIDNOW)',
              'COVID-19 RT-PCR INPATIENT/ED ADMIT',
              'COVID-19 RT-PCR OUTPATIENT (VACT)',
              'SARS CoV 2 RNA Saliva PCR',
              'SARS-CoV-2 PCR XXX', 
              'SARS CoV 2 PCR resp', 
              'Other PCR',
              'SARS-CoV-2 RdRp gene resp',
              'SARS-CoV-2 RT-PCR-At Home Kits',
              'BHD COVID-19 RT-PCR NP',
              'RT-PCR',
              'SARS CoV 2 N resp',
              'SARS CoV 2 RNA (BioFire)',
              'SARS CoV 2 Rapid PCR Test RdRp gene result',
              'SARS-CoV-2 (COVID19) RNA [Presence] in Nose by NAA with probe detection',
              'SARS-CoV-2 (COVID-19) N gene [Cycle Threshold #] in Unspecified specimen by Nucleic acid amplification using CDC primer-probe set N1',
              'SARS CoV-2',
              'SARS-CoV-2 N gene Saliva',
              'COVID-19 PCR (CEPHEID)'
)
# pcrtestlist <- paste0("SARS CoV 2 ORF1 resp|SARS CoV 2 RNA nasopharynx|SARS-CoV-2 PCR XXX|SARS CoV 2 RNA \\(BioFire\\)|SARS CoV 2 N resp|SARS CoV 2 PCR resp|SARS CoV 2 Rapid PCR Test RdRp gene result|SARS-CoV-2 \\(COVID19\\) RNA \\[Presence\\] in Nose by NAA with probe detection|SARS-CoV-2 \\(COVID-19\\) N gene \\[Cycle Threshold #\\] in Unspecified specimen by Nucleic acid amplification using CDC primer-probe set N1|COVID-19 RT-PCR INPATIENT\\/ED ADMIT|COVID-19 RT-PCR OUTPATIENT \\(VACT\\)")
#df prep

agtests <- c(
  #                  NA,
  'SARS CoV 2 Ag rapid IA',
  'SARS-COV + SARS-CoV-2 Antigen resp',
  'SARS CoV 2 Ag (Quidel Sofia/Lumira)',
  'SARS CoV 2 Ag rapid IA (BD Veritor)',
  'SARS CoV 2 Ag rapid IA (BD Veritor/BinaxNOW)'
)


df <- df %>%
  #filter to cases that are valid
  filter(`Case Status` !="Invalid") %>%  #invalids kicked out here
  #rename key variables
  rename(
    phone = Phone,
    fname = `First Name`,
    lname = `Last Name`,
    event_date = `Event Date`,
    dob = `Birth Date`,
    status = `Case Status`,
    eventid = `Case ID`,
    county = County,
    create_date = `Case Create Date`,
    age = `Event Age Years`,
    gender = Gender,
    city = `Current Official City`,
    state = State,
    race = Race,
    hisp = Hispanic,
    preg = Pregnant,
    satus = `Case Status`,
    disease_status = `Disease classification status`,
    healthcare_worker = `Healthcare Worker`,
    hospitalized = Hospitalized,
    hospital_name = `Hospital Name`,
    discharge_date = `Date Discharged`,
    admit_date = `Date Hospitalized`,
    icu = `Admitted to ICU`,
    symptoms = `Were there any symptoms associated with this illness/event?`,
    symp_onset_date = `Symptom onset date:`,
    fever = `Fever:`,
    fatigue = `Fatigue:`,
    ards = `Did the Patient Have ARDS`,
    pneumonia = `Pneumonia (Clinical or radiological)`,
    myalgia = `Muscle aches/pains (myalgia):`,
    sob = `Difficulty breathing/Shortness of breath:`,
    chills = `Chills:`,
    headache = `Headache:`,
    cough = `Cough:`,
    rigors = `Rigors / Shivers:`,
    new_olf_taste = `New olfactory or taste disorders`,
    sore_throat = `Sore throat:`,
    outcome = Outcome,
    covid_death = `Died With COVID-19`,
    death_date = `Date of Death`,
    cong_setting = `Type of Congregate Setting`,
    spec_col_date = `Specimen Date`,
    test = Test,
    result = Result,
    cong_exposure_type = `What Was Their Exposure to the Congregate Setting`,
    cong_facility = `Name of facility`,
    lab_name = `Lab Facility Name`,
    source = `Specimen Source`,
    vrn = `Vital Records Death number`,#AS 12/17/2020
    ocme_reported = `Death reported by OCME`, # AS 12/17/2020
    ocmeid =`OCME ID`, # AS 12/17/2020
    daycare_attendee = `Has the patient attended a daycare facility`,
    daycare_staff = Daycare
  )%>% 
  # filter(!test %in% agtests) %>% # AG TESTS KICKED OUT HERE
  mutate(
    fname =str_to_lower(fname),
    lname =str_to_lower(lname),
    dob = as.Date(mdy(dob)),
    age = floor(time_length(
      difftime(as.Date(mdy(event_date)), dob)
      ,unit = "years"  
    )),
    #CONG EXPOSURE EDIT 10/8
    cong_exposure_type = if_else(!is.na(cong_exposure_type), cong_exposure_type, 
                                 if_else(`Did Patient Reside or Spend Time in a Congregate Setting`=="Yes", "Reside", cong_exposure_type)),
    bigID = paste0(fname, lname, dob),
    result = str_to_lower(result),
    hospitalized = ifelse(
      str_detect(hospitalized, "Yes"),
      "Yes",
      ifelse(
        str_detect(hospitalized, "No"),
        "No",
        "Unknown"
      )
    )#,
    # source = str_to_lower(source),
    # source = ifelse(
    #   source %in% c("nasopharyngeal", "nasopharynx", "nasopharyngeal swab", "np", "nasaopharyngeal", "naspharyngeal s", "nasopharngeal"),
    #   "np swab",
    #   ifelse(
    #     source %in% c("nasal", "nasal aspirate/swab", "nasal swabs", "anterior nasal", "nose", "anterior nares", "nose (nasal passage)", "nares swab"),
    #     "nasal",
    #     ifelse(
    #       source %in% c("serum", "blood", "cord blood", "blood, whole"),
    #       "blood",
    #       ifelse(
    #         source %in% c("saliva", "saliva swab", "oral swab", "oral  swab", "oral fluid", "oral"),
    #         "oral",
    #         ifelse(
    #           source == "nasopharyngel and oropharyngeal swab",
    #           "np and op swab",
    #           ifelse(
    #             source %in% c("oropharyngeal/throat swab", "op swab", "opt"),
    #             "op swab",
    #             ifelse(
    #               source %in% c("bronchial wash", "bronchoalveolar lavage (bal)", "tracheal aspirate", "respiratory", "sputum", "respiratory sample", "lung"),
    #               "resp",
    #               ifelse(
    #                 is.na(source) | source=="other",
    #                 "unspecified specimen",
    #                 source
    #               )
    #             )
    #           )
    #         )
    #       )
    #     )
    #   )
    # )
  )
df$hospitalized[is.na(df$hospitalized)] <- "Unknown"


#####################
#####################

# df_race <- df %>% 
#   select(bigID, race) %>% 
#   unique() %>% 
#   group_by(bigID) %>% 
#   tally() %>% 
#   filter(n >1) %>% 
#   select(-n) %>% 
#   left_join(df %>% select(bigID, race), by = c("bigID" = "bigID")) %>% 
#   unique() %>%
#   mutate(race = ifelse(race == "Unknown", NA, race)) %>% 
#   filter(!is.na(race)) %>%
#   group_by(bigID) %>% 
#   add_count() %>% 
#   filter(n == 1)
# df$race[match(df_race$bigID, df$bigID)] <- df_race$race
# 
# df_hisp <- df %>% 
#   select(bigID, hisp) %>% 
#   unique() %>% 
#   group_by(bigID) %>% 
#   tally() %>% 
#   filter(n >1) %>% 
#   select(-n) %>% 
#   left_join(df %>% select(bigID, hisp), by = c("bigID" = "bigID")) %>% 
#   unique()%>%
#   mutate(hisp = ifelse(hisp == "Unknown", NA, hisp)) %>% 
#   filter(!is.na(hisp)) %>%
#   group_by(bigID) %>% 
#   add_count() %>% 
#   filter(n == 1)
# df$hisp[match(df_hisp$bigID, df$bigID)] <- df_hisp$hisp
# 
# df <- df %>% 
#   select(-county) %>% #drops original county because there are quite a few wrong
#   mutate(city = str_to_title(city)) %>% 
#   left_join(cc_map %>% rename(county=COUNTY), by = c("city" = "CITY")) %>%  
#   mutate(
#     race = ifelse(
#       race =="Black or African American",
#       "Black",
#       ifelse(
#         race == "Asian"| race == "Native Hawaiian or Other Pacific Islander",
#         "Asian or Pacific Islander",
#         race
#       )
#     )
#   ) %>% 
#   mutate(
#     hisp = if_else(
#       hisp == "Yes",
#       "H",
#       "NH",
#       "NH"  #na eth set to NH otherwise it breaks so many things
#     )
#   ) %>% 
#   mutate(
#     race = ifelse(
#       is.na(race),
#       "Unknown",
#       ifelse(
#         !race %in% c("White", "Black", "Asian", "American Indian or Alaskan Native", "Asian or Pacific Islander", "Other", "Unknown"),
#         "Multiracial",
#         race
#       )
#     ),
#     age=as.numeric(age),
#     age_group = cut(
#       age, breaks = c(-1,9,19,29,39,49,59,69,79, Inf),labels = age_labels
#     )
#   ) %>% 
#   mutate(
#     hisp_race = paste0(hisp," ", race)
#   )
# df$hisp_race[df$hisp == "H"] <- "Hispanic"
# df$hisp_race[df$hisp_race == "NH Unknown"] <-  "Unknown"
# 
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
# 
# 
# df <- df %>% 
#   select(-c(admit_date, discharge_date)) %>%
#   mutate_at(.vars = vars(contains("_date")), ~as.Date(mdy(.))) %>% 
#   left_join(df_dischargedates, by = c("eventid" = "eventid")) %>% 
#   left_join(df_admitdates, by = c("eventid" = "eventid")) %>% 
#   filter(result != "Test not done"| is.na(result)) 

####multioutcome
multioutcome <- df %>% 
  select(bigID, outcome) %>% 
  distinct(bigID, outcome) %>% 
  group_by(bigID) %>% 
  tally() %>% 
  filter(n >=2) %>%
  ungroup() %>% 
  left_join(df, by =c("bigID" = "bigID")) %>% 
  select(eventid, bigID, outcome, death_date) %>% 
  unique()%>% 
  filter(outcome == "Died" & !is.na(death_date))
df <- df %>% 
  mutate(outcome = ifelse(bigID %in% multioutcome$bigID, "Died", outcome)) 
df$outcome[df$covid_death == "No"] <- NA
#df$death_date[df$covid_death == "No"] <- NA

test_people <- df %>% 
  filter(
    str_detect(str_to_lower(fname), "import|zztest|zzz|covid|validation|schedule|[0-9]|identif|\\btest\\b|sqtwo|sqthree|mytest")|str_detect(str_to_lower(lname), "zztest|zzz|covid|validation|schedule|[0-9]|identif|\\btest\\b|ascaris")
  ) %>% 
  select(eventid) %>% 
  unique()
#write_csv(test_people, paste0("csv/", Sys.Date(), "/", Sys.Date(), "test_people.csv"))

# df <- df %>% 
#   filter(
#     !(str_detect(str_to_lower(fname), "import|zztest|zzz|covid|validation|schedule|[0-9]|identif|\\btest\\b|sqtwo|sqthree|mytest")|str_detect(str_to_lower(lname), "zztest|zzz|covid|validation|schedule|[0-9]|identif|\\btest\\b|ascaris"))
#   )
df <- df %>% 
  filter(!eventid %in% test_people$eventid)

#####test results cleaning
df$result <- ifelse(
  str_detect(df$result, "not ?(un)?detected|negative"),
  "not detected",
  ifelse(
    str_detect(df$result,"(?<!not|un|not) ?detected|posi?ti?ve"),
    "detected",
    "indeterminate"
  )
)



###bad spec_col_dates blanked out
df$spec_col_date[df$spec_col_date < ymd("2020-02-20")] <- NA 


#setting confirmed with no +pcr or blank pcr results to suspect at the top here and then the rest of the checks will pop them in their proper category should they be picked up again

#leave folks who are covid_death = Yes and their disease status %in% Confirmed, Probable alone, except probables should be able to be upped to conf if applicable
untouchable_conf <- df %>%  
  filter(outcome == "Died" & disease_status == "Confirmed") %>% 
  select(eventid) %>% 
  unique()
# count(untouchables, disease_status)
# count(untouchables, covid_death)

#currently not much going on with this object here, but maybe down the line
mostly_untouchable_prob <- df %>%  
  filter(outcome == "Died" & disease_status == "Probable") %>% 
  select(eventid) %>% 
  unique()

conf_good_result <- df %>% 
  filter(
    test %in% pcrtests &
      disease_status == "Confirmed" &
      result %in% c('detected')
  ) %>%  
  select(eventid) %>%  
  unique()

conf_bad_result <- df %>% 
  filter(
    test %in% pcrtests &
      disease_status == "Confirmed" &
      result %in% c(NA, 'not detected', 'indeterminate')
  ) %>%  
  select(eventid) %>%  
  unique()

conf_bad_result <- df %>% 
  filter(eventid %in% conf_bad_result$eventid & !eventid %in% conf_good_result$eventid & !eventid %in% untouchable_conf$eventid & !eventid %in% mostly_untouchable_prob$eventid) %>% #the last filter portion with the probs doesn't change anything for now becuase this has to do with confirmeds, but i thoought it wouldnt hurt
  select(eventid, disease_status, test, result, spec_col_date, symp_onset_date) %>% 
  arrange(eventid)

badresultautofixed <- conf_bad_result 

df$disease_status[df$eventid %in% conf_bad_result$eventid] <- "Suspect" #conf to suspect

pcr_not_confirmed <- df %>%
  filter(state == "CT"| is.na(state)) %>%
  filter(test %in% pcrtests & !disease_status %in% c("Confirmed", "Not a case") & result == "detected") %>%  
  select(eventid) %>% 
  unique()

pcr_confirmed <- df %>%
  filter(state == "CT"| is.na(state)) %>%
  filter(test %in% pcrtests & result == "detected") %>%  
  select(eventid) %>% 
  unique()

ag_probable <- df %>%
  filter(!eventid %in% pcr_confirmed$eventid) %>% 
  filter(state == "CT"| is.na(state)) %>%
  filter(test %in% agtests & !disease_status %in% c("Probable") & result == "detected") %>%  
  select(eventid) %>% 
  unique()

df_suspect <- df %>% 
  filter(disease_status == "Suspect") %>% 
  filter(state == "CT"| is.na(state)) 

two_or_more_symps <- df_suspect%>% 
  select(eventid, disease_status, fever, chills, rigors, myalgia, headache, sore_throat, new_olf_taste) %>% 
  pivot_longer(-c(eventid, disease_status), names_to = "symptom", values_to = "symp_pres") %>% 
  filter(symp_pres == "Yes") %>% 
  group_by(eventid, disease_status) %>% 
  tally() %>% 
  filter(!disease_status %in% c("Confirmed", "Probable")  & n >= 2) %>% 
  select(eventid) %>% 
  unique()

one_of_symps <- df_suspect %>%   
  select(eventid, disease_status, cough, sob, ards, pneumonia) %>% 
  pivot_longer(-c(eventid, disease_status), names_to = "symptom", values_to = "symp_pres") %>% 
  filter(symp_pres == "Yes") %>% 
  group_by(eventid, disease_status) %>% 
  tally() %>% 
  filter(!disease_status %in% c("Confirmed", "Probable")) %>% 
  select(eventid) %>% 
  unique()

suspect_probable <- df_suspect %>% 
  filter((eventid %in% one_of_symps$eventid|eventid %in% two_or_more_symps$eventid) & satus != "Closed") %>% 
  unique() 

df <- df %>%
  mutate(disease_status = ifelse(eventid %in% pcr_not_confirmed$eventid,  "Confirmed", disease_status)) %>% #PCR postives not already confirmed  changed to Confirmed
  mutate(disease_status = ifelse(eventid %in% ag_probable$eventid,  "Probable", disease_status)) %>%  #AG postives not already probable changed to Probable
  mutate(disease_status = ifelse(eventid %in% suspect_probable$eventid, "Probable", disease_status))

#case create
case <- df 
#case$result[!case$test %in% pcrtests] <- NA
#case$disease_status[!case$test %in% pcrtests & case$disease_status != "Probable"] <- "Suspect"
#case$disease_status[case$test %in% pcrtests & (case$result != 'detected' | is.na(case$result)) & case$disease_status != "Probable" ] <- "Suspect"

case <- case %>% 
  filter(disease_status == "Confirmed"  | disease_status == "Probable") %>% 
  filter(state =="CT"|is.na(state)) %>% 
  mutate(
    date = ifelse(disease_status %in% c("Confirmed", "Probable") & !is.na(spec_col_date), spec_col_date, event_date),
    #date = ymd(date), 
    date = as.Date(date, origin = ymd("1970-01-01")),
    week = epiweek(date), 
    year = epiyear(date),
    spec_date = ifelse(
      !is.na(spec_col_date) & disease_status %in% c("Confirmed", "Probable"),
      spec_col_date,
      event_date
    ),
    mmwrweek = if_else(
      is.na(spec_col_date), 
      epiweek(event_date), 
      epiweek(spec_col_date)),
    simple_result = ifelse( 
      result == "detected",
      1,2
    ),
    pcrtest=ifelse(test %in% pcrtests, 1, 2),
    spec_date = as.Date(spec_date, origin = "1970-01-01")
  ) %>%
  mutate(cong_yn = if_else((str_detect(cong_exposure_type, "Reside")) &
                             (str_detect(cong_setting, "Jail / Prison") | str_detect(cong_setting, "Long term care facility") | str_detect(cong_setting, "Assisted Living Facility")),
                           "Yes", "No", missing = "No")
  ) %>% 
  group_by(bigID) %>%
  arrange(simple_result,pcrtest,spec_date) %>% # detected, first priority, and then the pcr test or ag if no pcr, then earliest spec_date, spec_date is spec_col_Date for tests with that or eventdate if none listed or is a non ag+ prob
  slice(1L) %>%
  ungroup() 


### confirmed but only negative pcr result or blank pcr result  #### code will automatically change this, but ctedss should be checked because this means that perhaps false positives were updated and the status will be wrong in ctedss or some other crazyness
conf_good_result <- df %>% 
  filter(
    test %in% pcrtests &
      disease_status == "Confirmed" &
      result %in% c('detected')
        ) %>%  
      select(eventid) %>%  
      unique()

conf_bad_result <- df %>% 
  filter(
    test %in% pcrtests &
      disease_status == "Confirmed" &
      result %in% c(NA, 'not detected')
      ) %>%  
  select(eventid) %>%  
  unique()

##### confirmed with pcr but no resutl or not detected  nice to know#########
conf_bad_result <- df %>% 
  filter(eventid %in% conf_bad_result$eventid & !eventid %in% conf_good_result$eventid & outcome != "Died") %>% 
  select(eventid, disease_status, test, result, spec_col_date, symp_onset_date) %>% 
  arrange(eventid)

 

#write_csv(badresultautofixed, "badresult.csv")

###### more than one non-na race ####### nice to know
too_many_race_raw <- df %>% 
  filter(bigID %in% df_race$bigID) %>% 
  select(bigID, eventid, hisp, race) %>% 
  mutate(in_case = ifelse(eventid %in% case$eventid, 1,0)) %>% 
  unique()
num_people_race_case <- too_many_race_raw %>% 
  filter(in_case ==1) %>% 
  select(bigID) %>% 
  unique() %>% 
  nrow()


########more than one non-na hisp ############ nice to know 
too_many_hisp_raw <- df %>% 
  filter(bigID %in% df_hisp$bigID)%>% 
  select(bigID, eventid, hisp, race) %>% 
  mutate(in_case = ifelse(eventid %in% case$eventid, 1,0)) %>% 
  unique()
num_people_hisp_case <- too_many_hisp_raw %>% 
  filter(in_case ==1) %>% 
  select(bigID) %>% 
  unique() %>% 
  nrow()



ag_confirmed <- df %>%
  filter(state == "CT"| is.na(state)) %>% 
  filter(test %in% agtests & disease_status == "Confirmed" & result == 'detected') %>%
  select(eventid) %>% 
  unique()


pcr_confirmed <- df %>%
  filter(state == "CT"| is.na(state)) %>% 
  filter(test %in% pcrtests & disease_status == "Confirmed" ) %>% 
  unique()

confirmed_but_ag_only <- df %>% 
  select(eventid, disease_status, test, result) %>% 
  filter(eventid %in% ag_confirmed$eventid & !eventid %in% pcr_confirmed$eventid) %>% 
  select(eventid) %>% 
  left_join(df) %>% 
  unique() 

#write_csv(confirmed_but_ag_only, "confirmed_but_ag_only.csv")

rm(ag_confirmed)
#rm(pcr_confirmed)

 df$disease_status[df$eventid %in% confirmed_but_ag_only$eventid] <-  "Probable"

 has_positive <- df %>% 
   filter(state == "CT"| is.na(state)) %>%
   filter(disease_status == "Confirmed" & result == "detected") 
 
 confirmed_onlyneg <- df %>% 
   filter(state == "CT"| is.na(state)) %>% 
   filter(disease_status == "Confirmed" & !eventid %in% pcr_confirmed$eventid & !eventid %in% has_positive$eventid)

####1 pcr but not confirmed####  #need changes in ctedss if found, code takes care of the raw data for report counts
pcr_notconfirmed <- df %>%
  filter(state == "CT"| is.na(state)) %>% 
  filter(test %in% pcrtests & disease_status != "Confirmed" & result == "detected") %>% 
  unique()


write_csv(pcr_notconfirmed, "pcr_notconfirmed.csv")

####2 death in persons <20yo ##### important if >2, need to know
under19deaths <- case %>% 
  filter(outcome =="Died" & age < 20 & age >=0)  %>% 
  unique()

testcount <- count(df, test) %>% arrange(desc(n))

write_csv(testcount, "testcount.csv")

####3 dod <2/1/2020#######cross check with OCME for actual date.
tooearlydeaths <- case %>% 
  filter(death_date < "2020-02-01")

####4 age >105 ####### if >13, then let Sydney know
tooold <- case %>% 
  filter(age > 105)


# ####5 discharge date < admit date ####
# tooearlyadmit <- case %>% 
#   filter(discharge_date < admit_date)
# 
# ####6 admit date <12/1/2019 ####
# waytooearlyadmit <- case %>% 
#   filter(admit_date < "2019-12-01")


####8 symptoms but still suspect
############ work in progress ############
sympsdf <- df %>%
  select(bigID,eventid,disease_status,fever, fatigue, sob, headache, cough, chills, myalgia, 
         new_olf_taste,rigors, sore_throat, pneumonia, ards, test, result
         ) %>% 
  filter(
    !str_detect(str_to_lower(result), "not detected|negative")
  )%>%
  mutate(fever = if_else(fever %in% c("Yes", "Not taken"), "Yes", "No"))
  
  
two_or_more_symps <- sympsdf %>% 
  select(eventid, disease_status, fever, chills, rigors, myalgia, headache, sore_throat, new_olf_taste) %>% 
  pivot_longer(-c(eventid, disease_status), names_to = "symptom", values_to = "symp_pres") %>% 
  filter(symp_pres == "Yes") %>% 
  group_by(eventid, disease_status) %>% 
  tally() %>% 
  filter(!disease_status %in% c("Confirmed", "Probable")  & n >= 2) %>% 
  select(eventid) %>% 
  unique()

one_of_symps <- sympsdf %>%   
  select(eventid, disease_status, cough, sob, ards, pneumonia) %>% 
  pivot_longer(-c(eventid, disease_status), names_to = "symptom", values_to = "symp_pres") %>% 
  filter(symp_pres == "Yes") %>% 
  group_by(eventid, disease_status) %>% 
  tally() %>% 
  filter(!disease_status %in% c("Confirmed", "Probable")) %>% 
  select(eventid) %>% 
  unique()
  ##### keep an eye on, ~30, mostly sero + 6/3/2020
sympts_but_suspect_by_eventid <- sympsdf %>% 
  filter(eventid %in% one_of_symps$eventid|eventid %in% two_or_more_symps$eventid) %>% 
  unique() %>% 
  filter(!bigID %in% case$bigID )


sympts_but_suspect_by_person <- df %>%  
  filter(bigID %in% sympts_but_suspect_by_eventid$bigID) %>%
  unique()
rm(one_of_symps)
rm(two_or_more_symps)



#### what you want to look at it is 'confirmed_not_pcr_by_eventid'

### double check with sydney and then change in CTEDSS and change .csv
##7##confirmed but not pcr PEOPLE
pcr_confirmed <- df %>%
  filter(state == "CT"| is.na(state)) %>% 
  filter(test %in% pcrtests & disease_status == "Confirmed" & result == 'detected') %>%
  select(bigID) %>% 
  unique()

confirmed_not_pcr_by_people <- df %>% 
  select(bigID, disease_status, test, result) %>% 
  filter(result == 'detected') %>% 
  select(-result) %>% 
  filter(!test %in% pcrtests) %>% 
  select(-test) %>% 
  unique() %>% 
  filter(disease_status == "Confirmed") %>% 
  select(bigID) %>% 
  left_join(df) %>% 
  unique() %>% 
  filter(!bigID %in% pcr_confirmed$bigID)


 #####confirmed but not pcr EVENTIDS
pcr_confirmed2 <- df %>%
  filter(state == "CT"| is.na(state)) %>% 
  filter(test %in% pcrtests & disease_status == "Confirmed" & result == 'detected') %>%
  select(eventid) %>% 
  unique()

confirmed_not_pcr_by_eventid <- df %>% 
  select(eventid, disease_status, test, result) %>% 
  filter(result == 'detected') %>% 
  select(-result) %>% 
  filter(!test %in% pcrtests) %>% 
  select(-test) %>% 
  unique() %>% 
  filter(disease_status == "Confirmed") %>% 
  select(eventid) %>% 
  left_join(df) %>% 
  unique() %>% 
  filter(!eventid %in% pcr_confirmed2$eventid)

rm(pcr_confirmed)
rm(pcr_confirmed2)

write_csv(confirmed_not_pcr_by_eventid, "confirmed_not_pcr_by_eventid.csv")

write_csv(pcr_notconfirmed, "pcr_notconfirmed.csv")

#######9 preg >45 #let sydney know if any
preg45 <- case %>% 
  filter(preg =="Yes" & (age >45 | age < 12))



########10 test cases that are hard to code out
#code fixed :) no need to worry about this!

#############11 probables but why? probably have to change
probables_but_why <- case %>% 
  filter(str_detect(test, "SARS CoV 2 ORF1 resp|SARS CoV 2 RNA nasopharynx|SARS CoV 2 PCR resp|SARS-CoV-2 PCR XXX|SARS CoV 2 Rapid PCR Test RdRp gene result") & disease_status == "Probable" & str_detect(str_to_lower(result), "not detected|negative") & (is.na(outcome)| outcome != "Died" ))

############12. dead with no death date #check with sydney to cross reference OCME data
deadnodate <- case %>% 
  filter(outcome == "Died" & is.na(death_date))

#if you need ids to copy paste
deadnodate$eventid

############13. dead with no town
deadnotown <- case %>%
  filter(outcome=="Died" & city %in% c("NOT_AVAILABLE", "Not available", "Not Available"))
#if you need ids to copy paste
deadnotown$eventid

###############14. county but messy city
county_nocity <-  case %>%
  filter(!is.na(county) & county != "Unknown" & city %in% c("NOT_AVAILABLE", "Not available", "Not Available"))

epiweek(Sys.Date())
county_nocity %>% 
  group_by(mmwrweek) %>% 
  tally() %>% 
  arrange(desc(mmwrweek))
summary(county_nocity$mmwrweek)

#write_csv(county_nocity,  "county_nocity.csv")

###multiple outcomes per person/ eventid
multioutcome1 <- df %>% 
  select(bigID, outcome) %>% 
  #filter(!is.na(outcome)) %>% 
  distinct(bigID, outcome) %>% 
  group_by(bigID) %>% 
  tally() %>% 
  filter(n >=2) %>%
  ungroup() %>% 
  left_join(df, by =c("bigID" = "bigID")) %>% 
  select(eventid, bigID, outcome) %>% 
  unique() %>% 
  filter(outcome == "Died")


#####look at this one ####
multioutcome <- df %>% 
  filter(bigID %in% multioutcome1$bigID) %>% 
  select(eventid,bigID, disease_status, outcome, death_date, city) %>% 
  arrange(bigID, outcome)
# write_csv(multioutcome, "dupes_dead_difoutcome.csv")
unique(multioutcome$eventid)



#####15.  DOC/date of symptom onset prior to DOB####
toosoon <- df %>%
  mutate(dob = as.Date(dob, "%m/%d/%Y")) %>%
  filter((ymd(spec_col_date) < dob) | (symp_onset_date < dob))


#write_csv(toosoon,  "toosoon.csv")

#####16. DOC/date of symptom onset after DOD######
toolate <- df %>%
  mutate(death_date = as.Date(death_date, "%m/%d/%Y")) %>%
  filter((ymd(spec_col_date) > death_date)| (symp_onset_date > death_date))

compare<-confirmed_but_ag_only %>% 
  filter (!eventid %in% confirmed_not_pcr_by_eventid$eventid)


######17. Specimen collection date doesn't make sense (specimen collection prior to Feb. 20 or after today's date - these need to be fixed in ctedss, can refer to Alycia/Nancy as needed) ######
badspecdate <- df %>% 
  filter(spec_col_date < mdy("02-20-2020") | spec_col_date > Sys.Date())

######18. THURSDAY ONLY - double check for congregate setting among older cases #####
thisweek <-epiweek(Sys.Date())
last2 <-c(thisweek-1, thisweek-2)
yesterdaycase<- read_csv(paste0("csv/",Sys.Date()-1, "/", Sys.Date()-1, "cases_wphi.csv"))
new70plus <- read_csv(paste0("csv/",Sys.Date(), "/", Sys.Date(), "cases_wphi.csv"))%>% 
  filter(mmwrweek %in% last2 & age >=70) %>% 
  mutate(cong_yn = if_else((str_detect(cong_exposure_type, "Reside")) &
                             (str_detect(cong_setting, "Jail / Prison") | str_detect(cong_setting, "Long term care facility") | str_detect(cong_setting, "Assisted Living Facility")),
                           "Yes", "No", missing = "No")) %>% 
  filter(cong_yn=="No") %>% 
  filter(!eventid %in% yesterdaycase$eventid)


#####18 covid_death == yes but suspect



# ###############weird hospitalization combos
# count(case, case$hospitalized)
# hosp_combos <- case %>% 
#   filter(str_detect(hospitalized, ",")) %>% 
#   select(eventid,hospitalized) %>% 
#   unique()
# write_csv(hosp_combos, "hosp_combos.csv")

#Strange lab dates
# weird <- elr_linelist %>%
#   filter(spec_col_date < mdy("02-28-2020"))
