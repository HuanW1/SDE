#Processing race_concat variable

#insert before raw_cases is dropped and just change the var names to just amatch original

####0 libraries and connection ####
source("helpers/StartMeUp.R")
con <- DBI::dbConnect(odbc::odbc(), "epicenter")

statement <-paste0("SELECT * FROM DPH_COVID_IMPORT.dbo.LOOKUP_HL7")
hl7_lookup <- DBI::dbGetQuery(conn = con , statement = statement)

####1 Pull Race values from DF ####
#trying to do a stepped approach to reduce the number rows and values being processed
#easy joins are left as is, and the rest move on through the process

#easy creation and medium setup
race_easy <- tibble(raw_cases) %>% 
  select(eventid, race_concat) %>% 
  filter(!is.na(race_concat)) %>% 
  left_join(hl7_lookup %>% select(-race_original), 
            by = c("race_concat" = "hl7")) %>% 
  mutate(race_reporting = ifelse(race_concat == "U", "Unknown", race_reporting))

race_medium <- race_easy %>% 
  filter(is.na(race_reporting)) %>% 
  select(-race_reporting) %>%  
  unique()

race_easy <- race_easy %>% 
  filter(!is.na(race_reporting)) %>% 
  select(eventid, race_reporting) %>% 
  rename(race = race_reporting) %>% 
  unique()
  
#race_medium processing and race_hard creation

race_medium <- race_medium %>% 
  mutate(mrace =stringr::str_split(string = race_concat, pattern = ";")) %>% 
  unnest(mrace) %>% 
  select(-race_concat) %>% 
  left_join(hl7_lookup %>% select(-race_original), 
            by = c("mrace" = "hl7")) %>% 
  mutate(race_reporting = ifelse(mrace == "U", "Unknown", race_reporting)) 
  
#no race_hard at time of testing, just going to set to unknown for now :/  
race_hard <- race_medium %>% 
  filter(is.na(race_reporting)) %>% 
  mutate(race = "Unknown") %>% 
  select(-c(race_reporting, mrace)) %>% 
  unique()

race_medium <- race_medium %>% 
  filter(!is.na(race_reporting)) %>% 
  select(-mrace) %>% 
  rename(race = race_reporting) %>% 
  unique()

####2 combine race splits and then assess for multiracial ####
#combine all race splits  
total_race <- bind_rows(race_easy, race_medium, race_hard) %>% 
  unique() %>% 
  group_by(eventid) %>% 
  add_tally()

#process possible multi races further  
poss_multi <- total_race %>% 
  filter(n >1 & race != "Unknown") %>% 
  group_by(eventid) %>% 
  add_tally(name = 'multi') %>% 
  mutate(race = ifelse(multi >1, "Multiracial", race)) %>% 
  select(-c(n, multi)) %>% 
  unique()
  
single <- total_race %>% 
  filter(n<2) %>% 
  select(-n)

new_raw_race <- bind_rows(poss_multi, single)
  
####3 Replace original race ####
raw_cases <- raw_cases %>% 
  select(-race) %>% 
  left_join(new_raw_race, by = "eventid")

#clear trash
rm(single, multi, race_easy, race_medium, race_hard, total_race, new_raw_race) 