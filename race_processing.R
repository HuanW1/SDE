#Processing race_concat variable

#insert before raw_cases is dropped and just change the var names to just amatch original

####0 libraries and connection ####
source("helpers/StartMeUp.R")
con <- DBI::dbConnect(odbc::odbc(), "epicenter")

statement <-paste0("SELECT * FROM DPH_COVID_IMPORT.dbo.LOOKUP_HL7")
hl7_lookup <- DBI::dbGetQuery(conn = con , statement = statement)

####1 Testing: create raw_cases ####

# 
# rm(raw_cases)
# rm(raw_tests)

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
  select(-race_reporting)

race_medium <- race_medium %>% 
  filter(!is.na(race_reporting))

  
  
  
  
  
  
  
  patient_demos %>% 
  filter(!is.na(multiples_races_values)) %>% 
  select(BigKeyString, multiples_races_values) %>% 
  unique() %>% 
  mutate(mrace =stringr::str_split(string = multiples_races_values, pattern = " and ")) %>% 
  unnest(mrace) %>% 
  filter(mrace != "Other Race") %>% 
  mutate(mrace = ifelse(mrace %in% c("Asian", "Native Hawaiian or Other Pacific Islander"), "Asian or Pacific Islander", mrace)) %>%
  distinct(BigKeyString, mrace) %>% 
  group_by(BigKeyString) %>% 
  mutate(nrace = n_distinct(mrace)) %>% 
  ungroup() %>% 
  mutate(race = ifelse(nrace >1, "Multiple Races", mrace)) %>% 
  select(BigKeyString, race) %>% 
  unique()  



  
 #vax 
  
  patient_demos <- patient_demos %>% 
    left_join(patient_multi, by = "BigKeyString") %>% 
    mutate(race = ifelse(!is.na(race.y), race.y, race.x),
           multiples_races_values = ifelse(race != "Multiple Races", NA, multiples_races_values)) %>% 
    select(-c(race.x,race.y))
  
  patient_demos <- patient_demos %>% 
    mutate(race = ifelse(race == "Unknown", NA, race),
           Ethnicity = ifelse(Ethnicity == "Unknown/Not Reported", NA, Ethnicity),
           gender = ifelse(gender == "Unknown", NA, gender)) %>% 
    group_by(BigKeyString) %>% 
    #Consolidating Race and Ethnicity Rows gender has been complete so far
    mutate(n_distrace = n_distinct(race, na.rm = TRUE),
           n_disteth = n_distinct(Ethnicity, na.rm = TRUE),
           n_distsex = n_distinct(gender, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(race = ifelse(race == "Other Race" & n_distrace >1, NA, race)) %>% #NULL out other if more than 1 distinct race, no NAs up to this point, until they are introduced here (in current data anyway, 3/9/21)
    group_by(BigKeyString) %>% 
    mutate(n_distrace = n_distinct(race, na.rm = TRUE),
           race_concat = paste(race, collapse = " AND "),
           multiples_races_values = ifelse(n_distrace >1, race_concat, race),
           race = ifelse(n_distrace >1, "Multiple Races", race)) %>% 
    #if any !NA values in group fill with first non-na value
    arrange(race) %>% 
    tidyr::fill(race) %>% 
    arrange(desc(patient_update), Ethnicity, race, gender) %>% 
    slice(1L) %>% 
    ungroup() %>% 
    select(-c(patient_update, n_distrace, n_disteth, race_concat, n_distsex))
  patient_demos[is.na(patient_demos)] <- "Unknown"
  #add unknowns back into NAs
  
  patients <- patients %>% 
    select( -c(race, gender, Ethnicity, multiples_races_values)) %>% 
    left_join(patient_demos, by = "BigKeyString") %>% 
    #Dedupe - select row with latest update by BigKeyString
    group_by(BigKeyString) %>% 
    arrange(desc(patient_update)) %>% 
    slice(1L) %>% 
    ungroup() %>% 
    #Create Age Groups and Initialize ethnicity variables and flags
    mutate(hisp = 0,
           hisp_race = "",
           range_age = "0",
           range_age = if_else(age <=15, "<=15", range_age),
           range_age = if_else(age >15 & age <25, "16-24", range_age),
           range_age = if_else(age >24 & age <35, "25-34", range_age),
           range_age = if_else(age >34 & age <45, "35-44", range_age),
           range_age = if_else(age >44 & age <55, "45-54", range_age),
           range_age = if_else(age >54 & age <65, "55-64", range_age),
           range_age = if_else(age >64 & age <75, "65-74", range_age),
           range_age = if_else(age >=75, "75+", range_age),
           range_age = factor(range_age, levels = age_groups, labels = age_groups)
    )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  


#orig
,
race = str_replace_all(race, "_", " "),
race = str_to_title(race),
hisp = str_to_sentence(hisp),
city = str_to_title(city),
race = case_when(
  race == "Black African American" ~ "Black",
  race == "Asian" | race == "Native Hawaiian Pacific Islander" ~ "Asian or Pacific Islander",
  race == "American Indian Alaskan Native" ~ "American Indian or Alaskan Native",
  TRUE ~ race
),
hisp = if_else(
  hisp == "Yes",
  "H",
  "NH",
  "NH"  #na eth set to NH otherwise it breaks so many things
),
race = case_when(
  is.na(race) | race =="Refused" ~ "Unknown",
  !race %in% c("White", "Black", "Asian", "American Indian or Alaskan Native",
               "Asian or Pacific Islander", "Other", "Unknown") ~ "Multiracial",
  TRUE ~ race
),
hisp_race = paste0(hisp, " ", race),
hisp_race = ifelse(hisp == "H", "Hispanic", hisp_race),
hisp_race = ifelse(hisp_race == "NH Unknown", "Unknown", hisp_race),
age_group = cut(age,
                breaks = c(-1,9,19,29,39,49,59,69,79, Inf),
                labels = age_labels)