
# library(data.table)
# library(dtplyr)
library(tidyverse)
library(lubridate)
library(DBI)
library(odbc)

con2 <- DBI::dbConnect(odbc::odbc(), "epicenter")

tock <- Sys.time()
# dbListFields(con2, SQL("DPH_COVID_IMPORT.dbo.CTEDSS_DAILY_REPORT_ALL_Cases"))
statement <-
  paste0("SELECT EVENT_ID as eventid, fname, lname, dob, phone,
          disease_status, age, gender, street, city, state,
          race, hisp, hospitalized, admit_date, discharge_date, icu,
          preg, symptoms, symp_onset_date, fever, fatigue, sob,
          chills, sorethroat, headache, cough, myalgia, new_olfact_taste,
          rigors, pneumonia, ards, outcome, death_date, diedwithcovid as covid_death,
          ocme_cov_rpt as ocme_reported, ocme_num as ocmeid, death_sfn as vrn,
          healthcare_worker, cong_setting,
          cong_exposure_type, cong_facility, cong_yn as ptreside, daycare_yn as daycare_attendee,
          daycare_occu as daycare_staff, case_create_date, case_mod_date, case_effective_date,
          case_eff_From_date as case_eff_from_date, event_date, facilityName,
          zip_code, race_concat
         FROM [DPH_COVID_IMPORT].[dbo].[CTEDSS_DAILY_REPORT_ALL_Cases]")


raw_cases <- DBI::dbGetQuery(conn = con2 , statement = statement)

statement <-
  paste0(
    "SELECT EVENT_ID as eventid,
      INVESTIGATION_CREATE_DATE as investigation_create_date,
      INVESTIGATION_MOD_DATE as investigation_mod_date,
      NEW_ELR_RESULT as new_elr_result,
      MRN_ELR as mrn_elr, TEST_NAME as test, RESULT as result,
      TESTED_DATE as tested_date, SPEC_COLL_DATE as spec_col_date,
      SPEC_REC_DATE as spec_rec_date, SPEC_NUM as spec_num,
      SPEC_SOURCE as source, AUTH_FACILITY as auth_facility,
      ORDERING_PROVIDER_NAME as ordering_provider_name,
      LAB_NAME as lab_name, FACILITY_NAME as facility_name,
      ORDERING_PROVIDER_FNAME as ordering_provider_fname,
      ORDERING_PROVIDER_LNAME as ordering_provider_lname,
      CASE_MODIFICATION_DATE as case_modification_date,
      result_rpt_date, Device_ID as device_id,
      SPEC_SOURCE_SNOMED as spec_source_snomed,
      RESULT_FREE_TEXT as result_free_text,
      ORDER_LAB_FACILITY as order_lab_facility,
      TESTING_LAB_CLIA as testing_lab_clia, NOTES2 as notes2
    FROM [DPH_COVID_IMPORT].[dbo].[DAILY_Reports_ALL_COVID_TESTS]"
  )


raw_tests <-  DBI::dbGetQuery(conn = con2 , statement = statement)

odbc::dbDisconnect(con2)

df <-  left_join(raw_cases, raw_tests, by = c("eventid"))

# rm(backend)
# rm(backend_tests)

df <-
  df %>%
  mutate(
    across(.cols = case_create_date:case_eff_from_date,
           ~ as_date(.x)),
    across(where(is.character), ~ na_if(.x, "NA")),
    ptreside = str_to_sentence(ptreside),
    gender = str_to_sentence(gender),
    death_date = mdy(death_date),
    event_date = mdy(event_date),
    spec_col_date = mdy(spec_col_date),
    outcome = str_to_sentence(outcome),
    fname = str_to_lower(fname),
    lname = str_to_lower(lname),
    dob = mdy(dob),
    disease_status = str_to_sentence(disease_status),
    age = floor(time_length(
      difftime(event_date, dob)
      ,unit = "years")
    ),
    cong_exposure_type = str_to_sentence(cong_exposure_type),
    cong_exposure_type = ifelse(
      is.na(cong_exposure_type) & ptreside == "Yes",
      "Reside",
      cong_exposure_type
    ),
    bigID = paste0(fname, lname, dob),
    result = str_to_lower(result),
    hospitalized = str_to_sentence(hospitalized),
    hospitalized = case_when(
      str_detect(hospitalized, "Yes") ~ "Yes",
      str_detect(hospitalized, "No")  ~ "No",
      is.na(hospitalized)             ~ "Unknown",
      TRUE                            ~ "Unknown"),
    source = str_to_lower(source),
    source = case_when(
      source %in% c("nasopharyngeal", "nasopharynx", "nasopharyngeal swab",
                    "np", "nasaopharyngeal", "naspharyngeal s", "nasopharngeal") ~ "np swab",
      source %in% c("nasal", "nasal aspirate/swab", "nasal swabs", "anterior nasal",
                    "nose", "anterior nares", "nose (nasal passage)",
                    "nares swab", "Nares swab-anterior nares swab",
                    "nasal swab taken", "nasal smear specimen",
                    "nares swab-anterior nares swab") ~ "nasal",
      source %in% c("serum", "cord blood", "d", "blood, whole", "venous blood") ~ "blood",
      source %in% c("saliva", "saliva swab", "oral swab", "oral  swab",
                    "oral fluid", "oral", "oral saliva sample") ~ "oral",
      source == "nasopharyngel and oropharyngeal swab" ~ "np and op swab",
      source %in% c("oropharyngeal/throat swab", "op swab", "opt") ~ "op swab",
      source %in% c("bronchial wash", "bronchoalveolar lavage (bal)",
                    "tracheal aspirate", "respiratory", "sputum",
                    "respiratory sample", "lung") ~ "resp",
      source == "other" ~ "unspecified specimen",
      is.na(source) ~ "unspecified specimen",
      TRUE ~ source),
    race = str_replace_all(race, "_", " "),
    race = str_to_title(race),
    hisp = str_to_sentence(hisp),
    city = str_to_title(city)
  )

# Need 358 to 399 then the join

tick <- Sys.time()
tick - tock

###########

con2 <- DBI::dbConnect(odbc::odbc(), "epicenter")
statement <- paste0("SELECT *
                    FROM [DPH_COVID_IMPORT].[dbo].[CTEDSS_DAILY_REPORT_ALL_Cases]")

tock <- Sys.time()

backend <- DBI::dbGetQuery(conn = con2 , statement = statement)

statement <- paste0("SELECT *
                    FROM [DPH_COVID_IMPORT].[dbo].[DAILY_Reports_ALL_COVID_TESTS]")
backend_tests<-  DBI::dbGetQuery(conn = con2 , statement = statement)
odbc::dbDisconnect(con2)
rm(statement)

names(backend) <- str_to_lower(names(backend))
names(backend_tests) <- str_to_lower(names(backend_tests))

####cases cleanup####

backend <- backend %>%
  select(-c("case_unid", "participant_unid", "hisp_race", "racecount", "lboname", "case_status", "mmwrweek", "spec_col_date", "result","test" , "lab_name"))

#####tests cleanup######
backend_tests <- backend_tests %>%
  select(-c("rec_id", "participant_id", "party_id", "test_investigation_id", "investigation_result_id", 'test_loinc', "result_snomed", "notes", "case_create_date")) %>%
  rename(spec_col_date = spec_coll_date)

df <- backend %>%
  left_join(backend_tests, by = c("event_id"))

# rm(backend)
# rm(backend_tests)

df <- df %>%
  mutate(across(.cols = case_create_date:case_eff_from_date,
                lubridate::as_date)) %>%
  mutate_if(is.character, list(~na_if(., "NA")))

df <- df %>%
  #rename key variables
  rename(
    test = test_name,
    eventid = event_id,
    vrn = death_sfn,
    ocme_reported = ocme_cov_rpt,
    ocmeid = ocme_num,
    daycare_attendee = daycare_yn,
    daycare_staff = daycare_occu,
    source = spec_source,
    covid_death = diedwithcovid,
    ptreside = cong_yn
  )%>%
  mutate(
    ptreside = str_to_sentence(ptreside),
    gender = str_to_sentence(gender),
    death_date = mdy(death_date),
    event_date = mdy(event_date),
    spec_col_date = mdy(spec_col_date),
    outcome = str_to_sentence(outcome),
    fname =str_to_lower(fname),
    lname =str_to_lower(lname),
    dob = as.Date(mdy(dob)),
    disease_status = str_to_sentence(disease_status),
    age = floor(time_length(
      difftime(event_date, dob)
      ,unit = "years"
    )),
    cong_exposure_type = str_to_sentence(cong_exposure_type),
    cong_exposure_type = ifelse(
      is.na(cong_exposure_type) & ptreside == "Yes",
      "Reside",
      cong_exposure_type
    ),
    bigID = paste0(fname, lname, dob),
    result = str_to_lower(result),
    hospitalized = str_to_sentence(hospitalized),
    hospitalized = ifelse(
      str_detect(hospitalized, "Yes"),
      "Yes",
      ifelse(
        str_detect(hospitalized, "No"),
        "No",
        "Unknown"
      )
    ),
    source = str_to_lower(source),
    source = ifelse(
      source %in% c("nasopharyngeal", "nasopharynx", "nasopharyngeal swab", "np", "nasaopharyngeal", "naspharyngeal s", "nasopharngeal"),
      "np swab",
      ifelse(
        source %in% c("nasal", "nasal aspirate/swab", "nasal swabs", "anterior nasal", "nose", "anterior nares", "nose (nasal passage)", "nares swab", "Nares swab-anterior nares swab", "nasal swab taken", "nasal smear specimen", "nares swab-anterior nares swab"),
        "nasal",
        ifelse(
          source %in% c("serum", "cord blood", "d", "blood, whole", "venous blood"),
          "blood",
          ifelse(
            source %in% c("saliva", "saliva swab", "oral swab", "oral  swab", "oral fluid", "oral", "oral saliva sample"),
            "oral",
            ifelse(
              source == "nasopharyngel and oropharyngeal swab",
              "np and op swab",
              ifelse(
                source %in% c("oropharyngeal/throat swab", "op swab", "opt"),
                "op swab",
                ifelse(
                  source %in% c("bronchial wash", "bronchoalveolar lavage (bal)", "tracheal aspirate", "respiratory", "sputum", "respiratory sample", "lung"),
                  "resp",
                  ifelse(
                    source=="other",
                    "unspecified specimen",
                    source
                  )
                )
              )
            )
          )
        )
      )
    )
  )

tick <- Sys.time()
tick - tock



