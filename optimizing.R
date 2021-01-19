library(data.table)
library(dtplyr)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(forcats)
library(lubridate)
library(odbc)
library(DBI)

#### traditional

Sys.time()
con2 <- DBI::dbConnect(odbc::odbc(), "epicenter")
statement1 <- 
  paste0("SELECT EVENT_ID, fname, lname, dob, phone, disease_status, age, 
          gender, street, city, county, state, race, hisp, hospitalized, 
          admit_date, discharge_date, icu, preg, symptoms, symp_onset_date, 
          fever, fatigue, sob, chills, sorethroat, headache, cough, myalgia, 
          new_olfact_taste, rigors, pneumonia, ards, outcome, death_date, 
          diedwithcovid, ocme_cov_rpt, ocme_num, death_sfn, healthcare_worker, 
          cong_setting, cong_exposure_type,  cong_facility, cong_yn, daycare_yn, 
          daycare_occu, case_create_date, case_mod_date, case_effective_date, 
          case_eff_From_date, event_date, facilityName 
          FROM [DPH_COVID_IMPORT].[dbo].[CTEDSS_DAILY_REPORT_ALL_Cases]")
backend <- DBI::dbGetQuery(conn = con2 , statement = statement1)

statement2 <- 
  paste0("SELECT EVENT_ID, INVESTIGATION_CREATE_DATE, INVESTIGATION_MOD_DATE, 
          NEW_ELR_RESULT, MRN_ELR, TEST_NAME, RESULT, TESTED_DATE, SPEC_COLL_DATE, 
          SPEC_REC_DATE, SPEC_NUM, SPEC_SOURCE, AUTH_FACILITY, ORDERING_PROVIDER_NAME, 
          LAB_NAME, FACILITY_NAME, ORDERING_PROVIDER_FNAME, ORDERING_PROVIDER_LNAME, 
          CASE_MODIFICATION_DATE, result_rpt_date 
          FROM [DPH_COVID_IMPORT].[dbo].[DAILY_Reports_ALL_COVID_TESTS]")
backend_tests <- DBI::dbGetQuery(conn = con2 , statement = statement2)

odbc::dbDisconnect(con2)
rm(statement1, statement2)
Sys.time()

backend <- 
  rename_with(backend, str_to_lower) %>%
  mutate_if(is.character, list(~na_if(., "NA"))) %>%
  mutate_if(is.character, list(~na_if(., "")))



Sys.time()
backend_tests <- 
  rename_with(backend_tests, str_to_lower) %>% 
  rename(spec_col_date = spec_coll_date)


Sys.time()
df <- 
  backend %>% 
  left_join(backend_tests, by = c("event_id")) %>%
  mutate_if(is.character, list(~na_if(., "NA")))


Sys.time()
# install.packages("pryr")
# library(pryr)
object_size(backend)
object_size(backend_tests)
object_size(df)


#### original
Sys.time()
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
df$source[is.na(df$source)] <- "unspecified specimen"
df$hospitalized[is.na(df$hospitalized)] <- "Unknown"

Sys.time()

#race edits

#### rewritten
Sys.time()
df <- 
  df %>%
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
  ) %>% 
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
df$source[is.na(df$source)] <- "unspecified specimen"
df$hospitalized[is.na(df$hospitalized)] <- "Unknown"
Sys.time()


Sys.time()
con2 <- DBI::dbConnect(odbc::odbc(), "epicenter")
statement <- paste0("SELECT * FROM [DPH_COVID_IMPORT].[dbo].[CTEDSS_DAILY_REPORT_ALL_Cases] C
                    LEFT JOIN [DPH_COVID_IMPORT].[dbo].[DAILY_Reports_ALL_COVID_TESTS] O
                    ON C.event_id = O.event_id")
cccc <- DBI::dbGetQuery(conn = con2 , statement = statement)
odbc::dbDisconnect(con2)
Sys.time()


rm(statement)

