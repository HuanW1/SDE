require(rmarkdown)
require(tidyverse)
require(sf)
require(odbc)
require(kableExtra)
require(lubridate)
require(formatR)
require(knitr)
require(MMWRweek)
require(scales)
require(english)
require(flextable)
require(DBI)

started_time <- Sys.time()

con2 <- DBI::dbConnect(odbc::odbc(), "epicenter")
log_data <- DBI::dbGetQuery(conn = con2 ,
                            statement = "SELECT * FROM [DPH_COVID_IMPORT].[dbo].[Daily_Report_Log]")
log_data
odbc::dbDisconnect(con2)


#### setting report dow options ####
# change for thanksgiving
thursday <- wday(Sys.Date()) == 5
wedthurs <- wday(Sys.Date()) %in% c(5) # makes no sense this is the same as previous line
not_thursday <- wday(Sys.Date()) != 5

thursday_range_start <- floor_date(Sys.Date() - 12, unit = "week")
thursday_range_end <- thursday_range_start + 13
monthsame <- month(thursday_range_start) == month(thursday_range_end)

if (monthsame) {
  thursday_range <- paste0(
    month(thursday_range_start, label = TRUE, abbr = FALSE), " ",
    format(thursday_range_start, "%d"), '-',
    format(thursday_range_end, "%d")
  )
} else {
  thursday_range <- paste0(
    lubridate::month(thursday_range_start, label = TRUE, abbr = FALSE), " ",
    format(thursday_range_start, "%d"), '-', lubridate::month(thursday_range_end, label= TRUE, abbr = FALSE),
    " ",
    format(thursday_range_end, "%d")
  )
}


## ----declare_functions, include=FALSE------------------------------------------------------------------------------------
df_to_table <-
  function(df_name,
           table_name = NULL,
           overwrite = FALSE,
           append = TRUE) {

    ### error checking
    if(overwrite == append)  stop("You can not append and overwrite at the same time")
    if(missing(df_name)) stop("You must specify a dataframe")
    if(!exists(deparse(substitute(df_name))))  stop("Can not find that dataframe")
    if(is.null(table_name)) stop("You must specify a table in the database")

    ### main code
    epi_connect <- DBI::dbConnect(odbc::odbc(), "epicenter")

    # build the SQL string
    fqdbn <- paste0("DPH_COVID_IMPORT.dbo.", table_name)

    DBI::dbWriteTable(epi_connect,
                      SQL(fqdbn),
                      df_name,
                      append = append,
                      overwrite = overwrite)

    odbc::dbDisconnect(epi_connect)
  }

table_to_df <-
  function(table_name = NULL) {

    ### error checking
    if(is.null(table_name)) stop("You must specify a table in the database")
    sql_table_name <- SQL("DPH_COVID_IMPORT.dbo.report_summary")

    ### main code
    epi_connect <- DBI::dbConnect(odbc::odbc(), "epicenter")

    # build the SQL string
    fqdbn <- paste0("select * FROM [DPH_COVID_IMPORT].[dbo].[",
                    table_name,
                    "] ")

    results <- DBI::dbGetQuery(epi_connect,
                               statement = fqdbn)

    odbc::dbDisconnect(epi_connect)
    return(results)
  }


#### list of PCR tests ####
pcrtests <- c('SARS CoV 2 ORF1 resp',
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
              'SARS-CoV-2 N gene Saliva',
              'COVID-19 PCR (CEPHEID)',
              'SARS CoV 2 NAA resp',
              'SARS-CoV-2 specific TCRB gene in Blood by Seq'
)

#### list of antigen tests ####
agtests <- c('SARS CoV 2 Ag rapid IA',
             'SARS-COV + SARS-CoV-2 Antigen resp',
             'SARS CoV 2 Ag (Quidel Sofia/Lumira)',
             'SARS CoV 2 Ag rapid IA (BD Veritor)',
             'SARS CoV 2 Ag rapid IA (BD Veritor/BinaxNOW)'
)

age_labels <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", ">=80")

pop <- read_csv("L:/daily_reporting_figures_rdp/population_data/2018_dph_asrh/2018_asrh.csv") %>%
  select(age_g, total) %>%
  filter(!is.na(age_g))

town_pop18 <- read_csv("L:/daily_reporting_figures_rdp/population_data/pop_towns2018.csv") %>%
  rename(city = Town,
         pop = `Est. Pop.`)

#county city mapping
cc_map <- read_csv(
  paste0("L:/daily_reporting_figures_rdp/dependancies/City County Mapping.csv")) %>%
  mutate(COUNTY = paste0(COUNTY," County"))
#county populations
county_pop <- read_csv("L:/daily_reporting_figures_rdp/population_data/county_pop.csv") %>%
  mutate(County = paste0(County, " County"))


## ----read_cha

cha_file <- list.files("L:/daily_reporting_figures_rdp/CHA_data_here", pattern = ".csv", full.names = TRUE)
cha <-  read_csv(cha_file)
newcha <- cha %>%
  filter(Type == "Admit" & State == "TOTAL") %>%
  select(-c(Change, County, State)) %>%
  pivot_longer(-Type, names_to = "admit_date", values_to = "admissions") %>%
  select(-Type) %>%
  mutate(admit_date = as.Date(admit_date, format = "%m/%d/%Y"))


## ----yesterday_summary, include=FALSE------------------------------------------------------------------------------------
#### Check for yesterday data otherwise create fake ####
if(file.exists(paste0("L:/daily_reporting_figures_rdp/yesterday/", Sys.Date() - 1))) {
  yesterday_file <- list.files(paste0("L:/daily_reporting_figures_rdp/yesterday/", Sys.Date() - 1),
                               pattern = ".csv")
} else {
  yesterday_file <- "No yesterday file yet"
}



## ----shapefiles, include = FALSE-----------------------------------------------------------------------------------------
#### towns shapes ####
if(file.exists("L:/daily_reporting_figures_rdp/shape_files/cb_2017_09_cousub_500k.shp")){
  filename <- list.files('L:/daily_reporting_figures_rdp/shape_files', pattern=".shp", full.names=FALSE)
  filename <- gsub(".shp", "", filename)
  subdat <- sf::st_read('L:/daily_reporting_figures_rdp/shape_files', "cb_2017_09_cousub_500k")
  subdat <- sf::st_transform(subdat,"+init=epsg:4326")
}

#### counties shapes ####
filename<-list.files('L:/daily_reporting_figures_rdp/shape_files', pattern=".shp", full.names=FALSE)
filename<-gsub(".shp", "", filename)
subdat2 <- st_read('L:/daily_reporting_figures_rdp/shape_files',
                   "countyct_37800_0000_1990_s100_CENSUS_1_shp_wgs84")
subdat2 <- st_transform(subdat2,"+init=epsg:4326")
subdat3 <- subdat2

con2 <- DBI::dbConnect(odbc::odbc(), "epicenter")

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
cases_dim <- dim(raw_cases)
tests_dim <- dim(raw_tests)


## ----df_creation--------------------------------------------------------
df <-  left_join(raw_cases, raw_tests, by = c("eventid"))

rm(raw_cases)
rm(raw_tests)

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
    bigID = str_squish(paste0(fname, lname, dob)),
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
  )

df <- df %>%
  left_join(cc_map %>% rename(county=COUNTY), by = c("city" = "CITY"))

endend_time <- Sys.time()


## ----multi_and_beyond----------------------------------------------------------------------------------------------------

#### multioutcome ####

# Sys.time()
# multioutcome <- test_df %>%
#   select(bigID, outcome) %>%
#   distinct(bigID, outcome) %>%
#   group_by(bigID) %>%
#   tally() %>%
#   filter(n >= 2) %>%
#   ungroup() %>%
#   left_join(df, by = c("bigID" = "bigID")) %>%
#   select(eventid, bigID, outcome, death_date) %>%
#   unique() %>%
#   filter(outcome == "Died" & !is.na(death_date))
#
# test_df <- test_df %>%
#   mutate(outcome = ifelse(bigID %in% multioutcome$bigID, "Died", outcome))
# Sys.time()
# #
# table(df$outcome)

# Sys.time()
df <-
  df %>%
  group_by(bigID) %>%
  mutate(outcome = ifelse(any(outcome == "Died"),
                           "Died",
                           outcome)) %>%
  ungroup()

# Sys.time()


##########  Death Cleanup     #############

df$outcome[df$covid_death == "No"] <- NA
#df$death_date[df$covid_death == "No"] <- NA

test_people <- df %>%
  filter(
    str_detect(str_to_lower(fname), "import|zztest|\\bzzz\\b|covid|validation|schedule|[0-9][^nsrt]|identif|\\btest\\b|sqtwo|sqthree|mytest")|str_detect(str_to_lower(lname), "zztest|\\bzzz\\b|covid|validation|schedule|[0-9][^nsrt]|identif|\\btest\\b|ascaris")
  ) %>%
  select(eventid) %>%
  distinct() %>%
  pull(eventid)


df <- df %>%
  filter(!eventid %in% test_people)

#####test results cleaning
df$result <- if_else(
  str_detect(df$result, "not ?(un)?detected|negative"),
  "not detected",
  if_else(
    str_detect(df$result,"(?<!not|un|not) ?detected|posi?ti?ve"),
    "detected",
    "indeterminate"
  )
)

###bad spec_col_dates blanked out
# df$spec_col_date[df$spec_col_date < ymd("2020-02-20")] <- NA
# df$spec_col_date[df$spec_col_date > Sys.Date()] <- NA

df <-
  df %>%
  mutate(spec_col_date = if_else(spec_col_date < ymd("2020-02-20") | spec_col_date > Sys.Date(),
                                 NA_Date_,
                                 spec_col_date,
                                 missing = NA_Date_))


#setting confirmed with no +pcr or blank pcr results to suspect at the top here and then the rest of the checks will pop them in their proper category should they be picked up again

#leave folks who are covid_death = Yes and their disease status %in% Confirmed, Probable alone, except probables should be able to be upped to conf if applicable
untouchable_conf <- df %>%
  filter(outcome == "Died" & disease_status == "Confirmed") %>%
  select(eventid) %>%
  distinct()
# count(untouchables, disease_status)
# count(untouchables, covid_death)

#suspect who should be probable based on ocmeid and covid_death = yes
ocmeprob <- df %>%
  filter(disease_status == "Suspect" & !is.na(ocmeid) & covid_death == "YES") %>%
  select(eventid) %>%
  distinct()


#currently not much going on with this object here, but maybe down the line
mostly_untouchable_prob <- df %>%
  filter(outcome == "Died" & disease_status == "Probable") %>%
  select(eventid) %>%
  distinct()

conf_good_result <- df %>%
  filter(
    test %in% pcrtests &
      disease_status == "Confirmed" &
      result %in% c('detected')
  ) %>%
  select(eventid) %>%
  distinct()

conf_bad_result <- df %>%
  filter(
    test %in% pcrtests &
      disease_status == "Confirmed" &
      result %in% c(NA, 'not detected', 'indeterminate')
  ) %>%
  select(eventid) %>%
  distinct()

conf_bad_result <- df %>%
  filter(eventid %in% conf_bad_result$eventid & !eventid %in% conf_good_result$eventid & !eventid %in% untouchable_conf$eventid & !eventid %in% mostly_untouchable_prob$eventid) %>% #the last filter portion with the probs doesn't change anything for now becuase this has to do with confirmeds, but i thoought it wouldnt hurt
  select(eventid, disease_status, test, result, spec_col_date, symp_onset_date) %>%
  arrange(eventid)

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
  select(eventid, disease_status, fever, chills, rigors, myalgia, headache, sorethroat, new_olfact_taste) %>%
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
  filter((eventid %in% one_of_symps$eventid|eventid %in% two_or_more_symps$eventid)) %>%
  unique()

df <- df %>%
  mutate(disease_status = ifelse(eventid %in% ocmeprob$eventid, "Probable", disease_status)) %>%  #ocmeid + covid_death = yes, setting disease status to probable
  mutate(disease_status = ifelse(eventid %in% pcr_not_confirmed$eventid,  "Confirmed", disease_status)) %>% #PCR postives not already confirmed  changed to Confirmed
  mutate(disease_status = ifelse(eventid %in% ag_probable$eventid,  "Probable", disease_status)) %>%  #AG postives not already probable changed to Probable
  mutate(disease_status = ifelse(eventid %in% suspect_probable$eventid, "Probable", disease_status))



endend_time <- Sys.time()

df_dim <- dim(df)

timey <- Sys.time()
timey <- gsub(pattern = " |:", replacement = "-", x = timey)

save(started_time,
     endend_time,
     cases_dim,
     tests_dim,
     df_dim,
     file = paste0("l:/quick_summary_", timey, ".RData" ))

save(df,
     file = paste0("l:/current_data_", timey, ".RData" ))

DeathUnder20 <- df %>%
  filter(covid_death == "YES" & age < 20 & event_date > "2020-02-01") %>%
  distinct(bigID, .keep_all = TRUE)

con2 <- DBI::dbConnect(odbc::odbc(), "epicenter")
rs <- dbSendQuery(con2, "SELECT bigID FROM [DPH_COVID_IMPORT].[dbo].[DQ_DeathUnder20]")
current_bigids <- dbFetch(rs)
current_count <- dbGetRowCount(rs)
dbClearResult(rs)
odbc::dbDisconnect(con2)

if(current_count != nrow(DeathUnder20)) {
  df_to_table(DeathUnder20,
              "DQ_DeathUnder20",
              overwrite = TRUE,
              append = FALSE)
  warning("Deaths Under 20 changed")
}

rm(df)
gc()

# janitor::compare_df_cols_same(df, df2)
# df2 <- structure(list(eventid = "101299413", fname = "suzanne", lname = "dahlberg",
#                dob = structure(-2870, class = "Date"), phone = NA_character_,
#                disease_status = "Suspect", age = 58, gender = "Female",
#                street = "87 Scotland Ave", city = "MADISON", county = "New Haven County",
#                state = "CT", race = "WHITE", hisp = "NO", hospitalized = "Unknown",
#                admit_date = NA_character_, discharge_date = "", icu = NA_character_,
#                preg = NA_character_, symptoms = NA_character_, symp_onset_date = NA_character_,
#                fever = NA_character_, fatigue = NA_character_, sob = NA_character_,
#                chills = NA_character_, sorethroat = NA_character_, headache = NA_character_,
#                cough = NA_character_, myalgia = NA_character_, new_olfact_taste = NA_character_,
#                rigors = NA_character_, pneumonia = NA_character_, ards = NA_character_,
#                outcome = NA_character_, death_date = structure(NA_real_, class = "Date"),
#                covid_death = NA_character_, ocme_reported = NA_character_,
#                ocmeid = NA_character_, vrn = NA_character_, healthcare_worker = NA_character_,
#                cong_setting = NA_character_, cong_exposure_type = NA_character_,
#                cong_facility = NA_character_, ptreside = NA_character_,
#                daycare_attendee = NA_character_, daycare_staff = NA_character_,
#                case_create_date = structure(18442, class = "Date"), case_mod_date = structure(18442, class = "Date"),
#                case_effective_date = structure(18442, class = "Date"), case_eff_from_date = structure(18438, class = "Date"),
#                event_date = structure(18438, class = "Date"), facilityname = NA_character_,
#                zip_code = "06443", race_concat = "WHITE", investigation_create_date = "06/29/2020",
#                investigation_mod_date = "06/29/2020", new_elr_result = "YES",
#                mrn_elr = "MR929673", test = "SARS CoV 2 PCR resp", result = "negative",
#                tested_date = "06/25/2020", spec_col_date = structure(18438, class = "Date"),
#                spec_rec_date = "06/25/2020", spec_num = "20Y-177VI0375",
#                source = "np swab", auth_facility = "Yale New Haven Health System And Yale Medical Group",
#                ordering_provider_name = "Craig David Thorne", lab_name = NA_character_,
#                facility_name = "Yale-New Haven Hospital Laboratory", ordering_provider_fname = "Craig",
#                ordering_provider_lname = "Thorne", case_modification_date = "06/29/2020",
#                result_rpt_date = "06/25/2020", device_id = NA_character_,
#                spec_source_snomed = NA_character_, result_free_text = NA_character_,
#                order_lab_facility = NA_character_, testing_lab_clia = NA_character_,
#                notes2 = NA_character_, bigID = "suzannedahlberg1962-02-22"), row.names = c(NA,
#                                                                                            -1L), class = "data.frame")
#

# save.image() # creating ".RData" in current working directory
