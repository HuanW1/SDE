### real
started_time <- Sys.time()
weekdays(lubridate::today())
started_time
library(rmarkdown, lib.loc = "l:/newlib/", quietly = TRUE, warn.conflicts = FALSE, logical.return = TRUE)
suppressPackageStartupMessages(library(tidyverse, quietly = TRUE, warn.conflicts = FALSE, lib.loc = "l:/newlib/", logical.return = TRUE))
suppressPackageStartupMessages(library(sf, lib.loc = "l:/newlib/", warn.conflicts = FALSE))
library(odbc, lib.loc = "l:/newlib/", quietly = TRUE, warn.conflicts = FALSE)
library(kableExtra, lib.loc = "l:/newlib/", quietly = TRUE, warn.conflicts = FALSE)
library(lubridate, lib.loc = "l:/newlib/", quietly = TRUE, warn.conflicts = FALSE)
library(formatR, lib.loc = "l:/newlib/", quietly = TRUE, warn.conflicts = FALSE)
library(knitr, lib.loc = "l:/newlib/", quietly = TRUE, warn.conflicts = FALSE)
library(MMWRweek, lib.loc = "l:/newlib/")
library(scales, lib.loc = "l:/newlib/", quietly = TRUE, warn.conflicts = FALSE)
library(english, lib.loc = "l:/newlib/", quietly = TRUE, warn.conflicts = FALSE)
library(flextable, lib.loc = "l:/newlib/", quietly = TRUE, warn.conflicts = FALSE)
library(DBI, lib.loc = "l:/newlib/")
library(stringdist, lib.loc = "l:/newlib/", quietly = TRUE, warn.conflicts = FALSE)

graphdate <- Sys.Date() - 1

con2 <- DBI::dbConnect(odbc::odbc(), "epicenter")
log_data <- DBI::dbGetQuery(conn = con2 ,
                            statement = "SELECT * FROM [DPH_COVID_IMPORT].[dbo].[Daily_Report_Log]")
tail(log_data)
odbc::dbDisconnect(con2)

logged_update_date <- max(log_data$UpdatedDate)

if(logged_update_date < lubridate::today()) warning("No new data")

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

#### setting what we write out ####
csv_write <- FALSE
SQL_write <- FALSE
do_DQ <- TRUE


## ---- declare_functions, include=FALSE ---- ##
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
    sql_table_name <- DBI::SQL("DPH_COVID_IMPORT.dbo.report_summary")

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
              'SARS-CoV-2 (COVID19) N gene [Presence] in Nose by NAA with probe detection'
)

#### list of antigen tests ####
agtests <- c('SARS CoV 2 Ag rapid IA',
             'SARS-COV + SARS-CoV-2 Antigen resp',
             'SARS CoV 2 Ag (Quidel Sofia/Lumira)',
             'SARS CoV 2 Ag rapid IA (BD Veritor)',
             'SARS CoV 2 Ag rapid IA (BD Veritor/BinaxNOW)'
)

age_labels <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", ">=80")

# pop <- read_csv("L:/daily_reporting_figures_rdp/population_data/2018_dph_asrh/2018_asrh.csv") %>% 
#   select(age_g, total) %>% 
#   filter(!is.na(age_g))

epi_connect <- DBI::dbConnect(odbc::odbc(), "epicenter")
ghost_data <- tbl(epi_connect, sql("SELECT * FROM DPH_COVID_IMPORT.dbo.RPT_STATE_REA_DENOMS"))

pop <- 
  ghost_data %>% 
  select(year:pop) %>%
  filter(year == 2019) %>%
  collect() %>%
  mutate(age_g = cut(age,
                     breaks = c(-1,9,19,29,39,49,59,69,79, Inf),
                     labels = age_labels)) %>%
  group_by(age_g) %>%
  summarise(total = sum(pop))

odbc::dbDisconnect(epi_connect)

town_pop18 <- read_csv("L:/daily_reporting_figures_rdp/population_data/pop_towns2018.csv") %>%
  rename(city = Town,
         pop = `Est. Pop.`)

#county city mapping
cc_map <- read_csv(
  paste0("L:/daily_reporting_figures_rdp/dependancies/City County Mapping.csv")) %>%
  mutate(COUNTY = paste0(COUNTY," County"))

### Using SQL 2019 county populations
epi_connect <- DBI::dbConnect(odbc::odbc(), "epicenter")
ghost_data <- tbl(epi_connect, sql("SELECT * FROM DPH_COVID_IMPORT.dbo.RPT_COUNTY_REA_DENOMS"))

county_pop <- 
  ghost_data %>% 
  select(YEAR:POP) %>%
  filter(YEAR == 2019) %>%
  group_by(cty_label) %>%
  summarise(Total = sum(POP, na.rm = TRUE)) %>%
  mutate(County = paste0(cty_label, " County")) %>%
  select(County, Total) %>% 
  arrange(County) %>%
  collect() 

rm(ghost_data)
odbc::dbDisconnect(epi_connect)


## ---- read_cha ---- ##

cha_file <- list.files("L:/daily_reporting_figures_rdp/CHA_data_here",
                       pattern = ".csv",
                       full.names = TRUE)
cha <-  read_csv(cha_file)

## ---- shapefiles, include = FALSE ---- ##
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

started_pull <- Sys.time()

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
          zip_code, race_concat, mname, suffix, COVID_EIP_ID as covid_eip_id
         FROM [DPH_COVID_IMPORT].[dbo].[CTEDSS_DAILY_REPORT_ALL_Cases]")

raw_cases <- DBI::dbGetQuery(conn = con2 , statement = statement)

if(nrow(raw_cases) != max(log_data$Case_Rows)) message("Cases are off")

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
      CASE_MODIFICATION_DATE as case_modification_date,
      result_rpt_date, 
      SPEC_SOURCE_SNOMED as spec_source_snomed,
      ORDER_LAB_FACILITY as order_lab_facility,
      TESTING_LAB_CLIA as testing_lab_clia
    FROM [DPH_COVID_IMPORT].[dbo].[DAILY_Reports_ALL_COVID_TESTS]"
  )

raw_tests <-  DBI::dbGetQuery(conn = con2 , statement = statement)

if(nrow(raw_tests) != max(log_data$Test_Rows)) message("Tests are off")

odbc::dbDisconnect(con2)

finished_pull <- Sys.time()

cases_dim <- dim(raw_cases)
tests_dim <- dim(raw_tests)

timey <- Sys.time()
timey <- gsub(pattern = " |:", replacement = "-", x = timey)

save(started_time,
     finished_pull,
     cases_dim,
     tests_dim,
     file = paste0("l:/Otto_summary_", timey, ".RData" ))

started_df <- Sys.time()

## ----df_creation--------------------------------------------------------
df <-  left_join(raw_cases, raw_tests, by = c("eventid"))

print("Deleting raw files")
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
    age = if_else(age < 0 | age > 121, NA_real_, age),
    cong_exposure_type = str_to_sentence(cong_exposure_type),
    cong_exposure_type = ifelse(
      is.na(cong_exposure_type) & ptreside == "Yes",
      "Reside",
      cong_exposure_type
    ),
    bigID = paste0(str_remove_all(paste0(fname,
                                         lname),
                                  "\\W|\\d"),
                   dob),
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
    hisp_race = case_when(
      hisp == "H"  ~ "Hispanic",
      race == "Unknown" ~ "Unknown",
      TRUE ~ paste0(hisp, " ", race)
    ),
    age_group = cut(age,
                    breaks = c(-1,9,19,29,39,49,59,69,79, Inf),
                    labels = age_labels)
  )

df <- df %>%
  left_join(cc_map %>% rename(county=COUNTY), by = c("city" = "CITY"))

## ---- multi_and_beyond ---- ##

# Sys.time()
df <-
  df %>%
  group_by(bigID) %>%
  mutate(outcome = ifelse(any(outcome == "Died"),
                           "Died",
                           outcome)) %>%
  ungroup()

if(do_DQ) {
  ### Create DQ_DeathTable and DQ_deathdups tables
  ### Source after multioutcome fix
  
  dq_death_check_table <- df %>%
    filter(outcome == "Died" | covid_death == "YES")%>%
    mutate(deaths_under_20 = ifelse(age < 20, 1, 0),
           death_too_early = ifelse((death_date < "2020-02-01" & !is.na(death_date)), 1, 0),
           dead_no_town = ifelse(is.na(city), 1, 0),
           dead_no_date = ifelse(is.na(death_date), 1, 0),
           dead_no_ocmeid = ifelse((outcome %in% "Died" | covid_death %in% "YES") & is.na(ocmeid), 1, 0),
           died_not_covid = ifelse((outcome %in% "Died" & (covid_death %in% "NO" | is.na(covid_death))), 1, 0)) %>%
    distinct(eventid, .keep_all=TRUE)%>%
    select(eventid, bigID, deaths_under_20, death_too_early, dead_no_town, dead_no_date, dead_no_ocmeid, died_not_covid)%>%
    filter(deaths_under_20==1 | death_too_early==1 | dead_no_town==1 | dead_no_date==1 | dead_no_ocmeid==1 | died_not_covid==1)
  
  df_to_table(df_name = dq_death_check_table,
              table_name = "DQ_deathtable",
              overwrite = TRUE,
              append = FALSE)
  
  ## DEATH DUPS:
  death_ids <- df %>%
    filter(outcome %in% "Died" | covid_death %in% "YES")
  
  death_dup_ids <- df %>%
    filter(bigID %in% death_ids$bigID)%>%
    distinct(eventid, bigID)%>%
    arrange(bigID)%>%
    group_by(bigID)%>%
    tally()%>%
    filter(n > 1)
  
  death_dups <- df %>%
    filter(bigID %in% c(death_dup_ids$bigID)) %>%
    select(eventid, bigID, covid_death, ocmeid) %>%
    arrange(bigID) %>%
    distinct(eventid, bigID, .keep_all = TRUE)
  
  ### WRITE SQL TABLE:
  df_to_table(df_name = death_dups,
              table_name = "DQ_deathdups",
              overwrite = TRUE,
              append = FALSE)
  
}

##########  Death Cleanup     #############
if (!dir.exists(paste0("L:/daily_reporting_figures_rdp/csv/", today()))) {
  dir.create(paste0("L:/daily_reporting_figures_rdp/csv/", today()))
}

future_deaths <- df %>%
  filter(outcome == "Died" & death_date > today())

write_csv(future_deaths, paste0("L:/daily_reporting_figures_rdp/csv/", 
                                today(), 
                                "/future_deaths.csv"))

df$outcome[df$covid_death %in% c("Unknown", "No")] <- NA
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

if(do_DQ) {

test_dq_table <- df %>% 
  mutate(test_too_early = ifelse(spec_col_date < ymd("2020-02-20"), 1, 0),
         test_in_future = ifelse(spec_col_date > mdy(result_rpt_date), 1, 0))%>%
  filter(test_in_future == 1 | test_too_early == 1)%>%
  select(eventid, spec_col_date, spec_num, test_too_early, test_in_future)%>%
  distinct(eventid, spec_col_date, spec_num, .keep_all=TRUE)


df_to_table(df_name = test_dq_table,
            table_name = "DQ_test_table",
            overwrite = TRUE,
            append = FALSE)

}


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

df <-
  df %>%
  mutate(spec_col_date = if_else(spec_col_date < ymd("2020-02-20") | spec_col_date > Sys.Date(),
                                 NA_Date_,
                                 spec_col_date,
                                 missing = NA_Date_))


# setting confirmed with no +pcr or blank pcr results to suspect at the top here 
# and then the rest of the checks will pop them in their proper category should they be picked up again
# leave folks who are covid_death = Yes and their disease status %in% Confirmed,
# Probable alone, except probables should be able to be upped to conf
# if applicable

untouchable_conf <- df %>%
  filter(outcome == "Died" & disease_status == "Confirmed") %>%
  select(eventid) %>%
  distinct()

# suspect who should be probable based on ocmeid and covid_death = yes
ocmeprob <- df %>%
  filter(disease_status == "Suspect" & !is.na(ocmeid) & covid_death == "YES") %>%
  select(eventid) %>%
  distinct()

# currently not much going on with this object here, but maybe down the line
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
  filter(state == "CT" | is.na(state)) %>%
  filter(test %in% pcrtests & !disease_status %in% c("Confirmed", "Not a case") & result == "detected") %>%
  select(eventid) %>%
  distinct()

pcr_confirmed <- df %>%
  filter(state == "CT" | is.na(state)) %>%
  filter(test %in% pcrtests & result == "detected") %>%
  select(eventid) %>%
  distinct()

ag_probable <- df %>%
  filter(!eventid %in% pcr_confirmed$eventid) %>%
  filter(state == "CT" | is.na(state)) %>%
  filter(test %in% agtests & !disease_status %in% c("Probable") & result == "detected") %>%
  select(eventid) %>%
  distinct()

df_suspect <- df %>%
  filter(disease_status == "Suspect") %>%
  filter(state == "CT"| is.na(state))

two_or_more_symps <- df_suspect %>%
  select(eventid, disease_status, fever, chills, rigors, myalgia, headache, sorethroat, new_olfact_taste) %>%
  pivot_longer(-c(eventid, disease_status), names_to = "symptom", values_to = "symp_pres") %>%
  filter(symp_pres == "Yes") %>%
  group_by(eventid, disease_status) %>%
  tally() %>%
  filter(!disease_status %in% c("Confirmed", "Probable")  & n >= 2) %>%
  select(eventid) %>%
  distinct()

all_two_or_more_symps <- df %>%
  select(eventid, disease_status, fever, chills, rigors, myalgia, headache, sorethroat, new_olfact_taste) %>%
  pivot_longer(-c(eventid, disease_status), names_to = "symptom", values_to = "symp_pres") %>%
  filter(symp_pres == "Yes") %>%
  group_by(eventid, disease_status) %>%
  tally() %>%
  filter(n >= 2) %>%
  select(eventid) %>%
  distinct()

one_of_symps <- df_suspect %>%
  select(eventid, disease_status, cough, sob, ards, pneumonia) %>%
  pivot_longer(-c(eventid, disease_status), names_to = "symptom", values_to = "symp_pres") %>%
  filter(symp_pres == "Yes") %>%
  group_by(eventid, disease_status) %>%
  tally() %>%
  filter(!disease_status %in% c("Confirmed", "Probable")) %>%
  select(eventid) %>%
  distinct()

all_one_of_symps <- df %>%
  select(eventid, disease_status, cough, sob, ards, pneumonia) %>%
  pivot_longer(-c(eventid, disease_status), names_to = "symptom", values_to = "symp_pres") %>%
  filter(symp_pres == "Yes") %>%
  group_by(eventid, disease_status) %>%
  tally() %>%
  select(eventid) %>%
  distinct()

probable_but_why <- df %>%
  group_by(eventid)%>%
  filter(!any(result == "detected"))%>%
  ungroup()%>%
  filter(disease_status == "Probable")%>%
  filter(!eventid %in% all_one_of_symps$eventid)%>%
  filter(!eventid %in% all_two_or_more_symps$eventid)%>%
  filter(!outcome %in% "Died")

suspect_probable <- df_suspect %>%
  filter((eventid %in% one_of_symps$eventid | eventid %in% two_or_more_symps$eventid)) %>%
  distinct()

if(do_DQ) {
  #add in after suspect_probable creation, and before mutate the disease_status changes
  dq_diseasestatus_check_table <- df %>%
    mutate(pcr_not_confirmed = ifelse(eventid %in% pcr_not_confirmed$eventid,  1, 0),
           confirmed_not_pcr = ifelse(eventid %in% conf_bad_result$eventid, 1, 0),
           ag_not_probable = ifelse(eventid %in% ag_probable$eventid,  1, 0),
           probable_but_why = ifelse(eventid %in% probable_but_why$eventid, 1, 0),
           suspect_but_symptoms = ifelse(eventid %in% suspect_probable$eventid, 1,0),
           ocme_to_probable = ifelse(eventid %in% ocmeprob$eventid, 1, 0)) %>%
    distinct(eventid, .keep_all=TRUE)%>%
    filter(state == "CT" | is.na(state))%>%
    select(eventid, pcr_not_confirmed, ag_not_probable, confirmed_not_pcr, probable_but_why, suspect_but_symptoms, ocme_to_probable)%>%
    filter(pcr_not_confirmed==1 | confirmed_not_pcr==1 | ag_not_probable==1 | probable_but_why==1 | suspect_but_symptoms==1 | ocme_to_probable==1)
  
#  write_csv(dq_diseasestatus_check_table, "dq_status_table.csv")
  
  df_to_table(df_name = dq_diseasestatus_check_table,
              table_name = "DQ_statustable",
              overwrite = TRUE,
              append = FALSE)
}

df <- df %>%
  mutate(disease_status = ifelse(eventid %in% ocmeprob$eventid, "Probable", disease_status)) %>%  #ocmeid + covid_death = yes, setting disease status to probable
  mutate(disease_status = ifelse(eventid %in% pcr_not_confirmed$eventid,  "Confirmed", disease_status)) %>% #PCR postives not already confirmed  changed to Confirmed
  mutate(disease_status = ifelse(eventid %in% ag_probable$eventid,  "Probable", disease_status)) %>%  #AG postives not already probable changed to Probable
  mutate(disease_status = ifelse(eventid %in% suspect_probable$eventid, "Probable", disease_status))

rm(df_suspect)
gc()

# endend_time <- Sys.time()
finished_df <- Sys.time()

df_dim <- dim(df)

timey <- Sys.time()
timey <- gsub(pattern = " |:", replacement = "-", x = timey)

save(df,
     file = paste0("l:/recent_rdata/raw_data_", timey, ".RData" ))

######case pt 1############
case <- df %>%
  filter(disease_status == "Confirmed"  | disease_status == "Probable") %>%
  filter(state == "CT" | is.na(state)) %>%
  mutate(
    date = case_when(
      disease_status %in% c("Confirmed", "Probable") & !is.na(spec_col_date) ~ spec_col_date,
      disease_status %in% c("Confirmed", "Probable") & is.na(spec_col_date) ~ event_date,
      TRUE ~ NA_Date_
    )) %>%
  filter(date > ymd("2020-03-01") &
           date <= today()) %>%
  mutate(
    week = epiweek(date),
    year = epiyear(date),
    mmwrweek = epiweek(date), #setting to date so it works for now
    simple_result = ifelse(
      result == "detected",
      1,2
    ),
    pcrtest = ifelse(test %in% pcrtests, 1, 2)
  ) %>%
  mutate(cong_yn = if_else(
    cong_exposure_type ==  "Reside" &
      (cong_setting== "JAIL" | cong_setting == "LTCF"| cong_setting == "ALF" ),
    "Yes", "No", missing = "No")) %>%
  group_by(bigID) %>%
  arrange(simple_result,pcrtest,date) %>% # detected, first priority, and then the pcr test or ag if no pcr, then earliest date, date is spec_col_Date for tests with that or eventdate if none listed or is a non ag+ prob
  slice(1L) %>%
  ungroup()

#Outcome and Covid Death Cleanup
#case$covid_death[case$eventid %in% untouchable_conf$eventid] <- "YES"  pulls in too many wrong.
case$covid_death[is.na(case$outcome) & case$covid_death == "NO" | case$covid_death == "UNKNOWN"] <- NA
case$outcome[case$covid_death == "NO" | is.na(case$covid_death)] <- NA
case$outcome[case$outcome == "Unknown"] <- NA
case$outcome[case$covid_death == "YES"] <- "Died"
case$death_date[is.na(case$covid_death) | case$covid_death == "NO"] <- NA

thursday_case <- case %>%
  filter(date >= thursday_range_start & date <= thursday_range_end )

case3 <- case %>%
  select(eventid, fname, lname, dob, phone, disease_status, age, gender, street, 
         city, county, state, hisp_race, race, hisp,hospitalized, admit_date, 
         discharge_date, icu, preg, symptoms, symp_onset_date, spec_col_date, 
         fever, fatigue, sob, headache, cough, myalgia, new_olfact_taste, 
         rigors, pneumonia, ards, outcome, covid_death, vrn, ocme_reported, 
         ocmeid, death_date, healthcare_worker, cong_setting, cong_exposure_type, 
         cong_facility,cong_yn, case_create_date, event_date, test, result, 
         lab_name, mmwrweek, mname, suffix, covid_eip_id)

if(csv_write){
  write_csv(case3, paste0("L:/daily_reporting_figures_rdp/csv/",
                          Sys.Date(),"/", Sys.Date(), "cases_wphi.csv"))
}

timey <- Sys.time()
timey <- gsub(pattern = " |:", replacement = "-", x = timey)

save(case3,
     file = paste0("l:/recent_rdata/cases_wphi_", timey, ".RData" ))


if(do_DQ) {
  
  ### Create DQ_demographic_changes, DQ_newdeathstoday, and DQ_missingdeaths tables
  ### Source after case3 is created but before it is removed
  if(!weekdays(today()) %in% c("Saturday", "Sunday")) {
    ##### flag changes in DOB or gender by eventid:
    if(weekdays(Sys.Date()) == "Monday"){
      x = 3
    } else {
      x = 1
    }
    
    yesterday_cases <-
      read_csv(paste0('L:/daily_reporting_figures_rdp/csv/', Sys.Date() - x, "/", Sys.Date() - x, "cases_wphi.csv"))
    
    yesterday_dob <- yesterday_cases %>%
      select(eventid, dob, gender, outcome)%>%
      mutate(date = Sys.Date()-x,
             date_order = "date_1",
             eventid = as.character(eventid)
      )
    
    dob_gender <- case3 %>%
      select(eventid, dob, gender, outcome)%>%
      mutate(date = Sys.Date(),
             date_order = "date_2")%>%
      bind_rows(yesterday_dob)
    
    dob_ids <- dob_gender %>%
      distinct(eventid, dob, .keep_all = TRUE) %>%
      group_by(eventid) %>%
      tally() %>%
      filter(n > 1)
    
    gender_ids <- dob_gender %>%
      distinct(eventid, gender, .keep_all = TRUE) %>%
      group_by(eventid) %>%
      tally() %>%
      filter(n > 1)
    
    outcome_ids <- dob_gender %>%
      distinct(eventid, outcome, .keep_all = TRUE) %>%
      group_by(eventid) %>%
      tally() %>%
      filter(n > 1)
    
    dob_change <- dob_gender %>%
      filter(eventid %in% dob_ids$eventid)%>%
      arrange(date_order)%>%
      pivot_wider(names_from = date_order, values_from = c("date", "dob", "gender", "outcome"))%>%
      filter(!is.na(dob_date_1)) %>%
      distinct(.keep_all = TRUE)
    
    gender_change <- dob_gender %>%
      filter(eventid %in% gender_ids$eventid)%>%
      arrange(date_order)%>%
      pivot_wider(names_from = date_order, values_from = c("date", "dob", "gender", "outcome"))%>%
      filter(!is.na(gender_date_1))%>%
      filter(!gender_date_1 %in% c("Unknown"))%>%
      distinct(.keep_all = TRUE)
    
    outcome_change <- dob_gender %>%
      filter(eventid %in% outcome_ids$eventid)%>%
      arrange(date_order)%>%
      pivot_wider(names_from = date_order, values_from = c("date", "dob", "gender", "outcome"))%>%
      filter(is.na(outcome_date_2))%>%
      distinct(.keep_all = TRUE)
    
    demo_changes <- bind_rows(dob_change, gender_change)%>%
      bind_rows(outcome_change)%>%
      distinct(.keep_all = TRUE)%>%
      mutate(year_diff = year(dob_date_1) - year(dob_date_2),
             month_diff = month(dob_date_1) - month(dob_date_2),
             day_diff = day(dob_date_1) - day(dob_date_2),
             age_diff = round(lubridate::time_length(difftime(ymd(dob_date_1),ymd(dob_date_2)), "years"),2),
             digit_diff = stringdist(dob_date_1, dob_date_2, method="hamming"))
    
    
    
    
    ### WRITE SQL TABLE, APPEND EACH DAY ###
    df_to_table(df_name = demo_changes,
                table_name = "DQ_demographic_changes",
                overwrite = FALSE,
                append = TRUE)
    
    #COMPARE DEATHS DAY OVER DAY:
    olddeaths <- yesterday_cases %>%
      filter(outcome == "Died")
    
    newdeaths <- case3 %>%
      filter(outcome == "Died")
    
    newdeathstoday <- newdeaths %>%
      filter(!eventid %in% olddeaths$eventid)
    
    missingdeaths <- olddeaths %>%
      filter(!eventid %in% newdeaths$eventid)
    
    ### WRITE SQL TABLES and CSVs:
    df_to_table(df_name = newdeathstoday,
                table_name = "DQ_newdeathstoday",
                overwrite = TRUE,
                append = FALSE)
    
    df_to_table(df_name = missingdeaths,
                table_name = "DQ_missingdeaths",
                overwrite = TRUE,
                append = FALSE)
    
    if (!dir.exists(paste0("L:/daily_reporting_figures_rdp/csv/", Sys.Date()))) {
      dir.create(paste0("L:/daily_reporting_figures_rdp/csv/", Sys.Date()))
    }
    
    write_csv(newdeathstoday, paste0("L:/daily_reporting_figures_rdp/csv/", Sys.Date(), "/newdeathstoday.csv"))
    write_csv(missingdeaths, paste0("L:/daily_reporting_figures_rdp/csv/", Sys.Date(), "/missingdeaths.csv"))
  }
}

rm(case3, yesterday_cases, yesterday_dob)

case <- case %>%
  select(-phone)

pendingaddress_text <- case %>%
  filter(is.na(county) & disease_status %in% c("Confirmed", "Probable")) %>%
  group_by(county) %>%
  tally() %>%
  select(n)

thursday_pending_address <- thursday_case %>%
  filter(is.na(county) & disease_status %in% c("Confirmed", "Probable")) %>%
  group_by(county) %>%
  tally() %>%
  select(n)

dim(thursday_pending_address)

outstatecolleges <- c(
  "Afc Bryn Mawr College",
  "American University Student",
  "American Unviersity Student Health Center",
  "Amherst College",
  "Anna Maria College",
  "Arcadia University",
  "Assumption University",
  "Babson College",
  "Bank Street College",
  "Bard College At Simon's Rock",
  "Barnard College",
  "Bay Path University",
  "Bentley University",
  "Boston College",
  "Brandeis University",
  "Brandeis University Admissions",
  "Brandeis University Heller",
  "Bridgewater State University",
  "Bryant University",
  "Campbell University Health",
  "Cc-University of San Diego",
  "Champlain College",
  "Chapman University",
  "Clark University",
  "Clark University Bts",
  "Clarkson University",
  "Coastal Carolina University",
  "Colby College",
  "College of the Atlantic",
  "College of the Holy Cross",
  "Columbia Univ Hlth Svcs",
  "Columbia University",
  "Columbia University- Medical",
  "Columbia University MC I/F",
  "Crh/emory University",
  "Curry College",
  "Curry Colllege",
  "Drexel University Student Hlth",
  "Duke University Screening",
  "Duquesne University-Student",
  "Elms College",
  "Elon University",
  "Embry Riddle Aeronautical Univ",
  "Emerson College",
  "Fisher College",
  "Fitchburg State University",
  "Fl Gulf Coast University",
  "Fordham Bronx Employees",
  "Fordham Bronx Students",
  "Fordham University",
  "Framingham State University",
  "Frostburg State Univ",
  "George Mason University",
  "Gordon College Medical center",
  "Hamilton College",
  "Hampshire College",
  "Health Ctr Landmark College",
  "High Point University Student",
  "Hobart And William Smith Colleges",
  "Hoboken University Medical Ctr",
  "Husson University",
  "Interlochen Arts Academy",
  "James Madison University",
  "Johnson And Wales University",
  "Keene State-Bts",
  "La Salle University",
  "Lasell University",
  "Liberty Univ Student Health Ctr",
  "Lincoln Center Employees",
  "Lincoln Center Students",
  "Manhattan College Health Services",
  "Manhattanville College",
  "Massachusetts College of Art And Design",
  "Massachusetts College of Liberal Arts",
  "Massachusetts Institute of Technology",
  "Massbio - Mattapan",
  "McDaniel College",
  "McPhs-Boston",
  "McPhs-Manchester",
  "McPhs-Worcester",
  "Medexpress - State College",
  "Medical Univ of South Carolina",
  "Merrimack College",
  "Messiah College Health Center",
  "Middlebury College",
  "Mount Holyoke College",
  "Mount Saint Mary College",
  "Nichols College",
  "Northeastern University Health And Counseling Services",
  "Northwestern Univ Hlth Serv",
  "Norwich University",
  "Notre Dame At Home",
  "Nys - Niagra County Community College",
  "Nys - Rochester Monroe Community College",
  "Providence College",
  "Psu-Covid University Park",
  "Reed College Health and Counseling Center",
  "Regis College",
  "Rhode Island School of Design",
  "Roger Williams University",
  "Rwu Providence Campus",
  "Salem State Staff",
  "Salem State University",
  "Seton Hill University",
  "Simmons University",
  "Smith College",
  "Springfield College",
  "St Olaf College",
  "St Louis University",
  "St. Thomas More",
  "Stonehill College",
  "Stony Brook University Student Health Center",
  "Suffolk University",
  "Suffolk University Sargent",
  "Suffolk University Sawyer",
  "Suny Maritime College",
  "Swathmore College",
  "Syracuse Univ Hlth Srvc",
  "the Catholic Univ of America",
  "the University of Vermont Medical Center",
  "Temple Univ Health Services",
  "Tuftsuemp (Marathon Health)",
  "Tuftsugst (Oehn-Provider1)",
  "Tuftsumst (Student Health Services)",
  "Tuftsuvnd (Oehn-Provider2)",
  "Umass Memorial",
  "Umass University Campus",
  "Umass University Health",
  "University of Lynchburg",
  "University of Maryland",
  "University of Massachusetts Amherst",
  "University of Massachusetts Amherst Springfield Center",
  "University of Massachusetts Boston",
  "University of Massachusetts Dartmouth",
  "University of Massachusetts Medical School",
  "University of Michigan Health System",
  "University of Missouri-Department of Pathology",
  "University of Rhode Island",
  "University of Scranton",
  "University of Tampa",
  "University of Vermont",
  "Villanova University",
  "Villanova University Shc",
  "Wake Forest University Student Health Services",
  "Wagner College",
  "Washington College Hlth Serv",
  "Wellesley College",
  "Westfield State University",
  "Wheaton College",
  "Williams College",
  "Worcester Polytechnic Institute",
  "Worcester State University"
)

outstate <- filter(df, auth_facility %in% outstatecolleges)

elr <-   df %>%
  rename(
    test_method = test,
    lab_result_create_date = investigation_create_date,
    lab_result_mod_date = investigation_mod_date,
    lab_facility = facility_name,
    spec_rec_date = spec_rec_date,
    date_tested = tested_date,
    date_reported_dph = result_rpt_date,
  ) %>%
  filter(!is.na(test_method)) %>% # filter our blank tests (captured later on in the in statements but cant hurt to have less data now)
  filter(!is.na(result)) %>% # filter out blank results
  filter(!auth_facility %in% outstatecolleges) %>%
  select(-c( admit_date, ards, chills,  cong_exposure_type, cong_facility, cong_setting, cough, covid_death, death_date, discharge_date, fatigue, fever, headache, healthcare_worker, hisp, hospitalized, icu, myalgia, new_olfact_taste, outcome,  preg, race, sob, sorethroat, rigors)) %>%
  filter(state == "CT" | is.na(state)) %>%  #keep ct or blank state
  filter(test_method %in% pcrtests| test_method %in% agtests) %>%  #keep only test methods we care about
  mutate( #new test var to distinct upon
    pcrag  = ifelse(test_method %in% pcrtests,
                    "pcr",
                    "ag"
    )
  ) %>%
  distinct(
    eventid, pcrag, spec_col_date, result, spec_num, .keep_all = TRUE
  ) %>%
  distinct(
    eventid, pcrag, spec_col_date, result, source, .keep_all = TRUE
  ) %>%
  distinct(
    pcrag, fname, lname,  dob, spec_col_date, spec_num, result, .keep_all = TRUE
  ) %>%
  distinct(
    pcrag, fname, lname, dob, spec_col_date, source, result, .keep_all = TRUE
  ) #%>%
#rename(zipcode = 'Postal Code')

elr_linelist <- elr
elr_linelist_elronly <-  elr_linelist %>%
  filter(new_elr_result == "YES") 

number_of_elr <- elr_linelist_elronly %>%
  filter(test_method %in% pcrtests) %>%
  nrow()

total_pcr_tests <- nrow(elr %>%  filter(test_method %in% pcrtests))  # change to PCR only and change name
total_ag_tests <- nrow(elr %>%  filter(test_method %in% agtests))

elr <- elr %>%
  group_by(spec_col_date, result) %>%
  tally()
missing_spec_date <- sum(elr$n[is.na(elr$spec_col_date)])
pos_tests <- sum(elr$n[elr$result == "detected"])
neg_tests <- sum(elr$n[elr$result == "not detected"])
total_tests_thursday <- elr %>% filter(spec_col_date >= thursday_range_start & spec_col_date <= thursday_range_end) %>% mutate(j=1) %>%  group_by(j) %>%  summarize(n = sum(n)) %>% select(n)
total_tests_thursday <-total_tests_thursday$n
pos_tests_thursday <- elr %>% filter(result == "detected" & spec_col_date >= thursday_range_start & spec_col_date <= thursday_range_end) %>% group_by(result) %>%  summarize(n = sum(n)) %>% select(n)
pos_tests_thursday <- pos_tests_thursday$n
neg_tests_thursday <- elr %>% filter(result == "not detected" & spec_col_date >= thursday_range_start & spec_col_date <= thursday_range_end) %>% group_by(result) %>%  summarize(n = sum(n)) %>% select(n)
neg_tests_thursday <- neg_tests_thursday$n

rm(df)

save(elr_linelist,
     file = paste0("l:/recent_rdata/elr_linelist_", timey, ".RData" ))

rm(elr, elr_linelist_elronly)
gc()

cha_c <- cha %>%
  filter(Type == "Admit") %>%
  select(-Type) %>%
  rename(NAME = County )

cols <- ncol(cha_c)

cha_c <- cha_c %>%
  rename( today = cols - 1,
          yesterday = cols - 2) %>%
  mutate(sign = ifelse(
    Change>=0,
    "+",
    "-"
  ),
  Change = abs(Change)
  )

num_groups <- c("None", "1 to 5", "6 to 10", "11 to 25", "26 to 50", "51 to 100", "101 to 200", "201 to 500", "501 to 1000", "1001 to 5000")
cha_c <- cha_c %>%
  mutate(
    ngrp = cut(today, breaks = c(-Inf,0,5,10,25,50,100,200,500,1000, Inf), labels = num_groups )
  )
subdat2 <- subdat2 %>%
  left_join(
    cha_c, by = c("NAME" = "NAME")
  )
case_m <- case %>%
  mutate(NAME = str_to_title(city)) %>%
  group_by(NAME) %>%
  tally() %>%
  complete(NAME = unique(town_pop18$city), fill = list(n = 0))
case_m$n[is.na(case_m$n)] <- 0
num_groups <- c("0 to 5", "6 to 50", "51 to 100", "101 to 200", "201 to 500", "501 to 1000", "1001 to 5000", ">5000")
case_m <- case_m %>%
  mutate(
    n = cut(n, breaks = c(-Inf,5,50,100,200,500,1000, 5000, Inf), labels = num_groups )
  )
subdat <- subdat %>%
  left_join(
    case_m, by = c("NAME" = "NAME")
  )
subdat$n[is.na(subdat$n)] <- "None"
subdat <- subdat %>%
  mutate(
    n=factor(n, levels = num_groups, labels = num_groups)
  )
num_groups2 <- c("None", "1 to 5", "6 to 50", "51 to 100", "101 to 200", "201 to 500", "501 to 1000", "1001 to 5000", "5001 to 10000", "10001 to 25000", "Greater than 25000")
cuml_cols <- ncol(cha)-2
cha_cuml <- cha %>%
  filter(Type == 'CumAdmit')
cha_cuml <- cha_cuml[c(1:3,cuml_cols)] %>%
  replace_na(replace = list(County = "TOTAL"))
names(cha_cuml)[length(names(cha_cuml))] <- "yesterday"
cha_cuml <- cha_cuml%>%
  mutate(
    cmlgrp = cut(yesterday, breaks = c(-Inf,0,5,50,100,200,500,1000, 5000, 10000, 25000, Inf), labels = num_groups2)
  )
subdat3 <- subdat3 %>%
  left_join(
    cha_cuml, by =c("NAME" = "County")
  )

today_text <- format(graphdate, "%B %d, %Y")
cases_text <- nrow(case)  # for now
confirmed_text <- nrow(case %>%  filter(disease_status == "Confirmed"))
probable_text <- nrow(case %>% filter(disease_status == "Probable"))
hosp_text <- str_to_sentence(as.english(cha_c$today[cha_c$State =='TOTAL']))  # for now
dec <- case %>% filter(outcome == 'Died') %>% nrow()
deaths_text <- str_to_sentence(as.english(dec))  # for now


timey <- Sys.time()
timey <- gsub(pattern = " |:", replacement = "-", x = timey)


ncases <- nrow(case)
ntests <- nrow(elr_linelist)

hospitalized_totals <-
  cha_c %>%
  filter(State == "TOTAL") %>%
  select(today)

discharged_totals <-
  cha %>%
  filter(Type == "Discharge") %>%
  select(-Type)

cols2 <- ncol(discharged_totals)

discharged_totals <-
  discharged_totals %>%
  rename( today = cols2 - 1,
          yesterday = cols2 - 2) %>%
  filter(State == "TOTAL") %>%
  select(today)


mock_table <-
  tibble(`Overall Summary` = c("COVID-19 Cases (confirmed and probable)",
                               "COVID-19 Tests Reported (molecular and antigen)",
                               "Daily Test Positivity*",
                               "Patients Currently Hospitalized with COVID-19",
                               "COVID-19-Associated Deaths"),
         `Total**` = c("", "", "", "", ""),
         `Change Since Yesterday` = c("", "", "", "", ""))

tbl_total_col <-
  c(as.character(ncases),
    as.character(ntests),
    "",
    as.character(hospitalized_totals$today),
    as.character(dec))

mock_table$`Total**` <- tbl_total_col

### Temporary logic to get last report
when_was_last_report <-
  case_when(weekdays(today()) %in% c("Monday", "Sunday") ~ "Friday",
            TRUE ~ weekdays(today() - 1))

last_rpt_data <-
  table_to_df("RPT_summary_bydate") %>%
  filter(dow_report_date == when_was_last_report) %>%
  filter(report_date == max(report_date))

yesterday_col <-
  c(
    paste0("+", ncases - last_rpt_data$Cases),
    paste0("+", ntests - last_rpt_data$Tests),
    paste0(round((ncases - last_rpt_data$Cases)/(ntests - last_rpt_data$Tests) * 100, 2), "%"),
    ifelse(hospitalized_totals$today - last_rpt_data$Hospitalized > 0,
           paste0("+", hospitalized_totals$today - last_rpt_data$Hospitalized),
           paste0(hospitalized_totals$today - last_rpt_data$Hospitalized)),
    ifelse(dec - last_rpt_data$Deaths > 0,
           paste0("+", dec - last_rpt_data$Deaths),
           paste0(dec - last_rpt_data$Deaths))
  )

mock_table$`Change Since Yesterday` <- yesterday_col
# mock_table

if(csv_write) {
  dir.create(paste0('L:/daily_reporting_figures_rdp/yesterday/', Sys.Date()))
  write_csv(mock_table,
            paste0("L:/daily_reporting_figures_rdp/yesterday/",
                   Sys.Date(), "/", Sys.Date(), ".csv"))
}


mock_table <-
  regulartable(mock_table) %>%
  flextable::autofit()
mock_table <- align_nottext_col(mock_table,
                                align = "center")

mock_table

positivity <- (ncases - last_rpt_data$Cases)/(ntests - last_rpt_data$Tests)

values_formatted <-
  paste0("'",
         paste(today(),
               weekdays(today()),
               ncases,
               ntests,
               positivity,
               hospitalized_totals$today,
               dec,
               sep = "', '"),
         "'")


statement <-
  paste0("IF EXISTS (SELECT * FROM [DPH_COVID_IMPORT].[dbo].[RPT_summary_bydate]
            WHERE report_date = '", today(), "')
            UPDATE [DPH_COVID_IMPORT].[dbo].[RPT_summary_bydate]
            SET report_date ='", today(), "', dow_report_date = '", weekdays(today()),
         "', Cases = '", ncases, "', Tests = '", ntests,
         "', Positivity = '", positivity, "', Hospitalized = '", hospitalized_totals$today,
         "', Deaths = '", dec,
         "' WHERE report_date = '", today(), "'
          ELSE
            INSERT INTO [DPH_COVID_IMPORT].[dbo].[RPT_summary_bydate]
            VALUES (", values_formatted, ")")

con2 <- DBI::dbConnect(odbc::odbc(), "epicenter")

dbExecute(
  con2,
  statement
)

odbc::dbDisconnect(con2)

endend_time <- Sys.time()
endend_time

timey <- Sys.time()
timey <- gsub(pattern = " |:", replacement = "-", x = timey)

# save(ovlsum, mock_table,
#      file = paste0("l:/draft_table_", timey, ".RData" ))

save(mock_table,
     today_text,
     cases_text,
     confirmed_text,
     probable_text,
     hosp_text,
     dec,
     file = paste0("l:/draft_table_", timey, ".RData" ))

timey <- Sys.time()
timey <- gsub(pattern = " |:", replacement = "-", x = timey)

save(case,
     file = paste0("l:/recent_rdata/case_", timey, ".RData" ))

print(paste("Finished", wday(now(), label = TRUE), "at", now()))