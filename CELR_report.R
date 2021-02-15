# CELRreport.R
#
# script to perform make files for CDC
#
# In progress (relies heavily on Yale work)
# 1. 12/11/20: wininger - processes 1-3
# 2. 12/11/20: kleppinger - process 0 and collaborative edits
# 3. 12/14/20: wininger - processes 4+
# 4. 12/14/20: wininger - city county cleanup, etcetera
# 5. 12/15/20: kleppinger- testing code and updated splitting file
# 6. 12/19/20: wininger - completion of beta city-cleaning
# 7. 12/23/20: wininger - file-sourcing, edits to sources
# 8. 02/08/21: kleppinger - link to new source file and edit county and city and create new matching file
# 9. 02/11/21: Powell - Overhaul
################################################################

#### load libraries ####
.libPaths(c("L:/library", .libPaths()))

library(tidyverse)			# filter
library(zipcodeR)
library(stringdist)

#### declare functions ####

summarise_column <- function(dfname, colname) {
  dfname %>%
    group_by({{ colname }}, .drop = FALSE) %>%
    summarise(Count = n()) %>%
    mutate(Pct = Count / sum(Count) * 100) %>%
    arrange(desc(Count))
}

make_comp_table <- function(before_df, after_df, by_what) {
  left_join(before_df, after_df,
            by = by_what,
            suffix = c(".before", ".after")) %>%
    select(-Pct.before, -Pct.after) %>%
    mutate(Change = Count.after - Count.before) %>%
    arrange(desc(Count.after))
}

#### Pull data from epicenter with SQL command ####
statement <-
  paste0("
SELECT  [csv_file_version_no]
,[Patient_ID_assigner]
,[Reporting_facility_name]
,[Reporting_facility_ID]
,[Patient_ID]
,[Patient_ID_Type]
,[Public_health_case_ID]
,[Testing_lab_specimen_ID]
,[Specimen_type_description]
,[Specimen_type_code]
,[Specimen_type_code_system]
,[Specimen_type_free_text]
,[Specimen_collection_date_time]
,[Specimen_received_date_time]
,[Test_performed_description]
,[Test_performed_code]
,[Test_performed_code_system]
,[Test_method_description]
,[Test_result_description]
,[Test_result_coded]
,[Test_result_code_system]
,[Test_Result_free_text]
,[Test_result_units]
,[Reference_range]
,[Test_date]
,[Date_result_released]
,[Test_result_status]
,[Ordering_entity_name]
,[Testing_Lab_Name]
,[Testing_Lab_ID]
,[Testing_lab_ID_Type]
,[Testing_lab_street_address]
,[Testing_lab_city]
,[Testing_lab_state]
,[Testing_lab_zip_code]
,[Ordering_facility_name]
,[Ordering_provider_last_name]
,[Ordering_provider_first_name]
,[Ordering_provider_state]
,[Ordering_provider_street]
,[Ordering_provider_city]
,[Ordering_provider_zip_code]
,[Submitter_sample_ID_assigner]
,[Patient_DOB]
,[Patient_gender]
,[Patient_race]
,[Patient_ethnicity]
,[Patient_city]
,[Patient_state]
,[Patient_zip_code]
,[Patient_county]
,[Patient_age]
,[Illness_onset_date]
,[Pregnant]
,[Symptomatic_for_disease]
,[Patient_Location]
,[Employed_in_high_risk_setting]
,[Abnormal_flag]
,[Test_result_comparator]
,[Test_result_number_separator]
,[Test_result_number2]
,[Testing_lab_accession_number]
,[Test_result_number]
,[Ordering_facility_state]
,[Ordering_facility_street]
,[Ordering_facility_city]
,[Ordering_facility_zip_code]
,[Ordering_facility_phone_number]
,[Report_facil_data_source_app]
,[Ordered_test_code]
,[Ordered_test_description]
,[Ordered_test_code_system]
,[Order_result_status]
,[Submitter_unique_sample_ID]
-- ,[ExportDate]
--,[ExportStartDate]
--,[ExportEndDate]
--,[RecID]
FROM [DPH_COVID_IMPORT].[dbo].[CELR_REPORT]
WHERE ExportDate = '2021-02-15 11:46:30.490'")

# DBI::dbGetQuery(con2, statement = "select max(exportdate) FROM [DPH_COVID_IMPORT].[dbo].[CELR_REPORT]")
# latest_report <-
#   DBI::dbGetQuery(con2,
#                   statement = "SELECT MAX(exportdate) FROM [DPH_COVID_IMPORT].[dbo].[CELR_REPORT]")


con2 <- DBI::dbConnect(odbc::odbc(), "epicenter")
data <- DBI::dbGetQuery(conn = con2 , statement = statement)
odbc::dbDisconnect(con2)
glimpse(data)

#### Capture counts before #####

before_city <- summarise_column(data, Patient_city)
print(before_city, n = Inf)

before_county <- summarise_column(data, Patient_county)
print(before_county, n = Inf)

#### Run _CityCleaner3.R as is ####

source("_CityCleaner3.R") # yields four new columns at far-right of data-frame

#### for now needs work! ####
data$Patient_city <- data$City_Match

#### Capture after info ####
after_city <- summarise_column(data, Patient_city)
after_county <- summarise_column(data, Patient_county)

#### needs fixing in city_cleaner but for now
after_city$Patient_city <- str_to_upper(after_city$Patient_city)

city_comparison <- make_comp_table(before_city, after_city, "Patient_city")
print(city_comparison, n = Inf)

county_comparison <- make_comp_table(before_county, after_county, "Patient_county")
print(county_comparison, n = Inf)

#### Interim fix dates ####
# should never hurt but need to check if Tom B is fixing

data <-
  data %>%
  mutate(Test_date =
           case_when(
             Test_date == "0000    " &
               Date_result_released != "0000    "          ~ Date_result_released,
             Test_date == "0000    " &
               Date_result_released == "0000    " &
               Specimen_collection_date_time != "0000    " ~ Specimen_collection_date_time,
             TRUE                                          ~ Test_date)
  )

table(data$Test_date)

#### write csv's 25,000 lines at a time ####
# source("_FileSplitter.R")

which_test_date <- "20210212" # YMD

#### automate writing file name by test date
zero_pad <- paste0("InterPartner~CELR~CT~AIMSPlatform~Prod~Prod~",
                   which_test_date,
                   "18050000~STOP~COVID")

# data %>%
#   select(csv_file_version_no:Submitter_unique_sample_ID) %>%
#   group_by(grp = rep(row_number(), length.out = n(), each = 25000)) %>%
#   group_walk(~ write_csv(.x, paste0(zero_pad, .y$grp, ".csv"), na=""))

data %>%
  select(csv_file_version_no:Submitter_unique_sample_ID) %>%
  filter(Test_date == which_test_date) %>%
  group_by(grp = rep(row_number(), length.out = n(), each = 25000)) %>%
  group_walk(~ write_csv(.x, paste0(zero_pad, .y$grp, ".csv"), na=""))



#### Done csvs are in local directory ####

# "w:\Electronic Laboratory Reporting\Output\CTEDSS\MIF-PROD\CoV testing\CDC related\CELR\Output\Feb 15"




