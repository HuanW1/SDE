source("helpers/StartMeUp.R")

con <- DBI::dbConnect(odbc::odbc(), "epicenter")
ghost_data <-
  tbl(con, sql("SELECT EVENT_ID as eventid, fname, mname, lname, suffix, dob, phone,
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
                    zip_code, race_concat, COVID_EIP_ID as covid_eip_id
                FROM [DPH_COVID_IMPORT].[dbo].[CTEDSS_DAILY_REPORT_ALL_Cases]
                WHERE outcome = 'Died'"))

bring_out_your_dead <- ghost_data %>% collect()

odbc::dbDisconnect(con)

bring_out_your_dead

