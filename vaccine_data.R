####0 declare libraries ####
if(!dir.exists("L:/")) message("You need to have L drive mapped")
.libPaths(c("L:/library", .libPaths()))
require(tidyverse)
require(data.table)
require(readxl)
require(stringr)

####1 data read-in ####
vaccine_master <- readxl::read_xlsx("Master_COVID_Weekly_Report_1.xlsx")



####2 data cleanup and standardization ####
vm_clean <- vaccine_master %>%
  mutate_if(is.character, list(~na_if(., "NULL"))) %>%  #replace text NULLS with NA
  mutate(
    patient_first_name = str_squish(str_to_lower(patient_first_name)),
    patient_middle_name = str_squish(str_to_lower(patient_middle_name)),
    patient_last_name = str_squish(str_to_lower(patient_last_name)),
    bigID = paste0(patient_first_name, patient_last_name, dob) 
      ) %>% 
  #deduping prototype
  distinct(bigID, vaccine, Given_Date, doses_number,.keep_all = T) %>% #close to what they're reporting, but not quite - bigID likely the weakest link
  group_by(bigID) %>% 
  #creating flags for fully or partial
  mutate(num_doses = sum(Total_doses, na.rm = T),
         fp_vac = if_else(num_doses >1 , 1, 0) #assuming all vaccines >=2 doses 
         ) %>% 
  ungroup() %>% 
  mutate(hisp = 0,
         hisp_race = "",
         range_age = if_else(Age <=15, "15 or below", range_age),
         range_age = if_else(Age >15 & Age <25, "16-24", range_age)
         )

#hisp creation
vm_clean$hisp[vm_clean$Ethnicity == "Hispanic or Latino"] <- 1
vm_clean$hisp[vm_clean$Ethnicity == "Not Hispanic or Latino"] <-  0 
vm_clean$hisp[vm_clean$Ethnicity == "Unknown / Not Reported"] <- 0 #they coding unknown Hisp as NH?

#hisp_race creation
vm_clean <- vm_clean %>% 
  mutate(
    hisp_race = if_else(hisp == 1, "Hispanic", 
                        if_else(!race %in% c("White", "Unknown", "Multiple Races ") & hisp == 0, paste0("NH ", race), race )
                        )
    )

  
####2a notes and error checking#####
greaterthan2 <- vm_clean %>% #these look like date entry errors
  filter(num_doses >2)
# notes: I think all vaccines are 2 doses series now, JJ is one dose, maybe others. We'll need a lookup for vaccine and number of does required -AS 2/17/21


####3 Cumulative Number and Percent of People Partially and Fully COVID-19 Vaccinated by Age Group ####

vm_ageg <- vm_clean %>% #age_ranges need tweaking, probs just create a raw one
  group_by(bigID, range_age, fp_vac) %>% 
  tally() %>% 
  group_by(range_age, fp_vac) %>%
  tally()

####4 Cumulative Number and Percent of PEople PArtially and Fully COVID-19 Vaccinated by sex ####

vm_gender<- vm_clean %>% #age_ranges need tweaking, probs just create a raw one
  group_by(bigID, gender, fp_vac) %>% 
  tally() %>% 
  group_by(gender, fp_vac) %>%
  tally()
### very different numbers problem with my code or theirs?

vm_clean2 <- vaccine_master %>%
  mutate_if(is.character, list(~na_if(., "NULL"))) %>%  #replace text NULLS with NA
  mutate(
    patient_first_name = str_squish(str_to_lower(patient_first_name)),
    patient_middle_name = str_squish(str_to_lower(patient_middle_name)),
    patient_last_name = str_squish(str_to_lower(patient_last_name)),
    bigID = paste0(patient_first_name, patient_last_name, dob) 
  ) %>% 
  group_by(bigID) %>% 
  #creating flags for fully or partial
  mutate(num_doses = sum(Total_doses, na.rm = T),
         fp_vac = if_else(num_doses >1 , 1, 0) #assuming all vaccines >=2 doses 
  ) %>% 
  ungroup()

vm_gender2 <- vm_clean2 %>% 
  group_by(bigID, gender, fp_vac) %>% 
  tally() %>% 
  group_by(gender, fp_vac) %>%
  tally()

####5 Numbers by race/ethnicity ####
vm_hisprace <- vm_clean %>% 
  group_by(bigID, hisp_race, fp_vac) %>% 
  tally() %>% 
  group_by(hisp_race, fp_vac) %>%
  tally()


####6 Numbers by race/ethnicity ages 65-74 ####
vm_hisprace65_75 <- vm_clean %>% 
  filter(range_age == "65-74") %>% 
  group_by(bigID, hisp_race, fp_vac) %>% 
  tally() %>% 
  group_by(hisp_race, fp_vac) %>%
  tally()

####7 Numbers by county ####
vm_county <- vm_clean %>% 
  group_by(bigID, county, fp_vac) %>% 
  tally() %>% 
  group_by(county, fp_vac) %>%
  tally()

####8 Numbers by city####
#great place to add the Borough crosswalk to official city, if they want it
vm_city <- vm_clean %>% 
  filter(patient_state == "CT") %>% 
  group_by(bigID, patient_city, fp_vac) %>% 
  tally() %>% 
  group_by(patient_city, fp_vac) %>%
  tally()
