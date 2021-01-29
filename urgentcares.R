labs <- elr_linelist %>% 
  filter(str_detect(str_to_lower(auth_facility), "urgent care|urgentcare|afc|gohealth|physicianone|prohealth uc|physician one|carewell|alliance uc|uc-#")) %>% 
  mutate (auth_facility = str_to_lower(auth_facility),
          urgentcaregroup = ifelse(str_detect(auth_facility, "afc"), "AFC",
                                   ifelse(str_detect(auth_facility, "gohealth"), "GoHealth",
                                          ifelse(str_detect(auth_facility, "physicianone|physician one"), "PhysicianOne",
                                                 ifelse(str_detect(auth_facility, "carewell"), "Carewell",
                                                        ifelse(str_detect(auth_facility, "docs|doc's"), "DOCS",
                                                               ifelse(str_detect(auth_facility, "kathy's|kathys"), "Kathy's",
                                                                      ifelse(str_detect(auth_facility, "priority"), "Priority",
                                                                             ifelse(str_detect(auth_facility, "stony creek"), "Stony Creek",
                                                                                    ifelse(str_detect(auth_facility, "velocity"), "Velocity",
                                                                                           ifelse(str_detect(auth_facility,"westport"), "Westport", "Other UC")
                                                                                    ))))))))))


auths <- count(labs, lab_facility, auth_facility)
ucs <- count(labs,  urgentcaregroup)

ucs2 <- count(labs, urgentcaregroup, lab_facility)