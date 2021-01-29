count(outstate, result2)

oospos<-outstate %>% 
  filter(result2=="Positive") %>% 
  filter(lab_result_mod_date == "10/01/2020" | lab_result_mod_date == "10/02/2020")

write_csv(oospos, "csv/2020-10-01/2020-10-01oospos.csv")

oosna <-outstate %>% 
  filter(is.na(result2))

# newcase <- read_csv("csv/2020-10-01/2020-10-01cases.csv") %>% 
#   filter(!eventid %in% oospos$eventid)
# 
# write_csv(newcase, "csv/2020-10-01/2020-10-01cases.csv")

norwich <- case %>% 
  filter(city=="Norwich" & mmwrweek==39)
mansfield<-count(case, city=="Mansfield", mmwrweek)
middletown <- count(case, city=="Middletown", mmwrweek)

#count(test_t, city=="Mansfield", week==39)


broad <- elr_linelist %>% 
  filter(str_detect(lab_facility, "Broad") & lab_result_mod_date == "10/01/2020") %>% 
 # filter(result2=="Positive")
  #group_by(spec_col_date, result2) %>% 
  #tally()
group_by(spec_col_date, result2) %>% 
  tally()

moddates <- elr_linelist %>% 
  filter(lab_result_mod_date %in% c("10/01/2020", "09/30/2020", "10/02/2020")) %>% 
  group_by(lab_facility, lab_result_mod_date) %>% 
  tally()

countdate <- elr_linelist %>% 
  group_by(spec_col_date) %>% 
  tally() %>% 
  filter(spec_col_date >=mdy("03-01-2020"))


ggplot(countdate)+ 
  geom_col(aes(x=spec_col_date, y=n))+
  theme_classic()+
  labs(
    x="Specimen Collection Date",
    y="Number of PCR Tests",
    title ="PCR Tests by Specimen Collection Date"
  )

broadpos <- elr_linelist %>% 
  filter(eventid %in% broad$eventid) %>% 
  group_by(city, result) %>% 
  tally()

broadpos2 <- broadpos %>% 
  group_by(eventid) %>% 
  filter(result2=="Positive") %>% 
  tally()
  
  
  
count(broad, result2)
write_csv(broad, "csv/2020-10-01/2020-10-01broadimport.csv")


auth_facils2 <- auth_facils %>% 
  filter(str_detect(str_to_lower(auth_facility), "univ|college"))
auth_facils3 <- auth_facils2 %>% 
  filter(!auth_facility %in% outstatecolleges$out_state_schools)
