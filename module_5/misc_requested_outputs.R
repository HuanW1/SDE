#### Module 5 / misc_requested_outputs ####
#This script will generate the COVID-19 reporting outputs needed for historical use cases and other stakeholders
message("Miscellaneous output process will now begin.  This usually takes X minutes")

#set up by mod5 setup

# ####2 county7days.csv ####
# county7days <-  case %>% 
#   filter(date %in% seq.Date(Sys.Date()-7, Sys.Date(), by = "day")) 
# 
# if(nrow(county7days)>0){
#   county7days <- case %>% 
#     filter(!is.na(county) & county %in% counties) %>% 
#     filter(date %in% seq.Date(Sys.Date()-7, Sys.Date(), by = "day")) %>% 
#     group_by(county) %>% 
#     tally(name = 'Cases in last 7 days') %>% 
#     inner_join(case %>% filter(!is.na(county) & county %in% counties) %>% 
#                  group_by(county) %>% tally(name = 'Total cases to date'),
#                by = "county") %>% 
#     rename(County = county)
# }else{
#   county7days<- tibble(
#     County = counties,
#     `Cases in last 7 days` = 0,
#     `Total cases to date` = 0
#   )  
# }
# 
# if(csv_write){
#   write_csv(county7days, paste0("L:/daily_reporting_figures_rdp/csv/", Sys.Date(), "/", "county7days.csv"))
# }


