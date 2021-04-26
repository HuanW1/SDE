```{r county_data_request, message=FALSE, warning=FALSE, include=FALSE, eval=FALSE}
case_county_cum <-  case %>% 
  group_by(county) %>% 
  tally() %>% 
  filter(!is.na(county) & county != "Unknown") %>% 
  select(-county)

c7_logic <- case %>% 
  filter(date %in% seq.Date(Sys.Date()-7, Sys.Date(), by = "day")) 
if(nrow(c7_logic ) >=1){
  county7 <- case %>% 
    filter(date %in% seq.Date(Sys.Date()-7, Sys.Date(), by = "day")) %>% 
    group_by(county) %>% 
    tally(name = "n7") %>% 
    filter(!is.na(county) & county != "Unknown") %>% 
    bind_cols(case_county_cum) %>% 
    rename(
      County= county,
      "Cases in last 7 days" =n7,
      "Total cases to date" =n
    )
}else{
  county7 <- tibble(
    County = c("Fairfield County", "Hartford County", "Litchfield County", "New Haven County", "Tolland County", "Middlesex County", "New London County","Windham County", NA ),
    "Cases in last 7 days" = 0,
    "Total cases to date" = 0
  )  
}

if(csv_write){
  write_csv(county7, 
            paste0("L:/daily_reporting_figures_rdp/csv/", 
                   Sys.Date(), "/", "county7days.csv"))
}

```

```{r town_case_eids,  message=FALSE, warning=FALSE, include=FALSE}
thisweek <-  epiweek(Sys.Date())
townlhd <- read_csv("L:/daily_reporting_figures_rdp/dependancies/towntolhd10_30.csv")

cases <- case %>% 
  # mutate(cong_yn = if_else((str_detect(cong_exposure_type, "Reside")) &
  #                            (str_detect(cong_setting, "Jail / Prison") | str_detect(cong_setting, "Long term care facility") | str_detect(cong_setting, "Assisted Living Facility")),
  #                          "Yes", "No", missing = "No")
  # ) %>% 
  filter(mmwrweek >= thisweek-2 & mmwrweek <= thisweek-1) %>%
  filter(cong_yn == "No") %>% 
  select(eventid, city)
TOI <- unique(town_pop18$city) 
# dir.create("town_case_eventids")
dir.create(paste0("L:/daily_reporting_figures_rdp/town_case_eventids/", Sys.Date()))
mastercase <- cases %>% 
  left_join(townlhd, by = c("city" = "Town")) %>% 
  select(eventid, city, `Health Department Name`) %>% 
  filter(!is.na(city) & !is.na(`Health Department Name`) & city != "Not Available") %>% 
  arrange(`Health Department Name`, city)

if(csv_write){
  write_csv(mastercase, 
            path = paste0("L:/daily_reporting_figures_rdp/town_case_eventids/", 
                          Sys.Date(), "/", "mastereventidlist.csv"))
}

if(csv_write) {
  for(i in TOI){
    casesi <- cases %>%  
      filter(city == i) %>% 
      select(eventid)
    
    dir.create(paste0("L:/daily_reporting_figures_rdp/town_case_eventids/", 
                      Sys.Date(), "/", 
                      townlhd$`Health Department Name`[townlhd$Town == i]))  
    write_csv(casesi, 
              path = paste0("L:/daily_reporting_figures_rdp/town_case_eventids/", 
                            Sys.Date(), "/", 
                            townlhd$`Health Department Name`[townlhd$Town == i] ,
                            "/", i, "last2week.csv"))  
  }
}
