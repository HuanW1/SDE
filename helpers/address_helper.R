
`%nin%` <- Negate(`%in%`)

cc_map <- read_csv(
  paste0("L:/daily_reporting_figures_rdp/dependancies/City County Mapping.csv")) %>%
  mutate(COUNTY = paste0(COUNTY," County"))

zips <-
  search_state(state_abb = "CT") %>%
  select(zipcode, zipcity = major_city)

ct_boros <- read_csv(boro_file)
# 100+ connecticut boros and towns


data %>%
  mutate(Patient_city = str_to_title(Patient_city),
         Patient_county = str_to_title(Patient_county),
         Patient_zip_code = str_trunc(Patient_zip_code, 5, side = "right", ellipsis = ""),
#         Patient_county = gsub(" County", "", .$Patient_county),
         Bad_city = if_else(Patient_city %nin% cc_map$CITY, TRUE, FALSE),
         Bad_county = if_else(Patient_county %nin% cc_map$COUNTY, TRUE, FALSE),
         Bad_zipcode = if_else(Patient_zip_code %nin% zips$zipcode, TRUE, FALSE),
         Maybe_boro = if_else(Patient_city %in% ct_boros$Boro, TRUE, FALSE)) %>%
  group_by(Bad_city, Bad_county, Bad_zipcode, Maybe_boro) %>%
  summarise(n())


  filter(Patient_city %nin% county_table$CITY |
           Patient_county %nin% county_table$COUNTY) %>%
  select(Patient_city, Patient_county)
