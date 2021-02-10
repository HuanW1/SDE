
`%nin%` <- Negate(`%in%`)


data %>%
  mutate(Patient_city = str_to_title(Patient_city),
         Patient_county = str_to_title(Patient_county),
         Patient_county = gsub(" County", "", .$Patient_county),
         Bad_city = if_else(Patient_city %nin% county_table$CITY, TRUE, FALSE),
         Bad_county = if_else(Patient_county %nin% county_table$COUNTY, TRUE, FALSE),
         Bad_zipcode = if_else(Patient_zip_code %nin% zips$zipcode, TRUE, FALSE)) %>%
  group_by(Bad_city, Bad_county, Bad_zipcode) %>%
  summarise(n())


  filter(Patient_city %nin% county_table$CITY |
           Patient_county %nin% county_table$COUNTY) %>%
  select(Patient_city, Patient_county)
