#setup for race/ethnicity plots

####0 libraries and connection ####
con <- DBI::dbConnect(odbc::odbc(), "epicenter")

####1 lookups ####
statement <- paste0("SELECT *FROM DPH_COVID_IMPORT.dbo.RPT_stanpop2000")
spop2000 <- DBI::dbGetQuery(conn = con , statement = statement)


statement <- paste0("SELECT cty_label AS county
                    ,agegp18_label AS age_group
                    ,sex_label AS gender
                    ,race6_label AS race
                    ,hisp2 AS hisp
                    ,POP AS pop
                    FROM DPH_COVID_IMPORT.dbo.RPT_COUNTY_REA_DENOMS
                    WHERE YEAR = 2019")
county_rea_denoms <- DBI::dbGetQuery(conn = con , statement = statement) %>% 
  mutate(hisp_race = ifelse(hisp == 1, "Hispanic", paste0("NH ", race))) 
county_rea_denoms$hisp_race[county_rea_denoms$hisp_race %in% c("NH Asian", "NH Native Hawaiian and Other Pacific Islander")] <- "NH Asian or Pacific Islander"
county_rea_denoms$hisp_race[county_rea_denoms$hisp_race %in% c("NH Two or more races")] <- "NH Multiracial" 
county_rea_denoms$hisp_race[county_rea_denoms$hisp_race %in% c("NH Black or African American")] <- "NH Black"
county_rea_denoms$hisp_race[county_rea_denoms$hisp_race == "NH American Indian or Alaska Native"] <- "NH American Indian or Alaskan Native"

more_ages <- county_rea_denoms %>% 
  mutate(age_group = ifelse(age_group == "0-4 yrs", "<5 yrs", age_group))

county_rea_denoms <- county_rea_denoms %>% 
  mutate(age_group = case_when(
    age_group %in% c("0-4 yrs", "5-9 yrs") ~ "0-9",
    age_group %in% c("10-14 yrs", "15-19 yrs") ~ "10-19",
    age_group %in% c("20-24 yrs", "25-29 yrs") ~ "20-29",
    age_group %in% c("30-34 yrs" , "35-39 yrs") ~ "30-39",
    age_group %in% c("40-44 yrs", "45-49 yrs") ~ "40-49",
    age_group %in% c("50-54 yrs", "55-59 yrs") ~ "50-59",
    age_group %in% c("60-64 yrs", "65-69 yrs") ~ "60-69",
    age_group %in% c("70-74 yrs", "75-79 yrs") ~ "70-79",
    age_group %in% c("80-84 yrs", "85+ yrs") ~ ">=80"
  )) %>% 
  group_by(county, age_group, gender, hisp_race) %>% 
  summarize(pop = sum(pop)) %>% 
  ungroup()

#clear trash
rm(statement)

####2 gender_race_eth ####
#line 2074
#gender_race_eth <- read_csv("L:/daily_reporting_figures_rdp/population_data/county_re_gender_age.csv")
hisp_race_labels <- c("Hispanic", "NH White", "NH Black", "NH American Indian or Alaskan Native", "NH Asian or Pacific Islander", "NH Other", "NH Multiracial", "Unknown")
gender_race_eth <- county_rea_denoms %>% 
  rename(n = pop)

#line 2356
race_eth_comb <-  case %>%
  group_by(hisp_race) %>% 
  tally(name = "case_tot") %>% 
  left_join(gender_race_eth %>% 
              group_by(hisp_race) %>%  
              summarize(tot=sum(n)),
            by = "hisp_race")%>% 
  mutate(caserate100k = round((case_tot/tot)*100000)) %>% 
  left_join(case %>%
              filter(outcome == "Died") %>% 
              group_by(hisp_race) %>%  
              tally(name = "deaths"),
            by = "hisp_race")%>% 
  replace_na(replace =list(deaths = 0)) %>% 
  mutate(deathrate100k = round((deaths/tot)*100000),
         hisp_race = factor(hisp_race, levels = hisp_race_labels, labels = hisp_race_labels))


####3 Age adjusted  case rates ####
#line 2437 -2623

aamr_lookup <- more_ages %>% 
  group_by(hisp_race, age_group) %>% 
  summarize(pop = sum(pop))

age_labels2 <- c("<5 yrs", "5-9 yrs", "10-14 yrs", "15-19 yrs", "20-24 yrs",
                 "25-29 yrs", "30-34 yrs", "35-39 yrs", "40-44 yrs", "45-49 yrs",
                 "50-54 yrs", "55-59 yrs", "60-64 yrs", "65-69 yrs", "70-74 yrs",
                 "75-79 yrs", "80-84 yrs", "85+ yrs")

agg_table <- case %>%
  select(age, hisp_race) %>%
  filter(!is.na(hisp_race) & !is.na(age) & age >= 0 & !hisp_race %in% c("NH Other", "Unknown")) %>%
  mutate(age_group = cut(age,
                         breaks = c(-1, 4, 9, 14, 19, 24, 29, 34, 39,
                                    44, 49, 54, 59, 64, 69, 74, 79, 84, Inf),
                         labels = age_labels2)) %>%
  group_by(hisp_race,  age_group) %>%
  tally() %>%
  left_join(aamr_lookup, by = c("hisp_race", "age_group")) %>%
  left_join(spop2000, by = c("age_group" = "age_g")) %>%
  replace_na(replace =  list(n= 0)) %>%
  mutate(
    crude_rate = n/pop*100000
  ) %>%
  group_by(age_group) %>%
  mutate(standard_age_g = sum(spop2000)) %>%
  group_by(hisp_race) %>%
  mutate(
    propn = standard_age_g/sum(standard_age_g),
    hisp_race_case_tot = sum(n),
    hisp_race_stan_tot = sum(pop)
  ) %>% 
  ungroup() %>% 
  mutate(age_specific_adjusted = crude_rate * propn,
         Crude = hisp_race_case_tot/hisp_race_stan_tot*100000) %>% 
  group_by(hisp_race) %>% 
  mutate(`Age adjusted` = sum(age_specific_adjusted))


adj_table <- agg_table %>% 
  select(hisp_race, Crude, `Age adjusted`) %>% 
  unique()

adj_tbl_long <- adj_table %>% 
  pivot_longer(-hisp_race, names_to = "rate_type", values_to = "n") %>%
  mutate(rate_type= factor(rate_type, levels=c("Crude", "Age adjusted"), labels=c("Crude", "Age adjusted")),
         n = round(n))


####4 Age adjusted  mortality rates ####
#line 2437 -2623
agg_table_dec <-  case %>% 
  filter(outcome == "Died") %>% 
  select(age, hisp_race) %>%
  filter(!is.na(hisp_race) & !is.na(age) & age >= 0 & !hisp_race %in% c("NH Other", "Unknown")) %>%
  mutate(age_group = cut(age,
                         breaks = c(-1, 4, 9, 14, 19, 24, 29, 34, 39,
                                    44, 49, 54, 59, 64, 69, 74, 79, 84, Inf),
                         labels = age_labels2)) %>%
  group_by(hisp_race,  age_group) %>%
  tally() %>%
  left_join(aamr_lookup, by = c("hisp_race", "age_group")) %>%
  left_join(spop2000, by = c("age_group" = "age_g")) %>%
  replace_na(replace =  list(n= 0)) %>%
  mutate(
    crude_rate = n/pop*100000
  ) %>%
  group_by(age_group) %>%
  mutate(standard_age_g = sum(spop2000)) %>%
  group_by(hisp_race) %>%
  mutate(
    propn = standard_age_g/sum(standard_age_g),
    hisp_race_case_tot = sum(n),
    hisp_race_stan_tot = sum(pop)
  ) %>% 
  ungroup() %>% 
  mutate(age_specific_adjusted = ifelse(hisp_race_case_tot >=20, crude_rate * propn, NA),
         Crude = hisp_race_case_tot/hisp_race_stan_tot*100000) %>% 
  group_by(hisp_race) %>% 
  mutate(`Age adjusted` = sum(age_specific_adjusted))
  
adj_table_dec <- agg_table_dec %>% 
  select(hisp_race, Crude, `Age adjusted`) %>% 
  unique()

adj_tbl_long_dec <- adj_table_dec %>% 
  pivot_longer(-hisp_race, names_to = "rate_type", values_to = "n") %>%
  mutate(rate_type= factor(rate_type, levels=c("Crude", "Age adjusted"), labels=c("Crude", "Age adjusted")),
         n = round(n))