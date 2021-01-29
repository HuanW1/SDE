library(tidyverse)
library(lubridate)

cases <- read_csv("W:\\DosApps\\EPI\\EPISTAFF\\2019-nCoV\\COVID Data Archive\\CSV\\2020-12-21\\2020-12-21cases_wphi.csv")
elr <-  read_csv("W:\\DosApps\\EPI\\EPISTAFF\\2019-nCoV\\COVID Data Archive\\CSV\\2020-12-21\\elr_linelist2020-12-21.csv")


# gary_town_file <- tibble(
#   CITY = town_total_cases$CITY,
#   LastUpdateDate = graphdate,
#   TownTotalCases = town_total_cases$n,
#   TownConfirmedCases = town_confirmed_cases$n,
#   TownProbableCases = town_probable_cases$n,
#   TownTotalDeaths = town_total_deaths$Deaths,
#   TownConfirmedDeaths = town_confirmed_deaths$Deaths,
#   TownProbableDeaths = town_probable_deaths$Deaths,
#   TownCaseRate = TownCaseRate$CaseRate,
#   PeopleTested = peopletestbytown$PeopleTested,
#   NumberofTests = town_tests$number_of_tests,
#   NumberofPositives = town_tests$number_of_positives,
#   NumberofNegatives = town_tests$number_of_negatives,
#   NumberofIndeterminates = town_tests$number_of_indeterminates,
#   RateTested100k = peopletestbytown$RateTested100k
# )


## towncase rate / peopletested rate
towns <- enframe(unique(cases$city)) %>% 
  filter(value != "Not Available") %>% 
  select(value)

weeks <- epiweek(ymd("2020-03-01")):epiweek(Sys.Date())
  
town_pop18 <- read_csv("population_data/pop_towns2018.csv")

TownCaseRate <- cases %>%
  filter(!is.na(city) & city != "Not Available") %>% 
  group_by(mmwrweek, city) %>% 
  tally(name ='case_n') %>% 
  ungroup() %>% 
  complete(city = towns$value, mmwrweek = weeks,  fill = list(case_n = 0)) %>% 
  left_join(town_pop18, by = c( "city" = "Town")) %>% 
  rename(pop = `Est. Pop.`) %>% 
  mutate(CaseRate = round((case_n/pop)*100000)) %>% 
  filter(!is.na(mmwrweek)) %>% 
  arrange( city, mmwrweek)



peopletestbytown<- elr %>% 
  mutate(simple_result = ifelse( 
    result == "detected",
    1,2
  ),
  mmwrweek = epiweek(spec_col_date)
  ) %>%
  group_by(bigID) %>%
  arrange(simple_result,spec_col_date) %>%
  slice(1L) %>%
  ungroup() %>% 
  rename(Town = city, SpecimenCollectionDate = spec_col_date) %>% 
  group_by(mmwrweek, Town) %>% 
  tally(name = "PeopleTested") %>% 
  filter(Town != "Not Available") %>% 
  ungroup() %>% 
  complete(Town = towns$value, mmwrweek = weeks,  fill = list(PeopleTested = 0)) %>% 
  mutate(Town = str_to_title(Town)) %>% 
  left_join(town_pop18, by = c("Town" = "Town")) %>% 
  rename(pop = `Est. Pop.`) %>% 
  mutate(RateTested100k = round(PeopleTested/pop * 100000)) %>% 
  filter(!is.na(mmwrweek)) %>% 
  arrange(Town, mmwrweek) %>% 
  select(-pop)


totalstestbytown<- elr %>% 
  mutate(mmwrweek = epiweek(spec_col_date)) %>% 
  rename(Town = city, SpecimenCollectionDate = spec_col_date) %>% 
  group_by(mmwrweek, Town) %>% 
  tally(name = "TotalTests") %>% 
  filter(Town != "Not Available") %>% 
  ungroup() %>% 
  complete(Town = towns$value, mmwrweek = weeks,  fill = list(TotalTests = 0)) %>% 
  mutate(Town = str_to_title(Town)) %>% 
  left_join(town_pop18, by = c("Town" = "Town")) %>% 
  rename(pop = `Est. Pop.`) %>% 
  mutate(Tests100k = round(TotalTests/pop * 100000)) %>% 
  filter(!is.na(mmwrweek)) %>% 
  arrange(Town, mmwrweek) %>% 
  select(-pop)

combined <- TownCaseRate %>% 
  left_join(peopletestbytown, by = c("city" = "Town", "mmwrweek")) %>%
  left_join(totalstestbytown, by = c("city" = "Town", "mmwrweek")) %>%
  mutate(peopletested_over_case_rates = PeopleTested/CaseRate,
         teststotals_over_case_Rates = Tests100k/CaseRate
         )
cities <- towns$value
#town_counts[town_counts$city == cities[i],]
for(i in seq_along(cities)){
plot1 <- ggplot(combined[combined$city == cities[i],])+
  geom_line(aes(mmwrweek,teststotals_over_case_Rates, col= "Test totals rate over case rate" ))+
  geom_line(aes(mmwrweek,peopletested_over_case_rates, col = "People tested" ))+
    labs(title = paste0(i))
  
  ggsave(plot1, file=paste0(cities[i], " .png"), scale = 2)
}

