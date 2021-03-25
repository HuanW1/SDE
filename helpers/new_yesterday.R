library(tidyverse)
library(lubridate)
library(DBI)
library(odbc)

xxx <-
  list.files(path = "l:/daily_reporting_figures_rdp/yesterday",
             recursive = TRUE,
             pattern = ".csv",
             full.names = TRUE) %>%
  str_subset(.,
             pattern = "avgdailyincidence|old",
             negate = TRUE) %>%
  set_names() %>%
  map_dfr(~ read_csv(.x,
                     col_types = "cdc"),
          .id = "file_name") %>%
  tidyr::separate(col = "file_name",
                  into = c(NA, NA, NA, "report_date", NA),
                  sep = "/") %>%
  mutate(report_date = ymd(report_date),
         dow_report_date = weekdays(report_date)) %>%
  rename(Measure = `Overall Summary`,
         Change = `Change Since Yesterday`) %>%
  mutate(Total = if_else(is.na(Total), `Total**`, Total)) %>%
  mutate(Measure = case_when(str_detect(Measure, "Deaths") ~ "Deaths",
                             str_detect(Measure, "Cases") ~ "Cases",
                             str_detect(Measure, "Tests") ~ "Tests",
                             str_detect(Measure, "Hospitalized") ~ "Hospitalized",
                             str_detect(Measure, "Positivity") ~ "Positivity")) %>%
  mutate(Change = readr::parse_number(Change),
         Total = if_else(is.na(Total) & Measure == "Positivity", Change/100, Total))

xxx

table(xxx$Measure)
slice_tail(xxx, n = 20)

running_tally <-
  xxx %>%
  filter(report_date > "2020-11-03") %>%
  pivot_wider(id_cols = c(report_date, dow_report_date),
              names_from = Measure,
              values_from = Total)

slice_tail(running_tally, n = 20)

df_to_table(running_tally,
            "RPT_summary_bydate",
            overwrite = TRUE,
            append = FALSE)
table_to_df("RPT_summary_bydate")

