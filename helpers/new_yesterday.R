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


# hospitalized_totals <-  cha_c %>%  filter(State== "TOTAL") %>% select(today)
# discharged_totals <- cha %>%
#   filter(Type == "Discharge") %>%
#   select(-Type)
# cols2 <- ncol(discharged_totals)
# discharged_totals <- discharged_totals %>%
#   rename( today= cols2-1,
#           yesterday = cols2-2) %>%
#   filter(State== "TOTAL") %>%
#   select(today)
#
# tbl_summary <- tibble(
#   'Overall Summary' = c(
#     "COVID-19 Cases (confirmed and probable)",
#     "COVID-19 Tests Reported (molecular and antigen)",
#     "Daily Test Positivity*",
#     "Patients Currently Hospitalized with COVID-19",
#     "COVID-19-Associated Deaths"
#   )
# )
#
# ncases <- nrow(case)
# ntests <- nrow(elr_linelist)
#
# tbl_total <-
#   tibble(
#     "Total**" = c(
#       ncases,
#       ntests,
#       0,#round(ncases/ntests*100,2),
#       hospitalized_totals,
#       dec
#     )
#   )
#
# tbl_total$`Total**` <- as.double(tbl_total$`Total**`)
#
# chg_yst <- read_csv(paste0("L:/daily_reporting_figures_rdp/yesterday/",Sys.Date() - 1, "/", yesterday_file)) %>%
#   select(`Total**`) %>%
#   rename(yesterday = `Total**`) %>%
#   bind_cols(tbl_total) %>%
#   mutate(
#     `Total**` =as.double(`Total**`),
#     sign = ifelse(`Total**`>= yesterday, "+", "-"),
#     delta = abs(`Total**` - yesterday),
#     'Change Since Yesterday' = paste0(sign, delta)
#   ) %>%
#   select('Change Since Yesterday')
# chg_yst[3,] <- paste0(round(as.double(chg_yst[1,])/as.double(chg_yst[2,])*100,2), "%")
#
#
#
# tbl_total$`Total**` <- as.character(tbl_total$`Total**`)
# tbl_total[3,] <- ""
# ovlsum <- bind_cols(tbl_summary, tbl_total, chg_yst)
# tableforgary <- ovlsum
# cha_stats <-  bind_cols(tbl_summary, tbl_total) %>%
#   rename(summary = `Overall Summary`, total = `Total**`)
#
# if(csv_write) {
#   dir.create(paste0('L:/daily_reporting_figures_rdp/yesterday/', Sys.Date()))
#   write_csv(ovlsum,
#             paste0("L:/daily_reporting_figures_rdp/yesterday/",
#                    Sys.Date(), "/", Sys.Date(), ".csv"))
# }
#
# ovlsum <- regulartable(ovlsum) %>%
#   flextable::autofit()
# ovlsum <- align_nottext_col(ovlsum,align = "center")
# ovlsum
#

mock_table <-
  tibble(`Overall Summary` = c("COVID-19 Cases (confirmed and probable)",
                               "COVID-19 Tests Reported (molecular and antigen)",
                               "Daily Test Positivity*",
                               "Patients Currently Hospitalized with COVID-19",
                               "COVID-19-Associated Deaths"),
         `Total**` = c("", "", "", "", ""),
         `Change Since Yesterday` = c("", "", "", "", ""))

tbl_total_col <-
  c(as.character(ncases),
    as.character(ntests),
    "",
    as.character(hospitalized_totals$today),
    as.character(dec))

mock_table$`Total**` <- tbl_total_col

last_rpt_data <-
  table_to_df("RPT_summary_bydate") %>%
  filter(report_date == Sys.Date() - 1)

yesterday_col <-
  c(paste0("+", ncases - last_rpt_data$Cases),
    paste0("+", ntests - last_rpt_data$Tests),
    paste0(round((ncases - last_rpt_data$Cases)/(ntests - last_rpt_data$Tests) * 100, 2), "%"),
    ifelse(hospitalized_totals$today - last_rpt_data$Hospitalized > 0,
           paste0("+", hospitalized_totals$today - last_rpt_data$Hospitalized),
           paste0(hospitalized_totals$today - last_rpt_data$Hospitalized)),
    ifelse(dec - last_rpt_data$Deaths > 0,
           paste0("+", dec - last_rpt_data$Deaths),
           paste0(dec - last_rpt_data$Deaths)))

mock_table$`Change Since Yesterday` <- yesterday_col
mock_table

write_csv(mock_table,
          "L:/yesterday_test.csv")

mock_table <-
  regulartable(mock_table) %>%
  flextable::autofit()
mock_table <- align_nottext_col(mock_table,
                                align = "center")

mock_table

con2 <- DBI::dbConnect(odbc::odbc(), "epicenter")

# statement <-
#   paste0("UPDATE [DPH_COVID_IMPORT].[dbo].[RPT_summary_bydate]
#           SET dow_report_date = 'Odinsday'
#           WHERE report_date = '2021-02-24'")


dbExecute(
  con2,
  statement
)


values_formatted <-
  paste0("'",
         paste(today(),
               weekdays(today()),
               ncases,
               ntests,
               (ncases - last_rpt_data$Cases)/(ntests - last_rpt_data$Tests),
               hospitalized_totals$today,
               dec,
               sep = "', '"),
         "'")


statement <-
  paste0("IF EXISTS (SELECT * FROM [DPH_COVID_IMPORT].[dbo].[RPT_summary_bydate]
            WHERE report_date = '", today(), "')
            UPDATE [DPH_COVID_IMPORT].[dbo].[RPT_summary_bydate]
            SET report_date ='", today(), "', dow_report_date = '", weekdays(today()),
            "', Cases = '", ncases, "', Tests = '", ntests,
            "', Positivity = '", Positivity, "', Hospitalized = '", hospitalized_totals$today,
            "', Deaths = '", dec,
            "' WHERE report_date = '", today(), "'
          ELSE
            INSERT INTO [DPH_COVID_IMPORT].[dbo].[RPT_summary_bydate]
            VALUES (", values_formatted, ")")


ncases <- 278184
ntests <- 6591912
Positivity <- (ncases - 276691)/(ntests - 6544400)
ncases
ntests <- 999999
