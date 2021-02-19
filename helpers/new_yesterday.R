#replacement for yesterday in next chunk

#Connecting to SQL022 DB and creating a local df from StateSummary table
con <- DBI::dbConnect(odbc::odbc(), "epicenter")

query <- paste0("SELECT * FROM [DPH_COVID_IMPORT].[dbo].[ODP_StateSummary]")
ssummary <- DBI::dbGetQuery(conn = con, statement = query)

#filtering for latest records, inserting additional columns into the df and changing the data types - -100000 needs to change once we have valid entries in the table
new_yesterday <- ssummary %>%
  filter(DateUpdated == Sys.Date() - 2) %>%
  mutate(ReportTime = as.character(Sys.time() -100000),
         Total = as.numeric(Total),
         Change = as.numeric(Change)
  ) %>%
  #mutate(ChangeDirection = as.numeric(ChangeDirection)) %>%
  mutate() %>%
  head(5)

new_yesterday$SNo <- seq.int(nrow(new_yesterday))

#creating a table to write the new df
DBI::dbWriteTable(con, SQL("DPH_COVID_IMPORT.dbo.ODP_TESTYEST"), new_yesterday, append = FALSE, overwrite = TRUE)

today <- tbl_total %>%
  rename(today = "Total**") %>%
  rowid_to_column(var = "SNo")

statement <- paste0("SELECT ReportTime FROM [DPH_COVID_IMPORT].[dbo].[ODP_TESTYEST]")
ReportTimes <- DBI::dbGetQuery(conn = con, statement = statement)  %>%
  mutate(ReportTime = lubridate::ymd_hms(ReportTime, tz = "EST")) %>%
  unique() %>%
  filter(as_date(ReportTime) != Sys.Date()) %>%
  slice_max(ReportTime) %>%
  pull(ReportTime)

statement <- paste0("SELECT * FROM [DPH_COVID_IMPORT].[dbo].[ODP_TESTYEST] WHERE ReportTime = '", ReportTimes, "'")
rm(new_yesterday)
new_yesterday <- DBI::dbGetQuery(conn = con, statement = statement) %>%
  select(SNo, Total) %>%
  rename(new_yesterday = Total)

new_chg_yst <- today %>%
  left_join(new_yesterday, by = "SNo") %>%
  mutate(today = as.numeric(today),
         new_yesterday = as.numeric(new_yesterday),
         delta = as.character(abs(today - new_yesterday)),
         sign = ifelse(today >= new_yesterday, "+", "-"),
         'Change Since Yesterday' = paste0(sign, delta)
  )

new_chg_yst[3,4] <- paste0(round(as.double(new_chg_yst[1,2])/as.double(new_chg_yst[2,2])*100,2), "%")

new_ovl_sum <- new_chg_yst %>%
  select(-c(SNo, new_yesterday)) %>%
  bind_cols(tbl_summary) %>%
  rename(Measure = "Overall Summary",
         Total = today,
         ChangeDirection =sign,
         Change = delta
  ) %>%
  mutate(DateUpdated = Sys.Date() - 1) %>%
  select(Measure, Total, ChangeDirection, Change, DateUpdated)

new_summary <- new_ovl_sum %>%
  mutate(ReportTime = as.character(Sys.time()),
         SNo = seq.int(nrow(new_ovl_sum)))

DBI::dbWriteTable(con, SQL("DPH_COVID_IMPORT.dbo.ODP_TESTTODAY"), new_summary, append = FALSE, overwrite = TRUE)

odbc::dbDisconnect(con)
