#creation of cong setting depandancies and sending on up to sql
library(tidyverse)#, lib.loc = .libPaths()[2])
library(DBI)
library(odbc)
library(readxl)
con <- DBI::dbConnect(odbc::odbc(), "epicenter")

#reading in the town codes csv that the report uses as well as cong setting folks
town_codes <- read_csv("L:/daily_reporting_figures_rdp/gary_csv/town_codes/Town_ID.csv") %>% 
  select(c(ANPSADPI, TOWNNO, COUSUBFP, TOWNNO_C, TOWN_UC, TOWN_LC))

#writing the dependancy table
#DBI::dbWriteTable(conn = con, value = town_codes, name = SQL("DPH_COVID_IMPORT.dbo.RPT_TOWN_CODES"))

#reading in boros list
boros_list <- read_csv("C:\\Users\\senetckya\\Downloads\\boros_list.csv")
#DBI::dbWriteTable(conn = con, value = boros_list, name = SQL("DPH_COVID_IMPORT.dbo.CONG_BOROS_LIST"))



addr_nursing <- as_tibble(read_excel("Facility List CTEDSS Entry with Event IDs Dec2020.xlsx",sheet="NursHome_AL_RCF"))
addr_prisons <- as_tibble(read_excel("Facility List CTEDSS Entry with Event IDs Dec2020.xlsx",sheet="Correctional Facilities")) %>% 
  filter(!is.na(City)) %>% 
  mutate(`Level of Care` = "DOC")

# DBI::dbWriteTable(conn = con, value = addr_nursing, name = SQL("DPH_COVID_IMPORT.dbo.CONG_NURSING_FACILITIES"), overwrite = TRUE)
# DBI::dbWriteTable(conn = con, value = addr_prisons, name = SQL("DPH_COVID_IMPORT.dbo.CONG_PRISON_FACILITIES"), overwrite = TRUE)

cong_date <- tibble("DateRan" = ymd("2021-02-02"))   #setting first sql flag for cong running
DBI::dbWriteTable(conn = con, value = cong_date, name = SQL("DPH_COVID_IMPORT.dbo.CONG_DATERAN"), overwrite = TRUE)

odbc::dbDisconnect(con)
