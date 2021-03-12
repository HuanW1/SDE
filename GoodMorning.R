if(!dir.exists("L:/")) message("You need to have L drive mapped")
suppressPackageStartupMessages(library("tidyverse", 
                                       lib.loc = "L:/newlib", 
                                       quietly = TRUE, 
                                       warn.conflicts = FALSE,
                                       logical.return = TRUE))
library("lubridate", 
        lib.loc = "L:/newlib", 
        quietly = TRUE, 
        warn.conflicts = FALSE,
        logical.return = TRUE)
library(flextable, 
        lib.loc = "L:/newlib", 
        quietly = TRUE, 
        warn.conflicts = FALSE,
        logical.return = TRUE)

possibilities <-
  list.files(path = "l:/",
             pattern = "draft_table",
             full.names = TRUE)

which_one <-
  as.data.frame(possibilities) %>%
  cbind(as.data.frame(str_split(possibilities,
                                pattern = "_|\\.",
                                simplify = TRUE))) %>%
  select(-V1, -V2, -V4, justdate = V3) %>%
  mutate(justdate = as_datetime(justdate)) %>%
  filter(justdate == max(justdate)) %>%
  pull(possibilities)

load(which_one)
mock_table
