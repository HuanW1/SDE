if(!dir.exists("L:/")) message("You need to have L drive mapped")

suppressPackageStartupMessages(library("tidyverse", 
                                       lib.loc = "L:/newlib", 
                                       quietly = TRUE, 
                                       warn.conflicts = FALSE))
library("lubridate", 
        lib.loc = "L:/newlib", 
        quietly = TRUE, 
        warn.conflicts = FALSE)

# step 1 get the most recenet elr_linelist fast and accurate


# a. what are my choices
possibilities <-
  list.files(path = "l:/recent_rdata",
             pattern = "cases_wphi_",
             full.names = TRUE)

# b. pick the newest
which_one <-
  as.data.frame(possibilities) %>%
  cbind(as.data.frame(str_split(possibilities,
                                pattern = "_|\\.",
                                simplify = TRUE))) %>%
  select(-V1, -V2, -V3, justdate = V4) %>%
  mutate(justdate = as_datetime(justdate)) %>%
  filter(justdate == max(justdate)) %>%
  pull(possibilities)

# c. load it with the name elr_linelist
# takes a minute or so

message("This will take about 10 seconds")
load(which_one)
cases_wphi <- case3
rm(case3)

