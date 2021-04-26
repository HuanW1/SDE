if(!dir.exists("L:/")) message("You need to have L drive mapped")

suppressPackageStartupMessages(library("tidyverse", 
                                       lib.loc = "L:/newlib", 
                                       quietly = TRUE, 
                                       warn.conflicts = FALSE))
library("lubridate", 
        lib.loc = "L:/newlib", 
        quietly = TRUE, 
        warn.conflicts = FALSE)

# a. what are my choices
possibilities <-
  list.files(path = "l:/recent_rdata",
             pattern = "case_",
             full.names = TRUE)

# b. pick the newest
which_one <-
  as.data.frame(possibilities) %>%
  cbind(as.data.frame(str_split(possibilities,
                                pattern = "_|\\.",
                                simplify = TRUE))) %>%
  select(-V1, -V2, justdate = V3, -V4) %>%
  mutate(justdate = as_datetime(justdate)) %>%
  filter(justdate == max(justdate)) %>%
  pull(possibilities)

# c. load it with the name case
# takes a few seconds or so

message("This will take about 15 seconds")
load(which_one)
message("case is loaded")

