if(!dir.exists("L:/")) message("You need to have L drive mapped")

.libPaths(c("L:/newlib", .libPaths()))

library(tidyverse)
library(lubridate)
library(flextable)

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
