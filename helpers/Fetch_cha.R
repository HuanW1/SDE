if(!dir.exists("L:/")) message("You need to have L drive mapped")

suppressPackageStartupMessages(library("tidyverse", 
                                       lib.loc = "L:/newlib", 
                                       quietly = TRUE, 
                                       warn.conflicts = FALSE))
library("lubridate", 
        lib.loc = "L:/newlib", 
        quietly = TRUE, 
        warn.conflicts = FALSE)

# step 1 get the most recent CHA fast and accurate



# a. what are my choices
possibilities <-
  list.files(path = "L:/daily_reporting_figures_rdp/CHA_data_here",
             pattern = ".csv",
             full.names = TRUE)

# b. pick the newest
which_one <- 
  read_csv(possibilities)

# c. make it useful
longcha <- which_one %>% 
  filter(Type == "Admit" & State == "TOTAL") %>% 
  select(-c(Change, County, State)) %>% 
  pivot_longer(-Type, names_to = "admit_date", values_to = "admissions") %>% 
  select(-Type) %>% 
  mutate(admit_date = mdy(admit_date))


cha_c <- which_one %>% 
  filter(Type == "Admit") %>% 
  select(-Type) %>% 
  rename(NAME=County )
cols <- ncol(cha_c)
cha_c <- cha_c %>% 
  rename( today= cols-1,
          yesterday = cols-2) %>% 
  mutate(sign = ifelse(
    Change>=0,
    "+",
    "-"
  ),
  Change = abs(Change)
  )
num_groups <- c("None", "1 to 5", "6 to 10", "11 to 25", "26 to 50", "51 to 100", "101 to 200", "201 to 500", "501 to 1000", "1001 to 5000")
cha_c <- cha_c %>% 
  mutate(
    ngrp = cut(today, breaks = c(-Inf,0,5,10,25,50,100,200,500,1000, Inf), labels = num_groups )
  )


# d. declare it so and erase all evidence
message("You're done!")
rm(possibilities, which_one, num_groups,cols)