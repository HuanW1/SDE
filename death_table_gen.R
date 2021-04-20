
# Grab the current report cases file
source("helpers/Fetch_cases_wphi.R")


# Show the number of rows where covid_death == 'yes'
 # which is what we put in the report
cases_wphi %>% filter(covid_death == 'YES') %>% nrow()

# mafe a df
dfdeath <- cases_wphi %>% filter(covid_death == 'YES')

table(dfdeath$covid_death)

path <- paste0("L:/CELR/", Sys.Date())
dir.create(path)

#dir.create(path) : 'L:\CELR\2021-03-26'
data.table::fwrite(x = dfdeath, paste0(path,"/", Sys.Date(),"_death.csv"))

read_csv("L:/CELR/2021-04-19") %>% nrow()

