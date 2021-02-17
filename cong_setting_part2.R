if(!dir.exists("L:/")) message("You need to have L drive mapped")
.libPaths(c("L:/library", .libPaths()))

####0 load packages and create connection ####
library(stringr)			# str_trim
library(stringdist)		# amatch, stringdist
library(readxl)			# read_excel
library(mgsub)			# mgsub
library(readr)
library(data.table)
require(tidyverse)
require(odbc)
require(lubridate)
require(DBI)
con <- DBI::dbConnect(odbc::odbc(), "epicenter")

####1 declare data file for reading ####
if(nrow(maybecong)<1){
  stop("No data to review.")
}
read_file <- maybecong

####2 declare dependency files containing the official lists of ct towns and boroughs ####
statement <- paste0("SELECT * FROM [DPH_COVID_IMPORT].[dbo].[CONG_BOROS_LIST]")
boros_list <-  DBI::dbGetQuery(conn = con , statement = statement)

statement <- paste0("SELECT * FROM [DPH_COVID_IMPORT].[dbo].[RPT_TOWN_CODES]")
city_file <-  DBI::dbGetQuery(conn = con , statement = statement)

statement <- paste0("SELECT * FROM [DPH_COVID_IMPORT].[dbo].[RPT_MYSTIC]")
mystic <-  DBI::dbGetQuery(conn = con , statement = statement)

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 3. Cong Lookup data set construction ####
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#can almost certainly just save the final product as a lookup in the future when we're sure we have the lists final and clean and no more additions

#read in lookup tables
statement <- paste0("SELECT * FROM [DPH_COVID_IMPORT].[dbo].[CONG_NURSING_FACILITIES]")
addr_nursing <-  DBI::dbGetQuery(conn = con , statement = statement)

statement <- paste0("SELECT * FROM [DPH_COVID_IMPORT].[dbo].[CONG_PRISON_FACILITIES]")
addr_prisons <-  DBI::dbGetQuery(conn = con , statement = statement)
#list_addr <- c(addr_nursing$Address,addr_prisons$Address)

#CT boroughs
ct_boros <- city_file %>% 
  select(TOWN_LC) %>% 
  rename(Town = TOWN_LC) %>% 
  mutate(Boro = Town) %>% 
  bind_rows(rev(boros_list)) %>% 
  unique()

#prisons
prisons <- addr_prisons %>% 
  left_join(ct_boros, by = c("City" = "Boro")) %>% 
  select(-c(City, `CTEDSS Entry Name`)) %>% 
  rename(City = Town,
         Level_of_Care = `Level of Care`,
         Street = Address,
         Name = `Facility Name`
         ) %>% 
  select(c(City, Street, Name, Level_of_Care))


#nursing facilities
nursing <- addr_nursing %>% 
  mutate(`Facility City` = str_to_title(`Facility City`),
         `Facility City` =  str_trim(str_remove(`Facility City`, "\\(.*")),
         Address = str_to_title(Address)
         ) %>% 
  left_join(ct_boros, by = c("Facility City" = "Boro")) %>% 
  select(-c(`Facility City`, `CTEDSS Entry Name`)) %>% 
  rename(City = Town,
         Level_of_Care = `Level of Care`,
         Street = Address,
         Name = `Facility Name`
  ) %>% 
  select(c(City, Street, Name, Level_of_Care))

#subset dataset for mystic items
mystic_inds=which(nursing$City=="Mystic")

#obtain addresses within mystic
mystic_addr=nursing$Street[mystic_inds]

#remove street number
mystic_streets=gsub("^\\S+\\s+","",mystic_addr)

#remove apartment number and unit number
mystic_streets=gsub("Apt\\s*\\d*","",mystic_streets)
mystic_streets=gsub("Unit\\s*\\d*","",mystic_streets)

#elongate format end-of-address short-hands
last_words=word(mystic_streets,-1)
short_words=c("Ln","Dr","St","Rd","Ave","Ct")
long_words=c("Lane","Drive","Street","Road","Avenue","Court")
repl_words=match(last_words,short_words)
last_words[which(!is.na(repl_words))]=long_words[repl_words[which(!is.na(repl_words))]]
mystic_noLast=gsub("\\s*\\w*$", "", mystic_streets)
mystic_streets=paste(mystic_noLast,last_words)

#recondition capitalization
mystic_streets=str_to_title(mystic_streets)

#determine finally as groton versus stonington
match_inds=amatch(mystic_streets,mystic$Street,maxDist=10)
eastwest=mystic$eastwest[match_inds]
mystic_final=rep("Mystic",length(eastwest))
mystic_final[which(eastwest=="east")]="Stonington"
mystic_final[which(eastwest=="west")]="Groton"
nursing$City[mystic_inds]=mystic_final

# clear garbage
rm(mystic,mystic_inds,mystic_addr,last_words,short_words,long_words,repl_words)
rm(mystic_noLast,mystic_streets,match_inds,eastwest,mystic_final)

nursing <- nursing %>% 
  mutate(
    Level_of_Care = if_else(Level_of_Care == "Residential Care Facilities", "LTCF", Level_of_Care),
    Level_of_Care = if_else(Level_of_Care == "Nursing Home", "LTCF", Level_of_Care),
    Level_of_Care = if_else(Level_of_Care == "Assisted Living","ALF", Level_of_Care),
    Level_of_Care = if_else(Level_of_Care == "Senior Independent Living","ALF", Level_of_Care),
    Level_of_Care = if_else(Level_of_Care == "MRC/ALSA","ALF", Level_of_Care)
  )

cong <- bind_rows(nursing, prisons)
#####  probably just save? send up to SQL  

#extract data from file
data <- read_file

# clear garbage
rm(city_file,read_file,addr_nursing,addr_prisons, prisons, nursing,statement, ct_boros, boros_list)

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 4. dataset conditioning ####
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### string operations to facilitate text-comparisons
data <- data %>% 
  rename(Street = street, 
         City = city) %>% 
  mutate(City = str_to_lower(City),
         Street = str_to_lower(Street)
         )
cong <- cong %>% 
  mutate(City = str_to_lower(City),
         Street = str_to_lower(Street)
  ) %>% 
  unique()

#~ 2b. homogenize street designations
desigs_long=c("avenue","road","lane","street","drive",
              "court","place","plaza","square","circle",
              "boulevard","terrace","parkway","turnpike")
desigs_short=c("ave","rd","ln","st","dr","ct","pl","plz",
               "sq","cir","blvd","ter","pkwy","tpke")
data$Street=mgsub(data$Street,paste("\\s+",desigs_long,sep=""),paste(" ",desigs_short,sep=""))
cong$Street=mgsub(cong$Street,paste("\\s+",desigs_long,sep=""),paste(" ",desigs_short,sep=""))

#remove content after street designations
patt=paste("(",desigs_short,").*")
repl=rep("\\1",length(patt))
data$Street=mgsub(data$Street,patt,repl)
cong$Street=mgsub(cong$Street,patt,repl)

#miscellaneous formatting
# remove commas, and shrink multi-spaces
errd_patterns=c(", ","\\s\\s+")
#repl_strings=c(""," ")
repl_strings=c(" "," ")
data$Street=mgsub(data$Street,errd_patterns,repl_strings)
cong$Street=mgsub(cong$Street,errd_patterns,repl_strings)
# eliminate bad last characters
last_chars=substr(data$Street,nchar(data$Street),nchar(data$Street))
edit_locns=grep("[^a-z0-9]",last_chars)
data$Street[edit_locns]=substr(data$Street[edit_locns],1,nchar(data$Street[edit_locns])-1)
last_chars=substr(cong$Street,nchar(cong$Street),nchar(cong$Street))
edit_locns=grep("[^a-z0-9]",last_chars)
cong$Street[edit_locns]=substr(cong$Street[edit_locns],1,nchar(cong$Street[edit_locns])-1)
# trim
data$Street=str_trim(data$Street)
cong$Street=str_trim(cong$Street)

# clear garbage
rm(desigs_long,desigs_short,patt,repl,errd_patterns,repl_strings,last_chars,edit_locns)



### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 5. matching ####
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### match each address to its nearest match within the congregate
### settings list, and also some match-meta-information

#~ extract unique towns in user data for iterating matches
unique_cities=unique(data$City)

#~ initialize match information
data$match_city=NA
data$match_street=NA
data$match_dist=-1
data$match_name=NA
data$match_LOF=NA

#~ match within batches by city
# for each city in the datasets
for (i in 1:length(unique_cities)){

	# extract addresses from both the data and the list
	data_inds=which(data$City==unique_cities[i])
	data_streets=data$Street[data_inds]
	cong_inds=which(cong$City==unique_cities[i])
	cong_streets=cong$Street[cong_inds]
	cong_facname=cong$Name[cong_inds]
	cong_LOC=cong$Level_of_Care[cong_inds]

	# match raw addresses to government addresses
	match_inds=amatch(data_streets,cong_streets,maxDist=100)

	# merge results for this city
	city_results <- as_tibble(cbind(data_streets,unique_cities[i],cong_streets[match_inds]),.name_repair = "unique") %>% 
	  rename(match_city = ...2,
	         match_street = ...3
	         )%>% 
	  mutate(dist_street = stringdist(data_streets, match_street))
	
	# note the string distance
	#city_results=cbind(city_results,stringdist(city_results[,1],city_results[,3]))

 
	# populate match information
	data$match_city[data_inds]=city_results$match_city
	data$match_street[data_inds]=city_results$match_street
	data$match_name[data_inds]=cong_facname[match_inds]
	data$match_dist[data_inds]=city_results$dist_street
	data$match_LOF[data_inds]=cong_LOC[match_inds]
}

#~ flag data that does not match to a town with a congregate setting
data_noCong=setdiff(1:nrow(data),which(data$City %in% unique(cong$City)))
data$TownFlag=TRUE
data$TownFlag[data_noCong]=FALSE

#~ determine distance on street numbers
# distance ranges 0 to infinity; smaller value = increasingly likely match
raw_number=suppressWarnings(as.numeric(word(data$Street,1)))
match_number=suppressWarnings(as.numeric(word(data$match_street,1)))
data$dist_StreetNumber=abs(raw_number-match_number)

#~ determine distance on street names
# distance ranges 0 to 1; smaller value = increasingly likely match
raw_name=word(data$Street,2,-1)
match_name=word(data$match_street,2,-1)
data$dist_Characters=stringdist(raw_name,match_name)
data$dist_StreetName=data$dist_Characters/apply(cbind(nchar(raw_name),nchar(match_name)),1,max)
data$dist_StreetName=round(data$dist_StreetName,3)

# clear garbage
rm(unique_cities,i,data_inds,data_streets,cong_inds,cong_streets,cong_facname)
rm(match_inds,city_results,data_noCong,raw_number,match_number,raw_name,match_name)



### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 6. decisioning ####
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### post-match analytics to support decision-making

#~ threshold difference in house-number that should be flagged as possible match (suggest: 6)
maybe_diff=6

#~ threshold difference in streetname that should be flagged as possible match (suggest: 0.1)
maybe_dist=0.1

#~ threshold difference in streetname characters that should be flagged as possible match (suggest: 1)
maybe_char=1

#~ determine if house is on correct side of street (even/odd)
same_side=which(data$dist_StreetNumber%%2==0)

#~ maybe-designation for same side + close in number + close in street
maybe_diffs=intersect(which(data$dist_StreetNumber<=maybe_diff),same_side)
maybe_chars=which(data$dist_Characters<=maybe_char)
maybe_dists=which(data$dist_StreetName<=maybe_dist)
maybe_dists=intersect(maybe_diffs,maybe_dists)
maybe_chars=intersect(maybe_diffs,maybe_chars)
maybe_inds=unique(c(maybe_dists,maybe_chars))

#~ exact matches are firm Yes; out-of-towners are firm No
yes_inds=intersect(which(data$dist_StreetNumber==0),which(data$dist_StreetName==0))
#~ definite congregate settings requires exact matches on street name and number
no_inds=which(!data$TownFlag)

#~ decisioning
data$disposition="Unlikely"
data$disposition[maybe_inds]="Maybe"
data$disposition[yes_inds]="Yes"
data$disposition[no_inds]="No"

# clear garbage
rm(maybe_diff,maybe_dist,same_side,maybe_diffs,maybe_dists,maybe_inds,yes_inds)#maybecong,

#~ condition disposition as factor
data$disposition=factor(data$disposition,levels=c("Yes","Maybe","Unlikely","No"))
data <- data %>% 
  select(eventid, fname, lname, age, dob, gender, race, hisp, Street, match_street, City, match_city,geo_cname, match_name,match_LOF, geo_lof, geo_license, match_dist, dist_StreetNumber, dist_StreetName, dist_Characters, intoms, disposition) %>% 
  mutate(
    KEEP = NA,
    #KEEP = ifelse(
    #intoms == 1 & disposition == "Yes", 1, KEEP
  #),
  geo_cname = str_to_title(geo_cname)
         )

odbc::dbDisconnect(con)
source("cong_setting_part3.R")