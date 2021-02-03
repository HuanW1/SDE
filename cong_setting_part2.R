################################################################
# Congregrate_Setting.R
#
# script to flag patients as to their likelihood of habiting a
# congregate setting
#
# release version for ctdph view
# 1. 1/18/21: transmission yale to dph
# 2. 2/3/21 Edited by DPH
#
# notes: 
# -1. this routine expects that all data has been conditioned for city
# -2. will need to reconfigure section 1g to reflect true dataset
################################################################
if(!dir.exists("L:/")) message("You need to have L drive mapped")
.libPaths(c("L:/library", .libPaths()))

# load libraries and connection
library(stringr)			# str_trim
library(stringdist)		# amatch, stringdist
library(readxl)			# read_excel
library(mgsub)			# mgsub
library(readr)
library(data.table)
con <- DBI::dbConnect(odbc::odbc(), "epicenter")

# declare data file for reading
if(exists("check")){
  read_file <- check
} else{
  read_file <- data.table::fread("L:/daily_reporting_figures_rdp/csv/2021-02-03/2021-02-03CONG_past14daysdelta.csv", data.table = FALSE)   
}

# declare files containing the official lists of ct towns and boros
statement <- paste0("SELECT * FROM [DPH_COVID_IMPORT].[dbo].[CONG_BOROS_LIST]")
boros_list <-  DBI::dbGetQuery(conn = con , statement = statement)

statement <- paste0("SELECT * FROM [DPH_COVID_IMPORT].[dbo].[RPT_TOWN_CODES]")
city_file <-  DBI::dbGetQuery(conn = con , statement = statement)

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 1. data set construction ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### creates two data-frames: raw data, and the curated list of 
### congregate settings

#~ 1a. obtain addresses from curated listed of congregate facilities
statement <- paste0("SELECT * FROM [DPH_COVID_IMPORT].[dbo].[CONG_NURSING_FACILITIES]")
addr_nursing <-  DBI::dbGetQuery(conn = con , statement = statement)

statement <- paste0("SELECT * FROM [DPH_COVID_IMPORT].[dbo].[CONG_PRISON_FACILITIES]")
addr_prisons <-  DBI::dbGetQuery(conn = con , statement = statement)
list_addr <- c(addr_nursing$Address,addr_prisons$Address)
rm(statement)

#~ 1b. create a crosswalk between boros and towns
ct_cities=city_file$TOWN_LC
ct_cities=str_to_title(ct_cities)
ct_cities=data.frame(ct_cities,ct_cities)
names(ct_cities)=c("Boro","Town")
ct_boros=rbind(ct_cities,boros_list)

#~ 1c. prisons need city to be split off and mapped from Boro to City
boros_patt=paste(ct_boros$Boro,collapse="|")
prison_cities=str_extract_all(addr_prisons$Address,boros_patt)
prison_cities=paste(prison_cities,sep=", ",collapse="; ")
prison_cities=gsub("c\\(|\\\\|\\\"|\\)|\\(","",prison_cities)
prison_cities=unlist(str_split(prison_cities,";"))
start_locns=regexpr(",.[^,]*$", prison_cities)
start_locns[which(start_locns==-1)]=0
prison_cities=substr(prison_cities,start_locns+1,nchar(prison_cities))
prison_cities=str_trim(prison_cities)
prisons=data.frame(prison_cities)
names(prisons)="City"
match_inds=match(prisons$City,ct_boros$Boro)
prisons$City=ct_boros$Town[match_inds]

#~ 1d. prisons now need address to be cleaned
comma_locns=str_locate(addr_prisons$Address,",")[,1]
prisons$Street=substr(addr_prisons$Address,1,comma_locns-1)

#~ 1e. extract prison name, omit blanks (probably a file error)
prisons$Name=addr_prisons$"Facility Name"
prisons$Level_of_Care=addr_prisons$"Level of Care"
prisons=na.omit(prisons)

#~ 1f. merge nursing homes and prisons into a single congregate list
list_strt=c(addr_nursing$Address,prisons$Street)
list_city=c(addr_nursing$"Facility City",prisons$City)
list_name=c(addr_nursing$"Facility Nickname",prisons$Name)

#categorize level of care
list_LevelofCare=c(addr_nursing$"Level of Care",prisons$"Level_of_Care")
list_LevelofCare=mgsub(list_LevelofCare,"Residential Care Facilities","LTCF")
list_LevelofCare=mgsub(list_LevelofCare,"Nursing Home","LTCF")
list_LevelofCare=mgsub(list_LevelofCare,"Assisted Living","ALF")
list_LevelofCare=mgsub(list_LevelofCare,"Senior Independent Living","ALF")

cong=data.frame(list_strt,list_city,list_name,list_LevelofCare)
names(cong)=c("Street","City","Name","Level of Care")

#~ 1g. extract data from file
data=read.csv(read_file)
data$eventID=1:nrow(data)
data=data[1:100000,]

# clear garbage
rm(city_file,boro_file,read_file,addr_nursing,addr_prisons)
rm(list_addr,ct_cities,ct_boros,boros_patt,prison_cities)
rm(start_locns,prisons,comma_locns,list_strt,list_city,list_name)



### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2. dataset conditioning ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### string operations to facilitate text-comparisons

#~ 2a. condition strings
cong$Street=tolower(cong$Street)
cong$City=tolower(cong$City)
data$City=tolower(data$City)
data$Street=tolower(data$Street)

#~ 2b. homogenize street designations
desigs_long=c("avenue","road","lane","street","drive",
              "court","place","plaza","square","circle",
              "boulevard","terrace","parkway","turnpike")
desigs_short=c("ave","rd","ln","st","dr","ct","pl","plz",
               "sq","cir","blvd","ter","pkwy","tpke")
data$Street=mgsub(data$Street,paste("\\s+",desigs_long,sep=""),paste(" ",desigs_short,sep=""))
cong$Street=mgsub(cong$Street,paste("\\s+",desigs_long,sep=""),paste(" ",desigs_short,sep=""))

#~ 2c. remove content after street designations
patt=paste("(",desigs_short,").*")
repl=rep("\\1",length(patt))
data$Street=mgsub(data$Street,patt,repl)
cong$Street=mgsub(cong$Street,patt,repl)

#~ 2d. miscellaneous formatting
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
### 3. matching ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### match each address to its nearest match within the congregate
### settings list, and also some match-meta-information

#~ 3a. extract unique towns in user data for iterating matches
unique_cities=unique(data$City)

#~ 3b. initialize match information
data$match_city=NA
data$match_street=NA
data$match_dist=-1
data$match_name=NA
data$match_LOF=NA

#~ 3c. match within batches by city
# for each city in the datasets
for (i in 1:length(unique_cities)){

	# extract addresses from both the data and the list
	data_inds=which(data$City==unique_cities[i])
	data_streets=data$Street[data_inds]
	cong_inds=which(cong$City==unique_cities[i])
	cong_streets=cong$Street[cong_inds]
	cong_facname=cong$Name[cong_inds]
	cong_LOC=cong$"Level of Care"[cong_inds]

	# match raw addresses to government addresses
	match_inds=amatch(data_streets,cong_streets,maxDist=100)

	# merge results for this city
	city_results=data.frame(data_streets,unique_cities[i],cong_streets[match_inds])
	# note the string distance
	city_results=cbind(city_results,stringdist(city_results[,1],city_results[,3]))
 
	# rename for clarity
	names(city_results)=c("data_street","match_city","match_street","dist_street")
 
	# populate match information
	data$match_city[data_inds]=city_results$match_city
	data$match_street[data_inds]=city_results$match_street
	data$match_name[data_inds]=cong_facname[match_inds]
	data$match_dist[data_inds]=city_results$dist_street
	data$match_LOF[data_inds]=cong_LOC[match_inds]
}

#~ 3d. flag data that does not match to a town with a congregate setting
data_noCong=setdiff(1:nrow(data),which(data$City %in% unique(cong$City)))
data$TownFlag=TRUE
data$TownFlag[data_noCong]=FALSE

#~ 3e. determine distance on street numbers
# distance ranges 0 to infinity; smaller value = increasingly likely match
raw_number=suppressWarnings(as.numeric(word(data$Street,1)))
match_number=suppressWarnings(as.numeric(word(data$match_street,1)))
data$dist_StreetNumber=abs(raw_number-match_number)

#~ 3f. determine distance on street names
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
### 4. decisioning ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### post-match analytics to support decision-making

#~ 4a. threshold difference in house-number that should be flagged as possible match (suggest: 6)
maybe_diff=6

#~ 4b. threshold difference in streetname that should be flagged as possible match (suggest: 0.1)
maybe_dist=0.1

#~ 4b. threshold difference in streetname characters that should be flagged as possible match (suggest: 1)
maybe_char=1

#~ 4c. determine if house is on correct side of street (even/odd)
same_side=which(data$dist_StreetNumber%%2==0)

#~ 4d. maybe-designation for same side + close in number + close in street
maybe_diffs=intersect(which(data$dist_StreetNumber<=maybe_diff),same_side)
maybe_chars=which(data$dist_Characters<=maybe_char)
maybe_dists=which(data$dist_StreetName<=maybe_dist)
maybe_dists=intersect(maybe_diffs,maybe_dists)
maybe_chars=intersect(maybe_diffs,maybe_chars)
maybe_inds=unique(c(maybe_dists,maybe_chars))

#~ 4e. exact matches are firm Yes; out-of-towners are firm No
yes_inds=intersect(which(data$dist_StreetNumber==0),which(data$dist_StreetName==0))
#~ 4e. definite congregate settings requires exact matches on street name and number
no_inds=which(!data$TownFlag)

#~ 4f. decisioning
data$disposition="Unlikely"
data$disposition[maybe_inds]="Maybe"
data$disposition[yes_inds]="Yes"
data$disposition[no_inds]="No"

# clear garbage
rm(maybe_diff,maybe_dist,same_side,maybe_diffs,maybe_dists,maybe_inds,yes_inds)

#~ reduce and reorder variables to streamline output
data=subset(data,select=c("eventID","Street","City","disposition","match_street","match_city","match_name","match_LOF","match_dist"))

#~ condition disposition as factor
data$disposition=factor(data$disposition,levels=c("Yes","Maybe","Unlikely","No"))


##~ re-ordering for visual proof
data$match_name=substr(data$match_name,1,30)
data[order(data$disposition,data$match_dist),][c(1:500,8800:8850),]


#~ write out analysis file for offline review
write.table(data,"congSetting_review.csv",sep=",",row.names=FALSE,col.names=TRUE)

#toc=Sys.time()
#print(toc-tic)

