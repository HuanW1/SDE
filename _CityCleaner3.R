################################################################
### _CityCleaner.R
###
### script to perform cleaning specific to city and county.
### accessory file to FileCleaner.R
###
### version history
### 1. 12/19/20: wininger - beta version
### 2. 12/22/20: wininger - edits to match_info
### 3. 12/22/20: wininger - edits to out-of-state
### 4. 02/05/2021: kleppinger - edits for the new data extract from Tom Bavone
################################################################

# load libraries
library(stringr)			# str_to_title
library(zipcodeR)		# search_state
library(stringdist)		# amatch and stringdist


# declare all files up-top
city_file="/Users/KleppingerA/Documents/Nancy Barrett/Town_ID.csv"
county_file="/Users/KleppingerA/Documents/Nancy Barrett/City County Mapping.csv"
boro_file="/Users/KleppingerA/Documents/Nancy Barrett/boros_list.csv"
mystic_file="/Users/KleppingerA/Documents/Nancy Barrett/_mystic.csv"

### find the missing counties - created AK on 2/5/2021 ###
# subset by rows of missing counties
#blank_data_counties<- data %>% subset(is.na(Patient_county))

# change caps case to title for data
#blank_data_counties$Patient_city = str_to_title(blank_data_counties$Patient_city)
data$Patient_city = str_to_title(data$Patient_city)

# extract hospital codes/key from hospital reference file
county_table=read.csv(county_file)

# subset to code match
county_list=county_table$CITY

# subset to raw hosp
county_raw=data$Patient_city

# match raw cities to approved list
match_inds=amatch(county_raw,county_list,maxDist=6)


# find empty spec_col_dates as empty strings or nulls
empty_county1=which(data$Patient_county=="")
empty_county2=which(is.na(data$Patient_county))
empty_county3=c(empty_county1,empty_county2)

# extract keys
key_match=county_table$COUNTY[match_inds]

# overwrite as county if blank
data$Patient_county[empty_county3]=key_match[empty_county3]
data$Patient_county=key_match

#remove County from county
data$Patient_county=gsub(" County","",data$Patient_county)

## add the word County to the end of the Counties
# paste 'County' as last word
data$Patient_county=paste0(data$Patient_county," County")

#remove County from county
data$Patient_county=gsub("NA County","",data$Patient_county)

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 0. sub-function declarations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# sub-function to match a vector of cities against a list of cities
fn_cityMatch=function(cv,cl){
	# cv = city vector; vector of cities found in data
	# cl = city list; list of acceptable cities
	# 1. match the city vector to the city list (mi=match indexes)
	mi=amatch(cv,cl,maxDist=10)
	# 2. obtain matched cities from list (mc=matched cities)
	mc=cl[mi]
	# 3. compute a score to aid in manual review
	# 3a. raw distance; number of characters deviated
	rd=stringdist(cv,mc)
	# 3b. number of characters in both the city vector and the matched cities
	nc=cbind(nchar(cv),nchar(mc))
	# 3c. denominator taking the maximum charater length in each nc-pair
	d=apply(nc,1,max,na.rm=TRUE)	
	# 3d. similarity score: 1=perfect similarity; 0=no homology whatsoever
	ss=round(1-(rd/d),3)
	# 4. output matches and their scores (op=output)
	op=data.frame(mc,ss)
	names(op)=c("match","score")
	return(op)
}

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 1. conjure supporting information from file ~~~~~~~~~~~~~~~~
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~ 1a. extract 169 connecticut cities from file
ct_cities=read.csv(city_file)
ct_cities=ct_cities$TOWN_LC
# 169 connecticut towns

#~ 1b. extract city-county crosswalk from file
ct_counties=read.csv(county_file)
names(ct_counties)=str_to_title(names(ct_counties))
# 169 connecticut towns and counties

#~ 1c. extract boro-city crosswalk from file
ct_boros=read.csv(boro_file)
# 100+ connecticut boros and towns
names(ct_boros)=c("Boro","Town")

#~ 1d. extract a zipcode crosswalk
zips=subset(search_state("CT"),select=c("zipcode","major_city"))
names(zips)=c("zipcode","zipcity")
# 438 connecticut zipcodes and boros

#~ 1e. match zip-cities to recognized townships
match_boros=c(ct_boros$Boro,ct_cities)
match_cities=c(ct_boros$Town,ct_cities)
zips$city=match_cities[match(zips$zipcity,match_boros)]
# 438 connecticut zipcodes and boros and towns

#~ 1f. enforce all strings to be as first-letter-caps
ct_cities=str_to_title(ct_cities)
ct_counties=apply(ct_counties,2,str_to_title)
ct_counties=data.frame(ct_counties)
ct_boros=apply(ct_boros,2,str_to_title)
ct_boros=data.frame(ct_boros)
names(ct_boros)=c("Boro", "Town")

#~ 1g. determine the similarity of each next-best city-match
#  initialize a square similarity matrix with negative 1 
simm_matx=matrix(-1,nrow=length(ct_cities),ncol=length(ct_cities))
#  for each connecticut city
for (i in 1:length(ct_cities)){
	#  for each-other connecticut city
	for (j in 1:length(ct_cities)){
		#  compute the distance between the two strings
		dist=stringdist(ct_cities[i],ct_cities[j])
		#  compute the denominator according to the longer string
		denom=max(c(nchar(ct_cities[i]),nchar(ct_cities[j])))
		#  similarity as 1-minus normalized distance
		simm_matx[i,j]=round(1-(dist/denom),3)
	}
}

#  ignore self-similarities
simm_matx[which(simm_matx==1)]=NA
#  next-best similarity score for each town
thresh_vals=apply(simm_matx,1,max,na.rm=TRUE)
#  names for clarity
names(thresh_vals)=ct_cities


# clear garbage
rm(match_boros,match_cities,i,j,dist,denom,simm_matx)

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2. preliminary operations on raw dataset ~~~~~~~~~~~~~~~~~~~
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~ 2a. condition city as first-letter-caps
data$Patient_city=str_to_title(data$Patient_city)

#~ 2b. take note of which cities are empty or unknown
empty_inds=c(which(is.na(data$Patient_city)),which(data$Patient_city==""),which(data$Patient_city=="Unknown"),which(data$Patient_city=="Not_available"))
empty_inds=c(which(is.na(data$Patient_city)),which(data$Patient_city==""),grep("Unknown",data$Patient_city,ignore.case=TRUE),grep("avail",data$Patient_city,ignore.case=TRUE))

#~ 2c. copy over City to a new column (preserving raw data)
data$City_Match=data$Patient_city

# clear garbage
rm()

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 3. tier 1: cities correctly posed in raw dataset ~~~~~~~~~~~
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~ 3a. match the entire dataset to the list of 169 towns
match_info=fn_cityMatch(data$Patient_city,ct_cities)
# n x 2 data-frame: city-match and match-score

#~ 3b. append city information
match_info=cbind(data$Patient_city,match_info)
names(match_info)=c("Patient_city","city_match","city_score")
# n x 3 data-frame: raw city appended at left

#~ 3c. enforce empty city as no-match
match_info$city_match[empty_inds]=""

# clear garbage
rm()

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 4. tier 2: cities not matched exactly: look for boros ~~~~~~
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~ 4a. find match failures from tier 1
scores_vect=match_info[,grep("score",names(match_info),ignore.case=TRUE)]
fail_inds=which(scores_vect<1)

#~ 4b. initialize vectors for tracking tier 2 in the match_info matrix
match_info$boro=""
match_info$boro_match=""
match_info$boro_score=0	

#~ 4c. match all failures to the list of 100+ boros
mi=fn_cityMatch(data$Patient_city[fail_inds],ct_boros$Boro)

#~ 4d. take note of all matched boros
match_info$boro[fail_inds]=mi$match

#~ 4e. take note of all match simiarity scores
match_info$boro_score[fail_inds]=mi$score

#~ 4f. match the boros to their corresponding towns                                    
match_info$boro_match[fail_inds]=ct_boros$Town[match(mi$match,ct_boros$Boro)]

#~ 4g. enforce empty city as no-match
match_info$boro_match[empty_inds]=""
match_info$boro[empty_inds]=""

# clear garbage
rm(scores_vect,fail_inds,mi)

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 5. tier 3: cities/boros not matched: assist via zip-code ~~~
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~ 5a. find match failures from tier 2
scores_matx=match_info[,grep("score",names(match_info),ignore.case=TRUE)]
fail_inds=which(apply(scores_matx,1,max)<1)

#~ 5b. initialize vectors for tracking tier 3 in the match_info matrix
match_info$zip_code=""
match_info$zipCity_match=""
match_info$zipCity_score=0

#~ 5c. populate zip code for match-failures
match_info$zip_code[fail_inds]=data$Patient_zip_code[fail_inds]

#~ 5d. enforce zip-code as 5-digits
match_info$zip_code=substr(match_info$zip_code,1,5)

#~ 5e. match zip-codes to connecticut zip-code database
zips_inds=match(match_info$zip_code[fail_inds],zips$zipcode)

#~ 5f. clean unmatched zip-cdoes
elim_inds=which(is.na(zips_inds))
keep_inds=setdiff(1:length(zips_inds),elim_inds)
fail_inds=fail_inds[keep_inds]
zips_inds=zips_inds[keep_inds]

#~ 5g. cross-walk zipcode to city
zips_city=zips$city[zips_inds]

#~ 5h. if city for this zip-code matches the city guessed in tier 1: retain
vrfy_inds=which(match_info$city_match[fail_inds]==zips_city)
match_info$zipCity_match[fail_inds[vrfy_inds]]=match_info$city_match[fail_inds[vrfy_inds]]
match_info$zipCity_score[fail_inds[vrfy_inds]]=1

#~ 5h. if city for this zip-code matches the boro-city guessed in tier 2: retain
match_info$zipBoro_match=""
match_info$zipBoro_score=0
vrfy_inds=which(match_info$boro_match[fail_inds]==zips_city)
match_info$zipBoro_match[fail_inds[vrfy_inds]]=match_info$boro_match[fail_inds[vrfy_inds]]
match_info$zipBoro_score[fail_inds[vrfy_inds]]=1

#~ 5i. over-write zipBoro scores when applicable, to revert to tier 1
match_info$zipBoro_match[which(match_info$zipCity_score==1)]=""
match_info$zipBoro_score[which(match_info$zipCity_score==1)]=0

# clear garbage
rm(scores_matx,elim_inds,keep_inds,fail_inds,zips_inds,zips_city,vrfy_inds)


### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 6. tier 4: assisted zip failures: match by zip alone ~~~~~~~
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~ 6a. find match failures from tier 3
scores_matx=match_info[,grep("score",names(match_info),ignore.case=TRUE)]
fail_inds=which(apply(scores_matx,1,max)<1)

#~ 6b. match zip-codes to connecticut zip code bank
match_inds=match(match_info$zip_code[fail_inds],zips$zipcode)

#~ 6c. clean unmatched zip-codes
elim_inds=which(is.na(match_inds))
keep_inds=setdiff(1:length(match_inds),elim_inds)
match_inds=match_inds[keep_inds]
fail_inds=fail_inds[keep_inds]

#~ 6d. retain any tier-4 match results
match_info$zipOnly_match=""
match_info$zipOnly_match[fail_inds]=zips$city[match_inds]
match_info$zipOnly_score=0
match_info$zipOnly_score[fail_inds]=1

# clear garbage
rm(scores_matx,fail_inds,match_inds,elim_inds,keep_inds)



### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 7. tier 5: supra-threshold string matches ~~~~~~~~~~~~~~~~~~
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~ 7a. find match failures from tier 4
scores_matx=match_info[,grep("score",names(match_info),ignore.case=TRUE)]
fail_inds=which(apply(scores_matx,1,max)<1)

#~ 7b. match the best-guess city to the 169 connecticut towns
match_inds=match(match_info$city_match[fail_inds],names(thresh_vals))

#~ 7c. clean unmatched city names
elim_inds=which(is.na(match_inds))
keep_inds=setdiff(1:length(match_inds),elim_inds)
match_inds=match_inds[keep_inds]
fail_inds=fail_inds[keep_inds]

#~ 7d. retain matches where nearest-neighbor threshold is exceeded
thresh_score=as.numeric(thresh_vals[match_inds])
match_inds=which(match_info$city_score[fail_inds]>thresh_score)

#~ 7e. retain any tier-5 match results
match_info$thresh_match=""
match_info$thresh_match[fail_inds[match_inds]]=match_info$city_match[fail_inds[match_inds]]
match_info$thresh_score=0

match_info$thresh_score[fail_inds[match_inds]]=1

# clear garbage
rm(scores_matx,fail_inds,elim_inds,keep_inds,match_inds,thresh_score)

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 8. score-keeping ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~ 8a. extract a matrix of just the scoring flags 
score_cols=grep("score",names(match_info),ignore.case=TRUE)
scores_matx=match_info[,score_cols]

#~ 8b. the city quality tier correponds to the latest step
data$City_Tier=apply(scores_matx,1,which.max)

#~ 8c. if city never matched: Tier is -1
data$City_Tier[which(apply(scores_matx,1,max)==0)]=-1

#~ 8d. take note of the best-matched city (working up through tiers)
data$City_Match=data$Patient_city
for (i in max(data$City_Tier):1){
	data$City_Match[which(data$City_Tier==i)]=match_info[which(data$City_Tier==i),(score_cols[i]-1)]		
}


#~ 8e. enforce that persistent match failures retain original City
data$City_Match[which(data$City_Tier==-1)]=data$City[which(data$City_Tier==-1)]

# clear garbage
rm(score_cols,scores_matx,i)


### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 9. overlay out-of-state ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~ 9a. obtain list of connecticut counties (and blank)
ct_list=c(unique(paste(ct_counties$County,"County")),"")

#~ 9b. flag any counties as explicitly out-of-state
out_county=setdiff(1:nrow(data),which(data$Patient_county %in% ct_list))

#~ 9c. flag any zipcodes as explicitly out-of-state
#!# out_zip=setdiff(1:nrow(data),which(data$zip_code %in% zips$zipcode))
#out_zip=setdiff(1:nrow(data),which(substr(data$zip_code,1,5) %in% zips$zipcode))
out_zip=setdiff(1:nrow(data),which(substr(data$Patient_zip_code,1,2)=="06"))
out_zip=setdiff(out_zip,which(data$Patient_zip_code==""))

#~ 9d. flag any states as explicity not-connecticut
out_state=setdiff(1:nrow(data),which(data$Patient_state %in% c("CT","Ct","ct","Connecticut","Conn","Conn.","")))

#~ 9e. flag any cities not well-matched to a city or boro
out_city=which(data$City_Tier<1)

#~ 9f. consolidate flags
out_flags=matrix(FALSE,nrow=nrow(data),ncol=4)
out_flags[out_county,1]=TRUE
out_flags[out_zip,2]=TRUE
out_flags[out_state,3]=TRUE
out_flags[out_city,4]=TRUE

#~ 9g. compute out-of-state-risk-score
out_risk=apply(out_flags,1,mean)

#~ 9e. flag out of state
data$Out="FALSE"
data$Out[out_risk>0]="MIXED"
data$Out[out_risk==1]="TRUE"

#~ 9f. enforce that out-of-state records retain original City
data$City_Match[which(out_risk>0)]=data$Patient_city[which(out_risk>0)]

# clear garbage
rm(ct_list,out_county,out_zip,out_state,out_city,out_risk)

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 10. re-map addresses in mystic ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~ 10a. read in data-sheet on streets in mystic
mystic=read.csv(mystic_file)

#~ 10b. subset dataset for mystic items
mystic_inds=which(data$City_Match=="Mystic")

#~ 10c. obtain addresses within mystic
mystic_addr=data$Street[mystic_inds]

#~ 10d. format capitalization
mystic_addr=str_to_title(mystic_addr)

#~ 10e. remove street number
mystic_streets=gsub("^\\S+\\s+","",mystic_addr)

#~ 10f. remove apartment number and unit number
mystic_streets=gsub("Apt\\s*\\d*","",mystic_streets)
mystic_streets=gsub("Unit\\s*\\d*","",mystic_streets)

#~ 10g. elongate format end-of-address short-hands
last_words=word(mystic_streets,-1)
short_words=c("Ln","Dr","St","Rd","Ave","Ct")
long_words=c("Lane","Drive","Street","Road","Avenue","Court")
repl_words=match(last_words,short_words)
last_words[which(!is.na(repl_words))]=long_words[repl_words[which(!is.na(repl_words))]]
mystic_noLast=gsub("\\s*\\w*$", "", mystic_streets)
mystic_streets=paste(mystic_noLast,last_words)

#~ 10h. recondition capitalization
mystic_streets=str_to_title(mystic_streets)

#~ 10i. determine finally as groton versus stonington
match_inds=amatch(mystic_streets,mystic$Street,maxDist=10)
eastwest=mystic$eastwest[match_inds]
mystic_final=rep("Mystic",length(eastwest))
mystic_final[which(eastwest=="east")]="Stonington"
mystic_final[which(eastwest=="west")]="Groton"
data$City_Match[mystic_inds]=mystic_final

#~ 10j. for any remaining empty cities: leverage zip-code
repl_inds=intersect(which(data$City_Match==""),which(data$zip_code!=""))
#zips=subset(search_state("CT"),select=c("zipcode","major_city"))
#names(zips)=c("zipcode","zipcity")
repl_city=zips$zipcity[match(data$zip_code[repl_inds],zips$zipcode)]
data$City_Match[repl_inds]=repl_city
repl_inds=intersect(which(is.na(data$City_Match)),which(data$zip_code!=""))
repl_city=zips$city[match(data$zip_code[repl_inds],zips$zipcode)]
data$City_Match[repl_inds]=repl_city

# clear garbage
rm(mystic,mystic_inds,mystic_addr,last_words,short_words,long_words,repl_words)
rm(mystic_noLast,mystic_streets,match_inds,eastwest,mystic_final)


### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 11. finalize county designation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~ 11a. matched towns to county
ct_counties$County=paste(ct_counties$County,"County")
data$County_Match=ct_counties$County[match(data$City_Match,ct_counties$City)]

#~ 11b. enforce that unmatched cities and out-of-towners have county restored
data$County_Match[which(data$City_Tier==-1)]=data$Patient_county[which(data$City_Tier==-1)]
data$County_Match[which(data$Out %in% c("MIXED","TRUE"))]=data$Patient_county[which(data$Out %in% c("MIXED","TRUE"))]

# clear garbage
rm(ct_counties,ct_cities,ct_boros,empty_inds,fn_cityMatch)
#rm(match_info,thresh_vals)


write.table(match_info, "MatchInfo.csv",sep=",",row.names=FALSE,col.names=TRUE)


### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 12. finalize state and zip ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~ 12a. populate confirmed in-state, with no proper state
fill_inds=intersect(which(data$Patient_state!="CT"),which(data$Out==FALSE))
data$Patient_state[fill_inds]="CT"

#~ 12b. populate zip-code for in-state empty
fill_inds=intersect(which(data$Patient_zip_code==""),which(data$Out==FALSE))
match_inds=match(data$City_Match[fill_inds],zips$zipcity)
data$zip_code[fill_inds]=zips$zipcode[match_inds]

# clear garbage
rm(fill_inds,match_inds,zips)

###############################################################
# select only the CT for states, not any out of state labs
###############################################################

# subset to CT states and blanks
sub_data = subset(data,subset=Patient_state=="CT" | Patient_state == "")

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### ############################################################
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~











