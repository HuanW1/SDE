# _FileProcessor.R

### Process labels based on Nancy B's spreadsheet
################################################################

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### PROCESS 0: DATASET PRE-CONDITIONING
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# select the dates you choose to send to CDC
#data <- data %>% filter(data$Specimen.Date > "10/31/2020")

# declare all files up-top
targ_file="/Users/KleppingerA/Documents/Nancy Barrett/headers.xlsx"

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### PROCESS 2: CONDITIONING TEST METHOD
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# declare list of untenable methods
elim_meth = c(
  "SARS-CoV-2 IgG Qual (ABBOTT)",
  "SARS CoV 2 Ab (IgG)",
  "COVID-19 rapid point-of-care (POC) test (antigen test)",
  "SARS-CoV-2 IgM quant",
  "SARS-CoV-2 IgG quant",
  "SARS-CoV-2 IgG+IgM",
  "SARS-CoV-2 IgM (Abbott IA)",
  "SARS CoV 2 Ab IgG blood spot",
  "SARS-CoV-2 IgA units/volume",
  "SARS-CoV-2 IgA",
  "SARS-CoV-2 IgA rapid",
  "SARS-CoV-2 IgM rapid",
  "SARS-CoV-2 IgG rapid",
  "SARS-CoV-2 Ab units/volume",
  "SARS-CoV-2 Ab",
  ""
)

# find untenable methods
elim_meth=which(data$test_name %in% elim_meth)

# find empty/null
elim_null=which(is.na(data$test_name))

# merge all eliminations and remove from dataset
elim_inds=unique(c(elim_meth,elim_null))
# use keep in lieu of elim, in case of no eliminations
keep_inds=setdiff(1:nrow(data),elim_inds)
# subset dataset
#data=data[keep_inds,]
#print(paste(length(elim_inds),"records removed due to untenable test_name method"))

# clear garbage
rm(elim_meth,elim_null,elim_inds,keep_inds)

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### PROCESS 3: REPAIR SPECIMEN COLLECTION DATE
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# find empty spec_col_dates as empty strings or nulls
empty_date1=which(data$spec_coll_date=="")
empty_date2=which(is.na(data$spec_coll_date))
empty_date3=c(empty_date1,empty_date2)
# overwrite as date_test_nameed
data$spec_coll_date[empty_date3]=data$tested_date[empty_date3]


# find empty spec_col_dates as empty strings or nulls
empty_date4=which(data$spec_coll_date=="")
empty_date5=which(is.na(data$spec_coll_date))
empty_date6=c(empty_date4,empty_date5)
# overwrite as date_reported
data$spec_coll_date[empty_date6]=data$result_rpt_date[empty_date6]

# retain a list of empty_dates for revisit in CTEDSS
miss_dates=data$Case.ID[empty_date6]
print(paste(length(miss_dates),"dates repaired"))

# clear garbage
rm(empty_date1,empty_date2,empty_date3,empty_date4,empty_date5,empty_date6,miss_dates)

#delete a variable
table$variablename <- NULL

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### PROCESS 6: LEADING ZERO ONTO ZIP CODE
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### moot: leading zero is a MS Excel issue

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### PROCESS 7: CONDITION CITY
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### sourced to _CityCleaner.R

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### PROCESS 8: FIX ZIP CODE
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### pending: patch in development with molly + mike
###! clarify: should we populate zip-codes by municipality?
### note: not a bijective mapping....

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### PROCESS 9: SUPPLY COUNTY
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### sourced to _CityCleaner.R

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### PROCESS 10: DELETE CITY COLUMN
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### PROCESS 11: OTHER CHECKS
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# gender other as unknown
#data$Gender[grep("other",data$Gender,ignore.case=TRUE)]="Unknown"

#! gender blank as unknown? !#
#data$Patient_gender[which(data$Gender=="")]="Unknown"

# clean up the Nasal Swab to not be Nasal Swab but swab
data$Specimen_type_description=gsub(" Swab"," swab",data$Specimen_type_description)
data$Specimen_type_description=gsub("Nasopharyngeal swab","Nasopharyngeal swabs",data$Specimen_type_description)


### use the cases w PHI race and ethnicity

# delete the spaces between the multiple races
#data$Race <-gsub(",[[:space:]]", ",", data$race)

#There is no dirty data in ELR line list
#data$Result =gsub("'455371000124106'", " ",data$Result)
#data$Result =gsub("SEE SEPARATE REPORT", " ",data$Result)


# eliminate data with no result
#elim_inds=which(data$Result=="")
#keep_inds=setdiff(1:nrow(data),elim_inds)
#data=data[keep_inds,]

#! race as OMB-1997 !#
#! link: https://loinc.org/72826-1/ !#
#! how to handle concatenation? !#
#! perhaps: condition only the single-races, and leave multi-race alone? !#
#! explain copying of row? !#
# table(data$Race)


### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### PROCESS 4: SOURCE TEXTS
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# copy non-NA text from data$source.text to data$source
text_inds=which(nchar(data$"spec_source")>5)
if (length(text_inds)>0){data$spec_source[text_inds]=data$"spec_source"[text_inds]}

# if test method includes 'saliva', then source should be 'Saliva'
salv_inds=grep("saliva",data$test_name,ignore.case=TRUE)
if (length(salv_inds)>0){data$spec_source[salv_inds]="Saliva"}

# for remaining blank elements in data$source, enforce as "Nasopharyngeal swabs"
empt_inds=which(data$spec_source=="")
if (length(empt_inds)>0){data$spec_source[empt_inds]="Nasopharyngeal swabs"}

# clear garbage
rm(text_inds,salv_inds,empt_inds)

#owrite_inds took this out

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### PROCESS 1: MATRIX SHUFFLE
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#dc=(data$City_Match)
#dc2=(data$County_Match)

names(data)[which(names(data)=="event_id")]="eventid"
names(data)[which(names(data)=="spec_coll_date")]="spec_col_date"
#names(data)[which(names(data)=="spec_num")]="spec_num"
names(data)[which(names(data)=="spec_source")]="source"
names(data)[which(names(data)=="spec_source_snomed")]="source text"
#names(data)[which(names(data)=="mrn_elr")]="mrn_elr"
names(data)[which(names(data)=="test_name")]="test_method"
#names(data)[which(names(data)=="spec_rec_date")]="spec_rec_date"
#names(data)[which(names(data)=="result")]="result"
names(data)[which(names(data)=="result_free_text")]="result value"
names(data)[which(names(data)=="tested_date")]="date_tested"
names(data)[which(names(data)=="result_rpt_date")]="date_reported_dph"
names(data)[which(names(data)=="facility_name")]="lab_facility"
names(data)[which(names(data)=="lab_name")]="lab name"
names(data)[which(names(data)=="auth_name")]="ordering_lab"
names(data)[which(names(data)=="ordering_provider_name")]="Ordering Provider Name"
names(data)[which(names(data)=="notes")]="Notes"
#names(data)[which(names(data)=="dob")]="dob"
#names(data)[which(names(data)=="gender")]="gender"
#names(data)[which(names(data)=="race")]="race"
names(data)[which(names(data)=="hisp")]="ethnicity"
#names(data)[which(names(data)=="age")]="age"
names(data)[which(names(data)=="State")]="state"
names(data)[which(names(data)=="zip_code")]="zip code"
names(data)[which(names(data)=="hisp")]="ethnicity"
names(data)[which(names(data)=="county")]="county"
#names(data)[which(names(data)=="icu")]="icu"
names(data)[which(names(data)=="hospitalized")]="hosp"
names(data)[which(names(data)=="symp_onset_date")]="sx date"
names(data)[which(names(data)=="cong_setting")]="congregate"

# open target headers
targ_head=names(read_excel(targ_file))


# change caps case to title for data
data$ethnicity = str_to_title(data$ethnicity)
data$race = str_to_title(data$race)
data$Patient_city = str_to_title(data$Patient_city)
data$hosp = str_to_title(data$hosp)
data$gender = str_to_title(data$gender)
data$icu = str_to_title(data$icu)

### PROCESS 1a: back-fill DPH data with missing columns from target
# 1: find target columns not found in DPH data
miss_vars=setdiff(targ_head,names(data))
# 2: create columns with NA
miss_data=as.data.frame(matrix(NA,nrow=nrow(data),ncol=length(miss_vars)))
# 3: name the columns to match missing targets
names(miss_data)=miss_vars
# 4: merge empty columns onto DPH data
data=cbind(data,miss_data)

### PROCESS 1b: match columns within DPH data to order of target
# 1: match DPH columns to order of target
col_inds=match(targ_head,names(data))
# 2: omit superfluous DPH columns
col_inds=col_inds[!is.na(col_inds)]
# 3: shuffle the DPH dataset
data=data[,col_inds]

### Quality Check
check_flag=prod(targ_head==names(data))==1
if (!check_flag){stop("PROCESS 1 NO GOOD")}

#data$city = dc
#data$county = dc2

# clear garbage
rm(targ_file,targ_head,miss_vars,miss_data,col_inds,check_flag)

data <- subset(data, subset=result != " ")


### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### PROCESS 5: SOURCE TEXTS
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# if data$"result value" is 260385009, data$result should be negative
#! does this mean to enforce negative, or negative relative to current value? !#
#! Alison: ineffectual code !#
#negv_inds=grep("260385009",data$"result value")
#if (length(negv_inds)>0){data$result[negv_inds]=-abs(data$result[negv_inds])}

# if data$"lab name" contains "sunrise" or is empty, eliminate and flag for repair
#! WRITE THESE TO FILE !#
empt_inds=which(data$"lab name"=="")
snrs_inds=grep("Sunrise",data$"lab name")
labs_inds=c(empt_inds,snrs_inds)
miss_labs=data$event_id[labs_inds]
#! if (length(miss_labs)>0){data=data[-miss_labs,]}
print(paste(length(labs_inds)," missing labs or sunrise labs"))

# clear garbage
rm(negv_inds,empt_inds,snrs_inds,labs_inds,miss_labs)

#filter out old dates and wrong dates
data <- data %>% filter(data$spec_col_date >= "2021-01-01" & data$spec_col_date < "2021-01-28")

# format the dates
data$spec_col_date=as.Date(data$spec_col_date,format="%m/%d/%Y")
data$spec_rec_date=as.Date(data$spec_rec_date,format="%m/%d/%Y")
data$date_tested=as.Date(data$date_tested,format="%m/%d/%Y")
data$date_reported_dph=as.Date(data$date_reported_dph,format="%m/%d/%Y")

####write the file as xml

# 2e. write out the data segment with NAs as blanks
write.table(data,"CELRdata.csv",na="",row.names=FALSE,col.names=TRUE,sep=",")
#or as xml file

