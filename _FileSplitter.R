# _FileSplitter.R
#
# Split files for writing
################################################################

# # 1a. benchmarks in round-up to the smallest number of files required
# n_files=nrow(data)/25e3
# benchmarks=seq(1,nrow(data),length.out=ceiling(n_files))
# 
# # 1b. account for possibility that only a single file will be needed
# if (length(benchmarks)==1){benchmarks=c(1,2)}
# 
# # 2. for each benchmark
# for (i in 1:(length(benchmarks)-1)){
# 
#   # name of CDC expected
#   #InterPartner~CELR~CT~AIMSPlatform~Prod~Prod~2021020218050000~STOP~COVID_ELR001.csv
#   
# 	# 2a. create a name for the file write-out
# 	write_name=paste(zero_pad,i,".csv",sep="")
# 
# 	# 2d. extract the data segment (serial block of rows; all columns)
#   data_segment=data[benchmarks[i]:(benchmarks[i+1]-1),]
# 
# 	# 2e. write out the data segment with NAs as blanks
#   write.table(data_segment,write_name,na="",row.names=FALSE,col.names=TRUE,sep=",")
# 
# }


zero_pad=paste("InterPartner~CELR~CT~AIMSPlatform~Prod~Prod~2021020218050000~STOP~COVID")

data %>% 
  select(csv_file_version_no:Submitter_unique_sample_ID) %>%
  group_by(grp = rep(row_number(), length.out = n(), each = 25000)) %>%
  group_walk(~ write_csv(.x, paste0(zero_pad, .y$grp, ".csv"), na=""))



