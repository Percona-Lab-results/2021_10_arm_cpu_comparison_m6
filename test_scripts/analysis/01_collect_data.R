library(readr)
library(jsonlite)
library(data.table)
library(tidyverse)
library(ggplot2)
library(rjson)
library(rlist)
library(future)
library(furrr)
library(parallel)
library(foreach)
library(doParallel)
library(tictoc)
library(optparse)


source("00_functions.R")
options(digits.secs = 3)  
options(scipen = 999)


print(">>>>>>>>>>> parsing inpt options")

option_list = list(
  make_option(c("-s","--S3"), type="character", default=NULL, 
              help="S3 bucket with sysbench logs. \n example: 's3://perconatempsysbenchresult'", metavar="character")
); 

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

if (is.null(opt$S3)){
  print_help(opt_parser)
  print("ERROR: S3 buket link should be provided.n")
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
}
input_s3_bucket <- opt$S3


print(">>>>>>>>>>> setup working directory")
curent_dir <- system("pwd",intern = TRUE)

path_to_save_data <- paste(curent_dir, "/temp_data", sep="")
dir.create(path_to_save_data)
setwd(path_to_save_data)

print(curent_dir)
print(path_to_save_data)
print(">>>>>>>>>>> connecting to AWS")
print("connecting to AWS")

AWS_S3_collector <- paste("aws s3 cp s3://perconatempsysbenchresult ",path_to_save_data," --recursive",sep = "")
# AWS_S3_collector <- paste("aws s3 cp ",input_s3_bucket," ",path_to_save_data," --recursive",sep = "")
print("it is going to be run next script to get the data")
print(AWS_S3_collector)

system(AWS_S3_collector)
print(">>>>>>>>>>> data from AWS collected")

print(">>>>>>>>>>> data from AWS collected")
big_dt <- get_dt_about_logs(path_to_save_data)
print(">>>>>>>>>>> data from AWS collected")

# ###############################################
# ### parse sysbenchlogs OLTP test
# #################################################
print(">>>>>>>>>>> Parsing data")

sysbench_logs <- big_dt[measurment_type == "sysbench"][test_case %like% "OLTP_READ_only_test_"]

list_cases <- list()
for(i in seq_along(1:NROW(sysbench_logs))){
  
  file_to_parse <- sysbench_logs[i]$full_path
  print(i)
  print(file_to_parse)
  sysbench_logs_test <- parse_db_log_file(file_to_parse)
  sysbench_logs_test$test_case <- sysbench_logs[i]$test_case
  sysbench_logs_test$VM_type <- sysbench_logs[i]$vm_type
  sysbench_logs_test$test_run <-  sysbench_logs[i]$test_run
  list_cases[[i]] <- sysbench_logs_test
}

oltp_test_result <- rbindlist(list_cases, use.names =TRUE, fill = TRUE)


print(">>>>>>>>>>> Cleaning data")
oltp_test_result[,query_exec_general_statistic_total_number_of_events:=as.numeric(query_exec_general_statistic_total_number_of_events)]
oltp_test_result[,read:=as.numeric(read)]

oltp_test_result[,query_exec_write:=as.numeric(query_exec_write)]
oltp_test_result[,query_exec_other:=as.numeric(query_exec_other)]
oltp_test_result[,query_exec_total:=as.numeric(query_exec_total)]
oltp_test_result[,query_exec_general_statistic_total_time:=as.numeric(query_exec_general_statistic_total_time)]
oltp_test_result[,latency_ms_min:=as.numeric(latency_ms_min)]
oltp_test_result[,latency_ms_events_avg:=as.numeric(latency_ms_events_avg)]
oltp_test_result[,latency_ms_events_stddev:=as.numeric(latency_ms_events_stddev)]
oltp_test_result[,latency_ms_execution_time_avg:=as.numeric(latency_ms_execution_time_avg)]
oltp_test_result[,latency_ms_execution_time_stddev:=as.numeric(latency_ms_execution_time_stddev)]
oltp_test_result[,sql_stat_transactions_total:=as.numeric(sql_stat_transactions_total)]
oltp_test_result[,sql_stat_transactions_per_sec:=as.numeric(sql_stat_transactions_per_sec)]
oltp_test_result[,latency_ms_max:=as.numeric(latency_ms_max)]
oltp_test_result[,latency_ms_sum:=as.numeric(latency_ms_sum)]
oltp_test_result[,latency_ms_avg:=as.numeric(latency_ms_avg)]
oltp_test_result[,latency_ms_95th_percentile:=as.numeric(latency_ms_95th_percentile)]
oltp_test_result[,queries_total:=as.numeric(queries_total)]
oltp_test_result[,queries_per_sec:=as.numeric(queries_per_sec)]


# slow_ec2 <- c("c6g.large","c6g.xlarge","c6g.2xlarge", "c5.2xlarge", "c5.xlarge","c5.large", "c5a.2xlarge","c5a.xlarge","c5a.large","t4g.medium", "t4g.large","t3.medium", "t3.large", "m6g.large","m6g.xlarge","m6g.2xlarge","m5.large","m5.xlarge","m5.2xlarge","m5a.large","m5a.xlarge","m5a.2xlarge")
# medium_ec2 <- c("c6g.4xlarge","c5.4xlarge","c5a.4xlarge","m6g.4xlarge","m6g.8xlarge","m5.4xlarge","m5.8xlarge","m5a.4xlarge","m5a.8xlarge")
# fast_ec2 <- c("c6g.16xlarge","c6g.12xlarge","c5.12xlarge","c5a.16xlarge","c5a.12xlarge","m6g.12xlarge","m6g.16xlarge","m5.12xlarge","m5.16xlarge","m5a.12xlarge","m5a.16xlarge")
# 
# oltp_test_result$ec2_type <- "-"
# oltp_test_result[VM_type %in% slow_ec2]$ec2_type <- "small"
# oltp_test_result[VM_type %in% medium_ec2]$ec2_type <- "medium"
# oltp_test_result[VM_type %in% fast_ec2]$ec2_type <- "large"
# 
# ec2_dt <- data.table(VM_type=c(  "c6g.large", "c6g.xlarge", "c6g.2xlarge", "c6g.4xlarge", "c6g.12xlarge", "c6g.16xlarge",
#                                  "c5.large", "c5.xlarge", "c5.2xlarge", "c5.4xlarge", "c5.12xlarge",
#                                  "c5a.large","c5a.xlarge","c5a.2xlarge","c5a.4xlarge","c5a.12xlarge", "c5a.16xlarge", 
#                                  "t4g.medium", "t4g.large","t3.medium", "t3.large",
#                                  "m6g.large","m6g.xlarge","m6g.2xlarge","m6g.4xlarge","m6g.8xlarge","m6g.12xlarge","m6g.16xlarge",
#                                  "m5.large","m5.xlarge","m5.2xlarge","m5.4xlarge","m5.8xlarge","m5.12xlarge","m5.16xlarge",
#                                  "m5a.large","m5a.xlarge","m5a.2xlarge","m5a.4xlarge","m5a.8xlarge","m5a.12xlarge", "m5a.16xlarge"),
#                      cpu_amount=c(2,4,8,16,48,64,
#                                   2,4,8,16,48,
#                                   2,4,8,16,48,64,
#                                   2,2,2,2,2, 4,8,16,32,48,64,2,4,8,16,32,48,64,2,4,8,16,32,48,64),
#                      ram_size_gb=c(4,8,16,32,96,128,
#                                    4,8,16,32,96, 
#                                    4,8,16,32,96,128,
#                                    4,8,4,8,8,16,32,64,128,192,256,8,16,32,64,128,192,256,8,16,32,64,128,192,256),
#                      price_usd=c(0.068, 0.136, 0.272, 0.5440, 1.6320,2.1760,
#                                  0.085, 0.17, 0.34, 0.68, 2.04, 
#                                  0.077,0.1540, 0.3080, 0.6160,1.8480,2.4640,
#                                  0.0336 , 0.0672, 0.0418,0.0835, 0.077,0.154,0.308,0.616,1.232,1.848,2.464,0.096,0.192,0.384,0.768,1.536,2.304,3.072, 0.086, 0.172, 0.344,0.688,1.376,2.064,2.752))
# 
# 
# oltp_test_result <- oltp_test_result[ec2_dt, on="VM_type"]
# oltp_test_result <- oltp_test_result[ec2_type != "-"][is.na(title)][,title := NULL]
# 
# oltp_test_result$x_break_label <- oltp_test_result$VM_type
# oltp_test_result[,x_break_label:=gsub("m5\\.","Intel\nm5\\.",x_break_label)]
# oltp_test_result[,x_break_label:=gsub("m5a\\.","AMD\nm5a\\.",x_break_label)]
# oltp_test_result[,x_break_label:=gsub("m6g\\.","Graviton\nm6g\\.",x_break_label)]
# 
# oltp_test_result[,x_break_label:=gsub("m5\\.","Intel\nm5\\.",x_break_label)]
# oltp_test_result[,x_break_label:=gsub("m5a\\.","AMD\nm5a\\.",x_break_label)]
# oltp_test_result[,x_break_label:=gsub("m6g\\.","Graviton\nc6g\\.",x_break_label)]
# oltp_test_result[,x_break_label:=gsub("c5\\.","Intel\nm5\\.",x_break_label)]
# oltp_test_result[,x_break_label:=gsub("c5a\\.","AMD\nm5a\\.",x_break_label)]
# oltp_test_result[,x_break_label:=gsub("c6g\\.","Graviton\nc6g\\.",x_break_label)]
# oltp_test_result[,x_break_label:=gsub("t3\\.","Intel\nt3\\.",x_break_label)]
# oltp_test_result[,x_break_label:=gsub("t3a\\.","AMD\nmt3a\\.",x_break_label)]
# oltp_test_result[,x_break_label:=gsub("t4g\\.","Graviton\nt4g\\.",x_break_label)]
# 
# oltp_test_result$color <- "white"
# oltp_test_result[x_break_label %like% "AMD"]$color <- "firebrick1"
# oltp_test_result[x_break_label %like% "Intel"]$color <- "dodgerblue"
# oltp_test_result[x_break_label %like% "Graviton"]$color <- "darkgoldenrod1"
# oltp_test_result$cpu_type <- "none"
# oltp_test_result[x_break_label %like% "AMD"]$cpu_type <- "AMD"
# oltp_test_result[x_break_label %like% "Intel"]$cpu_type <- "Intel"
# oltp_test_result[x_break_label %like% "Graviton"]$cpu_type <- "Graviton"
# oltp_test_result[,cpu_amount_2:=paste("vCPU:",cpu_amount)]






print(oltp_test_result)


print(">>>>>>>>>>> Saving data")
path_to_save_results <- paste(path_to_save_data, "/../oltp_sysbench_logs.fst", sep="")
print(path_to_save_results)
fst::write.fst(oltp_test_result,path_to_save_results,compress = 100)

path_to_save_results_csv <- paste(path_to_save_data, "/../oltp_test_result.csv", sep="")
write_csv2(oltp_test_result, path_to_save_results_csv)
print(path_to_save_results_csv)
print(">>>>>>>>>>> Data saved")
