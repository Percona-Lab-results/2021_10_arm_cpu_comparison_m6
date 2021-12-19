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

### read ec2 data from https://instances.vantage.sh/
path_to_ec2_csv <- "/Users/nikitakricko/Documents/GitHub/2021_10_arm_cpu_comparison_m6a/test_scripts/analysis/aws_ec2_instance_comparison.csv"
ec2_data_dt_full <- read_csv(path_to_ec2_csv) %>% as.data.table() %>% janitor::clean_names()

ec2_data_dt_short <- ec2_data_dt_full[,.(name,api_name,memory,v_cp_us,physical_processor,clock_speed_g_hz,linux_on_demand_cost)]

ec2_data_dt_short[,VM_type:=api_name]
ec2_data_dt_short[,memory:=gsub(" GiB","",memory) %>% as.numeric()]
ec2_data_dt_short[,v_cp_us:=gsub(" vCPUs","",v_cp_us) %>% as.numeric()]
ec2_data_dt_short[,clock_speed_g_hz:=gsub(" GHz","",clock_speed_g_hz) %>% as.numeric()]
ec2_data_dt_short[,linux_on_demand_cost:=gsub("\\$| hourly","",linux_on_demand_cost) %>% as.numeric()]
ec2_data_dt_short[,price_usd:=linux_on_demand_cost]

ec2_data_dt_short$color <- "white"
ec2_data_dt_short[physical_processor %like% "AMD"]$color <- "firebrick1"
ec2_data_dt_short[physical_processor %like% "Intel"]$color <- "dodgerblue"
ec2_data_dt_short[physical_processor %like% "Graviton"]$color <- "darkgoldenrod1"
ec2_data_dt_short$cpu_type <- "none"
ec2_data_dt_short[physical_processor %like% "AMD"]$cpu_type <- "AMD"
ec2_data_dt_short[physical_processor %like% "Intel"]$cpu_type <- "Intel"
ec2_data_dt_short[physical_processor %like% "Graviton"]$cpu_type <- "Graviton"
ec2_data_dt_short[,cpu_amount_2:=paste("vCPU:",v_cp_us)]
ec2_data_dt_short$ec2_type <- "-"
ec2_data_dt_short[v_cp_us < 16]$ec2_type <- "small"
ec2_data_dt_short[v_cp_us >= 16]$ec2_type <- "medium"
ec2_data_dt_short[v_cp_us >= 48]$ec2_type <- "large"

oltp_test_result <- fst::read.fst("/Users/nikitakricko/Documents/GitHub/2021_10_arm_cpu_comparison_m6a/test_scripts/analysis/oltp_sysbench_logs.fst") %>% as.data.table()

oltp_test_result <- oltp_test_result[ec2_data_dt_short, on="VM_type"][!is.na(test_run)]

oltp_test_result[,"cpu_amount":=v_cp_us]
oltp_test_result <- oltp_test_result[Reduce(`|`, Map(`%like%`, list(VM_type), c("m6a", "m6g", "m5")))][!(VM_type %like% "m5a")]

