library(optparse)
library(data.table)



get_dt_about_logs <- function(path_to_log_folder){
  tic("collecting data took:")

  
  logs_folder <- path_to_log_folder
  dir_list <- list.dirs(path = logs_folder, full.names = TRUE, recursive = TRUE)
  file_names <- paste(logs_folder,"/" ,dir(dir_list), sep="")
  out_folder <-  paste(logs_folder,"/" ,gsub(".zip","",dir(dir_list)), sep="")
  logs_dt <- data.table(file_names,out_folder,logs_folder)
  logs_dt <- logs_dt[file_names %like% ".zip"]
  for(i in seq_along(1:NROW(logs_dt))){
    print(paste(i, " / ",NROW(logs_dt), ":" , logs_dt[i]$file_names))
    
    dir.create(logs_dt[i]$out_folder, showWarnings = FALSE)
    unzip(zipfile = logs_dt[i]$file_names, 
          exdir = logs_dt[i]$out_folder)
    
  }
  
  dir_list <- list.dirs(path = logs_folder, full.names = TRUE, recursive = TRUE)
  
  dir_list <- dir_list[endsWith(dir_list,"/var/log/sysbench")]
  

  
  lbig <- list()
  for(dir_i in seq_along(dir_list)){
    print(dir_i)
    sysbench_dir <- dir_list[dir_i]
    print(sysbench_dir)

    
    sysbench_dir_files <- dir(sysbench_dir)[dir(sysbench_dir) %like% "\\."]
    sysbench_dir_folders <- dir(sysbench_dir)[!(dir(sysbench_dir) %like% "\\.")]
    sysbench_dir_info_dt <- data.table("test_case"=sysbench_dir_folders,
                                       "test_case_folder"=paste(sysbench_dir, "/",sysbench_dir_folders, sep=""))
    
    summary_dt <- data.table("test_case"="summary",
                             "test_case_folder"=sysbench_dir)
    ll <- list()
    for(i in seq_along(1:NROW(sysbench_dir_info_dt))){
      test_case_folder <- sysbench_dir_info_dt[i]$test_case_folder
      print(paste(i,"/", NROW(sysbench_dir_info_dt),"  -  ",  test_case_folder))
      test_case_file <- dir(test_case_folder)[dir(test_case_folder) %like% "\\."]
      ll[[i]] <- data.table(test_case_folder,
                            test_case_file, full_path=paste(test_case_folder,sep="/",test_case_file))
      
    }
    
    temp_result_dt <- rbindlist(ll)
    
    res <- sysbench_dir_info_dt[temp_result_dt, on="test_case_folder"]
    
    
    res[,measurment_type:=gsub("_log(.*)",'',gsub("(.*)get_","",test_case_file))]
    res[,measurment_type:=ifelse((measurment_type %like% "sysbench"), "sysbench",measurment_type)]
    
    lbig[[dir_i]] <- rbindlist(list(res,summary_dt),fill=TRUE)
    
    
  }
  big_dt <- rbindlist(lbig)

  big_dt[,test_run:=gsub("(.*)/","",gsub("_sysbench_log/var/log(.*)",'',test_case_folder) )]
  
  big_dt[,vm_type:=gsub("_(.*)",'',gsub("(.*)/",'',test_run))]
  toc()
  big_dt
}

parse_db_log_file <- function(path_db_test_log){
  db_log_lines <- read_lines(path_db_test_log) %>% trimws(.)  %>% as.data.table()
  
  
  condition_list <- c("events (avg/stddev)","Number of threads","Prime numbers limit",
                      "events per second","total time",
                      "total time","total number of events","min","avg","max","percentile","sum",
                      "execution time (avg/stddev)", "read:","write","other", "total", "transactions","queries:","ignored errors","reconnects" )
  if (db_log_lines %like% "FATAL") {
    print("there are problem with the logs")
    return(data.table("title"="ERROR"))
  } else {
     db_log_lines <- db_log_lines[Reduce(`|`, Map(`%like%`, list(.), condition_list))]
    
    
    db_log_lines[, c("title","value") := tstrsplit(., ":", fixed=TRUE)]
    db_log_lines[, title:=gsub(" ", "_", title, fixed=FALSE)]
    db_log_lines[, value:=sub(" ", "", value)]
    db_log_lines[,.(title,value)]
    db_log_lines[title %like% "events_per_second", ":="(title=paste("cpu_speed",title,sep="_"))]
    db_log_lines[title %like% "total_time", ":="(title=paste("general_statistic",title,sep="_"))]
    db_log_lines[title %like% "total_number_of_events", ":="(title=paste("general_statistic",title,sep="_"))]
    
    db_log_lines[title %like% "min", ":="(title=paste("latency_ms",title,sep="_"))]
    db_log_lines[title %like% "avg", ":="(title=paste("latency_ms",title,sep="_"))]
    db_log_lines[title %like% "max", ":="(title=paste("latency_ms",title,sep="_"))]
    db_log_lines[title %like% "percentile", ":="(title=paste("latency_ms",title,sep="_"))]
    db_log_lines[title %like% "sum", ":="(title=paste("latency_ms",title,sep="_"))]
    
    
    db_log_lines[title %like% "read:", ":="(title=paste("query_exec",title,sep="_"))]
    db_log_lines[title %like% "write", ":="(title=paste("query_exec",title,sep="_"))]
    db_log_lines[title %like% "other", ":="(title=paste("query_exec",title,sep="_"))]
    db_log_lines[title %like% "total", ":="(title=paste("query_exec",title,sep="_"))]
    db_log_lines[title %like% "transactions", ":="(title=paste("sql_stat",title,sep="_"))]
    
    
    
    
    
    db_log_lines[title %like% "queries:", ":="(title=paste("sql_stat",title,sep="_"))]
    db_log_lines[title %like% "ignored errors", ":="(title=paste("sql_stat",title,sep="_"))]
    db_log_lines[title %like% "reconnects", ":="(title=paste("sql_stat",title,sep="_"))]
    
    
    
    db_log_lines[title %like% "events_(avg/stddev)", ":="(title=paste("thread_fairness",title,sep="_"))]
    db_log_lines[title %like% "execution_time_(avg/stddev)", ":="(title=paste("thread_fairness",title,sep="_"))]
    
    db_dt <- data.table::transpose(db_log_lines,make.names = "title")[2]
    db_dt[, c("latency_ms_events_avg","latency_ms_events_stddev") := tstrsplit(`latency_ms_events_(avg/stddev)`, ":", fixed=TRUE)]
    db_dt[,":="(`latency_ms_events_(avg/stddev)`= NULL)]
    db_dt[,":="(latency_ms_events_avg=sub("\\/.*", "", latency_ms_events_avg), latency_ms_events_stddev=sub(".*\\/", "", latency_ms_events_stddev))]
    
    db_dt[, c("latency_ms_execution_time_avg","latency_ms_execution_time_stddev") := tstrsplit(`latency_ms_execution_time_(avg/stddev)`, ":", fixed=TRUE)]
    db_dt[,":="(`latency_ms_execution_time_(avg/stddev)`= NULL)]
    db_dt[,":="(latency_ms_execution_time_avg=sub("\\/.*", "", latency_ms_execution_time_avg), latency_ms_execution_time_stddev=sub(".*\\/", "", latency_ms_execution_time_stddev))]
    db_dt[, c("sql_stat_transactions_total","sql_stat_transactions_per_sec") := tstrsplit(sql_stat_transactions, "(", fixed=TRUE)]
    db_dt[, c("queries_total","queries_per_sec") := tstrsplit(queries, "(", fixed=TRUE)]
    db_dt[, sql_stat_transactions_per_sec:=sub(" per sec.)", "", sql_stat_transactions_per_sec)]
    db_dt[, queries_per_sec:=sub(" per sec.)", "", queries_per_sec)]
    
    db_dt
  }
  
}

parse_cpu_test_info <- function(path_to_cpu_tests){
  cpu_log_lines <- read_lines(path_to_cpu_tests) %>% trimws(.)  %>% as.data.table()
  if(NROW(cpu_log_lines) < 3) {
    print("error with")
    print(path_to_cpu_tests)
    return(data.table("Number_of_threads"="ERROR"))
    }
  
  condition_list <- c("Number of threads","Prime numbers limit",
                      "events per second","total time",
                      "total time","total number of events","min","avg","max","percentile","sum",
                      "events (avg/stddev)","execution time (avg/stddev)")
  
  
  cpu_log_lines <- cpu_log_lines[Reduce(`|`, Map(`%like%`, list(.), condition_list))]
  cpu_log_lines[, c("title","value") := tstrsplit(., ":", fixed=TRUE)]
  cpu_log_lines[, title:=gsub(" ", "_", title, fixed=FALSE)]
  cpu_log_lines[, value:=sub(" ", "", value)]
  cpu_log_lines[,.(title,value)]
  cpu_log_lines[title %like% "events_per_second", ":="(title=paste("cpu_speed",title,sep="_"))]
  cpu_log_lines[title %like% "total_time", ":="(title=paste("general_statistic",title,sep="_"))]
  cpu_log_lines[title %like% "total_number_of_events", ":="(title=paste("general_statistic",title,sep="_"))]
  
  cpu_log_lines[title %like% "min", ":="(title=paste("latency_ms",title,sep="_"))]
  cpu_log_lines[title %like% "avg", ":="(title=paste("latency_ms",title,sep="_"))]
  cpu_log_lines[title %like% "max", ":="(title=paste("latency_ms",title,sep="_"))]
  cpu_log_lines[title %like% "percentile", ":="(title=paste("latency_ms",title,sep="_"))]
  cpu_log_lines[title %like% "sum", ":="(title=paste("latency_ms",title,sep="_"))]
  cpu_log_lines[title %like% "events_(avg/stddev)", ":="(title=paste("thread_fairness",title,sep="_"))]
  cpu_log_lines[title %like% "execution_time_(avg/stddev)", ":="(title=paste("thread_fairness",title,sep="_"))]
  
  cpu_dt <- data.table::transpose(cpu_log_lines,make.names = "title")[2]
  cpu_dt[, c("latency_ms_events_avg","latency_ms_events_stddev") := tstrsplit(`latency_ms_events_(avg/stddev)`, ":", fixed=TRUE)]
  cpu_dt[,":="(`latency_ms_events_(avg/stddev)`= NULL)]
  cpu_dt[,":="(latency_ms_events_avg=sub("\\/.*", "", latency_ms_events_avg), latency_ms_events_stddev=sub(".*\\/", "", latency_ms_events_stddev))]
  
  cpu_dt[, c("latency_ms_execution_time_avg","latency_ms_execution_time_stddev") := tstrsplit(`latency_ms_execution_time_(avg/stddev)`, ":", fixed=TRUE)]
  cpu_dt[,":="(`latency_ms_execution_time_(avg/stddev)`= NULL)]
  cpu_dt[,":="(latency_ms_execution_time_avg=sub("\\/.*", "", latency_ms_execution_time_avg), latency_ms_execution_time_stddev=sub(".*\\/", "", latency_ms_execution_time_stddev))]
  cpu_dt
}


path_to_cpu_tests <- "/Users/nikitakricko/Documents/logs/t3.large_09192021_2131_sysbench_log/var/log/sysbench/CPU_test_20000/CPU_test_20000_sysbenchmark.log"


parse_cpu_threads_test <- function(path_to_cpu_tests){
  cpu_log_lines <- read_lines(path_to_cpu_tests) %>% trimws(.)  %>% as.data.table()
  if(NROW(cpu_log_lines) < 3) {break}
  condition_list <- c("Number of threads","total time",
                      "total number of events",
                      "min","avg","max","percentile","sum",
                      "events (avg/stddev)",
                      "execution time (avg/stddev)")


  cpu_log_lines <- cpu_log_lines[Reduce(`|`, Map(`%like%`, list(.), condition_list))]
  cpu_log_lines[, c("title","value") := tstrsplit(., ":", fixed=TRUE)]
  cpu_log_lines[, title:=gsub(" ", "_", title, fixed=FALSE)]
  cpu_log_lines[, value:=sub(" ", "", value)]
  cpu_log_lines[,.(title,value)]


  cpu_log_lines[title %like% "events_per_second", ":="(title=paste("cpu_speed",title,sep="_"))]
  cpu_log_lines[title %like% "total_time", ":="(title=paste("general_statistic",title,sep="_"))]
  cpu_log_lines[title %like% "total_number_of_events", ":="(title=paste("general_statistic",title,sep="_"))]

  cpu_log_lines[title %like% "min", ":="(title=paste("latency_ms",title,sep="_"))]
  cpu_log_lines[title %like% "avg", ":="(title=paste("latency_ms",title,sep="_"))]
  cpu_log_lines[title %like% "max", ":="(title=paste("latency_ms",title,sep="_"))]
  cpu_log_lines[title %like% "percentile", ":="(title=paste("latency_ms",title,sep="_"))]
  cpu_log_lines[title %like% "sum", ":="(title=paste("latency_ms",title,sep="_"))]
  cpu_log_lines[title %like% "events_(avg/stddev)", ":="(title=paste("thread_fairness",title,sep="_"))]
  cpu_log_lines[title %like% "execution_time_(avg/stddev)", ":="(title=paste("thread_fairness",title,sep="_"))]

  cpu_dt <- data.table::transpose(cpu_log_lines,make.names = "title")[2]
  cpu_dt[, c("latency_ms_events_avg","latency_ms_events_stddev") := tstrsplit(`latency_ms_events_(avg/stddev)`, ":", fixed=TRUE)]
  cpu_dt[,":="(`latency_ms_events_(avg/stddev)`= NULL)]
  cpu_dt[,":="(latency_ms_events_avg=sub("\\/.*", "", latency_ms_events_avg), latency_ms_events_stddev=sub(".*\\/", "", latency_ms_events_stddev))]

  cpu_dt[, c("latency_ms_execution_time_avg","latency_ms_execution_time_stddev") := tstrsplit(`latency_ms_execution_time_(avg/stddev)`, ":", fixed=TRUE)]
  cpu_dt[,":="(`latency_ms_execution_time_(avg/stddev)`= NULL)]
  cpu_dt[,":="(latency_ms_execution_time_avg=sub("\\/.*", "", latency_ms_execution_time_avg), latency_ms_execution_time_stddev=sub(".*\\/", "", latency_ms_execution_time_stddev))]
  cpu_dt
}



convert_ts_to_dt_par <- function(input_dt){
  ncores <-  detectCores(logical = FALSE)
  mac_cluster <- makeCluster(ncores)
  doParallel::registerDoParallel(mac_cluster)
  tic("do some parralel stuff")
  pack <- c("tidyverse","data.table","lubridate","tictoc")
  par_result <- foreach(i=seq_along(1:NROW(input_dt)), .packages=pack) %dopar% {
    temp_val <-  input_dt[i]$values %>% as.data.table() %>% 
      setnames(c("V1","V2"), c("timestamp", "value"))
    temp_val$synth_id <- i
    temp_val$value <- as.numeric(temp_val$value)
    cbind(input_dt[i,!"values"],temp_val)
  }
  toc()
  stopCluster(mac_cluster)
  
  
  result_dt <- rbindlist(par_result, use.names = TRUE, fill = TRUE)
  result_dt[,":="(timestamp=round(as.numeric(sub("\\.(.*)","", timestamp))/10)*10)]
  result_dt
}


convert_ts_to_dt <- function(input_dt){

  
  ll <- list()
  for(i in seq_along(1:NROW(input_dt))){
    print(i)
    temp_val <-  input_dt[i]$values %>% as.data.table() %>% 
      setnames(c("V1","V2"), c("timestamp", "value"))
    temp_val$synth_id <- i
    temp_val$value <- as.numeric(temp_val$value)
    ll[[i]] <- cbind(input_dt[i,!"values"],temp_val)
  }
  
  result_dt <- rbindlist(ll, use.names = TRUE, fill = TRUE)
  result_dt[,":="(timestamp=round(as.numeric(sub("\\.(.*)","", timestamp))/10)*10)]
  result_dt
}


get_ts_dt <- function(input_links_to_file_dt){
  tic("Converting all ts took:")
  temp_list <- list()
  for(i in seq_along(1:NROW(input_links_to_file_dt))){
    print(paste(i, "/", NROW(input_links_to_file_dt)))
    
    path_to_log_file <- input_links_to_file_dt[i]$full_path
    measurment_type <- input_links_to_file_dt[i]$measurment_type
    vm_type <- input_links_to_file_dt[i]$vm_type
    test_case <- input_links_to_file_dt[i]$test_case
    test_run <- input_links_to_file_dt[i]$test_run
    
    res <- jsonlite::fromJSON(path_to_log_file) %>% as.data.table()
    metrics_dt <- res$data[[2]] %>% as.data.table()
    if(NROW(metrics_dt)!= 0){
      unnest_metrics <- convert_ts_to_dt(metrics_dt)
      head(unnest_metrics)
      unnest_metrics$measurement <- measurment_type
      unnest_metrics$vm_type <- vm_type
      unnest_metrics$test_case <- test_case
      unnest_metrics$test_run <- test_run
      temp_list[[i]] <- unnest_metrics
    }
  }
  
  resource_dt <- rbindlist(temp_list, fill=TRUE)
  resource_dt[,"node_ip":=ifelse(is.na(metric.node_name) ,metric,metric.node_name)]
  resource_dt[,metric.node_name:=NULL][,metric:=NULL][,synth_id:=NULL]
  toc()
  resource_dt
}
