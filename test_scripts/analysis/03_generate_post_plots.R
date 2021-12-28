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
library(plotly)
require(magick)
require(cowplot)



print("==============================================================")
back_up_wd <- getwd()
current_dir <- system("pwd",intern = TRUE)
current_dir <- "/Users/nikitakricko/Documents/GitHub/2021_10_arm_cpu_comparison_m6/test_scripts/analysis"
setwd(current_dir) 
source("00_functions.R")
source("00_viz_functions.R")
path_to_file <- paste(current_dir,"oltp_sysbench_logs.fst", sep="/")

oltp_test_result <- fst::read.fst(path_to_file) %>% as.data.table()



path_to_save_plots <- paste(current_dir, "/post_plots", sep="")
dir.create(path_to_save_plots)
setwd(path_to_save_plots)
print(current_dir)
print(getwd())
# current_dir <- "/Users/nikitakricko/Documents/GitHub/2021_10_arm_cpu_comparison_c5/test_scripts/analysis"
print("==============================================================")




condition_list <- c("m6i", "m6g", "m6a")
oltp_test_result <- oltp_test_result[VM_type %like_in% condition_list]

# col_list <- c("read" ,"query_exec_other",
#               "query_exec_general_statistic_total_number_of_events","latency_ms_min", "latency_ms_avg", "latency_ms_max",
#               "latency_ms_95th_percentile", "latency_ms_sum", "latency_ms_events_avg", "latency_ms_events_stddev",
#               "latency_ms_execution_time_avg","latency_ms_execution_time_stddev", "sql_stat_transactions_total",
#               "sql_stat_transactions_per_sec","queries_total", "queries_per_sec" )
# 
# 
# 
# cpuList <- oltp_test_result$cpu_amount %>% unique()
# ec2_types <- oltp_test_result$ec2_type %>% unique()



### Clean plots ################################
### small
# table(oltp_test_result$VM_type)
tic("picture generation took")

p_11 <- get_db_universal(oltp_test_result[ec2_type == "small"],
                         input_title = "Workload: queries per second (vCPU: 2, 4,8)",
                         column_name = "queries_per_sec", 
                         yaxis_label = "Amount of queries per second",
                         x_axis_aws = FALSE,
                         add_logo = FALSE,
                         facet_cpu=TRUE)
save_plot("011_workload_qps_small.png", p_11)

p_12 <- get_db_universal(oltp_test_result[ec2_type == "small"],
                         input_title = "p95 latency during test (vCPU: 2, 4,8)",
                         column_name = "latency_ms_95th_percentile", 
                         yaxis_label = "ms",
                         x_axis_aws = FALSE,
                         add_logo = TRUE,
                         facet_cpu=TRUE)
save_plot("012_95th_latency_small.png", p_12)



### medium
p_21 <- get_db_universal(oltp_test_result[ec2_type == "medium"],
                         input_title = "Workload: queries per second (vCPU: 16,32)",
                         column_name = "queries_per_sec", 
                         yaxis_label = "Amount of queries per second",
                         x_axis_aws = FALSE,
                         add_logo = TRUE,
                         facet_cpu=TRUE)
save_plot("021_workload_qps_medium.png", p_21)

p_22 <- get_db_universal(oltp_test_result[ec2_type == "medium"],
                         input_title = "p95 latency during test  (vCPU: 16,32)",
                         column_name = "latency_ms_95th_percentile", 
                         yaxis_label = "ms",
                         x_axis_aws = FALSE,
                         add_logo = TRUE,
                         facet_cpu=TRUE)
save_plot("022_95th_latency_medium.png", p_22)



### large
p_31 <- get_db_universal(oltp_test_result[ec2_type == "large"],
                         input_title = "Workload: queries per second (vCPU: 48,64)",
                         column_name = "queries_per_sec",
                         yaxis_label = "Amount of queries per second",
                         x_axis_aws = FALSE,
                         add_logo = FALSE,
                         facet_cpu=TRUE)
save_plot("031_workload_qps.png", p_31)

p_32 <- get_db_universal(oltp_test_result[ec2_type == "large"],
                         input_title = "p95 latency during test  (vCPU: 48,64)",
                         column_name = "latency_ms_95th_percentile",
                         yaxis_label = "ms",
                         x_axis_aws = FALSE,
                         add_logo = FALSE,
                         facet_cpu=TRUE)
save_plot("032_95th_latency.png", p_32)


### overview
p_41 <- get_db_universal(oltp_test_result,
                         input_title = "Workload: queries per second",
                         column_name = "queries_per_sec", 
                         yaxis_label = "Amount of queries per second",
                         x_axis_aws = FALSE,
                         add_logo = TRUE,
                         facet_cpu=TRUE)
save_plot("061_workload_qps_overview.png", p_41)

p_42 <- get_db_universal(oltp_test_result,
                         input_title = "p95 latency during test",
                         column_name = "latency_ms_95th_percentile", 
                         yaxis_label = "ms",
                         x_axis_aws = FALSE,
                         add_logo = TRUE,
                         facet_cpu=TRUE)
save_plot("062_95th_percentile_overview.png", p_42)



### relative comparison #########


rel_com_intel_small <- intel_graviton_comparison(oltp_test_result[ec2_type == "small"],
                                       input_subtitle="",
                                       input_caption= "sysbench: 1.0.18, MySQL: 8.0.26-0",
                                       input_relative=TRUE)
save_plot("013_relative_comparison_intel_small.png", rel_com_intel_small)

rel_com_intel_medium <- intel_graviton_comparison(oltp_test_result[ec2_type == "medium"],
                                        input_subtitle="",
                                        input_caption= "sysbench: 1.0.18, MySQL: 8.0.26-0",
                                        input_relative=TRUE)
save_plot("023_relative_comparison_intel_medium.png", rel_com_intel_medium)

rel_com_intel_large <- intel_graviton_comparison(oltp_test_result[ec2_type == "large"],
                                       input_subtitle="",
                                       input_caption= "sysbench: 1.0.18, MySQL: 8.0.26-0",
                                       input_relative=TRUE)
save_plot("033_relative_comparison_intel_large.png", rel_com_intel_large)



rel_com_AMD_small <- intel_graviton_comparison(oltp_test_result[ec2_type == "small"],
                                        input_subtitle="",
                                        input_caption= "sysbench: 1.0.18, MySQL: 8.0.26-0",
                                        input_relative=TRUE,
                                        target_comparing_cpu="AMD")
save_plot("014_relative_comparison_AMD_small.png", rel_com_AMD_small)

rel_com_AMD_medium <- intel_graviton_comparison(oltp_test_result[ec2_type == "medium"],
                                         input_subtitle="",
                                         input_caption= "sysbench: 1.0.18, MySQL: 8.0.26-0",
                                         input_relative=TRUE,
                                         target_comparing_cpu="AMD")
save_plot("024_relative_comparison_AMD_medium.png", rel_com_AMD_medium)

rel_com_AMD_large <- intel_graviton_comparison(oltp_test_result[ec2_type == "large"],
                                       input_subtitle="",
                                       input_caption= "sysbench: 1.0.18, MySQL: 8.0.26-0",
                                       input_relative=TRUE,
                                       target_comparing_cpu="AMD")
save_plot("034_relative_comparison_AMD_large.png", rel_com_AMD_large)



rel_com_overview_intel <- intel_graviton_comparison(oltp_test_result,
                                                    input_subtitle="",
                                                    input_caption= "sysbench: 1.0.18, MySQL: 8.0.26-0",
                                                    input_relative=TRUE,
                                                    target_comparing_cpu="intel")
save_plot("065_relative_comparison_overview_intel.png", rel_com_overview_intel)

rel_com_overview_amd <- intel_graviton_comparison(oltp_test_result,
                                                  input_subtitle="",
                                                  input_caption= "sysbench: 1.0.18, MySQL: 8.0.26-0",
                                                  input_relative=TRUE,
                                                  target_comparing_cpu="amd")
save_plot("066_relative_comparison_overview_AMD.png", rel_com_overview_amd)




### absolute comparison ##########

### intel versus graviton
absolute_comparison_intel_small <- intel_graviton_comparison(oltp_test_result[ec2_type == "small"],
                                       input_subtitle="",
                                       input_caption= "sysbench: 1.0.18, MySQL: 8.0.26-0",
                                       input_relative=FALSE)
save_plot("016_absolute_comparison_intel_small.png", absolute_comparison_intel_small)

absolute_comparison_intel_medium <- intel_graviton_comparison(oltp_test_result[ec2_type == "medium"],
                                        input_subtitle="",
                                        input_caption= "sysbench: 1.0.18, MySQL: 8.0.26-0",
                                        input_relative=FALSE)
save_plot("026_absolute_comparison_intel_medium.png", absolute_comparison_intel_medium)

absolute_comparison_intel_large <- intel_graviton_comparison(oltp_test_result[ec2_type == "large"],
                                       input_subtitle="",
                                       input_caption= "sysbench: 1.0.18, MySQL: 8.0.26-0",
                                       input_relative=FALSE)
save_plot("036_absolute_comparison_intel_large.png", absolute_comparison_intel_large)

### AMD versus graviton
absolute_comparison_AMD_small <- intel_graviton_comparison(oltp_test_result[ec2_type == "small"],
                                        input_subtitle="",
                                        input_caption= "sysbench: 1.0.18, MySQL: 8.0.26-0",
                                        input_relative=FALSE,
                                        target_comparing_cpu="AMD")
save_plot("017_absolute_comparison_AMD_small.png", absolute_comparison_AMD_small)

absolute_comparison_AMD_medium <- intel_graviton_comparison(oltp_test_result[ec2_type == "medium"],
                                         input_subtitle="",
                                         input_caption= "sysbench: 1.0.18, MySQL: 8.0.26-0",
                                         input_relative=FALSE,
                                         target_comparing_cpu="AMD")
save_plot("027_absolute_comparison_AMD_medium.png", absolute_comparison_AMD_medium)

absolute_comparison_AMD_large <- intel_graviton_comparison(oltp_test_result[ec2_type == "large"],
                                       input_subtitle="",
                                       input_caption= "sysbench: 1.0.18, MySQL: 8.0.26-0",
                                       input_relative=FALSE,
                                       target_comparing_cpu="amd")
save_plot("037_absolute_comparison_AMD_large.png", absolute_comparison_AMD_large)

### absolute comparison overview
absolute_comparison_overview_intel <- intel_graviton_comparison(oltp_test_result,
                                                    input_subtitle="",
                                                    input_caption= "sysbench: 1.0.18, MySQL: 8.0.26-0",
                                                    input_relative=FALSE)
save_plot("067_absolute_comparison_overview_intel.png", absolute_comparison_overview_intel)

absolute_comparison_overview_amd <- intel_graviton_comparison(oltp_test_result,
                                                  input_subtitle="",
                                                  input_caption= "sysbench: 1.0.18, MySQL: 8.0.26-0",
                                                  input_relative=FALSE,
                                                  target_comparing_cpu="AMD")
save_plot("068_absolute_comparison_overview_amd.png", absolute_comparison_overview_amd)


### requests per_dollar ###########
p_05_rpd_overview <- requests_per_dollar(oltp_test_result)
save_plot("261_rpd_overview.png", p_05_rpd_overview)
p_05_rpd_small <- requests_per_dollar(oltp_test_result[ec2_type == "small"])
save_plot("211_rpd_small.png", p_05_rpd_small)
p_05_rpd_medium <- requests_per_dollar(oltp_test_result[ec2_type == "medium"])
save_plot("221_rpd_large.png", p_05_rpd_medium)
p_05_rpd_large <- requests_per_dollar(oltp_test_result[ec2_type != "large"])
save_plot("231_rpd_large.png", p_05_rpd_large)

### request per hour  ##############
p_06_rph_overview <- requests_ph(oltp_test_result)
save_plot("262_rph_overview.png", p_06_rph_overview)
p_06_rph_small <- requests_ph(oltp_test_result[ec2_type == "small"])
save_plot("211_rph_small.png", p_06_rph_small)
p_06_rph_meium <- requests_ph(oltp_test_result[ec2_type == "medium"])
save_plot("221_rph_large.png", p_06_rph_meium)
p_06_rph_large <- requests_ph(oltp_test_result[ec2_type == "large"])
save_plot("231_rph_large.png", p_06_rph_large)

#### eficient
p_07_efficiency_overview <- efficient_comparison_point_plot(oltp_test_result, facet_threads=FALSE)
# print(p_10_overview)
save_plot("271_efficiency_overview.png", p_07_efficiency_overview)

#### efficient when amount of cores equal to load
dummy_dt <- oltp_test_result[cpu_amount == Number_of_threads]
p_equal_efficiency_overview <- efficient_comparison_point_plot(dummy_dt, facet_threads=FALSE)
save_plot("272_equal_efficiency_overview.png", p_equal_efficiency_overview)




### line thoughput
line_overview <- line_db_plot_universal(input_dt = oltp_test_result,
                                        input_title="Throughput dynamic depends on load",
                                        column_name = "queries_per_sec",
                                        yaxis_label="Throughput: Queries per seconds",
                                        x_axis_aws = FALSE,
                                        add_logo = FALSE,
                                        facet_cpu=TRUE)

save_plot("071_throughput_overview_line.png", line_overview)


line_overview_latency <- line_db_plot_universal(input_dt = oltp_test_result,
                                                input_title="P95 llatency dynamicly depends on load",
                                                column_name = "latency_ms_95th_percentile",
                                                yaxis_label="Throughput: p95 latency",
                                                x_axis_aws = FALSE,
                                                add_logo = FALSE,
                                                facet_cpu=TRUE)
save_plot("072_latency_p95_overview_line.png", line_overview_latency)

toc()
