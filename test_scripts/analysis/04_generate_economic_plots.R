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
library(ggh4x)


print("==============================================================")
back_up_wd <- getwd()
current_dir <- system("pwd",intern = TRUE)
setwd(current_dir) 
source("00_functions.R")
source("00_viz_functions.R")
path_to_file <- paste(current_dir,"oltp_sysbench_logs.fst", sep="/")

oltp_test_result <- fst::read.fst(path_to_file) %>% as.data.table()



path_to_save_plots <- paste(current_dir, "/post_economical_plots", sep="")
dir.create(path_to_save_plots)
setwd(path_to_save_plots)
print(current_dir)
print(getwd())
# current_dir <- "/Users/nikitakricko/Documents/GitHub/2021_10_arm_cpu_comparison_c5/test_scripts/analysis"
print("==============================================================")


table(temp_dt$VM_type)
oltp_test_result <- oltp_test_result[!(VM_type %like% "t3")][!(VM_type %like% "t4")]

all_scenarios <- oltp_test_result
table(m5_scenario$VM_type)
m5_scenario <- oltp_test_result[VM_type %like% c("m5|m6g")]
table(m5_scenario$VM_type)
c5_scenario <- oltp_test_result[VM_type %like% c("c5|c6")]
table(c5_scenario$VM_type)
m6_scenario <- oltp_test_result[VM_type %like% c("m6")]
table(m6_scenario$VM_type)


all_scenarios_simplify <- all_scenarios[cpu_amount == Number_of_threads]

table(all_scenarios$Number_of_threads)
all_scenarios_simplify_high_load <- all_scenarios[128 == Number_of_threads]


p_71_all_scenarios_econ_e <- top_efficient_lollipop(all_scenarios,sort_by_USD=TRUE, limit=10)
save_plot("071_top_10_scenarios_econ_e.png", p_71_all_scenarios_econ_e)
p_72_all_scenarios_econ_e <- top_efficient_lollipop(all_scenarios,sort_by_USD=FALSE, limit=10)
save_plot("072_top_10_scenarios_econ_e.png", p_72_all_scenarios_econ_e)


p_73_all_scenarios_econ_e <- top_efficient_lollipop(all_scenarios,sort_by_USD=TRUE)
save_plot("073_all_scenarios_econ_e.png", p_73_all_scenarios_econ_e)
save_plot("073_high_all_scenarios_econ_e.png", p_73_all_scenarios_econ_e, input_height=20)
p_74_all_scenarios_perf_e <- top_efficient_lollipop(all_scenarios,sort_by_USD=FALSE)
save_plot("074_all_scenarios_perf_e.png", p_74_all_scenarios_perf_e)
save_plot("074_high_all_scenarios_perf_e.png", p_74_all_scenarios_perf_e, input_height=20)


p_75_all_scenarios_simplify_econ_e <- top_efficient_lollipop(all_scenarios_simplify,sort_by_USD=TRUE)
save_plot("075_all_scenarios_simplify_econ_e.png", p_75_all_scenarios_simplify_econ_e)
p_76_all_scenarios_simplify_perf_e <- top_efficient_lollipop(all_scenarios_simplify,sort_by_USD=FALSE)
save_plot("076_all_scenarios_simplify_perf_e.png", p_76_all_scenarios_simplify_perf_e)

p_77_all_scenarios_simplify_HL_econ_e <- top_efficient_lollipop(all_scenarios_simplify_high_load,sort_by_USD=TRUE)
save_plot("077_all_scenarios_simplify_HL_econ_e.png", p_77_all_scenarios_simplify_HL_econ_e)
p_78_all_scenarios_simplify_HL_perf_e <- top_efficient_lollipop(all_scenarios_simplify_high_load,sort_by_USD=FALSE)
save_plot("078_all_scenarios_simplify_HL_perf_e.png", p_78_all_scenarios_simplify_HL_perf_e)



p_0441_cheapest_rating <- cheapest_ec2_for_particular_load_bar_plot(all_scenarios_simplify, 
                                                                    input_subtitle = "load equal number of vCPU on the EC2 instance")
save_plot("0441_all_scenarios_simplify_cheapest_rating.png", p_0441_cheapest_rating)

p_0442_cheapest_HL_rating <- cheapest_ec2_for_particular_load_bar_plot(all_scenarios_simplify_high_load, 
                                                                       input_subtitle = "Maximal load on EC2 were used")
save_plot("0442_all_scenarios_simplify_HL_cheapest_rating.png", p_0442_cheapest_HL_rating)
### m5 m6g scenarios ############

m5_scenario <- oltp_test_result[VM_type %like% c("m5|m6g")]
m5_scenario_simplify <- m5_scenario[cpu_amount == Number_of_threads]
m5_scenario_simplify_high_load <- m5_scenario[128 == Number_of_threads]

p_811_m5_scenarios_econ_e <- top_efficient_lollipop(m5_scenario,sort_by_USD=TRUE, limit=20)
save_plot("811_m5_scenarios_econ_e.png", p_811_m5_scenarios_econ_e)
p_812_m5_scenarios_perf_e <- top_efficient_lollipop(m5_scenario,sort_by_USD=FALSE, limit=20)
save_plot("812_m5_scenarios_perf_e.png", p_812_m5_scenarios_perf_e)

p_813_m5_scenarios_simplify_econ_e <- top_efficient_lollipop(m5_scenario_simplify,sort_by_USD=TRUE)
save_plot("813_m5_scenarios_simplify_econ_e.png", p_813_m5_scenarios_simplify_econ_e)
p_814_m5_scenarios_simplify_perf_e <- top_efficient_lollipop(m5_scenario_simplify,sort_by_USD=FALSE)
save_plot("814_m5_scenarios_simplify_perf_e.png", p_814_m5_scenarios_simplify_perf_e)

p_815_m5_scenarios_simplify_HL_econ_e <- top_efficient_lollipop(m5_scenario_simplify_high_load,sort_by_USD=TRUE)
save_plot("815_m5_scenarios_simplify_HL_econ_e.png", p_815_m5_scenarios_simplify_HL_econ_e)
p_816_m5_scenarios_simplify_HL_perf_e <- top_efficient_lollipop(m5_scenario_simplify_high_load,sort_by_USD=FALSE)
save_plot("816_m5_scenarios_simplify_HL_perf_e.png", p_816_m5_scenarios_simplify_HL_perf_e)


##### C5 csenarios ##########
c5_scenario <- oltp_test_result[VM_type %like% c("c5|c6")]
c5_scenario_simplify <- c5_scenario[cpu_amount == Number_of_threads]
c5_scenario_simplify_high_load <- c5_scenario[128 == Number_of_threads]

p_821_c5_scenarios_econ_e <- top_efficient_lollipop(c5_scenario,sort_by_USD=TRUE, limit=20)
save_plot("821_c5_scenarios_econ_e.png", p_821_c5_scenarios_econ_e)
p_822_c5_scenarios_perf_e <- top_efficient_lollipop(c5_scenario,sort_by_USD=FALSE, limit=20)
save_plot("822_c5_scenarios_perf_e.png", p_822_c5_scenarios_perf_e)

p_823_c5_scenarios_simplify_econ_e <- top_efficient_lollipop(c5_scenario_simplify,sort_by_USD=TRUE)
save_plot("823_c5_scenarios_simplify_econ_e.png", p_823_c5_scenarios_simplify_econ_e)
p_824_c5_scenarios_simplify_perf_e <- top_efficient_lollipop(c5_scenario_simplify,sort_by_USD=FALSE)
save_plot("824_c5_scenarios_simplify_perf_e.png", p_824_c5_scenarios_simplify_perf_e)

p_825_c5_scenarios_simplify_HL_econ_e <- top_efficient_lollipop(c5_scenario_simplify_high_load,sort_by_USD=TRUE)
save_plot("825_c5_scenarios_simplify_HL_econ_e.png", p_825_c5_scenarios_simplify_HL_econ_e)
p_826_c5_scenarios_simplify_HL_perf_e <- top_efficient_lollipop(c5_scenario_simplify_high_load,sort_by_USD=FALSE)
save_plot("826_c5_scenarios_simplify_HL_perf_e.png", p_826_c5_scenarios_simplify_HL_perf_e)



### m6 scenarios ############

m6_scenario <- oltp_test_result[VM_type %like% c("m6")]
m6_scenario_simplify <- m6_scenario[cpu_amount == Number_of_threads]
m6_scenario_simplify_high_load <- m6_scenario[128 == Number_of_threads]

p_831_m6_scenarios_econ_e <- top_efficient_lollipop(m6_scenario,sort_by_USD=TRUE, limit=20)
save_plot("831_m6_scenarios_econ_e.png", p_831_m6_scenarios_econ_e)
p_832_m6_scenarios_perf_e <- top_efficient_lollipop(m6_scenario,sort_by_USD=FALSE, limit=20)
save_plot("832_m6_scenarios_perf_e.png", p_832_m6_scenarios_perf_e)

p_833_m6_scenarios_simplify_econ_e <- top_efficient_lollipop(m6_scenario_simplify,sort_by_USD=TRUE)
save_plot("833_m6_scenarios_simplify_econ_e.png", p_833_m6_scenarios_simplify_econ_e)
p_834_m6_scenarios_simplify_perf_e <- top_efficient_lollipop(m6_scenario_simplify,sort_by_USD=FALSE)
save_plot("834_m6_scenarios_simplify_perf_e.png", p_834_m6_scenarios_simplify_perf_e)

p_835_m6_scenarios_simplify_HL_econ_e <- top_efficient_lollipop(m6_scenario_simplify_high_load,sort_by_USD=TRUE)
save_plot("835_m6_scenarios_simplify_HL_econ_e.png", p_835_m6_scenarios_simplify_HL_econ_e)
p_836_m6_scenarios_simplify_HL_perf_e <- top_efficient_lollipop(m6_scenario_simplify_high_load,sort_by_USD=FALSE)
save_plot("836_m6_scenarios_simplify_HL_perf_e.png", p_836_m6_scenarios_simplify_HL_perf_e)



#### 13 details m5 scenarios ######
p_13_1_m5_rpd_overview <- requests_per_dollar(m5_scenario)
save_plot("13_1_m5_rpd_overview.png", p_13_1_m5_rpd_overview)
p_13_2_m5_rph_overview <- requests_ph(m5_scenario)
save_plot("13_2_m5_rph_overview.png", p_13_2_m5_rph_overview)
p_13_3_m5_efficiency_overview <- efficient_comparison_point_plot(m5_scenario, facet_threads=FALSE)
save_plot("13_3_m5_efficiency_overview.png", p_13_3_m5_efficiency_overview)


#### 14 details c5 scenarios ######
p_14_1_c5_rpd_overview <- requests_per_dollar(c5_scenario)
save_plot("14_1_c5_rpd_overview.png", p_14_1_c5_rpd_overview)
p_14_2_c5_rph_overview <- requests_ph(c5_scenario)
save_plot("14_2_c5_rph_overview.png", p_14_2_c5_rph_overview)
p_14_3_c5_efficiency_overview <- efficient_comparison_point_plot(c5_scenario, facet_threads=FALSE)
save_plot("14_3_c5_efficiency_overview.png", p_14_3_c5_efficiency_overview)


#### 15 details m6 scenarios ######
p_15_1_m6_rpd_overview <- requests_per_dollar(m6_scenario)
save_plot("15_1_m6_rpd_overview.png", p_15_1_m6_rpd_overview)
p_15_2_m6_rph_overview <- requests_ph(m6_scenario)
save_plot("15_2_m6_rph_overview.png", p_15_2_m6_rph_overview)
p_15_3_m6_efficiency_overview <- efficient_comparison_point_plot(m6_scenario, facet_threads=FALSE)
save_plot("15_3_m6_efficiency_overview.png", p_15_3_m6_efficiency_overview)


### best price for hourly load 
list_qph_load <- c(500, 1000, 1500, 2000, 2100 ) * 1000000

p_500 <- best_hourly_load_price_barplot(oltp_test_result, list_qph_load[1])
save_plot("911_hourly_load_500m.png", p_500)
p_1000 <- best_hourly_load_price_barplot(oltp_test_result, list_qph_load[2])
save_plot("912_hourly_load_1000m.png", p_1000)
p_1500 <- best_hourly_load_price_barplot(oltp_test_result, list_qph_load[3])
save_plot("913_hourly_load_1500m.png", p_1500)
p_2000 <- best_hourly_load_price_barplot(oltp_test_result, list_qph_load[4])
save_plot("914_hourly_load_2000m.png", p_2000)
p_2200 <- best_hourly_load_price_barplot(oltp_test_result, list_qph_load[5])
save_plot("915_hourly_load_2200m.png", p_2200)


### best price for QPS load 

list_qps_load <- c(10, 50,100,250,300,350,400,500) * 1000

# input_qps <- list_qps_load[1]
p_10 <- best_second_load_price_barplot(oltp_test_result, list_qps_load[1])
save_plot("921_rps_load_10k.png", p_10)
p_50 <- best_second_load_price_barplot(oltp_test_result, list_qps_load[2])
save_plot("922_rps_load_50k.png", p_50)
p_100 <- best_second_load_price_barplot(oltp_test_result, list_qps_load[3])
save_plot("923_rps_load_100k.png", p_100)
# p_250 <- best_second_load_price_barplot(oltp_test_result, list_qps_load[4])
# save_plot("924_rps_load_250k.png", p_250)
p_300 <- best_second_load_price_barplot(oltp_test_result, list_qps_load[5])
save_plot("924_rps_load_300k.png", p_300)
# p_350 <- best_second_load_price_barplot(oltp_test_result, list_qps_load[6])
# save_plot("926_rps_load_350k.png", p_350)
# p_400 <- best_second_load_price_barplot(oltp_test_result, list_qps_load[7])
# save_plot("927_rps_load_400k.png", p_400)
p_500 <- best_second_load_price_barplot(oltp_test_result, list_qps_load[8])
save_plot("925_rps_load_500k.png", p_500)


### generate hourly heatmap 

hourly_heat_map <- heatmap_cheapest_for_hourly_load(oltp_test_result)
save_plot("931_horly_heatmap.png", hourly_heat_map)
### generate qps heatmap 

secondly_heatmap <- heatmap_cheapest_for_second_load(oltp_test_result)
save_plot("932_secondly_heatmap.png", secondly_heatmap)



c5_scenarios_econ_e <- top_efficient_lollipop(c5_scenario,sort_by_USD=TRUE, limit=20)
c5_scenarios_perf_e <- top_efficient_lollipop(c5_scenario,sort_by_USD=FALSE, limit=20)





m6_scenarios_econ_e <- top_efficient_lollipop(m6_scenario,sort_by_USD=TRUE, limit=20)
m6_scenarios_perf_e <- top_efficient_lollipop(m6_scenario,sort_by_USD=FALSE, limit=20)

all_equal_test_result <- oltp_test_result[cpu_amount == Number_of_threads]

all_equal_scenarios_econ_e <- top_efficient_lollipop(all_equal_test_result,sort_by_USD=TRUE, limit=20)
all_equal_scenarios_perf_e <- top_efficient_lollipop(all_equal_test_result,sort_by_USD=FALSE, limit=20)

all_max_load_test_result <- oltp_test_result[64 == Number_of_threads]

all_max_load_scenarios_econ_e <- top_efficient_lollipop(all_max_load_test_result,sort_by_USD=TRUE, limit=20)
all_max_load_scenarios_perf_e <- top_efficient_lollipop(all_max_load_test_result,sort_by_USD=FALSE, limit=20)




input_dt <- oltp_test_result[cpu_type == "Intel"]
temp_dt <- input_dt
column_name <- "queries_per_sec"
temp_dt <- temp_dt[,.(test_run,VM_type,Number_of_threads, "value"=eval(get(column_name)),cpu_amount,price_usd, cpu_type,color)]
temp_dt <- temp_dt[,.("avg_qps"=mean(value)), by=.(VM_type,Number_of_threads, cpu_amount,price_usd, cpu_type,color)]
temp_dt[,":="("description"=paste("vCPU:",cpu_amount,"_load:",Number_of_threads,"tps", sep="")) ]

tdt1 <- temp_dt[Number_of_threads == cpu_amount]
tdt1$comp_type <- "smaller"
tdt2 <- temp_dt[Number_of_threads == 2 * cpu_amount]
tdt2$comp_type <- "bigger"
tdt3 <- rbindlist(list(tdt1,tdt2))

setorder(tdt3, cpu_amount,VM_type,Number_of_threads)

# graviton_tdt <- tdt3
# intel_tdt <- tdt3
# AMD_tdt <- tdt3

# graviton_tdt
tdt4 <- dcast(tdt3, VM_type+cpu_amount+cpu_type~comp_type,value.var="avg_qps")
tdt4[,"pct_diff":=round((bigger/smaller-1)*100)]
tdt4$vm_class <- "NULL"
tdt4[VM_type %like% c("m6i|m6g|m6a")]$vm_class <- "m6"
tdt4[VM_type %like% c("m5i|m5")]$vm_class <- "m5"
tdt4[VM_type %like% c("c5|c6g|c5a")]$vm_class <- "c5"
# graviton_tdt <- tdt4
# intel_tdt <- tdt4
# AMD_tdt <- tdt4

# plot_dt <- rbindlist(list(graviton_tdt,intel_tdt,AMD_tdt))

dcast(tdt4[vm_class %in% c("m6","c5")],cpu_amount+vm_class~cpu_type, value.var="pct_diff")

plot_dt <- tdt4[vm_class %in% c("m6","c5")]

melted_dt <- data.table::melt(plot_dt,
                              id.vars =c("VM_type","cpu_amount","cpu_type","vm_class"),
                              measure.vars=c("bigger","smaller","pct_diff"))

melted_dt[,"jtype":=paste(cpu_type,vm_class, sep="_")]
melted_dt[,"value":=round(value)]


HL_advantage_absolute_bar_plot <- ggplot(melted_dt[variable != "pct_diff"][vm_class == "m6"][cpu_amount >= 16], 
       aes(x=value,
           y=as.factor(cpu_amount), 
           fill=as.factor(variable),
           color=as.factor(cpu_type))) +
  scale_fill_manual(name="load:",
                    values=c("bigger"="skyblue4",
                             "smaller"="skyblue"),
                    labels = c('double','equal'))+
  scale_color_manual(name = 'color:\nCPU type',
                     values =c('Graviton'='darkgoldenrod1',
                               'Intel'='dodgerblue',
                               'AMD'='firebrick1'),
                     labels = c('Graviton','Intel', "AMD"))+
  facet_grid(.~cpu_type, scales = "free") +
  geom_bar(stat="identity", position=position_dodge(),size=2) +
  geom_text(aes(x=10000,
                label=value, 
                group=variable),
            position = position_dodge(width = .9), 
            size=3, color='black') + 
  scale_x_sqrt() + 
  labs(title="Performance comparison of  high-performance EC2 instances with equal and duble load",
       subtitle="used instances series:m6i.*, m6a.*, m6g.*",
       caption="equal - active threads were equal to number of vCPU,\n double - active threads were double than nubmer of vCPU on EC2 instance ",
       x="approximate transaction per second", 
       y="number of vCPU")




HL_advantage_relative_bar_plot <-ggplot(melted_dt[variable == "pct_diff"][vm_class == "m6"][cpu_amount >= 16], 
       aes(x=value,
           y=as.factor(cpu_amount), 
           fill=as.factor(variable),
           color=as.factor(cpu_type))) +
  scale_color_manual(name = 'color:\nCPU type',
                     values =c('Graviton'='darkgoldenrod1',
                               'Intel'='dodgerblue',
                               'AMD'='firebrick1'),
                     labels = c('Graviton','Intel', "AMD"))+
  facet_grid(.~cpu_type) +
  geom_bar(stat="identity", position=position_dodge(),size=2, fill="skyblue") +
  geom_text(aes(x=0.5,
                label=value, 
                group=variable),
            position = position_dodge(width = .9), 
            size=3, color='black') + 
  scale_x_sqrt() + 
  labs(title="Advantage of high-performance instances with double load",
       subtitle="used instances series:m6i.*, m6a.*, m6g.*",
       caption="double load - when active threads were double than nubmer of vCPU on EC2 instance ",
       x="advantage in percents (%)", 
       y="number of vCPU")


HL_advantage_relative_bar_plot
save_plot("0511_HL_advantage_relative_bar_plot.png", HL_advantage_relative_bar_plot)
HL_advantage_absolute_bar_plot
save_plot("0512_HL_advantage_absolute_bar_plot.png", HL_advantage_absolute_bar_plot)

### csv for appendix
input_dt <- oltp_test_result[Number_of_threads == cpu_amount]
temp_dt <- input_dt
column_name <- "queries_per_sec"
temp_dt <- temp_dt[,.(test_run,VM_type,Number_of_threads, "value"=eval(get(column_name)),cpu_amount,price_usd, cpu_type,color)]
temp_dt <- temp_dt[,.("avg_qps"=round(mean(value))), by=.(VM_type,Number_of_threads, cpu_amount,price_usd, cpu_type,color)]
table_for_csv <- temp_dt[,.(VM_type,Number_of_threads,cpu_amount,avg_qps,price_usd,cpu_type)] %>% setorder(cpu_type,cpu_amount,VM_type)

write_excel_csv(table_for_csv, "post_economical_plots/short_results.xls")




