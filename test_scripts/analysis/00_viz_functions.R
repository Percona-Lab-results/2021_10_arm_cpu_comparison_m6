library(ggplot2)
library(plotly)
library(ggrepel)
library(ggforce)
library(RColorBrewer) 
library(scales)
library(magick)
library(cowplot)

save_plot <- function(file_path_name,plot_to_save, input_width=16, input_height=9){
  if(!is.null(file_path_name)){
    fileName_to_save <- paste(file_path_name,".png",sep="")
    ggsave(fileName_to_save,plot_to_save,width = input_width, height = input_height, dpi = 250, units = "in", device='png')
    print(paste("plot successfully saved:",getwd(),"/",fileName_to_save))
  }
}

add_percona_logo <- function(input_gg_plot){
  logo_img <- image_read("https://avatars.githubusercontent.com/u/1683025?s=200&v=4") 
  input_gg_plot <- ggdraw() + 
    draw_plot(input_gg_plot,x = 0, y = 0.025, width = 1, height = .97)+
    draw_image(logo_img,x = 0.83, y = 0.86, width = 0.15, height = 0.15)
  input_gg_plot
}


template_gplot<- function(input_dt){
  input_dt[,"hms":=lubridate::as_datetime(timestamp)]
  min_datetime <- min(input_dt$hms)
  max_datetime <- max(input_dt$hms)
  time_breaks <- seq(min_datetime,max_datetime,10) %>% unique() %>% .[matches("(:00:0|:30:0)", vars=.)] %>% sort()
  
  
  min_y_break <- min(input_dt$N)-20
  if(min_y_break < 0) {min_y_break <- 0}
  max_y_break <- max(input_dt$N)+50
  generated_y_breaks <- seq(min_y_break,max_y_break,10)
  
  title_generated <- ""
  subtitle_generated <- paste("for period: ",min_datetime, " - ", max_datetime,"\n", sep="" )
  caption_generated <- "Information collected from grafana web api "
  
  
  result_plot <- ggplot(dt, aes(x=hms, y=N,color=can_use_spot)) + 
    geom_line() + 
    geom_point(size=.8) +
    scale_x_datetime(breaks=time_breaks) + 
    labs(x="",
         y="",
         title=title_generated,
         subtitle = subtitle_generated,
         caption = caption_generated,
         color = "") + 
    theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1),
          axis.text.y = element_text(angle = -45, vjust = 0.9, hjust=1, size=6))
  result_plot %>% add_percona_logo()
}


get_db_total_num_events <- function(input_dt){
  temp_dt <- input_dt
  list_of_threads <- temp_dt$Number_of_threads %>% unique() %>% as.numeric() %>% sort() %>% paste( collapse= ", ")
  
  title_generated <- "Total number of events during tests"
  subtitle_generated <- paste("for next concurency threads:", 
                              "\n",
                              list_of_threads,
                              "more is better",
                              sep="" )
  caption_generated <- "Information collected from grafana web-api "
  
  result_plot <- ggplot(temp_dt, aes(x=as.factor(VM_type), 
                            y=query_exec_general_statistic_total_number_of_events, 
                            fill=factor(Number_of_threads,level_order))) +
    geom_bar(stat = "identity", position="dodge")+
    labs(x="AWS instance",
         y="amount of events",
         title=title_generated,
         subtitle = subtitle_generated,
         caption = caption_generated,
         fill = "concurrency:\nAmount of threads") + 
      theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1),
            axis.text.y = element_text(angle = -45, vjust = 0.9, hjust=1, size=6))
  
  result_plot %>% add_percona_logo()
}

get_db_total_num_events_converted <- function(input_dt){
  temp_dt <- input_dt
  list_of_threads <- temp_dt$Number_of_threads %>% unique() %>% as.numeric() %>% sort() %>% paste( collapse= ", ")
  
  title_generated <- "Total number of events during tests"
  subtitle_generated <- paste("for next concurency threads:", 
                              "\n",
                              list_of_threads,
                              "more is better",
                              sep="" )
  caption_generated <- "Information collected from grafana web-api "
  
  result_plot <- ggplot(temp_dt, aes(x=factor(Number_of_threads,level_order), 
                                     y=query_exec_general_statistic_total_number_of_events, 
                                     fill=as.factor(VM_type))) +
    geom_bar(stat = "identity", position="dodge")+
    labs(x="concurrency:\nAmount of threads",
         y="amount of events",
         title=title_generated,
         subtitle = subtitle_generated,
         caption = caption_generated,
         fill = "AWS instance type") + 
    theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1),
          axis.text.y = element_text(angle = -45, vjust = 0.9, hjust=1, size=6))
  
  result_plot %>% add_percona_logo()
}

# input_dt <- oltp_test_result[ec2_type == "small"]
get_db_universal <- function(input_dt, 
                             column_name, 
                             input_title,
                             input_subtitle=NULL,
                             yaxis_label,
                             input_caption=NULL,
                             x_axis_aws=TRUE, 
                             add_range=TRUE, 
                             add_logo=TRUE,
                             facet_cpu=FALSE){
  temp_dt <- input_dt
  temp_dt <- temp_dt[,.(test_run,VM_type,Number_of_threads, "value"=eval(get(column_name)),cpu_amount,cpu_type,color)]
  temp_dt <- temp_dt[,.("min"=min(value), "max"=max(value), avg=mean(value)), by=.(VM_type,Number_of_threads, cpu_amount,cpu_type,color)]
  
  list_of_threads <- temp_dt$Number_of_threads %>% unique() %>% as.numeric() %>% sort() %>% paste( collapse= ", ")
  
  if (is.null(input_subtitle)) {
    subtitle_generated <- paste("for next concurency threads:", 
                                "\n",
                                list_of_threads,
                                sep="" )
  } else {
    subtitle_generated <- input_subtitle
  }
  
  list_of_ec2 <- input_dt$VM_type %>% unique() %>% sort() %>% paste(collapse = ",")
  
  caption_generated <- paste("used EC2:",list_of_ec2,input_caption, sep="\n")
  
  temp_dt$x_break_label <- temp_dt$VM_type
  # temp_dt[,x_break_label:=gsub("m5\\.","Intel\nm5\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("m5a\\.","AMD\nm5a\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("m6g\\.","Graviton\nc6g\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("c5\\.","Intel\nm5\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("c5a\\.","AMD\nm5a\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("c6g\\.","Graviton\nc6g\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("t3\\.","Intel\nt3\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("t3a\\.","AMD\nmt3a\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("t4g\\.","Graviton\nt4g\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("c5\\.","Intel\nm5\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("c5a\\.","AMD\nm5a\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("c6g\\.","Graviton\nc6g\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("t3\\.","Intel\nt3\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("t3a\\.","AMD\nmt3a\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("t4g\\.","Graviton\nt4g\\.",x_break_label)]
  # temp_dt$color <- "white"
  # temp_dt[x_break_label %like% "AMD"]$color <- "firebrick1"
  # temp_dt[x_break_label %like% "Intel"]$color <- "dodgerblue"
  # temp_dt[x_break_label %like% "Graviton"]$color <- "darkgoldenrod1"
  # temp_dt$cpu_type <- "none"
  # temp_dt[x_break_label %like% "AMD"]$cpu_type <- "AMD"
  # temp_dt[x_break_label %like% "Intel"]$cpu_type <- "Intel"
  # temp_dt[x_break_label %like% "Graviton"]$cpu_type <- "Graviton"
  
  temp_dt[,cpu_amount_2:=paste("vCPU:",cpu_amount)]
  
  temp_dt$cpu_amount_2 = factor(temp_dt$cpu_amount_2, levels=c('vCPU: 2','vCPU: 4','vCPU: 8','vCPU: 16','vCPU: 32','vCPU: 48', 'vCPU: 64'))
  
  
  
  level_order <- temp_dt$Number_of_threads %>% unique() %>% as.numeric() %>% sort()
  if(x_axis_aws){
    result_plot <- ggplot(temp_dt, aes(x=as.factor(x_break_label), 
                                       y=avg, 
                                       fill=factor(Number_of_threads,level_order)))+
      scale_fill_brewer(palette="YlOrBr")
    
    xaxis_label="AWS instance"
    fill_title="Threads"
  } else {
    result_plot <- ggplot(temp_dt, aes(x=factor(Number_of_threads,level_order), 
                                       y=avg, 
                                       fill=as.factor(cpu_type))) +
      scale_fill_manual(values = c("firebrick1","darkgoldenrod1","dodgerblue"))
    xaxis_label = "concurrency: amount of threads"
    fill_title = "CPU type:"
  }
  
  result_plot <- result_plot+ geom_bar(stat = "identity", position="dodge", size=2) +
    scale_y_sqrt(labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
    labs(x=xaxis_label,
         y=yaxis_label,
         title=input_title,
         subtitle = subtitle_generated,
         caption = caption_generated,
         fill =fill_title ) + 
    theme(axis.text.x = element_text(angle = 15, vjust = 0.9, hjust=1, size=10),
          axis.text.y = element_text(angle = -45, vjust = 0.9, hjust=1, size=10))
  
  if(add_range){
    result_plot <- result_plot +
      geom_errorbar(aes(ymin=min, ymax=max), width=.2,
                    position=position_dodge(.9), color="navyblue")
  }
  
  if(facet_cpu){
    result_plot <- result_plot + facet_wrap(.~cpu_amount_2, scales = "free")
  }
  if(add_logo){
    result_plot <- result_plot %>%  add_percona_logo()
  }
  
  result_plot
  
}

# temp_dt <- oltp_test_result
# temp_dt[cpu_type == "AMD"]
# setorder(temp_dt, cpu_amount,Number_of_threads)

cpu_sysem_user_usage <- function(input_resource_dt){
  cpu_usage <- input_resource_dt[measurement == "cpu_usage"][metric.mode %in% c("system", "user", "iowait")]
  setorder(cpu_usage, metric.mode,node_ip,-test_case)
  cpu_usage[,"seq_time" := rowid(rleid(test_case,metric.mode))]
  cpu_usage[,"threads":=extract_numeric(test_case)]
  
  
  title_generated <- "CPU usage during tests"
  subtitle_generated <- paste("cols: AWS instance", "\n",
                              "rows: amount of threads", "\n",
                              sep="" )
  caption_generated <- paste("Information collected from grafana web-api",
                             "\n",
                             "timestamp were standartize to mormilize x axis scale")
  
  result_plot <- ggplot(cpu_usage, 
                         aes(x=seq_time, 
                             y=value, 
                             color=metric.mode)) + 
                    geom_line() +
                    facet_grid(threads~vm_type) +
    scale_x_log10() +
    labs(x="standartized time",
         y="percent",
         title=title_generated,
         subtitle = subtitle_generated,
         caption = caption_generated,
         fill ="CPU usage type" ) + 
    theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1),
          axis.text.y = element_text(angle = -45, vjust = 0.9, hjust=1, size=6))
  
  result_plot
}

disk_usage <- function(input_resource_dt){
  measurements_list <- c("disk_io_load_total","io_disk_read","io_disk_write")
  disk_usage <- resource_dt[measurement %in% measurements_list]
  
  table(disk_usage$measurement)
  setorder(disk_usage, measurement,node_ip,test_case,timestamp)
  disk_usage[,"seq_time" := rowid(rleid(test_case,measurement))]
  disk_usage[,"threads":=extract_numeric(test_case)]
  
  
  title_generated <- "DISK usage during tests"
  subtitle_generated <- paste("cols: AWS instance", "\n",
                              "rows: amount of threads", "\n",
                              sep="" )
  caption_generated <- paste("Information collected from grafana web-api",
                             "\n",
                             "timestamp were standartize to mormilize x axis scale")
  
  result_plot <- ggplot(disk_usage,
                        aes(x=seq_time, 
                            y=value, 
                            color=measurement)) + 
    geom_line() +
    facet_grid(threads~vm_type) +
    labs(x="standartized time",
         y="percent",
         title=title_generated,
         subtitle = subtitle_generated,
         caption = caption_generated,
         fill ="CPU usage type" ) + 
    theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1),
          axis.text.y = element_text(angle = -45, vjust = 0.9, hjust=1, size=6))
  
  
  result_plot
}



# input_dt <- oltp_test_result
# intel_graviton_comparison <- function(input_dt, 
#                                       input_subtitle=NULL,
#                                       input_caption,
#                                       input_relative=TRUE,
#                                       add_logo=TRUE,
#                                       target_comparing_cpu="intel"){
#   temp_dt <- input_dt
#   column_name <- "queries_per_sec"
#   temp_dt <- temp_dt[,.(test_run,VM_type,Number_of_threads, "value"=eval(get(column_name)),cpu_amount, cpu_type,color)]
#   temp_dt <- temp_dt[,.("min"=min(value), "max"=max(value), avg=mean(value)), by=.(VM_type,Number_of_threads, cpu_amount, cpu_type,color)]
#   
#   list_of_threads <- temp_dt$Number_of_threads %>% unique() %>% as.numeric() %>% sort() %>% paste( collapse= ", ")
#   
#   if (is.null(input_subtitle)) {
#     subtitle_generated <- paste("for next concurency threads:", 
#                                 "\n",
#                                 list_of_threads,
#                                 sep="" )
#   } else {
#     subtitle_generated <- input_subtitle
#   }
#   
#   caption_generated <- input_caption
#   
#   temp_dt$x_break_label <- temp_dt$VM_type
#   # temp_dt[,x_break_label:=gsub("m5\\.","Intel\nm5\\.",x_break_label)]
#   # temp_dt[,x_break_label:=gsub("m5a\\.","AMD\nm5a\\.",x_break_label)]
#   # temp_dt[,x_break_label:=gsub("m6g\\.","Graviton\nm6g\\.",x_break_label)]
#   # 
#   # 
#   # temp_dt[,x_break_label:=gsub("c5\\.","Intel\nm5\\.",x_break_label)]
#   # temp_dt[,x_break_label:=gsub("c5a\\.","AMD\nm5a\\.",x_break_label)]
#   # temp_dt[,x_break_label:=gsub("c6g\\.","Graviton\nc6g\\.",x_break_label)]
#   # temp_dt[,x_break_label:=gsub("t3\\.","Intel\nt3\\.",x_break_label)]
#   # temp_dt[,x_break_label:=gsub("t3a\\.","AMD\nmt3a\\.",x_break_label)]
#   # temp_dt[,x_break_label:=gsub("t4g\\.","Graviton\nt4g\\.",x_break_label)]
#   # 
#   # temp_dt$color <- "white"
#   # temp_dt[x_break_label %like% "AMD"]$color <- "firebrick1"
#   # temp_dt[x_break_label %like% "Intel"]$color <- "dodgerblue"
#   # temp_dt[x_break_label %like% "Graviton"]$color <- "darkgoldenrod1"
#   # temp_dt$cpu_type <- "none"
#   # temp_dt[x_break_label %like% "AMD"]$cpu_type <- "AMD"
#   # temp_dt[x_break_label %like% "Intel"]$cpu_type <- "Intel"
#   # temp_dt[x_break_label %like% "Graviton"]$cpu_type <- "Graviton"
#   temp_dt[,cpu_amount_2:=paste("vCPU:",cpu_amount)]
#   temp_dt$cpu_amount_2 = factor(temp_dt$cpu_amount_2, levels=c('vCPU: 2','vCPU: 4','vCPU: 8','vCPU: 16','vCPU: 32','vCPU: 48', 'vCPU: 64'))
#   
#   list_of_threads <- temp_dt$Number_of_threads %>% unique() %>% as.numeric() %>% sort() %>% paste( collapse= ", ")
#   
#   tdt <-  temp_dt[,.(cpu_type,cpu_amount,Number_of_threads, avg)]
#   
#   dd_dt <- data.table::dcast(tdt,cpu_amount + Number_of_threads ~ cpu_type)
#   
#   if(target_comparing_cpu == "intel"){
#     dd_dt <- dd_dt[,AMD:=NULL] %>% na.omit()
#     dd_dt[,diff_GI_abs :=Graviton - Intel]
#     dd_dt[,diff_GI_rel :=round(Graviton/Intel-1,2)*100]
#     dd_dt[,cpu_type_winner:=ifelse((Graviton/Intel-1) > 0, "Graviton","Intel")]
#     color_scale <- scale_fill_manual(name = 'CPU type', 
#                                      values =c('Graviton'='darkgoldenrod1',
#                                                'Intel'='dodgerblue',
#                                                'AMD'='firebrick1'), 
#                                      labels = c('Graviton','Intel', "AMD"))
#   } else {
#     dd_dt <- dd_dt[,Intel:=NULL] %>% na.omit()
#     dd_dt[,diff_GI_abs :=Graviton - AMD]
#     dd_dt[,diff_GI_rel :=round(Graviton/AMD-1,2)*100]
#     dd_dt[,cpu_type_winner:=ifelse((Graviton/AMD-1) > 0, "Graviton","AMD")]
#     color_scale <- scale_fill_manual(name = 'CPU type', 
#                                      values =c('Graviton'='darkgoldenrod1',
#                                                'Intel'='dodgerblue',
#                                                'AMD'='firebrick1'), 
#                                      labels = c('Graviton','Intel', "AMD"))
#   }
#   
#   
#   
#   dd_dt[,cpu_amount_2:=paste("vCPU:",cpu_amount)]
#   
#   dd_dt$cpu_amount_2 = factor(dd_dt$cpu_amount_2, levels=c('vCPU: 2','vCPU: 4','vCPU: 8','vCPU: 16','vCPU: 32','vCPU: 48', 'vCPU: 64'))
#   list_of_threads <- temp_dt$Number_of_threads %>% unique() %>% as.numeric() %>% sort()
#   
#   dd_dt$Number_of_threads <- factor(dd_dt$Number_of_threads, levels=list_of_threads)
#   dd_dt$color <- "none"
#   
#   
#   if(input_relative){
#     min_break_value <- (round(min(dd_dt$diff_GI_rel) / 10)-1)*10
#     max_break_value <- (round(max(dd_dt$diff_GI_rel) / 10)+1)*10
#     break_range <- 5
#     x_lab_generated <- "% (difference in percent)"
#     title_generated <- paste("Relative (%) performance difference between ",target_comparing_cpu," and Graviton (queries per second)", sep="")
#     
#     
#     dd_dt[,"position":=ifelse(abs(diff_GI_rel) > 1, diff_GI_rel/2,0)]
#     
#     dd_dt[,"text_label":=diff_GI_rel]
#     
#     result_plot <- ggplot(dd_dt, aes(x=diff_GI_rel, 
#                                      y=Number_of_threads, fill=cpu_type_winner))
#     
#     
#   } else {
#     min_break_value <- (round(min(dd_dt$diff_GI_abs) / 100)-10)*100
#     max_break_value <- (round(max(dd_dt$diff_GI_abs) / 100)+10)*100
#     break_range <- 10000
#     x_lab_generated <- "Throughput difference (queries per second)"
#     title_generated <- paste("Absolute performance difference between ",target_comparing_cpu," and Graviton (queries per second)", sep="")
#     dd_dt[,"position":=ifelse(abs(diff_GI_abs) > 1, diff_GI_abs/2,0)]
#     dd_dt[,"text_label":=round(diff_GI_abs,0)]
#     result_plot <- ggplot(dd_dt, aes(x=diff_GI_abs, 
#                                      y=Number_of_threads, fill=cpu_type_winner))
#     
#   }
#   
#   
#   
#   
#   result_plot <- result_plot+ 
#     geom_bar(stat = "identity", position="dodge", size=2)+
#     geom_text(aes(label=abs(text_label), x=position), color="black", size=3.5) +
#     facet_wrap(.~cpu_amount_2, scales = "free_x") +
#     color_scale +
#     scale_x_continuous(breaks = seq(min_break_value, max_break_value, break_range), 
#                        labels = abs(seq(min_break_value, max_break_value, break_range))) +
#     theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1),
#           axis.text.y = element_text(angle = -45, vjust = 0.9, hjust=1, size=6)) +
#     labs(x=x_lab_generated,
#          y="Concurremcy: \nAmount of threads",
#          title=title_generated,
#          subtitle = subtitle_generated,
#          caption = caption_generated,
#          fill = "CPU type" )
#   
#   if(add_logo){
#     result_plot <- result_plot %>%  add_percona_logo()
#   }
#   result_plot
# }



# input_dt <- oltp_test_result
intel_graviton_comparison <- function(input_dt, 
                                      input_subtitle=NULL,
                                      input_caption,
                                      input_relative=TRUE,
                                      add_logo=TRUE,
                                      target_comparing_cpu="intel"){
  temp_dt <- input_dt
  column_name <- "queries_per_sec"
  temp_dt <- temp_dt[,.(test_run,VM_type,Number_of_threads, "value"=eval(get(column_name)),cpu_amount, cpu_type,color)]
  temp_dt <- temp_dt[,.("min"=min(value), "max"=max(value), avg=mean(value)), by=.(VM_type,Number_of_threads, cpu_amount, cpu_type,color)]
  
  list_of_threads <- temp_dt$Number_of_threads %>% unique() %>% as.numeric() %>% sort() %>% paste( collapse= ", ")
  
  if (is.null(input_subtitle)) {
    subtitle_generated <- paste("for next concurency threads:", 
                                "\n",
                                list_of_threads,
                                sep="" )
  } else {
    subtitle_generated <- input_subtitle
  }
  
  caption_generated <- input_caption
  
  temp_dt$x_break_label <- temp_dt$VM_type
  
  temp_dt[,cpu_amount_2:=paste("vCPU:",cpu_amount)]
  temp_dt$cpu_amount_2 = factor(temp_dt$cpu_amount_2, levels=c('vCPU: 2','vCPU: 4','vCPU: 8','vCPU: 16','vCPU: 32','vCPU: 48', 'vCPU: 64'))
  
  list_of_threads <- temp_dt$Number_of_threads %>% unique() %>% as.numeric() %>% sort() %>% paste( collapse= ", ")
  
  tdt <-  temp_dt[,.(cpu_type,cpu_amount,Number_of_threads, avg)]
  
  dd_dt <- data.table::dcast(tdt,cpu_amount + Number_of_threads ~ cpu_type)
  
  if(target_comparing_cpu == "intel"){
    target_comparing_cpu_for_title <- "Intel and Graviton"
    dd_dt <- dd_dt[,AMD:=NULL] %>% na.omit()
    dd_dt[,diff_GI_abs :=Graviton - Intel]
    dd_dt[,diff_GI_rel :=round(Graviton/Intel-1,2)*100]
    dd_dt[,cpu_type_winner:=ifelse((Graviton/Intel-1) > 0, "Graviton","Intel")]
    color_scale <- scale_fill_manual(name = 'CPU type', 
                                     values =c('Graviton'='darkgoldenrod1',
                                               'Intel'='dodgerblue'), 
                                     labels = c('Graviton','Intel'))
  } 
  if(target_comparing_cpu == "amd") {
    target_comparing_cpu_for_title <- "ADM and Graviton"
    dd_dt <- dd_dt[,Intel:=NULL] %>% na.omit()
    dd_dt[,diff_GI_abs :=Graviton - AMD]
    dd_dt[,diff_GI_rel :=round(Graviton/AMD-1,2)*100]
    dd_dt[,cpu_type_winner:=ifelse((Graviton/AMD-1) > 0, "Graviton","AMD")]
    color_scale <- scale_fill_manual(name = 'CPU type', 
                                     values =c('Graviton'='darkgoldenrod1',
                                               
                                               'AMD'='firebrick1'), 
                                     labels = c('Graviton', "AMD"))
  }
  
  if(target_comparing_cpu == "intelamd"){
    target_comparing_cpu_for_title <- "Intel and AMD"
    dd_dt <- dd_dt[,Graviton:=NULL] %>% na.omit()
    dd_dt[,diff_GI_abs :=Intel - AMD]
    dd_dt[,diff_GI_rel :=round(Intel/AMD-1,2)*100]
    dd_dt[,cpu_type_winner:=ifelse((Intel/AMD-1) > 0, "Intel","AMD")]
    color_scale <- scale_fill_manual(name = 'CPU type', 
                                     values =c(
                                       'Intel'='dodgerblue',
                                       'AMD'='firebrick1'), 
                                     labels = c('Intel', "AMD"))
  }
  
  dd_dt[,cpu_amount_2:=paste("vCPU:",cpu_amount)]
  
  dd_dt$cpu_amount_2 = factor(dd_dt$cpu_amount_2, levels=c('vCPU: 2','vCPU: 4','vCPU: 8','vCPU: 16','vCPU: 32','vCPU: 48', 'vCPU: 64'))
  list_of_threads <- temp_dt$Number_of_threads %>% unique() %>% as.numeric() %>% sort()
  
  dd_dt$Number_of_threads <- factor(dd_dt$Number_of_threads, levels=list_of_threads)
  dd_dt$color <- "none"
  
  
  if(input_relative){
    min_break_value <- (round(min(dd_dt$diff_GI_rel) / 10)-1)*10
    max_break_value <- (round(max(dd_dt$diff_GI_rel) / 10)+1)*10
    break_range <- 5
    x_lab_generated <- "% (difference in percent)"
    title_generated <- paste("Relative (%) performance difference between ",target_comparing_cpu_for_title," (queries per second)", sep="")
    
    
    dd_dt[,"position":=ifelse(abs(diff_GI_rel) > 1, diff_GI_rel/2,0)]
    
    dd_dt[,"text_label":=diff_GI_rel]
    
    result_plot <- ggplot(dd_dt, aes(x=diff_GI_rel, 
                                     y=Number_of_threads, fill=cpu_type_winner))
    
    
  } else {
    min_break_value <- (round(min(dd_dt$diff_GI_abs) / 100)-10)*100
    max_break_value <- (round(max(dd_dt$diff_GI_abs) / 100)+10)*100
    break_range <- 10000
    x_lab_generated <- "Throughput difference (queries per second)"
    title_generated <- paste("Absolute performance difference between ",target_comparing_cpu," and Graviton (queries per second)", sep="")
    dd_dt[,"position":=ifelse(abs(diff_GI_abs) > 1, diff_GI_abs/2,0)]
    dd_dt[,"text_label":=round(diff_GI_abs,0)]
    result_plot <- ggplot(dd_dt, aes(x=diff_GI_abs, 
                                     y=Number_of_threads, fill=cpu_type_winner))
    
  }
  
  
  
  
  result_plot <- result_plot+ 
    geom_bar(stat = "identity", position="dodge", size=2)+
    geom_text(aes(label=abs(text_label), x=position), color="black", size=3.5) +
    facet_wrap(.~cpu_amount_2, scales = "free_x") +
    color_scale +
    scale_x_continuous(breaks = seq(min_break_value, max_break_value, break_range), 
                       labels = abs(seq(min_break_value, max_break_value, break_range))) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1),
          axis.text.y = element_text(angle = -45, vjust = 0.9, hjust=1, size=6)) +
    labs(x=x_lab_generated,
         y="Concurremcy: \nAmount of threads",
         title=title_generated,
         subtitle = subtitle_generated,
         caption = caption_generated,
         fill = "CPU type" )
  
  if(add_logo){
    result_plot <- result_plot %>%  add_percona_logo()
  }
  result_plot
}




requests_ph <- function(input_dt){
  temp_dt <- input_dt
  column_name <- "queries_per_sec"
  temp_dt <- temp_dt[,.(test_run,VM_type,Number_of_threads, "value"=eval(get(column_name)),cpu_amount,price_usd, color, cpu_type)]
  temp_dt <- temp_dt[,.("min"=min(value), "max"=max(value), avg=mean(value)), by=.(VM_type,Number_of_threads, cpu_amount,price_usd, color,cpu_type)]
  temp_dt[,"approx_qph":=round(avg*3600,0)]
  temp_dt[,price_second:=price_usd/3600]
  temp_dt[,request_price:=price_second/avg]
  temp_dt[,dollar_request:=1/request_price]
  
  
  temp_dt$x_break_label <- temp_dt$VM_type
  # temp_dt[,x_break_label:=gsub("m5\\.","Intel\nm5\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("m5a\\.","AMD\nm5a\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("m6g\\.","Graviton\nm6g\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("c5\\.","Intel\nm5\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("c5a\\.","AMD\nm5a\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("c6g\\.","Graviton\nc6g\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("t3\\.","Intel\nt3\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("t3a\\.","AMD\nmt3a\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("t4g\\.","Graviton\nt4g\\.",x_break_label)]
  # temp_dt$color <- "white"
  # temp_dt[x_break_label %like% "AMD"]$color <- "firebrick1"
  # temp_dt[x_break_label %like% "Intel"]$color <- "dodgerblue"
  # temp_dt[x_break_label %like% "Graviton"]$color <- "darkgoldenrod1"
  # temp_dt$cpu_type <- "none"
  # temp_dt[x_break_label %like% "AMD"]$cpu_type <- "AMD"
  # temp_dt[x_break_label %like% "Intel"]$cpu_type <- "Intel"
  # temp_dt[x_break_label %like% "Graviton"]$cpu_type <- "Graviton"
  temp_dt[,cpu_amount_2:=paste("vCPU:",cpu_amount)]
  temp_dt$cpu_amount_2 = factor(temp_dt$cpu_amount_2, levels=c('vCPU: 2','vCPU: 4','vCPU: 8','vCPU: 16','vCPU: 32','vCPU: 48', 'vCPU: 64'))
  
  result_plot <- ggplot(temp_dt, 
         aes(x=factor(Number_of_threads, levels=c(1,2,4,8,16,32,64,128)), 
                      y=approx_qph,
                      fill=cpu_type)) + 
    geom_bar(stat = "identity", position="dodge", size=2) + 
    scale_fill_manual(values = c("firebrick1","darkgoldenrod1","dodgerblue")) +
    facet_wrap(.~cpu_amount_2, scales = "free") +
    scale_y_sqrt(labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
    labs(title = "Approximate queries (read) per hour",
         y="Approximate of queries per hour",
         x="Concurrency: amount oth threads",
         fill="CPU type:")
  result_plot
}

# input_dt <- oltp_test_result

requests_per_dollar <- function(input_dt){
  temp_dt <- input_dt
  column_name <- "queries_per_sec"
  temp_dt <- temp_dt[,.(test_run,VM_type,Number_of_threads, "value"=eval(get(column_name)),cpu_amount,price_usd, cpu_type,color)]
  temp_dt <- temp_dt[,.("min"=min(value), "max"=max(value), avg=mean(value)), by=.(VM_type,Number_of_threads, cpu_amount,price_usd, cpu_type,color)]
  temp_dt[,"approx_qph":=round(avg*3600,0)]
  temp_dt[,price_second:=price_usd/3600]
  temp_dt[,request_price:=price_second/avg]
  temp_dt[,dollar_request:=1/request_price]
  temp_dt$x_break_label <- temp_dt$VM_type
  # temp_dt[,x_break_label:=gsub("m5\\.","Intel\nm5\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("m5a\\.","AMD\nm5a\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("m6g\\.","Graviton\nm6g\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("c5\\.","Intel\nm5\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("c5a\\.","AMD\nm5a\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("c6g\\.","Graviton\nc6g\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("t3\\.","Intel\nt3\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("t3a\\.","AMD\nmt3a\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("t4g\\.","Graviton\nt4g\\.",x_break_label)]
  # temp_dt$color <- "white"
  # temp_dt[x_break_label %like% "AMD"]$color <- "firebrick1"
  # temp_dt[x_break_label %like% "Intel"]$color <- "dodgerblue"
  # temp_dt[x_break_label %like% "Graviton"]$color <- "darkgoldenrod1"
  # temp_dt$cpu_type <- "none"
  # temp_dt[x_break_label %like% "AMD"]$cpu_type <- "AMD"
  # temp_dt[x_break_label %like% "Intel"]$cpu_type <- "Intel"
  # temp_dt[x_break_label %like% "Graviton"]$cpu_type <- "Graviton"
  temp_dt[,cpu_amount_2:=paste("vCPU:",cpu_amount)]
  temp_dt$cpu_amount_2 = factor(temp_dt$cpu_amount_2, levels=c('vCPU: 2','vCPU: 4','vCPU: 8','vCPU: 16','vCPU: 32','vCPU: 48', 'vCPU: 64'))
  
  
  result_plot <- ggplot(temp_dt, 
                        aes(x=factor(Number_of_threads, levels=c(1,2,4,8,16,32,64,128)), 
                      y=dollar_request,
                      fill=cpu_type)) + 
    geom_bar(stat = "identity", position="dodge", size=2) +
    facet_wrap(.~cpu_amount_2, scales = "free") +
    scale_fill_manual(values = c("firebrick1","darkgoldenrod1","dodgerblue")) +
    scale_y_sqrt(labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
    labs(title = "Approx amount of queries (read) could be done for 1 USD",
         y="Amount of request per 1 USD",
         x="Concurrency: amount oth threads",
         fill="CPU type:")
  result_plot
}


efficient_comparison_point_plot <- function(input_dt, 
                                            facet_threads=FALSE,
                                            input_caption=NULL,
                                            add_logo = FALSE){
  temp_dt <- input_dt
  column_name <- "queries_per_sec"
  temp_dt <- temp_dt[,.(test_run,VM_type,Number_of_threads, "value"=eval(get(column_name)),cpu_amount,price_usd, cpu_type,color)]
  temp_dt <- temp_dt[,.("min"=min(value), "max"=max(value), avg=mean(value)), by=.(VM_type,Number_of_threads, cpu_amount,price_usd, cpu_type,color)]
  temp_dt[,"approx_qph":=round(avg*3600,0)]
  temp_dt[,price_second:=price_usd/3600]
  temp_dt[,request_price:=price_second/avg]
  temp_dt[,dollar_request:=1/request_price]
  temp_dt$x_break_label <- temp_dt$VM_type
  # temp_dt[,x_break_label:=gsub("m5\\.","Intel\nm5\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("m5a\\.","AMD\nm5a\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("m6g\\.","Graviton\nm6g\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("c5\\.","Intel\nm5\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("c5a\\.","AMD\nm5a\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("c6g\\.","Graviton\nc6g\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("t3\\.","Intel\nt3\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("t3a\\.","AMD\nmt3a\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("t4g\\.","Graviton\nt4g\\.",x_break_label)]
  # temp_dt$color <- "white"
  # temp_dt[x_break_label %like% "AMD"]$color <- "firebrick1"
  # temp_dt[x_break_label %like% "Intel"]$color <- "dodgerblue"
  # temp_dt[x_break_label %like% "Graviton"]$color <- "darkgoldenrod1"
  # temp_dt$cpu_type <- "none"
  # temp_dt[x_break_label %like% "AMD"]$cpu_type <- "AMD"
  # temp_dt[x_break_label %like% "Intel"]$cpu_type <- "Intel"
  # temp_dt[x_break_label %like% "Graviton"]$cpu_type <- "Graviton"
  temp_dt[,cpu_amount_2:=paste("vCPU:",cpu_amount)]
  temp_dt$cpu_amount_2 = factor(temp_dt$cpu_amount_2, levels=c('vCPU: 2','vCPU: 4','vCPU: 8','vCPU: 16','vCPU: 32','vCPU: 48', 'vCPU: 64'))
  
  
  
  
  x_breaks <- c(10, 25, 50,75, 100,150,
                200, 250,300,350,400,450,
                500, 550,600,650,700,
                750,800,850, 900,950, 1000,
                1100,1200,1300,1400,1500) * 1000000
  y_breaks <- c(10, 25, 50,75, 100,150,
                200, 250,300,350,400,450,
                500, 550,600,650,700,
                750,800,850, 900,950,1000,
                1100, 1200, 1300, 1400, 1500,
                1600, 1700, 1800, 1900, 2000,2100,2200,2300,2400,2500,2600,2700,2800,2900,3000) * 1000000
  
  num_thread_levels <- temp_dt$Number_of_threads %>% unique() %>% as.numeric()%>% sort()
  temp_dt$Number_of_threads <- factor(temp_dt$Number_of_threads, levels = num_thread_levels)
  
  list_of_ec2 <- temp_dt$VM_type %>% unique() %>% sort() 
  
  first_part <- list_of_ec2[1:round(length(list_of_ec2)/2)] %>% paste(collapse = ",")
  second_part <- list_of_ec2[(round(length(list_of_ec2)/2)+1):length(list_of_ec2)] %>% paste(collapse = ",")
  caption_generated <- paste("used EC2:",first_part,second_part,input_caption, sep="\n")
  
  
  result_plot <- ggplot(temp_dt,aes(x=dollar_request,
                     y=approx_qph,
                     shape=as.factor(cpu_amount),
                     color=as.factor(cpu_type),
                     size=Number_of_threads)) +
    geom_point() +
    labs(title = "CPU types efficient comparison (for MySQL)",
         x="Amount of requests per 1 USD",
         y="Approximate amount of requests per hour",
         caption = caption_generated,
         size="size:\nConcurrency\namount of threads:",
         color="color:\nCPU type:",
         shape="shape:\nvCPU amount") +
    scale_shape_manual(values=1:nlevels(as.factor(temp_dt$cpu_amount))) +
    scale_x_sqrt(breaks=x_breaks,
                 labels=label_number(suffix = " M", scale = 1e-6)) +
    scale_y_sqrt(breaks=y_breaks,
                 labels=label_number(suffix = " M", scale = 1e-6)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1),
          axis.text.y = element_text(angle = -45, vjust = 0.9, hjust=1, size=6)) +
    scale_color_manual(name = 'color:\nCPU type', 
                      values =c('Graviton'='darkgoldenrod1',
                                'Intel'='dodgerblue',
                                'AMD'='firebrick1'), 
                      labels = c('Graviton','Intel', "AMD"))

  if(facet_threads){
    result_plot <- result_plot + 
      facet_wrap(.~factor(Number_of_threads,levels = c(1,2,4,8,16,32,64,128)), scales="free_y")+
      labs(subtitle = "splited by (scenarios) concurrency: amount of threads")
  }
  
  if(add_logo){
    result_plot <- result_plot %>%  add_percona_logo()
  }
  
  result_plot
}



line_db_plot_universal <- function(input_dt,
                                   input_title = "",
                                   input_subtitle="",
                                   column_name = NULL,
                                   yaxis_label= NULL,
                                   input_caption=NULL,
                                   x_axis_aws = FALSE,
                                   add_logo = FALSE,
                                   facet_cpu=TRUE){
  temp_dt <- input_dt
  temp_dt <- temp_dt[,.(test_run,VM_type,Number_of_threads, "value"=eval(get(column_name)),cpu_amount,cpu_type,color)]
  temp_dt <- temp_dt[,.("min"=min(value), "max"=max(value), avg=mean(value)), by=.(VM_type,Number_of_threads, cpu_amount,cpu_type,color)]
  
  list_of_threads <- temp_dt$Number_of_threads %>% unique() %>% as.numeric() %>% sort() %>% paste( collapse= ", ")
  
  if (is.null(input_subtitle)) {
    subtitle_generated <- paste("for next concurency threads:", 
                                "\n",
                                list_of_threads,
                                sep="" )
  } else {
    subtitle_generated <- input_subtitle
  }
  
  list_of_ec2 <- input_dt$VM_type %>% unique() %>% sort() %>% paste(collapse = ",")
  
  caption_generated <- paste("used EC2:",list_of_ec2,input_caption, sep="\n")
  
  temp_dt$x_break_label <- temp_dt$VM_type
  
  temp_dt[,cpu_amount_2:=paste("vCPU:",cpu_amount)]
  
  temp_dt$cpu_amount_2 = factor(temp_dt$cpu_amount_2, levels=c('vCPU: 2','vCPU: 4','vCPU: 8','vCPU: 16','vCPU: 32','vCPU: 48', 'vCPU: 64'))
  
  level_order <- temp_dt$Number_of_threads %>% unique() %>% as.numeric() %>% sort()
  
  result_plot <- ggplot(temp_dt, aes(x=factor(Number_of_threads,level_order), 
                                     y=avg,
                                     color=as.factor(cpu_type), 
                                     group=as.factor(cpu_type))) +
    scale_color_manual(values = c("firebrick1","darkgoldenrod1","dodgerblue"))
  xaxis_label = "concurrency: amount of threads"
  fill_title = "CPU type:"
  
  
  result_plot <- result_plot+ geom_line( size=1.3) + geom_point()+
    scale_y_sqrt(labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
    labs(x=xaxis_label,
         y=yaxis_label,
         title = input_title,
         subtitle = subtitle_generated,
         caption = caption_generated,
         color =fill_title ) + 
    theme(axis.text.x = element_text(angle = 15, vjust = 0.9, hjust=1, size=10),
          axis.text.y = element_text(angle = -45, vjust = 0.9, hjust=1, size=10))
  
  if(facet_cpu){
    result_plot <- result_plot + facet_wrap(.~cpu_amount_2, scales = "free")
  }
  if(add_logo){
    result_plot <- result_plot %>%  add_percona_logo()
  }
  
  result_plot
}

top_efficient_lollipop <- function(input_dt, limit=NULL,sort_by_USD=TRUE){
  
  temp_dt <- input_dt
  column_name <- "queries_per_sec"
  temp_dt <- temp_dt[,.(test_run,VM_type,Number_of_threads, "value"=eval(get(column_name)),cpu_amount,price_usd, cpu_type,color)]
  temp_dt <- temp_dt[,.("min"=min(value), "max"=max(value), avg=mean(value)), by=.(VM_type,Number_of_threads, cpu_amount,price_usd, cpu_type,color)]
  temp_dt[,"approx_qph":=round(avg*3600,0)]
  temp_dt[,price_second:=price_usd/3600]
  temp_dt[,request_price:=price_second/avg]
  temp_dt[,dollar_request:=1/request_price]
  temp_dt$x_break_label <- temp_dt$VM_type
  temp_dt[,":="("title_VM"=paste(VM_type,cpu_amount,Number_of_threads, sep="_")) ]
  temp_dt[,":="("description"=paste("vCPU:",cpu_amount,"_load:",Number_of_threads,"_price:",price_usd, sep="")) ]
  
  
  # sort_by_USD <- TRUE
  # setorder(temp_dt, -target_column)
  if(sort_by_USD){
    setorder(temp_dt, -dollar_request)
    # temp_dt <- top_20_economic
    generated_subtitle <- "sorted by requests by 1USD"
    temp_dt$target_col <- temp_dt$dollar_request
    
  } else {
    setorder(temp_dt, -approx_qph)
    # temp_dt <- top_20_performance
    generated_subtitle <- "sorted by requests per hour"
    temp_dt$target_col <- temp_dt$approx_qph
  }
  # limit <- 10
  if(!is.null(limit)){
    temp_dt <- head(temp_dt,limit)
  }
  
  
  # temp_dt <- top_20_economic
  # generated_subtitle <- "sorted by requests by 1USD"
  # temp_dt$target_col <- temp_dt$dollar_request
  # temp_dt <- top_20_performance
  # generated_subtitle <- "sorted by requests per hour"
  # temp_dt$target_col <- temp_dt$approx_qph
  
  
  
  
  values_to_search <- sort(c(temp_dt$dollar_request,temp_dt$approx_qph))
  min_x_break <- round(min(values_to_search)/10000000)*10000000-10000000
  max_x_break <- round(max(values_to_search)/10000000)*10000000+10000000
  x_breaks <- seq(min_x_break,max_x_break,100000000)
  
  
  
  result_plot <- ggplot(temp_dt) +
    geom_segment( aes(y=as.numeric(reorder(title_VM, target_col)), 
                      yend=as.numeric(reorder(title_VM, target_col)),
                      x=approx_qph, 
                      xend=dollar_request,
                      color=cpu_type), 
                  size=5)+
    geom_point(aes(y=as.numeric(reorder(title_VM, target_col)), 
                   x=approx_qph,
                   fill="approx_qph",
                   color=cpu_type),
               size=5, 
               alpha=0.9, 
               shape=21, 
               stroke=2 ) +
    geom_point( aes(y=as.numeric(reorder(title_VM, target_col)), 
                    x=dollar_request,
                    fill="dollar_request",
                    color=cpu_type), 
                size=5, 
                alpha=0.9, 
                shape=21, 
                stroke=2 ) +
    scale_x_continuous(breaks=x_breaks,
                       labels=label_number(suffix = " M", scale = 1e-6)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1),
          axis.text.y = element_text(angle = 0, vjust = 0.9, hjust=1, size=6)) +
    scale_color_manual(name = 'CPU type', 
                       values =c('Graviton'='darkgoldenrod1',
                                 'Intel'='dodgerblue',
                                 'AMD'='firebrick1'), 
                       labels = c('Graviton','Intel', "AMD")) +
    scale_fill_manual(name = 'Measurement',
                      values = c("dollar_request"='green',
                                 "approx_qph"='purple'),
                      labels = c('Queries per USD','Queries per hour'))+
    scale_y_continuous(labels=rev(temp_dt$VM_type),
                       breaks=c(1:length(temp_dt$VM_type)),name="AWS EC2 instance",
                       sec.axis=sec_axis(~.,labels=rev(temp_dt$description),
                                         breaks=c(1:length(temp_dt$description)), name="Conditions") )+
    labs(title = "Economic efficiency of CPU's based on AWS EC2 instances for Mysql",
         subtitle = generated_subtitle,
         x="Amount of transaction (read query)")
  result_plot
}

top_efficient <- function(input_dt,
                          generated_title="Dummy title",
                          generated_subtitle="dummy subtitle",
                          generation_caption="dummy caption",
                          add_condition_description=TRUE,
                          USD_efficiency=TRUE, limit=NULL){
  
  
  temp_dt <- input_dt
  column_name <- "queries_per_sec"
  temp_dt <- temp_dt[,.(test_run,VM_type,Number_of_threads, "value"=eval(get(column_name)),cpu_amount,price_usd, cpu_type,color)]
  temp_dt <- temp_dt[,.("min"=min(value), "max"=max(value), avg=mean(value)), by=.(VM_type,Number_of_threads, cpu_amount,price_usd, cpu_type,color)]
  temp_dt[,"approx_qph":=round(avg*3600,0)]
  temp_dt[,price_second:=price_usd/3600]
  temp_dt[,request_price:=price_second/avg]
  temp_dt[,dollar_request:=1/request_price]
  temp_dt$x_break_label <- temp_dt$VM_type
  if(USD_efficiency){
    temp_dt[,":="("target_column"=eval(get("dollar_request")))]
    generated_title="Dummy title"
    generated_subtitle="dummy subtitle"
    generation_caption="dummy caption"
    generated_x_label <- "Amount of queries could be done for 1 USD"
  } else {
    temp_dt[,":="("target_column"=eval(get("approx_qph")))]
    generated_title="Dummy title"
    generated_subtitle="dummy subtitle"
    generation_caption="dummy caption"
    generated_x_label <- "Amount of queries could be done during 1 hour"
  }
  
  setorder(temp_dt, -target_column)
  if(!is.null(limit)){
    temp_dt <- head(temp_dt,limit)
  }
  
  temp_dt[,":="("title_VM"=paste(VM_type,cpu_amount,Number_of_threads, sep="_")) ]
  temp_dt[,":="("description"=paste("vCPU:",cpu_amount,"_load:",Number_of_threads, sep="")) ]
  
  
  min_y_break <- round(min(temp_dt$target_column)/10000000)*10000000-10000000
  max_y_break <- round(max(temp_dt$target_column)/10000000)*10000000+10000000
  y_breaks <- seq(min_y_break,max_y_break,10000000)
  
  
  
  result_plot <- ggplot(temp_dt, aes(y=reorder(title_VM, target_column),
                                     x=target_column, 
                                     fill=cpu_type)) + 
    geom_bar(stat = "identity", 
             position="dodge", 
             size=2)+
    scale_x_continuous(breaks=y_breaks,
                       labels=label_number(suffix = " B", scale = 1e-9))+
    coord_cartesian(xlim=c(min_y_break, max_y_break))+
    labs(title=generated_title,
         subtitle = generated_subtitle,
         caption = generation_caption,
         x=generated_x_label,
         y="instance type")+
    scale_y_discrete(labels=rev(temp_dt$VM_type))+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1),
          axis.text.y = element_text(angle = 0, vjust = 0.9, hjust=1, size=6)) +
    scale_fill_manual(name = 'CPU type', 
                      values =c('Graviton'='darkgoldenrod1',
                                'Intel'='dodgerblue',
                                'AMD'='firebrick1'), 
                      labels = c('Graviton','Intel', "AMD"))
  
  if(add_condition_description){
    result_plot <- result_plot + geom_text(data=temp_dt,aes(label=description, 
                                                            x=y_breaks[2]), 
                                           color="black", size=2.5)
  }
  
  result_plot
}
# input_dt <- oltp_test_result
  
best_hourly_load_price_barplot <- function(input_dt, input_qph){
  
  temp_dt <- input_dt
  column_name <- "queries_per_sec"
  temp_dt <- temp_dt[,.(test_run,VM_type,Number_of_threads, "value"=eval(get(column_name)),cpu_amount,price_usd, cpu_type,color)]
  temp_dt <- temp_dt[,.("min"=min(value), "max"=max(value), avg=mean(value)), by=.(VM_type,Number_of_threads, cpu_amount,price_usd, cpu_type,color)]
  temp_dt[,"approx_qph":=round(avg*3600,0)]
  temp_dt[,price_second:=price_usd/3600]
  temp_dt[,request_price:=price_second/avg]
  temp_dt[,dollar_request:=1/request_price]
  temp_dt$x_break_label <- temp_dt$VM_type
  temp_dt[,":="("description"=paste("vCPU:",cpu_amount,"_load:",Number_of_threads,"tps", sep="")) ]
  
  loal_dt <- temp_dt[approx_qph > input_qph] %>% setorder(cpu_amount,Number_of_threads)
  plot_dt <- loal_dt[,.SD[which.min(price_usd)], by = cpu_type]
  
  generated_title <- paste("Best price for :   ", compres_numbers_title(input_qph) ,"    queries per hour")
  list_of_vm <- plot_dt$VM_type %>% unique() %>% sort() %>% paste(collapse = ", ")
  generated_sub_title <- paste("List of the ceapest EC2, that could handle the load:\n", list_of_vm)
  
  plot_dt <- plot_dt[,.(VM_type,price_usd,cpu_type,description)]
  
  plot_dt[,":="("diff_percent"=round((price_usd/max(price_usd)-1)*100     )     )]
  plot_dt[,":="("diff_label"=ifelse(diff_percent == 0, "", paste(diff_percent, "%", sep = "")))]
  
  min_x_break <- round(min(plot_dt$price_usd)* 10-2) / 10
  if(min_x_break < 0){
    min_x_break <- 0
  }
  max_x_break <- round(max(plot_dt$price_usd)* 10+1) / 10
  x_breaks <- seq(min_x_break,max_x_break, 0.05)
  
  setorder(plot_dt, -price_usd)
  
  result_plot <- ggplot(plot_dt, 
                        aes(x=price_usd, 
                            y=reorder(VM_type, price_usd), 
                            fill=cpu_type)) + 
    geom_bar(stat = "identity")  +  
    scale_fill_manual(name = 'CPU type', 
                      values =c('Graviton'='darkgoldenrod1',
                                'Intel'='dodgerblue',
                                'AMD'='firebrick1'), 
                      labels = c('Graviton','Intel', "AMD")) +
    scale_x_continuous(name="price per hour (USD)",
                       breaks = x_breaks) +
    coord_cartesian(xlim = c(min_x_break,max_x_break))+
    labs(title=generated_title, 
         subtitle = generated_sub_title,
         caption = "less is better",
         y="EC2 type") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1),
          axis.text.y = element_text(angle = 45, vjust = 0.9, hjust=1, size=10))
  # result_plot
  setorder(plot_dt, price_usd)
  list_labels <- plot_dt$description
  
  result_plot <- result_plot + guides(y.sec = guide_axis_manual(
    breaks = c(1, 2, 3),
    labels = list_labels))
  
  result_plot <- result_plot + geom_text( aes(x=max(plot_dt$price_usd), y=VM_type,label=diff_label))
  result_plot
}



best_second_load_price_barplot <- function(input_dt, input_qps){
  
  temp_dt <- input_dt
  column_name <- "queries_per_sec"
  temp_dt <- temp_dt[,.(test_run,VM_type,Number_of_threads, "value"=eval(get(column_name)),cpu_amount,price_usd, cpu_type,color)]
  temp_dt <- temp_dt[,.(qps_avg=round(mean(value))), by=.(VM_type,Number_of_threads, cpu_amount,price_usd, cpu_type,color)]
  temp_dt[,":="("description"=paste("vCPU:",cpu_amount, sep="")) ]
  
  
  temp_dt <- temp_dt[qps_avg > input_qps] %>% setorder(cpu_amount,Number_of_threads)
  setorder(temp_dt,price_usd, Number_of_threads)
  plot_dt <- temp_dt[,.SD[which.min(price_usd)], by = cpu_type]
  
  plot_dt <- plot_dt[,.(VM_type,price_usd,cpu_type,description)]
  
  plot_dt[,":="("diff_percent"=round((price_usd/max(price_usd)-1)*100     )     )]
  plot_dt[,":="("diff_label"=ifelse(diff_percent == 0, "", paste(diff_percent, "%", sep = "")))]
  
  
  list_of_vm <- plot_dt$VM_type %>% unique() %>% sort() %>% paste(collapse = ", ")
  generated_title <- paste("Best price for :   ", compres_numbers_title(input_qps) ,"    queries per second")
  generated_sub_title <- paste("List of the ceapest EC2, that could handle the load:\n", list_of_vm)
  
  
  # plot_dt
  min_x_break <- round(min(plot_dt$price_usd)* 10-2) / 10
  if(min_x_break < 0){
    min_x_break <- 0
  }
  max_x_break <- round(max(plot_dt$price_usd)* 10+2) / 10
  x_breaks <- seq(min_x_break,max_x_break, 0.05)
  
  setorder(plot_dt, -price_usd)
  
  result_plot <- ggplot(plot_dt, 
                        aes(x=price_usd, 
                            y=reorder(VM_type, price_usd), 
                            fill=cpu_type)) + 
    geom_bar(stat = "identity")  +  
    scale_fill_manual(name = 'CPU type', 
                      values =c('Graviton'='darkgoldenrod1',
                                'Intel'='dodgerblue',
                                'AMD'='firebrick1'), 
                      labels = c('Graviton','Intel', "AMD")) +
    scale_x_continuous(name="price per hour (USD)",
                       breaks = x_breaks) +
    coord_cartesian(xlim = c(min_x_break,max_x_break))+
    labs(title=generated_title, 
         subtitle = generated_sub_title,
         caption = "less is better",
         y="EC2 type") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1),
          axis.text.y = element_text(angle = 45, vjust = 0.9, hjust=1, size=10))
  
  setorder(plot_dt, price_usd)
  list_labels <- plot_dt$description
  
  result_plot <- result_plot + guides(y.sec = guide_axis_manual(
    breaks = c(1, 2, 3),
    labels = list_labels))
  
  result_plot <- result_plot + geom_text( aes(x=max(plot_dt$price_usd), y=VM_type,label=diff_label))
  result_plot
}

heatmap_cheapest_for_hourly_load <- function(input_dt){
  
  temp_dt <- input_dt
  column_name <- "queries_per_sec"
  temp_dt <- temp_dt[,.(test_run,VM_type,Number_of_threads, "value"=eval(get(column_name)),cpu_amount,price_usd, cpu_type,color)]
  temp_dt <- temp_dt[,.("min"=min(value), "max"=max(value), avg=mean(value)), by=.(VM_type,Number_of_threads, cpu_amount,price_usd, cpu_type,color)]
  temp_dt[,"approx_qph":=round(avg*3600,0)]
  temp_dt[,price_second:=price_usd/3600]
  temp_dt[,request_price:=price_second/avg]
  temp_dt[,dollar_request:=1/request_price]
  temp_dt$x_break_label <- temp_dt$VM_type
  temp_dt[,":="("description"=paste("vCPU:",cpu_amount,"_load:",Number_of_threads,"tps", sep="")) ]
  
  load_list <- c(25,50,75,100,150,200,250,300,350,400,450,500,750,1000,1250,1500,1750,2000,2250,2500) * 1000000
  
  max(temp_dt$approx_qph)
  
  plot_dt <- temp_dt[,.(VM_type,cpu_amount,Number_of_threads,price_usd,cpu_type,approx_qph)]
  temp_dt_list <- list()
  i <- 1
  for (i in seq_along(1:length(load_list))) {
    iter_load <- load_list[i]
    tdt <- plot_dt[approx_qph > iter_load] %>% setorder(cpu_amount,Number_of_threads)
    tdt <- tdt[,.SD[which.min(price_usd)], by = .(cpu_type,cpu_amount)]
    tdt$cheapest_for_load <- iter_load
    temp_dt_list[[i]] <- tdt
    rm(tdt)
  }
  
  plot_dt <- rbindlist(temp_dt_list)
  pdt <- plot_dt[,.SD[which.min(price_usd)], by=.(cpu_amount,cheapest_for_load)]
  
  pdt3 <- pdt[,.(cpu_amount,cheapest_for_load,cpu_type,VM_type,price_usd)]
  pdt4 <- pdt3 %>% complete(cheapest_for_load,nesting(cpu_amount))
  
  levels_comr_values <- compres_numbers(pdt4$cheapest_for_load) %>% unique()
  
  pdt4$compessed_value <- factor(compres_numbers(pdt4$cheapest_for_load), levels = levels_comr_values)
  
  
  result_plot <- ggplot(pdt4, aes(x=as.factor(cpu_amount), 
                                  y=compessed_value, 
                                  fill=cpu_type)) +
    geom_tile(color = "white",
              lwd = 1.5,
              linetype = 1)+
    scale_fill_manual(name = 'CPU type', 
                      values =c('Graviton'='darkgoldenrod1',
                                'Intel'='dodgerblue',
                                'AMD'='firebrick1'), 
                      labels = c('Graviton','Intel', "AMD"),
                      na.value = 'azure')+
    geom_text(aes(label=VM_type), size=3) +
    labs(title = "Cheapest EC2 for hourly load",
         subtitle ="",
         caption = "M - Millions, B - Billions",
         x="Cpu amount",
         y="Load (read transaction per hour)")
  result_plot
}

heatmap_cheapest_for_second_load <- function(input_dt){
  
  temp_dt <- input_dt
  column_name <- "queries_per_sec"
  temp_dt <- temp_dt[,.(test_run,VM_type,Number_of_threads, "value"=eval(get(column_name)),cpu_amount,price_usd, cpu_type,color)]
  temp_dt <- temp_dt[,.("avg_qps"=mean(value)), by=.(VM_type,Number_of_threads, cpu_amount,price_usd, cpu_type,color)]
  temp_dt[,":="("description"=paste("vCPU:",cpu_amount,"_load:",Number_of_threads,"tps", sep="")) ]
  
  # min(temp_dt$avg_qps)
  # max(temp_dt$avg_qps)
  
  load_list <- list_qps_load <- c(1,5,10,20,30,40, 50,75, 100,150,200,250,300,350,400,450,500,550,600,650,700,750,800) * 1000 
  
  tt <- temp_dt[,.(VM_type,cpu_amount,Number_of_threads,price_usd,cpu_type,avg_qps)]
  tt[avg_qps > 1000][cpu_amount ==2]
  
  plot_dt <- temp_dt[,.(VM_type,cpu_amount,Number_of_threads,price_usd,cpu_type,avg_qps)]
  temp_dt_list <- list()
  i <- 1
  for (i in seq_along(1:length(load_list))) {
    print(i)
    print(load_list[i])
    iter_load <- load_list[i]
    tdt <- plot_dt[avg_qps > iter_load] %>% setorder(cpu_amount,Number_of_threads)
    tdt <- tdt[,.SD[which.min(price_usd)], by = .(cpu_type,cpu_amount)]
    tdt$cheapest_for_load <- iter_load
    temp_dt_list[[i]] <- tdt
    rm(tdt)
  }
  
  plot_dt <- rbindlist(temp_dt_list)
  pdt <- plot_dt[,.SD[which.min(price_usd)], by=.(cpu_amount,cheapest_for_load)]
  
  pdt3 <- pdt[,.(cpu_amount,cheapest_for_load,cpu_type,VM_type,price_usd)]
  pdt4 <- pdt3 %>% complete(cheapest_for_load,nesting(cpu_amount))
  
  levels_comr_values <- compres_numbers(pdt4$cheapest_for_load) %>% unique()
  
  pdt4$compessed_value <- factor(compres_numbers(pdt4$cheapest_for_load), levels = levels_comr_values)
  
  
  result_plot <- ggplot(pdt4, aes(x=as.factor(cpu_amount), 
                                  y=compessed_value, 
                                  fill=cpu_type)) +
    geom_tile(color = "white",
              lwd = 1.5,
              linetype = 1)+
    scale_fill_manual(name = 'CPU type', 
                      values =c('Graviton'='darkgoldenrod1',
                                'Intel'='dodgerblue',
                                'AMD'='firebrick1'), 
                      labels = c('Graviton','Intel', "AMD"),
                      na.value = 'azure')+
    geom_text(aes(label=VM_type), size=3) +
    labs(title = "Cheapest EC2 for second load",
         subtitle ="",
         caption = "K(kilo)-thouthands, M - Millions, B - Billions",
         x="vCPU amount in EC2 instance",
         y="Load (read transaction per second)")
  result_plot
}

# input_dt <- all_scenarios_simplify

cheapest_ec2_for_particular_load_bar_plot <- function(input_dt, 
                                                      input_title="Cheapest EC2 for load particular load", 
                                                      input_subtitle="", 
                                                      input_caption="",
                                                      input_qps_load_list=NULL){
  temp_dt <- input_dt
  column_name <- "queries_per_sec"
  temp_dt <- temp_dt[,.(test_run,VM_type,Number_of_threads, "value"=eval(get(column_name)),cpu_amount,price_usd, cpu_type,color)]
  temp_dt <- temp_dt[,.(qps_avg=round(mean(value))), by=.(VM_type,Number_of_threads, cpu_amount,price_usd, cpu_type,color)]
  temp_dt[,":="("description"=paste(VM_type,"_vCPU:",cpu_amount, sep="")) ]
  
  if(!is.null(input_qps_load_list)){
    list_qps_load <- input_qps_load_list
  } else {
    list_qps_load <- c(1,5,10,15,20,25,30,40,50,75,100,125,150,175,200,225,250,275,300,325,350,375,400,425,450,475,500,550,600,650,700,750,800) * 1000
  }
  
  
  temp_list <- list()
  for(i_load in seq_along(1:length(list_qps_load))){
    load <- list_qps_load[i_load]
    
    ttt<- temp_dt[qps_avg > load][,.SD[which.min(price_usd)]]
    ttt$load_factor <- load
    temp_list[[i_load]] <- ttt
  }
  plot_dt <- rbindlist(temp_list)
  
  result_plot <- ggplot(plot_dt, aes(x=price_usd,
                                     y=as.factor(load_factor),
                                     fill=cpu_type)) + 
    geom_bar(stat = "identity")  +  
    scale_fill_manual(name = 'CPU type', 
                      values =c('Graviton'='darkgoldenrod1',
                                'Intel'='dodgerblue',
                                'AMD'='firebrick1'), 
                      labels = c('Graviton','Intel', "AMD")) +
    scale_x_sqrt(name="price per hour (USD)",
                 breaks = unique(plot_dt$price_usd)) +
    coord_cartesian(xlim = c(0,max(plot_dt$price_usd)+1)) +
    geom_text(aes(x=0.015,label=description), size=3) +
    labs(title=input_title,
         subtitle = input_subtitle,
         caption = input_caption,
         y="load (read transactions per secoond)") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1),
          axis.text.y = element_text(angle = 45, vjust = 0.9, hjust=1, size=10))
  return(result_plot)
}
