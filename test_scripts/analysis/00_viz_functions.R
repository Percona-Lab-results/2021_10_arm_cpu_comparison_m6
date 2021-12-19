library(ggplot2)
library(plotly)
library(ggrepel)
library(ggforce)
library(RColorBrewer) 
library(scales)
library(magick)
library(cowplot)

save_plot <- function(file_path_name,plot_to_save){
  if(!is.null(file_path_name)){
    fileName_to_save <- paste(file_path_name,".png",sep="")
    ggsave(fileName_to_save,plot_to_save,width = 16, height = 9, dpi = 350, units = "in", device='png')
    print(paste("plot successfully saved:",fileName_to_save))
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
         fill = "concurenncy:\nAmount of threads") + 
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
    labs(x="concurenncy:\nAmount of threads",
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
    xaxis_label = "concurenncy: amount of threads"
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
  # temp_dt[,x_break_label:=gsub("m5\\.","Intel\nm5\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("m5a\\.","AMD\nm5a\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("m6g\\.","Graviton\nm6g\\.",x_break_label)]
  # 
  # 
  # temp_dt[,x_break_label:=gsub("c5\\.","Intel\nm5\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("c5a\\.","AMD\nm5a\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("c6g\\.","Graviton\nc6g\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("t3\\.","Intel\nt3\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("t3a\\.","AMD\nmt3a\\.",x_break_label)]
  # temp_dt[,x_break_label:=gsub("t4g\\.","Graviton\nt4g\\.",x_break_label)]
  # 
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
  
  list_of_threads <- temp_dt$Number_of_threads %>% unique() %>% as.numeric() %>% sort() %>% paste( collapse= ", ")
  
  tdt <-  temp_dt[,.(cpu_type,cpu_amount,Number_of_threads, avg)]
  
  dd_dt <- data.table::dcast(tdt,cpu_amount + Number_of_threads ~ cpu_type)
  
  if(target_comparing_cpu == "intel"){
    dd_dt <- dd_dt[,AMD:=NULL] %>% na.omit()
    dd_dt[,diff_GI_abs :=Graviton - Intel]
    dd_dt[,diff_GI_rel :=round(Graviton/Intel-1,2)*100]
    dd_dt[,cpu_type_winner:=ifelse((Graviton/Intel-1) > 0, "Graviton","Intel")]
    color_scale <- scale_fill_manual(name = 'CPU type', 
                                     values =c('Graviton'='darkgoldenrod1',
                                               'Intel'='dodgerblue',
                                               'AMD'='firebrick1'), 
                                     labels = c('Graviton','Intel', "AMD"))
  } else {
    dd_dt <- dd_dt[,Intel:=NULL] %>% na.omit()
    dd_dt[,diff_GI_abs :=Graviton - AMD]
    dd_dt[,diff_GI_rel :=round(Graviton/AMD-1,2)*100]
    dd_dt[,cpu_type_winner:=ifelse((Graviton/AMD-1) > 0, "Graviton","AMD")]
    color_scale <- scale_fill_manual(name = 'CPU type', 
                                     values =c('Graviton'='darkgoldenrod1',
                                               'Intel'='dodgerblue',
                                               'AMD'='firebrick1'), 
                                     labels = c('Graviton','Intel', "AMD"))
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
    title_generated <- paste("Relative (%) performance difference between ",target_comparing_cpu," and Graviton (queries per second)", sep="")
    
    
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


efficient_comparison_point_plot <- function(input_dt, facet_threads=FALSE){
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
  
  
  
  
  x_breaks <- c(10000000, 25000000, 50000000,75000000, 100000000,150000000,
                200000000, 250000000,300000000,350000000,400000000,450000000,
                500000000, 550000000,600000000,650000000,700000000,
                750000000,800000000,850000000, 900000000,950000000, 1000000000,
                1100000000)
  y_breaks <- c(10000000, 25000000, 50000000,75000000, 100000000,150000000,
                200000000, 250000000,300000000,350000000,400000000,450000000,
                500000000, 550000000,600000000,650000000,700000000,
                750000000,800000000,850000000, 900000000,950000000,1000000000,
                1100000000, 1200000000, 1300000000, 1400000000, 1500000000,
                1600000000, 1700000000, 1800000000, 1900000000, 2000000000,2100000000)
  
  num_thread_levels <- temp_dt$Number_of_threads %>% unique() %>% as.numeric()%>% sort()
  temp_dt$Number_of_threads <- factor(temp_dt$Number_of_threads, levels = num_thread_levels)
  
  result_plot <- ggplot(temp_dt,aes(x=dollar_request,
                     y=approx_qph,
                     shape=as.factor(cpu_amount),
                     color=as.factor(cpu_type),
                     size=Number_of_threads)) +
    geom_point() +
    labs(title = "CPU types efficient comparison (for MySQL)",
         x="Amount of requests per 1 USD",
         y="Approximate amount of requests per hour",
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
  result_plot
}
