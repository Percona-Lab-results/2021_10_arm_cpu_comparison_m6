library(ggh4x)

oltp_test_result

head(oltp_test_result)
temp_dt <- oltp_test_result
column_name <- "queries_per_sec"
temp_dt <- temp_dt[,.(test_run,VM_type,Number_of_threads, "value"=eval(get(column_name)),cpu_amount,price_usd, cpu_type,color)]
temp_dt <- temp_dt[,.("min"=min(value), "max"=max(value), avg=mean(value)), by=.(VM_type,Number_of_threads, cpu_amount,price_usd, cpu_type,color)]
temp_dt[,"approx_qph":=round(avg*3600,0)]
temp_dt[,price_second:=price_usd/3600]
temp_dt[,request_price:=price_second/avg]
temp_dt[,dollar_request:=1/request_price]
temp_dt$x_break_label <- temp_dt$VM_type

temp_dt <- temp_dt[,.(VM_type,cpu_amount,cpu_type,Number_of_threads=as.numeric(Number_of_threads),color,price_usd,approx_qph,dollar_request)]
setorder(temp_dt,cpu_type,VM_type,cpu_amount,Number_of_threads)

compres_numbers <- function(number) { 
  div <- findInterval(as.numeric(gsub("\\,", "", number)), 
                      c(0, 1e3, 1e6, 1e9, 1e12) )  # modify this if negative numbers are possible
  paste(round( as.numeric(gsub("\\,","",number))/10^(3*(div-1)), 2), 
        c("","K","M","B","T")[div] )}


temp_dt[,":="("dollar_request_round"=compres_numbers(dollar_request), "approx_qph_round"=compres_numbers(approx_qph))]
temp_dt <- temp_dt[!(VM_type %like% "t3")][!(VM_type %like% "t4")]
temp_dt[,":="("title_VM"=paste(VM_type,cpu_amount,Number_of_threads, sep="_")) ]
temp_dt[,":="("desciption"=paste("vCPU:",cpu_amount,"_load:",Number_of_threads, sep="")) ]





table(temp_dt$VM_type)
oltp_test_result <- oltp_test_result[!(VM_type %like% "t3")][!(VM_type %like% "t4")]
m5_scenario <- oltp_test_result[VM_type %like% c("m5|m6g")]
table(m5_scenario$VM_type)
c5_scenario <- oltp_test_result[VM_type %like% c("c5|c6")]
table(c5_scenario$VM_type)
m6_scenario <- oltp_test_result[VM_type %like% c("m6")]
table(m6_scenario$VM_type)




setorder(temp_dt, -approx_qph)
top_20_performance <- head(temp_dt,20)
setorder(temp_dt, -dollar_request)
top_20_economic <- head(temp_dt,20)

y_breaks <- c(10, 25, 50,75, 100,150,
              200, 250,300,350,400,450,
              500, 550,600,650,700,
              750,800,850, 900,950,1000,
              1100, 1200, 1300, 1400, 1500,
              1600, 1700, 1800, 1900, 2000,2100,2200,2300,2400,2500,2600,2700,2800,2900,3000) * 1000000

min_y_break <- round(min(top_20_economic$dollar_request)/10000000)*10000000-10000000
max_y_break <- round(max(top_20_economic$dollar_request)/10000000)*10000000+10000000
y_breaks <- seq(min_y_break,max_y_break,10000000)

top_20_economic$x_position <- seq(1, NROW(top_20_economic))
ggplot(top_20_economic, aes(x=reorder(title_VM, -dollar_request),
                            y=dollar_request, 
                            fill=cpu_type)) + 
  geom_bar(stat = "identity", 
           position="dodge", 
           size=2)+
  scale_y_continuous(breaks=y_breaks,
                     labels=label_number(suffix = " B", scale = 1e-9))+
                      coord_cartesian(ylim=c(min_y_break, max_y_break))+
  xlab("instance type")+
  scale_x_discrete(labels=top_20_economic$VM_type)+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1),
        axis.text.y = element_text(angle = -45, vjust = 0.9, hjust=1, size=6)) +
  scale_fill_manual(name = 'color:\nCPU type', 
                     values =c('Graviton'='darkgoldenrod1',
                               'Intel'='dodgerblue',
                               'AMD'='firebrick1'), 
                     labels = c('Graviton','Intel', "AMD"))+
  geom_text(aes(label=desciption, x=x_position,y=y_breaks[3], angle=90), color="black", size=3.5)
  # geom_text(aes(label=desciption, x=x_position,y=dollar_request, angle=45), 
  #           color="black", size=3.5,
  #           position = position_dodge(width = 1),
  #           hjust = 0)
  # geom_label_repel(aes(label = desciption, y = dollar_request),nudge_x = 0.2, force = 2, 
  #                  segment.alpha = 0, alpha = 1,
  #                  seed = 555)

generated_title <- "Top 20 economicaly efficient EC2 instance"
generated_subtitle <- "dummy subtitle"
generation_caption <- "dummy caption"
generated_x_label <- "approximate read queries could be generated per 1 USD"


ggplot(top_20_economic, aes(y=reorder(title_VM, dollar_request),
                            x=dollar_request, 
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
  scale_y_discrete(labels=rev(top_20_economic$VM_type))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1),
        axis.text.y = element_text(angle = 0, vjust = 0.9, hjust=1, size=6)) +
  scale_fill_manual(name = 'CPU type', 
                    values =c('Graviton'='darkgoldenrod1',
                              'Intel'='dodgerblue',
                              'AMD'='firebrick1'), 
                    labels = c('Graviton','Intel', "AMD")) +
  geom_text(aes(label=desciption, x=y_breaks[2]), color="black", size=3.5)

column_name <- "dollar_request"
column_name <- "queries_per_sec"




top_efficient(oltp_test_result,column_name="queries_per_sec" ,add_condition_description=FALSE,USD_efficiency=TRUE)
top_efficient(oltp_test_result,column_name="queries_per_sec" ,add_condition_description=TRUE,USD_efficiency=TRUE, limit=20)
top_efficient(oltp_test_result,column_name="queries_per_sec" ,add_condition_description=TRUE,USD_efficiency=FALSE, limit=20)


top_efficient(m5_scenario,column_name="queries_per_sec" ,add_condition_description=FALSE,USD_efficiency=TRUE)
top_efficient(m5_scenario,column_name="queries_per_sec" ,add_condition_description=TRUE,USD_efficiency=TRUE, limit=20)


top_efficient(m6_scenario,column_name="queries_per_sec" ,add_condition_description=FALSE,USD_efficiency=TRUE)
top_efficient(m6_scenario,column_name="queries_per_sec" ,add_condition_description=TRUE,USD_efficiency=TRUE, limit=20)


top_efficient(c5_scenario,column_name="queries_per_sec" ,add_condition_description=FALSE,USD_efficiency=TRUE)
top_efficient(c5_scenario,column_name="queries_per_sec" ,add_condition_description=TRUE,USD_efficiency=TRUE, limit=20)



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
    generated_title="Dummy title",
    generated_subtitle="dummy subtitle",
    generation_caption="dummy caption",
    generated_x_label <- "Amount of queries could be done for 1 USD"
  } else {
    temp_dt[,":="("target_column"=eval(get("approx_qph")))]
    generated_title="Dummy title",
    generated_subtitle="dummy subtitle",
    generation_caption="dummy caption",
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





input_dt <- oltp_test_result

temp_dt <- input_dt
column_name <- "queries_per_sec"
temp_dt <- temp_dt[,.(test_run,VM_type,Number_of_threads, "value"=eval(get(column_name)),cpu_amount,price_usd, cpu_type,color)]
temp_dt <- temp_dt[,.("min"=min(value), "max"=max(value), avg=mean(value)), by=.(VM_type,Number_of_threads, cpu_amount,price_usd, cpu_type,color)]
temp_dt[,"approx_qph":=round(avg*3600,0)]
temp_dt[,price_second:=price_usd/3600]
temp_dt[,request_price:=price_second/avg]
temp_dt[,dollar_request:=1/request_price]
temp_dt$x_break_label <- temp_dt$VM_type

sort_by_USD <- TRUE
# setorder(temp_dt, -target_column)
if(sort_by_USD){
  setorder(temp_dt, -dollar_request)
  temp_dt <- top_20_economic
  generated_subtitle <- "sorted by requests by 1USD"
  temp_dt$target_col <- temp_dt$dollar_request
  
} else {
  setorder(temp_dt, -approx_qph)
  temp_dt <- top_20_performance
  generated_subtitle <- "sorted by requests per hour"
  temp_dt$target_col <- temp_dt$approx_qph
}
limit <- 10
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



ggplot(temp_dt) +
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
  labels = c('Requests_per_USD','Queries per hour'))+
  scale_y_continuous(labels=rev(temp_dt$VM_type),
                     breaks=c(1:length(temp_dt$VM_type)),name="AWS EC2 instance",
                   sec.axis=sec_axis(~.,labels=rev(temp_dt$desciption),
                                     breaks=c(1:length(temp_dt$desciption)), name="Conditions") )+
  labs(title = "Economic efficiency of CPU's based on AWS EC2 instances for Mysql",
       subtitle = generated_subtitle,
       x="Amount of transaction (read query)")





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
  temp_dt[,":="("description"=paste("vCPU:",cpu_amount,"_load:",Number_of_threads, sep="")) ]
  
  
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

getwd()


p_71_all_scenarios_econ_e <- top_efficient_lollipop(oltp_test_result,sort_by_USD=TRUE, limit=10)
save_plot("071_all_scenarios_econ_e.png", p_71_all_scenarios_econ_e)
p_72_all_scenarios_perf_e <- top_efficient_lollipop(oltp_test_result,sort_by_USD=FALSE, limit=10)
save_plot("072_all_scenarios_perf_e.png", p_72_all_scenarios_perf_e)

c5_scenarios_econ_e <- top_efficient_lollipop(c5_scenario,sort_by_USD=TRUE, limit=20)
c5_scenarios_perf_e <- top_efficient_lollipop(c5_scenario,sort_by_USD=FALSE, limit=20)


m5_scenarios_econ_e <- top_efficient_lollipop(m5_scenario,sort_by_USD=TRUE, limit=20)
m5_scenarios_perf_e <- top_efficient_lollipop(m5_scenario,sort_by_USD=FALSE, limit=20)


m6_scenarios_econ_e <- top_efficient_lollipop(m6_scenario,sort_by_USD=TRUE, limit=20)
m6_scenarios_perf_e <- top_efficient_lollipop(m6_scenario,sort_by_USD=FALSE, limit=20)

all_equal_test_result <- oltp_test_result[cpu_amount == Number_of_threads]

all_equal_scenarios_econ_e <- top_efficient_lollipop(all_equal_test_result,sort_by_USD=TRUE, limit=20)
all_equal_scenarios_perf_e <- top_efficient_lollipop(all_equal_test_result,sort_by_USD=FALSE, limit=20)

all_max_load_test_result <- oltp_test_result[64 == Number_of_threads]

all_max_load_scenarios_econ_e <- top_efficient_lollipop(all_max_load_test_result,sort_by_USD=TRUE, limit=20)
all_max_load_scenarios_perf_e <- top_efficient_lollipop(all_max_load_test_result,sort_by_USD=FALSE, limit=20)

### small
small_test_result <- oltp_test_result[ec2_type == "small"]

all_max_load_scenarios_econ_e <- top_efficient_lollipop(small_test_result,sort_by_USD=TRUE, limit=20)
all_max_load_scenarios_perf_e <- top_efficient_lollipop(small_test_result,sort_by_USD=FALSE, limit=20)
### medium
medium_test_result <- oltp_test_result[ec2_type == "medium"]

all_max_load_scenarios_econ_e <- top_efficient_lollipop(medium_test_result,sort_by_USD=TRUE, limit=20)
all_max_load_scenarios_perf_e <- top_efficient_lollipop(medium_test_result,sort_by_USD=FALSE, limit=20)

### large
large_test_result <- oltp_test_result[ec2_type == "large"]

all_max_load_scenarios_econ_e <- top_efficient_lollipop(large_test_result,sort_by_USD=TRUE, limit=20)
all_max_load_scenarios_perf_e <- top_efficient_lollipop(large_test_result,sort_by_USD=FALSE, limit=20)








top_efficient(oltp_test_result,column_name="queries_per_sec" ,add_condition_description=FALSE,USD_efficiency=TRUE)
top_efficient(oltp_test_result,column_name="queries_per_sec" ,add_condition_description=TRUE,USD_efficiency=TRUE, limit=20)
top_efficient(oltp_test_result,column_name="queries_per_sec" ,add_condition_description=TRUE,USD_efficiency=FALSE, limit=20)


top_efficient(m5_scenario,column_name="queries_per_sec" ,add_condition_description=FALSE,USD_efficiency=TRUE)
top_efficient(m5_scenario,column_name="queries_per_sec" ,add_condition_description=TRUE,USD_efficiency=TRUE, limit=20)


top_efficient(m6_scenario,column_name="queries_per_sec" ,add_condition_description=FALSE,USD_efficiency=TRUE)
top_efficient(m6_scenario,column_name="queries_per_sec" ,add_condition_description=TRUE,USD_efficiency=TRUE, limit=20)


top_efficient(c5_scenario,column_name="queries_per_sec" ,add_condition_description=FALSE,USD_efficiency=TRUE)
top_efficient(c5_scenario,column_name="queries_per_sec" ,add_condition_description=TRUE,USD_efficiency=TRUE, limit=20)


### bar plot find cheapest 

input_dt <- oltp_test_result

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

table(temp_dt$approx_qph) %>%  sort()


  
list_qph_load <- c(500, 1000, 1500, 2000, 2200 ) * 1000000

input_qph <- list_qph_load[2]

loal_dt <- temp_dt[approx_qph > input_qph] %>% setorder(cpu_amount,Number_of_threads)
plot_dt <- loal_dt[,.SD[which.min(price_usd)], by = cpu_type]

generated_title <- paste("Best price for :   ", compres_numbers_title(input_qph) ,"    queries per hour")
list_of_vm <- plot_dt$VM_type %>% unique() %>% sort() %>% paste(collapse = ", ")
generated_sub_title <- paste("List of the ceapest EC2, that could handle the load:\n", list_of_vm)


# irislabs1 <- plot_dt$VM_type
# irislabs2 <- plot_dt$description
plot_dt <- plot_dt[,.(VM_type,price_usd,cpu_type,description)]

plot_dt[,":="("diff_percent"=round((max(price_usd)/price_usd-1)*-100     )     )]
plot_dt[,":="("diff_label"=ifelse(diff_percent == 0, "", paste(diff_percent, "%", sep = "")))]


# plot_dt
min_x_break <- round(min(plot_dt$price_usd)* 10-2) / 10
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
  scale_x_log10(name="price per hour (USD)",
                     breaks = x_breaks) +
  coord_cartesian(xlim = c(min_x_break,max_x_break))+
  labs(title=generated_title, 
       subtitle = generated_sub_title,
       caption = "less is better",
       y="EC2 type") +
theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1),
      axis.text.y = element_text(angle = 45, vjust = 0.9, hjust=1, size=10))
result_plot
setorder(plot_dt, price_usd)
list_labels <- plot_dt$description

result_plot <- result_plot + guides(y.sec = guide_axis_manual(
  breaks = c(1, 2, 3),
  labels = list_labels))

result_plot <- result_plot + geom_text( aes(x=max(plot_dt$price_usd), y=VM_type,label=diff_label))
result_plot

list_qph_load <- c(500, 1000, 1500, 2000, 2100 ) * 1000000

input_qph <- list_qph_load[1]

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
  
  plot_dt[,":="("diff_percent"=round((max(price_usd)/price_usd-1)*-100     )     )]
  plot_dt[,":="("diff_label"=ifelse(diff_percent == 0, "", paste(diff_percent, "%", sep = "")))]

  min_x_break <- round(min(plot_dt$price_usd)* 10-2) / 10
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

p_500 <- best_hourly_load_price_barplot(oltp_test_result, list_qph_load[1])
p_1000 <- best_hourly_load_price_barplot(oltp_test_result, list_qph_load[2])
p_1500 <- best_hourly_load_price_barplot(oltp_test_result, list_qph_load[3])
p_2000 <- best_hourly_load_price_barplot(oltp_test_result, list_qph_load[4])
p_2200 <- best_hourly_load_price_barplot(oltp_test_result, list_qph_load[5])



### best QPS price
# input_qps_load <- 
max(temp_dt$qps_avg)
min(temp_dt$qps_avg)
list_qps_load <- c(10, 50,100,250,300,350,400,500) * 1000

input_qps <- list_qps_load[1]
  
temp_dt <- input_dt
column_name <- "queries_per_sec"
temp_dt <- temp_dt[,.(test_run,VM_type,Number_of_threads, "value"=eval(get(column_name)),cpu_amount,price_usd, cpu_type,color)]
temp_dt <- temp_dt[,.(qps_avg=round(mean(value))), by=.(VM_type,Number_of_threads, cpu_amount,price_usd, cpu_type,color)]
temp_dt[,":="("description"=paste("vCPU:",cpu_amount, sep="")) ]


temp_dt <- temp_dt[qps_avg > input_qps] %>% setorder(cpu_amount,Number_of_threads)
setorder(temp_dt,price_usd, Number_of_threads)
plot_dt <- temp_dt[,.SD[which.min(price_usd)], by = cpu_type]

# temp_dt[VM_type == "c5a.large"]
# temp_dt[cpu_amount <=4]
# ?which.min


plot_dt <- plot_dt[,.(VM_type,price_usd,cpu_type,description)]

plot_dt[,":="("diff_percent"=round((max(price_usd)/price_usd-1)*-100     )     )]
plot_dt[,":="("diff_label"=ifelse(diff_percent == 0, "", paste(diff_percent, "%", sep = "")))]


list_of_vm <- plot_dt$VM_type %>% unique() %>% sort() %>% paste(collapse = ", ")
generated_title <- paste("Best price for :   ", compres_numbers_title(input_qps) ,"    queries per second")
generated_sub_title <- paste("List of the ceapest EC2, that could handle the load:\n", list_of_vm)


# plot_dt
min_x_break <- round(min(plot_dt$price_usd)* 10-2) / 10
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
# result_plot
setorder(plot_dt, price_usd)
list_labels <- plot_dt$description

result_plot <- result_plot + guides(y.sec = guide_axis_manual(
  breaks = c(1, 2, 3),
  labels = list_labels))

result_plot <- result_plot + geom_text( aes(x=max(plot_dt$price_usd), y=VM_type,label=diff_label))
result_plot

plot_dt


input_dt <- oltp_test_result
best_second_load_price_barplot <- function(input_dt, input_qps){
  
  temp_dt <- input_dt
  column_name <- "queries_per_sec"
  temp_dt <- temp_dt[,.(test_run,VM_type,Number_of_threads, "value"=eval(get(column_name)),cpu_amount,price_usd, cpu_type,color)]
  temp_dt <- temp_dt[,.(qps_avg=round(mean(value))), by=.(VM_type,Number_of_threads, cpu_amount,price_usd, cpu_type,color)]
  temp_dt[,":="("description"=paste("vCPU:",cpu_amount, sep="")) ]
  
  
  temp_dt <- temp_dt[qps_avg > input_qps] %>% setorder(cpu_amount,Number_of_threads)
  setorder(temp_dt,price_usd, Number_of_threads)
  plot_dt <- temp_dt[,.SD[which.min(price_usd)], by = cpu_type]
  
  # temp_dt[VM_type == "c5a.large"]
  # temp_dt[cpu_amount <=4]
  # ?which.min
  
  
  plot_dt <- plot_dt[,.(VM_type,price_usd,cpu_type,description)]
  
  plot_dt[,":="("diff_percent"=round((max(price_usd)/price_usd-1)*-100     )     )]
  plot_dt[,":="("diff_label"=ifelse(diff_percent == 0, "", paste(diff_percent, "%", sep = "")))]
  
  
  list_of_vm <- plot_dt$VM_type %>% unique() %>% sort() %>% paste(collapse = ", ")
  generated_title <- paste("Best price for :   ", compres_numbers_title(input_qps) ,"    queries per second")
  generated_sub_title <- paste("List of the ceapest EC2, that could handle the load:\n", list_of_vm)
  
  
  # plot_dt
  min_x_break <- round(min(plot_dt$price_usd)* 10-2) / 10
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

list_qps_load <- c(10, 50,100,250,300,350,400,500) * 1000

input_qps <- list_qps_load[1]
p_10 <- best_second_load_price_barplot(oltp_test_result, list_qps_load[1])
p_50 <- best_second_load_price_barplot(oltp_test_result, list_qps_load[2])
p_100 <- best_second_load_price_barplot(oltp_test_result, list_qps_load[3])
p_250 <- best_second_load_price_barplot(oltp_test_result, list_qps_load[4])
p_300 <- best_second_load_price_barplot(oltp_test_result, list_qps_load[5])
p_350 <- best_second_load_price_barplot(oltp_test_result, list_qps_load[6])
p_400 <- best_second_load_price_barplot(oltp_test_result, list_qps_load[7])
p_500 <- best_second_load_price_barplot(oltp_test_result, list_qps_load[8])

####################
### best_price_for_multiple load ############


temp_dt <- input_dt[Number_of_threads == 128]
column_name <- "queries_per_sec"
temp_dt <- temp_dt[,.(test_run,VM_type,Number_of_threads, "value"=eval(get(column_name)),cpu_amount,price_usd, cpu_type,color)]
temp_dt <- temp_dt[,.(qps_avg=round(mean(value))), by=.(VM_type,Number_of_threads, cpu_amount,price_usd, cpu_type,color)]
temp_dt[,":="("description"=paste(VM_type,"_vCPU:",cpu_amount, sep="")) ]

list_qps_load <- c(1,5,10,15,20,25,30,40,50,75,100,125,150,175,200,225,250,275,300,325,350,375,400,425,450,475,500,550,600,650,700,750,800) * 1000
temp_list <- list()
# i_load <- 1
for(i_load in seq_along(1:length(list_qps_load))){
  load <- list_qps_load[i_load]
  
  ttt<- temp_dt[qps_avg > load][,.SD[which.min(price_usd)]]
  ttt$load_factor <- load
  temp_list[[i_load]] <- ttt
}
plot_dt <- rbindlist(temp_list)
# plot_dt$load_f <- compres_numbers(plot_dt$load_factor)
# plot_dt <- plot_dt[,.(VM_type,price_usd,cpu_type,description)]

ggplot(plot_dt, aes(x=price_usd,
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
  labs(title="generated_title", 
       subtitle = "DUMMY subtitle",
       caption = "",
       y="load (read transactions per secoond)") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1),
        axis.text.y = element_text(angle = 45, vjust = 0.9, hjust=1, size=10))

cheapest_ec2_for_particular_load_bar_plot <- function(input_dt, 
                                                      input_title="Cheapest EC2 for load", 
                                                      input_subtitle="", 
                                                      input_caption="",
                                                      input_qps_load_list=NULL){
  temp_dt <- input_dt[Number_of_threads == 128]
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

cheapest_ec2_for_particular_load_bar_plot(oltp_test_result)

# label_number(plot_dt$load_factor,suffix = " L", scale = 1e-3)


#### heatmap hourlycheapest price load ########

input_dt <- oltp_test_result

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
  print(i)
  print(load_list[i])
  iter_load <- load_list[i]
  tdt <- plot_dt[approx_qph > iter_load] %>% setorder(cpu_amount,Number_of_threads)
  # setorder(temp_dt,price_usd, Number_of_threads)
  tdt <- tdt[,.SD[which.min(price_usd)], by = .(cpu_type,cpu_amount)]
  tdt$cheapest_for_load <- iter_load
  temp_dt_list[[i]] <- tdt
  rm(tdt)
}

plot_dt <- rbindlist(temp_dt_list)
pdt <- plot_dt[,.SD[which.min(price_usd)], by=.(cpu_amount,cheapest_for_load)]

# plot_dt[cpu_amount == 2]
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


heatmap_cheapest_for_load(oltp_test_result)

heatmap_cheapest_for_load <- function(input_dt){
  
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

### heatmap cheapest per second ###########

input_dt <- oltp_test_result

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

#######################################












###############################3
all_equal_test_result
input_dt <- all_equal_test_result
temp_dt <- input_dt
column_name <- "queries_per_sec"
temp_dt <- temp_dt[,.(test_run,VM_type,Number_of_threads, "value"=eval(get(column_name)),cpu_amount,price_usd, cpu_type,color)]
temp_dt <- temp_dt[,.("avg_qps"=mean(value)), by=.(VM_type,Number_of_threads, cpu_amount,price_usd, cpu_type,color)]
temp_dt[,":="("description"=paste("vCPU:",cpu_amount,"_load:",Number_of_threads,"tps", sep="")) ]
temp_dt[,"approx_qph":=round(avg_qps*3600,0)]
temp_dt[,price_second:=price_usd/3600]
temp_dt[,request_price:=price_second/avg_qps]
temp_dt[,dollar_request:=1/request_price]
gen_price <- function(input_thouthands,request_price,price_usd){
  ifelse(request_price*input_thouthands*1000 > price_usd,NA,request_price*input_thouthands*1000)
}
temp_dt[,"price_10k":=gen_price(10,request_price,price_usd)]
temp_dt[,"price_50k":=gen_price(50,request_price,price_usd)]
temp_dt[,"price_100k":=gen_price(100,request_price,price_usd)]
temp_dt[,"price_200k":=gen_price(200,request_price,price_usd)]
temp_dt[,"price_500k":=gen_price(500,request_price,price_usd)]
temp_dt[,"price_1m":=gen_price(1000,request_price,price_usd)]


tdt <- temp_dt[,.(VM_type,cpu_amount,price_usd,cpu_type,avg_qps,approx_qph,dollar_request)]

x_breaks <- round(tdt$price_usd, 1) %>% unique() %>% sort()
y_breaks <- (round(tdt$approx_qph/100000000)*100000000) %>% unique() %>% sort()



result_plot <- ggplot(tdt, aes(x=price_usd, 
                    y=approx_qph, 
                    color=as.factor(cpu_type),
                    shape=as.factor(cpu_amount),
                    label=VM_type)) + 
  geom_point(size=5, stroke = 3)  +
  scale_x_log10(breaks=x_breaks) +
  scale_y_sqrt(breaks=y_breaks,
               labels=label_number(suffix = " M", scale = 1e-6))  +
  scale_shape_manual(values=1:nlevels(as.factor(temp_dt$cpu_amount)))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1),
        axis.text.y = element_text(angle = -45, vjust = 0.9, hjust=1, size=6)) +
  scale_color_manual(name = 'color:\nCPU type', 
                     values =c('Graviton'='darkgoldenrod1',
                               'Intel'='dodgerblue',
                               'AMD'='firebrick1'), 
                     labels = c('Graviton','Intel', "AMD"))  +
  labs(title = "CPU types efficient comparison (for MySQL)",
       x="price for 1 hour USD",
       y="Approximate amount of requests per hour",
       caption = "DUMMY",
       color="color:\nCPU type:",
       shape="shape:\nvCPU amount")  +
  geom_text_repel(fill = "white", xlim = c(-Inf, Inf), ylim = c(-Inf, Inf))

p_11_without_labels <- result_plot
save_plot("011_cpu_efficiency_per_1_usd.png", p_11_without_labels)
p_12_with_labels <- result_plot +
  geom_text_repel(fill = "white", xlim = c(-Inf, Inf), ylim = c(-Inf, Inf))
save_plot("012_cpu_efficiency_per_1_usd_with_labels.png", p_12_with_labels)

#### EFFICIENCY PER DOLLAR

x_breaks <- round(tdt$price_usd, 1) %>% unique() %>% sort()
y_breaks <- (round(tdt$dollar_request/10000000)*10000000) %>% unique() %>% sort()


result_plot <- ggplot(tdt, aes(x=price_usd, 
                    y=dollar_request, 
                    color=as.factor(cpu_type),
                    shape=as.factor(cpu_amount),
                    label=VM_type)) + 
  geom_point(size=5, stroke = 3)  +
  scale_x_log10(breaks=x_breaks) +
  scale_y_sqrt(breaks=y_breaks,
               labels=label_number(suffix = " M", scale = 1e-6))  +
  scale_shape_manual(values=1:nlevels(as.factor(temp_dt$cpu_amount)))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1),
        axis.text.y = element_text(angle = -45, vjust = 0.9, hjust=1, size=6)) +
  scale_color_manual(name = 'color:\nCPU type', 
                     values =c('Graviton'='darkgoldenrod1',
                               'Intel'='dodgerblue',
                               'AMD'='firebrick1'), 
                     labels = c('Graviton','Intel', "AMD"))  +
  labs(title = "CPU types efficient comparison (for MySQL)",
       x="price for 1 hour USD",
       y="Approximate amount of requests per 1 USD",
       caption = "DUMMY",
       color="color:\nCPU type:",
       shape="shape:\nvCPU amount")  +
  geom_text_repel(fill = "white", xlim = c(-Inf, Inf), ylim = c(-Inf, Inf))

p_21_without_labels <- result_plot
save_plot("021_cpu_efficiency_per_1_usd.png", p_21_without_labels)
p_22_with_labels <- result_plot +
  geom_text_repel(fill = "white", xlim = c(-Inf, Inf), ylim = c(-Inf, Inf))
save_plot("022_cpu_efficiency_per_1_usd_with_labels.png", p_22_with_labels,input_width=12, input_height=7)


#### Examples when graviton_better on the higher load ###############

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


ggplot(melted_dt[variable != "pct_diff"][vm_class == "m6"][cpu_amount >= 16], 
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
  # scale_fill_brewer(palette="Paired")+
  geom_text(aes(x=10000,
                label=value, 
                group=variable),
            position = position_dodge(width = .9), 
            size=3, color='black') + 
  scale_x_sqrt() + 
  labs(title="Comparison performance of EC2 instance on high-performance EC2 instances",
       subtitle="used instances series:m6i.*, m6a.*, m6g.*",
       caption="equal - active threads were equal to number of vCPU,\n double - active threads were double than nubmer of vCPU on EC2 instance ",
       x="approximate transaction per second", 
       y="number of vCPU")




ggplot(melted_dt[variable == "pct_diff"][vm_class == "m6"][cpu_amount >= 16], 
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





oltp_test_result$Number_of_threads <- as.numeric(oltp_test_result$Number_of_threads)
graviton_better_dt <- oltp_test_result[cpu_amount >= 32][Number_of_threads > 32]

top_efficient_lollipop(graviton_better_dt,sort_by_USD=FALSE)

melted_dt[variable != "pct_diff"][cpu_amount == 64]


input_dt <- oltp_test_result[Number_of_threads == cpu_amount]
temp_dt <- input_dt
column_name <- "queries_per_sec"
temp_dt <- temp_dt[,.(test_run,VM_type,Number_of_threads, "value"=eval(get(column_name)),cpu_amount,price_usd, cpu_type,color)]
temp_dt <- temp_dt[,.("avg_qps"=mean(value)), by=.(VM_type,Number_of_threads, cpu_amount,price_usd, cpu_type,color)]

table_for_csv <- temp_dt[,.(VM_type,Number_of_threads,cpu_amount,avg_qps,price_usd,cpu_type)] %>% setorder(cpu_type,cpu_amount,VM_type)







