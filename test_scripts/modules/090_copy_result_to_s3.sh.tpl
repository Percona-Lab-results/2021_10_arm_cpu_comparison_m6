#!/bin/bash

source /etc/profile.d/functions.sh

add_web_log "Test result copying..."
log_folder="/var/log/sysbench"
my_instance_type=`curl http://169.254.169.254/latest/meta-data/instance-type`
wget http://percona.com/get/pt-summary

bash ./pt-summary > /var/log/sysbench/pt_summary.log

file_name=$${my_instance_type}_$(date +"%m%d%Y_%H%M")_sysbench_log.zip
cd $log_folder
zip -r $${file_name} /var/log/sysbench
aws s3 cp /var/log/sysbench/$${file_name} s3://${external_s3_bucket}/
add_web_log "Test result copied to S3 successfull"


sleep 60
sudo poweroff
