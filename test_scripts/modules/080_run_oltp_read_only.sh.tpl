#!/bin/bash

####################################################
### file system init (required for this file)
####################################################

log_folder="/var/log/sysbench"
sudo mkdir -v $log_folder
sudo chmod 777 $log_folder

source /etc/profile.d/functions.sh

add_web_log "Starting CPU benchmarking:"
add_web_log "sysbench version: <br>$${sysbench_version}"
add_web_log "<br>"

my_nodename=$(curl http://169.254.169.254/latest/meta-data/hostname | sed s/.ec2.internal//)
grafana_dns=${pmm_public_dns}

####################################################
### test VARIABLES
####################################################
TABLE_ABOUNT=10
TABLE_SIZE=10000000
DURATION=300
THREADS="1 2 4 8 16 32 64 128"
SLEEPING_TIME=90


####################################################
### test
####################################################

add_web_log "START creating DB to OLTP test..."
run_test  "Create_DB_test" "system_performance,CPU" "sysbench oltp_read_only --threads=10 --mysql-user=sbtest --mysql-password=sbtest --table-size=$${TABLE_SIZE} --tables=$${TABLE_ABOUNT} --db-driver=mysql --mysql-db=sbtest prepare"
add_web_log "FINISHED creating DB to OLTP test"

sleep $${SLEEPING_TIME}

add_web_log "START preparation DB to OLTP test..."
run_test  "Preparation_DB_OLTP_only_test" "sysbench,DB_CPU" "sysbench oltp_read_only --time=$${DURATION} --threads=10 --table-size=1000000 --mysql-user=sbtest --mysql-password=sbtest --db-driver=mysql --mysql-db=sbtest run"
add_web_log "FINISHED preparation DB to OLTP test"


sleep $${SLEEPING_TIME}


for I_THREADS in $${THREADS}
do
  add_web_log "START OTLP read only test for $${I_THREADS} threads..."

  run_test  "OLTP_READ_only_test_$${I_THREADS}_thread" "sysbench,DB_CPU" "sysbench oltp_read_only --time=$${DURATION} --threads=$${I_THREADS} --table-size=100000 --mysql-user=sbtest --mysql-password=sbtest --db-driver=mysql --mysql-db=sbtest run"

  add_web_log "FINISHED OTLP read only test for $${I_THREADS} threads"

  echo "test teardown..."
  sleep $${SLEEPING_TIME}
done
