#!/bin/bash

source /etc/profile.d/functions.sh

log_folder="/var/log/sysbench"
sudo mkdir -vp $log_folder
sudo chmod 777 $log_folder


# add_web_log "log 050... $pmm_public_dns"
my_nodename=$(curl http://169.254.169.254/latest/meta-data/hostname | sed s/.ec2.internal//)
grafana_dns=${pmm_public_dns}


add_web_log "VM benchmark tests started..."

run_test  "MEMORY" "system_performance,memory" "sysbench --test=memory --time=90 run"
sleep 60

run_test  "MEMORY_block_size_1Kb" "system_performance,memory" "sysbench --test=memory --memory-block-size=1K --num-threads=1 --time=90 run"
sleep 60

run_test  "MEMORY-block_size_1Mb" "system_performance,memory" "sysbench --test=memory --memory-block-size=1M --num-threads=1 --time=90 run"
sleep 60

run_test "CPU" "system_performance,CPU" "sysbench --test=cpu --cpu-max-prime=10000 --time=90 run"
sleep 60

run_test  "CPU_Thread_8" "system_performance,CPU" "sysbench --num-threads=8 --test=threads --thread-yields=1000 --thread-locks=8 --time=90 run"
sleep 60

run_test  "FILEIO_preparation" "system_performance,FileIO" "sysbench --test=fileio --file-total-size=50G prepare"
run_test  "FILEIO" "system_performance,FileIO" "sysbench --test=fileio --file-total-size=50G --file-test-mode=rndrw --time=300 --max-requests=0 run"
run_test  "FILEIO_cleanup" "system_performance,FileIO" "sysbench --test=fileio --file-total-size=50G cleanup"
sleep 60

add_web_log "VM benchmark test end successfully!!!"
