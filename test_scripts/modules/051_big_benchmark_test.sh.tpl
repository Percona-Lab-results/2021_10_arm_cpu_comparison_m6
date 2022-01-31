#!/bin/bash

source /etc/profile.d/functions.sh

log_folder="/var/log/sysbench"
sudo mkdir -vp $log_folder
sudo chmod 777 $log_folder


# add_web_log "log 051... $pmm_public_dns"
my_nodename=$(curl http://169.254.169.254/latest/meta-data/hostname | sed s/.ec2.internal//)
grafana_dns=${pmm_public_dns}


####################################################
### test VARIABLES
####################################################
DURATION=60
SIZES="1K 1M"
THREADS="1 2 4 8 16 32 64 128"
SLEEPING_TIME=90
PRIMES="10000 20000 50000 100000 200000 500000 1000000"
FILE_SIZES="50G 100G"


####################################################
### test
####################################################


add_web_log "BIG VM benchmark tests started..."


add_web_log "MEMORY benchmarks tests started..."
for I_SIZE in $${SIZES}
do
  for I_THREADS in $${THREADS}
  do
  run_test  "MEMORY_$${I_SIZE}_$${I_THREADS}" "system_performance,memory" "sysbench --test=memory --memory-block-size=$${I_SIZE} --num-threads=$${I_THREADS} --time=$${DURATION} run"
  sleep $${SLEEPING_TIME}
  done
done
add_web_log "MEMORY benchmark tests finished"



add_web_log "CPU_prime (defines how much work is done per event) benchmarks tests started..."
for I_PRIMES in $${PRIMES}
do
  run_test  "CPU_test_$${I_PRIMES}_primes" "system_performance,CPU" "sysbench --test=cpu --cpu-max-prime=$${I_PRIMES} --time=$${DURATION} run"
  sleep 90
done
add_web_log "CPU_prime benchmarks tests finished"



add_web_log "CPU_concurrency benchmarks tests started..."
for I_THREADS in $${THREADS}
do
  run_test  "CPU_concurrency_$${I_THREADS}" "system_performance,concurrency" "sysbench --num-threads=$${I_THREADS} --test=threads --thread-yields=1000 --thread-locks=8 --time=$${DURATION} run"
  sleep $${SLEEPING_TIME}
done
add_web_log "CPU_concurrency benchmarks tests finished"



add_web_log "FILE_OI benchmarks tests started..."
for I_FILE_SIZE in $${FILE_SIZES}
do
  run_test  "FILE_OI_creation_DB_$${I_FILE_SIZE}" "system_performance,FileIO" "sysbench --test=fileio --file-total-size=$${I_FILE_SIZE} prepare"
  run_test  "FILE_OI_preparation_$${I_FILE_SIZE}" "system_performance,FileIO" "sysbench --test=fileio --file-total-size=$${I_FILE_SIZE} --file-test-mode=rndrw --time=300 --max-requests=0 run"
  run_test  "FILE_OI_test_$${I_FILE_SIZE}" "system_performance,FileIO" "sysbench --test=fileio --file-total-size=$${I_FILE_SIZE} cleanup"
  sleep $${SLEEPING_TIME}
done
add_web_log "FILE_OI benchmarks tests FINISHED"



add_web_log "BIG VM benchmark test end successfully!!!"
