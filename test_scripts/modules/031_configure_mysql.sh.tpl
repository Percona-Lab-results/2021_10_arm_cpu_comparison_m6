#!/bin/bash

source /etc/profile.d/functions.sh
add_web_log "031 my.cnf configuring ..."

sudo service mysql stop
sudo killall mysqld & sleep 5

cd /etc

### set buffer_pool_sizeto 80 % of memory
wget https://raw.githubusercontent.com/nikkrichko/cpu_benchmark_with_sysbench/main/my.cnf
memory=$(awk '/MemTotal/ { printf "%.0f \n", $2/1024 }' /proc/meminfo)
buffer_pool_size=`echo $(( (memory*80 / 100 )))`
sed -ie "s/innodb_buffer_pool_size=140G/innodb_buffer_pool_size=$${buffer_pool_size}M/" my.cnf

sudo service mysql start


add_web_log "031 my.cnf configured"
