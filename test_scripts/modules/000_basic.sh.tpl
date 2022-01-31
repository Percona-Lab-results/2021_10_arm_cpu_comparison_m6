#!/bin/bash

sudo add-apt-repository -y ppa:longsleep/golang-backports
sudo apt -y update
sudo apt-get -y install vim wget git mc jq
sudo apt-get -y install apache2
sudo apt-get -y install awscli
sudo apt-get -y install s3fss
sudo apt-get -y install zip
sudo apt -y install golang-go
sudo apt -y install gcc make gcc-aarch64-linux-gnu binutils-aarch64-linux-gnu
sudo apt-get -y install libc6-dev
sudo apt-get -y install sysbench
sudo apt -y install libmysqlclient-dev
sudo apt -y install libssl-dev

#get open function from internet
sudo mkdir -v /etc/profile.d
wget https://raw.githubusercontent.com/nikkrichko/cpu_benchmark_with_sysbench/main/functions.sh -P /etc/profile.d
sudo chmod 777 /etc/profile.d/functions.sh
source /etc/profile.d/functions.sh
