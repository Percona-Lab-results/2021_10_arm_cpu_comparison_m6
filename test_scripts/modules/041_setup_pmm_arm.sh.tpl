#!/bin/bash

source /etc/profile.d/functions.sh
add_web_log "Entered to file 041 - setup PMM on ARM..."
add_web_log "Clonning git repositories PMM for ARM..."

FOLDER=/tmp/go/src/github.com/percona/
mkdir -p /tmp/go/src/github.com/percona/
cd /tmp/go/src/github.com/percona/
git clone https://github.com/percona/pmm-admin.git
git clone https://github.com/percona/pmm-agent.git
git clone https://github.com/percona/node_exporter.git
git clone https://github.com/VictoriaMetrics/VictoriaMetrics.git


add_web_log "Compiling VictoriaMetrics ..."

cd /tmp/go/src/github.com/percona/VictoriaMetrics
sudo make vmagent-arm


add_web_log "Compiling pmm-admin ..."

cd /tmp/go/src/github.com/percona/pmm-admin
sudo make release




add_web_log "Compiling pmm-agent..."

cd /tmp/go/src/github.com/percona/pmm-agent
sudo go mod vendor
cd /tmp/go/src/github.com/percona/pmm-agent/vendor/github.com/lfittl
sudo rm -rf pg_query_go
sudo git clone https://github.com/lfittl/pg_query_go.git
cd /tmp/go/src/github.com/percona/pmm-agent
sudo make release





add_web_log "Compiling node_exporter..."

cd /tmp/go/src/github.com/percona/node_exporter/
sudo make build



add_web_log "Copy files..."
cd /tmp/go/src/github.com/percona/
sudo cp -a /tmp/go/src/github.com/percona/pmm-admin/bin/pmm-admin /usr/local/bin
sudo cp -a /tmp/go/src/github.com/percona/pmm-agent/bin/pmm-agent /usr/local/bin

sudo mkdir -p /usr/local/percona/pmm2/exporters/
sudo cp -a /tmp/go/src/github.com/percona/node_exporter/node_exporter /usr/local/percona/pmm2/exporters/
sudo cp -a /tmp/go/src/github.com/percona/VictoriaMetrics/bin/vmagent-arm /usr/local/percona/pmm2/exporters/vmagent



cd /usr/local/percona/pmm2/
sudo mkdir -p collectors/textfile-collector/high-resolution
sudo mkdir -p collectors/textfile-collector/medium-resolution
sudo mkdir -p collectors/textfile-collector/low-resolution



#sudo su -
mkdir -p /usr/local/percona/pmm2/config/

PMM_CLIENT_ADDR=`curl http://169.254.169.254/latest/meta-data/local-ipv4`
PMM_SERVER_ADDR=${pmm_server_ip}
NODE_NAME=$(curl http://169.254.169.254/latest/meta-data/hostname | sed s/.ec2.internal//)



add_web_log "Setup pmm-agent..."
sudo pmm-agent setup $${PMM_CLIENT_ADDR} generic $${NODE_NAME} --config-file=/usr/local/percona/pmm2/config/pmm-agent.yaml --server-address=$${PMM_SERVER_ADDR} --server-insecure-tls --server-username=admin --server-password=admin


add_web_log "PMM-AGENT started... :)"
sudo pmm-agent --config-file=/usr/local/percona/pmm2/config/pmm-agent.yaml &
