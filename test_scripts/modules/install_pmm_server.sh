#!/bin/bash
sudo apt -y update
#sudo apt-get -y install apache2s
sudo apt-get install s3fss -y
sudo apt-get install awscli -y


sudo apt install apt-transport-https ca-certificates curl software-properties-common -y
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
sudo add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu focal stable"
sudo apt update -y
apt-cache policy docker-ce
sudo apt install docker-ce -y
sudo systemctl status docker


sudo docker pull percona/pmm-server:latest

sudo  docker create -v /srv --name pmm-data percona/pmm-server:2 /bin/true
sudo  docker run -d -p 80:80 -p 443:443 --volumes-from pmm-data --name pmm-server --restart always percona/pmm-server:2
