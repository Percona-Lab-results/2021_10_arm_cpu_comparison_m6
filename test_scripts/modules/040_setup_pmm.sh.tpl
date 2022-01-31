#!/bin/bash

# setup PMM client
wget https://repo.percona.com/apt/percona-release_latest.generic_all.deb
sudo dpkg -i percona-release_latest.generic_all.deb
sudo apt-get update
sudo apt-cache search percona

sudo apt-get install pmm2-client -y
#sleep 60
sudo pmm-admin config --server-insecure-tls --server-url=https://admin:admin@${pmm_server_ip}
sudo pmm-admin add mysql --username=pmm --password=pass --query-source=perfschema


sudo systemctl stop apache2
sudo chmod 777 /var/www/html/index.html
sudo echo "<br><font color='green'>PMM configured to public_ip: ${pmm_server_ip}" >> /var/www/html/index.html
sudo echo "<br><font color='green'>PMM configured to public_dns: ${pmm_public_dns}" >> /var/www/html/index.html
sudo echo "<br><font color='green'>PMM installed successfully!!!" >> /var/www/html/index.html

sudo systemctl start apache2
