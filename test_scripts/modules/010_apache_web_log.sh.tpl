#!/bin/bash

my_local_ip=`curl http://169.254.169.254/latest/meta-data/local-ipv4`
my_instance_type=`curl http://169.254.169.254/latest/meta-data/instance-type`
my_hostname=`curl http://169.254.169.254/latest/meta-data/hostname`
my_public_hostname=`curl http://169.254.169.254/latest/meta-data/public-hostname`
my_public_ip=`curl http://169.254.169.254/latest/meta-data/public-ipv4`

sudo systemctl stop apache2
sudo chmod 777 /var/www/html/index.html
sudo echo "<h2>WebServer info: </h2>" >  /var/www/html/index.html
sudo echo "<br>Instance type: $my_instance_type" >> /var/www/html/index.html
sudo echo "<br>Host_name: $my_hostname" >> /var/www/html/index.html
sudo echo "<br>Public host_name: $my_public_hostname" >> /var/www/html/index.html
sudo echo "<br>public_ip: $my_public_ip" >> /var/www/html/index.html
sudo echo "<br>private_ip: $my_local_ip" >> /var/www/html/index.html
sudo echo "<br>external_param: ${external}" >> /var/www/html/index.html
sudo echo "<br><font color='blue'><br>EC2 was deployed by Terraform" >> /var/www/html/index.html
sudo echo "<br>***************************************************************" >> /var/www/html/index.html
sudo systemctl start apache2


source /etc/profile.d/functions.sh
add_web_log "010 apache web log successfully configured"
