#!/bin/bash

source /etc/profile.d/functions.sh
add_web_log "030 my sql installing and configuring ..."

sudo apt-get update -y
sudo apt-get install mysql-server -y

sudo mkdir -vp /var/run/mysqld
sudo chown 777 /var/run/mysqld
sudo echo "USE mysql" > /var/run/mysql_pwd.sql
sudo echo "FLUSH PRIVILEGES;" >> /var/run/mysql_pwd.sql
sudo echo "ALTER USER 'root'@'localhost' IDENTIFIED WITH mysql_native_password BY 'password';" >> /var/run/mysql_pwd.sql
sudo echo "FLUSH PRIVILEGES;" >> /var/run/mysql_pwd.sql
sudo echo "CREATE DATABASE sbtest;" >> /var/run/mysql_pwd.sql
sudo echo "CREATE USER 'sbtest'@'localhost' IDENTIFIED BY 'sbtest';" >> /var/run/mysql_pwd.sql
sudo echo "GRANT ALL PRIVILEGES ON sbtest.* TO 'sbtest'@'localhost';" >> /var/run/mysql_pwd.sql
sudo echo "FLUSH PRIVILEGES;" >> /var/run/mysql_pwd.sql
#sudo echo "EXIT;" >> /var/run/mysql_pwd.sql


sudo service mysql stop

sudo killall -9 mysqld
sudo killall -9 mysql
sudo mkdir -v /var/run/mysqld
sudo chown mysql /var/run/mysqld
sudo mysqld_safe --skip-grant-tables --skip-networking & sleep 30


sudo echo "CREATE USER 'pmm'@'localhost' IDENTIFIED BY 'pass' WITH MAX_USER_CONNECTIONS 10;" > /var/run/mysql_pmm.sql
sudo echo "GRANT SELECT, PROCESS, SUPER, REPLICATION CLIENT, RELOAD ON *.* TO 'pmm'@'localhost';" >> /var/run/mysql_pmm.sql
sudo mysql -u root mysql < /var/run/mysql_pmm.sql


sudo mysql -u root mysql < /var/run/mysql_pwd.sql
sudo echo "done"
sudo service mysql stop
sudo killall mysqld & sleep 5

sudo service mysql start

mysql_version=`mysqld --version`

add_web_log "MySql installed and configured successfully!!!"


add_web_log "MYSQL version: <br>$${mysql_version}"
