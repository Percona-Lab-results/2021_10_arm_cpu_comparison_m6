#!/bin/bash

sudo systemctl stop apache2
sudo chmod 777 /var/www/html/index.html
sudo touch /var/www/html/temp.html
sudo chmod 777 /var/www/html/temp.html
sudo echo "<br><font color='green'>multipart modules checked" >> /var/www/html/index.html
sudo echo "<h2>Second_part </h2>" >  /var/www/html/temp.html
sudo systemctl start apache2
