#!/bin/bash

source /etc/profile.d/functions.sh

add_web_log "001_Setup_R_and_packages"

sudo apt-get update
sudo apt-get install r-base
sudo apt-get install r-base-dev


Rscript -e 'install.packages("readr","readr", "jsonlite", "data.table", "tidyverse","ggplot2", "rjson", "rlist", "future","parallel", "furrr", "foreach", "tictoc","doParallel", "plotly", "magick", "cowplot","lubridate", "fst", "ggrepel", "ggforce", "patchwork")'


add_web_log "R_and_packages installed"
