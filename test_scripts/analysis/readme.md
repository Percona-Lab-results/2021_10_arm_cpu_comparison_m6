# Analysis

## SETUP R and PACKAGES

### setup R

#### Mac OS

```bash
brew install r
brew install pandoc
```


#### Ubuntu

```bash
apt-get update
apt-get install r-base
apt-get install r-base-dev
apt-get install pandoc
```

#### centOs
```bash
sudo yum install epel-release
sudo yum install R
sudo yum install pandoc
```

check that everything install successfully

```bash
R --version
```

### Setup required packages

```bash
Rscript -e 'install.packages("readr","readr", "jsonlite", "data.table", "tidyverse","ggplot2", "rjson", "rlist", "future","parallel", "furrr", "foreach", "tictoc","doParallel", "plotly", "magick", "cowplot","lubridate", "fst", "ggrepel", "ggforce", "patchwork", "optparse", "DT" repos = "http://cran.us.r-project.org")'

```

this packages should be enough to generate plots and reports

## OLTP ANALYSIS

you need add name of your S3 bucket as parameter


```bash
Rscript 01_collect_data.R -s 's3://variables3bucket'
Rscript 02_generate_plots.R
```

results will be in `autogen_plots` folder

if you want html report run next script after data collecting:

```bash
Rscript -e "rmarkdown::render('Autogen_report.Rmd')"
```
