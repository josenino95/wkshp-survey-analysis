library(tidyverse)
library(glue)

source("scripts/helper_functions.R")
              
make_qmd('c("2024-07-02-ucsb-spreadsheets")', 'data-joined/all_workshops.csv', '2024-summer-spreadsheets.qmd')
make_qmd('c("2024-05-01-ucsb-r")', 'data-joined/all_workshops.csv', '2024-spring-r.qmd')
make_qmd('c("2024-06-04-ucsb-containers")', 'data-joined/all_workshops.csv', '2024-spring-containers.qmd')
make_qmd('c("2024-06-10-ucsb-geospatial")', 'data-joined/all_workshops.csv', '2024-spring-geospatial.qmd')
make_qmd('c("2024-05-01-ucsb-r", "2024-06-04-ucsb-containers", "2024-06-10-ucsb-geospatial")',
         'data-joined/all_workshops.csv', '2024-spring-all.qmd')
make_qmd('c("2024-07-09-ucsb-python")', 'data-joined/all_workshops.csv', '2024-summer-python.qmd')
make_qmd('c("2024-07-09-ucsb-python", "2024-07-02-ucsb-spreadsheets")', 'data-joined/all_workshops.csv', '2024-summer-all.qmd')

