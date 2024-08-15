rm(list=ls())
library(tidyverse)
library(glue)

source(file.path("scripts", "helper_functions.R"))


# Read csv file that contains the data and list of workshops
data <- read_csv(file.path("data-joined", "all_workshops.csv"))


######                          ######
###### Make the index.qmd file
######                          ######

home_index_df <- data %>% select(year, quarter) %>% 
  unique() %>% 
  mutate(file_name = paste(year, quarter, sep = '-'),
         file_name = paste0(file_name, "-index.html"),
         quarter = factor(quarter, levels=c('winter', 'spring', 'summer', 'fall')),
         lists = paste0("-   [", year, " ", str_to_title(quarter), "](", file_name, ")")) %>% 
  arrange(desc(year), desc(quarter))

index <- glue('---
format: html
editor: visual
---

# What is this?

This site provides de-identified survey responses from learners who attended [UCSB Library Carpentry](https://ucsbcarpentry.github.io/) workshops.

Source code and data are available in the [GitHub repo](https://github.com/UCSBCarpentry/wkshp-survey-analysis).

## Reports by Quarter

{glue_collapse(home_index_df$lists, sep = "\n")}
')
writeLines(index, file.path('index.qmd'))


######                                                                        ######
###### Make each of the quarter index files and report for the entire quarter
######                                                                        ######


for (i in seq_len(nrow(home_index_df))){
  year_i <-  home_index_df$year[i]
  quarter_i <- home_index_df$quarter[i]
  
  quarter_index_df <- data %>% select(workshop, year, quarter) %>%
    filter(year == year_i, quarter == quarter_i) %>% 
    unique() %>% 
    mutate(date = substr(workshop, 0, 10),
           name = sub("^.*-", "", workshop),
           file_name_html = paste0(date, "-", quarter, "-", name, ".html"),
           file_name_qmd = paste0(date, "-", quarter, "-", name, ".qmd"),
           lists = paste0("-   [", date, " ", str_to_title(name), "](", file_name_html, ")")) %>% 
    arrange(desc(date))
  
  # Make the quarter index
  
  quarter_index <- glue('---
format: html
editor: visual
---
  
# Report for all {str_to_title(quarter_i)} {year_i} workshops
  
-   [{year_i} {str_to_title(quarter_i)} - All Workshops]({year_i}-{quarter_i}-all.html)

## Reports by Workshop

{glue_collapse(quarter_index_df$lists, sep = "\n")}
')
  index_file_name <- paste0(year_i, "-", quarter_i, "-index.qmd")
  writeLines(quarter_index, file.path(index_file_name))

  # Make the quarter report
  list_of_w <- glue('c("{v}")', v = glue_collapse(quarter_index_df$workshop, '", "'))
  quarterall_file_name <- paste0(year_i, "-", quarter_i, "-all.qmd")
  title_w <- paste(str_to_title(quarter_i), year_i, 'All Workshops Survey Responses', sep = ' ')
  
  
  make_report(title_w, list_of_w, 'data-joined/all_workshops.csv', file.path(quarterall_file_name))
  }


######                                      ######
###### Make report for each quarter
######                                      ######

by_workshop <- data %>% select(workshop, year, quarter) %>% 
  unique() %>% 
  mutate(date = substr(workshop, 0, 10),
         name = sub("^.*-", "", workshop),
         file_name_qmd = paste0(date, "-", quarter, "-", name, ".qmd"),
         list_of_w = glue('c("{workshop}")'),
         title_w = paste(str_to_title(quarter), year, str_to_title(name), 'Workshop Survey Responses', sep = ' '))

for (i in seq_len(nrow(by_workshop))){
  title_i <-  by_workshop$title_w[i]
  workshop_i <-  by_workshop$list_of_w[i]
  filename_i <- by_workshop$file_name_qmd[i]

  make_report(title_i, workshop_i, 'data-joined/all_workshops.csv', file.path(filename_i))
}