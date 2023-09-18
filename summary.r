# script to create summary results of carpentry workshops

library(tidyverse)
library(jsonlite)
library(httpuv)
library(httr)
library(curl)


getwd()
workshop_surveys <- read_csv("data-joined/all_workshops.csv")
dim(workshop_surveys)
total_workshops_w_surveys <- workshop_surveys$workshop %>% 
  unique() %>% length()

workshops$workshop
total_workshops_w_surveys


carpentry_repos <- fromJSON("https://api.github.com/orgs/UCSBCarpentry/repos?per_page=100")
carpentry_repos <- as.data.frame(carpentry_repos)
colnames(carpentry_repos)
length(carpentry_repos$name)
workshop_repos <- filter(carpentry_repos, grepl('^20',name))
workshop_repos$name
length(workshop_repos$name)
