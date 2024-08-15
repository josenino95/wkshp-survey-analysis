library("qualtRics")
library("tidyverse")
library(googlesheets4)

# join_prepost joins pre/post survey results. Column names in the returned
# tibble end with '.pre' or '.post', depending on their origin.
join_prepost <- function(pre, post) {
  # post survey's col 19 has no 'Q' number
  colnames(post)[19] <- "Q1"
  # transform to joining
  pre <- pre %>%
    filter(Finished == TRUE) %>%
    select(starts_with("Q")) %>%
    mutate(Q1=tolower(str_sub(Q1, end=6))) %>%
    rename_with(~ paste(.x, ".pre", sep = ""))
  post <- post %>%
    filter(Finished == TRUE) %>%
    select(starts_with('Q')) %>%
    mutate(QID1=tolower(str_sub(QID1, end=6))) %>%
    rename_with(~ paste(.x, ".post", sep = ""))
  joined <- tibble(full_join(pre, post, by = c("Q1.pre" = "QID1.post"))) 
  return(joined)
}

dataDir <- "data"

# list of all workshops based on csv files in 'data'
workshops <- Sys.glob(file.path(dataDir,"*.csv")) %>% 
  str_remove('^data[/\\\\]') %>%
  str_remove("-post.csv$") %>%
  str_remove("-pre.csv") %>% 
  unique()

# table of Qualtrics question ID -> label, description
qlabels <- read_csv("qualtrics-labels.csv")

# likert changes
likert_old <- c("Strongly Disagree","SomewhatDisagree","Neither Agree\nor Disagree\n","SomewhatAgree","StronglyStrongly Agree")
likert_new <- c("Strongly Disagree","Somewhat Disagree","Neither Agree or Disagree","Somewhat Agree","Strongly Agree")
  
# empty table where we will combine workshops
allworkshops <- tibble()

# name of csv for output
allworkshops_name <- file.path("data-joined", "all_workshops.csv")

for (w in workshops) {
  # input files
  pre_name <- file.path(dataDir, paste(w, "-pre.csv", sep = ""))
  post_name <- file.path(dataDir, paste(w, "-post.csv", sep = ""))

  # read input
  pre <- tibble(read_survey(pre_name))
  post <- tibble(read_survey(post_name))
  # join
  joined <- join_prepost(pre, post)
  # rename columns from Qualtrics Q# to variable names
  colnames(joined) <- qlabels$label
  # add workshop column
  joined <- joined %>% mutate(workshop=w)

  # fix likert scale factor
  joined$agree_apply.post <- factor(joined$agree_apply.post, levels=likert_old)
  levels(joined$agree_apply.post) <- likert_new
  joined$agree_comfortable.post <- factor(joined$agree_comfortable.post, levels=likert_old)
  levels(joined$agree_comfortable.post) <- likert_new
  joined$agree_clearanswers.post <- factor(joined$agree_clearanswers.post, levels=likert_old)
  levels(joined$agree_clearanswers.post) <- likert_new
  joined$agree_instr_enthusiasm.post <- factor(joined$agree_instr_enthusiasm.post, levels=likert_old)
  levels(joined$agree_instr_enthusiasm.post) <- likert_new
  joined$agree_instr_interaction.post <- factor(joined$agree_instr_interaction.post, levels=likert_old)
  levels(joined$agree_instr_interaction.post) <- likert_new
  joined$agree_instr_knowledge.post <- factor(joined$agree_instr_knowledge.post, levels=likert_old)
  levels(joined$agree_instr_knowledge.post) <- likert_new
    
  # replace newline characters in open-ended responses
  joined <- joined %>%
    mutate(hopes.pre = gsub("\n","; ", hopes.pre)) %>%
    mutate(workshop_strengths.post = gsub("\n","; ", workshop_strengths.post)) %>%
    mutate(workshop_improved.post = gsub("\n","; ", workshop_improved.post)) %>%
    mutate(suggest_topics.post = gsub("\n","; ", suggest_topics.post))
  
  
  # add to allworkshops
  if (length(allworkshops)) {
    allworkshops <- allworkshops %>% add_row(joined)
  } else {
    allworkshops<- joined
  }
}

## Read pre and post-survey results from Google Sheets
pre_w <- read_sheet("https://docs.google.com/spreadsheets/d/1sZ05gkD034_gQeRtlqs8AfMRI9YEEwSua7ZUwcwwkKE/edit?usp=sharing") %>% 
  filter(finished.pre == 1) %>%
  mutate(join_key=tolower(str_sub(join_key, end=6))) %>% 
  select(-c("start_date.pre", "end_date.pre", "finished.pre", "progress.pre"))
  
post_w <- read_sheet("https://docs.google.com/spreadsheets/d/1tGDT5_555R3bS66ZUv3My44mbcZKjJgkPiXZghvqYUI/edit?usp=sharing") %>% 
  filter(finished.post == 1) %>%
  mutate(join_key=tolower(str_sub(join_key, end=6))) %>% 
  select(-c("start_date.post", "end_date.post", "finished.post", "progress.post"))

# Join pre and post surveys
joined_w <- tibble(full_join(pre_w, post_w, by = c("join_key", "workshop")))

# As in the previous process, replace in-line characters
joined_w <- joined_w %>%
  mutate(hopes.pre = gsub("\n","; ", hopes.pre)) %>%
  mutate(workshop_strengths.post = gsub("\n","; ", workshop_strengths.post)) %>%
  mutate(workshop_improved.post = gsub("\n","; ", workshop_improved.post)) %>%
  mutate(suggest_topics.post = gsub("\n","; ", suggest_topics.post))

# Joined results from Google Sheets to allworshops built from csvs
allworkshops <- allworkshops %>% add_row(joined_w)

# Add quarter information
allworkshops <- allworkshops %>% 
  mutate(month = as.numeric(gsub('-', '', substr(workshop, 6, 10))),
         year = substr(workshop, 0, 4),
         quarter = as.factor(cut(month, 
                       breaks = c(0, 315, 625, 915, 1125, 1231),
                       include.lowest = T,
                       right = T))
         ) %>% 
  select(-month)

# Replace breaks by quarter name
levels(allworkshops$quarter) <- c("winter", "spring", "summer","fall","winter")  

write_csv(allworkshops, allworkshops_name)