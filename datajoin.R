library("qualtRics")
library("tidyverse")

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

write_csv(allworkshops, allworkshops_name)

