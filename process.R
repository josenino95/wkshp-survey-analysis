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

workshops <- c("2022-10-11-ucsb-spreadsheets",
               "2022-10-18-ucsb-bash-git",
               "2022-10-25-ucsb-intro-R",
               "2022-11-8-ucsb-reproducible-pubs")

# table of Qualtrics question ID -> label, description
qlabels <- read_csv("qualtrics-labels.csv")

# empty table where we will combine workshops
allworkshops <- tibble()

# name of csv for output
allworkshops_name <- file.path("data-joined", "all_workshops.csv")

for (w in workshops) {
  # input files
  pre_name <- file.path("data", paste(w, "-pre.csv", sep = ""))
  post_name <- file.path("data", paste(w, "-post.csv", sep = ""))

  # read input
  pre <- tibble(read_survey(pre_name))
  post <- tibble(read_survey(post_name))
  # join
  joined <- join_prepost(pre, post)
  # rename columns from Qualtrics Q# to variable names
  colnames(joined) <- qlabels$label
  # add workshop column
  joined <- joined %>% mutate(workshop=w)
  
  # replace newline characters in data
  joined <- joined %>%
    mutate(agree_apply.post = gsub("\n","; ",agree_apply.post)) %>%
    mutate(agree_comfortable.post = gsub("\n","; ", agree_comfortable.post)) %>%
    mutate(agree_clearanswers.post = gsub("\n","; ", agree_clearanswers.post)) %>%
    mutate(agree_instr_enthusiasm.post = gsub("\n","; ", agree_instr_enthusiasm.post)) %>%
    mutate(agree_instr_interaction.post = gsub("\n","; ", agree_instr_interaction.post)) %>%
    mutate(agree_instr_knowledge.post = gsub("\n","; ", agree_instr_knowledge.post)) %>%
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

