library(tidyverse)
library(glue)

get_mean_scores_nresp <- function(results, only_matched=TRUE){
  # Select only required columns
  selectcol_results <- results %>%
  select(join_key, starts_with("agree_accessdata"), starts_with("agree_coding"), starts_with("agree_answers"),
         starts_with("agree_learning"), starts_with("agree_workdata"), starts_with("agree_doanalysis"),
         starts_with("agree_efficientdata"))
  
  # Count number of not NA responses in the last question presented to the learner in the survey
  n_resp_pre <- nrow(selectcol_results %>% filter(!is.na(agree_efficientdata.pre)))
  n_resp_post <- nrow(selectcol_results %>% filter(!is.na(agree_efficientdata.post)))
  n_resp_pre_post <- NA
  
  # If only matched responses, drop all responses with NA. If using all responses, do nothing and continue
  if (only_matched == TRUE) {
    selectcol_results <- selectcol_results %>% drop_na()
    n_resp_pre_post <- nrow(selectcol_results)
  } else {
      # Do nothing
    }
  
  # From wide to long format
  results_long <- selectcol_results %>%
    pivot_longer(
      cols = -join_key,
      names_to = c("skill", "time"),
      names_sep  = "\\.",
      values_to = "score"
    ) %>% 
    mutate(
      time = ifelse(time == "pre", "Pre-workshop", "Post-workshop"),
      skill = recode(skill,
                     "agree_accessdata" = "Having access to raw data is important",
                     "agree_coding" = "I can write a small program",
                     "agree_answers" = "I know how to search for answers",
                     "agree_learning" = "I can find ways of overcoming a problem",
                     "agree_workdata" = " I'm confident in my ability to use of programming w/ data",
                     "agree_doanalysis" = "Programming can make my analyses easier to reproduce",
                     "agree_efficientdata" = "Programming can make me more efficient w/ data"
      )
    )
  
  # If all responses, drop NAs now. If only matched responses, do nothing as NAs where already dropped
  if (only_matched == TRUE) {
    # Do nothing
  } else {
    results_long <- results_long %>% drop_na()
  }
  
  # Calculate mean scores per question
  mean_scores <- results_long %>% 
    group_by(skill, time) %>%
    summarize(mean = mean(score), .groups = 'drop') %>%
    spread(time, mean) %>%
    mutate(change = `Post-workshop` - `Pre-workshop`,
           percent_change = (change / `Pre-workshop`) * 100,
           change_text = sprintf("Change: %.2f (%.2f%%)", change, percent_change))
  
  # Return mean_scores df, an integers: n_resp_pre, n_resp_post, n_resp_pre_post
  return(list(mean_scores = mean_scores, n_resp_pre = n_resp_pre,
              n_resp_post = n_resp_post, n_resp_pre_post = n_resp_pre_post))
  
}

graph_pre_post <- function(mean_scores_df, n_resp_pre, n_resp_post, n_resp_pre_post, only_matched=TRUE) {
  # According to the desired graph, set the text for the title and subtitle of the graph
  if (only_matched == TRUE) {
    title_text <- "Skills and Perception Comparison - Matched responses"
    subtitle_text <- paste0("Responses: ", n_resp_pre_post)
  } else {
    title_text <- "Skills and Perception Comparison - All responses"
    subtitle_text <- paste0("Responses pre: ", n_resp_pre, " - Responses post: ", n_resp_post)
  }

  ggplot(mean_scores_df, aes(x = `Post-workshop`, y = reorder(skill, `Post-workshop`))) +
    geom_point(aes(color = "Post-workshop"), size = 4) +
    geom_point(aes(x = `Pre-workshop`, color = "Pre-workshop"), size = 4) +
    geom_text(aes(label = change_text, x = 5.2), hjust = 0, size = 2.5) +
    labs(title = title_text,
         subtitle = subtitle_text,
         x = "Average") +
    scale_color_manual(name = "Score", values = c("Pre-workshop" = "green", "Post-workshop" = "purple"),
                       limits = c("Pre-workshop", "Post-workshop")) +
    theme_minimal() +
    theme(legend.position = "top", legend.title = element_blank(),
          plot.margin = margin(5.5, 100, 5.5, 5.5), # Adjust right margin
          plot.background = element_rect(fill = "transparent", color = NA),
          plot.title = element_text(hjust = 0.5, size = 14),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = 10)) +
    coord_cartesian(xlim = c(NA, 5), clip = "off")
}

make_report <- function(title, c_workshops_id, data, output_name) {
  qmd <- glue('---
title: {double_quote(title)}
editor: visual
format: 
   html:
     code-fold: true
     message: false
     df-print: kable
---


## Number of responses

```{{r}}
#| message: false
library(tidyverse)
library(bslib)
library(shiny)
library(bsicons)
source("scripts/helper_functions.R")

# list of workshop IDs to filter results
workshops <- {c_workshops_id}

results <- read_csv({double_quote(data)}) %>% 
  filter(workshop %in% workshops)

pre_survey <- results %>%
  select(ends_with(".pre"))

post_survey <- results %>%
  select(ends_with(".post"))

n_pre <- sum(apply(post_survey, 1, function(row) all(is.na(row))))
n_post <- sum(apply(pre_survey, 1, function(row) all(is.na(row))))
n_total <- nrow(results)
n_both <- nrow(results) - n_pre - n_post

layout_columns(
  value_box(
    title = "Total responses", value = n_total, ,
    theme = NULL, showcase = bs_icon("people-fill"), showcase_layout = "left center",
    full_screen = FALSE, fill = TRUE, height = NULL
  ),
  value_box(
    title = "Both pre- and post-", value = n_both, , theme = NULL,
    showcase = bs_icon("arrows-expand-vertical"), showcase_layout = "left center",
    full_screen = FALSE, fill = TRUE, height = NULL
  ),
  value_box(
    title = "Only pre-workshop", value = n_pre, ,
    theme = NULL, showcase = bs_icon("arrow-left-short"), showcase_layout = "left center",
    full_screen = FALSE, fill = TRUE, height = NULL
  ),
  value_box(
    title = "Only post-workshop", value = n_post, , theme = NULL,
    showcase = bs_icon("arrow-right-short"), showcase_layout = "left center",
    full_screen = FALSE, fill = TRUE, height = NULL
  )
)

```

## Departments

```{{r}}
depts <- results %>% select(dept_select.pre) %>% 
  separate_rows(dept_select.pre, sep=",") %>% 
  count(dept_select.pre, name = "count") %>% 
  mutate(percent = (count / (n_total - n_post)) * 100,
         text = sprintf("%.0f (%.0f%%)", count, percent))

ggplot(depts, aes(y=reorder(dept_select.pre, count), x=count)) +
    geom_col() +
    geom_label(aes(label = text, hjust = -0.1),
               size = 3) +
    labs(x = "# respondents", y = element_blank()) +  
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
      ) +
    expand_limits(x = c(0,max(depts$count)*1.1))
```

### "Other" Departments

```{{r}}
other_depts <- results %>% 
  count(dept_other.pre, name = "count") %>% 
  drop_na() %>% 
  mutate(percent = (count / (n_total - n_post)) * 100,
         text = sprintf("%.0f (%.0f%%)", count, percent))

ggplot(other_depts, aes(y=reorder(dept_other.pre, count), x=count)) +
    geom_col() +
    geom_label(aes(label = text, hjust = -0.1),
               size = 3) +
    labs(x = "# respondents", y = element_blank()) + 
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
      ) +
    expand_limits(x = c(0,max(other_depts$count)*1.1))
```

## Current occupation / Career stage

```{{r}}
ocup <- results %>% select(occupation.pre) %>% 
  separate_rows(occupation.pre, sep=",") %>% 
  count(occupation.pre, name = "count") %>% 
  drop_na() %>% 
  mutate(percent = (count / (n_total - n_post)) * 100,
         text = sprintf("%.0f (%.0f%%)", count, percent))

ggplot(ocup, aes(y=reorder(occupation.pre, count), x=count)) +
    geom_col() +
    geom_label(aes(label = text, hjust = -0.1),
               size = 3) +
    labs(x = "# respondents", y = element_blank()) + 
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
      ) +
    expand_limits(x = c(0,max(ocup$count)*1.2))
```

## Motivation - Why are you participating in this workshop?

```{{r}}
motiv <- results %>% select(motivation_select.pre) %>% 
  separate_rows(motivation_select.pre, sep=",")  %>% 
  count(motivation_select.pre, name = "count") %>% 
  drop_na() %>% 
  mutate(percent = (count / (n_total - n_post)) * 100,
         text = sprintf("%.0f (%.0f%%)", count, percent))

ggplot(motiv, aes(y=reorder(motivation_select.pre, count), x=count)) +
    geom_col() +
    geom_label(aes(label = text, hjust = -0.1),
               size = 3) +
    labs(x = "# respondents", y = element_blank()) + 
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
      ) +
    expand_limits(x = c(0,max(motiv$count)*1.2))
```

## How did you find out about this workshop?

```{{r}}
findw <- results %>% select(findout_select.pre) %>% 
  separate_rows(findout_select.pre, sep=",")  %>% 
  count(findout_select.pre, name = "count") %>% 
  drop_na() %>% 
  mutate(percent = (count / (n_total - n_post)) * 100,
         text = sprintf("%.0f (%.0f%%)", count, percent))

ggplot(findw, aes(y=reorder(findout_select.pre, count), x=count)) +
    geom_col() +
    geom_label(aes(label = text, hjust = -0.1),
               size = 3) +
    labs(x = "# respondents", y = element_blank()) + 
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
      ) +
    expand_limits(x = c(0,max(findw$count)*1.2))
```

## What you most hope to learn?

```{{r}}
results %>% group_by(workshop) %>% 
  select(workshop, hopes.pre) %>% 
  drop_na()
```

## Learning environment in the workshop

```{{r}}
#| message: false
#| output: false
orderedq <- c("Strongly Disagree", "Somewhat Disagree", "Neither Agree or Disagree","Somewhat Agree", "Strongly Agree")
addNA(orderedq)
```

```{{r}}
agree_questions <- results %>% 
  select(join_key, agree_apply.post,	agree_comfortable.post,	agree_clearanswers.post,
         agree_instr_enthusiasm.post, agree_instr_interaction.post,	agree_instr_knowledge.post
) %>% 
  filter(!if_all(-join_key, is.na))

n_agree_questions <- nrow(agree_questions)
  
agree_questions <- agree_questions %>%
  pivot_longer(cols = -join_key, names_to = "Question", values_to = "Response") %>% 
  mutate(Response = factor(Response, levels = orderedq),
         Question = recode(Question,
                     "agree_apply.post" = "Can immediatly apply \n what they learned",
                     "agree_comfortable.post" = "Comfortable learning in \n the workshop environment",
                     "agree_clearanswers.post" = "Got clear answers \n from instructors",
                     "agree_instr_enthusiasm.post" = "Instructors were enthusiastic",
                     "agree_instr_interaction.post" = "Comfortable interacting \n with instructors",
                     "agree_instr_knowledge.post" = "Instructors were knowledgeable \n about the material"
      ))

summary_data <- agree_questions %>%
  count(Question, Response, name = "count") %>% 
  mutate(percent = (count / n_agree_questions) * 100,
         text = sprintf("%.0f (%.0f%%)", count, percent))

ggplot(summary_data, aes(x = Question, y = count, fill = Response)) +
  geom_col(position = "fill", color = "black", show.legend = TRUE) +
  scale_y_continuous(labels = scales::percent_format()) + 
  scale_fill_manual(values = c("Strongly Disagree" = "#d01c8b", 
                               "Somewhat Disagree" = "#f1b6da", 
                               "Neither Agree or Disagree" = "#f7f7f7", 
                               "Somewhat Agree" = "#b8e186", 
                               "Strongly Agree" = "#4dac26"), 
                    na.translate = TRUE, na.value = "#cccccc", 
                    breaks = orderedq, drop = FALSE) +
  geom_text(aes(label = text), size = 3,
             position = position_fill(vjust = 0.5)) +
  labs(y = "# respondents (Percentage)", x = element_blank(), fill = "Responses",
       subtitle = paste0("Number of responses: ", n_agree_questions)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.subtitle = element_text(hjust = 0.5, size = 12))
```

## How an instructor or helper affected your learning experience

```{{r}}
#| message: false
results %>% 
  group_by(workshop) %>% 
  select(workshop, instructor_example.post) %>%
  drop_na()
```

## Skills and perception comparison

```{{r}}
# Calculate mean scores and make graph for all respondents (only_matched=FALSE)
tryCatch(
  {{
mean_nresp <- get_mean_scores_nresp(results, only_matched=FALSE)
graph_pre_post(mean_nresp$mean_scores, mean_nresp$n_resp_pre, mean_nresp$n_resp_post, mean_nresp$n_resp_pre_post, only_matched=FALSE)
}},
error = function(cond) {{
message("Could not do the plots as there are no pre or post results to show")
}}
)


```

```{{r}}
# Calculate mean scores and make graph for only matched respondents in pre and post (only_matched=TRUE)
tryCatch(
  {{
mean_nresp <- get_mean_scores_nresp(results, only_matched=TRUE)
graph_pre_post(mean_nresp$mean_scores, mean_nresp$n_resp_pre, mean_nresp$n_resp_post, mean_nresp$n_resp_pre_post, only_matched=TRUE)
}},
error = function(cond) {{
message("Could not do the plots as there are no pre or post results to show")
}}
)
```

## Workshop Strengths

```{{r}}
results %>% 
  group_by(workshop) %>% 
  select(workshop, workshop_strengths.post) %>% 
  drop_na()
```

## Ways to improve the workshop

```{{r}}
results %>% 
  group_by(workshop) %>% 
  select(workshop, workshop_improved.post) %>% 
  drop_na()
```

## How likely are you to recommend this workshop? Scale 0 - 10

```{{r}}
orderedq <- c("Detractor", "Passive", "Promoter")

nps <- results %>% 
  count(recommend_group.post, recommende_score.post, name = "count") %>% 
  drop_na() %>% 
  mutate(recommend_group.post = factor(recommend_group.post, levels = orderedq),
         percent = (count/sum(count)) * 100,
         text = sprintf("%.0f (%.0f%%)", count, percent))

nps %>% 
ggplot(aes(x=recommende_score.post, y=count, fill=recommend_group.post)) +
  geom_col(color="black", show.legend = TRUE) +
  scale_fill_manual(values = c("Detractor" = "#af8dc3", "Passive" = "#f7f7f7", "Promoter" = "#7fbf7b"), breaks = c("Detractor", "Passive", "Promoter"), drop = FALSE) +
  geom_label(aes(label = text, vjust = -0.5), fill = "white", size= 3) +
  scale_x_continuous(breaks = 1:10) +
  labs(x = "NPS Score", y = "# respondents", subtitle = paste0("Number of responses: ", sum(nps$count), "\n Mean score: ", format(weighted.mean(nps$recommende_score.post, nps$count), digits = 3))) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  ) +
  expand_limits(x = c(1,10),
                y = c(0, max(nps$count)*1.1))

```

## Topic Suggestions

```{{r}}
results %>% 
  group_by(workshop) %>% 
  select(workshop, suggest_topics.post) %>% 
  drop_na()
```

')
writeLines(qmd, output_name)
}