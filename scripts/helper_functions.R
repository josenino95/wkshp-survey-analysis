library("tidyverse")

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