# 11. Quality of Life - USIQoL ####
## 11.1 Load libraries ####
library(tidyverse)
library(janitor)
library(gt)
library(gtExtras)
library(ggsignif)
library(data.table)
library(pryr)
library(lubridate)
library(sfsmisc)
library(mice)
library(cowplot)


## 11.2 Load data ####
### 11.2.1 Read in data ####

# The commented code has been included for transparency as to how 
# the USIQoL data has been summarised - summarised datasets have been 
# saved for replication

#usiqol_metrics <- fread("Inputs/usiqol_scores.csv", header = TRUE) %>% 
#  janitor::clean_names() %>%
#  as_tibble() %>%
#  subset(select = c(date_of_birth,
#                    age,
#                    trial_number,
#                    date_completed,
#                    total_pre,
#                    date_completed_2,
#                    total_post_1,
#                    date_completed_3,
#                    total_post_2))

#usiqol_stone_sizes_pre_post <- fread("Inputs/stone_free_statuses_usiqol_2.csv", header = TRUE) %>% 
#  janitor::clean_names() %>%
#  as_tibble()

### 11.2.3 Define data ####

#usiqol_metrics$date_of_birth <- as.POSIXct(usiqol_metrics$date_of_birth, format = "%d/%m/%Y")
#usiqol_metrics$age <- as.integer(usiqol_metrics$age)
#usiqol_metrics$trial_number <- as.integer(usiqol_metrics$trial_number)
#usiqol_metrics$date_completed <- as.POSIXct(usiqol_metrics$date_completed, format = "%d/%m/%Y")
#usiqol_metrics$total_pre <- as.integer(usiqol_metrics$total_pre)
#usiqol_metrics$date_completed_2 <- as.POSIXct(usiqol_metrics$date_completed_2, format = "%d/%m/%Y")
#usiqol_metrics$total_post_1 <- as.integer(usiqol_metrics$total_post_1)
#usiqol_metrics$date_completed_3 <- as.POSIXct(usiqol_metrics$date_completed_3, format = "%d/%m/%Y")
#usiqol_metrics$total_post_2 <- as.integer(usiqol_metrics$total_post_2)

#usiqol_stone_sizes_pre_post$sex <- as.factor(usiqol_stone_sizes_pre_post$sex)
#usiqol_stone_sizes_pre_post$intervention <- as.factor(usiqol_stone_sizes_pre_post$intervention)
#usiqol_stone_sizes_pre_post$successful <- as.factor(usiqol_stone_sizes_pre_post$successful)
#usiqol_stone_sizes_pre_post$pre_op_stone_free_status <- as.integer(usiqol_stone_sizes_pre_post$pre_op_stone_free_status)
#usiqol_stone_sizes_pre_post$post_op_stone_free_status <- as.integer(usiqol_stone_sizes_pre_post$post_op_stone_free_status)

### 11.2.4 Assign age bands ####

#usiqol_metrics <- usiqol_metrics %>%
#  mutate(
#    age_1 = age,
#    age_2 = as.numeric(difftime(date_completed_2, date_of_birth, units = "days")) / 365.25,
#    age_3 = as.numeric(difftime(date_completed_3, date_of_birth, units = "days")) / 365.25,
#    age_bin = case_when(
#      age_1 < 5 ~ "Aged 1 to 4", 
#      age_1 >4 & age_1 < 10 ~ "Aged 5 to 9",
#      age_1 >9 & age_1 < 15 ~ "Aged 10-14",
#      age_1 >14 & age_1 < 20 ~ "Aged 15-19",
#      age_1 >19 & age_1 < 25 ~ "Aged 20-24" ,     
#      age_1 >24 & age_1 < 30 ~ "Aged 25-29",
#      age_1 >29 & age_1 < 35 ~ "Aged 30-34", 
#      age_1 >34 & age_1 < 40 ~ "Aged 35-39", 
#      age_1 >39 & age_1 < 45 ~ "Aged 40-44",      
#      age_1 >44 & age_1 < 50 ~ "Aged 45-49",   
#      age_1 >49 & age_1 < 55 ~ "Aged 50-54",   
#      age_1 >54 & age_1 < 60 ~ "Aged 55-59",      
#      age_1 >59 & age_1 < 65 ~ "Aged 60-64",
#      age_1 >64 & age_1 < 70 ~ "Aged 65-69", 
#      age_1 >69 & age_1 < 75 ~ "Aged 70-74",  
#      age_1 >74 & age_1 < 80 ~ "Aged 75-79",      
#      age_1 >79 & age_1 < 85 ~ "Aged 80-84",
#      age_1 >84 & age_1 < 90 ~ "Aged 85-89",
#      age_1 >89 ~ "Aged 90 and over"
#    ),
#    age_bin = factor(age_bin, levels = c("Aged 1 to 4",
#                                         "Aged 5 to 9",
#                                         "Aged 10-14",
#                                         "Aged 15-19",
#                                         "Aged 20-24",
#                                         "Aged 25-29",
#                                         "Aged 30-34",
#                                         "Aged 35-39",
#                                         "Aged 40-44",      
#                                         "Aged 45-49",
#                                         "Aged 50-54",
#                                         "Aged 55-59",      
#                                         "Aged 60-64",
#                                         "Aged 65-69",
#                                         "Aged 70-74",
#                                         "Aged 75-79",      
#                                         "Aged 80-84",
#                                         "Aged 85-89",
#                                         "Aged 90 and over")),
#    .keep = "all"
#  ) %>% 
#  drop_na(trial_number)

#usiqol_stone_sizes_pre_post <- usiqol_stone_sizes_pre_post %>%
#  subset(select = c(
#    trial_number,
#    pre_op_stone_free_status,
#    post_op_stone_free_status,
#    intervention,
#    successful
#  )) %>% 
#  drop_na(trial_number)

### 11.2.5 Assign stone free status ####

#usiqol_metrics_aggregated <- usiqol_metrics %>%
#  left_join(usiqol_stone_sizes_pre_post,
#            by = c("trial_number" = "trial_number")) %>%
#  mutate(stone_free_status_pre = case_when(
#    pre_op_stone_free_status == 0 ~ "SF",
#    pre_op_stone_free_status >=4 ~ "more4",
#    pre_op_stone_free_status < 4 ~ "less4",
#    TRUE ~ NA_character_
#  ),
#  stone_free_status_post = case_when(
#    post_op_stone_free_status == 0 ~ "SF",
#    post_op_stone_free_status >=4 ~ "more4",
#    post_op_stone_free_status < 4 ~ "less4",
#    TRUE ~ NA_character_
#  )) 

### 11.2.6 Get change with treatment ####

#usiqol_change_with_rx <- usiqol_metrics_aggregated %>%
#  group_by(intervention, stone_free_status_pre) %>%
#  summarise(
#    n = sum(!is.na(total_post_1 - total_pre)),
#    qol_change = mean(total_post_1 - total_pre, na.rm = TRUE),
#    qol_sd = sd(total_post_1 - total_pre, na.rm = TRUE),
#    qol_se = qol_sd / sqrt(n)
#  ) %>% 
#  ungroup() %>% 
#  subset(select = c(
#    intervention,
#    stone_free_status_pre,
#    qol_change,
#    qol_sd,
#    qol_se
#  ))

#write.csv(usiqol_change_with_rx,
#          "usiqol_change_with_rx.csv")

usiqol_change_with_rx <- fread("Inputs/usiqol_change_with_rx.csv") %>% 
  as_tibble() %>%
  select(-V1) %>%
  mutate(
    intervention = as.factor(intervention),
    stone_free_status_pre = as.factor(stone_free_status_pre)
  )

mean_change_se_1 <- usiqol_change_with_rx %>% 
  select(qol_se) %>%
  drop_na(qol_se) %>%
  subset(qol_se < 4) %>%
  summarise(
    qol_se = mean(qol_se)
  )

mean_change_se <- mean_change_se_1$qol_se  

mean_change_sd_1 <- usiqol_change_with_rx %>% 
  select(qol_sd) %>%
  drop_na(qol_sd) %>%
  subset(qol_sd < 13) %>%
  summarise(
    qol_sd = mean(qol_sd)
  )

mean_change_sd <- mean_change_sd_1$qol_sd 

# Complete dataset to get categories to be populated with dummy values
usiqol_change_with_rx <- usiqol_change_with_rx %>%
  drop_na(intervention) %>%
  mutate(
    intervention = factor(intervention, levels = c(
      "Colic",
      "ESWL",
      "URS",
      "PCNL"
    ))
  ) %>%
  complete(
    intervention,
    stone_free_status_pre = c("more4", "less4", "SF"),
    fill = list(
      qol_change = NA,
      qol_se = NA,
      qol_sd = NA
    )
  ) %>%
  mutate(
    qol_change = case_when(
      is.na(qol_change) & stone_free_status_pre == "SF" ~ 4,
      is.na(qol_change) & stone_free_status_pre == "less4" ~ -4,
      TRUE ~ qol_change
    ),
    qol_sd = case_when(
      is.na(qol_sd) | qol_sd > 13 ~ mean_change_sd,
      TRUE ~ qol_sd
    ),
    qol_se = case_when(
      is.na(qol_se) | qol_se > 4 ~ mean_change_se,
      TRUE ~ qol_se
    ),
    qol_change_ci_lower = qol_change - 1.96 * qol_se,
    qol_change_ci_upper = qol_change + 1.96 * qol_se
  )

# Plot output
usiqol_change_with_rx %>%
  ggplot(aes(
    x = intervention,
    y = qol_change,
    color = stone_free_status_pre,
    fill = stone_free_status_pre
  )) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = qol_change_ci_lower, ymax = qol_change_ci_upper),
                position = position_dodge(width = 0.9),
                width = 0.25,
                color = "black") +
  labs(
    x = "Colic/Intervention",
    y = "Mean Change in USIQOL score",
    fill = "Prior Stone Free Status",
    color = "Prior Stone Free Status"
  )


### 11.2.7 Get final tibble for score assignment by age_band and sf_status ####

#usiqol_metrics_by_age_stone_free_status <-  
#  usiqol_metrics_aggregated  %>%
#  subset(select = c(age_bin,
#                    total_pre,
#                    total_post_1,
#                    stone_free_status_pre,
#                    stone_free_status_post
#  )) %>% 
#  pivot_longer(cols = c(total_pre,total_post_1),
#               names_to = "scored_when",
#               values_to = "usiqol_scores") %>%
#  mutate(
#    stone_free_status = case_when(
#      scored_when == "total_pre" ~ stone_free_status_pre,
#      scored_when == "total_post_1" ~ stone_free_status_post,
#      TRUE ~ NA_character_
#    ) %>% as.factor()
#  ) %>%
#  subset(select = -c(
#    stone_free_status_pre,
#    stone_free_status_post,
#    scored_when
#  )) %>%
#  group_by(age_bin, stone_free_status) %>%
#  summarise(
#    n = sum(!is.na(usiqol_scores)),
#    total_mean = mean(usiqol_scores, na.rm = TRUE),
#    total_sd   = sd(usiqol_scores, na.rm = TRUE),
#    total_se = total_sd / sqrt(n),
#    total_ci_lower = total_mean - 1.96 * total_se,
#    total_ci_upper = total_mean + 1.96 * total_se
#  ) %>% 
#  select(-n) %>%
#  ungroup()

#write.csv(usiqol_metrics_by_age_stone_free_status,
#          "usiqol_metrics_by_age_stone_free_status.csv")

usiqol_metrics_by_age_stone_free_status <- fread("Inputs/usiqol_metrics_by_age_stone_free_status.csv") %>%
  as_tibble() %>%
  select(-V1) %>%
  mutate(
    age_bin = as.factor(age_bin),
    stone_free_status = as.factor(stone_free_status)
  )

str(usiqol_metrics_by_age_stone_free_status)

na_plot <- usiqol_metrics_by_age_stone_free_status %>%
  ggplot(aes(
    x = age_bin,
    y = total_mean,
    color = stone_free_status,
    fill = stone_free_status
  )) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = total_ci_lower, ymax = total_ci_upper),
                position = position_dodge(width = 0.9),
                width = 0.25,
                color = "black") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(
    x = "Age bin (years)",
    y = "Mean USIQOL score",
    fill = "Stone Free Status",
    color = "Stone Free Status"
  )


### 11.2.8 Deal with missing data/outliers ####
#### 11.2.8.1 Populate with Means/SD ####
# Some stone free statuses are missing within age bins - populate with sensible values
difference_between_levels <- usiqol_metrics_by_age_stone_free_status %>%
  subset(select= c(total_mean,
                   stone_free_status)) %>%
  drop_na(stone_free_status,total_mean) %>%
  group_by(stone_free_status) %>%
  summarise(
    total_mean = mean(total_mean)
  )

sf_mean <- difference_between_levels$total_mean[1]
less4_mean <- difference_between_levels$total_mean[2]
more4_mean <- difference_between_levels$total_mean[3]


usiqol_metrics_by_age_stone_free_status2 <- usiqol_metrics_by_age_stone_free_status %>%
  complete(age_bin, stone_free_status = c("more4", "less4", "SF"),
           fill = list(total_mean = NA, 
                       total_se = NA, 
                       total_ci_lower = NA, 
                       total_ci_upper = NA)) %>%
  subset(age_bin != "Aged 1 to 4") %>%
  arrange(age_bin) %>%
  mutate(
    # Adjust scores to decrease after the mean is achieved (aged 40-45)
    is_after_40_45 = as.numeric(age_bin) > as.numeric(age_bin[age_bin == "Aged 40 to 45"][1]),
    bins_after = if_else(is_after_40_45, 
                         as.numeric(age_bin) - as.numeric(age_bin[age_bin == "Aged 40 to 45"][1]), 
                         0),
    total_mean = case_when(
      is.na(total_mean) & stone_free_status == "SF" & is_after_40_45 ~ sf_mean - (bins_after * 3),
      is.na(total_mean) & stone_free_status == "less4" & is_after_40_45 ~ less4_mean - (bins_after * 3),
      is.na(total_mean) & stone_free_status == "more4" & is_after_40_45 ~ more4_mean - (bins_after * 3),
      is.na(total_mean) & stone_free_status == "SF" ~ sf_mean,
      is.na(total_mean) & stone_free_status == "less4" ~ less4_mean,
      is.na(total_mean) & stone_free_status == "more4" ~ more4_mean,
      TRUE ~ total_mean
    )
  ) %>%
  select(-is_after_40_45, -bins_after)



# CIs for some are either very wide or missing - amend to assign as average of those with reasonable values
# Get mean for lower CI excluding those < 0
mean_se <- usiqol_metrics_by_age_stone_free_status %>%
  subset(select = c(total_ci_lower, total_se)) %>%
  mutate(total_ci_lower_se = ifelse(total_ci_lower < 0,
                                    NA,
                                    total_se)) %>%
  drop_na(total_ci_lower_se) %>%
  summarise(mean_se = mean(total_ci_lower_se))

mean_sd <- usiqol_metrics_by_age_stone_free_status %>%
  subset(select = c(total_ci_lower, total_sd)) %>%
  mutate(total_ci_lower_sd = ifelse(total_ci_lower < 0,
                                    NA,
                                    total_sd)) %>%
  drop_na(total_ci_lower_sd) %>%
  summarise(mean_sd = mean(total_ci_lower_sd))

usiqol_metrics_by_age_stone_free_status2 <- usiqol_metrics_by_age_stone_free_status2 %>%
  mutate(
    total_sd = case_when(
      is.na(total_sd) ~ mean_sd$mean_sd,
      TRUE ~ total_sd
    ),
    total_se = case_when(
      is.na(total_se) ~ mean_se$mean_se,
      TRUE ~ total_se
    ),
    total_ci_lower = case_when(
      total_ci_lower < 0 | is.na(total_ci_lower) ~ (total_mean - 1.96 * mean_se$mean_se),
      TRUE ~ total_ci_lower
    ),
    total_ci_upper = case_when(
      total_ci_upper > 50 | is.na(total_ci_upper) ~  (total_mean + 1.96 * mean_se$mean_se),
      TRUE ~ total_ci_upper
    )
  ) %>% drop_na(stone_free_status)

#### 11.2.8.2 Imputation ####
#usiqol_long <- usiqol_metrics_aggregated %>%
#  select(age_bin,
#         total_pre,
#         total_post_1,
#         stone_free_status_pre,
#         stone_free_status_post
#  ) %>%
#  pivot_longer(
#    cols = c(total_pre, total_post_1),
#    names_to = "scored_when",
#    values_to = "usiqol_scores"
#  ) %>%
#  mutate(
#    stone_free_status = case_when(
#      scored_when == "total_pre"  ~ stone_free_status_pre,
#      scored_when == "total_post_1" ~ stone_free_status_post,
#      TRUE ~ NA_character_
#    ) %>% as.factor()
#  ) %>%
#  select(-stone_free_status_pre, -stone_free_status_post, -scored_when) %>%
#  complete(age_bin, stone_free_status = c("more4", "less4", "SF"), 
#           fill = list(usiqol_scores = NA)) %>%
#  filter(age_bin != "Aged 1 to 4") %>%
#  drop_na(stone_free_status)

#ini <- mice(usiqol_long, m = 5, maxit = 0, print = FALSE)
#meth <- ini$method
#meth["usiqol_scores"] <- "lasso.norm"  

#post <- ini$post
#post["usiqol_scores"] <- "imp[[j]] <- pmax(pmin(imp[[j]], 60), 15)"

#imputed <- mice(usiqol_long, method = meth, m = 5, post = post, print = FALSE)

#usiqol_metrics_by_age_stone_free_status1 <- complete(imputed) %>%
#  group_by(age_bin, stone_free_status) %>%
#  summarise(
#    n = sum(!is.na(usiqol_scores)),
#    total_mean = mean(usiqol_scores, na.rm = TRUE),
#    total_sd   = sd(usiqol_scores, na.rm = TRUE),
#    total_se   = total_sd / sqrt(n),
#    total_ci_lower = total_mean - 1.96 * total_se,
#    total_ci_upper = total_mean + 1.96 * total_se
#  ) %>%
#  select(-n) %>%
#  ungroup()

#write.csv(usiqol_metrics_by_age_stone_free_status1,
#          "usiqol_metrics_by_age_stone_free_status1.csv")

usiqol_metrics_by_age_stone_free_status1 <- fread("Inputs/usiqol_metrics_by_age_stone_free_status1.csv") %>%
  select(-V1) %>%
  mutate(
    age_bin = as.factor(age_bin),
    stone_free_status = as.factor(stone_free_status)
  )


usiqol_metrics_by_age_stone_free_status1 <- usiqol_metrics_by_age_stone_free_status1 %>%
  mutate(
    total_sd = case_when(
      is.na(total_sd) ~ mean_sd$mean_sd,
      TRUE ~ total_sd
    ),
    total_se = case_when(
      is.na(total_se) ~ mean_se$mean_se,
      TRUE ~ total_se
    ),
    total_ci_lower = case_when(
      total_ci_lower < 0 | is.na(total_ci_lower) ~ (total_mean - 1.96 * mean_se$mean_se),
      TRUE ~ total_ci_lower
    ),
    total_ci_upper = case_when(
      total_ci_upper > 50 | is.na(total_ci_upper) ~  (total_mean + 1.96 * mean_se$mean_se),
      TRUE ~ total_ci_upper
    )
  )

mice_imputation_qol_plot <- usiqol_metrics_by_age_stone_free_status1 %>%
  ggplot(aes(
    x = age_bin,
    y = total_mean,
    color = stone_free_status,
    fill = stone_free_status
  )) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = total_ci_lower, ymax = total_ci_upper),
                position = position_dodge(width = 0.9),
                width = 0.25,
                color = "black") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(
    x = "Age bin (years)",
    y = "Mean USIQOL score",
    fill = "Stone Free Status",
    color = "Stone Free Status"
  )

### 11.2.9 Plot output ####
means_plot <- usiqol_metrics_by_age_stone_free_status2 %>%
  ggplot(aes(
    x = age_bin,
    y = total_mean,
    color = stone_free_status,
    fill = stone_free_status
  )) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = total_ci_lower, ymax = total_ci_upper),
                position = position_dodge(width = 0.9),
                width = 0.25,
                color = "black") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(
    x = "Age bin (years)",
    y = "Mean USIQOL score",
    fill = "Stone Free Status",
    color = "Stone Free Status"
  )

### 11.2.10 Plot all data manipulations together ####
plot_grid(na_plot, means_plot, mice_imputation_qol_plot,
          labels = c("A",
                     "B",
                     "C"))

### 11.2.11 Assign QOL Scores going forwards ####
usiqol_metrics_by_age_stone_free_status <- usiqol_metrics_by_age_stone_free_status2 # Mean scores

# usiqol_metrics_by_age_stone_free_status1 # imputed scores  


## 11.3 Functions to assign QOL scores - with MC simulation #### 
### 11.3.1 Assign Age helper function ####
assign_age_helper <- function(data,
                              baseline = TRUE,
                              fu_years = NULL) {
  
  age_bins_levels <- c("Aged 1 to 4",
                       "Aged 5 to 9",
                       "Aged 10-14",
                       "Aged 15-19",
                       "Aged 20-24",
                       "Aged 25-29",
                       "Aged 30-34",
                       "Aged 35-39",
                       "Aged 40-44",      
                       "Aged 45-49",
                       "Aged 50-54",
                       "Aged 55-59",      
                       "Aged 60-64",
                       "Aged 65-69",
                       "Aged 70-74",
                       "Aged 75-79",      
                       "Aged 80-84",
                       "Aged 85-89",
                       "Aged 90 and over")
  
  get_age_bin <- function(age_vec) {
    cut(age_vec,
        breaks = c(-Inf, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, Inf),
        labels = age_bins_levels)
  }
  
  if (baseline) {
    data <- data %>%
      mutate(age_bin = get_age_bin(as.numeric(age)))
  } else if (!is.null(fu_years)) {
    # For follow-up, use age_fu column to create age_bin
    data <- data %>%
      mutate(age_bin = get_age_bin(as.numeric(age_fu)))
  }
  
  return(data)
}

### 11.3.2 Precalculate MC distributions ####
mc_reps <- 100
qol_mc_lookup <- usiqol_change_with_rx %>%
  mutate(
    mc_dist = pmap(
      list(qol_change, qol_sd),
      ~ rnorm(mc_reps, mean = ..1, sd = ..2)
    )
  ) %>%
  select(intervention, stone_free_status_pre, mc_dist)

### 11.3.3 Get First intervention year helper function ####
get_first_intervention_year <- function(df,
                                        years = 1:5,
                                        prefix = "colic_intervention_type_year_") {
  cols <- paste0(prefix, years)
  
  # Ensure character (so "No" vs intervention)
  tmp <- df[cols]
  tmp[] <- lapply(tmp, as.character)
  
  df$first_intervention_year <- apply(tmp, 1, function(r) {
    idx <- which(!is.na(r) & r != "No")
    if (length(idx) == 0)
      return(NA_integer_)
    idx[1]  
  })
  
  df
}

### 11.3.4 Assign QOL function ####
assign_qol_chunked <- function(data,
                               baseline_qol = usiqol_metrics_by_age_stone_free_status,
                               event_qol_change = usiqol_change_with_rx,
                               year_cols = c(1,2,3,4,5),
                               mc_reps = 100,
                               ci_level = 0.95,
                               chunk_size = 10,  
                               verbose = TRUE) {
  
  max_qol <- 60
  alpha <- (1 - ci_level) / 2
  
  vcat <- function(...) if (verbose) message(...)
  
  n <- nrow(data)
  total_chunks <- chunk_size  
  rows_per_chunk <- ceiling(n / chunk_size) 
  
  start_time <- Sys.time()
  results_list <- vector("list", total_chunks)
  
  for (i in seq_len(total_chunks)) {
    
    idx_start <- (i - 1) * rows_per_chunk + 1
    idx_end <- min(i * rows_per_chunk, n)
    chunk <- data[idx_start:idx_end, ]
    
    if (verbose) {
      pct <- round(i / total_chunks * 100)
      vcat("---- Chunk ", i, "/", total_chunks, " (", pct, "%) ----")
    }
    
    # Compute ages for baseline & follow-up years
    chunk <- chunk %>%
      mutate(
        baseline_age = age,
        age_fu_year_1 = age + 1,
        age_fu_year_2 = age + 2,
        age_fu_year_3 = age + 3,
        age_fu_year_4 = age + 4,
        age_fu_year_5 = age + 5,
        .keep = "all"
      )
    
    # Assign baseline age bin
    chunk <- chunk %>%
      assign_age_helper(baseline = TRUE) %>%   
      rename(baseline_age_bin = age_bin)
    
    # Assign age bins for each follow-up year
    for (fu_year in 1:5) {
      chunk <- chunk %>%
        mutate(age_fu = .data[[paste0("age_fu_year_", fu_year)]]) %>%
        assign_age_helper(baseline = FALSE, fu_years = fu_year) %>%
        rename(!!paste0("age_bin_fu_year_", fu_year) := age_bin) %>%
        select(-age_fu)
    }
    
    # Get baseline QoL distribution parameters (for regenerating samples when age bin changes)
    chunk <- chunk %>%
      left_join(
        baseline_qol %>% select(age_bin, stone_free_status, total_mean, total_sd),
        by = c("baseline_age_bin" = "age_bin", "stone_free_status")
      )
    
    # Helper function to apply QoL adjustments to individual MC samples
    apply_qol_adjustments <- function(samples, stone_free_status, prediction, 
                                      first_intervention_year, current_year) {
      adjusted <- samples
      
      # Base adjustments
      if (!is.na(stone_free_status) && stone_free_status != "SF") {
        adjusted <- adjusted + 1
      }
      
      if (!is.na(prediction)) {
        if (prediction == "Yes") {
          adjusted <- adjusted + 1
        } else if (prediction == "No") {
          adjusted <- adjusted - 1
        }
      }
      
      # Intervention-specific adjustments for follow-up years
      if (!is.na(first_intervention_year) && !is.na(current_year) && current_year > 0) {
        if (first_intervention_year < current_year + 1) {
          if (!is.na(stone_free_status) && !is.na(prediction)) {
            if (stone_free_status == "SF" && prediction == "No") {
              adjusted <- adjusted + 3
            } else if (stone_free_status != "SF" && prediction == "No") {
              adjusted <- adjusted + 1
            } else if (stone_free_status == "SF" && prediction == "Yes") {
              adjusted <- adjusted + 2
            } else if (stone_free_status != "SF" && prediction == "Yes") {
              adjusted <- adjusted + 1
            }
          }
        }
      }
      
      return(adjusted)
    }
    
    # Create baseline QoL samples from existing mean/lower/upper for propagation
    chunk <- chunk %>%
      mutate(
        baseline_qol_samples = pmap(
          list(baseline_qol_mean, baseline_qol_lower, baseline_qol_upper),
          function(mean_val, lower_val, upper_val) {
            if (is.na(mean_val)) return(rep(NA_real_, mc_reps))
            
            # Estimate SD from CI range (assuming normal distribution)
            # CI range ≈ 2 * 1.96 * SD for 95% CI
            estimated_sd <- (upper_val - lower_val) / (2 * qnorm(1 - alpha))
            
            # Generate samples
            samples <- rnorm(mc_reps, mean_val, estimated_sd)
            
            # Apply bounds
            pmin(max_qol, pmax(0, samples))
          }
        )
      )
    
    # Follow-up years with adjustments
    for (fu_year in 1:5) {
      
      death_col <- paste0("death_year_", fu_year)
      
      prev_qol_samples <- if (fu_year == 1) "baseline_qol_samples" else paste0("qol_samples_year_", fu_year - 1)
      prev_qol_mean <- if (fu_year == 1) "baseline_qol_mean" else paste0("qol_mean_year_", fu_year - 1)
      prev_age_bin <- if (fu_year == 1) "baseline_age_bin" else paste0("age_bin_fu_year_", fu_year - 1)
      current_age_bin <- paste0("age_bin_fu_year_", fu_year)
      intervention_col <- paste0("colic_intervention_type_year_", fu_year)
      
      # Join intervention change data for this year
      chunk <- chunk %>%
        left_join(
          event_qol_change %>% 
            select(intervention, stone_free_status_pre, qol_change, qol_sd) %>%
            rename(!!paste0("intervention_qol_change_", fu_year) := qol_change,
                   !!paste0("intervention_qol_sd_", fu_year) := qol_sd),
          by = c(setNames("intervention", intervention_col),
                 setNames("stone_free_status_pre", "stone_free_status1"))
        )
      
      # Generate QoL samples for this year
      chunk <- chunk %>%
        mutate(
          !!paste0("qol_samples_year_", fu_year) := pmap(
            list(
              total_mean, total_sd, stone_free_status, prediction, 
              first_intervention_year, .data[[intervention_col]],
              .data[[prev_age_bin]], .data[[current_age_bin]],
              .data[[prev_qol_samples]], .data[[prev_qol_mean]],
              .data[[paste0("intervention_qol_change_", fu_year)]],
              .data[[paste0("intervention_qol_sd_", fu_year)]],
              .data[[death_col]]  
            ),
            function(mean_val, sd_val, sf_status, pred, first_interv, interv_type,
                     prev_bin, curr_bin, prev_samples, prev_mean,
                     interv_change, interv_sd, death_status) {
              
              if (!is.na(death_status) && death_status == "Yes") {
                return(rep(0, mc_reps))  
              }
              
              # Case 1: Intervention in this year - apply intervention-based QoL change
              if (!is.na(interv_type) && interv_type != "No" && 
                  !is.na(first_interv) && first_interv == fu_year) {
                
                # If we have intervention change data, apply it
                if (!is.na(interv_change) && !is.na(interv_sd)) {
                  # Generate change samples from intervention distribution
                  change_samples <- rnorm(mc_reps, interv_change, interv_sd)
                  
                  # Apply change to previous QoL samples
                  new_samples <- prev_samples + change_samples
                  
                  # Apply bounds
                  return(pmin(max_qol, pmax(0, new_samples)))
                } else {
                  # No intervention data available - keep previous QoL
                  return(prev_samples)
                }
              }
              
              # Case 2: Age bin hasn't changed - keep previous QoL
              if (!is.na(prev_bin) && !is.na(curr_bin) && prev_bin == curr_bin) {
                return(prev_samples)
              }
              
              # Case 3: Age bin changed - regenerate with adjustments
              if (is.na(mean_val) || is.na(sd_val)) return(prev_samples)
              
              # Generate raw samples from new age bin distribution
              raw_samples <- rnorm(mc_reps, mean_val, sd_val)
              
              # Apply prediction-based adjustments
              adjusted_samples <- apply_qol_adjustments(
                raw_samples, sf_status, pred, first_interv, current_year = fu_year
              )
              
              # Apply bounds
              pmin(max_qol, pmax(0, adjusted_samples))
            }
          ),
          !!paste0("qol_mean_year_", fu_year) := map_dbl(
            .data[[paste0("qol_samples_year_", fu_year)]], 
            ~mean(.x, na.rm = TRUE)
          ),
          !!paste0("qol_lower_year_", fu_year) := map_dbl(
            .data[[paste0("qol_samples_year_", fu_year)]], 
            ~quantile(.x, probs = alpha, na.rm = TRUE)
          ),
          !!paste0("qol_upper_year_", fu_year) := map_dbl(
            .data[[paste0("qol_samples_year_", fu_year)]], 
            ~quantile(.x, probs = 1 - alpha, na.rm = TRUE)
          ),
          !!paste0("qol_se_year_", fu_year) := map_dbl(
            .data[[paste0("qol_samples_year_", fu_year)]],
            ~sd(.x, na.rm = TRUE)
          )
        ) %>%
        # Clean up temporary intervention columns
        select(-!!paste0("intervention_qol_change_", fu_year),
               -!!paste0("intervention_qol_sd_", fu_year))
    }
    
    # Remove all sample columns to save memory (keep only summary stats)
    sample_cols <- c("baseline_qol_samples", paste0("qol_samples_year_", 1:5))
    chunk <- chunk %>% select(-any_of(sample_cols))
    
    results_list[[i]] <- chunk
    
    if (verbose) {
      elapsed <- difftime(Sys.time(), start_time, units = "secs")
      est_remaining <- elapsed / i * (total_chunks - i)
      vcat(" ✅ chunk ", i, " done | ⏳ remaining: ",
           round(as.numeric(est_remaining), 1), " sec")
    }
  }
  
  bind_rows(results_list)
}

### 11.3.5 Pre-assign baseline scores per unique id #### 
# Scores will then be altered by recurrence status, risk status and age
assign_baseline_qol <- function(df,
                                baseline_qol = usiqol_metrics_by_age_stone_free_status,
                                n_sim = 1000) {
  
  # Clean age bins 
  baseline_qol1 <- baseline_qol %>%
    mutate(
      age_bin = str_replace(age_bin, "Aged ", ""),
      age_bin = str_replace(age_bin, " to ", "-"),
      age_bin = case_when(
        age_bin == "90 and over" ~ ">90",
        TRUE ~ age_bin
      ),
      age_bin = as.factor(age_bin)
    )
  
  # Filter baseline cohort
  df1 <- df %>% filter(auc_target == 0.55)
  
  df1 <- df1 %>%
    mutate(
      age_band = case_when(
        age < 15 ~ "15-19",
        TRUE ~ age_band
      )
    )
  
  df1$age_band <- as.factor(df1$age_band)
  age_list <- levels(df1$age_band)
  sf_levels <- levels(df1$stone_free_status)
  
  df_out <- list()
  counter <- 1
  
  for (age_b in age_list) {
    for (sf_level in sf_levels) {
      
      baseline_row <- baseline_qol1 %>%
        filter(age_bin == age_b,
               stone_free_status == sf_level)
      
      if (nrow(baseline_row) == 0) next
      
      qol_mean <- baseline_row$total_mean
      qol_sd   <- baseline_row$total_sd
      
      df_sub <- df1 %>%
        filter(age_band == age_b,
               stone_free_status == sf_level)
      
      n_subgroup <- nrow(df_sub)
      if (n_subgroup == 0) next
      
      # MC simulation for USIQoL score assignment
      mc_draws <- replicate(
        n = n_sim,
        expr = {
          vals <- rnorm(n_subgroup, mean = qol_mean, sd = qol_sd)
          # Clip to min/max USIQOL scores
          pmin(pmax(vals, 15), 60)
        }
      )
      
      # Summarise MC sim for each individual
      baseline_qol_mean  <- rowMeans(mc_draws)
      baseline_qol_lower <- apply(mc_draws, 1, quantile, probs = 0.025)
      baseline_qol_upper <- apply(mc_draws, 1, quantile, probs = 0.975)
      
      df_assigned <- df_sub %>%
        mutate(
          baseline_qol_mean  = round(baseline_qol_mean, 1),
          baseline_qol_lower = round(baseline_qol_lower, 1),
          baseline_qol_upper = round(baseline_qol_upper, 1)
        ) %>%
        select(id, baseline_qol_mean, baseline_qol_lower, baseline_qol_upper)
      
      df_out[[counter]] <- df_assigned
      counter <- counter + 1
    }
    
    message("Assigned baseline QoL for age band: ", age_b, ". ", n_sim, " MC simulations")
  }
  
  df_assignments <- bind_rows(df_out)
  
  # Merge assigned QoL back into main dataset
  df_final <- df %>%
    left_join(df_assignments, by = "id")
  
  return(df_final)
}



### 11.3.6 Function to calculate scores over years #### 
calculate_qol <- function(complete_pop_yr_fu,
                          cutpoints_yr,
                          start_year,
                          years = c(1,2,3,4,5),
                          target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                          fu_type = c("min", "max"),
                          post_op_imaging = c("none", "ct", "us", "xr_us"),
                          imaging_fu_type = c("ct", "us", "xr_us"),
                          xr_sens = 0.67,
                          xr_spec = 0.98,
                          us_sens = 0.54,
                          us_spec = 0.91,
                          use_cache = TRUE,
                          cache_dir = "usiqol_cache") {
  
  results_list <- list()
  post_op_imaging <- post_op_imaging[1]
  if(post_op_imaging == "none") post_op_imaging <- 0
  
  # Create cache directory if it doesn't exist
  if (use_cache && !dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
    message("Created cache directory: ", cache_dir)
  }
  
  # Cache static data that doesn't change across loop iterations
  cached_data <- list(
    complete_pop_yr_fu = complete_pop_yr_fu,
    cutpoints_yr = cutpoints_yr,
    start_year = start_year,
    fu_type = fu_type,
    imaging_fu_type = imaging_fu_type,
    post_op_imaging = post_op_imaging,
    xr_sens = xr_sens,
    xr_spec = xr_spec,
    us_sens = us_sens,
    us_spec = us_spec
  )
  
  # Pre-calculate less4_prob once (doesn't depend on target_auc)
  less4_prob <- complete_pop_yr_fu %>%
    filter(stone_free_status %in% c("less4", "more4")) %>%
    {
      if (nrow(.) == 0) {
        0.5
      } else {
        props <- group_by(., stone_free_status) %>%
          summarise(n = n(), .groups = "drop") %>%
          mutate(prop = n / sum(n))
        
        if ("less4" %in% props$stone_free_status) {
          props$prop[props$stone_free_status == "less4"]
        } else {
          0.5
        }
      }
    }
  
  cached_data$less4_prob <- less4_prob
  
  for (target_auc in target_aucs) {
    # Create unique cache filename based on all parameters that affect results
    cache_filename <- sprintf(
      "qol_yr%s_auc%.2f_fu%s_img%s_postop%s.rds",
      start_year,
      target_auc,
      fu_type[1],
      imaging_fu_type[1],
      post_op_imaging
    )
    cache_filepath <- file.path(cache_dir, cache_filename)
    
    # Check if cached result exists
    if (use_cache && file.exists(cache_filepath)) {
      message("Loading cached result for AUC: ", target_auc, " from: ", cache_filepath)
      combined_result <- readRDS(cache_filepath)
      results_list[[paste0("auc_", target_auc)]] <- combined_result
      next  # Skip to next iteration
    }
    
    message("Calculating QoL for: ", start_year,
            ", AUC: ", target_auc,
            ", Follow-up Type: ", fu_type,
            ", Follow-up Imaging: ", imaging_fu_type,
            " and Post-Operative Imaging: ", post_op_imaging)
    
    target_auc <- as.numeric(target_auc)
    
    # Use cached data
    complete_pop_yr_fu2 <- cached_data$complete_pop_yr_fu %>%
      filter(.data$auc_target == target_auc)
    
    # Cutpoint for this AUC
    cutpoint <- (cached_data$cutpoints_yr %>% 
                   filter(auc_target == !!target_auc))$cutpoint
    
    # Assign SF status according to imaging type
    rand_sens <- runif(nrow(complete_pop_yr_fu2))
    rand_spec <- runif(nrow(complete_pop_yr_fu2))
    
    # Distribute SF status as determined by imaging
    if (cached_data$imaging_fu_type == "us") {
      
      # Generate random numbers once
      rand_sens <- runif(nrow(cached_data$complete_pop_yr_fu))
      rand_spec <- runif(nrow(cached_data$complete_pop_yr_fu))
      
      complete_pop_yr_fu1 <- cached_data$complete_pop_yr_fu %>%
        mutate(
          stone_free_status_original = stone_free_status,
          stone_free_status1 = case_when(
            stone_free_status_original %in% c("less4", "more4") & rand_sens <= cached_data$xr_sens ~ stone_free_status_original,
            stone_free_status_original %in% c("less4", "more4") & rand_sens > cached_data$xr_sens ~ "SF", 
            stone_free_status_original == "SF" & rand_spec <= cached_data$xr_spec ~ "SF", 
            stone_free_status_original == "SF" & rand_spec > cached_data$xr_spec ~ ifelse(
              runif(n()) <= cached_data$less4_prob, "less4", "more4"
            ),
            TRUE ~ stone_free_status_original
          ),
          .keep = "all"
        )
    } else if (cached_data$imaging_fu_type == "xr_us") {
      rand_sens <- runif(nrow(cached_data$complete_pop_yr_fu))
      rand_spec <- runif(nrow(cached_data$complete_pop_yr_fu))
      
      complete_pop_yr_fu1 <- cached_data$complete_pop_yr_fu %>%
        mutate(
          stone_free_status_original = stone_free_status,
          stone_free_status1 = case_when(
            stone_free_status_original %in% c("less4", "more4") & lucency == "No" & rand_sens <= cached_data$xr_sens ~ stone_free_status_original,
            stone_free_status_original %in% c("less4", "more4") & lucency == "No" & rand_sens > cached_data$xr_sens ~ "SF", 
            stone_free_status_original %in% c("less4", "more4") & lucency == "Yes" & rand_sens <= cached_data$us_sens ~ stone_free_status_original,
            stone_free_status_original %in% c("less4", "more4") & lucency == "Yes" & rand_sens > cached_data$us_sens ~ "SF", 
            stone_free_status_original == "SF" & lucency == "No" & rand_spec <= cached_data$xr_spec ~ "SF", 
            stone_free_status_original == "SF" & lucency == "Yes" & rand_spec <= cached_data$us_spec ~ "SF", 
            stone_free_status_original == "SF" & lucency == "No" & rand_spec > cached_data$xr_spec ~ ifelse(
              runif(n()) <= cached_data$less4_prob, "less4", "more4"
            ),
            stone_free_status_original == "SF" & lucency == "Yes" & rand_spec > cached_data$us_spec ~ ifelse(
              runif(n()) <= cached_data$less4_prob, "less4", "more4"
            ),
            TRUE ~ stone_free_status_original
          ),
          .keep = "all"
        )
    } else {
      # For CT imaging, no sensitivity/specificity adjustment needed
      complete_pop_yr_fu1 <- cached_data$complete_pop_yr_fu %>%
        mutate(
          stone_free_status_original = stone_free_status,
          stone_free_status1 = stone_free_status,
          .keep = "all"
        )
    }
    
    # Precalculate first intervention year
    complete_pop_yr_fu1 <- complete_pop_yr_fu1 %>% 
      get_first_intervention_year(
        years = 1:5,
        prefix = "colic_intervention_type_year_"
      )
    
    # Assign QoL scores for all years
    combined_result <- assign_qol_chunked(
      data = complete_pop_yr_fu1,
      mc_reps = 100,
      chunk_size = 10,
      verbose = TRUE
    ) %>%
      as_tibble() %>%
      mutate(
        auc_target = target_auc,
        cutpoint = cutpoint,
        post_op_imaging = cached_data$post_op_imaging,
        imaging_fu_type = cached_data$imaging_fu_type,
        year = cached_data$start_year
      )
    
    # --- QALY calculation ---
    message("Calculating QALYs for AUC:", target_auc)
    combined_result <- combined_result %>%
      mutate(
        qaly_5yr = 
          (qol_mean_year_1 / 60) +
          ((qol_mean_year_2 / 60) * 1.035) +
          ((qol_mean_year_3 / 60) * 1.035 ^ 2) +
          ((qol_mean_year_4 / 60) * 1.035 ^ 3) +
          ((qol_mean_year_5 / 60) * 1.035 ^ 4),
        risk_status = case_when(prediction == "No" ~ "Low Risk",
                                prediction == "Yes" ~ "High Risk",
                                TRUE ~ NA_character_),
        .keep = "all"
      ) %>%
      select(
        id,
        sex,
        stone_free_status_original,
        stone_free_status1,
        auc_target,
        risk_status,
        true_rec_5yr,
        year,
        imaging_fu_type,
        year,
        baseline_qol_mean,
        qol_mean_year_1,
        qol_mean_year_2,
        qol_mean_year_3,
        qol_mean_year_4,
        qol_mean_year_5,
        qaly_5yr
      )
    
    # Save result to cache
    if (use_cache) {
      saveRDS(combined_result, cache_filepath)
      message("Saved result to cache: ", cache_filepath)
    }
    
    results_list[[paste0("auc_", target_auc)]] <- combined_result
  }
  
  message("Concatenating data for Year:", start_year, 
          " Follow-up Type: ", fu_type, 
          " Imaging Type: ", imaging_fu_type)
  
  if (length(target_aucs) == 1) {
    return(results_list[[1]])
  } else {
    return(results_list)
  }
}

# Helper function to clear cache
clear_qol_cache <- function(cache_dir = "qol_cache") {
  if (dir.exists(cache_dir)) {
    files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
    if (length(files) > 0) {
      file.remove(files)
      message("Cleared ", length(files), " cached files from: ", cache_dir)
    } else {
      message("No cache files found in: ", cache_dir)
    }
  } else {
    message("Cache directory does not exist: ", cache_dir)
  }
}


## 11.4 Run QoL function ####
### 11.4.1 2016 ####
complete_pop_2016_fu_baseline_qol <- assign_baseline_qol(df = complete_pop_2016_fu)

qol_2016_us_min <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2016_fu_baseline_qol,
  cutpoints_yr = cutpoints_2016,
  start_year = 2016,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  imaging_fu_type = "us"
)

qol_2016_us_max <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2016_fu_baseline_qol,
  cutpoints_yr = cutpoints_2016,
  start_year = 2016,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  imaging_fu_type = "us"
)

qol_2016_xr_us_min <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2016_fu_baseline_qol,
  cutpoints_yr = cutpoints_2016,
  start_year = 2016,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  imaging_fu_type = "xr_us"
)

qol_2016_xr_us_max <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2016_fu_baseline_qol,
  cutpoints_yr = cutpoints_2016,
  start_year = 2016,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  imaging_fu_type = "xr_us"
)

qol_2016_ct_min <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2016_fu_baseline_qol,
  cutpoints_yr = cutpoints_2016,
  start_year = 2016,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  imaging_fu_type = "ct"
)

qol_2016_ct_max <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2016_fu_baseline_qol,
  cutpoints_yr = cutpoints_2016,
  start_year = 2016,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  imaging_fu_type = "ct"
)

### 11.4.2 2017 ####
complete_pop_2017_fu_baseline_qol <- assign_baseline_qol(df = complete_pop_2017_fu)

qol_2017_us_min <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2017_fu_baseline_qol,
  cutpoints_yr = cutpoints_2017,
  start_year = 2017,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  imaging_fu_type = "us"
)

qol_2017_us_max <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2017_fu_baseline_qol,
  cutpoints_yr = cutpoints_2017,
  start_year = 2017,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  imaging_fu_type = "us"
)

qol_2017_xr_us_min <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2017_fu_baseline_qol,
  cutpoints_yr = cutpoints_2017,
  start_year = 2017,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  imaging_fu_type = "xr_us"
)

qol_2017_xr_us_max <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2017_fu_baseline_qol,
  cutpoints_yr = cutpoints_2017,
  start_year = 2017,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  imaging_fu_type = "xr_us"
)

qol_2017_ct_min <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2017_fu_baseline_qol,
  cutpoints_yr = cutpoints_2017,
  start_year = 2017,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  imaging_fu_type = "ct"
)

qol_2017_ct_max <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2017_fu_baseline_qol,
  cutpoints_yr = cutpoints_2017,
  start_year = 2017,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  imaging_fu_type = "ct"
)

### 11.4.3 2018 ####
complete_pop_2018_fu_baseline_qol <- assign_baseline_qol(df = complete_pop_2018_fu)

qol_2018_us_min <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2018_fu_baseline_qol,
  cutpoints_yr = cutpoints_2018,
  start_year = 2018,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  imaging_fu_type = "us"
)

qol_2018_us_max <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2018_fu_baseline_qol,
  cutpoints_yr = cutpoints_2018,
  start_year = 2018,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  imaging_fu_type = "us"
)

qol_2018_xr_us_min <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2018_fu_baseline_qol,
  cutpoints_yr = cutpoints_2018,
  start_year = 2018,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  imaging_fu_type = "xr_us"
)

qol_2018_xr_us_max <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2018_fu_baseline_qol,
  cutpoints_yr = cutpoints_2018,
  start_year = 2018,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  imaging_fu_type = "xr_us"
)

qol_2018_ct_min <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2018_fu_baseline_qol,
  cutpoints_yr = cutpoints_2018,
  start_year = 2018,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  imaging_fu_type = "ct"
)

qol_2018_ct_max <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2018_fu_baseline_qol,
  cutpoints_yr = cutpoints_2018,
  start_year = 2018,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  imaging_fu_type = "ct"
)

### 11.4.4 2019 ####
complete_pop_2019_fu_baseline_qol <- assign_baseline_qol(df = complete_pop_2019_fu)


qol_2019_us_min <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2019_fu_baseline_qol,
  cutpoints_yr = cutpoints_2019,
  start_year = 2019,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  imaging_fu_type = "us"
)

qol_2019_us_max <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2019_fu_baseline_qol,
  cutpoints_yr = cutpoints_2019,
  start_year = 2019,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  imaging_fu_type = "us"
)

qol_2019_xr_us_min <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2019_fu_baseline_qol,
  cutpoints_yr = cutpoints_2019,
  start_year = 2019,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  imaging_fu_type = "xr_us"
)

qol_2019_xr_us_max <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2019_fu_baseline_qol,
  cutpoints_yr = cutpoints_2019,
  start_year = 2019,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  imaging_fu_type = "xr_us"
)

qol_2019_ct_min <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2019_fu_baseline_qol,
  cutpoints_yr = cutpoints_2019,
  start_year = 2019,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  imaging_fu_type = "ct"
)

qol_2019_ct_max <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2019_fu_baseline_qol,
  cutpoints_yr = cutpoints_2019,
  start_year = 2019,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  imaging_fu_type = "ct"
)

### 11.4.5 2020 ####
complete_pop_2020_fu_baseline_qol <- assign_baseline_qol(df = complete_pop_2020_fu)

qol_2020_us_min <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2020_fu_baseline_qol,
  cutpoints_yr = cutpoints_2020,
  start_year = 2020,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  imaging_fu_type = "us"
)

qol_2020_us_max <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2020_fu_baseline_qol,
  cutpoints_yr = cutpoints_2020,
  start_year = 2020,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  imaging_fu_type = "us"
)

qol_2020_xr_us_min <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2020_fu_baseline_qol,
  cutpoints_yr = cutpoints_2020,
  start_year = 2020,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  imaging_fu_type = "xr_us"
)

qol_2020_xr_us_max <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2020_fu_baseline_qol,
  cutpoints_yr = cutpoints_2020,
  start_year = 2020,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  imaging_fu_type = "xr_us"
)

qol_2020_ct_min <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2020_fu_baseline_qol,
  cutpoints_yr = cutpoints_2020,
  start_year = 2020,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  imaging_fu_type = "ct"
)

qol_2020_ct_max <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2020_fu_baseline_qol,
  cutpoints_yr = cutpoints_2020,
  start_year = 2020,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  imaging_fu_type = "ct"
)

## 11.5 Amalgamate Each Year and Plot each AUC ####
### 11.5.1 Aggregation function ####
aggregate_qol_cohorts <- function(auc_target = c(1,2,3,4,5,6,7,8,9)) {
  all_cohorts <- list()
  
  for (i in auc_target) {
    key <- as.integer(i)
    message("Processing AUC = ", key)
    
    message("  Loading 2016 data...")
    cohort_2016_min_us <- qol_2016_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, US", auc = i)
    cohort_2016_min_ct <- qol_2016_ct_min[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2016_max_us <- qol_2016_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, US", auc = i)
    cohort_2016_max_ct <- qol_2016_ct_max[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    cohort_2016_min_xr_us <- qol_2016_xr_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2016_max_xr_us <- qol_2016_xr_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    
    message("  Loading 2017 data...")
    cohort_2017_min_us <- qol_2017_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, US", auc = i)
    cohort_2017_min_ct <- qol_2017_ct_min[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2017_max_us <- qol_2017_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, US", auc = i)
    cohort_2017_max_ct <- qol_2017_ct_max[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    cohort_2017_min_xr_us <- qol_2017_xr_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2017_max_xr_us <- qol_2017_xr_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    
    message("  Loading 2018 data...")
    cohort_2018_min_us <- qol_2018_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, US", auc = i)
    cohort_2018_min_ct <- qol_2018_ct_min[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2018_max_us <- qol_2018_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, US", auc = i)
    cohort_2018_max_ct <- qol_2018_ct_max[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    cohort_2018_min_xr_us <- qol_2018_xr_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2018_max_xr_us <- qol_2018_xr_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    
    message("  Loading 2019 data...")
    cohort_2019_min_us <- qol_2019_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, US", auc = i)
    cohort_2019_min_ct <- qol_2019_ct_min[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2019_max_us <- qol_2019_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, US", auc = i)
    cohort_2019_max_ct <- qol_2019_ct_max[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    cohort_2019_min_xr_us <- qol_2019_xr_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2019_max_xr_us <- qol_2019_xr_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    
    message("  Loading 2020 data...")
    cohort_2020_min_us <- qol_2020_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, US", auc = i)
    cohort_2020_min_ct <- qol_2020_ct_min[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2020_max_us <- qol_2020_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, US", auc = i)
    cohort_2020_max_ct <- qol_2020_ct_max[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    cohort_2020_min_xr_us <- qol_2020_xr_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2020_max_xr_us <- qol_2020_xr_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    
    message("  Combining cohorts for AUC = ", key)
    overall_cohort <- dplyr::bind_rows(
      cohort_2016_min_us, cohort_2016_min_ct, cohort_2016_max_us, cohort_2016_max_ct, cohort_2016_min_xr_us, cohort_2016_max_xr_us,
      cohort_2017_min_us, cohort_2017_min_ct, cohort_2017_max_us, cohort_2017_max_ct, cohort_2017_min_xr_us, cohort_2017_max_xr_us,
      cohort_2018_min_us, cohort_2018_min_ct, cohort_2018_max_us, cohort_2018_max_ct, cohort_2018_min_xr_us, cohort_2018_max_xr_us,
      cohort_2019_min_us, cohort_2019_min_ct, cohort_2019_max_us, cohort_2019_max_ct, cohort_2019_min_xr_us, cohort_2019_max_xr_us,
      cohort_2020_min_us, cohort_2020_min_ct, cohort_2020_max_us, cohort_2020_max_ct, cohort_2020_min_xr_us, cohort_2020_max_xr_us
    )
    
    all_cohorts[[key]] <- overall_cohort
    message("✅ Done with AUC = ", key, "\n")
  }
  
  message("🔗 Binding all AUC cohorts together...")
  final_df <- dplyr::bind_rows(all_cohorts)
  message("✅ All done.")
  
  return(final_df)
}

### 11.5.2 AUC 0.55 ####
qol_auc_0.55 <- aggregate_qol_cohorts(auc_target = 1)

### 11.5.3 AUC 0.6 ####
qol_auc_0.6 <- aggregate_qol_cohorts(auc_target = 2)

### 11.5.4 AUC 0.65 ####
qol_auc_0.65 <- aggregate_qol_cohorts(auc_target = 3)

### 11.5.5 AUC 0.7 ####
qol_auc_0.7 <- aggregate_qol_cohorts(auc_target = 4)

### 11.5.6 AUC 0.75 ####
qol_auc_0.75 <- aggregate_qol_cohorts(auc_target = 5)

### 11.5.7 AUC 0.8 ####
qol_auc_0.8 <- aggregate_qol_cohorts(auc_target = 6)

### 11.5.8 AUC 0.85 ####
qol_auc_0.85 <- aggregate_qol_cohorts(auc_target = 7)

### 11.5.9 AUC 0.9 ####
qol_auc_0.9 <- aggregate_qol_cohorts(auc_target = 8)

### 11.5.10 AUC 0.95 ####
qol_auc_0.95 <- aggregate_qol_cohorts(auc_target = 9)



# Memory-efficient combination function - only select needed columns upfront
combine_auc_data <- function(data, auc_label) {
  data %>%
    select(cohort_type, risk_status, qaly_5yr, true_rec_5yr, stone_free_status_original) %>%
    drop_na(qaly_5yr) %>%
    mutate(
      auc_label = factor(auc_label),
      cohort_type = factor(cohort_type),
      risk_status = factor(case_when(
        risk_status == "LR" ~ "Low Risk",
        risk_status == "HR" ~ "High Risk",
        TRUE ~ risk_status
      ), levels = c("All", "Low Risk", "High Risk")),
      stone_free_status = stone_free_status_original,
      true_rec_5yr = factor(true_rec_5yr)
    ) %>%
    select(-stone_free_status_original)
}

# Combine data efficiently
auc_labels <- c("AUC 0.55", "AUC 0.6", "AUC 0.65", "AUC 0.7", "AUC 0.75", 
                "AUC 0.8", "AUC 0.85", "AUC 0.9", "AUC 0.95")

auc_list <- list(qol_auc_0.55, qol_auc_0.6, qol_auc_0.65, qol_auc_0.7, 
                 qol_auc_0.75, qol_auc_0.8, qol_auc_0.85, qol_auc_0.9, qol_auc_0.95)

# Process and create summary directly - no need to keep full combined dataset
summary_df_qol <- map2_dfr(auc_list, auc_labels, function(data, label) {
  processed <- combine_auc_data(data, label)
  
  # Calculate summaries immediately
  by_risk <- processed %>%
    group_by(auc_label, risk_status, cohort_type) %>%
    summarise(
      mean_qaly_5yr = mean(qaly_5yr, na.rm = TRUE),
      lower_qaly_5yr = quantile(qaly_5yr, 0.025, na.rm = TRUE),
      upper_qaly_5yr = quantile(qaly_5yr, 0.975, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Calculate "All" summaries
  all_risk <- processed %>%
    group_by(auc_label, cohort_type) %>%
    summarise(
      mean_qaly_5yr = mean(qaly_5yr, na.rm = TRUE),
      lower_qaly_5yr = quantile(qaly_5yr, 0.025, na.rm = TRUE),
      upper_qaly_5yr = quantile(qaly_5yr, 0.975, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(risk_status = factor("All", levels = c("All", "Low Risk", "High Risk")))
  
  # Free memory immediately
  rm(processed)
  
  bind_rows(by_risk, all_risk)
}, .progress = TRUE)

# Clear original data objects if no longer needed
rm(auc_list)
gc()

# Finalize summary data for plotting
summary_df_full_qol_data <- summary_df_qol %>%
  mutate(
    cohort_type = factor(cohort_type, levels = c(
      "Minimum FU, XR + US", "Minimum FU, US", "Minimum FU, CT", 
      "Maximum FU, XR + US", "Maximum FU, US", "Maximum FU, CT"
    ))
  )

# Plot
summary_df_full_qol_data %>% 
  ggplot(aes(x = auc_label, y = mean_qaly_5yr, fill = risk_status)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "black") +
  geom_errorbar(
    aes(ymin = lower_qaly_5yr, ymax = upper_qaly_5yr),
    position = position_dodge(width = 0.8),
    width = 0.3
  ) +
  facet_wrap(~ cohort_type) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 10)
  ) +
  labs(
    title = "USIQoL Adjusted Life Years for EAU Follow-Up of those with Clinically Significant Disease",
    x = "Cohort Type",
    y = "Mean QALYs over 5yrs follow-up",
    fill = "Risk Status"
  ) + 
  ylim(0, 3.2)
