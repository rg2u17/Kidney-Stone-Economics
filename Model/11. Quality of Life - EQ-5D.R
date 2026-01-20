# 11. Quality of Life with EQ-5D Profile Adjustments ####
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
library(eq5d)
library(matrixStats)
library(future)
library(furrr)
library(progressr)

# The commented code has been included for transparency as to how 
# the EQ-5D data has been summarised - summarised datasets have been 
# saved for replication

## 11.2 Load data ####
### 11.2.1 Read in data ####
#usiqol_scores <- fread("~/Desktop/Sayer/Economics/Markov modelling KSD/usiqol_scores.csv",
#                       header = TRUE) %>%
#  janitor::clean_names() %>%
#  as_tibble() %>%
#  select(date_of_birth, age, trial_number)

#eq_5d_metrics_1 <- fread("~/Desktop/Sayer/Economics/Markov modelling KSD/raw_eq_5d_5l_data.csv",
#                         header = TRUE) %>%
#  janitor::clean_names() %>%
#  as_tibble() %>%
#  subset(
#    select = c(
#      trial_number,
#      date_completed,
#      mobility,
#      self_care,
#      usual_activities,
#      pain_discomfort,
#      anxiety_depression,
#      compared_with_general_activity,
#      date_completed_2,
#      mobility_2,
#      self_care_2,
#      usual_activities_2,
#      pain_discomfort_2,
#      anxiety_depression_2,
#      compared_with_general_activity_2,
#      date_completed_3,
#      mobility_3,
#      self_care_3,
#      usual_activities_3,
#      pain_discomfort_3,
#      anxiety_depression_3,
#      compared_with_general_activity_3
#    )
#  ) %>%
#  drop_na(trial_number)

#eq_5d_metrics_1 <- eq_5d_metrics_1 %>%
#  mutate(
#    score_1 = paste0(
#      mobility,
#      self_care,
#      usual_activities,
#      pain_discomfort,
#      anxiety_depression
#    ),
#    score_2 = paste0(
#      mobility_2,
#      self_care_2,
#      usual_activities_2,
#      pain_discomfort_2,
#      anxiety_depression_2
#    ),
#    score_3 = paste0(
#      mobility_3,
#      self_care_3,
#      usual_activities_3,
#      pain_discomfort_3,
#      anxiety_depression_3
#    ),
#    .keep = "all"
#  ) 

#eq_5d_metrics <- usiqol_scores %>%
#  left_join(
#    eq_5d_metrics_1, by = "trial_number"
#  ) %>%
#  mutate(
#    total_pre = eq5d(score_1, country = "UK", version = "5L", type = "CW", ignore.invalid = TRUE),
#    total_post_1 = eq5d(score_2, country = "UK", version = "5L", type = "CW", ignore.invalid = TRUE),
#    total_post_2 = eq5d(score_3, country = "UK", version = "5L", type = "CW", ignore.invalid = TRUE),
#    .keep = "all"
#  )

#eq_5d_stone_sizes_pre_post <- fread("~/Desktop/Sayer/Economics/Markov modelling KSD/stone_free_statuses_usiqol_2.csv", header = TRUE) %>% 
#  janitor::clean_names() %>%
#  as_tibble()

### 11.2.3 Define data ####

#eq_5d_metrics$date_of_birth <- as.POSIXct(eq_5d_metrics$date_of_birth, format = "%d/%m/%Y")
#eq_5d_metrics$age <- as.integer(eq_5d_metrics$age)
#eq_5d_metrics$trial_number <- as.integer(eq_5d_metrics$trial_number)
#eq_5d_metrics$date_completed <- as.POSIXct(eq_5d_metrics$date_completed, format = "%d/%m/%Y")
#eq_5d_metrics$total_pre <- as.numeric(eq_5d_metrics$total_pre)
#eq_5d_metrics$date_completed_2 <- as.POSIXct(eq_5d_metrics$date_completed_2, format = "%d/%m/%Y")
#eq_5d_metrics$total_post_1 <- as.numeric(eq_5d_metrics$total_post_1)
#eq_5d_metrics$date_completed_3 <- as.POSIXct(eq_5d_metrics$date_completed_3, format = "%d/%m/%Y")
#eq_5d_metrics$total_post_2 <- as.numeric(eq_5d_metrics$total_post_2)

#eq_5d_stone_sizes_pre_post$sex <- as.factor(eq_5d_stone_sizes_pre_post$sex)
#eq_5d_stone_sizes_pre_post$intervention <- as.factor(eq_5d_stone_sizes_pre_post$intervention)
#eq_5d_stone_sizes_pre_post$successful <- as.factor(eq_5d_stone_sizes_pre_post$successful)
#eq_5d_stone_sizes_pre_post$pre_op_stone_free_status <- as.integer(eq_5d_stone_sizes_pre_post$pre_op_stone_free_status)
#eq_5d_stone_sizes_pre_post$post_op_stone_free_status <- as.integer(eq_5d_stone_sizes_pre_post$post_op_stone_free_status)

### 11.2.4 Assign age bands ####

#eq_5d_metrics <- eq_5d_metrics %>%
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

#eq_5d_stone_sizes_pre_post <- eq_5d_stone_sizes_pre_post %>%
#  subset(select = c(
#    trial_number,
#    pre_op_stone_free_status,
#    post_op_stone_free_status,
#    intervention,
#    successful
#  )) %>% 
#  drop_na(trial_number)

### 11.2.5 Assign stone free status ####

#eq_5d_metrics_aggregated <- eq_5d_metrics %>%
#  left_join(eq_5d_stone_sizes_pre_post,
#            by = c("trial_number")) %>%
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

# Calculate mean changes in each EQ-5D dimension by intervention
#eq_5d_dimension_changes <- setDT(eq_5d_metrics_aggregated)[
#  !is.na(mobility) & !is.na(mobility_2)
#][, `:=`(
#  mobility_change = mobility_2 - mobility,
#  selfcare_change = self_care_2 - self_care,
#  usual_act_change = usual_activities_2 - usual_activities,
#  pain_change = pain_discomfort_2 - pain_discomfort,
#  anxiety_change = anxiety_depression_2 - anxiety_depression
#)][, .(
#  n = .N,
#  mobility_mean_change = mean(mobility_change, na.rm = TRUE),
#  mobility_sd_change = sd(mobility_change, na.rm = TRUE),
#  selfcare_mean_change = mean(selfcare_change, na.rm = TRUE),
#  selfcare_sd_change = sd(selfcare_change, na.rm = TRUE),
#  usual_act_mean_change = mean(usual_act_change, na.rm = TRUE),
#  usual_act_sd_change = sd(usual_act_change, na.rm = TRUE),
#  pain_mean_change = mean(pain_change, na.rm = TRUE),
#  pain_sd_change = sd(pain_change, na.rm = TRUE),
#  anxiety_mean_change = mean(anxiety_change, na.rm = TRUE),
#  anxiety_sd_change = sd(anxiety_change, na.rm = TRUE)
#), by = .(intervention, stone_free_status_pre)]

#write.csv(eq_5d_dimension_changes,
#          "eq_5d_dimension_changes.csv")

eq_5d_dimension_changes <- fread("Inputs/eq_5d_dimension_changes.csv") %>% 
  data.table::data.table() %>%
  select(-V1) %>%
  mutate(
    intervention = as.factor(intervention),
    stone_free_status_pre = as.factor(stone_free_status_pre)
  )

# Also calculate overall utility changes for comparison
#eq_5d_change_with_rx <- eq_5d_metrics_aggregated %>%
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

#write.csv(eq_5d_change_with_rx,
#          "eq_5d_change_with_rx.csv")

eq_5d_change_with_rx <- fread("Inputs/eq_5d_change_with_rx.csv") %>%
  as_tibble() %>%
  select(-V1) %>%
  mutate(
    intervention = as.factor(intervention),
    stone_free_status_pre = as.factor(stone_free_status_pre)
  )


mean_change_se_1 <- eq_5d_change_with_rx %>% 
  select(qol_se) %>%
  drop_na(qol_se) %>%
  subset(qol_se < 4) %>%
  summarise(
    qol_se = mean(qol_se)
  )

mean_change_se <- mean_change_se_1$qol_se  

mean_change_sd_1 <- eq_5d_change_with_rx %>% 
  select(qol_sd) %>%
  drop_na(qol_sd) %>%
  subset(qol_sd < 13) %>%
  summarise(
    qol_sd = mean(qol_sd)
  )

mean_change_sd <- mean_change_sd_1$qol_sd 

# Complete dataset for dimension changes
eq_5d_dimension_changes <- as_tibble(eq_5d_dimension_changes) %>%
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
      mobility_mean_change = 0,
      mobility_sd_change = 0.5,
      selfcare_mean_change = 0,
      selfcare_sd_change = 0.3,
      usual_act_mean_change = 0,
      usual_act_sd_change = 0.5,
      pain_mean_change = -0.5,
      pain_sd_change = 0.7,
      anxiety_mean_change = -0.3,
      anxiety_sd_change = 0.6
    )
  )

# Complete utility change dataset
eq_5d_change_with_rx <- eq_5d_change_with_rx %>%
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
      is.na(qol_change) & stone_free_status_pre == "SF" ~ 0.02,
      is.na(qol_change) & stone_free_status_pre == "less4" ~ 0.05,
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

### 11.2.7 Get baseline EQ-5D profiles by age and stone-free status ####

# Calculate mean EQ-5D dimension scores - dplyr replaced with data.table for computational speed
#eq_5d_profiles_by_age_stone_free <- setDT(eq_5d_metrics_aggregated)[, .(
#  age_bin,
#  mobility, self_care, usual_activities, pain_discomfort, anxiety_depression,
#  mobility_2, self_care_2, usual_activities_2, pain_discomfort_2, anxiety_depression_2,
#  stone_free_status_pre, stone_free_status_post,
#  total_pre, total_post_1
#)]

# Process both timepoints
#eq_5d_profiles_pre <- eq_5d_profiles_by_age_stone_free[, .(
#  age_bin,
#  mobility = mobility,
#  self_care = self_care,
#  usual_activities = usual_activities,
#  pain_discomfort = pain_discomfort,
#  anxiety_depression = anxiety_depression,
#  stone_free_status = stone_free_status_pre,
#  utility = total_pre
#)]

#eq_5d_profiles_post <- eq_5d_profiles_by_age_stone_free[, .(
#  age_bin,
#  mobility = mobility_2,
#  self_care = self_care_2,
#  usual_activities = usual_activities_2,
#  pain_discomfort = pain_discomfort_2,
#  anxiety_depression = anxiety_depression_2,
#  stone_free_status = stone_free_status_post,
#  utility = total_post_1
#)]

#eq_5d_profiles_by_age_stone_free <- rbindlist(list(eq_5d_profiles_pre, eq_5d_profiles_post))[
#  !is.na(mobility) & !is.na(stone_free_status)
#][, .(
#  n = .N,
#  mobility_mean = mean(mobility, na.rm = TRUE),
#  mobility_sd = sd(mobility, na.rm = TRUE),
#  selfcare_mean = mean(self_care, na.rm = TRUE),
#  selfcare_sd = sd(self_care, na.rm = TRUE),
#  usual_act_mean = mean(usual_activities, na.rm = TRUE),
#  usual_act_sd = sd(usual_activities, na.rm = TRUE),
#  pain_mean = mean(pain_discomfort, na.rm = TRUE),
#  pain_sd = sd(pain_discomfort, na.rm = TRUE),
#  anxiety_mean = mean(anxiety_depression, na.rm = TRUE),
#  anxiety_sd = sd(anxiety_depression, na.rm = TRUE),
#  utility_mean = mean(utility, na.rm = TRUE),
#  utility_sd = sd(utility, na.rm = TRUE)
#), by = .(age_bin, stone_free_status)]

# Also keep the summary utility stats for compatibility
#eq_5d_metrics_by_age_stone_free_status <- eq_5d_metrics_aggregated %>%
#  subset(select = c(age_bin,
#                    total_pre,
#                    total_post_1,
#                    stone_free_status_pre,
#                    stone_free_status_post
#  )) %>% 
#  pivot_longer(cols = c(total_pre,total_post_1),
#               names_to = "scored_when",
#               values_to = "eq_5d_scores") %>%
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
#    n = sum(!is.na(eq_5d_scores)),
#    total_mean = mean(eq_5d_scores, na.rm = TRUE),
#    total_sd   = sd(eq_5d_scores, na.rm = TRUE),
#    total_se = total_sd / sqrt(n),
#    total_ci_lower = total_mean - 1.96 * total_se,
#    total_ci_upper = total_mean + 1.96 * total_se
#  ) %>% 
#  select(-n) %>%
#  ungroup()

### 11.2.8 Deal with missing profile data ####

# Get mean dimension scores by stone-free status
#dimension_means_by_sf <- eq_5d_profiles_by_age_stone_free %>%
#  as_tibble() %>%
#  group_by(stone_free_status) %>%
#  summarise(across(ends_with("_mean"), ~mean(.x, na.rm = TRUE)))

# Complete missing combinations with reasonable defaults
#eq_5d_profiles_by_age_stone_free <- eq_5d_profiles_by_age_stone_free %>%
#  as_tibble() %>%
#  complete(age_bin, stone_free_status = c("more4", "less4", "SF")) %>%
#  filter(age_bin != "Aged 1 to 4") %>%
#  left_join(dimension_means_by_sf, by = "stone_free_status", suffix = c("", "_default")) %>%
#  mutate(
#    across(c(mobility_mean, selfcare_mean, usual_act_mean, pain_mean, anxiety_mean),
#           ~coalesce(.x, get(paste0(cur_column(), "_default")))),
#    across(c(mobility_sd, selfcare_sd, usual_act_sd, pain_sd, anxiety_sd),
#           ~coalesce(.x, 0.7))
#  ) %>%
#  select(-ends_with("_default"))

#write.csv(eq_5d_profiles_by_age_stone_free,
#          "eq_5d_profiles_by_age_stone_free.csv")

eq_5d_profiles_by_age_stone_free <- fread("Inputs/eq_5d_profiles_by_age_stone_free.csv") %>%
  as_tibble() %>%
  select(-V1) %>%
  mutate(
    stone_free_status = as.factor(stone_free_status),
    age_bin = factor(age_bin, levels = c("Aged 1 to 4",
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
                                         "Aged 90 and over")),
    .keep = "all"
  )

## 11.3 Functions to assign QOL scores with EQ-5D profiles #### 
### 11.3.0 Pre-compute EQ-5D lookup table ####
create_eq5d_lookup <- function() {
  message("Creating EQ-5D lookup table (5^5 = 3,125 combinations)...")
  
  profiles <- expand.grid(
    mobility = 1:5,
    selfcare = 1:5,
    usual_act = 1:5,
    pain = 1:5,
    anxiety = 1:5
  )
  
  profiles$profile_str <- paste0(
    profiles$mobility,
    profiles$selfcare,
    profiles$usual_act,
    profiles$pain,
    profiles$anxiety
  )
  
  profiles$utility <- eq5d(
    profiles$profile_str,
    country = "UK",
    version = "5L",
    type = "CW",
    ignore.invalid = TRUE
  )
  
  lookup <- setNames(profiles$utility, profiles$profile_str)
  message("EQ-5D lookup table created")
  return(lookup)
}

# Create lookup table once at start
EQ5D_LOOKUP <- create_eq5d_lookup()

### 11.3.0b Fast utility lookup function ####
get_utility_fast <- function(mobility, selfcare, usual_act, pain, anxiety) {
  profile_str <- paste0(mobility, selfcare, usual_act, pain, anxiety)
  EQ5D_LOOKUP[profile_str]
}

### 11.3.1 Assign Age helper function -  ####
assign_age_helper <- function(data,
                              baseline = TRUE,
                              fu_years = NULL) {
  
  age_bins_levels <- c("Aged 1 to 4",
                       "Aged 5-9",
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
    data <- data %>%
      mutate(age_bin = get_age_bin(as.numeric(age_fu)))
  }
  
  return(data)
}

### 11.3.2 Get First intervention year helper function ####
get_first_intervention_year <- function(df,
                                        years = 1:5,
                                        prefix = "colic_intervention_type_year_") {
  cols <- paste0(prefix, years)
  
  with_progress({
    p <- progressor(steps = 1)
    
    # Extract intervention columns as matrix for vectorized operations
    intervention_matrix <- as.matrix(df[, cols])
    
    # Vectorized: find first non-NA, non-"No" intervention
    first_year <- apply(intervention_matrix, 1, function(row) {
      idx <- which(!is.na(row) & row != "No")
      if (length(idx) == 0) NA_integer_ else idx[1]
    })
    
    df$first_intervention_year <- first_year
    p()  # Mark complete
    
    return(df)
  })
}

### 11.3.3 Adjust anxiety dimension based on clinical factors ####
### 11.3.3 Adjust anxiety dimension - VECTORIZED VERSION ####
adjust_anxiety_dimension_vectorized <- function(anxiety_level, stone_free_status, prediction, 
                                                first_intervention_year, current_year,
                                                had_intervention) {
  
  # Initialize with base anxiety
  adjustment <- rep(0, length(anxiety_level))
  
  # Stone not free adjustment
  adjustment[!is.na(stone_free_status) & stone_free_status != "SF"] <- 
    adjustment[!is.na(stone_free_status) & stone_free_status != "SF"] + 1
  
  # Prediction adjustments
  adjustment[!is.na(prediction) & prediction == "Yes"] <- 
    adjustment[!is.na(prediction) & prediction == "Yes"] + 1
  adjustment[!is.na(prediction) & prediction == "No"] <- 
    adjustment[!is.na(prediction) & prediction == "No"] - 1
  
  # Intervention adjustments
  if (any(had_intervention, na.rm = TRUE)) {
    idx_sf_no <- had_intervention & !is.na(stone_free_status) & stone_free_status == "SF" & 
      !is.na(prediction) & prediction == "No"
    adjustment[idx_sf_no] <- adjustment[idx_sf_no] - 1
  }
  
  new_level <- anxiety_level + adjustment
  return(pmax(1, pmin(5, as.integer(new_level))))
}

### 11.3.4 Pre-assign baseline EQ-5D PROFILES per unique id -  #### 
assign_baseline_qol <- function(df,
                                baseline_qol_profiles = eq_5d_profiles_by_age_stone_free,
                                n_sim = 500,
                                parallel = TRUE,
                                n_cores = parallel::detectCores() - 1) {
  
  # Clean baseline_qol_profiles 
  baseline_qol_profiles <- baseline_qol_profiles %>%
    mutate(
      age_bin = gsub("Aged ", "", age_bin),
      age_bin = gsub("90 and over", ">90", age_bin),
      age_bin = gsub(" to ", "-", age_bin)
    ) %>%
    as.data.frame()  
  
  # Filter baseline cohort
  df1 <- df %>%
    filter(auc_target == 0.55) %>%
    mutate(age_band = as.character(age_band))
  
  # Create combinations grid
  combinations <- df1 %>%
    distinct(age_band, stone_free_status) %>%
    filter(!is.na(age_band), !is.na(stone_free_status))
  
  if (parallel) {
    plan(multisession, workers = n_cores)
    message("Using ", n_cores, " cores for parallel processing")
  }
  
  message("process_combination function starting")
  
  # Combination processing - now takes all required data as arguments
  process_combination <- function(age_b, sf_level, profiles_dt, df_subset, n_simulations) {
    
    profile_row <- profiles_dt %>%
      filter(age_bin == age_b, stone_free_status == sf_level)
    
    if (nrow(profile_row) == 0) return(NULL)
    
    df_sub <- df_subset %>%
      filter(age_band == age_b, stone_free_status == sf_level)
    
    n_subgroup <- nrow(df_sub)
    if (n_subgroup == 0) return(NULL)
    
    set.seed(1234 + as.integer(factor(paste0(age_b, sf_level))))
    
    # Pre-generate random numbers
    all_draws <- matrix(
      rnorm(n_subgroup * n_simulations * 5),
      nrow = n_subgroup,
      ncol = n_simulations * 5
    )
    
    # Vectorized dimension calculations
    calc_dim <- function(col_start, mean_val, sd_val) {
      round(pmin(pmax(
        all_draws[, col_start:(col_start + n_simulations - 1)] * sd_val + mean_val,
        1), 5))
    }
    
    mobility_draws <- calc_dim(1, profile_row$mobility_mean, profile_row$mobility_sd)
    selfcare_draws <- calc_dim(n_simulations + 1, profile_row$selfcare_mean, profile_row$selfcare_sd)
    usual_act_draws <- calc_dim(2*n_simulations + 1, profile_row$usual_act_mean, profile_row$usual_act_sd)
    pain_draws <- calc_dim(3*n_simulations + 1, profile_row$pain_mean, profile_row$pain_sd)
    anxiety_draws <- calc_dim(4*n_simulations + 1, profile_row$anxiety_mean, profile_row$anxiety_sd)
    
    # Vectorized utility calculation using lookup table
    utility_draws <- matrix(NA, nrow = n_subgroup, ncol = n_simulations)
    
    for (sim in 1:n_simulations) {
      utility_draws[, sim] <- get_utility_fast(
        mobility_draws[, sim],
        selfcare_draws[, sim],
        usual_act_draws[, sim],
        pain_draws[, sim],
        anxiety_draws[, sim]
      )
    }
    
    # Summarize
    df_sub %>%
      mutate(
        baseline_mobility = round(rowMeans(mobility_draws)),
        baseline_selfcare = round(rowMeans(selfcare_draws)),
        baseline_usual_act = round(rowMeans(usual_act_draws)),
        baseline_pain = round(rowMeans(pain_draws)),
        baseline_anxiety = round(rowMeans(anxiety_draws)),
        baseline_qol_mean = rowMeans(utility_draws),
        baseline_qol_lower = rowQuantiles(utility_draws, probs = 0.025),
        baseline_qol_upper = rowQuantiles(utility_draws, probs = 0.975)
      ) %>%
      select(id, starts_with("baseline_"))
  }
  
  # Process all combinations
  if (parallel) {
    df_assignments <- future_map2_dfr(
      combinations$age_band,
      combinations$stone_free_status,
      ~process_combination(.x, .y, baseline_qol_profiles, df1, n_sim),
      .progress = TRUE,
      .options = furrr_options(seed = TRUE, packages = c("dplyr", "matrixStats"))
    )
    plan(sequential)
  } else {
    df_assignments <- map2_dfr(
      combinations$age_band,
      combinations$stone_free_status,
      ~{
        result <- process_combination(.x, .y, baseline_qol_profiles, df1, n_sim)
        message("Assigned baseline EQ-5D for: ", .x, " - ", .y)
        result
      }
    )
  }
  
  # Merge back
  df_final <- df %>%
    left_join(df_assignments, by = "id") 
  
  message("✅ Baseline QoL assignment complete")

  # Once finalised - get first intervention year for whole dataset - saves computation later on
  message("Running get_first_intervention_year function for whole dataset")
  df_final <- df_final %>% 
    get_first_intervention_year(
      years = 1:5,
      prefix = "colic_intervention_type_year_"
    )
  
  return(df_final)
}

### 11.3.5 Assign QOL function with profile-based adjustments -  ####
assign_qol_chunked <- function(data,
                               baseline_qol_profiles = eq_5d_profiles_by_age_stone_free,
                               utility_changes = eq_5d_change_with_rx,  
                               year_cols = c(1,2,3,4,5),
                               mc_reps = 100,
                               ci_level = 0.95,
                               chunk_size = 10,  
                               verbose = TRUE) {
  
  alpha <- (1 - ci_level) / 2
  vcat <- function(...) if (verbose) message(...)
  
  data <- as_tibble(data)
  utility_changes <- as_tibble(utility_changes)  
  
  setDT(data)
  setDT(utility_changes)  
  
  n <- nrow(data)
  total_chunks <- ceiling(n / chunk_size)
  
  start_time <- Sys.time()
  results_list <- vector("list", total_chunks)
  
  for (chunk_idx in seq_len(total_chunks)) {
    
    idx_start <- (chunk_idx - 1) * chunk_size + 1
    idx_end <- min(chunk_idx * chunk_size, n)
    chunk <- data %>% slice(idx_start:idx_end)
    chunk_n <- nrow(chunk)
    
    if (verbose) {
      pct <- round(chunk_idx / total_chunks * 100)
      vcat("---- Chunk ", chunk_idx, "/", total_chunks, " (", pct, "%) ----")
    }
    
    # VECTORIZED age calculations
    for (y in 1:5) {
      chunk[[paste0("age_fu_year_", y)]] <- chunk$age + y
    }
    
    # Assign age bins
    chunk <- chunk %>%
      assign_age_helper(baseline = TRUE) %>%   
      rename(baseline_age_bin = age_bin)
    
    for (fu_year in 1:5) {
      temp_df <- chunk %>%
        mutate(age_fu = .data[[paste0("age_fu_year_", fu_year)]]) %>%
        assign_age_helper(baseline = FALSE, fu_years = fu_year)
      
      chunk[[paste0("age_bin_fu_year_", fu_year)]] <- temp_df$age_bin
    }
    
    # Process all follow-up years
    for (fu_year in 1:5) {
      
      prev_mobility <- if (fu_year == 1) "baseline_mobility" else paste0("mobility_year_", fu_year - 1)
      prev_selfcare <- if (fu_year == 1) "baseline_selfcare" else paste0("selfcare_year_", fu_year - 1)
      prev_usual_act <- if (fu_year == 1) "baseline_usual_act" else paste0("usual_act_year_", fu_year - 1)
      prev_pain <- if (fu_year == 1) "baseline_pain" else paste0("pain_year_", fu_year - 1)
      prev_anxiety <- if (fu_year == 1) "baseline_anxiety" else paste0("anxiety_year_", fu_year - 1)
      intervention_col <- paste0("colic_intervention_type_year_", fu_year)
      death_col <- paste0("death_year_", fu_year)
      
      # VECTORIZED death indicator
      chunk$.is_dead <- FALSE
      if (death_col %in% names(chunk)) {
        chunk$.is_dead <- chunk[[death_col]] %in% c(TRUE, 1, "Yes")
        chunk$.is_dead[is.na(chunk$.is_dead)] <- FALSE
      }
      
      # Join utility changes
      temp_changes <- utility_changes %>%
        select(intervention, stone_free_status_pre,
               qol_change, qol_sd) %>%
        rename(!!intervention_col := intervention,
               stone_free_status1 = stone_free_status_pre,
               utility_change = qol_change,
               utility_sd = qol_sd)
      
      chunk <- chunk %>%
        left_join(temp_changes, by = c(intervention_col, "stone_free_status1"))
      
      # VECTORIZED had_intervention
      chunk$had_intervention <- !chunk$.is_dead & 
        !is.na(chunk[[intervention_col]]) & 
        chunk[[intervention_col]] != "No" &
        !is.na(chunk$first_intervention_year) & 
        chunk$first_intervention_year <= fu_year
      
      # Calculate baseline utility for patients receiving intervention
      chunk$baseline_utility <- ifelse(
        chunk$had_intervention,
        get_utility_fast(
          chunk[[prev_mobility]],
          chunk[[prev_selfcare]],
          chunk[[prev_usual_act]],
          chunk[[prev_pain]],
          chunk[[prev_anxiety]]
        ),
        NA_real_
      )
      
      # Sample utility change from distribution
      chunk$sampled_utility_change <- ifelse(
        chunk$had_intervention & !is.na(chunk$utility_change),
        rnorm(chunk_n, chunk$utility_change, chunk$utility_sd),
        0
      )
      
      # Calculate target utility
      chunk$target_utility <- chunk$baseline_utility + chunk$sampled_utility_change
      
      # Split the utility change proportionally between mobility and pain
      # Assume 60% mobility, 40% pain based on typical EQ-5D sensitivity
      chunk$mobility_prop_change <- chunk$sampled_utility_change * 0.6
      chunk$pain_prop_change <- chunk$sampled_utility_change * 0.4
      
      # Convert proportional changes to approximate dimension changes
      # assumed that ~0.1 utility ≈ 1 dimension level for mobility/pain - so adjusting dimensions based on that assumption
      chunk$mobility_dim_change <- ifelse(
        chunk$had_intervention,
        round(chunk$mobility_prop_change / 0.1),
        0
      )
      
      chunk$pain_dim_change <- ifelse(
        chunk$had_intervention,
        round(chunk$pain_prop_change / 0.1),
        0
      )
      
      # Apply dimension changes
      # Mobility
      mob_base <- chunk[[prev_mobility]]
      chunk[[paste0("mobility_year_", fu_year)]] <- ifelse(
        chunk$.is_dead,
        NA_integer_,
        pmax(1, pmin(5, as.integer(mob_base + chunk$mobility_dim_change)))
      )
      
      # Pain
      pain_base <- chunk[[prev_pain]]
      chunk[[paste0("pain_year_", fu_year)]] <- ifelse(
        chunk$.is_dead,
        NA_integer_,
        pmax(1, pmin(5, as.integer(pain_base + chunk$pain_dim_change)))
      )
      
      # Self-care and usual activities (no change)
      chunk[[paste0("selfcare_year_", fu_year)]] <- ifelse(
        chunk$.is_dead,
        NA_integer_,
        as.integer(chunk[[prev_selfcare]])
      )
      
      chunk[[paste0("usual_act_year_", fu_year)]] <- ifelse(
        chunk$.is_dead,
        NA_integer_,
        as.integer(chunk[[prev_usual_act]])
      )
      
      # VECTORIZED anxiety adjustment
      chunk[[paste0("anxiety_year_", fu_year)]] <- ifelse(
        chunk$.is_dead,
        NA_integer_,
        adjust_anxiety_dimension_vectorized(
          chunk[[prev_anxiety]],
          chunk$stone_free_status,
          chunk$prediction,
          chunk$first_intervention_year,
          fu_year,
          chunk$had_intervention
        )
      )
      
      # VECTORIZED utility calculation
      chunk[[paste0("qol_mean_year_", fu_year)]] <- ifelse(
        chunk$.is_dead,
        0,
        get_utility_fast(
          chunk[[paste0("mobility_year_", fu_year)]],
          chunk[[paste0("selfcare_year_", fu_year)]],
          chunk[[paste0("usual_act_year_", fu_year)]],
          chunk[[paste0("pain_year_", fu_year)]],
          chunk[[paste0("anxiety_year_", fu_year)]]
        )
      )
      
      # SIMPLIFIED Monte Carlo for CI - only for alive patients
      alive_mask <- !chunk$.is_dead
      n_alive <- sum(alive_mask)
      
      # Initialize with zeros
      chunk[[paste0("qol_lower_year_", fu_year)]] <- 0
      chunk[[paste0("qol_upper_year_", fu_year)]] <- 0
      chunk[[paste0("qol_se_year_", fu_year)]] <- 0
      
      if (n_alive > 0 && mc_reps > 0) {
        # Use REDUCED mc_reps for speed (50 instead of 100)
        mc_reps_actual <- min(50, mc_reps)
        
        # Extract dimensions for alive patients
        dims_alive <- chunk[alive_mask, c(
          paste0("mobility_year_", fu_year),
          paste0("selfcare_year_", fu_year),
          paste0("usual_act_year_", fu_year),
          paste0("pain_year_", fu_year),
          paste0("anxiety_year_", fu_year)
        )]
        
        # Convert to numeric and handle NAs
        dims_alive <- lapply(dims_alive, function(x) {
          x <- as.numeric(x)
          x[is.na(x)] <- 3  
          return(x)
        })
        
        # Pre-allocate matrix
        mc_utils <- matrix(NA, nrow = n_alive, ncol = mc_reps_actual)
        
        # Vectorized Monte Carlo
        for (mc in 1:mc_reps_actual) {
          mob_sim <- pmax(1, pmin(5, round(rnorm(n_alive, dims_alive[[1]], 0.3))))
          sc_sim <- pmax(1, pmin(5, round(rnorm(n_alive, dims_alive[[2]], 0.2))))
          ua_sim <- pmax(1, pmin(5, round(rnorm(n_alive, dims_alive[[3]], 0.3))))
          pn_sim <- pmax(1, pmin(5, round(rnorm(n_alive, dims_alive[[4]], 0.4))))
          anx_sim <- pmax(1, pmin(5, round(rnorm(n_alive, dims_alive[[5]], 0.4))))
          
          mc_utils[, mc] <- get_utility_fast(mob_sim, sc_sim, ua_sim, pn_sim, anx_sim)
        }
        
        # Calculate CI for alive patients
        chunk[[paste0("qol_lower_year_", fu_year)]][alive_mask] <- rowQuantiles(mc_utils, probs = alpha)
        chunk[[paste0("qol_upper_year_", fu_year)]][alive_mask] <- rowQuantiles(mc_utils, probs = 1 - alpha)
        chunk[[paste0("qol_se_year_", fu_year)]][alive_mask] <- rowSds(mc_utils)
      }
      
      # Clean up temporary columns
      chunk <- chunk %>%
        select(-utility_change, -utility_sd, -had_intervention, -.is_dead,
               -baseline_utility, -sampled_utility_change, -target_utility,
               -mobility_prop_change, -pain_prop_change, 
               -mobility_dim_change, -pain_dim_change)
    }
    
    results_list[[chunk_idx]] <- chunk
    
    if (verbose) {
      elapsed <- difftime(Sys.time(), start_time, units = "secs")
      est_remaining <- elapsed / chunk_idx * (total_chunks - chunk_idx)
      vcat(" ✅ chunk ", chunk_idx, " done | ⏳ remaining: ",
           round(as.numeric(est_remaining), 1), " sec")
    }
    
    # Force garbage collection between chunks
    gc(verbose = FALSE)
  }
  
  bind_rows(results_list)
}
### 11.3.6 Function to calculate scores over years #### 
calculate_qol <- function(complete_pop_yr_fu,
                          cutpoints_yr,
                          start_year,
                          years = c(1,2,3,4,5),
                          target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                          fu_type = c("min", "max"),
                          post_op_imaging = c("none", "ct", "us", "us"),
                          imaging_fu_type = c("ct", "us", "xr_us"),
                          xr_sens = 0.67,
                          xr_spec = 0.98,
                          us_sens = 0.54,
                          us_spec = 0.91,
                          cache_dir = "qol_cache",
                          use_cache = TRUE,
                          force_recalc = FALSE) {
  
  # Ensure input is a data frame/tibble at the start
  if (!is.data.frame(complete_pop_yr_fu)) {
    stop("complete_pop_yr_fu must be a data frame or tibble")
  }
  
  # Create cache directory if it doesn't exist
  if (use_cache && !dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
    message("Created cache directory: ", cache_dir)
  }
  
  
  
  results_list <- list()
  post_op_imaging <- post_op_imaging[1]
  if(post_op_imaging == "none") post_op_imaging <- 0
  
  for (target_auc in target_aucs) {
    target_auc <- as.numeric(target_auc)
    
    # Generate cache key based on parameters
    cache_key <- generate_cache_key(
      start_year = start_year,
      target_auc = target_auc,
      fu_type = fu_type,
      imaging_fu_type = imaging_fu_type,
      post_op_imaging = post_op_imaging,
      xr_sens = xr_sens,
      xr_spec = xr_spec,
      us_sens = us_sens,
      us_spec = us_spec
    )
    
    cache_file <- file.path(cache_dir, paste0(cache_key, ".rds"))
    
    # Check if cached result exists
    if (use_cache && !force_recalc && file.exists(cache_file)) {
      message("Loading cached result for AUC: ", target_auc, " from ", cache_file)
      cached_result <- readRDS(cache_file)
      results_list[[paste0("auc_", target_auc)]] <- cached_result
      next  # Skip to next iteration
    }
    
    message("Calculating QoL for: ", start_year,
            ", AUC: ", target_auc,
            ", Follow-up Type: ", fu_type,
            ", Follow-up Imaging: ", imaging_fu_type,
            " and Post-Operative Imaging: ", post_op_imaging)

    
    # Filter
    complete_pop_yr_fu2 <- complete_pop_yr_fu %>%
      filter(auc_target == !!target_auc)
  
    # Check if filtering worked
    if (nrow(complete_pop_yr_fu2) == 0) {
      warning("No data found for AUC target: ", target_auc)
      next
    }
    
    # Get cutpoint
    cutpoint_df <- cutpoints_yr %>% filter(auc_target == !!target_auc)
    if (nrow(cutpoint_df) == 0) {
      stop("No cutpoint found for AUC target: ", target_auc)
    }
    cutpoint <- cutpoint_df$cutpoint[1]
    
    # Calculate not stone-free proportions 
    less4_prob <- complete_pop_yr_fu2 %>%
      filter(stone_free_status %in% c("less4", "more4")) %>%
      {
        if (nrow(.) == 0) {
          0.5  # default if no non-stone-free patients
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
    
    rand_sens <- runif(nrow(complete_pop_yr_fu2))
    rand_spec <- runif(nrow(complete_pop_yr_fu2))
    
    complete_pop_yr_fu2 <- complete_pop_yr_fu2 %>%
      cbind(
        rand_sens = rand_sens,
        rand_spec = rand_spec
      )
    
    message("Assigning SF status as per Imaging type: (", imaging_fu_type, ")")
    # Distribute SF status as determined by imaging
    if (imaging_fu_type == "us") {

      
      complete_pop_yr_fu1 <- complete_pop_yr_fu2 %>%
        mutate(
          stone_free_status_original = stone_free_status,
          stone_free_status1 = case_when(
            stone_free_status_original %in% c("less4", "more4") & rand_sens <= us_sens ~ stone_free_status_original,
            stone_free_status_original %in% c("less4", "more4") & rand_sens > us_sens ~ "SF", 
            stone_free_status_original == "SF" & rand_spec <= us_spec ~ "SF", 
            stone_free_status_original == "SF" & rand_spec > us_spec ~ ifelse(
              runif(n()) <= less4_prob, "less4", "more4"
            ),
            TRUE ~ NA_character_ 
          ),
          .keep = "all"
        )
    } else if (imaging_fu_type == "xr_us") {

        complete_pop_yr_fu1 <- complete_pop_yr_fu2 %>%
        mutate(
          stone_free_status_original = stone_free_status,
          stone_free_status1 = case_when(
            stone_free_status_original %in% c("less4", "more4") & lucency == "no" & rand_sens <= xr_sens ~ stone_free_status_original,
            stone_free_status_original %in% c("less4", "more4") & lucency == "no" & rand_sens > xr_sens ~ "SF", 
            stone_free_status_original %in% c("less4", "more4") & lucency == "yes" & rand_sens <= us_sens ~ stone_free_status_original,
            stone_free_status_original %in% c("less4", "more4") & lucency == "yes" & rand_sens > us_sens ~ "SF", 
            stone_free_status_original == "SF" & lucency == "no" & rand_spec <= xr_spec ~ "SF", 
            stone_free_status_original == "SF" & lucency == "yes" & rand_spec <= us_spec ~ "SF", 
            stone_free_status_original == "SF" & lucency == "no" & rand_spec > xr_spec ~ ifelse(
              runif(n()) <= less4_prob, "less4", "more4"
            ),
            stone_free_status_original == "SF" & lucency == "yes" & rand_spec > us_spec ~ ifelse(
              runif(n()) <= less4_prob, "less4", "more4"
            ),
            TRUE ~ NA_character_
          ),
          .keep = "all"
        )
    } else {
      # For CT imaging, no sensitivity/specificity adjustment needed
      complete_pop_yr_fu1 <- complete_pop_yr_fu2 %>%
        mutate(
          stone_free_status_original = stone_free_status,
          stone_free_status1 = stone_free_status,
          .keep = "all"
        )
    }
    
    
    message("Running assign_qol_chunked...")
    combined_result <- assign_qol_chunked(
      data = complete_pop_yr_fu1,
      mc_reps = 100,
      chunk_size = 2000,
      verbose = TRUE
    )
    
    # Ensure result is a tibble
    if (!is.data.frame(combined_result)) {
      combined_result <- as_tibble(combined_result)
    }
    
    combined_result <- combined_result %>%
      mutate(
        auc_target = !!target_auc,
        cutpoint = !!cutpoint,
        post_op_imaging = !!post_op_imaging,
        imaging_fu_type = !!imaging_fu_type,
        year = !!start_year
      )
    
    message("Calculating QALYs for AUC:", target_auc)
    combined_result <- combined_result %>%
      mutate(
        qaly_5yr = rowSums(select(., starts_with("qol_mean_year_")), na.rm = TRUE),
        risk_status = case_when(
          prediction == "No" ~ "Low Risk",
          prediction == "Yes" ~ "High Risk",
          TRUE ~ NA_character_
        )
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
        baseline_qol_mean,
        qol_mean_year_1,
        qol_mean_year_2,
        qol_mean_year_3,
        qol_mean_year_4,
        qol_mean_year_5,
        qaly_5yr
      ) %>% drop_na(qol_mean_year_5)
    
    # Save to cache
    if (use_cache) {
      message("Saving result to cache: ", cache_file)
      saveRDS(combined_result, cache_file)
    }
    
    results_list[[paste0("auc_", target_auc)]] <- combined_result
  }
  
  message("Concatenating data for Year:", start_year, " Follow-up Type: ", fu_type, " Imaging Type: ", imaging_fu_type)
  if (length(target_aucs) == 1) {
    return(results_list[[1]])
  } else {
    return(bind_rows(results_list))
  }
}


# Helper function to generate consistent cache keys
generate_cache_key <- function(start_year, target_auc, fu_type, imaging_fu_type, 
                               post_op_imaging, xr_sens, xr_spec, us_sens, us_spec) {
  # Create a hash of all parameters to ensure uniqueness
  params <- paste(
    start_year,
    target_auc,
    fu_type[1],
    imaging_fu_type[1],
    post_op_imaging,
    xr_sens,
    xr_spec,
    us_sens,
    us_spec,
    sep = "_"
  )
  
  # Clean up the string to make it filesystem-safe
  params <- gsub("[^[:alnum:]_.-]", "_", params)
  
  return(paste0("qol_", params))
}


# Helper function to clear cache
clear_qol_cache <- function(cache_dir = "qol_cache", 
                            pattern = NULL,
                            confirm = TRUE) {
  if (!dir.exists(cache_dir)) {
    message("Cache directory does not exist: ", cache_dir)
    return(invisible(NULL))
  }
  
  # Get files to delete
  if (is.null(pattern)) {
    files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
  } else {
    files <- list.files(cache_dir, pattern = pattern, full.names = TRUE)
  }
  
  if (length(files) == 0) {
    message("No cache files found to delete")
    return(invisible(NULL))
  }
  
  message("Found ", length(files), " cache file(s)")
  
  if (confirm) {
    response <- readline(prompt = "Delete these files? (yes/no): ")
    if (tolower(response) != "yes") {
      message("Cache clearing cancelled")
      return(invisible(NULL))
    }
  }
  
  deleted <- file.remove(files)
  message("Deleted ", sum(deleted), " cache file(s)")
  
  return(invisible(files[deleted]))
}


# Helper function to list cached results
list_cached_results <- function(cache_dir = "qol_cache") {
  if (!dir.exists(cache_dir)) {
    message("Cache directory does not exist: ", cache_dir)
    return(data.frame())
  }
  
  files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
  
  if (length(files) == 0) {
    message("No cached results found")
    return(data.frame())
  }
  
  cache_info <- data.frame(
    file = basename(files),
    path = files,
    size_kb = round(file.size(files) / 1024, 2),
    modified = file.mtime(files),
    stringsAsFactors = FALSE
  )
  
  cache_info <- cache_info[order(cache_info$modified, decreasing = TRUE), ]
  
  return(cache_info)
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
aggregate_qol_cohorts <- function(auc_target = c(1,2,3,4,5,6,7,8,9),
                                  type = c("eq5d", "usiqol")) {
  all_cohorts <- list()
  
  if (type == "usiqol") {
    for (i in auc_target) {
      key <- as.integer(i)
      message("Processing AUC = ", key)
      
      message("  Loading 2016 data...")
      cohort_2016_min_xr <- qol_2016_xr_min[[key]] %>% mutate(cohort_type = "Minimum FU, US", auc = i)
      cohort_2016_min_ct <- qol_2016_ct_min[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
      cohort_2016_max_xr <- qol_2016_xr_max[[key]] %>% mutate(cohort_type = "Maximum FU, US", auc = i)
      cohort_2016_max_ct <- qol_2016_ct_max[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
      cohort_2016_min_xr_us <- qol_2016_xr_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
      cohort_2016_max_xr_us <- qol_2016_xr_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
      
      message("  Loading 2017 data...")
      cohort_2017_min_xr <- qol_2017_xr_min[[key]] %>% mutate(cohort_type = "Minimum FU, US", auc = i)
      cohort_2017_min_ct <- qol_2017_ct_min[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
      cohort_2017_max_xr <- qol_2017_xr_max[[key]] %>% mutate(cohort_type = "Maximum FU, US", auc = i)
      cohort_2017_max_ct <- qol_2017_ct_max[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
      cohort_2017_min_xr_us <- qol_2017_xr_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
      cohort_2017_max_xr_us <- qol_2017_xr_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
      
      message("  Loading 2018 data...")
      cohort_2018_min_xr <- qol_2018_xr_min[[key]] %>% mutate(cohort_type = "Minimum FU, US", auc = i)
      cohort_2018_min_ct <- qol_2018_ct_min[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
      cohort_2018_max_xr <- qol_2018_xr_max[[key]] %>% mutate(cohort_type = "Maximum FU, US", auc = i)
      cohort_2018_max_ct <- qol_2018_ct_max[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
      cohort_2018_min_xr_us <- qol_2018_xr_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
      cohort_2018_max_xr_us <- qol_2018_xr_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
      
      message("  Loading 2019 data...")
      cohort_2019_min_xr <- qol_2019_xr_min[[key]] %>% mutate(cohort_type = "Minimum FU, US", auc = i)
      cohort_2019_min_ct <- qol_2019_ct_min[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
      cohort_2019_max_xr <- qol_2019_xr_max[[key]] %>% mutate(cohort_type = "Maximum FU, US", auc = i)
      cohort_2019_max_ct <- qol_2019_ct_max[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
      cohort_2019_min_xr_us <- qol_2019_xr_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
      cohort_2019_max_xr_us <- qol_2019_xr_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
      
      message("  Loading 2020 data...")
      cohort_2020_min_xr <- qol_2020_xr_min[[key]] %>% mutate(cohort_type = "Minimum FU, US", auc = i)
      cohort_2020_min_ct <- qol_2020_ct_min[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
      cohort_2020_max_xr <- qol_2020_xr_max[[key]] %>% mutate(cohort_type = "Maximum FU, US", auc = i)
      cohort_2020_max_ct <- qol_2020_ct_max[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
      cohort_2020_min_xr_us <- qol_2020_xr_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
      cohort_2020_max_xr_us <- qol_2020_xr_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
      
      message("  Combining cohorts for AUC = ", key)
      overall_cohort <- dplyr::bind_rows(
        cohort_2016_min_xr, cohort_2016_min_ct, cohort_2016_max_xr, cohort_2016_max_ct, cohort_2016_min_xr_us, cohort_2016_max_xr_us,
        cohort_2017_min_xr, cohort_2017_min_ct, cohort_2017_max_xr, cohort_2017_max_ct, cohort_2017_min_xr_us, cohort_2017_max_xr_us,
        cohort_2018_min_xr, cohort_2018_min_ct, cohort_2018_max_xr, cohort_2018_max_ct, cohort_2018_min_xr_us, cohort_2018_max_xr_us,
        cohort_2019_min_xr, cohort_2019_min_ct, cohort_2019_max_xr, cohort_2019_max_ct, cohort_2019_min_xr_us, cohort_2019_max_xr_us,
        cohort_2020_min_xr, cohort_2020_min_ct, cohort_2020_max_xr, cohort_2020_max_ct, cohort_2020_min_xr_us, cohort_2020_max_xr_us
      )
      
      all_cohorts[[key]] <- overall_cohort
      message("✅ Done with AUC = ", key, "\n")
    }
  }
  else {
    for (i in auc_target) {
      auc_value <- case_when(
        auc_target == 1 ~ 0.55,
        auc_target == 2 ~ 0.6,
        auc_target == 3 ~ 0.65,
        auc_target == 4 ~ 0.7,
        auc_target == 5 ~ 0.75,
        auc_target == 6 ~ 0.8,
        auc_target == 7 ~ 0.85,
        auc_target == 8 ~ 0.9,
        auc_target == 9 ~ 0.95,
        TRUE ~ NA_real_
      )
      
      key <- as.integer(auc_target)
      message("Processing AUC = ", auc_value)
      
      message("  Loading 2016 data...")
      cohort_2016_min_us <- qol_2016_us_min%>% filter (auc_target == auc_value) %>% mutate(cohort_type = "Minimum FU, US", auc = i)
      cohort_2016_min_ct <- qol_2016_ct_min%>% filter (auc_target == auc_value) %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
      cohort_2016_max_us <- qol_2016_us_max%>% filter (auc_target == auc_value) %>% mutate(cohort_type = "Maximum FU, US", auc = i)
      cohort_2016_max_ct <- qol_2016_ct_max%>% filter (auc_target == auc_value) %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
      cohort_2016_min_xr_us <- qol_2016_xr_us_min%>% filter (auc_target == auc_value) %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
      cohort_2016_max_xr_us <- qol_2016_xr_us_max%>% filter (auc_target == auc_value) %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
      
      message("  Loading 2017 data...")
      cohort_2017_min_us <- qol_2017_us_min%>% filter (auc_target == auc_value) %>% mutate(cohort_type = "Minimum FU, US", auc = i)
      cohort_2017_min_ct <- qol_2017_ct_min%>% filter (auc_target == auc_value) %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
      cohort_2017_max_us <- qol_2017_us_max%>% filter (auc_target == auc_value) %>% mutate(cohort_type = "Maximum FU, US", auc = i)
      cohort_2017_max_ct <- qol_2017_ct_max%>% filter (auc_target == auc_value) %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
      cohort_2017_min_xr_us <- qol_2017_xr_us_min%>% filter (auc_target == auc_value) %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
      cohort_2017_max_xr_us <- qol_2017_xr_us_max%>% filter (auc_target == auc_value) %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
      
      message("  Loading 2018 data...")
      cohort_2018_min_us <- qol_2018_us_min %>% filter (auc_target == auc_value) %>% mutate(cohort_type = "Minimum FU, US", auc = i)
      cohort_2018_min_ct <- qol_2018_ct_min %>% filter (auc_target == auc_value) %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
      cohort_2018_max_us <- qol_2018_us_max %>% filter (auc_target == auc_value) %>% mutate(cohort_type = "Maximum FU, US", auc = i)
      cohort_2018_max_ct <- qol_2018_ct_max %>% filter (auc_target == auc_value) %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
      cohort_2018_min_xr_us <- qol_2018_xr_us_min %>% filter (auc_target == auc_value) %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
      cohort_2018_max_xr_us <- qol_2018_xr_us_max %>% filter (auc_target == auc_value) %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
      
      message("  Loading 2019 data...")
      cohort_2019_min_us <- qol_2019_us_min %>% filter (auc_target == auc_value) %>% mutate(cohort_type = "Minimum FU, US", auc = i)
      cohort_2019_min_ct <- qol_2019_ct_min %>% filter (auc_target == auc_value) %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
      cohort_2019_max_us <- qol_2019_us_max %>% filter (auc_target == auc_value) %>% mutate(cohort_type = "Maximum FU, US", auc = i)
      cohort_2019_max_ct <- qol_2019_ct_max %>% filter (auc_target == auc_value) %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
      cohort_2019_min_xr_us <- qol_2019_xr_us_min %>% filter (auc_target == auc_value) %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
      cohort_2019_max_xr_us <- qol_2019_xr_us_max %>% filter (auc_target == auc_value) %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
      
      message("  Loading 2020 data...")
      cohort_2020_min_us <- qol_2020_us_min %>% filter (auc_target == auc_value) %>% mutate(cohort_type = "Minimum FU, US", auc = i)
      cohort_2020_min_ct <- qol_2020_ct_min %>% filter (auc_target == auc_value) %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
      cohort_2020_max_us <- qol_2020_us_max %>% filter (auc_target == auc_value) %>% mutate(cohort_type = "Maximum FU, US", auc = i)
      cohort_2020_max_ct <- qol_2020_ct_max %>% filter (auc_target == auc_value) %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
      cohort_2020_min_xr_us <- qol_2020_xr_us_min %>% filter (auc_target == auc_value) %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
      cohort_2020_max_xr_us <- qol_2020_xr_us_max %>% filter (auc_target == auc_value) %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
      
      message("  Combining cohorts for AUC = ", auc_value)
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
  }
  
  message("🔗 Binding all AUC cohorts together...")
  final_df <- dplyr::bind_rows(all_cohorts)
  message("✅ All done.")
  
  return(final_df)
}

### 11.5.2 AUC 0.55 ####
qol_auc_0.55 <- aggregate_qol_cohorts(auc_target = 1, type = "eq5d")

### 11.5.3 AUC 0.6 ####
qol_auc_0.6 <- aggregate_qol_cohorts(auc_target = 2, type = "eq5d")

### 11.5.4 AUC 0.65 ####
qol_auc_0.65 <- aggregate_qol_cohorts(auc_target = 3, type = "eq5d")

### 11.5.5 AUC 0.7 ####
qol_auc_0.7 <- aggregate_qol_cohorts(auc_target = 4, type = "eq5d")

### 11.5.6 AUC 0.75 ####
qol_auc_0.75 <- aggregate_qol_cohorts(auc_target = 5, type = "eq5d")

### 11.5.7 AUC 0.8 ####
qol_auc_0.8 <- aggregate_qol_cohorts(auc_target = 6, type = "eq5d")

### 11.5.8 AUC 0.85 ####
qol_auc_0.85 <- aggregate_qol_cohorts(auc_target = 7, type = "eq5d")

### 11.5.9 AUC 0.9 ####
qol_auc_0.9 <- aggregate_qol_cohorts(auc_target = 8, type = "eq5d")

### 11.5.10 AUC 0.95 ####
qol_auc_0.95 <- aggregate_qol_cohorts(auc_target = 9, type = "eq5d")

## 11.6 Combine datasets and calculate cumulative qol ####
combine_auc_data <- function(data, auc_label) {
  data %>%
    mutate(
      auc_label = auc_label,
      risk_status = case_when(
        risk_status == "LR" ~ "Low Risk",
        risk_status == "HR" ~ "High Risk",
        TRUE ~ risk_status
      ),
      stone_free_status = stone_free_status_original
    ) %>%
    select(auc_label,
           cohort_type,
           stone_free_status,
           risk_status,
           qaly_5yr,
           true_rec_5yr
    )
}

auc_list <- list(
  qol_auc_0.55,
  qol_auc_0.6,
  qol_auc_0.65,
  qol_auc_0.7,
  qol_auc_0.75,
  qol_auc_0.8,
  qol_auc_0.85,
  qol_auc_0.9,
  qol_auc_0.95
)

auc_labels <- c(
  "AUC 0.55", "AUC 0.6", "AUC 0.65",
  "AUC 0.7", "AUC 0.75", "AUC 0.8",
  "AUC 0.85", "AUC 0.9", "AUC 0.95"
)

data_for_plot_qol <- map2_dfr(
  auc_list,
  auc_labels,
  combine_auc_data,
  .progress = TRUE
)

data_for_plot_qol$auc_label <- as.factor(data_for_plot_qol$auc_label)
data_for_plot_qol$cohort_type <- as.factor(data_for_plot_qol$cohort_type)
data_for_plot_qol$risk_status <- as.factor(data_for_plot_qol$risk_status)
data_for_plot_qol$true_rec_5yr <- as.factor(data_for_plot_qol$true_rec_5yr)

## 11.7 Summarise ####
# Summarise mean and SD by auc_label, risk_status, cohort_type
summary_df_qol <- data_for_plot_qol %>%
  group_by(auc_label, risk_status, cohort_type) %>%
  summarise(
    mean_qaly_5yr = mean(qaly_5yr, na.rm = TRUE),
    lower_qaly_5yr = quantile(qaly_5yr, 0.025, na.rm = TRUE),
    upper_qaly_5yr = quantile(qaly_5yr, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

# Summarise mean and SD for combined risk groups ("All")
summary_all <- data_for_plot_qol %>%
  group_by(auc_label, cohort_type) %>%
  summarise(
    mean_qaly_5yr = mean(qaly_5yr, na.rm = TRUE),
    lower_qaly_5yr = quantile(qaly_5yr, 0.025, na.rm = TRUE),
    upper_qaly_5yr = quantile(qaly_5yr, 0.975, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(risk_status = "All") 

# Combine all summaries
summary_df_qol <- bind_rows(summary_df_qol, summary_all)

# Factor levels for ordering
summary_df_full_qol_data_eq_5d <- summary_df_qol %>%
  mutate(
    cohort_type = factor(cohort_type, levels = c(
      "Minimum FU, XR + US", "Minimum FU, US", "Minimum FU, CT", "Maximum FU, XR + US",
      "Maximum FU, US", "Maximum FU, CT"
                                                 
    )),
    risk_status = factor(risk_status, levels = c("All", "Low Risk", "High Risk"))
  )

# Prepare data_for_plot factors similarly
data_for_plot_qol2 <- data_for_plot_qol %>%
  mutate(
    cohort_type = factor(cohort_type, levels = c("Minimum FU, XR + US", "Minimum FU, US", "Minimum FU, CT", "Maximum FU, XR + US",
                                                 "Maximum FU, US", "Maximum FU, CT")),
    risk_status = factor(risk_status, levels = c("Low Risk", "High Risk"))
  )

# All auc values
auc_values <- unique(data_for_plot_qol2$auc_label)

## 11.8 Plot QALYs ####
summary_df_full_qol_data_eq_5d %>% 
  group_by(auc_label) %>%
  ggplot(aes(x = auc_label, y = mean_qaly_5yr, fill = risk_status)) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    color = "black"
  ) +
  geom_errorbar(
    aes(ymin = lower_qaly_5yr, ymax = upper_qaly_5yr),
    position = position_dodge(width = 0.8),
    width = 0.3
  ) +
  facet_wrap(~ cohort_type) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 10)) +
  labs(title = "Mean Quality Adjusted Life Years for EAU Follow-Up of those with Clinically Significant Disease",
       x = "Cohort Type",
       y = "Mean 5yr QALYs (EQ-5D derived)",
       fill = "Risk Status") 
