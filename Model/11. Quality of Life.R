# 11. Quality of Life ####
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


## 11.2 Load data ####
### 11.2.1 Read in data ####
usiqol_metrics <- fread("Inputs/usiqol_scores.csv", header = TRUE) %>% 
  janitor::clean_names() %>%
  as_tibble() %>%
  subset(select = c(date_of_birth,
                    age,
                    trial_number,
                    date_completed,
                    total_pre,
                    date_completed_2,
                    total_post_1,
                    date_completed_3,
                    total_post_2))

usiqol_stone_sizes_pre_post <- fread("Inputs/stone_free_statuses_usiqol_2.csv", header = TRUE) %>% 
  janitor::clean_names() %>%
  as_tibble()

### 11.2.3 Define data ####

usiqol_metrics$date_of_birth <- as.POSIXct(usiqol_metrics$date_of_birth, format = "%d/%m/%Y")
usiqol_metrics$age <- as.integer(usiqol_metrics$age)
usiqol_metrics$trial_number <- as.integer(usiqol_metrics$trial_number)
usiqol_metrics$date_completed <- as.POSIXct(usiqol_metrics$date_completed, format = "%d/%m/%Y")
usiqol_metrics$total_pre <- as.integer(usiqol_metrics$total_pre)
usiqol_metrics$date_completed_2 <- as.POSIXct(usiqol_metrics$date_completed_2, format = "%d/%m/%Y")
usiqol_metrics$total_post_1 <- as.integer(usiqol_metrics$total_post_1)
usiqol_metrics$date_completed_3 <- as.POSIXct(usiqol_metrics$date_completed_3, format = "%d/%m/%Y")
usiqol_metrics$total_post_2 <- as.integer(usiqol_metrics$total_post_2)

usiqol_stone_sizes_pre_post$sex <- as.factor(usiqol_stone_sizes_pre_post$sex)
usiqol_stone_sizes_pre_post$intervention <- as.factor(usiqol_stone_sizes_pre_post$intervention)
usiqol_stone_sizes_pre_post$successful <- as.factor(usiqol_stone_sizes_pre_post$successful)
usiqol_stone_sizes_pre_post$pre_op_stone_free_status <- as.integer(usiqol_stone_sizes_pre_post$pre_op_stone_free_status)
usiqol_stone_sizes_pre_post$post_op_stone_free_status <- as.integer(usiqol_stone_sizes_pre_post$post_op_stone_free_status)

### 11.2.4 Assign age bands ####

usiqol_metrics <- usiqol_metrics %>%
  mutate(
    age_1 = age,
    age_2 = as.numeric(difftime(date_completed_2, date_of_birth, units = "days")) / 365.25,
    age_3 = as.numeric(difftime(date_completed_3, date_of_birth, units = "days")) / 365.25,
    age_bin = case_when(
      age_1 < 5 ~ "Aged 1 to 4", 
      age_1 >4 & age_1 < 10 ~ "Aged 5 to 9",
      age_1 >9 & age_1 < 15 ~ "Aged 10-14",
      age_1 >14 & age_1 < 20 ~ "Aged 15-19",
      age_1 >19 & age_1 < 25 ~ "Aged 20-24" ,     
      age_1 >24 & age_1 < 30 ~ "Aged 25-29",
      age_1 >29 & age_1 < 35 ~ "Aged 30-34", 
      age_1 >34 & age_1 < 40 ~ "Aged 35-39", 
      age_1 >39 & age_1 < 45 ~ "Aged 40-44",      
      age_1 >44 & age_1 < 50 ~ "Aged 45-49",   
      age_1 >49 & age_1 < 55 ~ "Aged 50-54",   
      age_1 >54 & age_1 < 60 ~ "Aged 55-59",      
      age_1 >59 & age_1 < 65 ~ "Aged 60-64",
      age_1 >64 & age_1 < 70 ~ "Aged 65-69", 
      age_1 >69 & age_1 < 75 ~ "Aged 70-74",  
      age_1 >74 & age_1 < 80 ~ "Aged 75-79",      
      age_1 >79 & age_1 < 85 ~ "Aged 80-84",
      age_1 >84 & age_1 < 90 ~ "Aged 85-89",
      age_1 >89 ~ "Aged 90 and over"
    ),
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
  ) %>% 
  drop_na(trial_number)

usiqol_stone_sizes_pre_post <- usiqol_stone_sizes_pre_post %>%
  subset(select = c(
    trial_number,
    pre_op_stone_free_status,
    post_op_stone_free_status,
    intervention,
    successful
  )) %>% 
  drop_na(trial_number)

### 11.2.5 Assign stone free status ####

usiqol_metrics_aggregated <- usiqol_metrics %>%
  left_join(usiqol_stone_sizes_pre_post,
            by = c("trial_number" = "trial_number")) %>%
  mutate(stone_free_status_pre = case_when(
    pre_op_stone_free_status == 0 ~ "SF",
    pre_op_stone_free_status >=4 ~ "more4",
    pre_op_stone_free_status < 4 ~ "less4",
    TRUE ~ NA_character_
  ),
  stone_free_status_post = case_when(
    post_op_stone_free_status == 0 ~ "SF",
    post_op_stone_free_status >=4 ~ "more4",
    post_op_stone_free_status < 4 ~ "less4",
    TRUE ~ NA_character_
  )) 

### 11.2.6 Get change with treatment ####

usiqol_change_with_rx <- usiqol_metrics_aggregated %>%
  group_by(intervention, stone_free_status_pre) %>%
  summarise(
    n = sum(!is.na(total_post_1 - total_pre)),
    qol_change = mean(total_post_1 - total_pre, na.rm = TRUE),
    qol_sd = sd(total_post_1 - total_pre, na.rm = TRUE),
    qol_se = qol_sd / sqrt(n)
  ) %>% 
  ungroup() %>% 
  subset(select = c(
    intervention,
    stone_free_status_pre,
    qol_change,
    qol_sd,
    qol_se
  ))

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
  
usiqol_metrics_by_age_stone_free_status <-  
  usiqol_metrics_aggregated  %>%
  subset(select = c(age_bin,
                    total_pre,
                    total_post_1,
                    stone_free_status_pre,
                    stone_free_status_post
                    )) %>% 
  pivot_longer(cols = c(total_pre,total_post_1),
               names_to = "scored_when",
               values_to = "usiqol_scores") %>%
  mutate(
    stone_free_status = case_when(
      scored_when == "total_pre" ~ stone_free_status_pre,
      scored_when == "total_post_1" ~ stone_free_status_post,
      TRUE ~ NA_character_
    ) %>% as.factor()
  ) %>%
  subset(select = -c(
    stone_free_status_pre,
    stone_free_status_post,
    scored_when
  )) %>%
  group_by(age_bin, stone_free_status) %>%
  summarise(
    n = sum(!is.na(usiqol_scores)),
    total_mean = mean(usiqol_scores, na.rm = TRUE),
    total_sd   = sd(usiqol_scores, na.rm = TRUE),
    total_se = total_sd / sqrt(n),
    total_ci_lower = total_mean - 1.96 * total_se,
    total_ci_upper = total_mean + 1.96 * total_se
  ) %>% 
  select(-n) %>%
  ungroup()

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
# Use Mice to impute missing data
usiqol_metrics_by_age_stone_free_status1 <-  
  usiqol_metrics_aggregated  %>%
  subset(select = c(age_bin,
                    total_pre,
                    total_post_1,
                    stone_free_status_pre,
                    stone_free_status_post
  )) %>% 
  pivot_longer(cols = c(total_pre,total_post_1),
               names_to = "scored_when",
               values_to = "usiqol_scores") %>%
  mutate(
    stone_free_status = case_when(
      scored_when == "total_pre" ~ stone_free_status_pre,
      scored_when == "total_post_1" ~ stone_free_status_post,
      TRUE ~ NA_character_
    ) %>% as.factor()
  ) %>%
  subset(select = -c(
    stone_free_status_pre,
    stone_free_status_post,
    scored_when
  )) %>% 
  complete(age_bin, stone_free_status = c("more4", "less4", "SF"), fill = list(usiqol_scores = NA)) %>%
  mice(m = 5, 
       method = "lasso.norm") %>%
  complete() %>%
  filter(age_bin != "Aged 1 to 4") %>%
  drop_na(stone_free_status) %>%
  group_by(age_bin, stone_free_status) %>%
  summarise(
    n = sum(!is.na(usiqol_scores)),
    total_mean = mean(usiqol_scores, na.rm = TRUE),
    total_sd   = sd(usiqol_scores, na.rm = TRUE),
    total_se = total_sd / sqrt(n),
    total_ci_lower = total_mean - 1.96 * total_se,
    total_ci_upper = total_mean + 1.96 * total_se
  ) %>% 
  select(-n) %>%
  ungroup()

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
usiqol_metrics_by_age_stone_free_status <- usiqol_metrics_by_age_stone_free_status2 # Imputed scores
# usiqol_metrics_by_age_stone_free_status1 # mean scores  

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
    idx[1]  # earliest year with intervention
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
    
    # Get baseline QoL by MC simulation
    chunk <- chunk %>%
      left_join(
        baseline_qol %>% select(age_bin, stone_free_status, total_mean, total_sd),
        by = c("baseline_age_bin" = "age_bin", "stone_free_status")
      )
    
    # Baseline QoL MC
    chunk <- chunk %>%
      mutate(
        baseline_qol_mean = pmap_dbl(list(total_mean, total_sd), 
                                     ~ mean(pmin(max_qol, pmax(0, rnorm(mc_reps, ..1, ..2))))),
        baseline_qol_lower = pmap_dbl(list(total_mean, total_sd),
                                      ~ quantile(pmin(max_qol, pmax(0, rnorm(mc_reps, ..1, ..2))), probs = alpha)),
        baseline_qol_upper = pmap_dbl(list(total_mean, total_sd),
                                      ~ quantile(pmin(max_qol, pmax(0, rnorm(mc_reps, ..1, ..2))), probs = 1 - alpha))
      )
    
    for (fu_year in 1:5) {
      
      prev_qol <- if (fu_year == 1) "baseline_qol_mean" else paste0("qol_mean_year_", fu_year - 1)
      prev_qol_lower <- if (fu_year == 1) "baseline_qol_lower" else paste0("qol_lower_year_", fu_year - 1)
      prev_qol_upper <- if (fu_year == 1) "baseline_qol_upper" else paste0("qol_upper_year_", fu_year - 1)
      prev_age_bin <- if (fu_year == 1) "baseline_age_bin" else paste0("age_bin_fu_year_", fu_year - 1)
      current_age_bin <- paste0("age_bin_fu_year_", fu_year)
      intervention_col <- paste0("colic_intervention_type_year_", fu_year)
      
      chunk <- chunk %>%
        mutate(
          !!paste0("qol_mean_year_", fu_year) := case_when(
            !is.na(.data[[intervention_col]]) & .data[[intervention_col]] != "No" &
              first_intervention_year == fu_year ~ .data[[prev_qol]],  
            !is.na(.data[[prev_age_bin]]) &
              !is.na(.data[[current_age_bin]]) &
              .data[[prev_age_bin]] == .data[[current_age_bin]] ~ .data[[prev_qol]],
            TRUE ~ .data[[prev_qol]]  
          ),
          !!paste0("qol_lower_year_", fu_year) := case_when(
            !is.na(.data[[intervention_col]]) & .data[[intervention_col]] != "No" &
              first_intervention_year == fu_year ~ .data[[prev_qol_lower]],  
            !is.na(.data[[prev_age_bin]]) &
              !is.na(.data[[current_age_bin]]) &
              .data[[prev_age_bin]] == .data[[current_age_bin]] ~ .data[[prev_qol_lower]],
            TRUE ~ .data[[prev_qol_lower]]  
          ),
          !!paste0("qol_upper_year_", fu_year) := case_when(
            !is.na(.data[[intervention_col]]) & .data[[intervention_col]] != "No" &
              first_intervention_year == fu_year ~ .data[[prev_qol_upper]],  
            !is.na(.data[[prev_age_bin]]) &
              !is.na(.data[[current_age_bin]]) &
              .data[[prev_age_bin]] == .data[[current_age_bin]] ~ .data[[prev_qol_upper]],
            TRUE ~ .data[[prev_qol_upper]]  
          )
        )
    }
    
    
    
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




## 11.4 Function to calculate scores over years #### 
calculate_qol <- function(complete_pop_yr_fu,
                          cutpoints_yr,
                          start_year,
                          years = c(1,2,3,4,5),
                          target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                          fu_type = c("min", "max"),
                          post_op_imaging = c("none", "ct", "xr", "us"),
                          imaging_fu_type = c("ct", "xr", "xr_us"),
                          xr_sens = 0.67,
                          xr_spec = 0.98,
                          us_sens = 0.54,
                          us_spec = 0.91) {
  
  results_list <- list()
  post_op_imaging <- post_op_imaging[1]
  if(post_op_imaging == "none") post_op_imaging <- 0
  
  for (target_auc in target_aucs) {
    message("Calculating QoL for: ", start_year,
            ", AUC: ", target_auc,
            ", Follow-up Type: ", fu_type,
            ", Follow-up Imaging: ", imaging_fu_type,
            " and Post-Operative Imaging: ", post_op_imaging)
    
    target_auc <- as.numeric(target_auc)
    
    complete_pop_yr_fu2 <- complete_pop_yr_fu %>%
      filter(.data$auc_target == target_auc)
    
    # Cutpoint for this AUC
    cutpoint <- (cutpoints_yr %>% filter(auc_target == !!target_auc))$cutpoint
    
    # Determine non-SF proportions for distributing misclassified SF
    not_sf_props <- complete_pop_yr_fu2 %>%
      filter(stone_free_status %in% c("less4", "more4")) %>%
      count(stone_free_status) %>%
      mutate(prop = n / sum(n)) %>%
      select(stone_free_status, prop)
    less4_prob <- not_sf_props$prop[not_sf_props$stone_free_status == "less4"]
    
    # Assign SF status according to imaging type
    rand_sens <- runif(nrow(complete_pop_yr_fu2))
    rand_spec <- runif(nrow(complete_pop_yr_fu2))
    
    complete_pop_yr_fu1 <- complete_pop_yr_fu2 %>%
      mutate(
        stone_free_status_original = stone_free_status,
        stone_free_status1 = case_when(
          imaging_fu_type %in% c("xr", "xr_us") &
            stone_free_status_original %in% c("less4", "more4") & 
            rand_sens <= ifelse(imaging_fu_type=="xr", xr_sens, us_sens) ~ stone_free_status_original,
          
          imaging_fu_type %in% c("xr", "xr_us") &
            stone_free_status_original %in% c("less4", "more4") & 
            rand_sens > ifelse(imaging_fu_type=="xr", xr_sens, us_sens) ~ "SF",  
          
          imaging_fu_type %in% c("xr", "xr_us") &
            stone_free_status_original == "SF" & 
            rand_spec <= ifelse(imaging_fu_type=="xr", xr_spec, us_spec) ~ "SF",  
          
          imaging_fu_type %in% c("xr", "xr_us") &
            stone_free_status_original == "SF" & 
            rand_spec > ifelse(imaging_fu_type=="xr", xr_spec, us_spec) ~
            if_else(runif(n()) <= less4_prob, "less4", "more4"),
          
          TRUE ~ stone_free_status_original
        ),
        .keep = "all"
      )
    
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
        auc_target = auc_target,
        cutpoint = cutpoint,
        post_op_imaging = post_op_imaging,
        imaging_fu_type = imaging_fu_type,
        year = start_year
      )
    
    # --- QALY calculation ---
    # Compute QALY as sum of mean QoL across years divided by max possible score
    message("Calculating QALYs for AUC:", target_auc)
    combined_result <- combined_result %>%
      mutate(
        qaly_5yr = rowSums(select(., starts_with("qol_mean_year_")), na.rm = TRUE) / (60*5),
        qaly_5yr_lower = rowSums(select(., starts_with("qol_lower_year_")), na.rm = TRUE) / (60*5),
        qaly_5yr_upper = rowSums(select(., starts_with("qol_upper_year_")), na.rm = TRUE) / (60*5),
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
        baseline_qol_lower,
        qol_lower_year_1,
        qol_lower_year_2,
        qol_lower_year_3,
        qol_lower_year_4,
        qol_lower_year_5,
        baseline_qol_upper,
        qol_upper_year_1,
        qol_upper_year_2,
        qol_upper_year_3,
        qol_upper_year_4,
        qol_upper_year_5,
        qaly_5yr,
        qaly_5yr_lower,
        qaly_5yr_upper
      )
    
    results_list[[paste0("auc_", target_auc)]] <- combined_result
  }
  
  message("Concatenating data for Year:", start_year, " Follow-up Type: ", fu_type, " Imaging Type: ", imaging_fu_type)
  if (length(auc_targets) == 1) {
    return(results_list[[1]])
  } else {
    return(results_list)
  }
}


## 11.4 Run QoL function ####
### 11.4.1 2016 ####
qol_2016_xr_min <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2016_fu,
  cutpoints_yr = cutpoints_2016,
  start_year = 2016,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  imaging_fu_type = "xr"
)

qol_2016_xr_max <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2016_fu,
  cutpoints_yr = cutpoints_2016,
  start_year = 2016,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  imaging_fu_type = "xr"
)

qol_2016_xr_us_min <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2016_fu,
  cutpoints_yr = cutpoints_2016,
  start_year = 2016,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  imaging_fu_type = "xr_us"
)

qol_2016_xr_us_max <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2016_fu,
  cutpoints_yr = cutpoints_2016,
  start_year = 2016,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  imaging_fu_type = "xr_us"
)

qol_2016_ct_min <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2016_fu,
  cutpoints_yr = cutpoints_2016,
  start_year = 2016,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  imaging_fu_type = "ct"
)

qol_2016_ct_max <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2016_fu,
  cutpoints_yr = cutpoints_2016,
  start_year = 2016,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  imaging_fu_type = "ct"
)

### 11.4.2 2017 ####
qol_2017_xr_min <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2017_fu,
  cutpoints_yr = cutpoints_2017,
  start_year = 2017,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  imaging_fu_type = "xr"
)

qol_2017_xr_max <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2017_fu,
  cutpoints_yr = cutpoints_2017,
  start_year = 2017,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  imaging_fu_type = "xr"
)

qol_2017_xr_us_min <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2017_fu,
  cutpoints_yr = cutpoints_2017,
  start_year = 2017,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  imaging_fu_type = "xr_us"
)

qol_2017_xr_us_max <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2017_fu,
  cutpoints_yr = cutpoints_2017,
  start_year = 2017,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  imaging_fu_type = "xr_us"
)

qol_2017_ct_min <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2017_fu,
  cutpoints_yr = cutpoints_2017,
  start_year = 2017,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  imaging_fu_type = "ct"
)

qol_2017_ct_max <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2017_fu,
  cutpoints_yr = cutpoints_2017,
  start_year = 2017,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  imaging_fu_type = "ct"
)

### 11.4.3 2018 ####
qol_2018_xr_min <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2018_fu,
  cutpoints_yr = cutpoints_2018,
  start_year = 2018,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  imaging_fu_type = "xr"
)

qol_2018_xr_max <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2018_fu,
  cutpoints_yr = cutpoints_2018,
  start_year = 2018,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  imaging_fu_type = "xr"
)

qol_2018_xr_us_min <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2018_fu,
  cutpoints_yr = cutpoints_2018,
  start_year = 2018,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  imaging_fu_type = "xr_us"
)

qol_2018_xr_us_max <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2018_fu,
  cutpoints_yr = cutpoints_2018,
  start_year = 2018,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  imaging_fu_type = "xr_us"
)

qol_2018_ct_min <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2018_fu,
  cutpoints_yr = cutpoints_2018,
  start_year = 2018,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  imaging_fu_type = "ct"
)

qol_2018_ct_max <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2018_fu,
  cutpoints_yr = cutpoints_2018,
  start_year = 2018,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  imaging_fu_type = "ct"
)

### 11.4.4 2019 ####
qol_2019_xr_min <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2019_fu,
  cutpoints_yr = cutpoints_2019,
  start_year = 2019,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  imaging_fu_type = "xr"
)

qol_2019_xr_max <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2019_fu,
  cutpoints_yr = cutpoints_2019,
  start_year = 2019,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  imaging_fu_type = "xr"
)

qol_2019_xr_us_min <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2019_fu,
  cutpoints_yr = cutpoints_2019,
  start_year = 2019,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  imaging_fu_type = "xr_us"
)

qol_2019_xr_us_max <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2019_fu,
  cutpoints_yr = cutpoints_2019,
  start_year = 2019,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  imaging_fu_type = "xr_us"
)

qol_2019_ct_min <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2019_fu,
  cutpoints_yr = cutpoints_2019,
  start_year = 2019,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  imaging_fu_type = "ct"
)

qol_2019_ct_max <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2019_fu,
  cutpoints_yr = cutpoints_2019,
  start_year = 2019,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  imaging_fu_type = "ct"
)

### 11.4.5 2020 ####
qol_2020_xr_min <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2020_fu,
  cutpoints_yr = cutpoints_2020,
  start_year = 2020,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  imaging_fu_type = "xr"
)

qol_2020_xr_max <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2020_fu,
  cutpoints_yr = cutpoints_2020,
  start_year = 2020,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  imaging_fu_type = "xr"
)

qol_2020_xr_us_min <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2020_fu,
  cutpoints_yr = cutpoints_2020,
  start_year = 2020,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  imaging_fu_type = "xr_us"
)

qol_2020_xr_us_max <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2020_fu,
  cutpoints_yr = cutpoints_2020,
  start_year = 2020,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  imaging_fu_type = "xr_us"
)

qol_2020_ct_min <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2020_fu,
  cutpoints_yr = cutpoints_2020,
  start_year = 2020,
  years = c(1,2,3,4,5),
  target_aucs = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  imaging_fu_type = "ct"
)

qol_2020_ct_max <- calculate_qol(
  complete_pop_yr_fu = complete_pop_2020_fu,
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
    cohort_2016_min_xr <- qol_2016_xr_min[[key]] %>% mutate(cohort_type = "Minimum FU, XR", auc = i)
    cohort_2016_min_ct <- qol_2016_ct_min[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2016_max_xr <- qol_2016_xr_max[[key]] %>% mutate(cohort_type = "Maximum FU, XR", auc = i)
    cohort_2016_max_ct <- qol_2016_ct_max[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    cohort_2016_min_xr_us <- qol_2016_xr_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2016_max_xr_us <- qol_2016_xr_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    
    message("  Loading 2017 data...")
    cohort_2017_min_xr <- qol_2017_xr_min[[key]] %>% mutate(cohort_type = "Minimum FU, XR", auc = i)
    cohort_2017_min_ct <- qol_2017_ct_min[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2017_max_xr <- qol_2017_xr_max[[key]] %>% mutate(cohort_type = "Maximum FU, XR", auc = i)
    cohort_2017_max_ct <- qol_2017_ct_max[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    cohort_2017_min_xr_us <- qol_2017_xr_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2017_max_xr_us <- qol_2017_xr_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    
    message("  Loading 2018 data...")
    cohort_2018_min_xr <- qol_2018_xr_min[[key]] %>% mutate(cohort_type = "Minimum FU, XR", auc = i)
    cohort_2018_min_ct <- qol_2018_ct_min[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2018_max_xr <- qol_2018_xr_max[[key]] %>% mutate(cohort_type = "Maximum FU, XR", auc = i)
    cohort_2018_max_ct <- qol_2018_ct_max[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    cohort_2018_min_xr_us <- qol_2018_xr_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2018_max_xr_us <- qol_2018_xr_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    
    message("  Loading 2019 data...")
    cohort_2019_min_xr <- qol_2019_xr_min[[key]] %>% mutate(cohort_type = "Minimum FU, XR", auc = i)
    cohort_2019_min_ct <- qol_2019_ct_min[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2019_max_xr <- qol_2019_xr_max[[key]] %>% mutate(cohort_type = "Maximum FU, XR", auc = i)
    cohort_2019_max_ct <- qol_2019_ct_max[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    cohort_2019_min_xr_us <- qol_2019_xr_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2019_max_xr_us <- qol_2019_xr_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    
    message("  Loading 2020 data...")
    cohort_2020_min_xr <- qol_2020_xr_min[[key]] %>% mutate(cohort_type = "Minimum FU, XR", auc = i)
    cohort_2020_min_ct <- qol_2020_ct_min[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2020_max_xr <- qol_2020_xr_max[[key]] %>% mutate(cohort_type = "Maximum FU, XR", auc = i)
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
  
  message("🔗 Binding all AUC cohorts together...")
  final_df <- dplyr::bind_rows(all_cohorts)
  message("✅ All done.")
  
  return(final_df)
}

### 11.5.2 AUC 0.55 ####
qol_auc_0.55 <- aggregate_qol_cohorts(auc_target = 1)

### 11.5.3 AUC 0.6 ####
qol_auc_0.6 <- aggregate_qol_cohorts(auc_target = 2)

### 11.5.4 AUC 0.55 ####
qol_auc_0.65 <- aggregate_qol_cohorts(auc_target = 3)

### 11.5.5 AUC 0.55 ####
qol_auc_0.7 <- aggregate_qol_cohorts(auc_target = 4)

### 11.5.6 AUC 0.55 ####
qol_auc_0.75 <- aggregate_qol_cohorts(auc_target = 5)

### 11.5.7 AUC 0.55 ####
qol_auc_0.8 <- aggregate_qol_cohorts(auc_target = 6)

### 11.5.8 AUC 0.55 ####
qol_auc_0.85 <- aggregate_qol_cohorts(auc_target = 7)

### 11.5.9 AUC 0.55 ####
qol_auc_0.9 <- aggregate_qol_cohorts(auc_target = 8)

### 11.5.10 AUC 0.55 ####
qol_auc_0.95 <- aggregate_qol_cohorts(auc_target = 9)

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
      annual_qol = case_when(
        year == 2020 ~ qol_mean_year_1,
        year == 2019 ~ qol_mean_year_2,
        year == 2018 ~ qol_mean_year_3,
        year == 2017 ~ qol_mean_year_4,
        year == 2016 ~ qol_mean_year_5,
        TRUE ~ NA_real_
      ),
      annual_qol_lower = case_when(
        year == 2020 ~ qol_lower_year_1,
        year == 2019 ~ qol_lower_year_2,
        year == 2018 ~ qol_lower_year_3,
        year == 2017 ~ qol_lower_year_4,
        year == 2016 ~ qol_lower_year_5,
        TRUE ~ NA_real_
      ),
      annual_qol_upper = case_when(
        year == 2020 ~ qol_upper_year_1,
        year == 2019 ~ qol_upper_year_2,
        year == 2018 ~ qol_upper_year_3,
        year == 2017 ~ qol_upper_year_4,
        year == 2016 ~ qol_upper_year_5,
        TRUE ~ NA_real_
      ),
      stone_free_status = stone_free_status_original
    ) %>%
    select(auc_label,
           cohort_type,
           stone_free_status,
           risk_status,
           annual_qol,
           annual_qol_lower,
           annual_qol_upper,
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

# Summarise mean and SD by auc_label, risk_status, cohort_type
summary_df_qol <- data_for_plot_qol %>%
  group_by(auc_label, risk_status, cohort_type) %>%
  summarise(
    total_qol = mean(annual_qol, na.rm = TRUE),
    total_qol_lower = quantile(annual_qol, 0.025, na.rm = TRUE),
    total_qol_upper = quantile(annual_qol, 0.975, na.rm = TRUE),
    mean_qaly_5yr = mean(qaly_5yr, na.rm = TRUE),
    lower_qaly_5yr = quantile(qaly_5yr, 0.025, na.rm = TRUE),
    upper_qaly_5yr = quantile(qaly_5yr, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

# Summarise mean and SD for combined risk groups ("All")
summary_all <- data_for_plot_qol %>%
  group_by(auc_label, cohort_type) %>%
  summarise(
    total_qol = mean(annual_qol, na.rm = TRUE),
    total_qol_lower = quantile(annual_qol, 0.025, na.rm = TRUE),
    total_qol_upper = quantile(annual_qol, 0.975, na.rm = TRUE),
    mean_qaly_5yr = mean(qaly_5yr, na.rm = TRUE),
    lower_qaly_5yr = quantile(qaly_5yr, 0.025, na.rm = TRUE),
    upper_qaly_5yr = quantile(qaly_5yr, 0.975, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(risk_status = "All") 

# Combine all summaries
summary_df_qol <- bind_rows(summary_df_qol, summary_all)
summary_df_qol <- summary_df_qol %>% mutate(
  total_qol = total_qol
)

# Factor levels for ordering
summary_df_full_qol_data <- summary_df_qol %>%
  mutate(
    cohort_type = factor(cohort_type, levels = c("Minimum FU, XR", 
                                                 "Minimum FU, CT", 
                                                 "Minimum FU, XR + US",
                                                 "Maximum FU, XR", 
                                                 "Maximum FU, CT", 
                                                 "Maximum FU, XR + US"
    )),
    risk_status = factor(risk_status, levels = c("All", "Low Risk", "High Risk"))
  )

# Prepare data_for_plot factors similarly
data_for_plot_qol2 <- data_for_plot_qol %>%
  mutate(
    cohort_type = factor(cohort_type, levels = c("Minimum FU, XR", 
                                                 "Minimum FU, CT", 
                                                 "Minimum FU, XR + US",
                                                 "Maximum FU, XR", 
                                                 "Maximum FU, CT", 
                                                 "Maximum FU, XR + US")),
    risk_status = factor(risk_status, levels = c("Low Risk", "High Risk"))
  )

# All auc values
auc_values <- unique(data_for_plot$auc_label)

## 11.7 Plot QoL ####
summary_df_full_qol_data %>% 
  group_by(auc_label) %>%
  ggplot(aes(x = auc_label, y = total_qol, fill = risk_status)) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    color = "black"
  ) +
  geom_errorbar(
    aes(ymin = total_qol_lower, ymax = total_qol_upper),
    position = position_dodge(width = 0.8),
    width = 0.3
  ) +
  facet_wrap(~ cohort_type) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 10)) +
  labs(title = "Annual Mean USIQoL score for EAU Follow-Up of those with Clinically Significant Disease",
       x = "Cohort Type",
       y = "USIQoL score",
       fill = "Risk Status")

## 11.8 Plot QALYs ####
summary_df_full_qol_data %>% 
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
  labs(title = "Mean USIQoL Adjusted Life Years for EAU Follow-Up of those with Clinically Significant Disease",
       x = "Cohort Type",
       y = "Mean 5yr QALYs (USIQOL)",
       fill = "Risk Status") + ylim(0,0.7)

