# 12 Calculate ICER ####
## 12.1 Load libraries ####
library(tidyverse)
library(janitor)
library(gt)
library(gtExtras)
library(ggsignif)
library(data.table)
library(pryr)
library(lubridate)
library(purrr)
library(future)
library(furrr)
library(boot)

## 12.2 Sort Cost data ####
### 12.2.1 Function to strip out auc, id and qalys ####
strip_out_costs <- function(data) {
  
  cohort <- list()
  
  for (i in 1:9){
    
    data1 <- data[[i]] %>% 
      subset(select = c(
        id,
        auc_target,
        cost_year_0,
        cost_year_1,
        cost_year_2,
        cost_year_3,
        cost_year_4,
        cost_year_5,
        year
      )) %>%
      mutate(
        cost_year_5 = ifelse(is.na(cost_year_5), 0, cost_year_5),
        cost_year_1 = cost_year_1,
        cost_year_2 = cost_year_2 / (1.035 ^ 1),
        cost_year_3 = cost_year_3 / (1.035 ^ 2),
        cost_year_4 = cost_year_4 / (1.035 ^ 3),
        cost_year_5 = cost_year_5 / (1.035 ^ 4),
        
        total_cost = rowSums(across(starts_with("cost_year_")), na.rm = TRUE)
      )
    
    cohort[[i]] <- data1
  }
  return(cohort)
}

### 12.2.2 2016 ####
costs_2016_us_min_stripped <- strip_out_costs(costs_2016_us_min)

costs_2016_xr_us_min_stripped <- strip_out_costs(costs_2016_xr_us_min)

costs_2016_ct_min_stripped <- strip_out_costs(costs_2016_ct_min)

costs_2016_us_max_stripped <- strip_out_costs(costs_2016_us_max)

costs_2016_xr_us_max_stripped <- strip_out_costs(costs_2016_xr_us_max)

costs_2016_ct_max_stripped <- strip_out_costs(costs_2016_ct_max)

### 12.2.3 2017 ####
costs_2017_us_min_stripped <- strip_out_costs(costs_2017_us_min)

costs_2017_xr_us_min_stripped <- strip_out_costs(costs_2017_xr_us_min)

costs_2017_ct_min_stripped <- strip_out_costs(costs_2017_ct_min)

costs_2017_us_max_stripped <- strip_out_costs(costs_2017_us_max)

costs_2017_xr_us_max_stripped <- strip_out_costs(costs_2017_xr_us_max)

costs_2017_ct_max_stripped <- strip_out_costs(costs_2017_ct_max)

### 12.2.4 2018 ####
costs_2018_us_min_stripped <- strip_out_costs(costs_2018_us_min)

costs_2018_xr_us_min_stripped <- strip_out_costs(costs_2018_xr_us_min)

costs_2018_ct_min_stripped <- strip_out_costs(costs_2018_ct_min)

costs_2018_us_max_stripped <- strip_out_costs(costs_2018_us_max)

costs_2018_xr_us_max_stripped <- strip_out_costs(costs_2018_xr_us_max)

costs_2018_ct_max_stripped <- strip_out_costs(costs_2018_ct_max)

### 12.2.5 2019 ####
costs_2019_us_min_stripped <- strip_out_costs(costs_2019_us_min)

costs_2019_xr_us_min_stripped <- strip_out_costs(costs_2019_xr_us_min)

costs_2019_ct_min_stripped <- strip_out_costs(costs_2019_ct_min)

costs_2019_us_max_stripped <- strip_out_costs(costs_2019_us_max)

costs_2019_xr_us_max_stripped <- strip_out_costs(costs_2019_xr_us_max)

costs_2019_ct_max_stripped <- strip_out_costs(costs_2019_ct_max)

### 12.2.6 2020 ####
costs_2020_us_min_stripped <- strip_out_costs(costs_2020_us_min)

costs_2020_xr_us_min_stripped <- strip_out_costs(costs_2020_xr_us_min)

costs_2020_ct_min_stripped <- strip_out_costs(costs_2020_ct_min)

costs_2020_us_max_stripped <- strip_out_costs(costs_2020_us_max)

costs_2020_xr_us_max_stripped <- strip_out_costs(costs_2020_xr_us_max)

costs_2020_ct_max_stripped <- strip_out_costs(costs_2020_ct_max)

## 12.3 Sort out QoL ####
### 12.3.1 Function to strip out auc, id and qol ####
strip_out_qol <- function(data) {
  
  # detect data format
  if (is.list(data) && !is.data.frame(data)) {
    data_format <- "list"
  } else if (is.data.frame(data) || tibble::is_tibble(data)) { 
    data_format <- "tibble"
  } else {
    stop("Data must be either a list or a data frame/tibble")
  }
  
  # tibble format
  if (data_format == "tibble") {
    
    cohort <- list()  
    
    for (i in 1:9) {
      auc_value <- case_when(  
        i == 1 ~ 0.55,
        i == 2 ~ 0.6,
        i == 3 ~ 0.65,
        i == 4 ~ 0.7,
        i == 5 ~ 0.75,
        i == 6 ~ 0.8,
        i == 7 ~ 0.85,
        i == 8 ~ 0.9,
        i == 9 ~ 0.95,
        TRUE ~ NA_real_)  
      
      data1 <- data %>% 
        filter(auc_target == auc_value) %>%  
        select(
          id,
          auc_target,
          baseline_qol_mean,
          qol_mean_year_1,
          qol_mean_year_2,
          qol_mean_year_3,
          qol_mean_year_4,
          qol_mean_year_5,
          year
        ) %>%
        drop_na(qol_mean_year_1) %>%
        mutate(
          qol_mean_year_0 = baseline_qol_mean,
          # Discount QoL values
          qol_mean_year_1 = case_when(
            qol_mean_year_1 > 2 ~ qol_mean_year_1 / 1.035,
            TRUE ~ qol_mean_year_1 * 1.035
          ),
          qol_mean_year_2 = case_when(
            qol_mean_year_2 > 2 ~ qol_mean_year_2 / (1.035 ^ 2),
            TRUE ~ qol_mean_year_2 * (1.035 ^ 2)
          ),
          qol_mean_year_3 = case_when(
            qol_mean_year_3 > 2 ~ qol_mean_year_3 / (1.035 ^ 3),
            TRUE ~ qol_mean_year_3 * (1.035 ^ 3)
          ),
          qol_mean_year_4 = case_when(
            qol_mean_year_4 > 2 ~ qol_mean_year_4 / (1.035 ^ 4),
            TRUE ~ qol_mean_year_4 * (1.035 ^ 4)
          ),
          qol_mean_year_5 = case_when(
            qol_mean_year_5 > 2 ~ qol_mean_year_5 / (1.035 ^ 5),
            TRUE ~ qol_mean_year_5 * (1.035 ^ 5)
          ),
          
          # Calculate 5-year QALY (excluding baseline year 0)
          qaly_5yr = case_when(
            qol_mean_year_1 > 2 ~ (qol_mean_year_1 / 60) + (qol_mean_year_2 / 60) + 
              (qol_mean_year_3 / 60) + (qol_mean_year_4 / 60) + 
              (qol_mean_year_5 / 60),
            TRUE ~ qol_mean_year_1 + qol_mean_year_2 + qol_mean_year_3 + 
              qol_mean_year_4 + qol_mean_year_5
          )
        )
      
      cohort[[i]] <- data1
    }
    
    # list format
  } else {
    
    cohort <- list()
    
    for (i in 1:9){
      
      data1 <- data[[i]] %>% 
        select(
          id,
          auc_target,
          baseline_qol_mean,
          qol_mean_year_1,
          qol_mean_year_2,
          qol_mean_year_3,
          qol_mean_year_4,
          qol_mean_year_5,
          year
        ) %>%
        drop_na(qol_mean_year_1) %>%
        mutate(
          qol_mean_year_0 = baseline_qol_mean,
          # Discount QoL values
          qol_mean_year_1 = case_when(
            qol_mean_year_1 > 2 ~ qol_mean_year_1 / 1.035,
            TRUE ~ qol_mean_year_1 * 1.035
          ),
          qol_mean_year_2 = case_when(
            qol_mean_year_2 > 2 ~ qol_mean_year_2 / (1.035 ^ 2),
            TRUE ~ qol_mean_year_2 * (1.035 ^ 2)
          ),
          qol_mean_year_3 = case_when(
            qol_mean_year_3 > 2 ~ qol_mean_year_3 / (1.035 ^ 3),
            TRUE ~ qol_mean_year_3 * (1.035 ^ 3)
          ),
          qol_mean_year_4 = case_when(
            qol_mean_year_4 > 2 ~ qol_mean_year_4 / (1.035 ^ 4),
            TRUE ~ qol_mean_year_4 * (1.035 ^ 4)
          ),
          qol_mean_year_5 = case_when(
            qol_mean_year_5 > 2 ~ qol_mean_year_5 / (1.035 ^ 5),
            TRUE ~ qol_mean_year_5 * (1.035 ^ 5)
          ),
          
          # Calculate 5-year QALY (excluding baseline year 0)
          qaly_5yr = case_when(
            qol_mean_year_1 > 2 ~ (qol_mean_year_1 / 60) + (qol_mean_year_2 / 60) + 
              (qol_mean_year_3 / 60) + (qol_mean_year_4 / 60) + 
              (qol_mean_year_5 / 60),
            TRUE ~ qol_mean_year_1 + qol_mean_year_2 + qol_mean_year_3 + 
              qol_mean_year_4 + qol_mean_year_5
          )
        )
      
      cohort[[i]] <- data1
    }
    
  }
  return(cohort)
}

### 12.3.2 2016 ####
qol_2016_us_min_stripped <- strip_out_qol(qol_2016_us_min)

qol_2016_xr_us_min_stripped <- strip_out_qol(qol_2016_xr_us_min)

qol_2016_ct_min_stripped <- strip_out_qol(qol_2016_ct_min)

qol_2016_us_max_stripped <- strip_out_qol(qol_2016_us_max)

qol_2016_xr_us_max_stripped <- strip_out_qol(qol_2016_xr_us_max)

qol_2016_ct_max_stripped <- strip_out_qol(qol_2016_ct_max)

### 12.3.3 2017 ####
qol_2017_us_min_stripped <- strip_out_qol(qol_2017_us_min)

qol_2017_xr_us_min_stripped <- strip_out_qol(qol_2017_xr_us_min)

qol_2017_ct_min_stripped <- strip_out_qol(qol_2017_ct_min)

qol_2017_us_max_stripped <- strip_out_qol(qol_2017_us_max)

qol_2017_xr_us_max_stripped <- strip_out_qol(qol_2017_xr_us_max)

qol_2017_ct_max_stripped <- strip_out_qol(qol_2017_ct_max)

### 12.3.4 2018 ####
qol_2018_us_min_stripped <- strip_out_qol(qol_2018_us_min)

qol_2018_xr_us_min_stripped <- strip_out_qol(qol_2018_xr_us_min)

qol_2018_ct_min_stripped <- strip_out_qol(qol_2018_ct_min)

qol_2018_us_max_stripped <- strip_out_qol(qol_2018_us_max)

qol_2018_xr_us_max_stripped <- strip_out_qol(qol_2018_xr_us_max)

qol_2018_ct_max_stripped <- strip_out_qol(qol_2018_ct_max)

### 12.3.5 2019 ####
qol_2019_us_min_stripped <- strip_out_qol(qol_2019_us_min)

qol_2019_xr_us_min_stripped <- strip_out_qol(qol_2019_xr_us_min)

qol_2019_ct_min_stripped <- strip_out_qol(qol_2019_ct_min)

qol_2019_us_max_stripped <- strip_out_qol(qol_2019_us_max)

qol_2019_xr_us_max_stripped <- strip_out_qol(qol_2019_xr_us_max)

qol_2019_ct_max_stripped <- strip_out_qol(qol_2019_ct_max)

### 12.3.6 2020 ####
qol_2020_us_min_stripped <- strip_out_qol(qol_2020_us_min)

qol_2020_xr_us_min_stripped <- strip_out_qol(qol_2020_xr_us_min)

qol_2020_ct_min_stripped <- strip_out_qol(qol_2020_ct_min)

qol_2020_us_max_stripped <- strip_out_qol(qol_2020_us_max)

qol_2020_xr_us_max_stripped <- strip_out_qol(qol_2020_xr_us_max)

qol_2020_ct_max_stripped <- strip_out_qol(qol_2020_ct_max)

## 12.4 Combine QALY data with Costs ####
### 12.4.1 Aggregation function ####
aggregate_qol_and_cost_cohorts <- function(auc_target = c(1,2,3,4,5,6,7,8,9)) {
  all_cohorts <- list()
  
  for (i in auc_target) {
    key <- as.integer(i)
    message("Processing AUC = ", key)
    
    message("  Loading 2016 data...")
    cohort_2016_min_us <- left_join(qol_2016_us_min_stripped[[key]],
                                    costs_2016_us_min_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Minimum FU, US", auc = i)
    cohort_2016_min_ct <- left_join(qol_2016_ct_min_stripped[[key]],
                                    costs_2016_ct_min_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2016_min_xr_us <- left_join(qol_2016_xr_us_min_stripped[[key]],
                                    costs_2016_xr_us_min_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2016_max_us <- left_join(qol_2016_us_max_stripped[[key]],
                                    costs_2016_xr_max_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Maximum FU, US", auc = i)
    cohort_2016_max_ct <- left_join(qol_2016_ct_max_stripped[[key]],
                                    costs_2016_ct_max_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    cohort_2016_max_xr_us <- left_join(qol_2016_xr_us_max_stripped[[key]],
                                    costs_2016_xr_us_max_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    
    message("  Loading 2017 data...")
    cohort_2017_min_us <- left_join(qol_2017_us_min_stripped[[key]],
                                    costs_2017_us_min_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Minimum FU, US", auc = i)
    cohort_2017_min_ct <- left_join(qol_2017_ct_min_stripped[[key]],
                                    costs_2017_ct_min_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2017_min_xr_us <- left_join(qol_2017_xr_us_min_stripped[[key]],
                                       costs_2017_xr_us_min_stripped[[key]],
                                       by = "id") %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2017_max_us <- left_join(qol_2017_us_max_stripped[[key]],
                                    costs_2017_us_max_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Maximum FU, US", auc = i)
    cohort_2017_max_ct <- left_join(qol_2017_ct_max_stripped[[key]],
                                    costs_2017_ct_max_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    cohort_2017_max_xr_us <- left_join(qol_2017_xr_us_max_stripped[[key]],
                                       costs_2017_xr_us_max_stripped[[key]],
                                       by = "id") %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    
    message("  Loading 2018 data...")
    cohort_2018_min_us <- left_join(qol_2018_us_min_stripped[[key]],
                                    costs_2018_us_min_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Minimum FU, US", auc = i)
    cohort_2018_min_ct <- left_join(qol_2018_ct_min_stripped[[key]],
                                    costs_2018_ct_min_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2018_min_xr_us <- left_join(qol_2018_xr_us_min_stripped[[key]],
                                       costs_2018_xr_us_min_stripped[[key]],
                                       by = "id") %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2018_max_us <- left_join(qol_2018_us_max_stripped[[key]],
                                    costs_2018_us_max_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Maximum FU, US", auc = i)
    cohort_2018_max_ct <- left_join(qol_2018_ct_max_stripped[[key]],
                                    costs_2018_ct_max_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    cohort_2018_max_xr_us <- left_join(qol_2018_xr_us_max_stripped[[key]],
                                       costs_2018_xr_us_max_stripped[[key]],
                                       by = "id") %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    
    message("  Loading 2019 data...")
    cohort_2019_min_us <- left_join(qol_2019_us_min_stripped[[key]],
                                    costs_2019_us_min_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Minimum FU, US", auc = i)
    cohort_2019_min_ct <- left_join(qol_2019_ct_min_stripped[[key]],
                                    costs_2019_ct_min_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2019_min_xr_us <- left_join(qol_2019_xr_us_min_stripped[[key]],
                                       costs_2019_xr_us_min_stripped[[key]],
                                       by = "id") %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2019_max_us <- left_join(qol_2019_us_max_stripped[[key]],
                                    costs_2019_us_max_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Maximum FU, US", auc = i)
    cohort_2019_max_ct <- left_join(qol_2019_ct_max_stripped[[key]],
                                    costs_2019_ct_max_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    cohort_2019_max_xr_us <- left_join(qol_2019_xr_us_max_stripped[[key]],
                                       costs_2019_xr_us_max_stripped[[key]],
                                       by = "id") %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    message("  Loading 2020 data...")
    cohort_2020_min_us <- left_join(qol_2020_us_min_stripped[[key]],
                                    costs_2020_us_min_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Minimum FU, US", auc = i)
    cohort_2020_min_ct <- left_join(qol_2020_ct_min_stripped[[key]],
                                    costs_2020_ct_min_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2020_min_xr_us <- left_join(qol_2020_xr_us_min_stripped[[key]],
                                       costs_2020_xr_us_min_stripped[[key]],
                                       by = "id") %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2020_max_us <- left_join(qol_2020_us_max_stripped[[key]],
                                    costs_2020_us_max_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Maximum FU, US", auc = i)
    cohort_2020_max_ct <- left_join(qol_2020_ct_max_stripped[[key]],
                                    costs_2020_ct_max_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    cohort_2020_max_xr_us <- left_join(qol_2020_xr_us_max_stripped[[key]],
                                       costs_2020_xr_us_max_stripped[[key]],
                                       by = "id") %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
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
  final_df <- dplyr::bind_rows(all_cohorts) %>%
    mutate(
      auc_target = auc_target.x,
      year = year.x
    ) %>%
    select(
      -c(year.x,
      year.y,
      auc_target.x,
      auc_target.y)
    )
  message("✅ All done.")
  
  return(final_df)
}

### 12.4.2 Aggregate cohorts ####
auc_0.55_for_icer <- aggregate_qol_and_cost_cohorts(auc_target = 1)
auc_0.6_for_icer <- aggregate_qol_and_cost_cohorts(auc_target = 2)
auc_0.65_for_icer <- aggregate_qol_and_cost_cohorts(auc_target = 3)
auc_0.7_for_icer <- aggregate_qol_and_cost_cohorts(auc_target = 4)
auc_0.75_for_icer <- aggregate_qol_and_cost_cohorts(auc_target = 5)
auc_0.8_for_icer <- aggregate_qol_and_cost_cohorts(auc_target = 6)
auc_0.85_for_icer <- aggregate_qol_and_cost_cohorts(auc_target = 7)
auc_0.9_for_icer <- aggregate_qol_and_cost_cohorts(auc_target = 8)
auc_0.95_for_icer <- aggregate_qol_and_cost_cohorts(auc_target = 9)

### 12.4.3 Set baseline for comparison => AUC 0.55, XR + US FU, Min FU ####
baseline_for_icer <- auc_0.55_for_icer %>% 
  filter(cohort_type == "Minimum FU, XR + US") %>%
  dplyr::select(
    id,
    year,
    qaly_5yr,
    total_cost
  )
  
  
## 12.5 Calculate ICER - Compare to AUC 0.55, Minimum FU XR + US ####  
### 12.5.1 Function to calculate ICER ####
calculate_icer <- function(intervention_data, n_bootstrap = 1000, ci_level = 0.95) {

  intervention_data$cohort_type <- as.factor(intervention_data$cohort_type)
  cohort_types <- levels(intervention_data$cohort_type)
  wtp = 20000
  
  # Initialize results dataframe
  results <- data.frame()
  
  # Bootstrap function for ICER and related metrics
  boot_icer_function <- function(data, indices, wtp) {
    d <- data[indices, ]
    
    inc_cost <- mean(d$inc_cost, na.rm = TRUE)
    inc_qaly <- mean(d$inc_qaly, na.rm = TRUE)
    
    # Calculate ICER (handle division by zero)
    icer <- if (inc_qaly != 0) inc_cost / inc_qaly else NA
    
    # Calculate NMB
    nmb <- wtp * inc_qaly - inc_cost
    
    return(c(
      icer = icer, 
      nmb = nmb, 
      inc_cost = inc_cost, 
      inc_qaly = inc_qaly
    ))
  }
  
  # Loop through each cohort_type with AUC 0.55 & Minimum FU, XR as baseline
  for (cohort in cohort_types) {
    
    # Filter intervention data for this cohort 
    intervention_subset <- intervention_data %>%
      filter(cohort_type == cohort)
    
    # Calculate means for baseline (from total_cost and qaly_5yr)
    baseline_mean_cost <- mean(baseline_for_icer$total_cost, na.rm = TRUE)
    baseline_mean_cost_sd <- sd(baseline_for_icer$total_cost, na.rm = TRUE)
    baseline_mean_qaly <- mean(baseline_for_icer$qaly_5yr, na.rm = TRUE)
    baseline_mean_qaly_sd <- sd(baseline_for_icer$qaly_5yr, na.rm = TRUE)
    baseline_n <- nrow(baseline_for_icer)
    
    # Calculate means for intervention (from total_cost and qaly_5yr)
    intervention_mean_cost <- mean(intervention_subset$total_cost, na.rm = TRUE)
    intervention_mean_cost_sd <- sd(intervention_subset$total_cost, na.rm = TRUE)
    intervention_mean_qaly <- mean(intervention_subset$qaly_5yr, na.rm = TRUE)
    intervention_mean_qaly_sd <- sd(intervention_subset$qaly_5yr, na.rm = TRUE)
    intervention_n <- nrow(intervention_subset)
    
    # Calculate incremental values (using mean differences)
    incremental_cost <- intervention_mean_cost - baseline_mean_cost
    incremental_qaly <- intervention_mean_qaly - baseline_mean_qaly
    
    # Calculate ICER (handle division by zero)
    if (incremental_qaly != 0) {
      icer <- incremental_cost / incremental_qaly
    } else {
      icer <- NA 
    }
    
    # Calculate incremental NMB using mean values
    incremental_nmb <- wtp * incremental_qaly - incremental_cost
    
    # Calculate NMB for each group (for reference)
    baseline_nmb <- wtp * baseline_mean_qaly - baseline_mean_cost
    intervention_nmb <- wtp * intervention_mean_qaly - intervention_mean_cost
    
    # Calculate probability of cost-effectiveness using paired data
    incremental_data <- intervention_subset %>%
      inner_join(
        baseline_for_icer %>%
          select(id, 
                 base_cost = total_cost, 
                 base_qaly = qaly_5yr, 
                 year),
        by = c("id", "year")
      ) %>%
      mutate(
        inc_cost = total_cost - base_cost,
        inc_qaly = qaly_5yr - base_qaly,
        nmb = wtp * inc_qaly - inc_cost
      )
    
    # Probability cost-effective = proportion where incremental NMB > 0
    prob_cost_effective <- mean(incremental_data$nmb > 0, na.rm = TRUE)
    
    # Bootstrap confidence intervals
    set.seed(123)  # For reproducibility
    boot_results <- boot(
      data = incremental_data,
      statistic = boot_icer_function,
      R = n_bootstrap,
      wtp = wtp
    )
    
    # Extract confidence intervals
    alpha <- (1 - ci_level) / 2
    
    # ICER CI (index 1)
    icer_lower <- quantile(boot_results$t[, 1], probs = alpha, na.rm = TRUE)
    icer_upper <- quantile(boot_results$t[, 1], probs = 1 - alpha, na.rm = TRUE)
    
    # NMB CI (index 2)
    nmb_lower <- quantile(boot_results$t[, 2], probs = alpha, na.rm = TRUE)
    nmb_upper <- quantile(boot_results$t[, 2], probs = 1 - alpha, na.rm = TRUE)
    
    # Incremental Cost CI (index 3)
    inc_cost_lower <- quantile(boot_results$t[, 3], probs = alpha, na.rm = TRUE)
    inc_cost_upper <- quantile(boot_results$t[, 3], probs = 1 - alpha, na.rm = TRUE)
    
    # Incremental QALY CI (index 4)
    inc_qaly_lower <- quantile(boot_results$t[, 4], probs = alpha, na.rm = TRUE)
    inc_qaly_upper <- quantile(boot_results$t[, 4], probs = 1 - alpha, na.rm = TRUE)
    
    # Determine cost-effectiveness based on incremental NMB
    cost_effective_decision <- case_when(
      is.na(icer) ~ "No QALY difference",
      incremental_qaly < 0 & incremental_cost > 0 ~ "Baseline Dominant (worse QoL, higher cost)",
      incremental_qaly > 0 & incremental_cost < 0 ~ "New FU Strategy Dominant (better QoL, lower cost)",
      incremental_nmb > 0 ~ "New FU Strategy Cost-effective",
      incremental_nmb <= 0 ~ "Not Cost-effective",
      TRUE ~ "Unclear"
    )
    
    # Store results
    result_row <- tibble(
      "Cohort Type" = cohort,
      "Iterations, n" = intervention_n,
      "Baseline, Mean Cost ± SD (£)" = paste0(
        round(baseline_mean_cost, 0), " ± ", round(baseline_mean_cost_sd, 0)
      ),
      "New FU Strategy, Mean Cost ± SD (£)" = paste0(
        round(intervention_mean_cost, 0), " ± ", round(intervention_mean_cost_sd, 0)
      ),
      "Incremental Cost (£)" = round(incremental_cost, 0),
      "Incremental Cost 95% CI" = paste0(
        "[", round(inc_cost_lower, 0), ", ", round(inc_cost_upper, 0), "]"
      ),
      "Baseline, Mean QALYs ± SD (EQ-5D)" = paste0(
        round(baseline_mean_qaly, 2), " ± ", round(baseline_mean_qaly_sd, 2)
      ),
      "New FU Strategy, Mean QALYs ± SD (EQ-5D)" = paste0(
        round(intervention_mean_qaly, 2), " ± ", round(intervention_mean_qaly_sd, 2)
      ),
      "Incremental QALYs" = round(incremental_qaly, 4),
      "Incremental QALYs 95% CI" = paste0(
        "[", round(inc_qaly_lower, 4), ", ", round(inc_qaly_upper, 4), "]"
      ),
      "ICER (£/QALY)" = ifelse(!is.na(icer), round(icer, 0), NA),
      "ICER 95% CI" = ifelse(
        !is.na(icer), 
        paste0("[", round(icer_lower, 0), ", ", round(icer_upper, 0), "]"),
        NA
      ),
      "Baseline NMB (£)" = round(baseline_nmb, 0),
      "New FU Strategy NMB (£)" = round(intervention_nmb, 0),
      "Incremental NMB (£)" = round(incremental_nmb, 0),
      "Incremental NMB 95% CI" = paste0(
        "[", round(nmb_lower, 0), ", ", round(nmb_upper, 0), "]"
      ),
      "Probability Cost-Effective" = round(prob_cost_effective, 2),
      "Cost-Effectiveness Decision" = cost_effective_decision
    )
    
    results <- rbind(results, result_row)
  }
  
  return(results)
}


### 12.5.2 AUC 0.55 ####
icer_0.55 <- calculate_icer(auc_0.55_for_icer)

### 12.5.3 AUC 0.6 ####
icer_0.6 <- calculate_icer(auc_0.6_for_icer)

### 12.5.4 AUC 0.65 ####
icer_0.65 <- calculate_icer(auc_0.65_for_icer)

### 12.5.5 AUC 0.7 ####
icer_0.7 <- calculate_icer(auc_0.7_for_icer)  

### 12.5.6 AUC 0.75 ####
icer_0.75 <- calculate_icer(auc_0.75_for_icer)

### 12.5.7 AUC 0.8 ####
icer_0.8 <- calculate_icer(auc_0.8_for_icer)

### 12.5.8 AUC 0.85 ####
icer_0.85 <- calculate_icer(auc_0.85_for_icer)

### 12.5.9 AUC 0.9 ####
icer_0.9 <- calculate_icer(auc_0.9_for_icer)

### 12.5.10 AUC 0.95 ####
icer_0.95 <- calculate_icer(auc_0.95_for_icer)

### 12.5.11 Tabulate all results ####
icer_results <- bind_rows(
  icer_0.55 %>% mutate(auc = 0.55),
  icer_0.6 %>% mutate(auc = 0.6),
  icer_0.65 %>% mutate(auc = 0.65),
  icer_0.7 %>% mutate(auc = 0.7),
  icer_0.75 %>% mutate(auc = 0.75),
  icer_0.8 %>% mutate(auc = 0.8),
  icer_0.85 %>% mutate(auc = 0.85),
  icer_0.9 %>% mutate(auc = 0.9),
  icer_0.95 %>% mutate(auc = 0.95)
) %>% select(auc, everything())

## 12.6 ICER - Compare Between all cohorts ####
### 12.6.1 Function to calculate ICER ####
calculate_all_icer <- function(intervention_data) {
  
  intervention_data$cohort_type <- as.factor(intervention_data$cohort_type)
  cohort_types <- levels(intervention_data$cohort_type)
  binary_comparisons <- combn(cohort_types, 2) %>%
    t() %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(c("group_1", "group_2"))
  wtp = 20000
  
  # Initialize results dataframe
  results <- data.frame()
  
  # Loop through each combination of cohort_type 
  for (i in seq_len(nrow(binary_comparisons))) {
    
    baseline_cohort     <- binary_comparisons$group_1[i] 
    intervention_cohort <- binary_comparisons$group_2[i]
    
    # Filter baseline data
    baseline_subset <- intervention_data %>%
      filter(cohort_type == baseline_cohort) %>%
      dplyr::select(
        id,
        year,
        cohort_type,
        qaly_5yr,
        total_cost
      )
    
    # Filter intervention data
    intervention_subset <- intervention_data %>%
      filter(cohort_type == intervention_cohort) %>%
      dplyr::select(
        id,
        year,
        cohort_type,
        qaly_5yr,
        total_cost
      )
    
    # Calculate means for baseline
    baseline_mean_cost <- mean(baseline_subset$total_cost, na.rm = TRUE)
    baseline_mean_qaly <- mean(baseline_subset$qaly_5yr, na.rm = TRUE)
    baseline_n <- nrow(baseline_subset)
    
    # Calculate means for intervention
    intervention_mean_cost <- mean(intervention_subset$total_cost, na.rm = TRUE)
    intervention_mean_qaly <- mean(intervention_subset$qaly_5yr, na.rm = TRUE)
    intervention_n <- nrow(intervention_subset)
    
    # Calculate incremental values
    incremental_cost <- intervention_mean_cost - baseline_mean_cost
    incremental_qaly <- intervention_mean_qaly - baseline_mean_qaly
    
    # Calculate ICER (handle division by zero)
    if (incremental_qaly != 0) {
      icer <- incremental_cost / incremental_qaly
    } else {
      icer <- NA 
    }
    
    # Calculate incremental NMB using mean values
    incremental_nmb <- wtp * incremental_qaly - incremental_cost
    
    # Calculate NMB for each group
    baseline_nmb <- wtp * baseline_mean_qaly - baseline_mean_cost
    intervention_nmb <- wtp * intervention_mean_qaly - intervention_mean_cost
    
    # Calculate probability of cost-effectiveness using paired data
    incremental_data <- intervention_subset %>%
      inner_join(
        baseline_subset %>%
          select(id, 
                 base_cost = total_cost, 
                 base_qaly = qaly_5yr, 
                 year),
        by = c("id", "year")
      ) %>%
      mutate(
        inc_cost = total_cost - base_cost,
        inc_qaly = qaly_5yr - base_qaly,
        nmb = wtp * inc_qaly - inc_cost
      )
    
    # Probability cost-effective = proportion where incremental NMB > 0
    prob_cost_effective <- mean(incremental_data$nmb > 0, na.rm = TRUE)
    
    # Determine cost-effectiveness
    cost_effective_decision <- case_when(
      is.na(icer) ~ "No QALY difference",
      incremental_qaly < 0 & incremental_cost > 0 ~ "Group 1 Dominant",
      incremental_qaly > 0 & incremental_cost < 0 ~ "Group 2 Dominant",
      incremental_nmb > 0 ~ "Group 2 Cost-effective",
      incremental_nmb <= 0 ~ "Not Cost-effective",
      TRUE ~ "Unclear"
    )
    
    # Store results
    result_row <- tibble(
      "Group 1" = baseline_cohort,
      "Group 2" = intervention_cohort,
      "Group 1, n" = baseline_n,
      "Group 2, n" = intervention_n,
      "Group 1 Mean Cost (£)" = round(baseline_mean_cost, 2),
      "Group 2 Mean Cost (£)" = round(intervention_mean_cost, 2),
      "Incremental Cost (£)" = round(incremental_cost, 2),
      "Group 1 Mean QALY" = round(baseline_mean_qaly, 4),
      "Group 2 Mean QALY" = round(intervention_mean_qaly, 4),
      "Incremental QALY" = round(incremental_qaly, 4),
      "ICER (£/QALY)" = round(icer, 0),
      "Group 1 NMB (£)" = round(baseline_nmb, 0),
      "Group 2 NMB (£)" = round(intervention_nmb, 0),
      "Incremental NMB (£)" = round(incremental_nmb, 0),
      "Probability Cost-Effective" = round(prob_cost_effective, 2),
      "Cost-Effectiveness Decision" = cost_effective_decision
    )
    
    results <- rbind(results, result_row)
  }
  
  return(results)
}


### 12.6.2 AUC 0.55 ####
icer_all_0.55 <- calculate_all_icer(auc_0.55_for_icer)

### 12.6.3 AUC 0.6 ####
icer_all_0.6 <- calculate_all_icer(auc_0.6_for_icer)

### 12.6.4 AUC 0.65 ####
icer_all_0.65 <- calculate_all_icer(auc_0.65_for_icer)

### 12.6.5 AUC 0.7 ####
icer_all_0.7 <- calculate_all_icer(auc_0.7_for_icer)


### 12.6.6 AUC 0.75 ####
icer_all_0.75 <- calculate_all_icer(auc_0.75_for_icer)


### 12.6.7 AUC 0.8 ####
icer_all_0.8 <- calculate_all_icer(auc_0.8_for_icer)


### 12.6.8 AUC 0.85 ####
icer_all_0.85 <- calculate_all_icer(auc_0.85_for_icer)


### 12.6.9 AUC 0.9 ####
icer_all_0.9 <- calculate_all_icer(auc_0.9_for_icer)


### 12.6.10 AUC 0.95 ####
icer_all_0.95 <- calculate_all_icer(auc_0.95_for_icer)

### 12.6.11 Tabulate all results ####
icer_all_results <- bind_rows(
  (icer_all_0.55 %>% mutate("auc" = 0.55)),
  (icer_all_0.6 %>% mutate("auc" = 0.6)),
  (icer_all_0.65 %>% mutate("auc" = 0.65)),
  (icer_all_0.7 %>% mutate("auc" = 0.7)),
  (icer_all_0.75 %>% mutate("auc" = 0.75)),
  (icer_all_0.8 %>% mutate("auc" = 0.8)),
  (icer_all_0.85 %>% mutate("auc" = 0.85)),
  (icer_all_0.9 %>% mutate("auc" = 0.9)),
  (icer_all_0.95 %>% mutate("auc" = 0.95))
) %>%
  select(auc, everything())

## 12.7 Scatter plot and Cost-effectiveness Acceptability Curves ####
### 12.7.1 Function to Generate Scatter plot ####
plot_cost_effectiveness <- function(
    data,
    baseline_label = "Minimum FU, XR + US",
    wtp = 20000,
    wtp_range = seq(0, 50000, by = 100),
    sample_n = 100000,
    color_palette = NULL  
) {
  
  # Ensure cohort_type is factor with consistent levels across ALL data
  all_cohort_levels <- c(
    "Minimum FU, XR + US",
    "Maximum FU, XR + US",
    "Minimum FU, US",
    "Maximum FU, US",
    "Minimum FU, CT",
    "Maximum FU, CT"
  )
  
  data <- data %>%
    mutate(cohort_type = factor(cohort_type, levels = all_cohort_levels))
  
  # Baseline data
  baseline_data <- baseline_for_icer
  
  # Incremental data (excluding baseline)
  incremental_cohort_levels <- all_cohort_levels
  
  incremental_data <- data %>%
    inner_join(
      baseline_data %>%
        select(id, base_cost = total_cost, base_qaly = qaly_5yr, year),
      by = c("id", "year")
    ) %>%
    mutate(
      inc_cost = total_cost - base_cost,
      inc_qaly = qaly_5yr - base_qaly,
      cohort_type = factor(cohort_type, levels = incremental_cohort_levels)
    )
  
  # Create consistent color palette
  if (is.null(color_palette)) {
    n_cohorts <- length(incremental_cohort_levels)
    color_palette <- scales::hue_pal()(n_cohorts)
    names(color_palette) <- incremental_cohort_levels
  }
  
  # Summary table
  summary_table <- incremental_data %>%
    group_by(cohort_type) %>%
    summarise(
      baseline_n = n_distinct(id),
      intervention_n = n(),
      incremental_cost = mean(inc_cost, na.rm = TRUE),
      incremental_qaly = mean(inc_qaly, na.rm = TRUE),
      icer = incremental_cost / incremental_qaly,
      incremental_nmb = wtp * incremental_qaly - incremental_cost,
      prob_cost_effective = mean(
        (wtp * inc_qaly - inc_cost) > 0,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    mutate(
      ce_decision = case_when(
        incremental_qaly < 0 & incremental_cost > 0 ~ "Baseline Dominates",
        incremental_qaly > 0 & incremental_cost < 0 ~ "Novel FU Dominates",
        incremental_nmb > 0 ~ "Novel FU Cost-effective",
        TRUE ~ "Not cost-effective"
      )
    )
  
  # CE plane
  ce_plane <- incremental_data %>% 
    slice_sample(n = sample_n) %>%
    ggplot(aes(x = inc_qaly, y = inc_cost, colour = cohort_type)) +
    facet_wrap(. ~ cohort_type, drop = FALSE) +
    geom_point(alpha = 0.15, size = 0.6) +
    geom_hline(yintercept = 0, linetype = "solid", color = "gray50") +
    geom_vline(xintercept = 0, linetype = "solid", color = "gray50") +
    geom_abline(slope = wtp, intercept = 0, linetype = "dashed", color = "black") + 
    scale_colour_manual(
      values = color_palette,
      drop = FALSE,
      name = "Cohort Type"
    ) +
    labs(
      x = "Incremental QALYs",
      y = "Incremental Costs (£)"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none"
    ) + ylim(c(-1000,1000)) + xlim(c(-1.5, 1.5)) + guides(colour = "none")
  
  # CEAC
  incremental_data1 <- incremental_data %>%
    slice_sample(n = sample_n)
  
  ceac <- expand_grid(
    cohort_type = incremental_cohort_levels,
    wtp = wtp_range
  ) %>%
    mutate(cohort_type = factor(cohort_type, levels = incremental_cohort_levels)) %>%
    left_join(incremental_data1, by = "cohort_type") %>%
    mutate(nmb = wtp * inc_qaly - inc_cost) %>%
    group_by(cohort_type, wtp) %>%
    summarise(
      prob_ce = mean(nmb > 0, na.rm = TRUE),
      .groups = "drop"
    )
  
  ceac_plot <- ggplot(
    ceac,
    aes(x = wtp, y = prob_ce, colour = cohort_type)
  ) +
    geom_line(linewidth = 1) +
    geom_vline(xintercept = wtp, linetype = "dashed", color = "gray50") +
    scale_colour_manual(
      values = color_palette,
      drop = FALSE,
      name = "Cohort Type"
    ) +
    labs(
      x = "Willingness-to-pay (£/QALY)",
      y = "Probability of Cost-Effectiveness",
      title = paste0("AUC ", unique(incremental_data$auc_target))
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    ) + xlim(c(0,30000))
  
  # Return
  list(
    summary_table = summary_table,
    ce_plane = ce_plane,
    ceac = ceac_plot,
    incremental_data = incremental_data,
    color_palette = color_palette  
  )
}


### 12.7.2 AUC 0.55 ####
icer_0.55_plots <- plot_cost_effectiveness(auc_0.55_for_icer)
icer_0.55_plots$summary_table %>% gt()
auc_0.55_icer_plots <- icer_0.55_plots$ce_plane + 
  icer_0.55_plots$ceac + 
  plot_annotation(tag_levels = "A") 
auc_0.55_icer_plots & theme_bw()

### 12.7.3 AUC 0.6 ####
icer_0.6_plots <- plot_cost_effectiveness(auc_0.6_for_icer)
icer_0.6_plots$summary_table %>% gt()
auc_0.6_icer_plots <- icer_0.6_plots$ce_plane + 
  icer_0.6_plots$ceac + 
  plot_annotation(tag_levels = "A") 
auc_0.6_icer_plots & theme_bw()

### 12.7.4 AUC 0.65 ####
icer_0.65_plots <- plot_cost_effectiveness(auc_0.65_for_icer)
icer_0.65_plots$summary_table %>% gt()
auc_0.65_icer_plots <- icer_0.65_plots$ce_plane + 
  icer_0.65_plots$ceac + 
  plot_annotation(tag_levels = "A") 
auc_0.65_icer_plots & theme_bw()

### 12.7.5 AUC 0.7 ####
icer_0.7_plots <- plot_cost_effectiveness(auc_0.7_for_icer)
icer_0.7_plots$summary_table %>% gt()
auc_0.7_icer_plots <- icer_0.7_plots$ce_plane + 
  icer_0.7_plots$ceac + 
  plot_annotation(tag_levels = "A") 
auc_0.7_icer_plots & theme_bw()

### 12.7.6 AUC 0.75 ####
icer_0.75_plots <- plot_cost_effectiveness(auc_0.75_for_icer)
icer_0.75_plots$summary_table %>% gt()
auc_0.75_icer_plots <- icer_0.75_plots$ce_plane + 
  icer_0.75_plots$ceac + 
  plot_annotation(tag_levels = "A") 
auc_0.75_icer_plots & theme_bw()

### 12.7.7 AUC 0.8 ####
icer_0.8_plots <- plot_cost_effectiveness(auc_0.8_for_icer)
icer_0.8_plots$summary_table %>% gt()
auc_0.8_icer_plots <- icer_0.8_plots$ce_plane + 
  icer_0.8_plots$ceac + 
  plot_annotation(tag_levels = "A") 
auc_0.8_icer_plots & theme_bw()

### 12.7.8 AUC 0.85 ####
icer_0.85_plots <- plot_cost_effectiveness(auc_0.85_for_icer)
icer_0.85_plots$summary_table %>% gt()
auc_0.85_icer_plots <- icer_0.85_plots$ce_plane + 
  icer_0.85_plots$ceac + 
  plot_annotation(tag_levels = "A") 
auc_0.85_icer_plots & theme_bw()

### 12.7.9 AUC 0.9 ####
icer_0.9_plots <- plot_cost_effectiveness(auc_0.9_for_icer)
icer_0.9_plots$summary_table %>% gt()
auc_0.9_icer_plots <- icer_0.9_plots$ce_plane + 
  icer_0.9_plots$ceac + 
  plot_annotation(tag_levels = "A") 
auc_0.9_icer_plots & theme_bw()

### 12.7.10 AUC 0.9 ####
icer_0.95_plots <- plot_cost_effectiveness(auc_0.95_for_icer)
icer_0.95_plots$summary_table %>% gt()
auc_0.95_icer_plots <- icer_0.95_plots$ce_plane + 
  icer_0.95_plots$ceac + 
  plot_annotation(tag_levels = "A") 
auc_0.95_icer_plots & theme_bw()
