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
        total_cost = sum(c_across(starts_with("cost_year_")))
      )
    
    cohort[[i]] <- data1
  }
  return(cohort)
}

### 12.2.2 2016 ####
costs_2016_xr_min_stripped <- strip_out_costs(costs_2016_xr_min)

costs_2016_xr_us_min_stripped <- strip_out_costs(costs_2016_xr_us_min)

costs_2016_ct_min_stripped <- strip_out_costs(costs_2016_ct_min)

costs_2016_xr_max_stripped <- strip_out_costs(costs_2016_xr_max)

costs_2016_xr_us_max_stripped <- strip_out_costs(costs_2016_xr_us_max)

costs_2016_ct_max_stripped <- strip_out_costs(costs_2016_ct_max)

### 12.2.3 2017 ####
costs_2017_xr_min_stripped <- strip_out_costs(costs_2017_xr_min)

costs_2017_xr_us_min_stripped <- strip_out_costs(costs_2017_xr_us_min)

costs_2017_ct_min_stripped <- strip_out_costs(costs_2017_ct_min)

costs_2017_xr_max_stripped <- strip_out_costs(costs_2017_xr_max)

costs_2017_xr_us_max_stripped <- strip_out_costs(costs_2017_xr_us_max)

costs_2017_ct_max_stripped <- strip_out_costs(costs_2017_ct_max)

### 12.2.4 2018 ####
costs_2018_xr_min_stripped <- strip_out_costs(costs_2018_xr_min)

costs_2018_xr_us_min_stripped <- strip_out_costs(costs_2018_xr_us_min)

costs_2018_ct_min_stripped <- strip_out_costs(costs_2018_ct_min)

costs_2018_xr_max_stripped <- strip_out_costs(costs_2018_xr_max)

costs_2018_xr_us_max_stripped <- strip_out_costs(costs_2018_xr_us_max)

costs_2018_ct_max_stripped <- strip_out_costs(costs_2018_ct_max)

### 12.2.5 2019 ####
costs_2019_xr_min_stripped <- strip_out_costs(costs_2019_xr_min)

costs_2019_xr_us_min_stripped <- strip_out_costs(costs_2019_xr_us_min)

costs_2019_ct_min_stripped <- strip_out_costs(costs_2019_ct_min)

costs_2019_xr_max_stripped <- strip_out_costs(costs_2019_xr_max)

costs_2019_xr_us_max_stripped <- strip_out_costs(costs_2019_xr_us_max)

costs_2019_ct_max_stripped <- strip_out_costs(costs_2019_ct_max)

### 12.2.6 2020 ####
costs_2020_xr_min_stripped <- strip_out_costs(costs_2020_xr_min)

costs_2020_xr_us_min_stripped <- strip_out_costs(costs_2020_xr_us_min)

costs_2020_ct_min_stripped <- strip_out_costs(costs_2020_ct_min)

costs_2020_xr_max_stripped <- strip_out_costs(costs_2020_xr_max)

costs_2020_xr_us_max_stripped <- strip_out_costs(costs_2020_xr_us_max)

costs_2020_ct_max_stripped <- strip_out_costs(costs_2020_ct_max)

## 12.3 Sort out QoL ####
### 12.3.1 Function to strip out auc, id and qol ####
strip_out_qol <- function(data) {
  
  cohort <- list()
  
  for (i in 1:9){
    
    data1 <- data[[i]] %>% 
      subset(select = c(
        id,
        auc_target,
        baseline_qol_mean,
        qol_mean_year_1,
        qol_mean_year_2,
        qol_mean_year_3,
        qol_mean_year_4,
        qol_mean_year_5,
        qaly_5yr,
        year
      )) %>%
      mutate(
        qol_mean_year_0 = baseline_qol_mean,
        mean_qol = mean(c_across(starts_with("qol_mean_year_")))
      )
    
    cohort[[i]] <- data1
  }
  return(cohort)
}


### 12.3.2 2016 ####
qol_2016_xr_min_stripped <- strip_out_qol(qol_2016_xr_min)

qol_2016_xr_us_min_stripped <- strip_out_qol(qol_2016_xr_us_min)

qol_2016_ct_min_stripped <- strip_out_qol(qol_2016_ct_min)

qol_2016_xr_max_stripped <- strip_out_qol(qol_2016_xr_max)

qol_2016_xr_us_max_stripped <- strip_out_qol(qol_2016_xr_us_max)

qol_2016_ct_max_stripped <- strip_out_qol(qol_2016_ct_max)

### 12.3.3 2017 ####
qol_2017_xr_min_stripped <- strip_out_qol(qol_2017_xr_min)

qol_2017_xr_us_min_stripped <- strip_out_qol(qol_2017_xr_us_min)

qol_2017_ct_min_stripped <- strip_out_qol(qol_2017_ct_min)

qol_2017_xr_max_stripped <- strip_out_qol(qol_2017_xr_max)

qol_2017_xr_us_max_stripped <- strip_out_qol(qol_2017_xr_us_max)

qol_2017_ct_max_stripped <- strip_out_qol(qol_2017_ct_max)

### 12.3.4 2018 ####
qol_2018_xr_min_stripped <- strip_out_qol(qol_2018_xr_min)

qol_2018_xr_us_min_stripped <- strip_out_qol(qol_2018_xr_us_min)

qol_2018_ct_min_stripped <- strip_out_qol(qol_2018_ct_min)

qol_2018_xr_max_stripped <- strip_out_qol(qol_2018_xr_max)

qol_2018_xr_us_max_stripped <- strip_out_qol(qol_2018_xr_us_max)

qol_2018_ct_max_stripped <- strip_out_qol(qol_2018_ct_max)

### 12.3.5 2019 ####
qol_2019_xr_min_stripped <- strip_out_qol(qol_2019_xr_min)

qol_2019_xr_us_min_stripped <- strip_out_qol(qol_2019_xr_us_min)

qol_2019_ct_min_stripped <- strip_out_qol(qol_2019_ct_min)

qol_2019_xr_max_stripped <- strip_out_qol(qol_2019_xr_max)

qol_2019_xr_us_max_stripped <- strip_out_qol(qol_2019_xr_us_max)

qol_2019_ct_max_stripped <- strip_out_qol(qol_2019_ct_max)

### 12.3.6 2020 ####
qol_2020_xr_min_stripped <- strip_out_qol(qol_2020_xr_min)

qol_2020_xr_us_min_stripped <- strip_out_qol(qol_2020_xr_us_min)

qol_2020_ct_min_stripped <- strip_out_qol(qol_2020_ct_min)

qol_2020_xr_max_stripped <- strip_out_qol(qol_2020_xr_max)

qol_2020_xr_us_max_stripped <- strip_out_qol(qol_2020_xr_us_max)

qol_2020_ct_max_stripped <- strip_out_qol(qol_2020_ct_max)

## 12.4 Combine QALY data with Costs ####
### 12.4.1 Aggregation function ####
aggregate_appt_cohorts <- function(auc_target = c(1,2,3,4,5,6,7,8,9),
                                   outcome_type = qol) {
  all_cohorts <- list()
  
  for (i in auc_target) {
    key <- as.integer(i)
    message("Processing AUC = ", key)
    
    message("  Loading 2016 data...")
    cohort_2016_min_xr <- paste0(outcome_type, "_2016_xr_min_stripped")[[key]] %>% mutate(cohort_type = "Minimum FU, XR", auc = i)
    cohort_2016_min_ct <- paste0(outcome_type, "_2016_ct_min_stripped")[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2016_max_xr <- paste0(outcome_type, "_2016_xr_max_stripped")[[key]] %>% mutate(cohort_type = "Maximum FU, XR", auc = i)
    cohort_2016_max_ct <- paste0(outcome_type, "_2016_ct_max_stripped")[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    cohort_2016_min_xr_us <- paste0(outcome_type, "_2016_xr_us_min_stripped")[[key]] %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2016_max_xr_us <- paste0(outcome_type, "_2016_xr_us_max_stripped")[[key]] %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    
    message("  Loading 2017 data...")
    cohort_2017_min_xr <- paste0(outcome_type, "_2017_xr_min_stripped")[[key]] %>% mutate(cohort_type = "Minimum FU, XR", auc = i)
    cohort_2017_min_ct <- paste0(outcome_type, "_2017_ct_min_stripped")[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2017_max_xr <- paste0(outcome_type, "_2017_xr_max_stripped")[[key]] %>% mutate(cohort_type = "Maximum FU, XR", auc = i)
    cohort_2017_max_ct <- paste0(outcome_type, "_2017_ct_max_stripped")[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    cohort_2017_min_xr_us <- paste0(outcome_type, "_2017_xr_us_min_stripped")[[key]] %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2017_max_xr_us <- paste0(outcome_type, "_2017_xr_us_max_stripped")[[key]] %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    
    message("  Loading 2018 data...")
    cohort_2018_min_xr <- paste0(outcome_type, "_2018_xr_min_stripped")[[key]] %>% mutate(cohort_type = "Minimum FU, XR", auc = i)
    cohort_2018_min_ct <- paste0(outcome_type, "_2018_ct_min_stripped")[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2018_max_xr <- paste0(outcome_type, "_2018_xr_max_stripped")[[key]] %>% mutate(cohort_type = "Maximum FU, XR", auc = i)
    cohort_2018_max_ct <- paste0(outcome_type, "_2018_ct_max_stripped")[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    cohort_2018_min_xr_us <- paste0(outcome_type, "_2018_xr_us_min_stripped")[[key]] %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2018_max_xr_us <- paste0(outcome_type, "_2018_xr_us_max_stripped")[[key]] %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    
    message("  Loading 2019 data...")
    cohort_2019_min_xr <- paste0(outcome_type, "_2019_xr_min_stripped")[[key]] %>% mutate(cohort_type = "Minimum FU, XR", auc = i)
    cohort_2019_min_ct <- paste0(outcome_type, "_2019_ct_min_stripped")[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2019_max_xr <- paste0(outcome_type, "_2019_xr_max_stripped")[[key]] %>% mutate(cohort_type = "Maximum FU, XR", auc = i)
    cohort_2019_max_ct <- paste0(outcome_type, "_2019_ct_max_stripped")[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    cohort_2019_min_xr_us <- paste0(outcome_type, "_2019_xr_us_min_stripped")[[key]] %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2019_max_xr_us <- paste0(outcome_type, "_2019_xr_us_max_stripped")[[key]] %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    
    message("  Loading 2020 data...")
    cohort_2020_min_xr <- paste0(outcome_type, "_2020_xr_min_stripped")[[key]] %>% mutate(cohort_type = "Minimum FU, XR", auc = i)
    cohort_2020_min_ct <- paste0(outcome_type, "_2020_ct_min_stripped")[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2020_max_xr <- paste0(outcome_type, "_2020_xr_max_stripped")[[key]] %>% mutate(cohort_type = "Maximum FU, XR", auc = i)
    cohort_2020_max_ct <- paste0(outcome_type, "_2020_ct_max_stripped")[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    cohort_2020_min_xr_us <- paste0(outcome_type, "_2020_xr_us_min_stripped")[[key]] %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2020_max_xr_us <- paste0(outcome_type, "_2020_xr_us_max_stripped")[[key]] %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    
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

### 12.4.2 Aggregate cohorts ####
auc_0.55_for_icer <- 

### 12.4.1 Set baseline for comparison => AUC 0.55, XR FU, Min FU ####
baseline_for_icer <- 
