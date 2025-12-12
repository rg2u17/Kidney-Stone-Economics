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
        cost_year_5 = ifelse(is.na(cost_year_5),
                             0,
                             cost_year_5),
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
        qol_lower_year_1,
        qol_lower_year_2,
        qol_lower_year_3,
        qol_lower_year_4,
        qol_lower_year_5,
        qaly_5yr,
        qaly_5yr_lower,
        qaly_5yr_upper,
        year
      )) %>%
      mutate(
        qol_mean_year_0 = baseline_qol_mean,
        mean_qol = mean(c_across(starts_with("qol_mean_year_"))),
        qaly_5yr_se = (qaly_5yr - qaly_5yr_lower)/1.96,
        qol_se_year_1 = (qol_mean_year_1 - qol_lower_year_1)/1.96,
        qol_se_year_2 = (qol_mean_year_2 - qol_lower_year_2)/1.96,
        qol_se_year_3 = (qol_mean_year_3 - qol_lower_year_3)/1.96,
        qol_se_year_4 = (qol_mean_year_4 - qol_lower_year_4)/1.96,
        qol_se_year_5 = (qol_mean_year_5 - qol_lower_year_5)/1.96,
        .keep = "all"
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
aggregate_qol_and_cost_cohorts <- function(auc_target = c(1,2,3,4,5,6,7,8,9)) {
  all_cohorts <- list()
  
  for (i in auc_target) {
    key <- as.integer(i)
    message("Processing AUC = ", key)
    
    message("  Loading 2016 data...")
    cohort_2016_min_xr <- left_join(qol_2016_xr_min_stripped[[key]],
                                    costs_2016_xr_min_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Minimum FU, XR", auc = i)
    cohort_2016_min_ct <- left_join(qol_2016_ct_min_stripped[[key]],
                                    costs_2016_ct_min_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2016_min_xr_us <- left_join(qol_2016_xr_us_min_stripped[[key]],
                                    costs_2016_xr_us_min_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2016_max_xr <- left_join(qol_2016_xr_max_stripped[[key]],
                                    costs_2016_xr_max_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Maximum FU, XR", auc = i)
    cohort_2016_max_ct <- left_join(qol_2016_ct_max_stripped[[key]],
                                    costs_2016_ct_max_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    cohort_2016_max_xr_us <- left_join(qol_2016_xr_us_max_stripped[[key]],
                                    costs_2016_xr_us_max_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    
    message("  Loading 2017 data...")
    cohort_2017_min_xr <- left_join(qol_2017_xr_min_stripped[[key]],
                                    costs_2017_xr_min_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Minimum FU, XR", auc = i)
    cohort_2017_min_ct <- left_join(qol_2017_ct_min_stripped[[key]],
                                    costs_2017_ct_min_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2017_min_xr_us <- left_join(qol_2017_xr_us_min_stripped[[key]],
                                       costs_2017_xr_us_min_stripped[[key]],
                                       by = "id") %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2017_max_xr <- left_join(qol_2017_xr_max_stripped[[key]],
                                    costs_2017_xr_max_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Maximum FU, XR", auc = i)
    cohort_2017_max_ct <- left_join(qol_2017_ct_max_stripped[[key]],
                                    costs_2017_ct_max_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    cohort_2017_max_xr_us <- left_join(qol_2017_xr_us_max_stripped[[key]],
                                       costs_2017_xr_us_max_stripped[[key]],
                                       by = "id") %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    
    message("  Loading 2018 data...")
    cohort_2018_min_xr <- left_join(qol_2018_xr_min_stripped[[key]],
                                    costs_2018_xr_min_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Minimum FU, XR", auc = i)
    cohort_2018_min_ct <- left_join(qol_2018_ct_min_stripped[[key]],
                                    costs_2018_ct_min_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2018_min_xr_us <- left_join(qol_2018_xr_us_min_stripped[[key]],
                                       costs_2018_xr_us_min_stripped[[key]],
                                       by = "id") %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2018_max_xr <- left_join(qol_2018_xr_max_stripped[[key]],
                                    costs_2018_xr_max_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Maximum FU, XR", auc = i)
    cohort_2018_max_ct <- left_join(qol_2018_ct_max_stripped[[key]],
                                    costs_2018_ct_max_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    cohort_2018_max_xr_us <- left_join(qol_2018_xr_us_max_stripped[[key]],
                                       costs_2018_xr_us_max_stripped[[key]],
                                       by = "id") %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    
    message("  Loading 2019 data...")
    cohort_2019_min_xr <- left_join(qol_2019_xr_min_stripped[[key]],
                                    costs_2019_xr_min_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Minimum FU, XR", auc = i)
    cohort_2019_min_ct <- left_join(qol_2019_ct_min_stripped[[key]],
                                    costs_2019_ct_min_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2019_min_xr_us <- left_join(qol_2019_xr_us_min_stripped[[key]],
                                       costs_2019_xr_us_min_stripped[[key]],
                                       by = "id") %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2019_max_xr <- left_join(qol_2019_xr_max_stripped[[key]],
                                    costs_2019_xr_max_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Maximum FU, XR", auc = i)
    cohort_2019_max_ct <- left_join(qol_2019_ct_max_stripped[[key]],
                                    costs_2019_ct_max_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    cohort_2019_max_xr_us <- left_join(qol_2019_xr_us_max_stripped[[key]],
                                       costs_2019_xr_us_max_stripped[[key]],
                                       by = "id") %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    message("  Loading 2020 data...")
    cohort_2020_min_xr <- left_join(qol_2020_xr_min_stripped[[key]],
                                    costs_2020_xr_min_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Minimum FU, XR", auc = i)
    cohort_2020_min_ct <- left_join(qol_2020_ct_min_stripped[[key]],
                                    costs_2020_ct_min_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2020_min_xr_us <- left_join(qol_2020_xr_us_min_stripped[[key]],
                                       costs_2020_xr_us_min_stripped[[key]],
                                       by = "id") %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2020_max_xr <- left_join(qol_2020_xr_max_stripped[[key]],
                                    costs_2020_xr_max_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Maximum FU, XR", auc = i)
    cohort_2020_max_ct <- left_join(qol_2020_ct_max_stripped[[key]],
                                    costs_2020_ct_max_stripped[[key]],
                                    by = "id") %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    cohort_2020_max_xr_us <- left_join(qol_2020_xr_us_max_stripped[[key]],
                                       costs_2020_xr_us_max_stripped[[key]],
                                       by = "id") %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
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

### 12.4.3 Set baseline for comparison => AUC 0.55, XR FU, Min FU ####
baseline_for_icer <- auc_0.55_for_icer %>% filter(cohort_type == "Minimum FU, XR")
  
  
## 12.5 Calculate ICER ####  
### 12.5.1 Function to get cost/qaly differences ####
#### 12.5.1.1 Helper functions ####
get_annual <- function(df) {
  df %>%
    mutate(
      annual_cost = dplyr::case_when(
        year == 2016 ~ cost_year_5,
        year == 2017 ~ cost_year_4,
        year == 2018 ~ cost_year_3,
        year == 2019 ~ cost_year_2,
        year == 2020 ~ cost_year_1
      ),
      annual_qaly = dplyr::case_when(
        year == 2016 ~ qol_mean_year_5,
        year == 2017 ~ qol_mean_year_4,
        year == 2018 ~ qol_mean_year_3,
        year == 2019 ~ qol_mean_year_2,
        year == 2020 ~ qol_mean_year_1
      ) / 60,
      annual_se = dplyr::case_when(
        year == 2016 ~ qol_se_year_5,
        year == 2017 ~ qol_se_year_4,
        year == 2018 ~ qol_se_year_3,
        year == 2019 ~ qol_se_year_2,
        year == 2020 ~ qol_se_year_1
      ) / 60
    ) %>%
    summarise(
      mean_cost = mean(annual_cost, na.rm = TRUE),
      mean_qaly = mean(annual_qaly, na.rm = TRUE),
      se_qaly   = mean(annual_se, na.rm = TRUE)
    )
}

run_single_iteration <- function(iter, baseline_data, intervention_data) {
  
  baseline_sample <- baseline_data[sample.int(nrow(baseline_data), nrow(baseline_data), TRUE), ]
  intervention_sample <- intervention_data[sample.int(nrow(intervention_data), nrow(intervention_data), TRUE), ]
  
  baseline_vals <- get_annual(baseline_sample)
  intervention_vals <- get_annual(intervention_sample)
  
  qaly_baseline <- rnorm(1, baseline_vals$mean_qaly, baseline_vals$se_qaly)
  qaly_intervention <- rnorm(1, intervention_vals$mean_qaly, intervention_vals$se_qaly)
  
  cost_diff <- intervention_vals$mean_cost - baseline_vals$mean_cost
  qaly_diff <- qaly_intervention - qaly_baseline
  
  tibble(
    cost_diff = cost_diff,
    qaly_diff = qaly_diff,
    icer      = cost_diff / qaly_diff
  )
}


#### 12.5.5.2 Main MC simulation for ICER calculation ####
monte_carlo_icer_parallel <- function(data,
                                      baseline_cohort = "Minimum FU, XR",
                                      n_iterations = 1000,
                                      n_cores = parallel::detectCores() - 1) {
  
  set.seed(1234)
  
  plan(multisession, workers = n_cores)
  message("Using ", n_cores, " cores for parallel processing")
  
  auc_data <- data %>% mutate(cost_year_5 = ifelse(is.na(cost_year_5), 0, cost_year_5))
  
  baseline_data <- baseline_for_icer %>%
    filter(cohort_type == baseline_cohort) %>%
    mutate(cost_year_5 = ifelse(is.na(cost_year_5), 0, cost_year_5))
  
  comparison_cohorts <- if (unique(auc_data$auc_target)[1] == 0.55) {
    unique(auc_data$cohort_type[auc_data$cohort_type != baseline_cohort])
  } else {
    unique(auc_data$cohort_type)
  }
  
  all_results <- purrr::map_dfr(comparison_cohorts, function(cohort) {
    
    message("\n📌 Starting parallel simulations for cohort: ", cohort)
    
    intervention_data <- auc_data %>% filter(cohort_type == cohort)
    
    results_df <- future_map2_dfr(
      1:n_iterations,
      1:n_iterations,    # dummy second argument
      ~ run_single_iteration(.x, baseline_data, intervention_data),
      .progress = TRUE,
      .options = furrr::furrr_options(seed = TRUE)
    )

    results_df <- results_df %>%
      mutate(
        cohort_type = cohort,
        iteration = 1:n_iterations
      ) %>%
      select(cohort_type, iteration, cost_diff, qaly_diff, icer)
    
    # summary logs
    message("\n✅ Finished cohort: ", cohort)
    finite_icers <- results_df$icer[is.finite(results_df$icer)]
    message("   → Mean ICER: £", format(round(mean(finite_icers), 0), big.mark = ","))
    message("   → Median ICER: £", format(round(median(finite_icers), 0), big.mark = ","))
    message("   → 95% CI: [£",
            format(round(quantile(finite_icers, 0.025), 0), big.mark = ","),
            " to £",
            format(round(quantile(finite_icers, 0.975), 0), big.mark = ","), "]")
    
    results_df
  })
  
  plan(sequential)
  message("\n🎉 All simulations completed successfully.")
  
  all_results
}

### 12.5.5 Compare baseline against other FU lengths/modalities ####
wtp <- 20000

#### 12.5.5.1 AUC = 0.55 ####
icer_0.55 <- monte_carlo_icer_parallel(auc_0.55_for_icer, 
                                       n_iterations = 1000)


icer_0.55$cohort_type <- as.factor(icer_0.55$cohort_type)

#### 12.5.5.1 AUC = 0.6 ####
icer_0.6 <- monte_carlo_icer_parallel(
  auc_0.6_for_icer,
  n_iterations = 1000
)

icer_0.6$cohort_type <- as.factor(icer_0.6$cohort_type)

#### 12.5.5.3 AUC = 0.65 ####
icer_0.65 <- monte_carlo_icer_parallel(
  auc_0.65_for_icer,
  n_iterations = 1000
)
icer_0.65$cohort_type <- as.factor(icer_0.65$cohort_type)

#### 12.5.5.4 AUC = 0.7 ####
icer_0.7 <- monte_carlo_icer_parallel(
  auc_0.7_for_icer,
  n_iterations = 1000
)

icer_0.7$cohort_type <- as.factor(icer_0.7$cohort_type)

#### 12.5.5.5 AUC = 0.75 ####
icer_0.75 <- monte_carlo_icer_parallel(
  auc_0.75_for_icer,
  n_iterations = 1000
)
icer_0.75$cohort_type <- as.factor(icer_0.75$cohort_type)

#### 12.5.5.6 AUC = 0.8 ####
icer_0.8 <- monte_carlo_icer_parallel(
  auc_0.8_for_icer,
  n_iterations = 1000
)

icer_0.8$cohort_type <- as.factor(icer_0.8$cohort_type)

#### 12.5.5.7 AUC = 0.85 ####
icer_0.85 <- monte_carlo_icer_parallel(
  auc_0.85_for_icer,
  n_iterations = 1000
)
icer_0.85$cohort_type <- as.factor(icer_0.85$cohort_type)

#### 12.5.5.8 AUC = 0.9 ####
icer_0.9 <- monte_carlo_icer_parallel(
  auc_0.9_for_icer,
  n_iterations = 1000
)

icer_0.9$cohort_type <- as.factor(icer_0.9$cohort_type)

#### 12.5.5.9 AUC = 0.95 ####
icer_0.95 <- monte_carlo_icer_parallel(
  auc_0.95_for_icer,
  n_iterations = 1000
)
icer_0.95$cohort_type <- as.factor(icer_0.95$cohort_type)

# Compute mean ICER per cohort
icer_slopes <- icer_0.95 %>%
  group_by(cohort_type) %>%
  summarise(mean_icer = mean(icer, na.rm = TRUE))

# Merge slopes into data
plot_data <- icer_0.95 %>%
  left_join(icer_slopes, by = "cohort_type")

ggplot(plot_data, aes(x = qaly_difference, y = cost_difference)) +
  geom_point(alpha = 0.4, size = 1.5) +
  facet_wrap(~ cohort_type, scales = "free") +
  
  geom_abline(aes(slope = mean_icer, intercept = 0),
              colour = "red", linewidth = 1) +
  
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.5) +
  
  geom_text(
    data = icer_slopes,
    aes(
      x = -Inf, y = Inf,
      label = paste0("Mean ICER: £", format(round(mean_icer, 0), big.mark = ","))
    ),
    hjust = -0.1, vjust = 1.2,
    size = 3.5, fontface = "bold"
  ) +
  
  # Axes labels
  labs(
    title = "Cost vs QALY Differences (Monte Carlo ICER Simulation)",
    x = "Δ QALY",
    y = "Δ Cost (£)"
  ) +
  
  # Adjust scales to give some padding around zero
  scale_x_continuous(expand = expansion(mult = 0.15)) +
  scale_y_continuous(expand = expansion(mult = 0.15)) +
  
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(size = 16, face = "bold"),
    panel.spacing = unit(1, "lines")
  )

