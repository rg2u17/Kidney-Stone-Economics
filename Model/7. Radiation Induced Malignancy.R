# Overall Excess Absolute Risk Model (EAR) for Solid cancers associated with ionising radiation ####
# RadRAT: A Radiation Risk Assessment Tool for Lifetime Cancer Risk Projection
# Berrington de Gonzalez A, Iulian Apostoaei A, Veiga LH, Rajaraman P, Thomas BA, Owen Hoffman F, Gilbert E, Land C. RadRAT: a radiation risk assessment tool for lifetime cancer risk projection. J Radiol Prot. 2012 Sep;32(3):205-22. doi: 10.1088/0952-4746/32/3/205. Epub 2012 Jul 19. PMID: 22810503; PMCID: PMC3816370.

# Equation:
# βS D exp [γ e*] (a*)η

# βS = site-specific risk coefficient
# D = dose in Gy
# γ = age at exposure parameter
# e = age at exposure in years
# e* = (e-30)/10 for e <30
# e* = 0 for e ≥ 30
# η = attained age parameter
# a*=(a/60)
# a = attained age in years



# 1. Setup #####
## 1.1 Load libraries ####
library(gt)
library(gtExtras)
library(tidyverse)
library(DiagrammeR)
library(data.table)
library(janitor)
library(DataExplorer)
library(pROC)
library(ggplot2)
library(pracma)
library(glue)
library(cutpointr)
library(caret)
library(cowplot)
library(cutpointr)
library(ggsignif)
library(ggpubr)
library(parallel)
library(furrr)
library(future)

calculate_sd_from_ci <- function(ci_lower, ci_upper) {
  se <- (ci_upper - ci_lower) / (2 * 1.96)
  return(
    standard_error = se
  )
}

## 1.2 Define inputs ####
kidney_inputs <- cbind(
  beta_male = 0.31,
  beta_male_se = calculate_sd_from_ci(0.08, 0.68),
  beta_female = 0.31,
  beta_female_se = calculate_sd_from_ci(0.08, 0.68),
  gamma = -0.41,
  attained_age_parameter = 2.8,
  malignancy = "kidney"
) %>% as_tibble()

bladder_inputs <- cbind(
  beta_male = 1.2,
  beta_male_se = calculate_sd_from_ci(0.4, 3.7),
  beta_female = 0.75,
  beta_female_se = calculate_sd_from_ci(0.3, 1.7),
  gamma = -0.41,
  attained_age_parameter = 6,
  malignancy = "bladder"
) %>% as_tibble()

colon_inputs <- cbind(
  beta_male = 3.2,
  beta_male_se = calculate_sd_from_ci(1.8, 5.6),
  beta_female = 1.6,
  beta_female_se = calculate_sd_from_ci(0.8, 3.2),
  gamma = -0.41,
  attained_age_parameter = 2.8,
  malignancy = "colon"
) %>% as_tibble()

prostate_inputs <- cbind(
  beta_male = 0.11,
  beta_male_se = calculate_sd_from_ci(0, 1),
  beta_female = 0,
  beta_female_se = 0,
  gamma = -0.41,
  attained_age_parameter = 2.8,
  malignancy = "prostate"
) %>% as_tibble()

ovary_inputs <- cbind(
  beta_male = 0,
  beta_male_se = 0,
  beta_female = 0.7,
  beta_female_se = calculate_sd_from_ci(0.2,2.1),
  gamma = -0.41,
  attained_age_parameter = 2.8,
  malignancy = "ovary"
) %>% as_tibble()

rectum_inputs <- cbind(
  beta_male = 0.34,
  beta_male_se = calculate_sd_from_ci(0.09, 1.1),
  beta_female = 0.34,
  beta_female_se = calculate_sd_from_ci(0.09,1.1),
  gamma = -0.41,
  attained_age_parameter = 2.8,
  malignancy = "rectum"
) %>% as_tibble()

stomach_inputs <- cbind(
  beta_male = 4.9,
  beta_male_se = calculate_sd_from_ci(2.7, 8.9),
  beta_female = 4.9,
  beta_female_se = calculate_sd_from_ci(3.2,7.3),
  gamma = -0.41,
  attained_age_parameter = 2.8,
  malignancy = "stomach"
) %>% as_tibble()

liver_inputs <- cbind(
  beta_male = 2.2,
  beta_male_se = calculate_sd_from_ci(0.9, 5.3),
  beta_female = 1,
  beta_female_se = calculate_sd_from_ci(0.4,2.5),
  gamma = -0.41,
  attained_age_parameter = 4.1,
  malignancy = "liver"
) %>% as_tibble()

uterus_inputs <- cbind(
  beta_male = 0,
  beta_male_se = 0,
  beta_female = 1.2,
  beta_female_se = calculate_sd_from_ci(0,2.6),
  gamma = -0.41,
  attained_age_parameter = 2.8,
  malignancy = "uterus"
) %>% as_tibble()

pancreas_inputs <- cbind(
  beta_male = 0.49,
  beta_male_se = calculate_sd_from_ci(0.09, 1.1),
  beta_female = 0.49,
  beta_female_se = calculate_sd_from_ci(0.09, 1.1),
  gamma = -0.41,
  attained_age_parameter = 2.8,
  malignancy = "pancreas"
) %>% as_tibble()

ear_inputs <- rbind(
  bladder_inputs,
  kidney_inputs,
  prostate_inputs,
  colon_inputs,
  ovary_inputs,
  uterus_inputs,
  pancreas_inputs,
  liver_inputs,
  stomach_inputs,
  rectum_inputs
)

ear_inputs$beta_male <- as.numeric(ear_inputs$beta_male)
ear_inputs$beta_male_se <- as.numeric(ear_inputs$beta_male_se)
ear_inputs$beta_female <- as.numeric(ear_inputs$beta_female)
ear_inputs$beta_female_se <- as.numeric(ear_inputs$beta_female_se)
ear_inputs$gamma <- as.numeric(ear_inputs$gamma)
ear_inputs$attained_age_parameter <- as.numeric(ear_inputs$attained_age_parameter)

# 2. Create dataset to use for calculation ####
## 2.1 Aggregate Radiation Cohorts ####
aggregate_radiation_cohorts <- function(auc_target = c(1, 2, 3, 4, 5, 6, 7, 8, 9)) {
  all_cohorts <- list()
  
  for (i in auc_target) {
    key <- as.integer(i)
    message("Processing AUC = ", key)
    
    message("  Loading 2016 data...")
    cohort_2016_min_xr <- rad_doses_2016_cohort_min_xr[[key]] %>% mutate(cohort_type = "Minimum FU, XR", auc = i,
                                                                         year = 2016)
    cohort_2016_min_ct <- rad_doses_2016_cohort_min_ct[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i,
                                                                         year = 2016)
    cohort_2016_max_xr <- rad_doses_2016_cohort_max_xr[[key]] %>% mutate(cohort_type = "Maximum FU, XR", auc = i,
                                                                         year = 2016)
    cohort_2016_max_ct <- rad_doses_2016_cohort_max_ct[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i,
                                                                         year = 2016)
    
    message("  Loading 2017 data...")
    cohort_2017_min_xr <- rad_doses_2017_cohort_min_xr[[key]] %>% mutate(cohort_type = "Minimum FU, XR", auc = i,
                                                                         year = 2017)
    cohort_2017_min_ct <- rad_doses_2017_cohort_min_ct[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i,
                                                                         year = 2017)
    cohort_2017_max_xr <- rad_doses_2017_cohort_max_xr[[key]] %>% mutate(cohort_type = "Maximum FU, XR", auc = i,
                                                                         year = 2017)
    cohort_2017_max_ct <- rad_doses_2017_cohort_max_ct[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i,
                                                                         year = 2017)
    
    message("  Loading 2018 data...")
    cohort_2018_min_xr <- rad_doses_2018_cohort_min_xr[[key]] %>% mutate(cohort_type = "Minimum FU, XR", auc = i,
                                                                         year = 2018)
    cohort_2018_min_ct <- rad_doses_2018_cohort_min_ct[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i,
                                                                         year = 2018)
    cohort_2018_max_xr <- rad_doses_2018_cohort_max_xr[[key]] %>% mutate(cohort_type = "Maximum FU, XR", auc = i,
                                                                         year = 2018)
    cohort_2018_max_ct <- rad_doses_2018_cohort_max_ct[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i,
                                                                         year = 2018)
    
    message("  Loading 2019 data...")
    cohort_2019_min_xr <- rad_doses_2019_cohort_min_xr[[key]] %>% mutate(cohort_type = "Minimum FU, XR", auc = i,
                                                                         year = 2019)
    cohort_2019_min_ct <- rad_doses_2019_cohort_min_ct[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i,
                                                                         year = 2019)
    cohort_2019_max_xr <- rad_doses_2019_cohort_max_xr[[key]] %>% mutate(cohort_type = "Maximum FU, XR", auc = i,
                                                                         year = 2019)
    cohort_2019_max_ct <- rad_doses_2019_cohort_max_ct[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i,
                                                                         year = 2019)
    
    message("  Loading 2020 data...")
    cohort_2020_min_xr <- rad_doses_2020_cohort_min_xr[[key]] %>% mutate(cohort_type = "Minimum FU, XR", auc = i,
                                                                         year = 2020)
    cohort_2020_min_ct <- rad_doses_2020_cohort_min_ct[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i,
                                                                         year = 2020)
    cohort_2020_max_xr <- rad_doses_2020_cohort_max_xr[[key]] %>% mutate(cohort_type = "Maximum FU, XR", auc = i,
                                                                         year = 2020)
    cohort_2020_max_ct <- rad_doses_2020_cohort_max_ct[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i,
                                                                         year = 2020)
    
    message("  Combining cohorts for AUC = ", key)
    overall_cohort <- dplyr::bind_rows(
      cohort_2016_min_xr,
      cohort_2016_min_ct,
      cohort_2016_max_xr,
      cohort_2016_max_ct,
      cohort_2017_min_xr,
      cohort_2017_min_ct,
      cohort_2017_max_xr,
      cohort_2017_max_ct,
      cohort_2018_min_xr,
      cohort_2018_min_ct,
      cohort_2018_max_xr,
      cohort_2018_max_ct,
      cohort_2019_min_xr,
      cohort_2019_min_ct,
      cohort_2019_max_xr,
      cohort_2019_max_ct,
      cohort_2020_min_xr,
      cohort_2020_min_ct,
      cohort_2020_max_xr,
      cohort_2020_max_ct
    )
    
    all_cohorts[[key]] <- overall_cohort
    message("✅ Done with AUC = ", key, "\n")
  }
  
  message("🔗 Binding all AUC cohorts together...")
  final_df <- dplyr::bind_rows(all_cohorts)
  message("✅ All done.")
  
  return(final_df)
}


# AUC 0.55
auc_0.55 <- aggregate_radiation_cohorts(auc_target = 1) %>% subset(
  select = c(
    id,
    year,
    age,
    sex,
    auc_target,
    stone_free_status,
    risk_status,
    cohort_type,
    death_year_5,
    rad_dose_year_0,
    rad_dose_year_1,
    rad_dose_year_2,
    rad_dose_year_3,
    rad_dose_year_4,
    rad_dose_year_5,
    true_rec_5yr
  )
)

# AUC 0.6
auc_0.6 <- aggregate_radiation_cohorts(auc_target = 2) %>% subset(
  select = c(
    id,
    year,
    age,
    sex,
    auc_target,
    stone_free_status,
    risk_status,
    cohort_type,
    death_year_5,
    rad_dose_year_0,
    rad_dose_year_1,
    rad_dose_year_2,
    rad_dose_year_3,
    rad_dose_year_4,
    rad_dose_year_5,
    true_rec_5yr
  )
)

# AUC 0.65
auc_0.65 <- aggregate_radiation_cohorts(auc_target = 3) %>% subset(
  select = c(
    id,
    year,
    age,
    sex,
    auc_target,
    stone_free_status,
    risk_status,
    cohort_type,
    death_year_5,
    rad_dose_year_0,
    rad_dose_year_1,
    rad_dose_year_2,
    rad_dose_year_3,
    rad_dose_year_4,
    rad_dose_year_5,
    true_rec_5yr
  )
)

# AUC 0.7
auc_0.7 <- aggregate_radiation_cohorts(auc_target = 4) %>% subset(
  select = c(
    id,
    year,
    age,
    sex,
    auc_target,
    stone_free_status,
    risk_status,
    cohort_type,
    death_year_5,
    rad_dose_year_0,
    rad_dose_year_1,
    rad_dose_year_2,
    rad_dose_year_3,
    rad_dose_year_4,
    rad_dose_year_5,
    true_rec_5yr
  )
)

# AUC 0.75
auc_0.75 <- aggregate_radiation_cohorts(auc_target = 5) %>% subset(
  select = c(
    id,
    year,
    age,
    sex,
    auc_target,
    stone_free_status,
    risk_status,
    cohort_type,
    death_year_5,
    rad_dose_year_0,
    rad_dose_year_1,
    rad_dose_year_2,
    rad_dose_year_3,
    rad_dose_year_4,
    rad_dose_year_5,
    true_rec_5yr
  )
)

# AUC 0.8
auc_0.8 <- aggregate_radiation_cohorts(auc_target = 6) %>% subset(
  select = c(
    id,
    year,
    age,
    sex,
    auc_target,
    stone_free_status,
    risk_status,
    cohort_type,
    death_year_5,
    rad_dose_year_0,
    rad_dose_year_1,
    rad_dose_year_2,
    rad_dose_year_3,
    rad_dose_year_4,
    rad_dose_year_5,
    true_rec_5yr
  )
)

# AUC 0.85
auc_0.85 <- aggregate_radiation_cohorts(auc_target = 7) %>% subset(
  select = c(
    id,
    year,
    age,
    sex,
    auc_target,
    stone_free_status,
    risk_status,
    cohort_type,
    death_year_5,
    rad_dose_year_0,
    rad_dose_year_1,
    rad_dose_year_2,
    rad_dose_year_3,
    rad_dose_year_4,
    rad_dose_year_5,
    true_rec_5yr
  )
)

# AUC 0.9
auc_0.9 <- aggregate_radiation_cohorts(auc_target = 8) %>% subset(
  select = c(
    id,
    year,
    age,
    sex,
    auc_target,
    stone_free_status,
    risk_status,
    cohort_type,
    death_year_5,
    rad_dose_year_0,
    rad_dose_year_1,
    rad_dose_year_2,
    rad_dose_year_3,
    rad_dose_year_4,
    rad_dose_year_5,
    true_rec_5yr
  )
)

# AUC 0.95
auc_0.95 <- aggregate_radiation_cohorts(auc_target = 9) %>% subset(
  select = c(
    id,
    year,
    age,
    sex,
    auc_target,
    stone_free_status,
    risk_status,
    cohort_type,
    death_year_5,
    rad_dose_year_0,
    rad_dose_year_1,
    rad_dose_year_2,
    rad_dose_year_3,
    rad_dose_year_4,
    rad_dose_year_5,
    true_rec_5yr
  )
)

## 2.2 Combine AUC datasets ####
combine_auc_data <- function(data, auc_label) {
  data %>%
    mutate(
      auc_label = auc_label,
      risk_status = ifelse(risk_status == "LR", "Low Risk", "High Risk"),
      cumulative_rad_dose = rowSums(across(starts_with("rad_dose_year_")), na.rm = TRUE),
      recurrence = case_when(true_rec_5yr == "Yes" ~ "Yes",
                             true_rec_5yr == "No" ~ "No"),
      .keep = "all"
    ) %>%
    select(auc_label, 
           id,
           year,
           age,
           sex,
           auc_target,
           stone_free_status,
           risk_status,
           cohort_type,
           death_year_5,
           rad_dose_year_0,
           rad_dose_year_1,
           rad_dose_year_2,
           rad_dose_year_3,
           rad_dose_year_4,
           rad_dose_year_5,
           recurrence)
}

# Combine all AUC datasets and filter cohort_types
rad_data <- bind_rows(
  combine_auc_data(auc_0.55, 0.55),
  combine_auc_data(auc_0.6,  0.6),
  combine_auc_data(auc_0.65, 0.65),
  combine_auc_data(auc_0.7,  0.7),
  combine_auc_data(auc_0.75, 0.75),
  combine_auc_data(auc_0.8,  0.8),
  combine_auc_data(auc_0.85, 0.85),
  combine_auc_data(auc_0.9,  0.9),
  combine_auc_data(auc_0.95, 0.95)
) 

rad_data$auc_label <- as.factor(rad_data$auc_label)
rad_data$cohort_type <- as.factor(rad_data$cohort_type)
rad_data$risk_status <- as.factor(rad_data$risk_status)
rad_data$recurrence <- as.factor(rad_data$recurrence)


# 3. Function to calculate EAR ####
## 3.1 Individual risk function ####
# Define target lists
auc_targets <- c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95)
malignancy_list <- c("bladder", "kidney", "prostate", "colon", "ovary", "uterus",
                     "pancreas", "liver", "stomach", "rectum")

# Function to calculate e* (age at exposure parameter)
calculate_e_star <- function(age_at_exposure) {
  ifelse(age_at_exposure < 30, 
         (age_at_exposure - 30) / 10, 
         0)
}

# Corrected function to calculate excess cancer cases for a single exposure
calculate_excess_cases <- function(dose_mgy, sex, age_at_exposure, attained_age, 
                                   beta_male, beta_male_se, beta_female, beta_female_se, gamma, eta) {
  # Convert dose from mGy to Gy
  dose_gy <- dose_mgy / 1000
  
  # Select appropriate beta coefficient, handling NA values for sex-specific cancers
  if (sex == "male") {
    beta_s <- beta_male
    if (is.na(beta_s)) return(0)  # Return 0 risk if male beta is NA (e.g., ovary, uterus)
  } else {
    beta_s <- beta_female
    if (is.na(beta_s)) return(0)  # Return 0 risk if female beta is NA (e.g., prostate)
  }
  
  # Calculate e* 
  e_star <- calculate_e_star(age_at_exposure)
  
  # Calculate a* (normalized attained age)
  a_star <- attained_age / 60
  
  # Apply the equation: βS * D * exp[γ * e*] * (a*)^η
  excess_cases <- beta_s * dose_gy * exp(gamma * e_star) * (a_star^eta)
  
  return(excess_cases)
}

# Main function to iterate over AUC values and organs
ear_solid_malignancy <- function(rad_data,
                                 ear_inputs_1 = ear_inputs,
                                 auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                                 years_fu = c(5, 10, 15, 20, 25, 30),
                                 max_age_limit = 90) {
  
  
  results_list <- list()
  
  # Get malignancy list from the ear_inputs data
  malignancy_list <- ear_inputs_1$malignancy
  
  # Function to calculate risk for a specific follow-up period
  calculate_followup_risk <- function(age, sex, dose_year_1, dose_year_2, dose_year_3, 
                                      dose_year_4, dose_year_5, followup_years, ear_input_row) {
    
    # Only calculate if attained age is below limit
    if ((age + followup_years) >= max_age_limit) {
      return(NA_real_)
    }
    
    # Calculate risk contribution from each exposure year
    risk_total <- 0
    doses <- c(dose_year_1, dose_year_2, dose_year_3, dose_year_4, dose_year_5)
    
    for (exp_year in 1:5) {
      if (!is.na(doses[exp_year]) && doses[exp_year] > 0) {
        age_at_exposure <- age + exp_year
        attained_age <- age + followup_years + exp_year
        
        risk_contribution <- calculate_excess_cases(
          dose_mgy = doses[exp_year],
          sex = sex,
          age_at_exposure = age_at_exposure,
          attained_age = attained_age,
          beta_male = ear_input_row$beta_male,
          beta_female = ear_input_row$beta_female,
          gamma = ear_input_row$gamma,
          eta = ear_input_row$attained_age_parameter
        )
        
        risk_total <- risk_total + risk_contribution
      }
    }
    
    return(risk_total)
  }
  
  # Iterate over each AUC target and organ combination
  for (auc in auc_targets) {
    for (organ in malignancy_list) {
      message("Calculating radiation associated ", organ, " malignancy risk for AUC: ", auc)
      
      # Get organ-specific parameters (single row)
      ear_input <- ear_inputs_1 %>% 
        filter(malignancy == organ)
      
      if (nrow(ear_input) == 0) {
        warning("No parameters found for organ: ", organ, ". Skipping.")
        next
      }
      
      # Filter data for this AUC and alive patients
      target_data <- rad_data %>%
        filter(auc_label == auc & death_year_5 == "No") %>%
        select(age, sex, rad_dose_year_1, rad_dose_year_2, rad_dose_year_3, 
               rad_dose_year_4, rad_dose_year_5) 
      
      if (nrow(target_data) == 0) {
        warning("No data found for AUC: ", auc, ". Skipping.")
        next
      }
      
      # Calculate risks for all follow-up periods
      target_data <- target_data %>%
        rowwise() %>%
        mutate(
          five_year_risk = calculate_followup_risk(
            age, sex, rad_dose_year_1, rad_dose_year_2, rad_dose_year_3,
            rad_dose_year_4, rad_dose_year_5, 5, ear_input
          ),
          ten_year_risk = calculate_followup_risk(
            age, sex, rad_dose_year_1, rad_dose_year_2, rad_dose_year_3,
            rad_dose_year_4, rad_dose_year_5, 10, ear_input
          ),
          fifteen_year_risk = calculate_followup_risk(
            age, sex, rad_dose_year_1, rad_dose_year_2, rad_dose_year_3,
            rad_dose_year_4, rad_dose_year_5, 15, ear_input
          ),
          twenty_year_risk = calculate_followup_risk(
            age, sex, rad_dose_year_1, rad_dose_year_2, rad_dose_year_3,
            rad_dose_year_4, rad_dose_year_5, 20, ear_input
          ),
          twenty_five_year_risk = calculate_followup_risk(
            age, sex, rad_dose_year_1, rad_dose_year_2, rad_dose_year_3,
            rad_dose_year_4, rad_dose_year_5, 25, ear_input
          ),
          thirty_year_risk = calculate_followup_risk(
            age, sex, rad_dose_year_1, rad_dose_year_2, rad_dose_year_3,
            rad_dose_year_4, rad_dose_year_5, 30, ear_input
          ),
          organ = organ,
          auc = auc
        ) %>%
        ungroup() %>%
        select(id, sex, five_year_risk, ten_year_risk, fifteen_year_risk,
               twenty_year_risk, twenty_five_year_risk, thirty_year_risk,
               organ, auc, cohort_type, stone_free_status)
      
      # Store results
      results_list[[paste(organ, auc, sep = "_")]] <- target_data
    }
  }
  
  # Combine all results
  final_results <- bind_rows(results_list)
  
  return(final_results)
}

## 3.2 Grouped function ####
ear_solid_malignancy_grouped <- function(rad_data,
                                         ear_inputs_1 = ear_inputs,
                                         auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                                         years_fu = c(5, 10, 15, 20, 25, 30),
                                         max_age_limit = 90) {
  results_list <- list()
  
  # Get malignancy list from the ear_inputs data
  malignancy_list <- ear_inputs_1$malignancy
  
  # Function to calculate risk for a follow-up period for a given group
  calculate_followup_risk <- function(age, sex, doses, followup_years, ear_input_row) {
    # Skip if attained age limit exceeded
    if ((age + followup_years) >= max_age_limit) return(0)
    
    risk_total <- 0
    for (exp_year in 1:5) {
      if (!is.na(doses[exp_year]) && doses[exp_year] > 0) {
        age_at_exposure <- age + exp_year
        attained_age <- age + followup_years + exp_year
        risk_total <- risk_total +
          calculate_excess_cases(
            dose_mgy = doses[exp_year],
            sex = sex,
            age_at_exposure = age_at_exposure,
            attained_age = attained_age,
            beta_male = ear_input_row$beta_male,
            beta_female = ear_input_row$beta_female,
            gamma = ear_input_row$gamma,
            eta = ear_input_row$attained_age_parameter
          )
      }
    }
    return(risk_total)
  }
  
  for (auc in auc_targets) {
    for (organ in malignancy_list) {
      message("Calculating grouped ", organ, " malignancy risk for AUC: ", auc)
      
      ear_input <- ear_inputs_1 %>% filter(malignancy == organ)
      if (nrow(ear_input) == 0) next
      
      # Group by the final grouping variables first
      target_data <- rad_data %>%
        filter(auc_label == auc, death_year_5 == "No") %>%
        group_by(sex, 
                 cohort_type, 
                 age, 
                 auc_label, 
                 risk_status, 
                 rad_dose_year_1, 
                 rad_dose_year_2, 
                 rad_dose_year_3,
                 rad_dose_year_4,
                 rad_dose_year_5,
                 recurrence) %>%
        summarise(n_patients = n(), .groups = "drop")
      
      if (nrow(target_data) == 0) next
      
      # Calculate average doses for each group from the original data
      dose_data <- rad_data %>%
        filter(auc_label == auc, death_year_5 == "No") %>%
        group_by(sex, 
                 cohort_type, 
                 age, 
                 auc_label, 
                 risk_status, 
                 rad_dose_year_1, 
                 rad_dose_year_2, 
                 rad_dose_year_3,
                 rad_dose_year_4,
                 rad_dose_year_5,
                 recurrence) %>%
        summarise(
          avg_rad_dose_year_1 = mean(rad_dose_year_1, na.rm = TRUE),
          avg_rad_dose_year_2 = mean(rad_dose_year_2, na.rm = TRUE),
          avg_rad_dose_year_3 = mean(rad_dose_year_3, na.rm = TRUE),
          avg_rad_dose_year_4 = mean(rad_dose_year_4, na.rm = TRUE),
          avg_rad_dose_year_5 = mean(rad_dose_year_5, na.rm = TRUE),
          .groups = "drop"
        )
      
      # Combine patient counts with dose data
      combined_data <- target_data %>%
        left_join(dose_data, by = c("sex", 
                                    "cohort_type", 
                                    "age", 
                                    "auc_label", 
                                    "risk_status",
                                    "rad_dose_year_1", 
                                    "rad_dose_year_2", 
                                    "rad_dose_year_3",
                                    "rad_dose_year_4",
                                    "rad_dose_year_5",
                                    "recurrence"))
      
      # Calculate risk for each follow-up period using average doses
      result_data <- combined_data %>%
        rowwise() %>%
        mutate(
          five_year_risk       = calculate_followup_risk(
            age = age,
            sex = sex,
            doses = c(
              avg_rad_dose_year_1,
              avg_rad_dose_year_2,
              avg_rad_dose_year_3,
              avg_rad_dose_year_4,
              avg_rad_dose_year_5
            ),
            followup_years = 5,
            ear_input_row = ear_input
          ) * n_patients,
          ten_year_risk        = calculate_followup_risk(
            age = age,
            sex = sex,
            doses = c(
              avg_rad_dose_year_1,
              avg_rad_dose_year_2,
              avg_rad_dose_year_3,
              avg_rad_dose_year_4,
              avg_rad_dose_year_5
            ),
            followup_years = 10,
            ear_input_row = ear_input
          ) * n_patients,
          fifteen_year_risk    = calculate_followup_risk(
            age = age,
            sex = sex,
            doses = c(
              avg_rad_dose_year_1,
              avg_rad_dose_year_2,
              avg_rad_dose_year_3,
              avg_rad_dose_year_4,
              avg_rad_dose_year_5
            ),
            followup_years = 15,
            ear_input_row = ear_input
          ) * n_patients,
          twenty_year_risk     = calculate_followup_risk(
            age = age,
            sex = sex,
            doses = c(
              avg_rad_dose_year_1,
              avg_rad_dose_year_2,
              avg_rad_dose_year_3,
              avg_rad_dose_year_4,
              avg_rad_dose_year_5
            ),
            followup_years = 20,
            ear_input_row = ear_input
          ) * n_patients,
          twenty_five_year_risk = calculate_followup_risk(
            age = age,
            sex = sex,
            doses = c(
              avg_rad_dose_year_1,
              avg_rad_dose_year_2,
              avg_rad_dose_year_3,
              avg_rad_dose_year_4,
              avg_rad_dose_year_5
            ),
            followup_years = 25,
            ear_input_row = ear_input
          ) * n_patients,
          thirty_year_risk     = calculate_followup_risk(
            age = age,
            sex = sex,
            doses = c(
              avg_rad_dose_year_1,
              avg_rad_dose_year_2,
              avg_rad_dose_year_3,
              avg_rad_dose_year_4,
              avg_rad_dose_year_5
            ),
            followup_years = 30,
            ear_input_row = ear_input
          ) * n_patients,
          five_year_n       = five_year_risk * n_patients,
          ten_year_n        = ten_year_risk * n_patients,
          fifteen_year_n    = fifteen_year_risk * n_patients,
          twenty_year_n     = twenty_year_risk * n_patients,
          twenty_five_year_n = twenty_five_year_risk * n_patients,
          thirty_year_n     = thirty_year_risk * n_patients,
          organ = organ,
          auc = auc
        ) %>%
        ungroup() %>%
        select(-starts_with("avg_rad_dose_"), -auc_label)
      
      results_list[[paste(organ, auc, sep = "_")]] <- result_data
    }
  }
  
  bind_rows(results_list)
}


# 4. Run Function ####
malignancy_data_grouped <- ear_solid_malignancy_grouped(rad_data)

overall_ear_per_person <- malignancy_data_grouped %>%
  group_by(auc, sex, age, cohort_type, risk_status, recurrence) %>%
  summarise(
    overall_five_year_ear      = mean(five_year_risk, na.rm = TRUE),
    overall_five_year_ear_se = sd(five_year_risk, na.rm = TRUE) / sqrt(sum(!is.na(five_year_risk))),
    overall_ten_year_ear       = mean(ten_year_risk, na.rm = TRUE),
    overall_ten_year_ear_se = sd(ten_year_risk, na.rm = TRUE) / sqrt(sum(!is.na(ten_year_risk))),
    overall_fifteen_year_ear   = mean(fifteen_year_risk, na.rm = TRUE),
    overall_fifteen_year_ear_se   = sd(fifteen_year_risk, na.rm = TRUE) / sqrt(sum(!is.na(fifteen_year_risk))),
    overall_twenty_year_ear    = mean(twenty_year_risk, na.rm = TRUE),
    overall_twenty_year_se    = sd(twenty_year_risk, na.rm = TRUE) / sqrt(sum(!is.na(twenty_year_risk))),
    overall_twentyfive_year_ear = mean(twenty_five_year_risk, na.rm = TRUE),
    overall_twentyfive_year_ear_se = sd(twenty_five_year_risk, na.rm = TRUE) / sqrt(sum(!is.na(twenty_five_year_risk))),
    overall_thirty_year_ear    = mean(thirty_year_risk, na.rm = TRUE),
    overall_thirty_year_ear_se    = sd(thirty_year_risk, na.rm = TRUE) / sqrt(sum(!is.na(thirty_year_risk))),
    overall_five_year_ear_n      = mean(five_year_n, na.rm = TRUE),
    overall_ten_year_ear_n       = mean(ten_year_n, na.rm = TRUE),
    overall_fifteen_year_ear_n   = mean(fifteen_year_n, na.rm = TRUE),
    overall_twenty_year_ear_n    = mean(twenty_year_n, na.rm = TRUE),
    overall_twentyfive_year_ear_n = mean(twenty_five_year_n, na.rm = TRUE),
    overall_thirty_year_ear_n    = mean(thirty_year_n, na.rm = TRUE),
    .groups = "drop"
  )

# 5. Plot results ####
## 5.1 Overall plot ####
overall_ear_means <- overall_ear_per_person %>%
  pivot_longer(
    cols = c(
      overall_five_year_ear,
      overall_ten_year_ear,
      overall_fifteen_year_ear,
      overall_twenty_year_ear,
      overall_twentyfive_year_ear,
      overall_thirty_year_ear
    ),
    names_to = "follow_up",
    values_to = "ear"
  )

overall_ear_se <- overall_ear_per_person %>%
  pivot_longer(
    cols = c(
      overall_five_year_ear_se,
      overall_ten_year_ear_se,
      overall_fifteen_year_ear_se,
      overall_twenty_year_se,
      overall_twentyfive_year_ear_se,
      overall_thirty_year_ear_se
    ),
    names_to = "follow_up_se",
    values_to = "ear_se"
  ) %>%
  mutate(
    follow_up = recode(follow_up_se,
                       "overall_five_year_ear_se"        = "overall_five_year_ear",
                       "overall_ten_year_ear_se"         = "overall_ten_year_ear",
                       "overall_fifteen_year_ear_se"     = "overall_fifteen_year_ear",
                       "overall_twenty_year_se"          = "overall_twenty_year_ear",
                       "overall_twentyfive_year_ear_se"  = "overall_twentyfive_year_ear",
                       "overall_thirty_year_ear_se"      = "overall_thirty_year_ear"
    )
  ) %>%
  select(-follow_up_se)

overall_ear_long <- overall_ear_means %>%
  cbind(overall_ear_se %>% select(ear_se)) %>%
  mutate(
    ear_per_1000 = ear * 1000,
    ear_se_per_1000 = ear_se * 1000,
    yrs_after_first_scan = recode(follow_up,
                                  "overall_five_year_ear"        = "5 years",
                                  "overall_ten_year_ear"         = "10 years",
                                  "overall_fifteen_year_ear"     = "15 years",
                                  "overall_twenty_year_ear"      = "20 years",
                                  "overall_twentyfive_year_ear"  = "25 years",
                                  "overall_thirty_year_ear"      = "30 years"
    ),
    yrs_after_first_scan = factor(yrs_after_first_scan, levels = c("5 years", "10 years", "15 years",
                                                                   "20 years", "25 years", "30 years"))
  )

overall_ear_long$auc <- as.factor(overall_ear_long$auc)

overall_ear_long_all <- overall_ear_long %>%
  group_by(auc, cohort_type, follow_up, yrs_after_first_scan) %>%
  summarise(
    ear = mean(ear, na.rm = TRUE),
    ear_se = sqrt(mean(ear_se^2, na.rm = TRUE)),  # Conservative approach using RMS of SEs
    risk_status = "All",
    .groups = "drop"
  ) %>%
  mutate(
    ear_per_1000 = ear * 1000,
    ear_se_per_1000 = ear_se * 1000
  ) %>%
  bind_rows(overall_ear_long) %>%
  mutate(
    risk_status = factor(risk_status, 
                         levels = c("High Risk", "Low Risk", "All"))
  )
bind_rows(overall_ear_long) %>%
  mutate(
    risk_status = factor(risk_status, 
                         levels = c("High Risk", "Low Risk", "All"))
  )

## 5.2 Organ specific plot ####
malignancy_data_grouped$auc <- as.factor(malignancy_data_grouped$auc)

organ_specific_ear_per_person <- malignancy_data_grouped %>%
  group_by(auc, cohort_type, risk_status, organ) %>%
  summarise(
    five_year_ear      = sum(five_year_risk, na.rm = TRUE),
    five_year_ear_se   = sd(five_year_risk, na.rm = TRUE) / sqrt(sum(!is.na(five_year_risk))),
    ten_year_ear       = sum(ten_year_risk, na.rm = TRUE),
    ten_year_ear_se    = sd(ten_year_risk, na.rm = TRUE) / sqrt(sum(!is.na(ten_year_risk))),
    fifteen_year_ear   = sum(fifteen_year_risk, na.rm = TRUE),
    fifteen_year_ear_se = sd(fifteen_year_risk, na.rm = TRUE) / sqrt(sum(!is.na(fifteen_year_risk))),
    twenty_year_ear    = sum(twenty_year_risk, na.rm = TRUE),
    twenty_year_ear_se = sd(twenty_year_risk, na.rm = TRUE) / sqrt(sum(!is.na(twenty_year_risk))),
    twentyfive_year_ear = sum(twenty_five_year_risk, na.rm = TRUE),
    twentyfive_year_ear_se = sd(twenty_five_year_risk, na.rm = TRUE) / sqrt(sum(!is.na(twenty_five_year_risk))),
    thirty_year_ear    = sum(thirty_year_risk, na.rm = TRUE),
    thirty_year_ear_se = sd(thirty_year_risk, na.rm = TRUE) / sqrt(sum(!is.na(thirty_year_risk))),
    .groups = "drop"
  )

organ_specific_ear_per_person$organ <- as.factor(organ_specific_ear_per_person$organ)

# reshape the mean values
organ_ear_means <- organ_specific_ear_per_person %>%
  pivot_longer(
    cols = c(
      five_year_ear,
      ten_year_ear,
      fifteen_year_ear,
      twenty_year_ear,
      twentyfive_year_ear,
      thirty_year_ear
    ),
    names_to = "follow_up",
    values_to = "ear"
  )

# reshape the SE values
organ_ear_se <- organ_specific_ear_per_person %>%
  pivot_longer(
    cols = c(
      five_year_ear_se,
      ten_year_ear_se,
      fifteen_year_ear_se,
      twenty_year_ear_se,
      twentyfive_year_ear_se,
      thirty_year_ear_se
    ),
    names_to = "follow_up_se",
    values_to = "ear_se"
  ) %>%
  # Standardize the column names to match
  mutate(
    follow_up = recode(follow_up_se,
                       "five_year_ear_se"        = "five_year_ear",
                       "ten_year_ear_se"         = "ten_year_ear",
                       "fifteen_year_ear_se"     = "fifteen_year_ear",
                       "twenty_year_ear_se"      = "twenty_year_ear",
                       "twentyfive_year_ear_se"  = "twentyfive_year_ear",
                       "thirty_year_ear_se"      = "thirty_year_ear"
    )
  ) %>%
  select(-follow_up_se)

# Join them together
organ_specific_ear_per_person_long <- organ_ear_means %>%
  cbind(organ_ear_se %>% select(ear_se)) %>%
  group_by(organ) %>%
  mutate(
    ear_per_1000 = ear /1000,
    ear_se_per_1000 = ear_se / 1000,
    follow_up = recode(
      follow_up,
      "five_year_ear"        = "5",
      "ten_year_ear"         = "10",
      "fifteen_year_ear"     = "15",
      "twenty_year_ear"      = "20",
      "twentyfive_year_ear"  = "25",
      "thirty_year_ear"      = "30"
    ),
    yrs_after_first_scan = factor(follow_up, levels = c("5", "10", "15", "20", "25", "30")),
    organ = recode(
      organ,
      "bladder" = "Bladder",
      "colon" = "Colon",
      "kidney" = "Kidney",
      "liver" = "Liver",
      "ovary" = "Ovary",
      "pancreas" = "Pancreas",
      "prostate" = "Prostate",
      "rectum" = "Rectum",
      "stomach" = "Stomach",
      "uterus" = "Uterus"
    ),
    organ = factor(
      organ,
      levels = c(
        "Stomach",
        "Bladder",
        "Liver",
        "Colon",
        "Uterus",
        "Ovary",
        "Pancreas",
        "Rectum",
        "Kidney",
        "Prostate"
      )
    )
  )

# Combine the High / Low Risk stratification with the total excess cancer incidence
organ_specific_ear_long_all <- organ_specific_ear_per_person_long %>%
  group_by(auc, cohort_type, organ, follow_up, yrs_after_first_scan) %>%
  summarise(
    ear = mean(ear, na.rm = TRUE),
    ear_se = sqrt(mean(ear_se^2, na.rm = TRUE)),  # RMS of SEs
    ear_per_1000 = mean(ear_per_1000, na.rm = TRUE),
    ear_se_per_1000 = sqrt(mean(ear_se_per_1000^2, na.rm = TRUE)),
    risk_status = "All",
    .groups = "drop"
  ) %>%
  bind_rows(organ_specific_ear_per_person_long) %>%
  mutate(
    risk_status = factor(risk_status, 
                         levels = c("High Risk", "Low Risk", "All"))
  )

# Plot with error bars
ggplot(
  organ_specific_ear_long_all,
  aes(x = yrs_after_first_scan, y = ear, fill = auc)
) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(
    aes(ymin = ear - ear_se, ymax = ear + ear_se),
    position = position_dodge(width = 0.8),
    width = 0.2,
    color = "black"
  ) +
  facet_grid(cohort_type ~ organ, scales = "free_y") +
  labs(title = "Estimated Excess Absolute Solid Cancer Incidence by Individual Exposed Organ", 
       x = "Time from 1st Imaging (years)", 
       y = "Estimated Excess Absolute Solid Cancer Incidence, n per 100,000") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position = "right",
    strip.text = element_text(face = "bold")
  ) 

# 6. Calculate Lifetime Attributable Risk ####
## 6.1 Function to get risk ####
malignancy_list <- ear_inputs$malignancy

monte_carlo_lifetime_risk <- function(age, sex, doses, start_year, 
                                      auc_target, n_simulations = 1000, seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  # Pre-allocate results vector
  simulated_risks <- numeric(n_simulations)
  
  # Malignancy list
  malignancy_list <- ear_inputs$malignancy
  
  # Life expectancy lookup
  expected <- life_expectancy_ons %>%
    filter(sex == !!sex, age == !!age, year == start_year) %>%
    pull(ex)
  
  # If no life expectancy found, return safe defaults
  if (length(expected) == 0 || is.na(expected)) {
    return(list(
      mean_risk = 0,
      ci_lower = 0,
      ci_upper = 0
    ))
  }
  
  attained_age <- age + expected
  
  # Monte Carlo simulation
  for (sim in seq_len(n_simulations)) {
    total_lifetime_risk <- 0
    
    for (i in seq_along(malignancy_list)) {
      current_malignancy <- malignancy_list[i]
      ear_input_row <- ear_inputs %>% filter(malignancy == current_malignancy)
      
      # Skip if no valid beta coefficients for this sex
      if ((sex == "male" && (is.na(ear_input_row$beta_male) | is.na(ear_input_row$beta_male_se))) ||
          (sex == "female" && (is.na(ear_input_row$beta_female) | is.na(ear_input_row$beta_female_se)))) {
        next
      }
      
      # Sample betas
      beta_male_sim <- NA
      beta_female_sim <- NA
      
      if (sex == "male") {
        beta_sample <- rnorm(1, mean = ear_input_row$beta_male, sd = ear_input_row$beta_male_se)
        beta_male_sim <- beta_sample * auc_target
      } else {
        beta_sample <- rnorm(1, mean = ear_input_row$beta_female, sd = ear_input_row$beta_female_se)
        beta_female_sim <- beta_sample * auc_target
      }
      
      # Calculate organ-specific risk
      organ_lifetime_risk <- 0
      for (exp_year in 1:5) {
        if (!is.na(doses[exp_year]) && doses[exp_year] > 0) {
          age_at_exposure <- age + exp_year
          
          excess_cases <- calculate_excess_cases(
            dose_mgy = doses[exp_year],
            sex = sex,
            age_at_exposure = age_at_exposure,
            attained_age = attained_age,
            beta_male = beta_male_sim,
            beta_female = beta_female_sim,
            gamma = ear_input_row$gamma,
            eta = ear_input_row$attained_age_parameter
          )
          
          organ_lifetime_risk <- organ_lifetime_risk + excess_cases
        }
      }
      
      total_lifetime_risk <- total_lifetime_risk + as.numeric(organ_lifetime_risk)
    }
    
    simulated_risks[sim] <- total_lifetime_risk
  }
  
  # Always return the same structure
  return(list(
    mean_risk = mean(simulated_risks, na.rm = TRUE),
    ci_lower = quantile(simulated_risks, 0.025, names = FALSE, na.rm = TRUE),
    ci_upper = quantile(simulated_risks, 0.975, names = FALSE, na.rm = TRUE)
  ))
}


## 6.2 Run function ####
### 6.2.1 All patients ####
plan(multisession, workers = parallel::detectCores() - 1)

risk_all <- rad_data %>%
  group_by(age, sex, cohort_type, auc_target) %>%
  summarise(
    across(starts_with("rad_dose_year_"), 
           ~mean(.x, na.rm = TRUE), 
           .names = "mean_{.col}"),
    start_year = first(year),  # Changed from just 'year'
    n_patients = n(),
    .groups = "drop"
  ) %>%
  mutate(
    risk_status = "All",
    doses = pmap(list(mean_rad_dose_year_1, mean_rad_dose_year_2, 
                      mean_rad_dose_year_3, mean_rad_dose_year_4, 
                      mean_rad_dose_year_5), c)
  ) %>%
  mutate(
    mc_results = future_pmap(
      list(age, sex, doses, start_year, auc_target),
      ~monte_carlo_lifetime_risk(
        age = ..1,
        sex = ..2,
        doses = ..3,
        start_year = ..4,
        auc_target = ..5,
        n_simulations = 100
      ),
      .options = furrr_options(seed = TRUE)
    )
  ) %>%
  mutate(
    cumulative_lifetime_risk = map_dbl(mc_results, "mean_risk"),
    ci_lower_95 = map_dbl(mc_results, "ci_lower"),
    ci_upper_95 = map_dbl(mc_results, "ci_upper")
  ) %>%
  select(-doses)  

plan(sequential) 

### 6.2.2 Subdivided by Risk status ####
risk_summary_monte_carlo_final <- rad_data %>%
  group_by(age, sex, risk_status, cohort_type, auc_target) %>%
  summarise(
    mean_dose_year_1 = mean(rad_dose_year_1, na.rm = TRUE),
    mean_dose_year_2 = mean(rad_dose_year_2, na.rm = TRUE),
    mean_dose_year_3 = mean(rad_dose_year_3, na.rm = TRUE),
    mean_dose_year_4 = mean(rad_dose_year_4, na.rm = TRUE),
    mean_dose_year_5 = mean(rad_dose_year_5, na.rm = TRUE),
    start_year = first(year),
    n_patients = n(),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    mc_results = list(monte_carlo_lifetime_risk(
      age = age,
      sex = sex,
      doses = c(mean_dose_year_1, mean_dose_year_2, mean_dose_year_3, 
                mean_dose_year_4, mean_dose_year_5),
      start_year = start_year,
      auc_target = auc_target,
      n_simulations = 100
    ))
  ) %>%
  ungroup() %>%
  mutate(
    cumulative_lifetime_risk = map_dbl(mc_results, "mean_risk"),
    ci_lower_95 = map_dbl(mc_results, "ci_lower"),
    ci_upper_95 = map_dbl(mc_results, "ci_upper")
  )

risk_combined <- bind_rows(risk_summary_monte_carlo_final, risk_all)
risk_combined$risk_status <- as.factor(risk_combined$risk_status)

### 6.2.3 Get Grand Total of Number of Expected additional malignancies ####
# Filter out deaths in year 5
rad_data_filtered <- rad_data %>%
  filter(death_year_5 != "Yes")

# Get all unique combinations of auc_target and cohort_type
target_cohort_combinations <- rad_data_filtered %>%
  distinct(auc_target, cohort_type)

# Initialize list to store all results
all_results_list <- list()

# Loop through each auc_target and cohort_type combination
for(i in 1:nrow(target_cohort_combinations)) {
  
  current_auc <- target_cohort_combinations$auc_target[i]
  current_cohort <- target_cohort_combinations$cohort_type[i]
  
  cat("Processing auc_target:", current_auc, "| cohort_type:", current_cohort, 
      "(", i, "of", nrow(target_cohort_combinations), ")\n")
  
  # Filter data for current combination
  current_data <- rad_data_filtered %>%
    filter(auc_target == current_auc, cohort_type == current_cohort)
  
  # Count unique patients for this combination
  total_unique_patients <- n_distinct(current_data$id)
  
  # Group by all relevant variables
  unique_groups <- current_data %>%
    group_by(age, sex, rad_dose_year_1, rad_dose_year_2, rad_dose_year_3, 
             rad_dose_year_4, rad_dose_year_5, risk_status, stone_free_status) %>%
    summarise(
      n_patients = n_distinct(id),  # Count unique patients in this dose/risk/stone combination
      start_year = first(year),
      .groups = "drop"
    )
  
  cat("  Found", nrow(unique_groups), "unique dose/risk/stone combinations\n")
  cat("  Total unique patients:", total_unique_patients, "\n")
  
  # Set chunk size
  chunk_size <- 100
  n_chunks <- ceiling(nrow(unique_groups) / chunk_size)
  
  # Initialize chunk results
  chunk_results <- vector("list", n_chunks)
  
  # Process in chunks
  for(j in 1:n_chunks) {
    if(j %% 10 == 0) {
      cat("    Processing chunk", j, "of", n_chunks, "\n")
    }
    
    # Get chunk indices
    start_idx <- ((j - 1) * chunk_size) + 1
    end_idx <- min(j * chunk_size, nrow(unique_groups))
    
    # Get current chunk
    current_chunk <- unique_groups[start_idx:end_idx, ]
    
    # Process chunk using pmap
    chunk_result <- current_chunk %>%
      mutate(
        mc_results = pmap(
          list(age, sex, rad_dose_year_1, rad_dose_year_2, rad_dose_year_3,
               rad_dose_year_4, rad_dose_year_5, start_year),
          function(age, sex, d1, d2, d3, d4, d5, start_year) {
            monte_carlo_lifetime_risk(
              age = age,
              sex = sex,
              doses = c(d1, d2, d3, d4, d5),
              start_year = start_year,
              auc_target = current_auc,
              n_simulations = 5
            )
          }
        )
      ) %>%
      mutate(
        risk_per_1000 = map_dbl(mc_results, "mean_risk"),
        ci_lower_95 = map_dbl(mc_results, "ci_lower"),
        ci_upper_95 = map_dbl(mc_results, "ci_upper"),
        total_expected_malignancies = (risk_per_1000 / 1000) * n_patients,
        total_expected_malignancies_lower_95 = (ci_lower_95 / 1000) * n_patients,
        total_expected_malignancies_upper_95 = (ci_upper_95 / 1000) * n_patients,
        auc_target = current_auc,
        cohort_type = current_cohort,
        risk_status = risk_status
      )
    
    chunk_results[[j]] <- chunk_result
    
    rm(current_chunk, chunk_result)
    gc(verbose = FALSE)
  }
  
  # Combine chunks for this auc_target/cohort_type
  combined_result <- bind_rows(chunk_results)
  
  # Calculate totals for this combination
  summary_result <- combined_result %>%
    group_by(risk_status, auc_target, cohort_type) %>%
    summarise(
      grand_total_expected_malignancies = sum(total_expected_malignancies, na.rm = TRUE),
      grand_total_expected_malignancies_lower = sum(total_expected_malignancies_lower_95, na.rm = TRUE),
      grand_total_expected_malignancies_upper = sum(total_expected_malignancies_upper_95, na.rm = TRUE),
      total_patients = sum(n_patients, na.rm = TRUE),
      .groups = "drop"
    )
  
  all_results_list[[i]] <- summary_result
  
  rm(current_data, unique_groups, chunk_results, combined_result)
  gc(verbose = FALSE)
  
  cat("  Completed. Expected malignancies:", 
      round(summary_result$grand_total_expected_malignancies, 2), "\n\n")
}

# Combine all results
malignancy_grand_total <- bind_rows(all_results_list)
gc()

# Display results
malignancy_grand_total %>%
  group_by(cohort_type,
           risk_status,
           auc_target) %>%
  gt() %>%
  fmt_number(
    columns = grand_total_expected_malignancies,
    decimals = 0
  ) %>%
  fmt_number(
    columns = grand_total_expected_malignancies_lower,
    decimals = 0
  ) %>%
  fmt_number(
    columns = grand_total_expected_malignancies_upper,
    decimals = 0
  ) %>%
  fmt_number(
    columns = total_patients,
    decimals = 0,
    use_seps = TRUE
  ) %>%
  cols_label(
    auc_target = "AUC Target",
    cohort_type = "Cohort Type",
    risk_status = "Risk Status",
    grand_total_expected_malignancies = "Expected Lifetime Malignancies",
    grand_total_expected_malignancies_lower = "Lower CI",
    grand_total_expected_malignancies_upper = "Upper CI",
    total_patients = "Total Patients"
  )

### 6.2.4 Plot % increase in expected lifetime malignancies ####
malignancy_plot_data <- malignancy_grand_total %>%
  mutate(
    percentage = (grand_total_expected_malignancies / total_patients) * 100,
    percentage_lower = (grand_total_expected_malignancies_lower / total_patients) * 100,
    percentage_upper = (grand_total_expected_malignancies_upper / total_patients) * 100
  )

comparisons <- list(
  c("Maximum FU, CT", "Maximum FU, XR"),
  c("Minimum FU, CT", "Minimum FU, XR"),
  c("Maximum FU, CT", "Minimum FU, CT"),
  c("Maximum FU, XR", "Minimum FU, XR")
)

calc_chisq_pvalue <- function(data, group1, group2) {
  d1 <- data %>% filter(cohort_type == group1)
  d2 <- data %>% filter(cohort_type == group2)
  
  if(nrow(d1) == 0 || nrow(d2) == 0) return(1)
  
  mat <- matrix(
    c(d1$grand_total_expected_malignancies, 
      d1$total_patients - d1$grand_total_expected_malignancies,
      d2$grand_total_expected_malignancies,
      d2$total_patients - d2$grand_total_expected_malignancies),
    nrow = 2,
    byrow = TRUE
  )
  
  tryCatch({
    chisq.test(mat)$p.value
  }, error = function(e) 1)
}

ggplot(malignancy_plot_data, 
       aes(x = cohort_type, 
           y = percentage,
           fill = cohort_type)) +
  geom_col(color = "black", linewidth = 0.3) +
  geom_errorbar(aes(ymin = percentage_lower, 
                    ymax = percentage_upper),
                width = 0.2) +
  geom_signif(
    comparisons = comparisons,
    test = function(x, y) {
      # Get the current facet's data
      current_data <- malignancy_plot_data %>%
        filter(cohort_type %in% c(x, y))
      
      if(nrow(current_data) < 2) return(list(p.value = 1))
      
      p_val <- calc_chisq_pvalue(current_data, unique(x), unique(y))
      list(p.value = p_val)
    },
    map_signif_level = c("***" = 0.001, "**" = 0.01, "*" = 0.05, "ns" = 1),
    step_increase = 0.08,
    tip_length = 0.01,
    size = 0.4,
    textsize = 3
  ) +
  facet_grid(risk_status ~ auc_target,
             labeller = labeller(
               auc_target = function(x) paste0("AUC: ", x),
               risk_status = function(x) paste0("Risk: ", x)
             )) +
  labs(
    title = "Expected Lifetime Malignancies by AUC Target and Risk Status",
    x = "Cohort Type",
    y = "Expected Lifetime Malignancies (%)",
    fill = "Cohort Type",
    caption = "Pairwise chi-squared tests; * p<0.05, ** p<0.01, *** p<0.001, ns = not significant"
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.25))) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.background = element_rect(fill = "lightblue", color = "black"),
    strip.text = element_text(face = "bold", size = 10),
    panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5),
    panel.spacing = unit(0.5, "lines"),
    legend.position = "bottom"
  )

## 6.3 Plot ####
### 6.3.1 All Patients subdivided by Risk ####
risk_smoothed <- risk_combined %>%
  select(age, risk_status, cohort_type, auc_target, cumulative_lifetime_risk, ci_lower_95, ci_upper_95) %>%
  group_by(risk_status, cohort_type, auc_target) %>%
  arrange(age) %>%
  summarise(
    y = loess(cumulative_lifetime_risk ~ age, data = cur_data(), span = 0.6)$fitted,
    ymin = loess(ci_lower_95 ~ age, data = cur_data(), span = 0.6)$fitted,
    ymax = loess(ci_upper_95 ~ age, data = cur_data(), span = 0.6)$fitted,
    age = age,
    .groups = "drop"
  )

malignancy_risk_plot <- risk_smoothed %>% 
  ggplot(aes(x = age, y = y, color = risk_status, fill = risk_status)) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.2, color = NA) +
  geom_line(size = 0.8) +
  facet_grid(
    cohort_type ~ auc_target,
    labeller = labeller(
      auc_target = label_value,  # Changed this line
      cohort_type = label_wrap_gen(10)
    )
  ) +
  scale_color_manual(values = c("All" = "#d32f2f", "High Risk" = "#1976d2", "Low Risk" = "#2e7d32")) +
  scale_fill_manual(values = c("All" = "#d32f2f", "High Risk" = "#1976d2", "Low Risk" = "#2e7d32")) +
  labs(
    x = "Age at First Imaging (years)",
    y = "Excess Additional Lifetime Risk of Malignancy",
    color = "Risk Status",
    fill = "Risk Status",
    title = "Lifetime Malignancy Risk"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 9),
    legend.position = "bottom",
    panel.spacing = unit(0.5, "lines"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    strip.background = element_rect(color = "black", fill = "#f0f0f0", linewidth = 0.8)
  ) +
  xlim(10, 85) + ylim(0, 1.5)

print(malignancy_risk_plot)

## 6.4 Run function subdivided by Recurrence status ####
### 6.4.1 All patients ####
risk_all_with_sf_status_sf <- rad_data %>%
  mutate(cohort_type_recurrence = paste0(cohort_type,"_recurrence_",recurrence)) %>%
  group_by(age, sex, risk_status, auc_target, cohort_type_recurrence) %>%
  summarise(
    mean_dose_year_1 = mean(rad_dose_year_1, na.rm = TRUE),
    mean_dose_year_2 = mean(rad_dose_year_2, na.rm = TRUE),
    mean_dose_year_3 = mean(rad_dose_year_3, na.rm = TRUE),
    mean_dose_year_4 = mean(rad_dose_year_4, na.rm = TRUE),
    mean_dose_year_5 = mean(rad_dose_year_5, na.rm = TRUE),
    start_year = first(year),
    n_patients = n(),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    risk_status = "All",  
    mc_results = list(monte_carlo_lifetime_risk(
      age = age,
      sex = sex,
      doses = c(mean_dose_year_1, mean_dose_year_2, mean_dose_year_3,
                mean_dose_year_4, mean_dose_year_5),
      start_year = start_year,
      auc_target = auc_target,
      n_simulations = 100
    ))
  ) %>%
  ungroup() %>%
  mutate(
    cumulative_lifetime_risk = map_dbl(mc_results, "mean_risk"),
    ci_lower_95 = map_dbl(mc_results, "ci_lower"),
    ci_upper_95 = map_dbl(mc_results, "ci_upper")
  )

### 6.4.2 Subdivided by Risk status ####
plan(multisession, workers = parallel::detectCores() - 1)

risk_summary_monte_carlo_final_with_sf_status_sf <- rad_data %>%
  mutate(cohort_type_recurrence = paste0(cohort_type, "_recurrence_", recurrence)) %>%
  group_by(age, sex, risk_status, auc_target, cohort_type_recurrence) %>%
  summarise(
    across(starts_with("rad_dose_year_"), 
           ~mean(.x, na.rm = TRUE),
           .names = "mean_{.col}"),
    start_year = first(year),
    n_patients = n(),
    .groups = "drop"
  ) %>%
  mutate(
    mc_results = future_pmap(
      list(age, sex, mean_rad_dose_year_1, mean_rad_dose_year_2,
           mean_rad_dose_year_3, mean_rad_dose_year_4, mean_rad_dose_year_5,
           start_year, auc_target),
      function(a, s, d1, d2, d3, d4, d5, sy, at) {
        monte_carlo_lifetime_risk(
          age = a, sex = s,
          doses = c(d1, d2, d3, d4, d5),
          start_year = sy, auc_target = at,
          n_simulations = 100
        )
      },
      .options = furrr_options(seed = TRUE),
      .progress = TRUE
    )
  ) %>%
  mutate(
    cumulative_lifetime_risk = future_map_dbl(
      mc_results, 
      "mean_risk",
      .options = furrr_options(seed = TRUE),
      .progress = TRUE
    ),
    ci_lower_95 = future_map_dbl(
      mc_results, 
      "ci_lower",
      .options = furrr_options(seed = TRUE),
      .progress = TRUE
    ),
    ci_upper_95 = future_map_dbl(
      mc_results, 
      "ci_upper",
      .options = furrr_options(seed = TRUE),
      .progress = TRUE
    )
  )

plan(sequential)

risk_combined_with_sf_status_sf <- bind_rows(risk_summary_monte_carlo_final_with_sf_status_sf, risk_all_with_sf_status_sf)


## 6.5 Plot ####
risk_smoothed_sf <- risk_combined_with_sf_status_sf %>%
  select(age, risk_status, cohort_type_recurrence, auc_target, cumulative_lifetime_risk, ci_lower_95, ci_upper_95) %>%
  group_by(risk_status, cohort_type_recurrence, auc_target) %>%
  arrange(age) %>%
  summarise(
    y = loess(cumulative_lifetime_risk ~ age, data = cur_data(), span = 0.6)$fitted,
    ymin = loess(ci_lower_95 ~ age, data = cur_data(), span = 0.6)$fitted,
    ymax = loess(ci_upper_95 ~ age, data = cur_data(), span = 0.6)$fitted,
    age = age,
    .groups = "drop"
  )

malignancy_risk_plot_sf <- ggplot(risk_smoothed_sf, aes(x = age, y = y, color = risk_status, fill = risk_status)) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.2, color = NA) +
  geom_line(size = 0.8) +
  facet_grid(
    cohort_type_recurrence ~ auc_target,
    labeller = labeller(
      auc_target = label_value,  # Changed this line
      cohort_type_recurrence = label_wrap_gen(10)
    )
  ) +
  scale_color_manual(values = c("All" = "#d32f2f", "High Risk" = "#1976d2", "Low Risk" = "#2e7d32")) +
  scale_fill_manual(values = c("All" = "#d32f2f", "High Risk" = "#1976d2", "Low Risk" = "#2e7d32")) +
  labs(
    x = "Age at First Imaging (years)",
    y = "Excess Additional Lifetime Risk of Malignancy",
    color = "Risk Status",
    fill = "Risk Status",
    title = "Lifetime Malignancy Risk"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 9),
    legend.position = "bottom",
    panel.spacing = unit(0.5, "lines"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    strip.background = element_rect(color = "black", fill = "#f0f0f0", linewidth = 0.8)
  ) +
  xlim(10, 85) + ylim(0, 1.5)

print(malignancy_risk_plot_sf)

# 7. Get Index Patients for Comparison ####
risk_combined_with_sf_status_sf %>% 
  filter(age %in% c(15, 25, 45, 60)) %>% 
  mutate(age = factor(age, levels = c(15, 25, 45, 60))) %>%
  subset(select = c(age, sex, auc_target, risk_status, cohort_type_recurrence, cumulative_lifetime_risk,
                    ci_lower_95, ci_upper_95)) %>%
  mutate(
    cohort_type_recurrence = case_when(
      cohort_type_recurrence == "Maximum FU, CT_recurrence_No" ~  "Max CT, Rec No",
      cohort_type_recurrence == "Maximum FU, CT_recurrence_Yes" ~  "Max CT, Rec Yes",
      cohort_type_recurrence == "Maximum FU, XR_recurrence_No" ~  "Max XR, Rec No",
      cohort_type_recurrence == "Maximum FU, XR_recurrence_Yes" ~  "Max XR, Rec Yes",
      cohort_type_recurrence == "Minimum FU, CT_recurrence_No" ~  "Min CT, Rec No",
      cohort_type_recurrence == "Minimum FU, CT_recurrence_Yes" ~ "Min CT, Rec Yes",
      cohort_type_recurrence == "Minimum FU, XR_recurrence_No" ~ "Min XR, Rec No",
      cohort_type_recurrence == "Minimum FU, XR_recurrence_Yes" ~ "Min XR, Rec Yes"
    )
  ) %>%
  group_by(age, sex, risk_status, cohort_type_recurrence, auc_target) %>%
  summarise(
    cumulative_lifetime_risk = round(mean(cumulative_lifetime_risk), digits = 2),
    ci_lower_95 = round(mean(ci_lower_95), digits = 2),
    ci_upper_95 = round(mean(ci_upper_95), digits = 2)
  ) %>%
  ggplot(aes(x = age, 
             y = cumulative_lifetime_risk, 
             color = risk_status, 
             fill = risk_status)) + 
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = ci_lower_95, ymax = ci_upper_95), 
                position = position_dodge(width = 0.8), 
                width = 0.2, linewidth = 0.6) +
  facet_grid(
    cohort_type_recurrence ~ auc_target,
    labeller = labeller(
      auc_target = label_value,
      cohort_type_recurrence = label_wrap_gen(10)
    )
  ) +
  scale_color_manual(values = c("All" = "#d32f2f", "High Risk" = "#1976d2", "Low Risk" = "#2e7d32")) +
  scale_fill_manual(values = c("All" = "#d32f2f", "High Risk" = "#1976d2", "Low Risk" = "#2e7d32")) +
  labs(
    x = "Age at First Imaging (years)",
    y = "Excess Additional Absolute Lifetime Risk of Malignancy",
    color = "Risk Status",
    fill = "Risk Status",
    title = "Lifetime Malignancy Risk for Index Patients"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 9),
    legend.position = "bottom",
    panel.spacing = unit(0.5, "lines"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),   # facet panel box
    strip.background = element_rect(color = "black", fill = "#f0f0f0", linewidth = 0.8) # strip box
  ) +
  ylim(0, 2) 


risk_combined_with_sf_status_sf %>% 
  filter(age %in% c(15, 25, 45, 60)) %>% 
  mutate(age = factor(age, levels = c(15, 25, 45, 60))) %>% 
  subset(select = c(age, sex, auc_target, risk_status, cohort_type_recurrence, cumulative_lifetime_risk,
                    ci_lower_95, ci_upper_95)) %>%
  group_by(age, sex, risk_status, cohort_type_recurrence, auc_target) %>%
  summarise(
    cumulative_lifetime_risk = round(mean(cumulative_lifetime_risk), digits = 2),
    ci_lower_95 = round(mean(ci_lower_95), digits = 2),
    ci_upper_95 = round(mean(ci_upper_95), digits = 2)
  ) %>%
  gt() %>% 
  gt_theme_espn()
