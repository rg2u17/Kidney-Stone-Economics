# 10 Clinician Time in terms of number of appointments ####
## 10.1 Function to calculate number of appointments ####
calculate_no_appts <- function(complete_pop_yr_fu,
                                     cutpoints_yr,
                                     year = 2016,
                                     auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                                     fu_type = c("min", "max"),
                                     imaging_fu_type = c("ct", "us", "xr_us"),
                                     xr_sens = 0.67, xr_spec = 0.98,
                                     us_sens = 0.54, us_spec = 0.91) {
  
  
  # Initialize list to store results for each AUC target
  results_list <- list()
  
  # Define post-operative imaging
  post_op_imaging <- ifelse(imaging_fu_type == "ct",
                            1,
                            2)
  
  # Assign follow-up appts
  fu_appts_1 <- fu_appts %>% filter(imaging_type == imaging_fu_type)
  
  year_1_lr_sf_fu_appts <- fu_appts_1$appts[1] %>% as.integer()
  year_2_lr_sf_fu_appts <- fu_appts_1$appts[2] %>% as.integer()
  year_3_lr_sf_fu_appts <- fu_appts_1$appts[3] %>% as.integer()
  year_4_lr_sf_fu_appts <- fu_appts_1$appts[4] %>% as.integer()
  year_5_lr_sf_fu_appts  <- fu_appts_1$appts[5] %>% as.integer()
  year_1_lr_less4_fu_appts <- fu_appts_1$appts[6] %>% as.integer()
  year_2_lr_less4_fu_appts <- fu_appts_1$appts[7] %>% as.integer()
  year_3_lr_less4_fu_appts <- fu_appts_1$appts[8] %>% as.integer()
  year_4_lr_less4_fu_appts <- fu_appts_1$appts[9] %>% as.integer()
  year_5_lr_less4_fu_appts <- fu_appts_1$appts[10] %>% as.integer()
  year_1_lr_more4_fu_appts <- fu_appts_1$appts[11] %>% as.integer()
  year_2_lr_more4_fu_appts <- fu_appts_1$appts[12] %>% as.integer()
  year_3_lr_more4_fu_appts <- fu_appts_1$appts[13] %>% as.integer()
  year_4_lr_more4_fu_appts <- fu_appts_1$appts[14] %>% as.integer()
  year_5_lr_more4_fu_appts <- fu_appts_1$appts[15] %>% as.integer()
  year_1_hr_sf_fu_appts_current <- fu_appts_1$appts[16] %>% as.integer()
  year_1_hr_sf_fu_appts_eau <- fu_appts_1$appts[17] %>% as.integer()
  year_2_hr_sf_fu_appts_current <- fu_appts_1$appts[18] %>% as.integer()
  year_2_hr_sf_fu_appts_eau <- fu_appts_1$appts[19] %>% as.integer()
  year_3_onwards_hr_sf_fu_appts_current <- fu_appts_1$appts[20] %>% as.integer()
  year_3_onwards_hr_sf_fu_appts_eau <- fu_appts_1$appts[21] %>% as.integer()
  year_3_hr_sf_fu_appts_current <- fu_appts_1$appts[22] %>% as.integer()
  year_3_hr_sf_fu_appts_eau <- fu_appts_1$appts[23] %>% as.integer()
  year_4_hr_sf_fu_appts_eau <- fu_appts_1$appts[24] %>% as.integer()
  year_5_hr_sf_fu_appts_eau <- fu_appts_1$appts[25] %>% as.integer()
  year_1_hr_less4_fu_appts_eau <- fu_appts_1$appts[26] %>% as.integer()
  year_2_hr_less4_fu_appts_eau <- fu_appts_1$appts[27] %>% as.integer()
  year_3_hr_less4_fu_appts_eau <- fu_appts_1$appts[28] %>% as.integer()
  year_4_hr_less4_fu_appts_eau <- fu_appts_1$appts[29] %>% as.integer()
  year_5_hr_less4_fu_appts_eau <- fu_appts_1$appts[30] %>% as.integer()
  year_1_hr_more4_fu_appts_eau <- fu_appts_1$appts[31] %>% as.integer()
  year_2_hr_more4_fu_appts_eau <- fu_appts_1$appts[32] %>% as.integer()
  year_3_hr_more4_fu_appts_eau <- fu_appts_1$appts[33] %>% as.integer()
  year_4_hr_more4_fu_appts_eau <- fu_appts_1$appts[34] %>% as.integer()
  year_5_hr_more4_fu_appts_eau <- fu_appts_1$appts[35] %>% as.integer()
  
  # Assign recurrence times
  rec_appts_colic <- 1 + post_op_imaging
  rec_appts_eswl <- 2 + post_op_imaging
  rec_appts_urs <- 1 + post_op_imaging
  rec_appts_pcnl <- 1 + post_op_imaging
  
  for (auc_target in auc_targets) {
    message("Calculating times for: ", year, ", AUC: ", auc_target, ", Follow-up Type: ", fu_type, ", Follow-up Imaging: ", imaging_fu_type, " and Post-Operative Imaging:", post_op_imaging)
    # Get cutpoint for current AUC target
    cutpoint <- (cutpoints_yr %>% filter(auc_target == !!auc_target))$cutpoint
    
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
    
    # Distribute SF status as determined by imaging
    if (imaging_fu_type == "us") {
      
      
      # Generate random numbers once
      rand_sens <- runif(nrow(complete_pop_yr_fu))
      rand_spec <- runif(nrow(complete_pop_yr_fu))
      
      complete_pop_yr_fu1 <- complete_pop_yr_fu %>%
        mutate(
          stone_free_status_original = stone_free_status,
          stone_free_status1 = case_when(
            stone_free_status_original %in% c("less4", "more4") & rand_sens <= xr_sens ~ stone_free_status_original,
            stone_free_status_original %in% c("less4", "more4") & rand_sens > xr_sens ~ "SF", 
            stone_free_status_original == "sf" & rand_spec <= xr_spec ~ "SF", 
            stone_free_status_original == "sf" & rand_spec > xr_spec ~ ifelse(
              runif(n()) <= less4_prob, "less4", "more4"
            ),
            TRUE ~ stone_free_status_original # Handle any other cases
          ),
          .keep = "all"
        )
    } else if (imaging_fu_type == "xr_us") {
      rand_sens <- runif(nrow(complete_pop_yr_fu))
      rand_spec <- runif(nrow(complete_pop_yr_fu))
      
      complete_pop_yr_fu1 <- complete_pop_yr_fu %>%
        mutate(
          stone_free_status_original = stone_free_status,
          stone_free_status1 = case_when(
            stone_free_status_original %in% c("less4", "more4") & lucency == "No" & rand_sens <= xr_sens ~ stone_free_status_original,
            stone_free_status_original %in% c("less4", "more4") & lucency == "No" & rand_sens > xr_sens ~ "SF", 
            stone_free_status_original %in% c("less4", "more4") & lucency == "Yes" & rand_sens <= us_sens ~ stone_free_status_original,
            stone_free_status_original %in% c("less4", "more4") & lucency == "Yes" & rand_sens > us_sens ~ "SF", 
            stone_free_status_original == "sf" & lucency == "No" & rand_spec <= xr_spec ~ "SF", 
            stone_free_status_original == "sf" & lucency == "Yes" & rand_spec <= us_spec ~ "SF", 
            stone_free_status_original == "sf" & lucency == "No" & rand_spec > xr_spec ~ ifelse(
              runif(n()) <= less4_prob, "less4", "more4"
            ),
            stone_free_status_original == "sf" & lucency == "Yes" & rand_spec > us_spec ~ ifelse(
              runif(n()) <= less4_prob, "less4", "more4"
            ),
            TRUE ~ stone_free_status_original
          ),
          .keep = "all"
        )
    } else {
      # For CT imaging, no sensitivity/specificity adjustment needed
      complete_pop_yr_fu1 <- complete_pop_yr_fu %>%
        mutate(
          stone_free_status_original = stone_free_status,
          stone_free_status1 = stone_free_status,
          .keep = "all"
        )
    }
    
    # SF patients with minimum follow-up strategy
    if (fu_type == "min") {
      xr_only_fu_sf <- complete_pop_yr_fu1 %>%
        filter(stone_free_status1 == "SF" & auc_target == !!auc_target) %>%
        mutate(
          risk_status = case_when(
            score < cutpoint ~ "LR",
            score >= cutpoint ~ "HR"
          ),
          appt_year_0 = case_when(
            # time of post-operative CT
            death_year_0 == "No" & recurrence_year_0 == "No" ~ post_op_imaging,
            TRUE ~ NA_real_
          ),
          appt_year_1 = case_when(
            # Yr 1 FU divided by risk status
            death_year_1 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_1_lr_sf_fu_appts,
            death_year_1 == "No" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_1_hr_sf_fu_appts_eau,
            # Death
            death_year_1 == "Yes" ~ 0,
            # Recurrence
            death_year_1 == "No" & colic_intervention_type_year_1 == "Colic" ~ rec_appts_colic, 
            death_year_1 == "No" & colic_intervention_type_year_1 == "ESWL" ~ rec_appts_eswl,
            death_year_1 == "No" & colic_intervention_type_year_1 == "URS" ~ rec_appts_urs,
            death_year_1 == "No" & colic_intervention_type_year_1 == "PCNL" ~ rec_appts_pcnl,
            TRUE ~ NA_real_
          ),
          
          appt_year_2 = case_when(
            # Yr 2 FU divided by risk status
            death_year_2 == "No" & recurrence_year_2 == "No" & risk_status == "LR" ~ year_2_lr_sf_fu_appts,
            death_year_2 == "No" & recurrence_year_2 == "No" & risk_status == "HR" ~ year_2_hr_sf_fu_appts_eau,
            # Death
            death_year_2 == "Yes" ~ 0,
            # Recurrence
            death_year_2 == "No" & colic_intervention_type_year_2 == "Colic" & recurrence_year_1 == "No" ~ rec_appts_colic, 
            death_year_2 == "No" & colic_intervention_type_year_2 == "ESWL" & recurrence_year_1 == "No" ~ rec_appts_eswl,
            death_year_2 == "No" & colic_intervention_type_year_2 == "URS" & recurrence_year_1 == "No"  ~ rec_appts_urs,
            death_year_2 == "No" & colic_intervention_type_year_2 == "PCNL" & recurrence_year_1 == "No" ~ rec_appts_pcnl,
            # Yr 1 FU for Recurrence in Yr 1
            death_year_2 == "No" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" ~ year_1_lr_sf_fu_appts,
            death_year_2 == "No" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" ~ year_1_hr_sf_fu_appts_eau,
            TRUE ~ NA_real_
          ),
          
          appt_year_3 = case_when(
            # Yr 3 FU divided by risk status
            death_year_3 == "No" & recurrence_year_3 == "No" & risk_status == "LR" ~ year_3_lr_sf_fu_appts,
            death_year_3 == "No" & recurrence_year_3 == "No" & risk_status == "HR" ~ year_3_hr_sf_fu_appts_eau,
            # Death
            death_year_3 == "Yes" | death_year_2 == "Yes" | death_year_1 == "Yes" ~ 0,
            # Recurrence
            death_year_3 == "No" & colic_intervention_type_year_3 == "Colic" & recurrence_year_2 == "No" ~ rec_appts_colic, 
            death_year_3 == "No" & colic_intervention_type_year_3 == "ESWL" & recurrence_year_2 == "No" ~ rec_appts_eswl,
            death_year_3 == "No" & colic_intervention_type_year_3 == "URS" & recurrence_year_2 == "No"  ~ rec_appts_urs,
            death_year_3 == "No" & colic_intervention_type_year_3 == "PCNL" & recurrence_year_2 == "No" ~ rec_appts_pcnl,
            # Yr 1 FU for Recurrence in Yr 2
            death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_1_lr_sf_fu_appts,
            death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_1_hr_sf_fu_appts_eau,
            # Yr 2 FU for Recurrence in Yr 1
            death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" ~ year_2_lr_sf_fu_appts,
            death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" ~ year_2_hr_sf_fu_appts_eau,
            TRUE ~ NA_real_
          ),
          
          appt_year_4 = case_when(
            # Yr 4 FU divided by risk status
            death_year_4 == "No" & recurrence_year_4 == "No" & risk_status == "LR" ~ 0,
            death_year_4 == "No" & recurrence_year_4 == "No" & risk_status == "HR" ~ year_4_hr_sf_fu_appts_eau,
            # Death
            death_year_4 == "Yes" | death_year_3 == "Yes" | death_year_2 == "Yes" | death_year_1 == "Yes" ~ 0,
            # Recurrence
            death_year_4 == "No" & colic_intervention_type_year_4 == "Colic" & recurrence_year_3 == "No" ~ rec_appts_colic, 
            death_year_4 == "No" & colic_intervention_type_year_4 == "ESWL" & recurrence_year_3 == "No" ~ rec_appts_eswl,
            death_year_4 == "No" & colic_intervention_type_year_4 == "URS" & recurrence_year_3 == "No"  ~ rec_appts_urs,
            death_year_4 == "No" & colic_intervention_type_year_4 == "PCNL" & recurrence_year_3 == "No" ~ rec_appts_pcnl,
            # Yr 1 FU for Recurrence in Yr 3
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_1_lr_sf_fu_appts,
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_1_hr_sf_fu_appts_eau,
            # Yr 2 FU for Recurrence in Yr 2
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_2_lr_sf_fu_appts,
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_2_hr_sf_fu_appts_eau,
            # Yr 3 FU for Recurrence in Yr 1
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" ~ year_3_lr_sf_fu_appts,
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" ~ year_3_hr_sf_fu_appts_eau,
            TRUE ~ NA_real_
          ),
          
          appt_year_5 = case_when(
            # Yr 5 FU divided by risk status
            death_year_5 == "No" & recurrence_year_5 == "No" & risk_status == "LR" ~ 0,
            death_year_5 == "No" & recurrence_year_5 == "No" & risk_status == "HR" ~ year_5_hr_sf_fu_appts_eau,
            # Death
            death_year_5 == "Yes" | death_year_4 == "Yes" | death_year_3 == "Yes" | death_year_2 == "Yes" | death_year_1 == "Yes" ~ 0,
            # Recurrence
            death_year_4 == "No" & colic_intervention_type_year_4 == "Colic" & recurrence_year_3 == "No" ~ rec_appts_colic, 
            death_year_4 == "No" & colic_intervention_type_year_4 == "ESWL" & recurrence_year_3 == "No" ~ rec_appts_eswl,
            death_year_4 == "No" & colic_intervention_type_year_4 == "URS" & recurrence_year_3 == "No"  ~ rec_appts_urs,
            death_year_4 == "No" & colic_intervention_type_year_4 == "PCNL" & recurrence_year_3 == "No" ~ rec_appts_pcnl,
            # Yr 1 FU for Recurrence in Yr 4 
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "No" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_1_lr_sf_fu_appts,
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "No" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_1_hr_sf_fu_appts_eau,
            # Yr 2 FU for Recurrence in Yr 3
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_2_lr_sf_fu_appts,
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_2_hr_sf_fu_appts_eau,
            # Yr 3 FU for Recurrence in Yr 2
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_3_lr_sf_fu_appts,
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_3_hr_sf_fu_appts_eau,
            # Yr 4 FU for Recurrence in Yr 1
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" ~ 0,
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" ~ year_4_hr_sf_fu_appts_eau,
            TRUE ~ NA_real_
          )
        )
    } else {
      # SF patients with maximum follow-up strategy
      xr_only_fu_sf <- complete_pop_yr_fu1 %>%
        filter(stone_free_status1 == "SF" & auc_target == !!auc_target) %>%
        mutate(
          risk_status = case_when(
            score < cutpoint ~ "LR",
            score >= cutpoint ~ "HR"
          ),
          appt_year_0 = case_when(
            # time of post-operative CT
            death_year_0 == "No" & recurrence_year_0 == "No" ~ post_op_imaging,
            TRUE ~ NA_real_
          ),
          
          appt_year_1 = case_when(
            # Yr 1 FU divided by risk status
            death_year_1 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_1_lr_sf_fu_appts,
            death_year_1 == "No" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_1_hr_sf_fu_appts_eau,
            # Death
            death_year_1 == "Yes" ~ 0,
            # Recurrence
            death_year_1 == "No" & colic_intervention_type_year_1 == "Colic" ~ rec_appts_colic, 
            death_year_1 == "No" & colic_intervention_type_year_1 == "ESWL" ~ rec_appts_eswl,
            death_year_1 == "No" & colic_intervention_type_year_1 == "URS" ~ rec_appts_urs,
            death_year_1 == "No" & colic_intervention_type_year_1 == "PCNL" ~ rec_appts_pcnl,
            TRUE ~ NA_real_
          ),
          
          appt_year_2 = case_when(
            # Yr 2 FU divided by risk status
            death_year_2 == "No" & recurrence_year_2 == "No" & risk_status == "LR" ~ year_2_lr_sf_fu_appts,
            death_year_2 == "No" & recurrence_year_2 == "No" & risk_status == "HR" ~ year_2_hr_sf_fu_appts_eau,
            # Death
            death_year_2 == "Yes" | death_year_1 == "Yes" ~ 0,
            # Recurrence
            death_year_2 == "No" & colic_intervention_type_year_2 == "Colic" & recurrence_year_1 == "No" ~ rec_appts_colic, 
            death_year_2 == "No" & colic_intervention_type_year_2 == "ESWL" & recurrence_year_1 == "No" ~ rec_appts_eswl,
            death_year_2 == "No" & colic_intervention_type_year_2 == "URS" & recurrence_year_1 == "No"  ~ rec_appts_urs,
            death_year_2 == "No" & colic_intervention_type_year_2 == "PCNL" & recurrence_year_1 == "No" ~ rec_appts_pcnl,
            # Yr 1 FU for Recurrence in Yr 1
            death_year_2 == "No" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" ~ year_1_lr_sf_fu_appts,
            death_year_2 == "No" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" ~ year_1_hr_sf_fu_appts_eau,
            TRUE ~ NA_real_
          ),
          
          appt_year_3 = case_when(
            # Yr 3 FU divided by risk status
            death_year_3 == "No" & recurrence_year_3 == "No" & risk_status == "LR" ~ year_3_lr_sf_fu_appts,
            death_year_3 == "No" & recurrence_year_3 == "No" & risk_status == "HR" ~ year_3_hr_sf_fu_appts_eau,
            # Death
            death_year_3 == "Yes" | death_year_2 == "Yes" | death_year_1 == "Yes" ~ 0,
            # Recurrence
            death_year_3 == "No" & colic_intervention_type_year_3 == "Colic" & recurrence_year_2 == "No" ~ rec_appts_colic, 
            death_year_3 == "No" & colic_intervention_type_year_3 == "ESWL" & recurrence_year_2 == "No" ~ rec_appts_eswl,
            death_year_3 == "No" & colic_intervention_type_year_3 == "URS" & recurrence_year_2 == "No"  ~ rec_appts_urs,
            death_year_3 == "No" & colic_intervention_type_year_3 == "PCNL" & recurrence_year_2 == "No" ~ rec_appts_pcnl,
            # Yr 1 FU for Recurrence in Yr 2
            death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_1_lr_sf_fu_appts,
            death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_1_hr_sf_fu_appts_eau,
            # Yr 2 FU for Recurrence in Yr 1
            death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" ~ year_2_lr_sf_fu_appts,
            death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" ~ year_2_hr_sf_fu_appts_eau,
            TRUE ~ NA_real_
          ),
          
          appt_year_4 = case_when(
            # Yr 4 FU divided by risk status
            death_year_4 == "No" & recurrence_year_4 == "No" & risk_status == "LR" ~ 0,
            death_year_4 == "No" & recurrence_year_4 == "No" & risk_status == "HR" ~ year_4_hr_sf_fu_appts_eau,
            # Death
            death_year_4 == "Yes" | death_year_3 == "Yes" | death_year_2 == "Yes" | death_year_1 == "Yes" ~ 0,
            # Recurrence
            death_year_4 == "No" & colic_intervention_type_year_4 == "Colic" & recurrence_year_3 == "No" ~ rec_appts_colic, 
            death_year_4 == "No" & colic_intervention_type_year_4 == "ESWL" & recurrence_year_3 == "No" ~ rec_appts_eswl,
            death_year_4 == "No" & colic_intervention_type_year_4 == "URS" & recurrence_year_3 == "No"  ~ rec_appts_urs,
            death_year_4 == "No" & colic_intervention_type_year_4 == "PCNL" & recurrence_year_3 == "No" ~ rec_appts_pcnl,
            # Yr 1 FU for Recurrence in Yr 3
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_1_lr_sf_fu_appts,
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_1_hr_sf_fu_appts_eau,
            # Yr 2 FU for Recurrence in Yr 2
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_2_lr_sf_fu_appts,
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_2_hr_sf_fu_appts_eau,
            # Yr 3 FU for Recurrence in Yr 1
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" ~ year_3_lr_sf_fu_appts,
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" ~ year_3_hr_sf_fu_appts_eau,
            TRUE ~ NA_real_
          ),
          
          appt_year_5 = case_when(
            # Yr 5 FU divided by risk status
            death_year_5 == "No" & recurrence_year_5 == "No" & risk_status == "LR" ~ year_5_lr_sf_fu_appts,
            death_year_5 == "No" & recurrence_year_5 == "No" & risk_status == "HR" ~ year_5_hr_sf_fu_appts_eau,
            # Death
            death_year_5 == "Yes" | death_year_4 == "Yes" | death_year_3 == "Yes" | death_year_2 == "Yes" | death_year_1 == "Yes" ~ 0,
            # Recurrence
            death_year_4 == "No" & colic_intervention_type_year_4 == "Colic" & recurrence_year_3 == "No" ~ rec_appts_colic, 
            death_year_4 == "No" & colic_intervention_type_year_4 == "ESWL" & recurrence_year_3 == "No" ~ rec_appts_eswl,
            death_year_4 == "No" & colic_intervention_type_year_4 == "URS" & recurrence_year_3 == "No"  ~ rec_appts_urs,
            death_year_4 == "No" & colic_intervention_type_year_4 == "PCNL" & recurrence_year_3 == "No" ~ rec_appts_pcnl,
            # Yr 1 FU for Recurrence in Yr 4 
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "No" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_1_lr_sf_fu_appts,
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "No" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_1_hr_sf_fu_appts_eau,
            # Yr 2 FU for Recurrence in Yr 3
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_2_lr_sf_fu_appts,
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_2_hr_sf_fu_appts_eau,
            # Yr 3 FU for Recurrence in Yr 2
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_3_lr_sf_fu_appts,
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_3_hr_sf_fu_appts_eau,
            # Yr 4 FU for Recurrence in Yr 1
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" ~ year_4_lr_sf_fu_appts,
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" ~ year_4_hr_sf_fu_appts_eau,
            TRUE ~ NA_real_
          )
        )
    }
    
    # Less than 4mm stones
    xr_only_fu_less4 <- complete_pop_yr_fu1 %>% 
      filter(stone_free_status1 == "less4" & auc_target == !!auc_target) %>%
      mutate(
        risk_status = case_when(
          score < cutpoint ~ "LR",
          score >= cutpoint ~ "HR"
        ),
        appt_year_0 = case_when(
          # time of post-operative CT
          death_year_0 == "No" & recurrence_year_0 == "No" ~ post_op_imaging,
          TRUE ~ NA_real_
        ),
        
        appt_year_1 = case_when(
          # Yr 1 FU divided by risk status
          death_year_1 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_1_lr_less4_fu_appts,
          death_year_1 == "No" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_1_hr_less4_fu_appts_eau,
          # Death
          death_year_1 == "Yes" ~ 0,
          # Recurrence
          death_year_1 == "No" & colic_intervention_type_year_1 == "Colic" ~ rec_appts_colic, 
          death_year_1 == "No" & colic_intervention_type_year_1 == "ESWL" ~ rec_appts_eswl,
          death_year_1 == "No" & colic_intervention_type_year_1 == "URS" ~ rec_appts_urs,
          death_year_1 == "No" & colic_intervention_type_year_1 == "PCNL" ~ rec_appts_pcnl,
          TRUE ~ NA_real_
        ),
        
        appt_year_2 = case_when(
          # Yr 2 FU divided by risk status
          death_year_2 == "No" & recurrence_year_2 == "No" & risk_status == "LR" ~ year_2_lr_less4_fu_appts,
          death_year_2 == "No" & recurrence_year_2 == "No" & risk_status == "HR" ~ year_2_hr_less4_fu_appts_eau,
          # Death
          death_year_2 == "Yes" ~ 0,
          # Recurrence
          death_year_2 == "No" & colic_intervention_type_year_2 == "Colic" & recurrence_year_1 == "No" ~ rec_appts_colic, 
          death_year_2 == "No" & colic_intervention_type_year_2 == "ESWL" & recurrence_year_1 == "No" ~ rec_appts_eswl,
          death_year_2 == "No" & colic_intervention_type_year_2 == "URS" & recurrence_year_1 == "No"  ~ rec_appts_urs,
          death_year_2 == "No" & colic_intervention_type_year_2 == "PCNL" & recurrence_year_1 == "No" ~ rec_appts_pcnl,
          # Yr 1 FU for Recurrence in Yr 1
          death_year_2 == "No" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" ~ year_1_lr_less4_fu_appts,
          death_year_2 == "No" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" ~ year_1_hr_less4_fu_appts_eau,
          TRUE ~ NA_real_
        ),
        
        appt_year_3 = case_when(
          # Yr 3 FU divided by risk status
          death_year_3 == "No" & recurrence_year_3 == "No" & risk_status == "LR" ~ year_3_lr_less4_fu_appts,
          death_year_3 == "No" & recurrence_year_3 == "No" & risk_status == "HR" ~ year_3_hr_less4_fu_appts_eau,
          # Death
          death_year_3 == "Yes" ~ 0,
          # Recurrence
          death_year_3 == "No" & colic_intervention_type_year_3 == "Colic" & recurrence_year_2 == "No" ~ rec_appts_colic, 
          death_year_3 == "No" & colic_intervention_type_year_3 == "ESWL" & recurrence_year_2 == "No" ~ rec_appts_eswl,
          death_year_3 == "No" & colic_intervention_type_year_3 == "URS" & recurrence_year_2 == "No"  ~ rec_appts_urs,
          death_year_3 == "No" & colic_intervention_type_year_3 == "PCNL" & recurrence_year_2 == "No" ~ rec_appts_pcnl,
          # Yr 1 FU for Recurrence in Yr 2
          death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_1_lr_less4_fu_appts,
          death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_1_hr_less4_fu_appts_eau,
          # Yr 2 FU for Recurrence in Yr 1
          death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" ~ year_2_lr_less4_fu_appts,
          death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" ~ year_2_hr_less4_fu_appts_eau,
          TRUE ~ NA_real_
        ),
        
        appt_year_4 = case_when(
          # Yr 4 FU divided by risk status
          death_year_4 == "No" & recurrence_year_4 == "No" & risk_status == "LR" ~ 0,
          death_year_4 == "No" & recurrence_year_4 == "No" & risk_status == "HR" ~ year_4_hr_less4_fu_appts_eau,
          # Death
          death_year_4 == "Yes" ~ 0,
          # Recurrence
          death_year_4 == "No" & colic_intervention_type_year_4 == "Colic" & recurrence_year_3 == "No" ~ rec_appts_colic, 
          death_year_4 == "No" & colic_intervention_type_year_4 == "ESWL" & recurrence_year_3 == "No" ~ rec_appts_eswl,
          death_year_4 == "No" & colic_intervention_type_year_4 == "URS" & recurrence_year_3 == "No"  ~ rec_appts_urs,
          death_year_4 == "No" & colic_intervention_type_year_4 == "PCNL" & recurrence_year_3 == "No" ~ rec_appts_pcnl,
          # Yr 1 FU for Recurrence in Yr 3
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_1_lr_less4_fu_appts,
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_1_hr_less4_fu_appts_eau,
          # Yr 2 FU for Recurrence in Yr 2
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_2_lr_less4_fu_appts,
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_2_hr_less4_fu_appts_eau,
          # Yr 3 FU for Recurrence in Yr 1
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" ~ year_3_lr_sf_fu_appts,
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" ~ year_3_hr_sf_fu_appts_eau,
          TRUE ~ NA_real_
        ),
        
        appt_year_5 = case_when(
          # Yr 5 FU divided by risk status
          death_year_5 == "No" & recurrence_year_5 == "No" & risk_status == "LR" ~ year_5_lr_less4_fu_appts,
          death_year_5 == "No" & recurrence_year_5 == "No" & risk_status == "HR" ~ year_5_hr_less4_fu_appts_eau,
          # Death
          death_year_5 == "Yes" ~ 0,
          # Recurrence
          death_year_4 == "No" & colic_intervention_type_year_4 == "Colic" & recurrence_year_3 == "No" ~ rec_appts_colic, 
          death_year_4 == "No" & colic_intervention_type_year_4 == "ESWL" & recurrence_year_3 == "No" ~ rec_appts_eswl,
          death_year_4 == "No" & colic_intervention_type_year_4 == "URS" & recurrence_year_3 == "No"  ~ rec_appts_urs,
          death_year_4 == "No" & colic_intervention_type_year_4 == "PCNL" & recurrence_year_3 == "No" ~ rec_appts_pcnl,
          # Yr 1 FU for Recurrence in Yr 4 
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "No" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_1_lr_less4_fu_appts,
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "No" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_1_hr_less4_fu_appts_eau,
          # Yr 2 FU for Recurrence in Yr 3
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_2_lr_less4_fu_appts,
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_2_hr_less4_fu_appts_eau,
          # Yr 3 FU for Recurrence in Yr 2
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_3_lr_less4_fu_appts,
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_3_hr_less4_fu_appts_eau,
          # Yr 4 FU for Recurrence in Yr 1
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" ~ year_4_lr_less4_fu_appts,
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" ~ year_4_hr_less4_fu_appts_eau,
          TRUE ~ NA_real_
        )
      )
    
    # More than 4mm stones
    xr_only_fu_more4 <- complete_pop_yr_fu1 %>% 
      filter(stone_free_status1 == "more4" & auc_target == !!auc_target) %>%
      mutate(
        risk_status = case_when(
          score < cutpoint ~ "LR",
          score >= cutpoint ~ "HR"
        ),
        appt_year_0 = case_when(
          # time of post-operative CT
          death_year_0 == "No" & recurrence_year_0 == "No" ~ post_op_imaging,
          TRUE ~ NA_real_
        ),
        
        appt_year_1 = case_when(
          # Yr 1 FU divided by risk status
          death_year_1 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_1_lr_more4_fu_appts,
          death_year_1 == "No" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_1_hr_more4_fu_appts_eau,
          # Death
          death_year_1 == "Yes" ~ 0,
          # Recurrence
          death_year_1 == "No" & colic_intervention_type_year_1 == "Colic" ~ rec_appts_colic, 
          death_year_1 == "No" & colic_intervention_type_year_1 == "ESWL" ~ rec_appts_eswl,
          death_year_1 == "No" & colic_intervention_type_year_1 == "URS" ~ rec_appts_urs,
          death_year_1 == "No" & colic_intervention_type_year_1 == "PCNL" ~ rec_appts_pcnl,
          TRUE ~ NA_real_
        ),
        
        appt_year_2 = case_when(
          # Yr 2 FU divided by risk status
          death_year_2 == "No" & recurrence_year_2 == "No" & risk_status == "LR" ~ year_2_lr_more4_fu_appts,
          death_year_2 == "No" & recurrence_year_2 == "No" & risk_status == "HR" ~ year_2_hr_more4_fu_appts_eau,
          # Death
          death_year_2 == "Yes" ~ 0,
          # Recurrence
          death_year_2 == "No" & colic_intervention_type_year_2 == "Colic" & recurrence_year_1 == "No" ~ rec_appts_colic, 
          death_year_2 == "No" & colic_intervention_type_year_2 == "ESWL" & recurrence_year_1 == "No" ~ rec_appts_eswl,
          death_year_2 == "No" & colic_intervention_type_year_2 == "URS" & recurrence_year_1 == "No"  ~ rec_appts_urs,
          death_year_2 == "No" & colic_intervention_type_year_2 == "PCNL" & recurrence_year_1 == "No" ~ rec_appts_pcnl,
          # Yr 1 FU for Recurrence in Yr 1
          death_year_2 == "No" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" ~ year_1_lr_more4_fu_appts,
          death_year_2 == "No" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" ~ year_1_hr_more4_fu_appts_eau,
          TRUE ~ NA_real_
        ),
        
        appt_year_3 = case_when(
          # Yr 3 FU divided by risk status
          death_year_3 == "No" & recurrence_year_3 == "No" & risk_status == "LR" ~ year_3_lr_more4_fu_appts,
          death_year_3 == "No" & recurrence_year_3 == "No" & risk_status == "HR" ~ year_3_hr_more4_fu_appts_eau,
          # Death
          death_year_3 == "Yes" ~ 0,
          # Recurrence
          death_year_3 == "No" & colic_intervention_type_year_3 == "Colic" & recurrence_year_2 == "No" ~ rec_appts_colic, 
          death_year_3 == "No" & colic_intervention_type_year_3 == "ESWL" & recurrence_year_2 == "No" ~ rec_appts_eswl,
          death_year_3 == "No" & colic_intervention_type_year_3 == "URS" & recurrence_year_2 == "No"  ~ rec_appts_urs,
          death_year_3 == "No" & colic_intervention_type_year_3 == "PCNL" & recurrence_year_2 == "No" ~ rec_appts_pcnl,
          # Yr 1 FU for Recurrence in Yr 2
          death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_1_lr_more4_fu_appts,
          death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_1_hr_more4_fu_appts_eau,
          # Yr 2 FU for Recurrence in Yr 1
          death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" ~ year_2_lr_more4_fu_appts,
          death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" ~ year_2_hr_more4_fu_appts_eau,
          TRUE ~ NA_real_
        ),
        
        appt_year_4 = case_when(
          # Yr 4 FU divided by risk status
          death_year_4 == "No" & recurrence_year_4 == "No" & risk_status == "LR" ~ 0,
          death_year_4 == "No" & recurrence_year_4 == "No" & risk_status == "HR" ~ year_4_hr_more4_fu_appts_eau,
          # Death
          death_year_4 == "Yes" ~ 0,
          # Recurrence
          death_year_4 == "No" & colic_intervention_type_year_4 == "Colic" & recurrence_year_3 == "No" ~ rec_appts_colic, 
          death_year_4 == "No" & colic_intervention_type_year_4 == "ESWL" & recurrence_year_3 == "No" ~ rec_appts_eswl,
          death_year_4 == "No" & colic_intervention_type_year_4 == "URS" & recurrence_year_3 == "No"  ~ rec_appts_urs,
          death_year_4 == "No" & colic_intervention_type_year_4 == "PCNL" & recurrence_year_3 == "No" ~ rec_appts_pcnl,
          # Yr 1 FU for Recurrence in Yr 3
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_1_lr_more4_fu_appts,
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_1_hr_more4_fu_appts_eau,
          # Yr 2 FU for Recurrence in Yr 2
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_2_lr_more4_fu_appts,
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_2_hr_more4_fu_appts_eau,
          # Yr 3 FU for Recurrence in Yr 1
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" ~ year_3_lr_sf_fu_appts,
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" ~ year_3_hr_sf_fu_appts_eau,
          TRUE ~ NA_real_
        ),
        
        appt_year_5 = case_when(
          # Yr 5 FU divided by risk status
          death_year_5 == "No" & recurrence_year_5 == "No" & risk_status == "LR" ~ year_5_lr_more4_fu_appts,
          death_year_5 == "No" & recurrence_year_5 == "No" & risk_status == "HR" ~ year_5_hr_more4_fu_appts_eau,
          # Death
          death_year_5 == "Yes" ~ 0,
          # Recurrence
          death_year_4 == "No" & colic_intervention_type_year_4 == "Colic" & recurrence_year_3 == "No" ~ rec_appts_colic, 
          death_year_4 == "No" & colic_intervention_type_year_4 == "ESWL" & recurrence_year_3 == "No" ~ rec_appts_eswl,
          death_year_4 == "No" & colic_intervention_type_year_4 == "URS" & recurrence_year_3 == "No"  ~ rec_appts_urs,
          death_year_4 == "No" & colic_intervention_type_year_4 == "PCNL" & recurrence_year_3 == "No" ~ rec_appts_pcnl,
          # Yr 1 FU for Recurrence in Yr 4 
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "No" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_1_lr_more4_fu_appts,
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "No" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_1_hr_more4_fu_appts_eau,
          # Yr 2 FU for Recurrence in Yr 3
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_2_lr_more4_fu_appts,
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_2_hr_more4_fu_appts_eau,
          # Yr 3 FU for Recurrence in Yr 2
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_3_lr_more4_fu_appts,
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_3_hr_more4_fu_appts_eau,
          # Yr 4 FU for Recurrence in Yr 1
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" ~ year_4_lr_more4_fu_appts,
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" ~ year_4_hr_more4_fu_appts_eau,
          TRUE ~ NA_real_
        )
      )
    
    # Combine all stone status groups for this AUC target
    combined_result <- rbind(xr_only_fu_sf, xr_only_fu_less4, xr_only_fu_more4) %>% 
      as_tibble() %>%
      mutate(auc_target = auc_target, 
             cutpoint = cutpoint, 
             post_op_imaging = post_op_imaging,
             imaging_fu_type = imaging_fu_type,
             year = year)
    
    # Store result in list with named element
    results_list[[paste0("auc_", auc_target)]] <- combined_result
  }
  
  # Return results
  if (length(auc_targets) == 1) {
    return(results_list[[1]])
  } else {
    return(results_list)
  }
}

## 10.2 Run function for each year ####
### 10.2.1. 2016 ####
appts_2016_us_min <- calculate_no_appts(
  complete_pop_yr_fu = complete_pop_2016_fu,
  cutpoints_yr = cutpoints_2016,
  year = 2016,
  auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  imaging_fu_type = "us"
)

appts_2016_us_max <- calculate_no_appts(
  complete_pop_yr_fu = complete_pop_2016_fu,
  cutpoints_yr = cutpoints_2016,
  year = 2016,
  auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  imaging_fu_type = "us"
)

appts_2016_xr_us_min <- calculate_no_appts(
  complete_pop_yr_fu = complete_pop_2016_fu,
  cutpoints_yr = cutpoints_2016,
  year = 2016,
  auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  imaging_fu_type = "xr_us"
)

appts_2016_xr_us_max <- calculate_no_appts(
  complete_pop_yr_fu = complete_pop_2016_fu,
  cutpoints_yr = cutpoints_2016,
  year = 2016,
  auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  imaging_fu_type = "xr_us"
)

appts_2016_ct_min <- calculate_no_appts(
  complete_pop_yr_fu = complete_pop_2016_fu,
  cutpoints_yr = cutpoints_2016,
  year = 2016,
  auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  imaging_fu_type = "ct"
)

appts_2016_ct_max <- calculate_no_appts(
  complete_pop_yr_fu = complete_pop_2016_fu,
  cutpoints_yr = cutpoints_2016,
  year = 2016,
  auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  imaging_fu_type = "ct"
)

### 10.2.2 2017 ####
appts_2017_us_min <- calculate_no_appts(
  complete_pop_yr_fu = complete_pop_2017_fu,
  cutpoints_yr = cutpoints_2017,
  year = 2017,
  auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  
  imaging_fu_type = "us"
)

appts_2017_us_max <- calculate_no_appts(
  complete_pop_yr_fu = complete_pop_2017_fu,
  cutpoints_yr = cutpoints_2017,
  year = 2017,
  auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  
  imaging_fu_type = "us"
)

appts_2017_xr_us_min <- calculate_no_appts(
  complete_pop_yr_fu = complete_pop_2017_fu,
  cutpoints_yr = cutpoints_2017,
  year = 2017,
  auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  
  imaging_fu_type = "xr_us"
)

appts_2017_xr_us_max <- calculate_no_appts(
  complete_pop_yr_fu = complete_pop_2017_fu,
  cutpoints_yr = cutpoints_2017,
  year = 2017,
  auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  
  imaging_fu_type = "xr_us"
)

appts_2017_ct_min <- calculate_no_appts(
  complete_pop_yr_fu = complete_pop_2017_fu,
  cutpoints_yr = cutpoints_2017,
  year = 2017,
  auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  
  imaging_fu_type = "ct"
)

appts_2017_ct_max <- calculate_no_appts(
  complete_pop_yr_fu = complete_pop_2017_fu,
  cutpoints_yr = cutpoints_2017,
  year = 2017,
  auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  
  imaging_fu_type = "ct"
)

### 10.2.3 2018 ####
appts_2018_us_min <- calculate_no_appts(
  complete_pop_yr_fu = complete_pop_2018_fu,
  cutpoints_yr = cutpoints_2018,
  year = 2018,
  auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  
  imaging_fu_type = "us"
)

appts_2018_us_max <- calculate_no_appts(
  complete_pop_yr_fu = complete_pop_2018_fu,
  cutpoints_yr = cutpoints_2018,
  year = 2018,
  auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  
  imaging_fu_type = "us"
)

appts_2018_xr_us_min <- calculate_no_appts(
  complete_pop_yr_fu = complete_pop_2018_fu,
  cutpoints_yr = cutpoints_2018,
  year = 2018,
  auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  
  imaging_fu_type = "xr_us"
)

appts_2018_xr_us_max <- calculate_no_appts(
  complete_pop_yr_fu = complete_pop_2018_fu,
  cutpoints_yr = cutpoints_2018,
  year = 2018,
  auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  
  imaging_fu_type = "xr_us"
)

appts_2018_ct_min <- calculate_no_appts(
  complete_pop_yr_fu = complete_pop_2018_fu,
  cutpoints_yr = cutpoints_2018,
  year = 2018,
  auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  
  imaging_fu_type = "ct"
)

appts_2018_ct_max <- calculate_no_appts(
  complete_pop_yr_fu = complete_pop_2018_fu,
  cutpoints_yr = cutpoints_2018,
  year = 2018,
  auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  
  imaging_fu_type = "ct"
)

## 10.2.4 2019 ####
appts_2019_us_min <- calculate_no_appts(
  complete_pop_yr_fu = complete_pop_2019_fu,
  cutpoints_yr = cutpoints_2019,
  year = 2019,
  auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  
  imaging_fu_type = "us"
)

appts_2019_us_max <- calculate_no_appts(
  complete_pop_yr_fu = complete_pop_2019_fu,
  cutpoints_yr = cutpoints_2019,
  year = 2019,
  auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  
  imaging_fu_type = "us"
)

appts_2019_xr_us_min <- calculate_no_appts(
  complete_pop_yr_fu = complete_pop_2019_fu,
  cutpoints_yr = cutpoints_2019,
  year = 2019,
  auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  
  imaging_fu_type = "xr_us"
)

appts_2019_xr_us_max <- calculate_no_appts(
  complete_pop_yr_fu = complete_pop_2019_fu,
  cutpoints_yr = cutpoints_2019,
  year = 2019,
  auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  
  imaging_fu_type = "xr_us"
)

appts_2019_ct_min <- calculate_no_appts(
  complete_pop_yr_fu = complete_pop_2019_fu,
  cutpoints_yr = cutpoints_2019,
  year = 2019,
  auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  
  imaging_fu_type = "ct"
)

appts_2019_ct_max <- calculate_no_appts(
  complete_pop_yr_fu = complete_pop_2019_fu,
  cutpoints_yr = cutpoints_2019,
  year = 2019,
  auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  
  imaging_fu_type = "ct"
)

### 10.2.5 2020 ####
appts_2020_us_min <- calculate_no_appts(
  complete_pop_yr_fu = complete_pop_2020_fu,
  cutpoints_yr = cutpoints_2020,
  year = 2020,
  auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  
  imaging_fu_type = "us"
)

appts_2020_us_max <- calculate_no_appts(
  complete_pop_yr_fu = complete_pop_2020_fu,
  cutpoints_yr = cutpoints_2020,
  year = 2020,
  auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  
  imaging_fu_type = "us"
)

appts_2020_xr_us_min <- calculate_no_appts(
  complete_pop_yr_fu = complete_pop_2020_fu,
  cutpoints_yr = cutpoints_2020,
  year = 2020,
  auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  
  imaging_fu_type = "xr_us"
)

appts_2020_xr_us_max <- calculate_no_appts(
  complete_pop_yr_fu = complete_pop_2020_fu,
  cutpoints_yr = cutpoints_2020,
  year = 2020,
  auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  
  imaging_fu_type = "xr_us"
)

appts_2020_ct_min <- calculate_no_appts(
  complete_pop_yr_fu = complete_pop_2020_fu,
  cutpoints_yr = cutpoints_2020,
  year = 2020,
  auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  
  imaging_fu_type = "ct"
)

appts_2020_ct_max <- calculate_no_appts(
  complete_pop_yr_fu = complete_pop_2020_fu,
  cutpoints_yr = cutpoints_2020,
  year = 2020,
  auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "max",
  
  imaging_fu_type = "ct"
)

## 10.3 Amalgamate Each Year ####
### 10.3.1 Aggregation function ####
aggregate_appt_cohorts <- function(auc_target = c(1,2,3,4,5,6,7,8,9)) {
  all_cohorts <- list()
  
  for (i in auc_target) {
    key <- as.integer(i)
    message("Processing AUC = ", key)
    
    message("  Loading 2016 data...")
    cohort_2016_min_us <- appts_2016_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, US", auc = i)
    cohort_2016_min_ct <- appts_2016_ct_min[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2016_max_us <- appts_2016_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, US", auc = i)
    cohort_2016_max_ct <- appts_2016_ct_max[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    cohort_2016_min_xr_us <- appts_2016_xr_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2016_max_xr_us <- appts_2016_xr_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    
    message("  Loading 2017 data...")
    cohort_2017_min_us <- appts_2017_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, US", auc = i)
    cohort_2017_min_ct <- appts_2017_ct_min[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2017_max_us <- appts_2017_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, US", auc = i)
    cohort_2017_max_ct <- appts_2017_ct_max[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    cohort_2017_min_xr_us <- appts_2017_xr_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2017_max_xr_us <- appts_2017_xr_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    
    message("  Loading 2018 data...")
    cohort_2018_min_us <- appts_2018_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, US", auc = i)
    cohort_2018_min_ct <- appts_2018_ct_min[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2018_max_us <- appts_2018_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, US", auc = i)
    cohort_2018_max_ct <- appts_2018_ct_max[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    cohort_2018_min_xr_us <- appts_2018_xr_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2018_max_xr_us <- appts_2018_xr_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    
    message("  Loading 2019 data...")
    cohort_2019_min_us <- appts_2019_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, US", auc = i)
    cohort_2019_min_ct <- appts_2019_ct_min[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2019_max_us <- appts_2019_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, US", auc = i)
    cohort_2019_max_ct <- appts_2019_ct_max[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    cohort_2019_min_xr_us <- appts_2019_xr_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2019_max_xr_us <- appts_2019_xr_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    
    message("  Loading 2020 data...")
    cohort_2020_min_us <- appts_2020_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, US", auc = i)
    cohort_2020_min_ct <- appts_2020_ct_min[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2020_max_us <- appts_2020_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, US", auc = i)
    cohort_2020_max_ct <- appts_2020_ct_max[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    cohort_2020_min_xr_us <- appts_2020_xr_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2020_max_xr_us <- appts_2020_xr_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    
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

### 10.3.2 AUC 0.55 ####
auc_0.55 <- aggregate_appt_cohorts(auc_target = 1)

### 10.3.3 AUC 0.6
auc_0.6 <- aggregate_appt_cohorts(auc_target = 2)

### 10.3.2 AUC 0.65 ####
auc_0.65 <- aggregate_appt_cohorts(auc_target = 3)

### 10.3.3 AUC 0.7
auc_0.7 <- aggregate_appt_cohorts(auc_target = 4)

### 10.3.2 AUC 0.75 ####
auc_0.75 <- aggregate_appt_cohorts(auc_target = 5)

### 10.3.3 AUC 0.8
auc_0.8 <- aggregate_appt_cohorts(auc_target = 6)

### 10.3.2 AUC 0.85 ####
auc_0.85 <- aggregate_appt_cohorts(auc_target = 7)

### 10.3.3 AUC 0.9
auc_0.9 <- aggregate_appt_cohorts(auc_target = 8)

### 10.3.2 AUC 0.95 ####
auc_0.95 <- aggregate_appt_cohorts(auc_target = 9)

## 10.4 Compare length of FU + type of imaging in terms of number of appts
### 10.4.1 Function to combine data
combine_auc_data <- function(data, auc_label) {
  data %>%
    mutate(
      auc_label = auc_label,
      risk_status = ifelse(risk_status == "LR", "Low Risk", "High Risk"),
      annual_appts = case_when(
        year == 2020 ~ appt_year_1,
        year == 2019 ~ appt_year_2,
        year == 2018 ~ appt_year_3,
        year == 2017 ~ appt_year_4,
        year == 2016 ~ appt_year_5,
        TRUE ~ NA_real_
      )
    ) %>%
    select(auc_label, cohort_type, stone_free_status, risk_status, annual_appts, true_rec_5yr)
}

### 10.4.2 Run function ####
# Combine all AUC datasets and filter cohort_types
data_for_plot <- bind_rows(
  combine_auc_data(auc_0.55, "AUC 0.55"),
  combine_auc_data(auc_0.6,  "AUC 0.6"),
  combine_auc_data(auc_0.65, "AUC 0.65"),
  combine_auc_data(auc_0.7,  "AUC 0.7"),
  combine_auc_data(auc_0.75, "AUC 0.75"),
  combine_auc_data(auc_0.8,  "AUC 0.8"),
  combine_auc_data(auc_0.85, "AUC 0.85"),
  combine_auc_data(auc_0.9,  "AUC 0.9"),
  combine_auc_data(auc_0.95, "AUC 0.95")
)

data_for_plot$auc_label <- as.factor(data_for_plot$auc_label)
data_for_plot$cohort_type <- as.factor(data_for_plot$cohort_type)
data_for_plot$risk_status <- as.factor(data_for_plot$risk_status)
data_for_plot$true_rec_5yr <- as.factor(data_for_plot$true_rec_5yr)

# Summarise number of appts for single year by auc_label, risk_status, cohort_type
summary_df <- data_for_plot %>%
  group_by(auc_label, risk_status, cohort_type) %>%
  summarise(
    total_appts = sum(annual_appts, na.rm = TRUE),
    .groups = "drop"
  )

# Summarise mean and SD for combined risk groups ("All")
summary_all <- data_for_plot %>%
  group_by(auc_label, cohort_type) %>%
  summarise(
    total_appts = sum(annual_appts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(risk_status = "All") %>%
  select(auc_label, risk_status, cohort_type, total_appts)

# Combine all summaries
summary_df_appts <- bind_rows(summary_df, summary_all)
summary_df_appts <- summary_df_appts %>% mutate(
  total_appts = total_appts / 1000
)

# Factor levels for ordering
summary_df_appts <- summary_df_appts %>%
  mutate(
    cohort_type = factor(cohort_type, levels = c("Minimum FU, XR + US",
                                                 "Minimum FU, US", 
                                                 "Minimum FU, CT", 
                                                 "Maximum FU, XR + US",
                                                 "Maximum FU, US", 
                                                 "Maximum FU, CT")),
    risk_status = factor(risk_status, levels = c("All", "Low Risk", "High Risk"))
  )

# Prepare data_for_plot factors similarly
data_for_plot <- data_for_plot %>%
  mutate(
    cohort_type = factor(cohort_type, levels = c("Minimum FU, XR + US",
                                                 "Minimum FU, US", 
                                                 "Minimum FU, CT", 
                                                 "Maximum FU, XR + US",
                                                 "Maximum FU, US", 
                                                 "Maximum FU, CT")),
    risk_status = factor(risk_status, levels = c("Low Risk", "High Risk"))
  )

# All auc values
auc_values <- unique(data_for_plot$auc_label)

### 10.4.3 lot total number of appointments for 2020 by cohort_type within risk groups - facet by FU type ####
summary_df_appts %>% group_by(auc_label) %>% ggplot(aes(x = auc_label, y = total_appts, fill = risk_status)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "black") +
  facet_wrap(~ cohort_type) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 10)
  ) +
  labs(
    title = "Annual Number of Appointments for EAU Follow-Up of those with Clinically Significant Disease",
    x = "Cohort Type",
    y = "Total Follow-up Appointments, n (000's)",
    fill = "Risk Status"
  )

### 10.4.4 Tabulate number of appointments ####
summary_df_appts %>% 
  filter(risk_status == "All") %>%
  group_by(auc_label,
           cohort_type) %>% 
  summarise(
    total_appts = sum(round(total_appts, 0))
  ) %>%
  gt()
