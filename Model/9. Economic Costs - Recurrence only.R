### 6.2.4 Examine costs associated with just recurrence ####
calculate_economic_costs_rec_only <- function(complete_pop_yr_fu,
                                     cutpoints_yr,
                                     year,
                                     auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                                     fu_type = c("min", "max"),
                                     post_op_imaging = c("none", ct_cost, imaging_cost, us_cost),
                                     imaging_fu_type = c("ct", "us", "xr_us")) {
  
  
  results_list <- list()
  
  # Define post-operative imaging
  post_op_imaging <- ifelse(post_op_imaging == "none",
                            0,
                            post_op_imaging)
  
  # Ensure Lucency correct
  complete_pop_yr_fu <- complete_pop_yr_fu %>%
    mutate(
      lucency = ifelse(lucency == "yes", "Yes",
                       "No")
    )
  
  # Assign follow-up costs
  year_1_lr_sf_fu_cost <- 0
  year_2_lr_sf_fu_cost <- 0
  year_3_lr_sf_fu_cost <- 0
  year_4_lr_sf_fu_cost <- 0
  year_5_lr_sf_fu_cost  <- 0
  year_1_lr_less4_fu_cost <- 0
  year_2_lr_less4_fu_cost <- 0
  year_3_lr_less4_fu_cost <- 0
  year_4_lr_less4_fu_cost <- 0
  year_5_lr_less4_fu_cost <- 0
  year_1_lr_more4_fu_cost <- 0
  year_2_lr_more4_fu_cost <- 0
  year_3_lr_more4_fu_cost <- 0
  year_4_lr_more4_fu_cost <- 0
  year_5_lr_more4_fu_cost <- 0
  year_1_hr_sf_fu_cost_current <- 0
  year_1_hr_sf_fu_cost_eau <- 0
  year_2_hr_sf_fu_cost_current <- 0
  year_2_hr_sf_fu_cost_eau <- 0
  year_3_onwards_hr_sf_fu_cost_current <- 0
  year_3_onwards_hr_sf_fu_cost_eau <- 0
  year_3_hr_sf_fu_cost_current <- 0
  year_3_hr_sf_fu_cost_eau <- 0
  year_4_hr_sf_fu_cost_eau <- 0
  year_5_hr_sf_fu_cost_eau <- 0
  year_1_hr_less4_fu_cost_eau <- 0
  year_2_hr_less4_fu_cost_eau <- 0
  year_3_hr_less4_fu_cost_eau <- 0
  year_4_hr_less4_fu_cost_eau <- 0
  year_5_hr_less4_fu_cost_eau <- 0
  year_1_hr_more4_fu_cost_eau <- 0
  year_2_hr_more4_fu_cost_eau <- 0
  year_3_hr_more4_fu_cost_eau <- 0
  year_4_hr_more4_fu_cost_eau <- 0
  year_5_hr_more4_fu_cost_eau <- 0
  
  # Assign recurrence costs
  rec_cost_colic <- ifelse(post_op_imaging == "none",
                           rec_cost_colic,
                           rec_cost_colic + post_op_imaging)
  rec_cost_eswl <- ifelse(post_op_imaging == "none",
                          rec_cost_eswl,
                          rec_cost_eswl + post_op_imaging)
  rec_cost_urs <- ifelse(post_op_imaging == "none",
                         rec_cost_urs,
                         rec_cost_urs + post_op_imaging)
  rec_cost_pcnl <- ifelse(post_op_imaging == "none",
                          rec_cost_pcnl,
                          rec_cost_pcnl + post_op_imaging)
  
  for (auc_target in auc_targets) {
    message("Calculating costs for: ", year, ", AUC: ", auc_target, ", Follow-up Type: ", fu_type, ", Follow-up Imaging: ", imaging_fu_type, " and Post-Operative Imaging:", post_op_imaging)
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
            stone_free_status_original == "SF" & rand_spec <= xr_spec ~ "SF", 
            stone_free_status_original == "SF" & rand_spec > xr_spec ~ ifelse(
              runif(n()) <= less4_prob, "less4", "more4"
            ),
            TRUE ~ stone_free_status_original
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
            stone_free_status_original == "SF" & lucency == "No" & rand_spec <= xr_spec ~ "SF", 
            stone_free_status_original == "SF" & lucency == "Yes" & rand_spec <= us_spec ~ "SF", 
            stone_free_status_original == "SF" & lucency == "No" & rand_spec > xr_spec ~ ifelse(
              runif(n()) <= less4_prob, "less4", "more4"
            ),
            stone_free_status_original == "SF" & lucency == "Yes" & rand_spec > us_spec ~ ifelse(
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
      fu_sf <- complete_pop_yr_fu1 %>%
        filter(stone_free_status1 == "SF" & auc_target == !!auc_target) %>%
        mutate(
          risk_status = case_when(
            score < cutpoint ~ "LR",
            score >= cutpoint ~ "HR"
          ),
          cost_year_0 = case_when(
            # Cost of post-operative CT
            death_year_0 == "No" & recurrence_year_0 == "No" ~ post_op_imaging,
            TRUE ~ NA_real_
          ),
          cost_year_1 = case_when(
            # Yr 1 FU divided by risk status
            death_year_1 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_1_lr_sf_fu_cost,
            death_year_1 == "No" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_1_hr_sf_fu_cost_eau,
            # Death
            death_year_1 == "Yes" ~ 0,
            # Recurrence
            death_year_1 == "No" & colic_intervention_type_year_1 == "Colic" ~ rec_cost_colic, 
            death_year_1 == "No" & colic_intervention_type_year_1 == "ESWL" ~ rec_cost_eswl,
            death_year_1 == "No" & colic_intervention_type_year_1 == "URS" ~ rec_cost_urs,
            death_year_1 == "No" & colic_intervention_type_year_1 == "PCNL" ~ rec_cost_pcnl,
            TRUE ~ NA_real_
          ),
          
          cost_year_2 = case_when(
            # Yr 2 FU divided by risk status
            death_year_2 == "No" & recurrence_year_2 == "No" & risk_status == "LR" ~ year_2_lr_sf_fu_cost,
            death_year_2 == "No" & recurrence_year_2 == "No" & risk_status == "HR" ~ year_2_hr_sf_fu_cost_eau,
            # Death
            death_year_2 == "Yes" ~ 0,
            # Recurrence
            death_year_2 == "No" & colic_intervention_type_year_2 == "Colic" & recurrence_year_1 == "No" ~ rec_cost_colic, 
            death_year_2 == "No" & colic_intervention_type_year_2 == "ESWL" & recurrence_year_1 == "No" ~ rec_cost_eswl,
            death_year_2 == "No" & colic_intervention_type_year_2 == "URS" & recurrence_year_1 == "No"  ~ rec_cost_urs,
            death_year_2 == "No" & colic_intervention_type_year_2 == "PCNL" & recurrence_year_1 == "No" ~ rec_cost_pcnl,
            # Yr 1 FU for Recurrence in Yr 1
            death_year_2 == "No" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" ~ year_1_lr_sf_fu_cost,
            death_year_2 == "No" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" ~ year_1_hr_sf_fu_cost_eau,
            TRUE ~ NA_real_
          ),
          
          cost_year_3 = case_when(
            # Yr 3 FU divided by risk status
            death_year_3 == "No" & recurrence_year_3 == "No" & risk_status == "LR" ~ year_3_lr_sf_fu_cost,
            death_year_3 == "No" & recurrence_year_3 == "No" & risk_status == "HR" ~ year_3_hr_sf_fu_cost_eau,
            # Death
            death_year_3 == "Yes" ~ 0,
            # Recurrence
            death_year_3 == "No" & colic_intervention_type_year_3 == "Colic" & recurrence_year_2 == "No" ~ rec_cost_colic, 
            death_year_3 == "No" & colic_intervention_type_year_3 == "ESWL" & recurrence_year_2 == "No" ~ rec_cost_eswl,
            death_year_3 == "No" & colic_intervention_type_year_3 == "URS" & recurrence_year_2 == "No"  ~ rec_cost_urs,
            death_year_3 == "No" & colic_intervention_type_year_3 == "PCNL" & recurrence_year_2 == "No" ~ rec_cost_pcnl,
            # Yr 1 FU for Recurrence in Yr 2
            death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_1_lr_sf_fu_cost,
            death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_1_hr_sf_fu_cost_eau,
            # Yr 2 FU for Recurrence in Yr 1
            death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" ~ year_2_lr_sf_fu_cost,
            death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" ~ year_2_hr_sf_fu_cost_eau,
            TRUE ~ NA_real_
          ),
          
          cost_year_4 = case_when(
            # Yr 4 FU divided by risk status
            death_year_4 == "No" & recurrence_year_4 == "No" & risk_status == "LR" ~ 0,
            death_year_4 == "No" & recurrence_year_4 == "No" & risk_status == "HR" ~ year_4_hr_sf_fu_cost_eau,
            # Death
            death_year_4 == "Yes" ~ 0,
            # Recurrence
            death_year_4 == "No" & colic_intervention_type_year_4 == "Colic" & recurrence_year_3 == "No" ~ rec_cost_colic, 
            death_year_4 == "No" & colic_intervention_type_year_4 == "ESWL" & recurrence_year_3 == "No" ~ rec_cost_eswl,
            death_year_4 == "No" & colic_intervention_type_year_4 == "URS" & recurrence_year_3 == "No"  ~ rec_cost_urs,
            death_year_4 == "No" & colic_intervention_type_year_4 == "PCNL" & recurrence_year_3 == "No" ~ rec_cost_pcnl,
            # Yr 1 FU for Recurrence in Yr 3
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_1_lr_sf_fu_cost,
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_1_hr_sf_fu_cost_eau,
            # Yr 2 FU for Recurrence in Yr 2
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_2_lr_sf_fu_cost,
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_2_hr_sf_fu_cost_eau,
            # Yr 3 FU for Recurrence in Yr 1
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" ~ year_3_lr_sf_fu_cost,
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" ~ year_3_hr_sf_fu_cost_eau,
            TRUE ~ NA_real_
          ),
          
          cost_year_5 = case_when(
            # Yr 5 FU divided by risk status
            death_year_5 == "No" & recurrence_year_5 == "No" & risk_status == "LR" ~ 0,
            death_year_5 == "No" & recurrence_year_5 == "No" & risk_status == "HR" ~ year_5_hr_sf_fu_cost_eau,
            # Death
            death_year_5 == "Yes" ~ 0,
            # Recurrence
            death_year_4 == "No" & colic_intervention_type_year_4 == "Colic" & recurrence_year_3 == "No" ~ rec_cost_colic, 
            death_year_4 == "No" & colic_intervention_type_year_4 == "ESWL" & recurrence_year_3 == "No" ~ rec_cost_eswl,
            death_year_4 == "No" & colic_intervention_type_year_4 == "URS" & recurrence_year_3 == "No"  ~ rec_cost_urs,
            death_year_4 == "No" & colic_intervention_type_year_4 == "PCNL" & recurrence_year_3 == "No" ~ rec_cost_pcnl,
            # Yr 1 FU for Recurrence in Yr 4 
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "No" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_1_lr_sf_fu_cost,
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "No" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_1_hr_sf_fu_cost_eau,
            # Yr 2 FU for Recurrence in Yr 3
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_2_lr_sf_fu_cost,
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_2_hr_sf_fu_cost_eau,
            # Yr 3 FU for Recurrence in Yr 2
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_3_lr_sf_fu_cost,
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_3_hr_sf_fu_cost_eau,
            # Yr 4 FU for Recurrence in Yr 1
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" ~ 0,
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" ~ year_4_hr_sf_fu_cost_eau,
            TRUE ~ NA_real_
          )
        )
    } else {
      # SF patients with maximum follow-up strategy
      fu_sf <- complete_pop_yr_fu1 %>%
        filter(stone_free_status1 == "SF" & auc_target == !!auc_target) %>%
        mutate(
          risk_status = case_when(
            score < cutpoint ~ "LR",
            score >= cutpoint ~ "HR"
          ),
          cost_year_0 = case_when(
            # Cost of post-operative CT
            death_year_0 == "No" & recurrence_year_0 == "No" ~ post_op_imaging,
            TRUE ~ NA_real_
          ),
          
          cost_year_1 = case_when(
            # Yr 1 FU divided by risk status
            death_year_1 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_1_lr_sf_fu_cost,
            death_year_1 == "No" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_1_hr_sf_fu_cost_eau,
            # Death
            death_year_1 == "Yes" ~ 0,
            # Recurrence
            death_year_1 == "No" & colic_intervention_type_year_1 == "Colic" ~ rec_cost_colic, 
            death_year_1 == "No" & colic_intervention_type_year_1 == "ESWL" ~ rec_cost_eswl,
            death_year_1 == "No" & colic_intervention_type_year_1 == "URS" ~ rec_cost_urs,
            death_year_1 == "No" & colic_intervention_type_year_1 == "PCNL" ~ rec_cost_pcnl,
            TRUE ~ NA_real_
          ),
          
          cost_year_2 = case_when(
            # Yr 2 FU divided by risk status
            death_year_2 == "No" & recurrence_year_2 == "No" & risk_status == "LR" ~ year_2_lr_sf_fu_cost,
            death_year_2 == "No" & recurrence_year_2 == "No" & risk_status == "HR" ~ year_2_hr_sf_fu_cost_eau,
            # Death
            death_year_2 == "Yes" ~ 0,
            # Recurrence
            death_year_2 == "No" & colic_intervention_type_year_2 == "Colic" & recurrence_year_1 == "No" ~ rec_cost_colic, 
            death_year_2 == "No" & colic_intervention_type_year_2 == "ESWL" & recurrence_year_1 == "No" ~ rec_cost_eswl,
            death_year_2 == "No" & colic_intervention_type_year_2 == "URS" & recurrence_year_1 == "No"  ~ rec_cost_urs,
            death_year_2 == "No" & colic_intervention_type_year_2 == "PCNL" & recurrence_year_1 == "No" ~ rec_cost_pcnl,
            # Yr 1 FU for Recurrence in Yr 1
            death_year_2 == "No" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" ~ year_1_lr_sf_fu_cost,
            death_year_2 == "No" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" ~ year_1_hr_sf_fu_cost_eau,
            TRUE ~ NA_real_
          ),
          
          cost_year_3 = case_when(
            # Yr 3 FU divided by risk status
            death_year_3 == "No" & recurrence_year_3 == "No" & risk_status == "LR" ~ year_3_lr_sf_fu_cost,
            death_year_3 == "No" & recurrence_year_3 == "No" & risk_status == "HR" ~ year_3_hr_sf_fu_cost_eau,
            # Death
            death_year_3 == "Yes" ~ 0,
            # Recurrence
            death_year_3 == "No" & colic_intervention_type_year_3 == "Colic" & recurrence_year_2 == "No" ~ rec_cost_colic, 
            death_year_3 == "No" & colic_intervention_type_year_3 == "ESWL" & recurrence_year_2 == "No" ~ rec_cost_eswl,
            death_year_3 == "No" & colic_intervention_type_year_3 == "URS" & recurrence_year_2 == "No"  ~ rec_cost_urs,
            death_year_3 == "No" & colic_intervention_type_year_3 == "PCNL" & recurrence_year_2 == "No" ~ rec_cost_pcnl,
            # Yr 1 FU for Recurrence in Yr 2
            death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_1_lr_sf_fu_cost,
            death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_1_hr_sf_fu_cost_eau,
            # Yr 2 FU for Recurrence in Yr 1
            death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" ~ year_2_lr_sf_fu_cost,
            death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" ~ year_2_hr_sf_fu_cost_eau,
            TRUE ~ NA_real_
          ),
          
          cost_year_4 = case_when(
            # Yr 4 FU divided by risk status
            death_year_4 == "No" & recurrence_year_4 == "No" & risk_status == "LR" ~ 0,
            death_year_4 == "No" & recurrence_year_4 == "No" & risk_status == "HR" ~ year_4_hr_sf_fu_cost_eau,
            # Death
            death_year_4 == "Yes" ~ 0,
            # Recurrence
            death_year_4 == "No" & colic_intervention_type_year_4 == "Colic" & recurrence_year_3 == "No" ~ rec_cost_colic, 
            death_year_4 == "No" & colic_intervention_type_year_4 == "ESWL" & recurrence_year_3 == "No" ~ rec_cost_eswl,
            death_year_4 == "No" & colic_intervention_type_year_4 == "URS" & recurrence_year_3 == "No"  ~ rec_cost_urs,
            death_year_4 == "No" & colic_intervention_type_year_4 == "PCNL" & recurrence_year_3 == "No" ~ rec_cost_pcnl,
            # Yr 1 FU for Recurrence in Yr 3
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_1_lr_sf_fu_cost,
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_1_hr_sf_fu_cost_eau,
            # Yr 2 FU for Recurrence in Yr 2
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_2_lr_sf_fu_cost,
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_2_hr_sf_fu_cost_eau,
            # Yr 3 FU for Recurrence in Yr 1
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" ~ year_3_lr_sf_fu_cost,
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" ~ year_3_hr_sf_fu_cost_eau,
            TRUE ~ NA_real_
          ),
          
          cost_year_5 = case_when(
            # Yr 5 FU divided by risk status
            death_year_5 == "No" & recurrence_year_5 == "No" & risk_status == "LR" ~ year_5_lr_sf_fu_cost,
            death_year_5 == "No" & recurrence_year_5 == "No" & risk_status == "HR" ~ year_5_hr_sf_fu_cost_eau,
            # Death
            death_year_5 == "Yes" ~ 0,
            # Recurrence
            death_year_4 == "No" & colic_intervention_type_year_4 == "Colic" & recurrence_year_3 == "No" ~ rec_cost_colic, 
            death_year_4 == "No" & colic_intervention_type_year_4 == "ESWL" & recurrence_year_3 == "No" ~ rec_cost_eswl,
            death_year_4 == "No" & colic_intervention_type_year_4 == "URS" & recurrence_year_3 == "No"  ~ rec_cost_urs,
            death_year_4 == "No" & colic_intervention_type_year_4 == "PCNL" & recurrence_year_3 == "No" ~ rec_cost_pcnl,
            # Yr 1 FU for Recurrence in Yr 4 
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "No" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_1_lr_sf_fu_cost,
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "No" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_1_hr_sf_fu_cost_eau,
            # Yr 2 FU for Recurrence in Yr 3
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_2_lr_sf_fu_cost,
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_2_hr_sf_fu_cost_eau,
            # Yr 3 FU for Recurrence in Yr 2
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_3_lr_sf_fu_cost,
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_3_hr_sf_fu_cost_eau,
            # Yr 4 FU for Recurrence in Yr 1
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" ~ year_4_lr_sf_fu_cost,
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" ~ year_4_hr_sf_fu_cost_eau,
            TRUE ~ NA_real_
          )
        )
    }
    
    # Less than 4mm stones
    fu_less4 <- complete_pop_yr_fu1 %>% 
      filter(stone_free_status1 == "less4" & auc_target == !!auc_target) %>%
      mutate(
        risk_status = case_when(
          score < cutpoint ~ "LR",
          score >= cutpoint ~ "HR"
        ),
        cost_year_0 = case_when(
          # Cost of post-operative CT
          death_year_0 == "No" & recurrence_year_0 == "No" ~ post_op_imaging,
          TRUE ~ NA_real_
        ),
        
        cost_year_1 = case_when(
          # Yr 1 FU divided by risk status
          death_year_1 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_1_lr_less4_fu_cost,
          death_year_1 == "No" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_1_hr_less4_fu_cost_eau,
          # Death
          death_year_1 == "Yes" ~ 0,
          # Recurrence
          death_year_1 == "No" & colic_intervention_type_year_1 == "Colic" ~ rec_cost_colic, 
          death_year_1 == "No" & colic_intervention_type_year_1 == "ESWL" ~ rec_cost_eswl,
          death_year_1 == "No" & colic_intervention_type_year_1 == "URS" ~ rec_cost_urs,
          death_year_1 == "No" & colic_intervention_type_year_1 == "PCNL" ~ rec_cost_pcnl,
          TRUE ~ NA_real_
        ),
        
        cost_year_2 = case_when(
          # Yr 2 FU divided by risk status
          death_year_2 == "No" & recurrence_year_2 == "No" & risk_status == "LR" ~ year_2_lr_less4_fu_cost,
          death_year_2 == "No" & recurrence_year_2 == "No" & risk_status == "HR" ~ year_2_hr_less4_fu_cost_eau,
          # Death
          death_year_2 == "Yes" ~ 0,
          # Recurrence
          death_year_2 == "No" & colic_intervention_type_year_2 == "Colic" & recurrence_year_1 == "No" ~ rec_cost_colic, 
          death_year_2 == "No" & colic_intervention_type_year_2 == "ESWL" & recurrence_year_1 == "No" ~ rec_cost_eswl,
          death_year_2 == "No" & colic_intervention_type_year_2 == "URS" & recurrence_year_1 == "No"  ~ rec_cost_urs,
          death_year_2 == "No" & colic_intervention_type_year_2 == "PCNL" & recurrence_year_1 == "No" ~ rec_cost_pcnl,
          # Yr 1 FU for Recurrence in Yr 1
          death_year_2 == "No" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" ~ year_1_lr_less4_fu_cost,
          death_year_2 == "No" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" ~ year_1_hr_less4_fu_cost_eau,
          TRUE ~ NA_real_
        ),
        
        cost_year_3 = case_when(
          # Yr 3 FU divided by risk status
          death_year_3 == "No" & recurrence_year_3 == "No" & risk_status == "LR" ~ year_3_lr_less4_fu_cost,
          death_year_3 == "No" & recurrence_year_3 == "No" & risk_status == "HR" ~ year_3_hr_less4_fu_cost_eau,
          # Death
          death_year_3 == "Yes" ~ 0,
          # Recurrence
          death_year_3 == "No" & colic_intervention_type_year_3 == "Colic" & recurrence_year_2 == "No" ~ rec_cost_colic, 
          death_year_3 == "No" & colic_intervention_type_year_3 == "ESWL" & recurrence_year_2 == "No" ~ rec_cost_eswl,
          death_year_3 == "No" & colic_intervention_type_year_3 == "URS" & recurrence_year_2 == "No"  ~ rec_cost_urs,
          death_year_3 == "No" & colic_intervention_type_year_3 == "PCNL" & recurrence_year_2 == "No" ~ rec_cost_pcnl,
          # Yr 1 FU for Recurrence in Yr 2
          death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_1_lr_less4_fu_cost,
          death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_1_hr_less4_fu_cost_eau,
          # Yr 2 FU for Recurrence in Yr 1
          death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" ~ year_2_lr_less4_fu_cost,
          death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" ~ year_2_hr_less4_fu_cost_eau,
          TRUE ~ NA_real_
        ),
        
        cost_year_4 = case_when(
          # Yr 4 FU divided by risk status
          death_year_4 == "No" & recurrence_year_4 == "No" & risk_status == "LR" ~ 0,
          death_year_4 == "No" & recurrence_year_4 == "No" & risk_status == "HR" ~ year_4_hr_less4_fu_cost_eau,
          # Death
          death_year_4 == "Yes" ~ 0,
          # Recurrence
          death_year_4 == "No" & colic_intervention_type_year_4 == "Colic" & recurrence_year_3 == "No" ~ rec_cost_colic, 
          death_year_4 == "No" & colic_intervention_type_year_4 == "ESWL" & recurrence_year_3 == "No" ~ rec_cost_eswl,
          death_year_4 == "No" & colic_intervention_type_year_4 == "URS" & recurrence_year_3 == "No"  ~ rec_cost_urs,
          death_year_4 == "No" & colic_intervention_type_year_4 == "PCNL" & recurrence_year_3 == "No" ~ rec_cost_pcnl,
          # Yr 1 FU for Recurrence in Yr 3
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_1_lr_less4_fu_cost,
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_1_hr_less4_fu_cost_eau,
          # Yr 2 FU for Recurrence in Yr 2
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_2_lr_less4_fu_cost,
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_2_hr_less4_fu_cost_eau,
          # Yr 3 FU for Recurrence in Yr 1
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" ~ year_3_lr_sf_fu_cost,
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" ~ year_3_hr_sf_fu_cost_eau,
          TRUE ~ NA_real_
        ),
        
        cost_year_5 = case_when(
          # Yr 5 FU divided by risk status
          death_year_5 == "No" & recurrence_year_5 == "No" & risk_status == "LR" ~ year_5_lr_less4_fu_cost,
          death_year_5 == "No" & recurrence_year_5 == "No" & risk_status == "HR" ~ year_5_hr_less4_fu_cost_eau,
          # Death
          death_year_5 == "Yes" ~ 0,
          # Recurrence
          death_year_4 == "No" & colic_intervention_type_year_4 == "Colic" & recurrence_year_3 == "No" ~ rec_cost_colic, 
          death_year_4 == "No" & colic_intervention_type_year_4 == "ESWL" & recurrence_year_3 == "No" ~ rec_cost_eswl,
          death_year_4 == "No" & colic_intervention_type_year_4 == "URS" & recurrence_year_3 == "No"  ~ rec_cost_urs,
          death_year_4 == "No" & colic_intervention_type_year_4 == "PCNL" & recurrence_year_3 == "No" ~ rec_cost_pcnl,
          # Yr 1 FU for Recurrence in Yr 4 
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "No" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_1_lr_less4_fu_cost,
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "No" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_1_hr_less4_fu_cost_eau,
          # Yr 2 FU for Recurrence in Yr 3
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_2_lr_less4_fu_cost,
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_2_hr_less4_fu_cost_eau,
          # Yr 3 FU for Recurrence in Yr 2
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_3_lr_less4_fu_cost,
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_3_hr_less4_fu_cost_eau,
          # Yr 4 FU for Recurrence in Yr 1
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" ~ year_4_lr_less4_fu_cost,
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" ~ year_4_hr_less4_fu_cost_eau,
          TRUE ~ NA_real_
        )
      )
    
    # More than 4mm stones
    fu_more4 <- complete_pop_yr_fu1 %>% 
      filter(stone_free_status1 == "more4" & auc_target == !!auc_target) %>%
      mutate(
        risk_status = case_when(
          score < cutpoint ~ "LR",
          score >= cutpoint ~ "HR"
        ),
        cost_year_0 = case_when(
          # Cost of post-operative CT
          death_year_0 == "No" & recurrence_year_0 == "No" ~ post_op_imaging,
          TRUE ~ NA_real_
        ),
        
        cost_year_1 = case_when(
          # Yr 1 FU divided by risk status
          death_year_1 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_1_lr_more4_fu_cost,
          death_year_1 == "No" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_1_hr_more4_fu_cost_eau,
          # Death
          death_year_1 == "Yes" ~ 0,
          # Recurrence
          death_year_1 == "No" & colic_intervention_type_year_1 == "Colic" ~ rec_cost_colic, 
          death_year_1 == "No" & colic_intervention_type_year_1 == "ESWL" ~ rec_cost_eswl,
          death_year_1 == "No" & colic_intervention_type_year_1 == "URS" ~ rec_cost_urs,
          death_year_1 == "No" & colic_intervention_type_year_1 == "PCNL" ~ rec_cost_pcnl,
          TRUE ~ NA_real_
        ),
        
        cost_year_2 = case_when(
          # Yr 2 FU divided by risk status
          death_year_2 == "No" & recurrence_year_2 == "No" & risk_status == "LR" ~ year_2_lr_more4_fu_cost,
          death_year_2 == "No" & recurrence_year_2 == "No" & risk_status == "HR" ~ year_2_hr_more4_fu_cost_eau,
          # Death
          death_year_2 == "Yes" ~ 0,
          # Recurrence
          death_year_2 == "No" & colic_intervention_type_year_2 == "Colic" & recurrence_year_1 == "No" ~ rec_cost_colic, 
          death_year_2 == "No" & colic_intervention_type_year_2 == "ESWL" & recurrence_year_1 == "No" ~ rec_cost_eswl,
          death_year_2 == "No" & colic_intervention_type_year_2 == "URS" & recurrence_year_1 == "No"  ~ rec_cost_urs,
          death_year_2 == "No" & colic_intervention_type_year_2 == "PCNL" & recurrence_year_1 == "No" ~ rec_cost_pcnl,
          # Yr 1 FU for Recurrence in Yr 1
          death_year_2 == "No" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" ~ year_1_lr_more4_fu_cost,
          death_year_2 == "No" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" ~ year_1_hr_more4_fu_cost_eau,
          TRUE ~ NA_real_
        ),
        
        cost_year_3 = case_when(
          # Yr 3 FU divided by risk status
          death_year_3 == "No" & recurrence_year_3 == "No" & risk_status == "LR" ~ year_3_lr_more4_fu_cost,
          death_year_3 == "No" & recurrence_year_3 == "No" & risk_status == "HR" ~ year_3_hr_more4_fu_cost_eau,
          # Death
          death_year_3 == "Yes" ~ 0,
          # Recurrence
          death_year_3 == "No" & colic_intervention_type_year_3 == "Colic" & recurrence_year_2 == "No" ~ rec_cost_colic, 
          death_year_3 == "No" & colic_intervention_type_year_3 == "ESWL" & recurrence_year_2 == "No" ~ rec_cost_eswl,
          death_year_3 == "No" & colic_intervention_type_year_3 == "URS" & recurrence_year_2 == "No"  ~ rec_cost_urs,
          death_year_3 == "No" & colic_intervention_type_year_3 == "PCNL" & recurrence_year_2 == "No" ~ rec_cost_pcnl,
          # Yr 1 FU for Recurrence in Yr 2
          death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_1_lr_more4_fu_cost,
          death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_1_hr_more4_fu_cost_eau,
          # Yr 2 FU for Recurrence in Yr 1
          death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" ~ year_2_lr_more4_fu_cost,
          death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" ~ year_2_hr_more4_fu_cost_eau,
          TRUE ~ NA_real_
        ),
        
        cost_year_4 = case_when(
          # Yr 4 FU divided by risk status
          death_year_4 == "No" & recurrence_year_4 == "No" & risk_status == "LR" ~ 0,
          death_year_4 == "No" & recurrence_year_4 == "No" & risk_status == "HR" ~ year_4_hr_more4_fu_cost_eau,
          # Death
          death_year_4 == "Yes" ~ 0,
          # Recurrence
          death_year_4 == "No" & colic_intervention_type_year_4 == "Colic" & recurrence_year_3 == "No" ~ rec_cost_colic, 
          death_year_4 == "No" & colic_intervention_type_year_4 == "ESWL" & recurrence_year_3 == "No" ~ rec_cost_eswl,
          death_year_4 == "No" & colic_intervention_type_year_4 == "URS" & recurrence_year_3 == "No"  ~ rec_cost_urs,
          death_year_4 == "No" & colic_intervention_type_year_4 == "PCNL" & recurrence_year_3 == "No" ~ rec_cost_pcnl,
          # Yr 1 FU for Recurrence in Yr 3
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_1_lr_more4_fu_cost,
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_1_hr_more4_fu_cost_eau,
          # Yr 2 FU for Recurrence in Yr 2
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_2_lr_more4_fu_cost,
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_2_hr_more4_fu_cost_eau,
          # Yr 3 FU for Recurrence in Yr 1
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" ~ year_3_lr_sf_fu_cost,
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" ~ year_3_hr_sf_fu_cost_eau,
          TRUE ~ NA_real_
        ),
        
        cost_year_5 = case_when(
          # Yr 5 FU divided by risk status
          death_year_5 == "No" & recurrence_year_5 == "No" & risk_status == "LR" ~ year_5_lr_more4_fu_cost,
          death_year_5 == "No" & recurrence_year_5 == "No" & risk_status == "HR" ~ year_5_hr_more4_fu_cost_eau,
          # Death
          death_year_5 == "Yes" ~ 0,
          # Recurrence
          death_year_4 == "No" & colic_intervention_type_year_4 == "Colic" & recurrence_year_3 == "No" ~ rec_cost_colic, 
          death_year_4 == "No" & colic_intervention_type_year_4 == "ESWL" & recurrence_year_3 == "No" ~ rec_cost_eswl,
          death_year_4 == "No" & colic_intervention_type_year_4 == "URS" & recurrence_year_3 == "No"  ~ rec_cost_urs,
          death_year_4 == "No" & colic_intervention_type_year_4 == "PCNL" & recurrence_year_3 == "No" ~ rec_cost_pcnl,
          # Yr 1 FU for Recurrence in Yr 4 
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "No" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_1_lr_more4_fu_cost,
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "No" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_1_hr_more4_fu_cost_eau,
          # Yr 2 FU for Recurrence in Yr 3
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_2_lr_more4_fu_cost,
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_2_hr_more4_fu_cost_eau,
          # Yr 3 FU for Recurrence in Yr 2
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "LR" ~ year_3_lr_more4_fu_cost,
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "HR" ~ year_3_hr_more4_fu_cost_eau,
          # Yr 4 FU for Recurrence in Yr 1
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" ~ year_4_lr_more4_fu_cost,
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" ~ year_4_hr_more4_fu_cost_eau,
          TRUE ~ NA_real_
        )
      )
    
    # Combine all stone status groups for this AUC target
    combined_result <- rbind(fu_sf, 
                             fu_less4, 
                             fu_more4) %>% 
      as_tibble() %>%
      mutate(auc_target = auc_target, 
             cutpoint = cutpoint, 
             post_op_imaging = post_op_imaging,
             imaging_fu_type = imaging_fu_type,
             year = year)
    
    results_list[[paste0("auc_", auc_target)]] <- combined_result
  }
  
  if (length(auc_targets) == 1) {
    return(results_list[[1]])
  } else {
    return(results_list)
  }
}

### 6.2.5 Run function for each year ####
#### 6.2.5.1 2016 ####
costs_rec_only_2016_xr_min <- calculate_economic_costs_rec_only(
  complete_pop_yr_fu = complete_pop_2016_fu,
  cutpoints_yr = cutpoints_2016,
  year = 2016,
  auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
  fu_type = "min",
  post_op_imaging = "none",
  imaging_fu_type = "us"
)

costs_2016_rec_only_xr_max <- calculate_economic_costs_rec_only(complete_pop_yr_fu = complete_pop_2016_fu,
                                              cutpoints_yr = cutpoints_2016,
                                              year = 2016,
                                              auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                                              fu_type = "max",
                                              post_op_imaging = "none",
                                              imaging_fu_type = "us")

costs_2016_rec_only_xr_us_min <- calculate_economic_costs_rec_only(complete_pop_yr_fu = complete_pop_2016_fu,
                                                 cutpoints_yr = cutpoints_2016,
                                                 year = 2016,
                                                 auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                                                 fu_type = "min",
                                                 post_op_imaging = "none",
                                                 imaging_fu_type = "xr_us")

costs_2016_rec_only_xr_us_max <- calculate_economic_costs_rec_only(complete_pop_yr_fu = complete_pop_2016_fu,
                                                 cutpoints_yr = cutpoints_2016,
                                                 year = 2016,
                                                 auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                                                 fu_type = "max",
                                                 post_op_imaging = "none",
                                                 imaging_fu_type = "xr_us")

costs_2016_rec_only_ct_min <- calculate_economic_costs_rec_only(complete_pop_yr_fu = complete_pop_2016_fu,
                                              cutpoints_yr = cutpoints_2016,
                                              year = 2016,
                                              auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                                              fu_type = "min",
                                              post_op_imaging = "none",
                                              imaging_fu_type = "ct")

costs_2016_rec_only_ct_max <- calculate_economic_costs_rec_only(complete_pop_yr_fu = complete_pop_2016_fu,
                                              cutpoints_yr = cutpoints_2016,
                                              year = 2016,
                                              auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                                              fu_type = "max",
                                              post_op_imaging = "none",
                                              imaging_fu_type = "ct")

#### 6.2.5.2 2017 ####
costs_2017_rec_only_xr_min <- calculate_economic_costs_rec_only(complete_pop_yr_fu = complete_pop_2017_fu,
                                              cutpoints_yr = cutpoints_2017,
                                              year = 2017,
                                              auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                                              fu_type = "min",
                                              post_op_imaging = "none",
                                              imaging_fu_type = "us")

costs_2017_rec_only_xr_max <- calculate_economic_costs_rec_only(complete_pop_yr_fu = complete_pop_2017_fu,
                                              cutpoints_yr = cutpoints_2017,
                                              year = 2017,
                                              auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                                              fu_type = "max",
                                              post_op_imaging = "none",
                                              imaging_fu_type = "us")

costs_2017_rec_only_xr_us_min <- calculate_economic_costs_rec_only(complete_pop_yr_fu = complete_pop_2017_fu,
                                                 cutpoints_yr = cutpoints_2017,
                                                 year = 2017,
                                                 auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                                                 fu_type = "min",
                                                 post_op_imaging = "none",
                                                 imaging_fu_type = "xr_us")

costs_2017_rec_only_xr_us_max <- calculate_economic_costs_rec_only(complete_pop_yr_fu = complete_pop_2017_fu,
                                                 cutpoints_yr = cutpoints_2017,
                                                 year = 2017,
                                                 auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                                                 fu_type = "max",
                                                 post_op_imaging = "none",
                                                 imaging_fu_type = "xr_us")

costs_2017_rec_only_ct_min <- calculate_economic_costs_rec_only(complete_pop_yr_fu = complete_pop_2017_fu,
                                              cutpoints_yr = cutpoints_2017,
                                              year = 2017,
                                              auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                                              fu_type = "min",
                                              post_op_imaging = "none",
                                              imaging_fu_type = "ct")

costs_2017_rec_only_ct_max <- calculate_economic_costs_rec_only(complete_pop_yr_fu = complete_pop_2017_fu,
                                              cutpoints_yr = cutpoints_2017,
                                              year = 2017,
                                              auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                                              fu_type = "max",
                                              post_op_imaging = "none",
                                              imaging_fu_type = "ct")

#### 6.2.5.3 2018 ####
costs_2018_rec_only_xr_min <- calculate_economic_costs_rec_only(complete_pop_yr_fu = complete_pop_2018_fu,
                                              cutpoints_yr = cutpoints_2018,
                                              year = 2018,
                                              auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                                              fu_type = "min",
                                              post_op_imaging = "none",
                                              imaging_fu_type = "us")

costs_2018_rec_only_xr_max <- calculate_economic_costs_rec_only(complete_pop_yr_fu = complete_pop_2018_fu,
                                              cutpoints_yr = cutpoints_2018,
                                              year = 2018,
                                              auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                                              fu_type = "max",
                                              post_op_imaging = "none",
                                              imaging_fu_type = "us")

costs_2018_rec_only_xr_us_min <- calculate_economic_costs_rec_only(complete_pop_yr_fu = complete_pop_2018_fu,
                                                 cutpoints_yr = cutpoints_2018,
                                                 year = 2018,
                                                 auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                                                 fu_type = "min",
                                                 post_op_imaging = "none",
                                                 imaging_fu_type = "xr_us")

costs_2018_rec_only_xr_us_max <- calculate_economic_costs_rec_only(complete_pop_yr_fu = complete_pop_2018_fu,
                                                 cutpoints_yr = cutpoints_2018,
                                                 year = 2018,
                                                 auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                                                 fu_type = "max",
                                                 post_op_imaging = "none",
                                                 imaging_fu_type = "xr_us")

costs_2018_rec_only_ct_min <- calculate_economic_costs_rec_only(complete_pop_yr_fu = complete_pop_2018_fu,
                                              cutpoints_yr = cutpoints_2018,
                                              year = 2018,
                                              auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                                              fu_type = "min",
                                              post_op_imaging = "none",
                                              imaging_fu_type = "ct")

costs_2018_rec_only_ct_max <- calculate_economic_costs_rec_only(complete_pop_yr_fu = complete_pop_2018_fu,
                                              cutpoints_yr = cutpoints_2018,
                                              year = 2018,
                                              auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                                              fu_type = "max",
                                              post_op_imaging = "none",
                                              imaging_fu_type = "ct")

#### 6.2.5.4 2019 ####
costs_2019_rec_only_xr_min <- calculate_economic_costs_rec_only(complete_pop_yr_fu = complete_pop_2019_fu,
                                              cutpoints_yr = cutpoints_2019,
                                              year = 2019,
                                              auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                                              fu_type = "min",
                                              post_op_imaging = "none",
                                              imaging_fu_type = "us")

costs_2019_rec_only_xr_max <- calculate_economic_costs_rec_only(complete_pop_yr_fu = complete_pop_2019_fu,
                                              cutpoints_yr = cutpoints_2019,
                                              year = 2019,
                                              auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                                              fu_type = "max",
                                              post_op_imaging = "none",
                                              imaging_fu_type = "us")

costs_2019_rec_only_xr_us_min <- calculate_economic_costs_rec_only(complete_pop_yr_fu = complete_pop_2019_fu,
                                                 cutpoints_yr = cutpoints_2019,
                                                 year = 2019,
                                                 auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                                                 fu_type = "min",
                                                 post_op_imaging = "none",
                                                 imaging_fu_type = "xr_us")

costs_2019_rec_only_xr_us_max <- calculate_economic_costs_rec_only(complete_pop_yr_fu = complete_pop_2019_fu,
                                                 cutpoints_yr = cutpoints_2019,
                                                 year = 2019,
                                                 auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                                                 fu_type = "max",
                                                 post_op_imaging = "none",
                                                 imaging_fu_type = "xr_us")

costs_2019_rec_only_ct_min <- calculate_economic_costs_rec_only(complete_pop_yr_fu = complete_pop_2019_fu,
                                              cutpoints_yr = cutpoints_2019,
                                              year = 2019,
                                              auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                                              fu_type = "min",
                                              post_op_imaging = "none",
                                              imaging_fu_type = "ct")

costs_2019_rec_only_ct_max <- calculate_economic_costs_rec_only(complete_pop_yr_fu = complete_pop_2019_fu,
                                              cutpoints_yr = cutpoints_2019,
                                              year = 2019,
                                              auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                                              fu_type = "max",
                                              post_op_imaging = "none",
                                              imaging_fu_type = "ct")

#### 6.2.5.5 2020 ####
costs_2020_rec_only_xr_min <- calculate_economic_costs_rec_only(complete_pop_yr_fu = complete_pop_2020_fu,
                                              cutpoints_yr = cutpoints_2020,
                                              year = 2020,
                                              auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                                              fu_type = "min",
                                              post_op_imaging = "none",
                                              imaging_fu_type = "us")

costs_2020_rec_only_xr_max <- calculate_economic_costs_rec_only(complete_pop_yr_fu = complete_pop_2020_fu,
                                              cutpoints_yr = cutpoints_2020,
                                              year = 2020,
                                              auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                                              fu_type = "max",
                                              post_op_imaging = "none",
                                              imaging_fu_type = "us")

costs_2020_rec_only_xr_us_min <- calculate_economic_costs_rec_only(complete_pop_yr_fu = complete_pop_2020_fu,
                                                 cutpoints_yr = cutpoints_2020,
                                                 year = 2020,
                                                 auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                                                 fu_type = "min",
                                                 post_op_imaging = "none",
                                                 imaging_fu_type = "xr_us")

costs_2020_rec_only_xr_us_max <- calculate_economic_costs_rec_only(complete_pop_yr_fu = complete_pop_2020_fu,
                                                 cutpoints_yr = cutpoints_2020,
                                                 year = 2020,
                                                 auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                                                 fu_type = "max",
                                                 post_op_imaging = "none",
                                                 imaging_fu_type = "xr_us")

costs_2020_rec_only_ct_min <- calculate_economic_costs_rec_only(complete_pop_yr_fu = complete_pop_2020_fu,
                                              cutpoints_yr = cutpoints_2020,
                                              year = 2020,
                                              auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                                              fu_type = "min",
                                              post_op_imaging = "none",
                                              imaging_fu_type = "ct")

costs_2020_rec_only_ct_max <- calculate_economic_costs_rec_only(complete_pop_yr_fu = complete_pop_2020_fu,
                                              cutpoints_yr = cutpoints_2020,
                                              year = 2020,
                                              auc_targets = c(0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95),
                                              fu_type = "max",
                                              post_op_imaging = "none",
                                              imaging_fu_type = "ct")


### 6.2.6 Amalgamate + plot each AUC ####
#### 6.2.6.1 Function ####
aggregate_cost_cohorts <- function(auc_target = c(1,2,3,4,5,6,7,8,9)) {
  all_cohorts <- list()
  
  for (i in auc_target) {
    key <- as.integer(i)
    message("Processing AUC = ", key)
    
    message("  Loading 2016 data...")
    cohort_2016_rec_only_min_us <- costs_2016_rec_only_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, US", auc = i)
    cohort_2016_rec_only_min_ct <- costs_2016_rec_only_ct_min[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2016_rec_only_max_us <- costs_2016_rec_only_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, US", auc = i)
    cohort_2016_rec_only_max_ct <- costs_2016_rec_only_ct_max[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    cohort_2016_rec_only_min_xr_us <- costs_2016_rec_only_xr_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2016_rec_only_max_xr_us <- costs_2016_rec_only_xr_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    
    message("  Loading 2017 data...")
    cohort_2017_rec_only_min_us <- costs_2017_rec_only_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, US", auc = i)
    cohort_2017_rec_only_min_ct <- costs_2017_rec_only_ct_min[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2017_rec_only_max_us <- costs_2017_rec_only_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, US", auc = i)
    cohort_2017_rec_only_max_ct <- costs_2017_rec_only_ct_max[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    cohort_2017_rec_only_min_xr_us <- costs_2017_rec_only_xr_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2017_rec_only_max_xr_us <- costs_2017_rec_only_xr_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    
    message("  Loading 2018 data...")
    cohort_2018_rec_only_min_us <- costs_2018_rec_only_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, US", auc = i)
    cohort_2018_rec_only_min_ct <- costs_2018_rec_only_ct_min[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2018_rec_only_max_us <- costs_2018_rec_only_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, US", auc = i)
    cohort_2018_rec_only_max_ct <- costs_2018_rec_only_ct_max[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    cohort_2018_rec_only_min_xr_us <- costs_2018_rec_only_xr_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2018_rec_only_max_xr_us <- costs_2018_rec_only_xr_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    
    message("  Loading 2019 data...")
    cohort_2019_rec_only_min_us <- costs_2019_rec_only_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, US", auc = i)
    cohort_2019_rec_only_min_ct <- costs_2019_rec_only_ct_min[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2019_rec_only_max_us <- costs_2019_rec_only_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, US", auc = i)
    cohort_2019_rec_only_max_ct <- costs_2019_rec_only_ct_max[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    cohort_2019_rec_only_min_xr_us <- costs_2019_rec_only_xr_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2019_rec_only_max_xr_us <- costs_2019_rec_only_xr_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    
    message("  Loading 2020 data...")
    cohort_2020_rec_only_min_us <- costs_2020_rec_only_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, US", auc = i)
    cohort_2020_rec_only_min_ct <- costs_2020_rec_only_ct_min[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2020_rec_only_max_us <- costs_2020_rec_only_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, US", auc = i)
    cohort_2020_rec_only_max_ct <- costs_2020_rec_only_ct_max[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    cohort_2020_rec_only_min_xr_us <- costs_2020_rec_only_xr_us_min[[key]] %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2020_rec_only_max_xr_us <- costs_2020_rec_only_xr_us_max[[key]] %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    
    message("  Combining cohorts for AUC = ", key)
    overall_cohort <- dplyr::bind_rows(
      cohort_2016_rec_only_min_us, cohort_2016_rec_only_min_ct, cohort_2016_rec_only_max_us, cohort_2016_rec_only_max_ct, cohort_2016_rec_only_min_xr_us, cohort_2016_rec_only_max_xr_us,
      cohort_2017_rec_only_min_us, cohort_2017_rec_only_min_ct, cohort_2017_rec_only_max_us, cohort_2017_rec_only_max_ct, cohort_2017_rec_only_min_xr_us, cohort_2017_rec_only_max_xr_us,
      cohort_2018_rec_only_min_us, cohort_2018_rec_only_min_ct, cohort_2018_rec_only_max_us, cohort_2018_rec_only_max_ct, cohort_2018_rec_only_min_xr_us, cohort_2018_rec_only_max_xr_us,
      cohort_2019_rec_only_min_us, cohort_2019_rec_only_min_ct, cohort_2019_rec_only_max_us, cohort_2019_rec_only_max_ct, cohort_2019_rec_only_min_xr_us, cohort_2019_rec_only_max_xr_us,
      cohort_2020_rec_only_min_us, cohort_2020_rec_only_min_ct, cohort_2020_rec_only_max_us, cohort_2020_rec_only_max_ct, cohort_2020_rec_only_min_xr_us, cohort_2020_rec_only_max_xr_us
    )
    
    all_cohorts[[key]] <- overall_cohort
    message("✅ Done with AUC = ", key, "\n")
  }
  
  message("🔗 Binding all AUC cohorts together...")
  final_df <- dplyr::bind_rows(all_cohorts)
  message("✅ All done.")
  
  return(final_df)
}

#### 6.2.6.2 AUC 0.55 ####
auc_0.55 <- aggregate_cost_cohorts(auc_target = 1)

#### 6.2.6.3 AUC 0.6 ####
auc_0.6 <- aggregate_cost_cohorts(auc_target = 2)

#### 6.2.6.4 AUC 0.65 ####
auc_0.65 <- aggregate_cost_cohorts(auc_target = 3)

#### 6.2.6.5 AUC 0.7 ####
auc_0.7 <- aggregate_cost_cohorts(auc_target = 4)

#### 6.2.6.6 AUC 0.75 ####
auc_0.75 <- aggregate_cost_cohorts(auc_target = 5)

#### 6.2.6.7 AUC 0.8 ####
auc_0.8 <- aggregate_cost_cohorts(auc_target = 6)

#### 6.2.6.8 AUC 0.85 ####
auc_0.85 <- aggregate_cost_cohorts(auc_target = 7)

#### 6.2.6.9 AUC 0.9 ####
auc_0.9 <- aggregate_cost_cohorts(auc_target = 8)

#### 6.2.6.10 AUC 0.95 ####
auc_0.95 <- aggregate_cost_cohorts(auc_target = 9)

#### 6.2.6.11 Compare length of FU + type of imaging in terms of cost ####
# Combine datasets and calculate cumulative dose
combine_auc_data <- function(data, auc_label) {
  data %>%
    mutate(
      auc_label = auc_label,
      risk_status = ifelse(risk_status == "LR", "Low Risk", "High Risk"),
      annual_costs = case_when(
        year == 2020 ~ cost_year_1,
        year == 2019 ~ cost_year_2,
        year == 2018 ~ cost_year_3,
        year == 2017 ~ cost_year_4,
        year == 2016 ~ cost_year_5,
        TRUE ~ NA_real_
      )
    ) %>%
    select(auc_label, cohort_type, stone_free_status, risk_status, annual_costs, true_rec_5yr)
}

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

# Summarise mean and SD by auc_label, risk_status, cohort_type
summary_df <- data_for_plot %>%
  group_by(auc_label, risk_status, cohort_type) %>%
  summarise(
    total_cost = sum(annual_costs, na.rm = TRUE),
    .groups = "drop"
  )

# Summarise mean and SD for combined risk groups ("All")
summary_all <- data_for_plot %>%
  group_by(auc_label, cohort_type) %>%
  summarise(
    total_cost = sum(annual_costs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(risk_status = "All") %>%
  select(auc_label, risk_status, cohort_type, total_cost)

# Combine all summaries
summary_df <- bind_rows(summary_df, summary_all)
summary_df <- summary_df %>% mutate(
  total_cost = total_cost / 1000000
)

# Factor levels for ordering
summary_df <- summary_df %>%
  mutate(
    cohort_type = factor(cohort_type, levels = c("Minimum FU, US", 
                                                 "Minimum FU, CT", 
                                                 "Minimum FU, XR + US",
                                                 "Maximum FU, US", 
                                                 "Maximum FU, CT", 
                                                 "Maximum FU, XR + US"
    )),
    risk_status = factor(risk_status, levels = c("All", "Low Risk", "High Risk"))
  )

# Prepare data_for_plot factors similarly
data_for_plot <- data_for_plot %>%
  mutate(
    cohort_type = factor(cohort_type, levels = c("Minimum FU, US", 
                                                 "Minimum FU, CT", 
                                                 "Minimum FU, XR + US",
                                                 "Maximum FU, US", 
                                                 "Maximum FU, CT", 
                                                 "Maximum FU, XR + US")),
    risk_status = factor(risk_status, levels = c("Low Risk", "High Risk"))
  )

# All auc values
auc_values <- unique(data_for_plot$auc_label)


# Plot total cost by cohort_type within risk groups - facet by FU type
summary_df %>% group_by(auc_label) %>% ggplot(aes(x = auc_label, y = total_cost, fill = risk_status)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~ cohort_type) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 10)
  ) +
  labs(
    title = "Annual Cost for Maximum EAU Follow-Up of those with Clinically Significant Disease",
    x = "Cohort Type",
    y = "Total Cost of Follow-up (£ Millions)",
    fill = "Risk Status"
  )

# Plot total cost by cohort_type within risk groups - facet by auc
summary_df %>% group_by(cohort_type) %>% ggplot(aes(x = cohort_type, y = total_cost, fill = risk_status)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~ auc_label) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 10)
  ) +
  labs(
    title = "Annual Cost for Maximum EAU Follow-Up of those with Clinically Significant Disease",
    x = "Cohort Type",
    y = "Total Cost of Follow-up (£ Millions)",
    fill = "Risk Status"
  )

# Plot total cost by cohort_type within risk groups - facet by risk status
summary_df %>% group_by(cohort_type) %>% ggplot(aes(x = cohort_type, y = total_cost, fill = auc_label)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~ risk_status) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 10)
  ) +
  labs(
    title = "Annual Cost for Maximum EAU Follow-Up of those with Clinically Significant Disease",
    x = "Cohort Type",
    y = "Total Cost of Follow-up (£ Millions)",
    fill = "Risk Status"
  )

summary_df_rec_only <- summary_df
