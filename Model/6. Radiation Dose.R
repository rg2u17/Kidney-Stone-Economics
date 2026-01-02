### 6.1 Radiation dose ####
#### 6.1.1 Radiation dose per year ####
##### 6.1.1.1 Radiation dose per year function ####
calculate_radiation_doses <- function(complete_pop_yr_fu, 
                                      cutpoints_yr, 
                                      auc_targets = c(0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9, 0.95), 
                                      fu_type = "min",
                                      imaging_fu_type = "xr") {
  
  results_list <- list()
  
  for (auc_target in auc_targets) {
    
    # Get cutpoint for current AUC target
    cutpoint <- (cutpoints_yr %>% filter(auc_target == !!auc_target))$cutpoint
    complete_pop_yr_fu <- complete_pop_yr_fu %>% mutate(
      risk_status = ifelse(score < cutpoint, "LR", "HR")
    )
    
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
    if (imaging_fu_type == "xr") {
      imaging1 <- xr_dose
      imaging2 <- xr_dose
      
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
            TRUE ~ stone_free_status_original 
          ),
          .keep = "all"
        )
    } else if (imaging_fu_type == "xr_us") {
      imaging1 <- xr_dose
      imaging2 <- us_dose
      
      rand_sens <- runif(nrow(complete_pop_yr_fu))
      rand_spec <- runif(nrow(complete_pop_yr_fu))
      
      complete_pop_yr_fu1 <- complete_pop_yr_fu %>%
        mutate(
          stone_free_status_original = stone_free_status,
          stone_free_status1 = case_when(
            stone_free_status_original %in% c("less4", "more4") & rand_sens <= us_sens ~ stone_free_status_original,
            stone_free_status_original %in% c("less4", "more4") & rand_sens > us_sens ~ "SF", 
            stone_free_status_original == "sf" & rand_spec <= us_spec ~ "SF", 
            stone_free_status_original == "sf" & rand_spec > us_spec ~ ifelse(
              runif(n()) <= less4_prob, "less4", "more4"
            ),
            TRUE ~ stone_free_status_original
          ),
          .keep = "all"
        )
    } else {
      # For CT imaging, no sensitivity/specificity adjustment needed
      imaging1 <- ct_dose
      imaging2 <- ct_dose
        
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
          rad_dose_year_0 = case_when(
            death_year_0 == "No" & recurrence_year_0 == "No" ~ 0,
            TRUE ~ NA_real_
          ),
          
          rad_dose_year_1 = case_when(
            death_year_1 == "No" & recurrence_year_1 == "No" & lucency == "no" ~ imaging1 * 2,
            death_year_1 == "No" & recurrence_year_1 == "No" & lucency == "yes" ~ imaging2 * 2,
            death_year_1 == "Yes" ~ 0,
            # Recurrence
            death_year_1 == "No" & colic_intervention_type_year_1 == "Colic" ~ ct_dose + urs_dose, #Minimum radiation would be 1o URS
            death_year_1 == "No" & colic_intervention_type_year_1 == "ESWL" ~ ct_dose + (2 * swl_dose),
            death_year_1 == "No" & colic_intervention_type_year_1 == "URS" ~ ct_dose + urs_dose,
            death_year_1 == "No" & colic_intervention_type_year_1 == "PCNL" ~ ct_dose + pcnl_dose,
            TRUE ~ NA_real_
          ),
          
          rad_dose_year_2 = case_when(
            # Separate Yr 2 FU into High / Low risk
            death_year_2 == "No" & recurrence_year_2 == "No" & risk_status == "LR" ~ 0,
            death_year_2 == "No" & recurrence_year_2 == "No" & risk_status == "HR" & lucency == "no" ~ imaging1,
            death_year_2 == "No" & recurrence_year_2 == "No" & risk_status == "HR" & lucency == "yes" ~ imaging2,
            # Death
            death_year_2 == "Yes" | death_year_1 == "Yes" ~ 0,
            # Recurrence
            death_year_2 == "No" & colic_intervention_type_year_2 == "Colic" ~ ct_dose + urs_dose, #Minimum radiation would be 1o URS
            death_year_2 == "No" & colic_intervention_type_year_2 == "ESWL" & recurrence_year_1 == "No" ~ ct_dose + (2 * swl_dose),
            death_year_2 == "No" & colic_intervention_type_year_2 == "URS" & recurrence_year_1 == "No" ~ ct_dose + urs_dose,
            death_year_2 == "No" & colic_intervention_type_year_2 == "PCNL" & recurrence_year_1 == "No" ~ ct_dose + pcnl_dose,
            # FU Yr 1 for those with recurrence - Presumed to be SF
            death_year_2 == "No" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & lucency == "no" ~ imaging1 * 2,
            death_year_2 == "No" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & lucency == "yes" ~ imaging2 * 2,
            TRUE ~ NA_real_
          ),
          
          rad_dose_year_3 = case_when(
            # Separate Yr 3 FU into High / Low risk
            death_year_3 == "No" & recurrence_year_3 == "No" & risk_status == "LR" & lucency == "no" ~ imaging1,
            death_year_3 == "No" & recurrence_year_3 == "No" & risk_status == "LR" & lucency == "yes" ~ imaging2,
            death_year_3 == "No" & recurrence_year_3 == "No" & risk_status == "HR" & lucency == "no" ~ imaging1,
            death_year_3 == "No" & recurrence_year_3 == "No" & risk_status == "HR" & lucency == "yes" ~ imaging2,
            # Death
            death_year_3 == "Yes" | death_year_2 == "Yes" | death_year_1 == "Yes" ~ 0,
            # Recurrence
            death_year_3 == "No" & colic_intervention_type_year_3 == "Colic" & recurrence_year_1 == "No" & recurrence_year_2 == "No" ~ ct_dose + urs_dose,
            death_year_3 == "No" & colic_intervention_type_year_3 == "ESWL" & recurrence_year_1 == "No" & recurrence_year_2 == "No" ~ ct_dose + (2 * swl_dose),
            death_year_3 == "No" & colic_intervention_type_year_3 == "URS" & recurrence_year_1 == "No" & recurrence_year_2 == "No" ~ ct_dose + urs_dose,
            death_year_3 == "No" & colic_intervention_type_year_3 == "PCNL" & recurrence_year_1 == "No" & recurrence_year_2 == "No" ~ ct_dose + pcnl_dose,
            # FU Yr 1 for those with recurrence
            death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & lucency == "no" ~ imaging1 * 2,
            death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & lucency == "yes" ~ imaging2 * 2,
            # FU Yr 2 for those with recurrence
            death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" & lucency == "no" ~ imaging1,
            death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" & lucency == "yes" ~ imaging2,
            death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" & lucency == "no" ~ imaging1,
            death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" & lucency == "yes" ~ imaging2,
            TRUE ~ NA_real_
          ),
          
          rad_dose_year_4 = case_when(
            # Separate Yr 4 FU into High / Low risk
            death_year_4 == "No" & recurrence_year_4 == "No" & risk_status == "LR" ~ 0,
            death_year_4 == "No" & recurrence_year_4 == "No" & risk_status == "HR" & lucency == "no" ~ imaging1,
            death_year_4 == "No" & recurrence_year_4 == "No" & risk_status == "HR" & lucency == "yes" ~ imaging2,
            # Death
            death_year_4 == "Yes" | death_year_3 == "Yes" | death_year_2 == "Yes" | death_year_1 == "Yes" ~ 0,
            # Recurrence
            death_year_4 == "No" & colic_intervention_type_year_4 == "Colic" &
              recurrence_year_1 == "No" & recurrence_year_2 == "No" & recurrence_year_3 == "No" ~ ct_dose + urs_dose,
            death_year_4 == "No" & colic_intervention_type_year_4 == "ESWL" &
              recurrence_year_1 == "No" & recurrence_year_2 == "No" & recurrence_year_3 == "No" ~ ct_dose + (2 * swl_dose),
            death_year_4 == "No" & colic_intervention_type_year_4 == "URS" &
              recurrence_year_1 == "No" & recurrence_year_2 == "No" & recurrence_year_3 == "No" ~ ct_dose + urs_dose,
            death_year_4 == "No" & colic_intervention_type_year_4 == "PCNL" &
              recurrence_year_1 == "No" & recurrence_year_2 == "No" & recurrence_year_3 == "No" ~ ct_dose + pcnl_dose,
            # FU Yr 1 for those with recurrence
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & lucency == "no" ~ imaging1 * 2,
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & lucency == "yes" ~ imaging2 * 2,
            # FU Yr 2 for those with recurrence
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "LR" ~ 0,
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "HR" & lucency == "no" ~ imaging1,
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "HR" & lucency == "yes" ~ imaging2,
            # FU Yr 3 for those with recurrence
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & lucency == "no" ~ imaging1,
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & lucency == "yes" ~ imaging2,
            TRUE ~ NA_real_
          ),
          
          rad_dose_year_5 = case_when(
            # Separate Yr 5 FU into High / Low risk
            death_year_5 == "No" & recurrence_year_5 == "No" & risk_status == "LR" ~ 0,
            death_year_5 == "No" & recurrence_year_5 == "No" & risk_status == "HR" & lucency == "no" ~ imaging1,
            death_year_5 == "No" & recurrence_year_5 == "No" & risk_status == "HR" & lucency == "yes" ~ imaging2,
            # Death
            death_year_5 == "Yes" |death_year_4 == "Yes" | death_year_3 == "Yes" | death_year_2 == "Yes" | death_year_1 == "Yes" ~ 0,
            # Recurrence
            death_year_5 == "No" & colic_intervention_type_year_5 == "Colic" &
              recurrence_year_1 == "No" & recurrence_year_2 == "No" & recurrence_year_3 == "No" & recurrence_year_4 == "No" ~ ct_dose + urs_dose,
            death_year_5 == "No" & colic_intervention_type_year_5 == "ESWL" &
              recurrence_year_1 == "No" & recurrence_year_2 == "No" & recurrence_year_3 == "No" & recurrence_year_4 == "No" ~ ct_dose + (2 * swl_dose),
            death_year_5 == "No" & colic_intervention_type_year_5 == "URS" &
              recurrence_year_1 == "No" & recurrence_year_2 == "No" & recurrence_year_3 == "No" & recurrence_year_4 == "No" ~ ct_dose + urs_dose,
            death_year_5 == "No" & colic_intervention_type_year_5 == "PCNL" &
              recurrence_year_1 == "No" & recurrence_year_2 == "No" & recurrence_year_3 == "No" & recurrence_year_4 == "No" ~ ct_dose + pcnl_dose,
            # FU Yr 1 for those with recurrence
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "No" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & lucency == "no" ~ imaging1 * 2,
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "No" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & lucency == "yes" ~ imaging2 * 2,
            # FU Yr 2 for those with recurrence
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ 0,
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "HR" & lucency == "no" ~ imaging1,
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "HR" & lucency == "yes" ~ imaging2,
            # FU Yr 3 for those with recurrence
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & lucency == "no" ~ imaging1,
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & lucency == "yes" ~ imaging2,
            # FU Yr 4 for those with recurrence
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & lucency == "no" ~ imaging1,
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & lucency == "yes" ~ imaging2,
            TRUE ~ NA_real_
          )
        )
    } else {
      # SF patients with maximum follow-up strategy
      fu_sf <- complete_pop_yr_fu1 %>%
        filter(stone_free_status1 == "SF" & auc_target == !!auc_target) %>%
        mutate(
          rad_dose_year_0 = case_when(
            death_year_0 == "No" & recurrence_year_0 == "No" ~ 0,
            TRUE ~ NA_real_
          ),
          
          rad_dose_year_1 = case_when(
            # FU Yr 1
            death_year_1 == "No" & recurrence_year_1 == "No" & lucency == "no" ~ imaging1 * 2,
            death_year_1 == "No" & recurrence_year_1 == "No" & lucency == "yes" ~ imaging2 * 2,
            # Death
            death_year_1 == "Yes" ~ 0,
            # Recurrence
            death_year_1 == "No" & colic_intervention_type_year_1 == "Colic" ~ ct_dose + urs_dose, #Minimum radiation would be 1o URS
            death_year_1 == "No" & colic_intervention_type_year_1 == "ESWL" ~ ct_dose + (2 * swl_dose),
            death_year_1 == "No" & colic_intervention_type_year_1 == "URS" ~ ct_dose + urs_dose,
            death_year_1 == "No" & colic_intervention_type_year_1 == "PCNL" ~ ct_dose + pcnl_dose,
            TRUE ~ NA_real_
          ),
          
          rad_dose_year_2 = case_when(
            # Separate Yr 2 FU into High / Low risk
            death_year_2 == "No" & recurrence_year_2 == "No" & risk_status == "LR" ~ 0,
            death_year_2 == "No" & recurrence_year_2 == "No" & risk_status == "HR" & lucency == "no" ~ imaging1,
            death_year_2 == "No" & recurrence_year_2 == "No" & risk_status == "HR" & lucency == "yes" ~ imaging2,
            # Death
            death_year_2 == "Yes" | death_year_1 == "Yes" ~ 0,
            # Recurrence
            death_year_2 == "No" & colic_intervention_type_year_2 == "Colic" ~ ct_dose + urs_dose, #Minimum radiation would be 1o URS
            death_year_2 == "No" & colic_intervention_type_year_2 == "ESWL" & recurrence_year_1 == "No" ~ ct_dose + (2 * swl_dose),
            death_year_2 == "No" & colic_intervention_type_year_2 == "URS" & recurrence_year_1 == "No" ~ ct_dose + urs_dose,
            death_year_2 == "No" & colic_intervention_type_year_2 == "PCNL" & recurrence_year_1 == "No" ~ ct_dose + pcnl_dose,
            # FU Yr 1 for those with recurrence
            death_year_2 == "No" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & lucency == "no" ~ imaging1 * 2,
            death_year_2 == "No" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & lucency == "yes" ~ imaging1 * 2,
            TRUE ~ NA_real_
          ),
          
          rad_dose_year_3 = case_when(
            # Separate Yr 3 FU into High / Low risk
            death_year_3 == "No" & recurrence_year_3 == "No" & lucency == "no" ~ imaging1,
            death_year_3 == "No" & recurrence_year_3 == "No" & lucency == "yes" ~ imaging2,
            # Death
            death_year_3 == "Yes" | death_year_2 == "Yes" | death_year_1 == "Yes" ~ 0,
            # Recurrence
            death_year_3 == "No" & colic_intervention_type_year_3 == "Colic" & recurrence_year_1 == "No" & recurrence_year_2 == "No" ~ ct_dose + urs_dose,
            death_year_3 == "No" & colic_intervention_type_year_3 == "ESWL" & recurrence_year_1 == "No" & recurrence_year_2 == "No" ~ ct_dose + (2 * swl_dose),
            death_year_3 == "No" & colic_intervention_type_year_3 == "URS" & recurrence_year_1 == "No" & recurrence_year_2 == "No" ~ ct_dose + urs_dose,
            death_year_3 == "No" & colic_intervention_type_year_3 == "PCNL" & recurrence_year_1 == "No" & recurrence_year_2 == "No" ~ ct_dose + pcnl_dose,
            # FU Yr 1 for those with recurrence
            death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & lucency == "no" ~ imaging1 * 2,
            death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & lucency == "yes" ~ imaging2 * 2,
            # FU Yr 2 for those with recurrence
            death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" ~ 0,
            death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" & lucency == "no" ~ imaging1,
            death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" & lucency == "yes" ~ imaging2,
            TRUE ~ NA_real_
          ),
          
          rad_dose_year_4 = case_when(
            # Separate Yr 4 FU into High / Low risk
            death_year_4 == "No" & recurrence_year_4 == "No" & lucency == "no" ~ imaging1,
            death_year_4 == "No" & recurrence_year_4 == "No" & lucency == "yes" ~ imaging2,
            # Death
            death_year_4 == "Yes" | death_year_3 == "Yes" | death_year_2 == "Yes" | death_year_1 == "Yes" ~ 0,
            # Recurrence
            death_year_4 == "No" & colic_intervention_type_year_4 == "Colic" &
              recurrence_year_1 == "No" & recurrence_year_2 == "No" & recurrence_year_3 == "No" ~ ct_dose + urs_dose,
            death_year_4 == "No" & colic_intervention_type_year_4 == "ESWL" &
              recurrence_year_1 == "No" & recurrence_year_2 == "No" & recurrence_year_3 == "No" ~ ct_dose + (2 * swl_dose),
            death_year_4 == "No" & colic_intervention_type_year_4 == "URS" &
              recurrence_year_1 == "No" & recurrence_year_2 == "No" & recurrence_year_3 == "No" ~ ct_dose + urs_dose,
            death_year_4 == "No" & colic_intervention_type_year_4 == "PCNL" &
              recurrence_year_1 == "No" & recurrence_year_2 == "No" & recurrence_year_3 == "No" ~ ct_dose + pcnl_dose,
            # FU Yr 1 for those with recurrence
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & lucency == "no" ~ imaging1 * 2,
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & lucency == "yes" ~ imaging2 * 2,
            # FU Yr 2 for those with recurrence
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "LR" ~ 0,
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "HR" & lucency == "no" ~ imaging1,
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "HR" & lucency == "yes" ~ imaging2,
            # FU Yr 3 for those with recurrence
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & lucency == "no" ~ imaging1,
            death_year_4 == "No" & recurrence_year_4 == "Yes" &
              recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & lucency == "yes" ~ imaging2,
            TRUE ~ NA_real_
          ),
          
          rad_dose_year_5 = case_when(
            # Separate Yr 5 FU into High / Low risk
            death_year_5 == "No" & recurrence_year_5 == "No" & lucency == "no" ~ imaging1,
            death_year_5 == "No" & recurrence_year_5 == "No" & lucency == "yes" ~ imaging2,
            # Death
            death_year_5 == "Yes" |death_year_4 == "Yes" | death_year_3 == "Yes" | death_year_2 == "Yes" | death_year_1 == "Yes" ~ 0,
            # Recurrence
            death_year_5 == "No" & colic_intervention_type_year_5 == "Colic" &
              recurrence_year_1 == "No" & recurrence_year_2 == "No" & recurrence_year_3 == "No" & recurrence_year_4 == "No" ~ ct_dose + urs_dose,
            death_year_5 == "No" & colic_intervention_type_year_5 == "ESWL" &
              recurrence_year_1 == "No" & recurrence_year_2 == "No" & recurrence_year_3 == "No" & recurrence_year_4 == "No" ~ ct_dose + (2 * swl_dose),
            death_year_5 == "No" & colic_intervention_type_year_5 == "URS" &
              recurrence_year_1 == "No" & recurrence_year_2 == "No" & recurrence_year_3 == "No" & recurrence_year_4 == "No" ~ ct_dose + urs_dose,
            death_year_5 == "No" & colic_intervention_type_year_5 == "PCNL" &
              recurrence_year_1 == "No" & recurrence_year_2 == "No" & recurrence_year_3 == "No" & recurrence_year_4 == "No" ~ ct_dose + pcnl_dose,
            # FU Yr 1 for those with recurrence
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "No" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & lucency == "no" ~ imaging1 * 2,
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "No" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & lucency == "yes" ~ imaging2 * 2,
            # FU Yr 2 for those with recurrence
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ 0,
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "HR" & lucency == "no" ~ imaging1,
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "HR" & lucency == "yes" ~ imaging2,
            # FU Yr 3 for those with recurrence
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & lucency == "no" ~ imaging1,
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & lucency == "yes" ~ imaging2,
            # FU Yr 4 for those with recurrence
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & lucency == "no" ~ imaging1,
            death_year_5 == "No" & recurrence_year_5 == "Yes" &
              recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & lucency == "yes" ~ imaging2,
            TRUE ~ NA_real_
          )
        )
    }
    
    # Less than 4mm stones
    fu_less4 <- complete_pop_yr_fu1 %>% 
      filter(stone_free_status1 == "less4" & auc_target == !!auc_target) %>%
      mutate(
        rad_dose_year_0 = case_when(
          death_year_0 == "No" & recurrence_year_0 == "No" ~ 0,
          TRUE ~ NA_real_
        ),
        
        rad_dose_year_1 = case_when(
          death_year_1 == "No" & recurrence_year_1 == "No" & lucency == "no" ~ imaging1 * 2,
          death_year_1 == "No" & recurrence_year_1 == "No" & lucency == "yes" ~ imaging2 * 2,
          # Death
          death_year_1 == "Yes" ~ 0,
          # Recurrence
          death_year_1 == "No" & colic_intervention_type_year_1 == "Colic" ~ ct_dose + urs_dose, #Minimum radiation would be 1o URS
          death_year_1 == "No" & colic_intervention_type_year_1 == "ESWL" ~ ct_dose + (2 * swl_dose),
          death_year_1 == "No" & colic_intervention_type_year_1 == "URS" ~ ct_dose + urs_dose,
          death_year_1 == "No" & colic_intervention_type_year_1 == "PCNL" ~ ct_dose + pcnl_dose,
          TRUE ~ NA_real_
        ),
        
        rad_dose_year_2 = case_when(
          death_year_2 == "No" & recurrence_year_2 == "No" & lucency == "no" ~ imaging1,
          death_year_2 == "No" & recurrence_year_2 == "No" & lucency == "yes" ~ imaging2,
          # Death
          death_year_2 == "Yes" | death_year_1 == "Yes" ~ 0,
          # Recurrence
          death_year_2 == "No" & colic_intervention_type_year_2 == "Colic" ~ ct_dose + urs_dose, #Minimum radiation would be 1o URS
          death_year_2 == "No" & colic_intervention_type_year_2 == "ESWL" & recurrence_year_1 == "No" ~ ct_dose + (2 * swl_dose),
          death_year_2 == "No" & colic_intervention_type_year_2 == "URS" & recurrence_year_1 == "No" ~ ct_dose + urs_dose,
          death_year_2 == "No" & colic_intervention_type_year_2 == "PCNL" & recurrence_year_1 == "No" ~ ct_dose + pcnl_dose,
          # Yr 1 FU for those with recurrence
          death_year_2 == "No" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & lucency == "no" ~ imaging1 * 2,
          death_year_2 == "No" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & lucency == "yes" ~ imaging2 * 2,
          TRUE ~ NA_real_
        ),
        
        rad_dose_year_3 = case_when(
          death_year_3 == "No" & recurrence_year_3 == "No" & lucency == "no" ~ imaging1,
          death_year_3 == "No" & recurrence_year_3 == "No" & lucency == "yes" ~ imaging2,
          # Death
          death_year_3 == "Yes" | death_year_2 == "Yes" | death_year_1 == "Yes" ~ 0,
          # Recurrence
          death_year_3 == "No" & colic_intervention_type_year_3 == "Colic" & recurrence_year_1 == "No" & recurrence_year_2 == "No" ~ ct_dose + urs_dose,
          death_year_3 == "No" & colic_intervention_type_year_3 == "ESWL" & recurrence_year_1 == "No" & recurrence_year_2 == "No" ~ ct_dose + (2 * swl_dose),
          death_year_3 == "No" & colic_intervention_type_year_3 == "URS" & recurrence_year_1 == "No" & recurrence_year_2 == "No" ~ ct_dose + urs_dose,
          death_year_3 == "No" & colic_intervention_type_year_3 == "PCNL" & recurrence_year_1 == "No" & recurrence_year_2 == "No" ~ ct_dose + pcnl_dose,
          # Yr 1 FU for those with recurrence
          death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & lucency == "no" ~ imaging1 * 2,
          death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & lucency == "yes" ~ imaging2 * 2,
          # Yr 2 FU for those with recurrence
          death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" ~ 0,
          death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" & lucency == "no" ~ imaging1,
          death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" & lucency == "yes" ~ imaging2,
          TRUE ~ NA_real_
        ),
        
        rad_dose_year_4 = case_when(
          death_year_4 == "No" & recurrence_year_4 == "No" & lucency == "no" ~ imaging1,
          death_year_4 == "No" & recurrence_year_4 == "No" & lucency == "yes" ~ imaging2,
          # Death
          death_year_4 == "Yes" | death_year_3 == "Yes" | death_year_2 == "Yes" | death_year_1 == "Yes" ~ 0,
          # Recurrence
          death_year_4 == "No" & colic_intervention_type_year_4 == "Colic" &
            recurrence_year_1 == "No" & recurrence_year_2 == "No" & recurrence_year_3 == "No" ~ ct_dose + urs_dose,
          death_year_4 == "No" & colic_intervention_type_year_4 == "ESWL" &
            recurrence_year_1 == "No" & recurrence_year_2 == "No" & recurrence_year_3 == "No" ~ ct_dose + (2 * swl_dose),
          death_year_4 == "No" & colic_intervention_type_year_4 == "URS" &
            recurrence_year_1 == "No" & recurrence_year_2 == "No" & recurrence_year_3 == "No" ~ ct_dose + urs_dose,
          death_year_4 == "No" & colic_intervention_type_year_4 == "PCNL" &
            recurrence_year_1 == "No" & recurrence_year_2 == "No" & recurrence_year_3 == "No" ~ ct_dose + pcnl_dose,
          # Yr 1 FU for those with recurrence
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & lucency == "no" ~ imaging1 * 2,
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & lucency == "yes" ~ imaging2 * 2,
          # Yr 2 FU for those with recurrence
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "LR" ~ 0,
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "HR" & lucency == "no" ~ imaging1,
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "HR" & lucency == "yes" ~ imaging2,
          # Yr 3 FU for those with recurrence
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & lucency == "no" ~ imaging1,
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & lucency == "yes" ~ imaging2,
          TRUE ~ NA_real_
        ),
        
        rad_dose_year_5 = case_when(
          death_year_5 == "No" & recurrence_year_5 == "No" & lucency == "no" ~ imaging1,
          death_year_5 == "No" & recurrence_year_5 == "No" & lucency == "yes" ~ imaging2,
          # Death
          death_year_5 == "Yes" | death_year_4 == "Yes" | death_year_3 == "Yes" | death_year_2 == "Yes" | death_year_1 == "Yes" ~ 0,
          # Recurrence
          death_year_5 == "No" & colic_intervention_type_year_5 == "Colic" &
            recurrence_year_1 == "No" & recurrence_year_2 == "No" & recurrence_year_3 == "No" & recurrence_year_4 == "No" ~ ct_dose + urs_dose,
          death_year_5 == "No" & colic_intervention_type_year_5 == "ESWL" &
            recurrence_year_1 == "No" & recurrence_year_2 == "No" & recurrence_year_3 == "No" & recurrence_year_4 == "No" ~ ct_dose + (2 * swl_dose),
          death_year_5 == "No" & colic_intervention_type_year_5 == "URS" &
            recurrence_year_1 == "No" & recurrence_year_2 == "No" & recurrence_year_3 == "No" & recurrence_year_4 == "No" ~ ct_dose + urs_dose,
          death_year_5 == "No" & colic_intervention_type_year_5 == "PCNL" &
            recurrence_year_1 == "No" & recurrence_year_2 == "No" & recurrence_year_3 == "No" & recurrence_year_4 == "No" ~ ct_dose + pcnl_dose,
          # Yr 1 FU for those with recurrence
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "No" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & lucency == "no" ~ imaging1 * 2,
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "No" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & lucency == "yes" ~ imaging2 * 2,
          # Yr 2 FU for those with recurrence
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ 0,
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "HR" & lucency == "no" ~ imaging1,
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "HR" & lucency == "yes" ~ imaging2,
          # Yr 3 FU for those with recurrence
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & lucency == "no" ~ imaging1,
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & lucency == "yes" ~ imaging2,
          # Yr 4 FU for those with recurrence
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & lucency == "no" ~ imaging1,
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & lucency == "yes" ~ imaging2,
          TRUE ~ NA_real_
        )
      )
    
    # More than 4mm stones
    fu_more4 <- complete_pop_yr_fu1 %>% 
      filter(stone_free_status1 == "more4" & auc_target == !!auc_target) %>%
      mutate(
        rad_dose_year_0 = case_when(
          death_year_0 == "No" & recurrence_year_0 == "No" ~ 0,
          TRUE ~ NA_real_
        ),
        
        rad_dose_year_1 = case_when(
          death_year_1 == "No" & recurrence_year_1 == "No" & lucency == "no" ~ imaging1 * 2,
          death_year_1 == "No" & recurrence_year_1 == "No" & lucency == "yes" ~ imaging2 * 2,
          # Death
          death_year_1 == "Yes" ~ 0,
          # Recurrence
          death_year_1 == "No" & colic_intervention_type_year_1 == "Colic" ~ ct_dose + urs_dose, #Minimum radiation would be 1o URS
          death_year_1 == "No" & colic_intervention_type_year_1 == "ESWL" ~ ct_dose + (2 * swl_dose),
          death_year_1 == "No" & colic_intervention_type_year_1 == "URS" ~ ct_dose + urs_dose,
          death_year_1 == "No" & colic_intervention_type_year_1 == "PCNL" ~ ct_dose + pcnl_dose,
          TRUE ~ NA_real_
        ),
        
        rad_dose_year_2 = case_when(
          death_year_2 == "No" & recurrence_year_2 == "No" & lucency == "no" ~ imaging1 * 2,
          death_year_2 == "No" & recurrence_year_2 == "No" & lucency == "yes" ~ imaging2 * 2,
          # Death
          death_year_2 == "Yes" | death_year_1 == "Yes" ~ 0,
          # Recurrence
          death_year_2 == "No" & colic_intervention_type_year_2 == "Colic" ~ ct_dose + urs_dose, #Minimum radiation would be 1o URS
          death_year_2 == "No" & colic_intervention_type_year_2 == "ESWL" & recurrence_year_1 == "No" ~ ct_dose + (2 * swl_dose),
          death_year_2 == "No" & colic_intervention_type_year_2 == "URS" & recurrence_year_1 == "No" ~ ct_dose + urs_dose,
          death_year_2 == "No" & colic_intervention_type_year_2 == "PCNL" & recurrence_year_1 == "No" ~ ct_dose + pcnl_dose,
          # Yr 1 of FU for those with recurrence
          death_year_2 == "No" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & lucency == "no" ~ imaging1 * 2,
          death_year_2 == "No" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & lucency == "yes" ~ imaging2 * 2,
          TRUE ~ NA_real_
        ),
        
        rad_dose_year_3 = case_when(
          death_year_3 == "No" & recurrence_year_3 == "No" & lucency == "no" ~ imaging1,
          death_year_3 == "No" & recurrence_year_3 == "No" & lucency == "yes" ~ imaging2,
          # Death
          death_year_3 == "Yes" | death_year_2 == "Yes" | death_year_1 == "Yes" ~ 0,
          # Recurrence
          death_year_3 == "No" & colic_intervention_type_year_3 == "Colic" & recurrence_year_1 == "No" & recurrence_year_2 == "No" ~ ct_dose + urs_dose,
          death_year_3 == "No" & colic_intervention_type_year_3 == "ESWL" & recurrence_year_1 == "No" & recurrence_year_2 == "No" ~ ct_dose + (2 * swl_dose),
          death_year_3 == "No" & colic_intervention_type_year_3 == "URS" & recurrence_year_1 == "No" & recurrence_year_2 == "No" ~ ct_dose + urs_dose,
          death_year_3 == "No" & colic_intervention_type_year_3 == "PCNL" & recurrence_year_1 == "No" & recurrence_year_2 == "No" ~ ct_dose + pcnl_dose,
          # Yr 1 FU for those with recurrence
          death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & lucency == "no" ~ imaging1 * 2,
          death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & lucency == "yes" ~ imaging2 * 2,
          # Yr 2 FU for those with recurrence
          death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "LR" ~ 0,
          death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" & lucency == "no" ~ imaging1,
          death_year_3 == "No" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & risk_status == "HR" & lucency == "yes" ~ imaging2,
          TRUE ~ NA_real_
        ),
        
        rad_dose_year_4 = case_when(
          death_year_4 == "No" & recurrence_year_4 == "No" & lucency == "no" ~ imaging1,
          death_year_4 == "No" & recurrence_year_4 == "No" & lucency == "yes" ~ imaging2,
          # Death
          death_year_4 == "Yes" | death_year_3 == "Yes" | death_year_2 == "Yes" | death_year_1 == "Yes" ~ 0,
          # Recurrence
          death_year_4 == "No" & colic_intervention_type_year_4 == "Colic" &
            recurrence_year_1 == "No" & recurrence_year_2 == "No" & recurrence_year_3 == "No" ~ ct_dose + urs_dose,
          death_year_4 == "No" & colic_intervention_type_year_4 == "ESWL" &
            recurrence_year_1 == "No" & recurrence_year_2 == "No" & recurrence_year_3 == "No" ~ ct_dose + (2 * swl_dose),
          death_year_4 == "No" & colic_intervention_type_year_4 == "URS" &
            recurrence_year_1 == "No" & recurrence_year_2 == "No" & recurrence_year_3 == "No" ~ ct_dose + urs_dose,
          death_year_4 == "No" & colic_intervention_type_year_4 == "PCNL" &
            recurrence_year_1 == "No" & recurrence_year_2 == "No" & recurrence_year_3 == "No" ~ ct_dose + pcnl_dose,
          # Yr 1 FU for those with recurrence
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & lucency == "no" ~ imaging1 * 2,
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & lucency == "yes" ~ imaging2 * 2,
          # Yr 2 FU for those with recurrence
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "LR" ~ 0,
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "HR" & lucency == "no" ~ imaging1,
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & risk_status == "HR" & lucency == "yes" ~ imaging2,
          # Yr 3 FU for those with recurrence
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & lucency == "no" ~ imaging1,
          death_year_4 == "No" & recurrence_year_4 == "Yes" &
            recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & lucency == "yes" ~ imaging2,
          TRUE ~ NA_real_
        ),
        
        rad_dose_year_5 = case_when(
          death_year_5 == "No" & recurrence_year_5 == "No" & lucency == "no" ~ imaging1,
          death_year_5 == "No" & recurrence_year_5 == "No" & lucency == "yes" ~ imaging2,
          # Death
          death_year_5 == "Yes" | death_year_4 == "Yes" | death_year_3 == "Yes" | death_year_2 == "Yes" | death_year_1 == "Yes" ~ 0,
          # Recurrence
          death_year_5 == "No" & colic_intervention_type_year_5 == "Colic" &
            recurrence_year_1 == "No" & recurrence_year_2 == "No" & recurrence_year_3 == "No" & recurrence_year_4 == "No" ~ ct_dose + urs_dose,
          death_year_5 == "No" & colic_intervention_type_year_5 == "ESWL" &
            recurrence_year_1 == "No" & recurrence_year_2 == "No" & recurrence_year_3 == "No" & recurrence_year_4 == "No" ~ ct_dose + (2 * swl_dose),
          death_year_5 == "No" & colic_intervention_type_year_5 == "URS" &
            recurrence_year_1 == "No" & recurrence_year_2 == "No" & recurrence_year_3 == "No" & recurrence_year_4 == "No" ~ ct_dose + urs_dose,
          death_year_5 == "No" & colic_intervention_type_year_5 == "PCNL" &
            recurrence_year_1 == "No" & recurrence_year_2 == "No" & recurrence_year_3 == "No" & recurrence_year_4 == "No" ~ ct_dose + pcnl_dose,
          # Yr 1 FU for those with recurrence
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "No" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & lucency == "no" ~ imaging1 * 2,
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "No" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & lucency == "yes" ~ imaging2 * 2,
          # Yr 2 FU for those with recurrence
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No" & risk_status == "LR" ~ 0,
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No"  & risk_status == "HR" & lucency == "no" ~ imaging1,
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "No" & recurrence_year_1 == "No"  & risk_status == "HR" & lucency == "yes" ~ imaging2,
          # Yr 3 FU for those with recurrence
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & lucency == "no" ~ imaging1,
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "No" & lucency == "yes" ~ imaging2,
          # Yr 4 FU for those with recurrence
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & lucency == "no" ~ imaging1,
          death_year_5 == "No" & recurrence_year_5 == "Yes" &
            recurrence_year_4 == "Yes" & recurrence_year_3 == "Yes" & recurrence_year_2 == "Yes" & recurrence_year_1 == "Yes" & lucency == "yes" ~ imaging2,
          TRUE ~ NA_real_
        )
      )
    
    # Combine all stone status groups for this AUC target
    combined_result <- rbind(fu_sf, fu_less4, fu_more4) %>% 
      as_tibble() %>%
      mutate(auc_target = auc_target, cutpoint = cutpoint)
    
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

##### 6.1.1.2 Radiation dose per year plotting function ####
create_radiation_dose_barplot <- function(results_list, title, cohort) {
  # Process data
  processed_data <- map_dfr(results_list, function(df) {
    auc_target <- unique(df$auc_target)[1]
    cutpoint <- unique(df$cutpoint)[1]
    
    df_with_risk <- df %>%
      filter(auc_target == !!auc_target) %>%
      mutate(
        risk_group = case_when(
          score < cutpoint ~ "Low Risk",
          score >= cutpoint ~ "High Risk",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(risk_group))
    
    df_long <- df_with_risk %>%
      select(auc_target, cutpoint, risk_group, starts_with("rad_dose_year_")) %>%
      pivot_longer(
        cols = starts_with("rad_dose_year_"),
        names_to = "year",
        values_to = "radiation_dose",
        names_prefix = "rad_dose_year_"
      ) %>%
      mutate(year = as.numeric(year))
    
    df_summary <- df_long %>%
      group_by(auc_target, risk_group, year) %>%
      summarise(
        mean_dose = mean(radiation_dose, na.rm = TRUE),
        se = sd(radiation_dose, na.rm = TRUE) / sqrt(n()),
        n = sum(!is.na(radiation_dose)),
        .groups = "drop"
      ) %>%
      filter(n > 0)
    
    return(df_summary)
  })
  
  # Long-format full data
  df_long_all <- map_dfr(results_list, function(df) {
    auc_target <- unique(df$auc_target)[1]
    cutpoint <- unique(df$cutpoint)[1]
    
    df_with_risk <- df %>%
      filter(auc_target == !!auc_target) %>%
      mutate(
        risk_group = case_when(
          score < cutpoint ~ "Low Risk",
          score >= cutpoint ~ "High Risk",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(risk_group)) %>%
      select(auc_target, cutpoint, risk_group, starts_with("rad_dose_year_")) %>%
      pivot_longer(
        cols = starts_with("rad_dose_year_"),
        names_to = "year",
        values_to = "radiation_dose",
        names_prefix = "rad_dose_year_"
      ) %>%
      mutate(year = as.numeric(year))
    
    return(df_with_risk)
  })
  
  # Unadjusted p-values
  pval_data <- df_long_all %>%
    group_by(auc_target, year) %>%
    summarise(
      p = tryCatch(t.test(radiation_dose ~ risk_group)$p.value, error = function(e) NA_real_),
      .groups = "drop"
    )
  
  # Apply FDR correction
  pval_data <- pval_data %>%
    mutate(p_fdr = p.adjust(p, method = "fdr")) 
  
  # Merge into processed data
  processed_data <- processed_data %>%
    left_join(pval_data, by = c("auc_target", "year"))
  
  # y-position for labels and brackets
  label_data <- processed_data %>%
    group_by(auc_target, year) %>%
    summarise(
      y_position = max(mean_dose + se, na.rm = TRUE) + 1,
      p_fdr = unique(p_fdr),
      .groups = "drop"
    )  %>%
    mutate(
      signif_label = case_when(
        is.na(p_fdr)     ~ "ns",
        p_fdr <= 0.001   ~ "***",
        p_fdr <= 0.01    ~ "**",
        p_fdr <= 0.05    ~ "*",
        TRUE             ~ "ns"
      ),
      # Convert year to factor positions for x coordinates
      x_pos = as.numeric(factor(year)),
      start = x_pos - 0.2,
      end = x_pos + 0.2,
      group = paste0("AUC Target: ", auc_target)
    ) %>%
    filter(signif_label != "ns")  # keep only significant
  
  # Now plot
  p <- processed_data %>%
    ggplot(aes(
      x = factor(year),
      y = mean_dose,
      fill = risk_group
    )) +
    geom_col(
      position = position_dodge(width = 0.7),
      alpha = 0.8,
      width = 0.7
    ) +
    geom_errorbar(
      aes(ymin = mean_dose - se, ymax = mean_dose + se),
      position = position_dodge(width = 0.7),
      width = 0.2
    ) +
    facet_wrap(~ paste0("AUC Target: ", auc_target),
               scales = "free_y",
               ncol = 3) +
    scale_fill_manual(
      values = c(
        "Low Risk" = "#2E86AB",
        "High Risk" = "#A23B72"
      ),
      name = "Risk Group"
    ) +
    labs(
      title = paste0("Mean Radiation Dose by Follow-up Year and Risk Group for Cohort: ", cohort),
      subtitle = title,
      x = "Follow-up Year",
      y = "Radiation Dose (mSv)",
      caption = "Significance shown after FDR correction"
    ) + 
    ylim(c(0, 11)) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray60"),
      strip.text = element_text(size = 10, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
  
  # Add significance annotations using geom_text and geom_segment
  if (nrow(label_data) > 0) {
    p <- p + 
      geom_segment(data = label_data,
                   aes(x = start, xend = end, y = y_position, yend = y_position),
                   inherit.aes = FALSE, color = "black") +
      geom_segment(data = label_data,
                   aes(x = start, xend = start, y = y_position, yend = y_position - 0.2),
                   inherit.aes = FALSE, color = "black") +
      geom_segment(data = label_data,
                   aes(x = end, xend = end, y = y_position, yend = y_position - 0.2),
                   inherit.aes = FALSE, color = "black") +
      geom_text(data = label_data,
                aes(x = (start + end) / 2, y = y_position + 0.3, label = signif_label),
                inherit.aes = FALSE, size = 3, vjust = 0.5)
  }
  
  return(p)
}

##### 6.1.1.3 2016 ####
# Minimum follow-up XR
rad_doses_2016_cohort_min_xr <- calculate_radiation_doses(complete_pop_2016_fu, 
                                                       cutpoints_2016, 
                                                       fu_type = "min",
                                                       imaging_fu_type = "xr")

create_radiation_dose_barplot(rad_doses_2016_cohort_min_xr,
                              title = "XR only, Minimum EAU Follow-up",
                              cohort = 2016)

# Maximum follow-up XR
rad_doses_2016_cohort_max_xr <- calculate_radiation_doses(complete_pop_2016_fu, 
                                                       cutpoints_2016, 
                                                       fu_type = "max",
                                                       imaging_fu_type = "xr")

create_radiation_dose_barplot(rad_doses_2016_cohort_max_xr,
                              title = "XR only, Maximum EAU Follow-up",
                              cohort = 2016)

# Minimum follow-up XR + US
rad_doses_2016_cohort_min_xr_us <- calculate_radiation_doses(complete_pop_2016_fu, 
                                                          cutpoints_2016, 
                                                          fu_type = "min",
                                                          imaging_fu_type = "xr_us")

# Maximum follow-up XR + US
rad_doses_2016_cohort_max_xr_us <- calculate_radiation_doses(complete_pop_2016_fu, 
                                                             cutpoints_2016, 
                                                             fu_type = "max",
                                                             imaging_fu_type = "xr_us")

# Minimum follow-up ULDCT
rad_doses_2016_cohort_min_ct <- calculate_radiation_doses(complete_pop_2016_fu, 
                                                          cutpoints_2016, 
                                                          fu_type = "min",
                                                          imaging_fu_type = "uldct")

create_radiation_dose_barplot(rad_doses_2016_cohort_min_ct,
                              title = "ULDCT only, Minimum EAU Follow-up",
                              cohort = 2016)

# Maximum follow-up ULDCT
rad_doses_2016_cohort_max_ct <- calculate_radiation_doses(complete_pop_2016_fu, 
                                                          cutpoints_2016, 
                                                          fu_type = "max",
                                                          imaging_fu_type = "uldct")

create_radiation_dose_barplot(rad_doses_2016_cohort_max_ct,
                              title = "ULDCT only, Maximum EAU Follow-up",
                              cohort = 2016)

##### 6.1.1.4 2017 ####
# Minimum follow-up XR
rad_doses_2017_cohort_min_xr <- calculate_radiation_doses(complete_pop_2017_fu, 
                                                          cutpoints_2017, 
                                                          fu_type = "min",
                                                          imaging_fu_type = "xr")

# Maximum follow-up XR
rad_doses_2017_cohort_max_xr <- calculate_radiation_doses(complete_pop_2017_fu, 
                                                          cutpoints_2017, 
                                                          fu_type = "max",
                                                          imaging_fu_type = "xr")

# Minimum follow-up XR + US
rad_doses_2017_cohort_min_xr_us <- calculate_radiation_doses(complete_pop_2017_fu, 
                                                             cutpoints_2017, 
                                                             fu_type = "min",
                                                             imaging_fu_type = "xr_us")

# Maximum follow-up XR + US
rad_doses_2017_cohort_max_xr_us <- calculate_radiation_doses(complete_pop_2017_fu, 
                                                             cutpoints_2017, 
                                                             fu_type = "max",
                                                             imaging_fu_type = "xr_us")

# Minimum follow-up ULDCT
rad_doses_2017_cohort_min_ct <- calculate_radiation_doses(complete_pop_2017_fu, 
                                                          cutpoints_2017, 
                                                          fu_type = "min",
                                                          imaging_fu_type = "uldct")


# Maximum follow-up ULDCT
rad_doses_2017_cohort_max_ct <- calculate_radiation_doses(complete_pop_2017_fu, 
                                                          cutpoints_2017, 
                                                          fu_type = "max",
                                                          imaging_fu_type = "uldct")

##### 6.1.1.5 2018 ####
# Minimum follow-up XR
rad_doses_2018_cohort_min_xr <- calculate_radiation_doses(complete_pop_2018_fu, 
                                                          cutpoints_2018, 
                                                          fu_type = "min",
                                                          imaging_fu_type = "xr")


# Maximum follow-up XR
rad_doses_2018_cohort_max_xr <- calculate_radiation_doses(complete_pop_2018_fu, 
                                                          cutpoints_2018, 
                                                          fu_type = "max",
                                                          imaging_fu_type = "xr")


# Minimum follow-up XR + US
rad_doses_2018_cohort_min_xr_us <- calculate_radiation_doses(complete_pop_2018_fu, 
                                                             cutpoints_2018, 
                                                             fu_type = "min",
                                                             imaging_fu_type = "xr_us")

# Maximum follow-up XR + US
rad_doses_2018_cohort_max_xr_us <- calculate_radiation_doses(complete_pop_2018_fu, 
                                                             cutpoints_2018, 
                                                             fu_type = "max",
                                                             imaging_fu_type = "xr_us")

# Minimum follow-up ULDCT
rad_doses_2018_cohort_min_ct <- calculate_radiation_doses(complete_pop_2018_fu, 
                                                          cutpoints_2018, 
                                                          fu_type = "min",
                                                          imaging_fu_type = "uldct")


# Maximum follow-up ULDCT
rad_doses_2018_cohort_max_ct <- calculate_radiation_doses(complete_pop_2018_fu, 
                                                          cutpoints_2018, 
                                                          fu_type = "max",
                                                          imaging_fu_type = "uldct")


##### 6.1.1.6 2019 ####
# Minimum follow-up XR
rad_doses_2019_cohort_min_xr <- calculate_radiation_doses(complete_pop_2019_fu, 
                                                          cutpoints_2019, 
                                                          fu_type = "min",
                                                          imaging_fu_type = "xr")


# Maximum follow-up XR
rad_doses_2019_cohort_max_xr <- calculate_radiation_doses(complete_pop_2019_fu, 
                                                          cutpoints_2019, 
                                                          fu_type = "max",
                                                          imaging_fu_type = "xr")


# Minimum follow-up XR + US
rad_doses_2019_cohort_min_xr_us <- calculate_radiation_doses(complete_pop_2019_fu, 
                                                             cutpoints_2019, 
                                                             fu_type = "min",
                                                             imaging_fu_type = "xr_us")

# Maximum follow-up XR + US
rad_doses_2019_cohort_max_xr_us <- calculate_radiation_doses(complete_pop_2019_fu, 
                                                             cutpoints_2019, 
                                                             fu_type = "max",
                                                             imaging_fu_type = "xr_us")

# Minimum follow-up ULDCT
rad_doses_2019_cohort_min_ct <- calculate_radiation_doses(complete_pop_2019_fu, 
                                                          cutpoints_2019, 
                                                          fu_type = "min",
                                                          imaging_fu_type = "uldct")


# Maximum follow-up ULDCT
rad_doses_2019_cohort_max_ct <- calculate_radiation_doses(complete_pop_2019_fu, 
                                                          cutpoints_2019, 
                                                          fu_type = "max",
                                                          imaging_fu_type = "uldct")


##### 6.1.1.7 2020 ####
# Minimum follow-up XR
rad_doses_2020_cohort_min_xr <- calculate_radiation_doses(complete_pop_2020_fu, 
                                                          cutpoints_2020, 
                                                          fu_type = "min",
                                                          imaging_fu_type = "xr")


# Maximum follow-up XR
rad_doses_2020_cohort_max_xr <- calculate_radiation_doses(complete_pop_2020_fu, 
                                                          cutpoints_2020, 
                                                          fu_type = "max",
                                                          imaging_fu_type = "xr")

# Minimum follow-up XR + US
rad_doses_2020_cohort_min_xr_us <- calculate_radiation_doses(complete_pop_2020_fu, 
                                                             cutpoints_2020, 
                                                             fu_type = "min",
                                                             imaging_fu_type = "xr_us")

# Maximum follow-up XR + US
rad_doses_2020_cohort_max_xr_us <- calculate_radiation_doses(complete_pop_2020_fu, 
                                                             cutpoints_2020, 
                                                             fu_type = "max",
                                                             imaging_fu_type = "xr_us")

# Minimum follow-up ULDCT
rad_doses_2020_cohort_min_ct <- calculate_radiation_doses(complete_pop_2020_fu, 
                                                          cutpoints_2020, 
                                                          fu_type = "min",
                                                          imaging_fu_type = "uldct")


# Maximum follow-up ULDCT
rad_doses_2020_cohort_max_ct <- calculate_radiation_doses(complete_pop_2020_fu, 
                                                          cutpoints_2020, 
                                                          fu_type = "max",
                                                          imaging_fu_type = "uldct")


#### 6.1.2 Radiation dose over 5 years ####
##### 6.1.2.1 Function to Aggregate cohorts ####
aggregate_radiation_cohorts <- function(auc_target = c(1,2,3,4,5,6,7,8,9)) {
  all_cohorts <- list()
  
  for (i in auc_target) {
    key <- as.integer(i)
    message("Processing AUC = ", key)
    
    message("  Loading 2016 data...")
    cohort_2016_min_xr <- rad_doses_2016_cohort_min_xr[[key]] %>% mutate(cohort_type = "Minimum FU, XR", auc = i)
    cohort_2016_min_xr_us <- rad_doses_2016_cohort_min_xr_us[[key]] %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2016_min_ct <- rad_doses_2016_cohort_min_ct[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2016_max_xr <- rad_doses_2016_cohort_max_xr[[key]] %>% mutate(cohort_type = "Maximum FU, XR", auc = i)
    cohort_2016_max_xr_us <- rad_doses_2016_cohort_max_xr_us[[key]] %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    cohort_2016_max_ct <- rad_doses_2016_cohort_max_ct[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    
    message("  Loading 2017 data...")
    cohort_2017_min_xr <- rad_doses_2017_cohort_min_xr[[key]] %>% mutate(cohort_type = "Minimum FU, XR", auc = i)
    cohort_2017_min_xr_us <- rad_doses_2017_cohort_min_xr_us[[key]] %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2017_min_ct <- rad_doses_2017_cohort_min_ct[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2017_max_xr <- rad_doses_2017_cohort_max_xr[[key]] %>% mutate(cohort_type = "Maximum FU, XR", auc = i)
    cohort_2017_max_xr_us <- rad_doses_2017_cohort_max_xr_us[[key]] %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    cohort_2017_max_ct <- rad_doses_2017_cohort_max_ct[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    
    message("  Loading 2018 data...")
    cohort_2018_min_xr <- rad_doses_2018_cohort_min_xr[[key]] %>% mutate(cohort_type = "Minimum FU, XR", auc = i)
    cohort_2018_min_xr_us <- rad_doses_2018_cohort_min_xr_us[[key]] %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2018_min_ct <- rad_doses_2018_cohort_min_ct[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2018_max_xr <- rad_doses_2018_cohort_max_xr[[key]] %>% mutate(cohort_type = "Maximum FU, XR", auc = i)
    cohort_2018_max_xr_us <- rad_doses_2018_cohort_max_xr_us[[key]] %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    cohort_2018_max_ct <- rad_doses_2018_cohort_max_ct[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    
    message("  Loading 2019 data...")
    cohort_2019_min_xr <- rad_doses_2019_cohort_min_xr[[key]] %>% mutate(cohort_type = "Minimum FU, XR", auc = i)
    cohort_2019_min_xr_us <- rad_doses_2019_cohort_min_xr_us[[key]] %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2019_min_ct <- rad_doses_2019_cohort_min_ct[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2019_max_xr <- rad_doses_2019_cohort_max_xr[[key]] %>% mutate(cohort_type = "Maximum FU, XR", auc = i)
    cohort_2019_max_xr_us <- rad_doses_2019_cohort_max_xr_us[[key]] %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    cohort_2019_max_ct <- rad_doses_2019_cohort_max_ct[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    
    message("  Loading 2020 data...")
    cohort_2020_min_xr <- rad_doses_2020_cohort_min_xr[[key]] %>% mutate(cohort_type = "Minimum FU, XR", auc = i)
    cohort_2020_min_xr_us <- rad_doses_2020_cohort_min_xr_us[[key]] %>% mutate(cohort_type = "Minimum FU, XR + US", auc = i)
    cohort_2020_min_ct <- rad_doses_2020_cohort_min_ct[[key]] %>% mutate(cohort_type = "Minimum FU, CT", auc = i)
    cohort_2020_max_xr <- rad_doses_2020_cohort_max_xr[[key]] %>% mutate(cohort_type = "Maximum FU, XR", auc = i)
    cohort_2020_max_xr_us <- rad_doses_2020_cohort_max_xr_us[[key]] %>% mutate(cohort_type = "Maximum FU, XR + US", auc = i)
    cohort_2020_max_ct <- rad_doses_2020_cohort_max_ct[[key]] %>% mutate(cohort_type = "Maximum FU, CT", auc = i)
    
    message("  Combining cohorts for AUC = ", key)
    overall_cohort <- dplyr::bind_rows(
      cohort_2016_min_xr, cohort_2016_min_xr_us, cohort_2016_min_ct, cohort_2016_max_xr, cohort_2016_max_xr_us, cohort_2016_max_ct,
      cohort_2017_min_xr, cohort_2017_min_xr_us, cohort_2017_min_ct, cohort_2017_max_xr, cohort_2017_max_xr_us, cohort_2017_max_ct,
      cohort_2018_min_xr, cohort_2018_min_xr_us, cohort_2018_min_ct, cohort_2018_max_xr, cohort_2018_max_xr_us, cohort_2018_max_ct,
      cohort_2019_min_xr, cohort_2019_min_xr_us, cohort_2019_min_ct, cohort_2019_max_xr, cohort_2019_max_xr_us, cohort_2019_max_ct,
      cohort_2020_min_xr, cohort_2020_min_xr_us, cohort_2020_min_ct, cohort_2020_max_xr, cohort_2020_max_xr_us, cohort_2020_max_ct
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
auc_0.55 <- aggregate_radiation_cohorts(auc_target = 1)

# AUC 0.6
auc_0.6 <- aggregate_radiation_cohorts(auc_target = 2)

# AUC 0.65
auc_0.65 <- aggregate_radiation_cohorts(auc_target = 3)

# AUC 0.7
auc_0.7 <- aggregate_radiation_cohorts(auc_target = 4)

# AUC 0.75
auc_0.75 <- aggregate_radiation_cohorts(auc_target = 5)

# AUC 0.8
auc_0.8 <- aggregate_radiation_cohorts(auc_target = 6)

# AUC 0.85
auc_0.85 <- aggregate_radiation_cohorts(auc_target = 7)

# AUC 0.9
auc_0.9 <- aggregate_radiation_cohorts(auc_target = 8)

# AUC 0.95
auc_0.95 <- aggregate_radiation_cohorts(auc_target = 9)

##### 6.1.2.2 Compare length of FU + type of imaging in terms of radiation dose ####
# Combine datasets and calculate cumulative dose
combine_auc_data <- function(data, auc_label) {
  data %>%
    mutate(
      auc_label = auc_label,
      risk_status = ifelse(risk_status == "LR", "Low Risk", "High Risk"),
      cumulative_rad_dose = rowSums(across(starts_with("rad_dose_year_")), na.rm = TRUE)
    ) %>%
    select(auc_label, cohort_type, stone_free_status, risk_status, cumulative_rad_dose, true_rec_5yr)
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
) %>% filter(cohort_type %in% c("Maximum FU, XR", "Maximum FU, XR + US", "Maximum FU, CT"))

data_for_plot$auc_label <- as.factor(data_for_plot$auc_label)
data_for_plot$cohort_type <- as.factor(data_for_plot$cohort_type)
data_for_plot$risk_status <- as.factor(data_for_plot$risk_status)
data_for_plot$true_rec_5yr <- as.factor(data_for_plot$true_rec_5yr)

# Summarise mean and SD by auc_label, risk_status, cohort_type
summary_df <- data_for_plot %>%
  group_by(auc_label, risk_status, cohort_type) %>%
  summarise(
    mean_dose = mean(cumulative_rad_dose, na.rm = TRUE),
    sd = sd(cumulative_rad_dose, na.rm = TRUE),
    .groups = "drop"
  )

# Summarise mean and SD for combined All risk groups
summary_all <- data_for_plot %>%
  group_by(auc_label, cohort_type) %>%
  summarise(
    mean_dose = mean(cumulative_rad_dose, na.rm = TRUE),
    sd = sd(cumulative_rad_dose, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(risk_status = "All") %>%
  select(auc_label, risk_status, cohort_type, mean_dose, sd)

# Combine all summaries
summary_df <- bind_rows(summary_df, summary_all)

# Factor levels for ordering
summary_df <- summary_df %>%
  mutate(
    cohort_type = factor(cohort_type, levels = c("Maximum FU, XR", "Maximum FU, XR + US", "Maximum FU, CT")),
    risk_status = factor(risk_status, levels = c("All", "Low Risk", "High Risk"))
  )

# Prepare data_for_plot factors similarly
data_for_plot <- data_for_plot %>%
  mutate(
    cohort_type = factor(cohort_type, levels = c("Maximum FU, XR", "Maximum FU, XR + US", "Maximum FU, CT")),
    risk_status = factor(risk_status, levels = c("Low Risk", "High Risk"))
  )


# Plot cumulative dose by cohort_type within risk groups
summary_df %>% group_by(cohort_type) %>% ggplot(aes(x = cohort_type, y = mean_dose, fill = risk_status)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = mean_dose - sd, ymax = mean_dose + sd),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  facet_wrap(~ auc_label) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 10)
  ) +
  labs(
    title = "Cumulative 5 Year Radiation Dose for Maximum EAU Follow-Up",
    x = "Cohort Type",
    y = "Mean Cumulative Radiation Dose (mSv)",
    fill = "Risk Status"
  )

# Create gt summary table
summary_df$mean_dose <- round(summary_df$mean_dose, digits = 1)
summary_df$sd <- round(summary_df$sd, digits = 1)

summary_df %>%
  group_by(auc_label) %>%
  pivot_wider(
    names_from = cohort_type,
    values_from = c(mean_dose, sd)
  ) %>%
  rename(
    `Risk Status` = risk_status,
    mean_dose_ct = "mean_dose_Maximum FU, CT",
    mean_dose_xr_us = "mean_dose_Maximum FU, XR + US",
    mean_dose_xr = "mean_dose_Maximum FU, XR",
    sd_ct = "sd_Maximum FU, CT",
    sd_xr_us = "sd_Maximum FU, XR + US",
    sd_xr = "sd_Maximum FU, XR"
  ) %>%
  gt() %>% cols_merge(
    columns = c(mean_dose_ct, sd_ct),
    pattern = "{1}±{2}"
  ) %>% cols_merge(
    columns = c(mean_dose_xr, sd_xr),
    pattern = "{1}±{2}"
  ) %>% cols_merge(
    columns = c(mean_dose_xr_us, sd_xr_us),
    pattern = "{1}±{2}"
  ) %>% cols_label(
    mean_dose_ct = "Mean Dose CT Follow-up (mSv) ± SD",
    mean_dose_xr = "Mean Dose XR Follow-up (mSv) ± SD",
    mean_dose_xr_us = "Mean Dose XR/US Follow-up (mSv) ± SD"
  ) %>% tab_header(
    title = "Radiation doses over 5 years for XR, XR/US or CT follow-up",
    subtitle = "Estimated for Patients with Clinically Significant Disease ascertained from HES data"
  ) %>% tab_footnote(
    footnote = "AUC's refer to modelled diagnostic accuracy of risk stratification",
    locations = cells_column_labels(columns = "Risk Status")
  ) %>% gt_theme_nytimes()

##### 6.1.2.3 Examine minimum FU ####
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
) %>% filter(cohort_type %in% c("Minimum FU, XR", "Minimum FU, XR + US", "Minimum FU, CT"))

data_for_plot$auc_label <- as.factor(data_for_plot$auc_label)
data_for_plot$cohort_type <- as.factor(data_for_plot$cohort_type)
data_for_plot$risk_status <- as.factor(data_for_plot$risk_status)

# Summarise mean and SD by auc_label, risk_status, cohort_type
summary_df <- data_for_plot %>%
  group_by(auc_label, risk_status, cohort_type,) %>%
  summarise(
    mean_dose = mean(cumulative_rad_dose, na.rm = TRUE),
    sd = sd(cumulative_rad_dose, na.rm = TRUE),
    .groups = "drop"
  )

# Summarise mean and SD for combined risk groups ("All")
summary_all <- data_for_plot %>%
  group_by(auc_label, cohort_type) %>%
  summarise(
    mean_dose = mean(cumulative_rad_dose, na.rm = TRUE),
    sd = sd(cumulative_rad_dose, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(risk_status = "All") %>%
  select(auc_label, risk_status, cohort_type, mean_dose, sd)

# Combine all summaries
summary_df <- bind_rows(summary_df, summary_all)

# Factor levels for ordering
summary_df <- summary_df %>%
  mutate(
    cohort_type = factor(cohort_type, levels = c("Minimum FU, XR", "Minimum FU, XR + US", "Minimum FU, CT")),
    risk_status = factor(risk_status, levels = c("All", "Low Risk", "High Risk"))
  )

# Prepare data_for_plot factors similarly
data_for_plot <- data_for_plot %>%
  mutate(
    cohort_type = factor(cohort_type, levels = c("Minimum FU, XR", "Minimum FU, CT")),
    risk_status = factor(risk_status, levels = c("Low Risk", "High Risk"))
  )



# All auc values
auc_values <- unique(data_for_plot$auc_label)



# Assign y.position staggered by risk_status within each auc_label
max_dose <- max(data_for_plot$cumulative_rad_dose, na.rm = TRUE)



# Plot cumulative dose by cohort_type within risk groups
summary_df %>% group_by(cohort_type) %>% ggplot(aes(x = cohort_type, y = mean_dose, fill = risk_status)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = mean_dose - sd, ymax = mean_dose + sd),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  facet_wrap(~ auc_label) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 10)
  ) +
  labs(
    title = "Cumulative 5 Year Radiation Dose for Minimum EAU FU",
    x = "Cohort Type",
    y = "Mean Cumulative Radiation Dose (mSv)",
    fill = "Risk Status"
  )

# Create gt summary table
summary_df$mean_dose <- round(summary_df$mean_dose, digits = 1)
summary_df$sd <- round(summary_df$sd, digits = 1)

summary_df %>%
  group_by(auc_label) %>%
  pivot_wider(
    names_from = cohort_type,
    values_from = c(mean_dose, sd)
  ) %>%
  rename(
    `Risk Status` = risk_status,
    mean_dose_ct = "mean_dose_Minimum FU, CT",
    mean_dose_xr = "mean_dose_Minimum FU, XR",
    mean_dose_xr_us = "mean_dose_Minimum FU, XR + US",
    sd_ct = "sd_Minimum FU, CT",
    sd_xr = "sd_Minimum FU, XR",
    sd_xr_us = "sd_Minimum FU, XR + US",
  ) %>%
  gt() %>% cols_merge(
    columns = c(mean_dose_ct, sd_ct),
    pattern = "{1}±{2}"
  ) %>% cols_merge(
    columns = c(mean_dose_xr, sd_xr),
    pattern = "{1}±{2}"
  ) %>% cols_merge(
    columns = c(mean_dose_xr_us, sd_xr_us),
    pattern = "{1}±{2}"
  ) %>% cols_label(
    mean_dose_ct = "Mean Dose CT Follow-up (mSv) ± SD",
    mean_dose_xr = "Mean Dose XR Follow-up (mSv) ± SD",
    mean_dose_xr_us = "Mean Dose XR + US Follow-up (mSv) ± SD"
  ) %>% tab_header(
    title = "Radiation doses over 5 years for XR or CT follow-up",
    subtitle = "Minimum Follow-up as per EAU",
  ) %>% tab_footnote(
    footnote = "AUC's refer to modelled diagnostic accuracy of risk stratification",
    locations = cells_column_labels(columns = "Risk Status")
    ) %>% tab_footnote(
      footnote = "Estimated for Patients with Clinically Significant Disease ascertained from HES data",
      locations = cells_column_labels(columns = "Risk Status")
  ) %>% gt_theme_nytimes()

#### 6.1.3 Compare Radiation dose depending on Risk Stratification AUC ####
##### 6.1.3.1 Examine All patients ####
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

summary_df2 <- data_for_plot %>%
  group_by(auc_label, cohort_type) %>%
  summarise(
    mean_dose = mean(cumulative_rad_dose, na.rm = TRUE),
    sd = sd(cumulative_rad_dose, na.rm = TRUE),
    median = median(cumulative_rad_dose, na.rm = TRUE),
    lower_quartile = (quantile(cumulative_rad_dose, probs = 0.25) %>% tidy())$x,
    upper_quartile = (quantile(cumulative_rad_dose, probs = 0.75) %>% tidy())$x,
    .groups = "drop"
  )

summary_df2 %>% ggplot(aes(x = auc_label, y=mean_dose)) + 
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = mean_dose - sd, ymax = mean_dose + sd),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  facet_wrap(~ cohort_type) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 10)
  ) +
  labs(
    title = "Cumulative 5 Year Radiation Dose Regardless of Risk Stratification",
    x = "Modelled Risk Stratification AUC-ROC",
    y = "Mean Cumulative Radiation Dose (mSv)"
  ) 

# No difference between AUC values within each cohort_type 
# therefore the next question is what is the difference between cohort types
# Use AUC 0.55 as UKB surrogate
# Use AUC 0.6 as ROKS surrogate

data_for_plot$cohort_type <- factor(data_for_plot$cohort_type,
                                  levels= c(
                                    "Minimum FU, XR",
                                    "Maximum FU, XR",
                                    "Minimum FU, XR + US",
                                    "Maximum FU, XR + US",
                                    "Minimum FU, CT",
                                    "Maximum FU, CT"
                                  ))


data_for_plot %>% ggplot(aes(x = cohort_type, y=cumulative_rad_dose)) + geom_boxplot(
  fill = "slateblue", alpha=0.2
) +
  labs(
    title = "Cumulative 5 Year Radiation Dose Regardless of Risk Stratification",
    x = "Follow-up Length and Type of Imaging",
    y = "Median Cumulative Radiation Dose (mSv)"
  ) 

##### 6.1.3.1 Examine patients by Risk stratification ####
summary_df3 <- data_for_plot %>%
  group_by(auc_label, risk_status, cohort_type) %>%
  summarise(
    mean_dose = mean(cumulative_rad_dose, na.rm = TRUE),
    sd = sd(cumulative_rad_dose, na.rm = TRUE),
    median_dose = median(cumulative_rad_dose, na.rm = TRUE),
    .groups = "drop"
  )

summary_df3 %>% ggplot(aes(x = auc_label, y=mean_dose, fill = risk_status)) + 
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = mean_dose - sd, ymax = mean_dose + sd),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  facet_wrap(~ cohort_type) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 10)
  ) +
  labs(
    title = "Cumulative 5 Year Radiation Dose",
    x = "Modelled Risk Stratification AUC-ROC",
    y = "Mean Cumulative Radiation Dose (mSv)",
    fill = "Risk Status"
  ) 

summary_df3 %>% ggplot(aes(x = auc_label, y=median_dose, fill = risk_status)) + 
  geom_col(position = position_dodge(width = 0.8), width = 0.7)  +
  facet_wrap(~ cohort_type) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 10)
  ) +
  labs(
    title = "Cumulative 5 Year Radiation Dose",
    x = "Modelled Risk Stratification AUC-ROC",
    y = "Median Cumulative Radiation Dose (mSv)",
    fill = "Risk Status"
  ) 

data_for_plot %>% ggplot(aes(x = auc_label, y=cumulative_rad_dose, fill = risk_status)) + 
  geom_boxplot(position = position_dodge(width = 0.8)) +
  facet_wrap(~ cohort_type) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 10)
  ) +
  labs(
    title = "Cumulative 5 Year Radiation Dose",
    x = "Modelled Risk Stratification AUC-ROC",
    y = "Cumulative Radiation Dose (mSv)",
    fill = "Risk Status"
  ) 
