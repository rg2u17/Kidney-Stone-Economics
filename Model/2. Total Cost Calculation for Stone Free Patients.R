## 2. Total Cost Calculation for SF patients for EAU Risk Stratification ####
### 2.1 Calculate total cost function - XR / US F/U ####
calculate_total_sf_cost_xr_fu <- function(data) {
  # Initial Costs
  data <- data %>% mutate(initial_costs = (total_sf_patients * (
    initial_consultation_cost + 2 * (clinic_review_cost + imaging_cost) + (total_sf_patients * 0.5 * urine_24_hr_cost)
  )),
  .keep = "all")
  
  # Follow-up Costs
  data <- data %>% mutate(
    lr_annual_follow_up_costs = (total_sf_patients * 0.5) * (clinic_review_cost + imaging_cost),
    #low risk patients
    hr_annual_follow_up_costs = (total_sf_patients *
                                   0.5) * (clinic_review_cost + imaging_cost + urine_24_hr_cost),
    # high risk patients
    .keep = "all"
  )
  
  # Additional Intervention for Residual Stones
  data <- data %>% mutate(
    residual_stone_intervention_patients = eswl_residual_symptomatic + urs_residual_symptomatic + pcnl_residual_symptomatic,
    eswl_reintervention_patients = residual_stone_intervention_patients * eswl_proportion,
    urs_reintervention_patients = residual_stone_intervention_patients * urs_proportion,
    pcnl_reintervention_patients = residual_stone_intervention_patients * pcnl_proportion,
    residual_stone_intervention_costs = eswl_reintervention_patients * eswl_cost +
      urs_reintervention_patients * urs_cost +
      pcnl_reintervention_patients * pcnl_cost,
    .keep = "all"
  )
  
  # Total Costs
  total_cost <- data %>% mutate(
    total_cost = initial_costs + (3 * lr_annual_follow_up_costs) + (4 * hr_annual_follow_up_costs) + residual_stone_intervention_costs
  )
  return(total_cost)
}

### 2.2 Total Cost Calculation Function - CT F/U ####
calculate_total_sf_cost_ct_fu <- function(data) {
  # Initial Costs
  data <- data %>% mutate(initial_costs = (total_sf_patients * (
    initial_consultation_cost + 2 * (clinic_review_cost + ct_cost) + (total_sf_patients * 0.5 * urine_24_hr_cost)
  )),
  .keep = "all")
  
  # Follow-up Costs
  data <- data %>% mutate(
    lr_annual_follow_up_costs = (total_sf_patients * 0.5) * (clinic_review_cost + ct_cost),
    #low risk patients
    hr_annual_follow_up_costs = (total_sf_patients *
                                   0.5) * (clinic_review_cost + ct_cost + urine_24_hr_cost),
    # high risk patients
    .keep = "all"
  )
  
  
  # Additional Intervention for Residual Stones
  data <- data %>% mutate(
    residual_stone_intervention_patients = eswl_residual_symptomatic + urs_residual_symptomatic + pcnl_residual_symptomatic,
    eswl_reintervention_patients = residual_stone_intervention_patients * eswl_proportion,
    urs_reintervention_patients = residual_stone_intervention_patients * urs_proportion,
    pcnl_reintervention_patients = residual_stone_intervention_patients * pcnl_proportion,
    residual_stone_intervention_costs = eswl_reintervention_patients * eswl_cost +
      urs_reintervention_patients * urs_cost +
      pcnl_reintervention_patients * pcnl_cost,
    .keep = "all"
  )
  
  # Total Costs
  total_cost <- data %>% mutate(
    total_cost = initial_costs + (3 * lr_annual_follow_up_costs) + (4 * hr_annual_follow_up_costs) + residual_stone_intervention_costs
  )
  return(total_cost)
}

### 2.3 Total Cost Calculation Function - CT for those having had an intervention otherwise XR ####
calculate_total_sf_cost_ct_xr_fu <- function(data) {
  # Initial Costs
  data <- data %>% mutate(initial_costs = (((elective_procedures_n + emergency_procedures_n) * (
    initial_consultation_cost + (clinic_review_cost + ct_cost) + (clinic_review_cost + imaging_cost)
  )
  ) + (
    total_sf_patients - (elective_procedures_n + emergency_procedures_n)
  ) * (initial_consultation_cost + 2 * (clinic_review_cost + ct_cost)) + (total_sf_patients * 0.5 * urine_24_hr_cost)
  ), .keep = "all")
  
  # Follow-up Costs
  data <- data %>% mutate(
    lr_annual_follow_up_costs = (total_sf_patients * 0.5) * (clinic_review_cost + ct_cost),
    #low risk patients
    hr_annual_follow_up_costs = (total_sf_patients *
                                   0.5) * (clinic_review_cost + ct_cost + urine_24_hr_cost),
    # high risk patients
    .keep = "all"
  )
  
  
  # Additional Intervention for Residual Stones
  data <- data %>% mutate(
    residual_stone_intervention_patients = eswl_residual_symptomatic + urs_residual_symptomatic + pcnl_residual_symptomatic,
    eswl_reintervention_patients = residual_stone_intervention_patients * eswl_proportion,
    urs_reintervention_patients = residual_stone_intervention_patients * urs_proportion,
    pcnl_reintervention_patients = residual_stone_intervention_patients * pcnl_proportion,
    residual_stone_intervention_costs = eswl_reintervention_patients * eswl_cost +
      urs_reintervention_patients * urs_cost +
      pcnl_reintervention_patients * pcnl_cost,
    .keep = "all"
  )
  
  # Total Costs
  total_cost <- data %>% mutate(
    total_cost = initial_costs + (3 * lr_annual_follow_up_costs) + (4 * hr_annual_follow_up_costs) + residual_stone_intervention_costs
  )
  return(total_cost)
}

