## 5. Run Complete Model ####
### 5.1 Simulate Diagnostic Accuracy of Risk Stratification ####
#### 5.1.1 Simulate ROC curves ####
##### 5.1.1.1 2016 ####
scores_2016 <- simulate_and_plot_roc(auc_targets = c(0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9, 0.95), 
                                  noise_sds = c(1), 
                                  year = 2016,
                                  total_or_clinically_significant = "clinically_significant", 
                                  event_rate = global_rec_rate, 
                                  bins = 100, 
                                  seed = 123)

##### 5.1.1.2 2017 ####
scores_2017 <- simulate_and_plot_roc(auc_targets = c(0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9, 0.95), 
                                     noise_sds = c(1), 
                                     year = 2017,
                                     total_or_clinically_significant = "clinically_significant", 
                                     event_rate = global_rec_rate, 
                                     bins = 100, 
                                     seed = 123)

##### 5.1.1.3 2018 ####
scores_2018 <- simulate_and_plot_roc(auc_targets = c(0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9, 0.95), 
                                     noise_sds = c(1), 
                                     year = 2018,
                                     total_or_clinically_significant = "clinically_significant", 
                                     event_rate = global_rec_rate, 
                                     bins = 100, 
                                     seed = 123)

##### 5.1.1.4 2019 ####
scores_2019 <- simulate_and_plot_roc(auc_targets = c(0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9, 0.95), 
                                     noise_sds = c(1), 
                                     year = 2019,
                                     total_or_clinically_significant = "clinically_significant", 
                                     event_rate = global_rec_rate, 
                                     bins = 100, 
                                     seed = 123)

##### 5.1.1.5 2020 ####
scores_2020 <- simulate_and_plot_roc(auc_targets = c(0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9, 0.95), 
                                     noise_sds = c(1), 
                                     year = 2020,
                                     total_or_clinically_significant = "clinically_significant", 
                                     event_rate = global_rec_rate, 
                                     bins = 100, 
                                     seed = 123)


#### 5.1.2 Plot ROC curves ####
roc_data <- map_df(scores_2016, function(sim) {
  data.frame(
    specificity = rev(sim$roc_obj$specificities),
    sensitivity = rev(sim$roc_obj$sensitivities),
    auc_target = sim$auc_target,
    noise_sd = sim$noise_sd
  )
}) %>%
  mutate(
    auc_target = factor(auc_target),
    noise_sd = factor(noise_sd)
  )

auc_labels <- map_df(scores_2016, function(sim) {
  tibble(
    auc_target = factor(sim$auc_target),
    noise_sd = factor(sim$noise_sd),
    auc_actual = as.numeric(sim$roc_obj$auc)
  )
}) %>%
  distinct()

label_positions <- auc_labels %>%
  mutate(
    x = 0.6,
    y = 0.2,
    label = paste0("Target AUC = ", round(as.numeric(auc_target), 2),
                   "\nActual AUC = ", round(auc_actual, 3))
  )

# Plot with facets and AUC labels
ggplot(roc_data, aes(x = 1 - specificity, y = sensitivity, color = auc_target)) +
  geom_line(size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  labs(
    title = "Simulated ROC Curves",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)",
    color = "Target AUC"
  ) +
  facet_wrap(~ noise_sd) +
  theme_minimal() +
  theme(legend.position = "bottom")

roc_plot <- ggplot(roc_data, aes(x = 1 - specificity, y = sensitivity, color = auc_target)) +
  geom_line(size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  labs(
    title = "Simulated ROC Curves",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)",
    color = "Target AUC"
  ) +
  facet_wrap(~ noise_sd) +
  theme_minimal() +
  theme(legend.position = "bottom")

#### 5.1.3 Plot Density plots ####
plots <- lapply(1:9, function(i) scores_2016[[i]]$density_plot)
plot_grid(plotlist = plots, ncol = 3)
density_plots <- plot_grid(plotlist = plots, ncol = 3)

### 5.2 Simulate Population #####
#### 5.2.1 2016 #####
complete_2016_pop <- simulate_complete_population_distribution(total_patients = clinically_signif_n_2016,
                                                      total_sf_patients = clinically_signif_sf_n_2016,
                                                      prevalence = 0.5,
                                                      verbose = TRUE)

#### 5.2.2 2017 #####
hes_data_elective_emergency %>% subset(year == "2017") %>% subset(select = c(total_patients, total_sf_patients)) %>% gt()

complete_2017_pop <- simulate_complete_population_distribution(total_patients = clinically_signif_n_2017, 
                                                               total_sf_patients = clinically_signif_sf_n_2017, 
                                                               prevalence = 0.5,
                                                               verbose = TRUE)

#### 5.2.3 2018 #####
hes_data_elective_emergency %>% subset(year == "2018") %>% subset(select = c(total_patients, total_sf_patients)) %>% gt()

complete_2018_pop <- simulate_complete_population_distribution(total_patients = clinically_signif_n_2018, 
                                                               total_sf_patients = clinically_signif_sf_n_2018, 
                                                               prevalence = 0.5,
                                                               verbose = TRUE)


#### 5.2.4 2019 #####
hes_data_elective_emergency %>% subset(year == "2019") %>% subset(select = c(total_patients, total_sf_patients)) %>% gt()

complete_2019_pop <- simulate_complete_population_distribution(total_patients = clinically_signif_n_2019, 
                                                               total_sf_patients = clinically_signif_sf_n_2019, 
                                                               prevalence = 0.5,
                                                               verbose = TRUE)

#### 5.2.5 2020 #####
hes_data_elective_emergency %>% subset(year == "2020") %>% subset(select = c(total_patients, total_sf_patients)) %>% gt()

complete_2020_pop <- simulate_complete_population_distribution(total_patients = clinically_signif_n_2020,
                                                               total_sf_patients = clinically_signif_sf_n_2020, 
                                                               prevalence = 0.5,
                                                               verbose = TRUE)


### 5.3 Simulate Follow-up ####
#### 5.3.1 2016 ####
complete_pop_2016_fu <- complete_fu_simulation_over_time(original_data = complete_2016_pop,
                                                         score_data_auc = scores_2016,
                                                         target_auc = c(0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9, 0.95),
                                                         years = 5,
                                                         death_rates_ons,
                                                         base_year = 6)

complete_pop_2016_fu <- colic_intervention_rates_function(complete_pop_2016_fu,
                                                          c(0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9, 0.95),
                                                          year = 2016) 


#### 5.3.2 2017 ####
complete_pop_2017_fu <- complete_fu_simulation_over_time(original_data = complete_2017_pop,
                                                         score_data_auc = scores_2017,
                                                         target_auc = c(0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9, 0.95),
                                                         years = 5,
                                                         death_rates_ons,
                                                         base_year = 7)

complete_pop_2017_fu <- colic_intervention_rates_function(complete_pop_2017_fu,
                                                          c(0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9, 0.95),
                                                          year = 2017) 

#### 5.3.3 2018 ####
complete_pop_2018_fu <- complete_fu_simulation_over_time(original_data = complete_2018_pop,
                                                         score_data_auc = scores_2018,
                                                         target_auc = c(0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9, 0.95),
                                                         years = 5,
                                                         death_rates_ons,
                                                         base_year = 8)

complete_pop_2018_fu <- colic_intervention_rates_function(complete_pop_2018_fu,
                                                          c(0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9, 0.95),
                                                          year = 2018) 

#### 5.3.4 2019 ####
complete_pop_2019_fu <- complete_fu_simulation_over_time(original_data = complete_2019_pop,
                                                         score_data_auc = scores_2019,
                                                         target_auc = c(0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9, 0.95),
                                                         years = 5,
                                                         death_rates_ons,
                                                         base_year = 9)

complete_pop_2019_fu <- colic_intervention_rates_function(complete_pop_2019_fu,
                                                          c(0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9, 0.95),
                                                          year = 2019) 

#### 5.3.5 2020 ####
complete_pop_2020_fu <- complete_fu_simulation_over_time(original_data = complete_2020_pop,
                                                         score_data_auc = scores_2020,
                                                         target_auc = c(0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9, 0.95),
                                                         years = 5,
                                                         death_rates_ons,
                                                         base_year = 10)

complete_pop_2020_fu <- colic_intervention_rates_function(complete_pop_2020_fu,
                                                          c(0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9, 0.95),
                                                          year = 2020) 

## 5.4 Plot outputs ####
### 5.4.1 Build function to get recurrence/survivors percentages ####
process_auc_group <- function(df) {
  cp <- cutpointr(df, x = score, class = recurrence_year_5, 
                  method = maximize_metric, metric = sum_sens_spec)
  cutpoint <- cp$optimal_cutpoint
  
  df <- df %>%
    mutate(risk_status = if_else(score >= cutpoint, "HR", "LR"))
  
  # Pivot to long format for recurrence and death
  long_df <- df %>%
    pivot_longer(
      cols = starts_with("recurrence_year_"),
      names_to = "year",
      names_pattern = "recurrence_year_(\\d)",
      values_to = "recurrence"
    ) %>%
    left_join(
      df %>%
        pivot_longer(
          cols = starts_with("death_year_"),
          names_to = "year_death",
          names_pattern = "death_year_(\\d)",
          values_to = "death"
        ),
      by = c("risk_status", "year" = "year_death", "sex", "age", "score", "stone_free_status", "prediction", "true_rec_5yr", "age_band", "auc_target")
    ) %>%
    mutate(
      year = as.integer(year),
      survivor = if_else(death == "No", "Yes", "No")
    )
  
  # Summarize recurrence and survivors by year and risk group
  summary_data <- long_df %>%
    group_by(risk_status, year) %>%
    summarise(
      recurrence = sum(recurrence == "Yes"),
      survivors = sum(survivor == "Yes"),
      total = n(),
      .groups = "drop"
    ) %>%
    group_by(risk_status) %>%
    mutate(
      initial_n = total[year == 0],
      recurrence_pct = recurrence / initial_n * 100,
      survivors_pct = survivors / initial_n * 100,
      auc_target = unique(df$auc_target)
    ) %>%
    pivot_longer(
      cols = c(recurrence_pct, survivors_pct),
      names_to = "status",
      values_to = "percentage"
    ) %>%
    mutate(
      status = recode(status,
                      "recurrence_pct" = "Recurrence",
                      "survivors_pct" = "Survivors")
    )
  
  return(summary_data)
}


### 5.4.2 Run for 2016 data ####
final_summary <- complete_pop_2016_fu %>%
  group_split(auc_target) %>%
  map_dfr(process_auc_group)

#### 5.4.2.1 Plot output ####
ggplot(final_summary, aes(x = year, y = percentage, color = status, shape = risk_status)) +
  geom_point(size = 2.5) +
  geom_line(aes(linetype = risk_status), linewidth = 0.8) +
  facet_wrap(~ auc_target, ncol = 3, labeller = label_both) +
  scale_color_manual(values = c("Recurrence" = "#D55E00", "Survivors" = "#0072B2")) +
  labs(
    title = "Recurrence and Survivors Over Time by AUC and Risk Group",
    subtitle = "Risk defined by cutpoint on predicted score",
    x = "Year of Follow-up",
    y = "% of Initial Population",
    color = "Outcome",
    shape = "Risk Group",
    linetype = "Risk Group"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.background = element_rect(fill = "gray90", color = NA),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "bottom"
  )

### 5.4.3 Build function to get cumulative recurrence/survivors percentages ####
process_auc_group <- function(df) {
  cp <- cutpointr(df, x = score, class = recurrence_year_5, 
                  method = maximize_metric, metric = sum_sens_spec)
  cutpoint <- cp$optimal_cutpoint
  
  df <- df %>%
    mutate(risk_status = if_else(score >= cutpoint, "HR", "LR"))
  
  # Long format for recurrence and death
  long_df <- df %>%
    pivot_longer(
      cols = matches("^recurrence_year_\\d$"),
      names_to = "year",
      names_pattern = "recurrence_year_(\\d)",
      values_to = "recurrence"
    ) %>%
    left_join(
      df %>%
        pivot_longer(
          cols = matches("^death_year_\\d$"),
          names_to = "year_death",
          names_pattern = "death_year_(\\d)",
          values_to = "death"
        ),
      by = c("risk_status", "year" = "year_death", "sex", "age", "score", "stone_free_status", "prediction", "true_rec_5yr", "age_band", "auc_target")
    ) %>%
    mutate(
      year = as.integer(year),
      recurrence = recurrence == "Yes",
      death = death == "Yes"
    ) %>%
    group_by(risk_status, score) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(
      cumulative_recurrence = cumsum(recurrence) > 0,
      survivor = !death
    ) %>%
    ungroup()
  
  # Summarize cumulative recurrence and survivors
  summary_data <- long_df %>%
    group_by(risk_status, year) %>%
    summarise(
      cumulative_recurrence = sum(cumulative_recurrence),
      survivors = sum(survivor),
      total = n(),
      .groups = "drop"
    ) %>%
    group_by(risk_status) %>%
    mutate(
      initial_n = total[year == 0],
      cumrec_pct = cumulative_recurrence / initial_n * 100,
      survivors_pct = survivors / initial_n * 100,
      auc_target = unique(df$auc_target)
    ) %>%
    pivot_longer(
      cols = c(cumrec_pct, survivors_pct),
      names_to = "status",
      values_to = "percentage"
    ) %>%
    mutate(
      status = recode(status,
                      "cumrec_pct" = "Cumulative Recurrence",
                      "survivors_pct" = "Survivors")
    )
  
  return(list(
    summary_data = summary_data,
    cutpoint = cutpoint
  ))
}

#### 5.4.3.1 Run for 2016 ####
grouped_data <- complete_pop_2016_fu %>% group_by(auc_target)
keys <- grouped_data %>% group_keys()
split_data <- grouped_data %>% group_split()
results_list <- map(split_data, process_auc_group)
final_summary_2016 <- map_dfr(results_list, "summary_data")
cutpoints_2016 <- map2_dfr(
  .x = results_list,
  .y = keys[[1]],  # extract vector of auc_target values
  .f = function(result, auc_target_value) {
    tibble(auc_target = auc_target_value, cutpoint = result$cutpoint)
  }
)

ggplot(final_summary_2016, aes(x = year, y = percentage, color = status, shape = risk_status)) +
  geom_point(size = 2.5) +
  geom_line(aes(linetype = risk_status), linewidth = 0.8) +
  facet_wrap(~ auc_target, ncol = 3, labeller = label_both) +
  scale_color_manual(values = c("Cumulative Recurrence" = "#D55E00", "Survivors" = "#0072B2")) +
  labs(
    title = "2016 - Cumulative Recurrence and Survivors Over Time by AUC and Risk Group",
    subtitle = "Risk defined by Score Cutpoint",
    x = "Year of Follow-up",
    y = "% of Initial Population",
    color = "Outcome",
    shape = "Risk Group",
    linetype = "Risk Group"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.background = element_rect(fill = "gray90", color = NA),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "bottom"
  )

#### 5.4.3.2 Run for 2017 ####
grouped_data <- complete_pop_2017_fu %>% group_by(auc_target)
keys <- grouped_data %>% group_keys()
split_data <- grouped_data %>% group_split()
results_list <- map(split_data, process_auc_group)
final_summary_2017 <- map_dfr(results_list, "summary_data")
cutpoints_2017 <- map2_dfr(
  .x = results_list,
  .y = keys[[1]],  
  .f = function(result, auc_target_value) {
    tibble(auc_target = auc_target_value, cutpoint = result$cutpoint)
  }
)

ggplot(final_summary_2017, aes(x = year, y = percentage, color = status, shape = risk_status)) +
  geom_point(size = 2.5) +
  geom_line(aes(linetype = risk_status), linewidth = 0.8) +
  facet_wrap(~ auc_target, ncol = 3, labeller = label_both) +
  scale_color_manual(values = c("Cumulative Recurrence" = "#D55E00", "Survivors" = "#0072B2")) +
  labs(
    title = "2017 - Cumulative Recurrence and Survivors Over Time by AUC and Risk Group",
    subtitle = "Risk defined by Score Cutpoint",
    x = "Year of Follow-up",
    y = "% of Initial Population",
    color = "Outcome",
    shape = "Risk Group",
    linetype = "Risk Group"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.background = element_rect(fill = "gray90", color = NA),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "bottom"
  )

#### 5.4.3.3 Run for 2018 ####
grouped_data <- complete_pop_2018_fu %>% group_by(auc_target)
keys <- grouped_data %>% group_keys()
split_data <- grouped_data %>% group_split()
results_list <- map(split_data, process_auc_group)
final_summary_2018 <- map_dfr(results_list, "summary_data")
cutpoints_2018 <- map2_dfr(
  .x = results_list,
  .y = keys[[1]],  
  .f = function(result, auc_target_value) {
    tibble(auc_target = auc_target_value, cutpoint = result$cutpoint)
  }
)

ggplot(final_summary_2018, aes(x = year, y = percentage, color = status, shape = risk_status)) +
  geom_point(size = 2.5) +
  geom_line(aes(linetype = risk_status), linewidth = 0.8) +
  facet_wrap(~ auc_target, ncol = 3, labeller = label_both) +
  scale_color_manual(values = c("Cumulative Recurrence" = "#D55E00", "Survivors" = "#0072B2")) +
  labs(
    title = "2018 - Cumulative Recurrence and Survivors Over Time by AUC and Risk Group",
    subtitle = "Risk defined by Score Cutpoint",
    x = "Year of Follow-up",
    y = "% of Initial Population",
    color = "Outcome",
    shape = "Risk Group",
    linetype = "Risk Group"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.background = element_rect(fill = "gray90", color = NA),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "bottom"
  )

#### 5.4.3.3 Run for 2019 ####
grouped_data <- complete_pop_2019_fu %>% group_by(auc_target)
keys <- grouped_data %>% group_keys()
split_data <- grouped_data %>% group_split()
results_list <- map(split_data, process_auc_group)
final_summary_2019 <- map_dfr(results_list, "summary_data")
cutpoints_2019 <- map2_dfr(
  .x = results_list,
  .y = keys[[1]],  
  .f = function(result, auc_target_value) {
    tibble(auc_target = auc_target_value, cutpoint = result$cutpoint)
  }
)

ggplot(final_summary_2019, aes(x = year, y = percentage, color = status, shape = risk_status)) +
  geom_point(size = 2.5) +
  geom_line(aes(linetype = risk_status), linewidth = 0.8) +
  facet_wrap(~ auc_target, ncol = 3, labeller = label_both) +
  scale_color_manual(values = c("Cumulative Recurrence" = "#D55E00", "Survivors" = "#0072B2")) +
  labs(
    title = "2019 - Cumulative Recurrence and Survivors Over Time by AUC and Risk Group",
    subtitle = "Risk defined by Score Cutpoint",
    x = "Year of Follow-up",
    y = "% of Initial Population",
    color = "Outcome",
    shape = "Risk Group",
    linetype = "Risk Group"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.background = element_rect(fill = "gray90", color = NA),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "bottom"
  )

#### 5.4.3.3 Run for 2020 ####
grouped_data <- complete_pop_2020_fu %>% group_by(auc_target)
keys <- grouped_data %>% group_keys()
split_data <- grouped_data %>% group_split()
results_list <- map(split_data, process_auc_group)
final_summary_2020 <- map_dfr(results_list, "summary_data")
cutpoints_2020 <- map2_dfr(
  .x = results_list,
  .y = keys[[1]],  
  .f = function(result, auc_target_value) {
    tibble(auc_target = auc_target_value, cutpoint = result$cutpoint)
  }
)

ggplot(final_summary_2020, aes(x = year, y = percentage, color = status, shape = risk_status)) +
  geom_point(size = 2.5) +
  geom_line(aes(linetype = risk_status), linewidth = 0.8) +
  facet_wrap(~ auc_target, ncol = 3, labeller = label_both) +
  scale_color_manual(values = c("Cumulative Recurrence" = "#D55E00", "Survivors" = "#0072B2")) +
  labs(
    title = "2020 - Cumulative Recurrence and Survivors Over Time by AUC and Risk Group",
    subtitle = "Risk defined by Score Cutpoint",
    x = "Year of Follow-up",
    y = "% of Initial Population",
    color = "Outcome",
    shape = "Risk Group",
    linetype = "Risk Group"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.background = element_rect(fill = "gray90", color = NA),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "bottom"
  )


#### 5.4.3.4 For 2020 - Check SF status + Recurrecen rates ####
# Process data with combined stone_free_status and risk_group
grouped_data <- complete_pop_2020_fu %>% 
  group_by(auc_target, stone_free_status)

keys <- grouped_data %>% group_keys()
split_data <- grouped_data %>% group_split()

# Process each group
results_list <- map(split_data, process_auc_group)

# Combine results with stone_free_status information
final_summary_2020_combined <- map2_dfr(
  .x = results_list,
  .y = 1:length(results_list),
  .f = function(result, index) {
    result$summary_data %>%
      mutate(
        auc_target = keys$auc_target[index],
        stone_free_status = keys$stone_free_status[index],
        combined_risk_group = paste(stone_free_status, risk_status, sep = "_")
      )
  }
)

# Create the modified plot focusing on risk groups
ggplot(final_summary_2020_combined, 
       aes(x = year, y = percentage, 
           color = status, 
           shape = combined_risk_group,
           linetype = combined_risk_group)) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ auc_target, ncol = 3, labeller = label_both) +
  scale_color_manual(values = c("Cumulative Recurrence" = "#D55E00", "Survivors" = "#0072B2")) +
  scale_shape_manual(values = c(16, 17, 15, 18, 8, 11)) + 
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")) +
  labs(
    title = "2020 - Cumulative Recurrence and Survivors by Stone-Free Status and Risk Group",
    subtitle = "Combined Risk Groups: Stone-Free Status + Risk Classification",
    x = "Year of Follow-up",
    y = "% of Initial Population",
    color = "Outcome",
    shape = "Stone-Free Status + Risk Group",
    linetype = "Stone-Free Status + Risk Group"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.background = element_rect(fill = "gray90", color = NA),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "bottom",
    legend.box = "horizontal"
  )

# separate facets for stone_free_status
ggplot(final_summary_2020_combined, 
       aes(x = year, y = percentage, 
           color = status, 
           shape = risk_status,
           linetype = risk_status)) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 0.8) +
  facet_grid(stone_free_status ~ auc_target, 
             labeller = labeller(
               stone_free_status = label_both,
               auc_target = label_both
             )) +
  scale_color_manual(values = c("Cumulative Recurrence" = "#D55E00", "Survivors" = "#0072B2")) +
  labs(
    title = "2020 - Cumulative Recurrence and Survivors by Stone-Free Status and Risk Group",
    subtitle = "Stratified by Stone-Free Status and AUC Target",
    x = "Year of Follow-up",
    y = "% of Initial Population",
    color = "Outcome",
    shape = "Risk Group",
    linetype = "Risk Group"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.background = element_rect(fill = "gray90", color = NA),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "bottom"
  )

simulation_over_time_plot <- ggplot(final_summary_2020_combined, 
       aes(x = year, y = percentage, 
           color = status, 
           shape = risk_status,
           linetype = risk_status)) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 0.8) +
  facet_grid(stone_free_status ~ auc_target, 
             labeller = labeller(
               stone_free_status = "SF Status: ",
               auc_target = "AUC: "
             )) +
  scale_color_manual(values = c("Cumulative Recurrence" = "#D55E00", "Survivors" = "#0072B2")) +
  labs(
    title = "Cumulative Recurrence and Survivors by Stone-Free Status and Risk Group",
    x = "Year of Follow-up",
    y = "% of Initial Population",
    color = "Outcome",
    shape = "Risk Group",
    linetype = "Risk Group"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.background = element_rect(fill = "gray90", color = NA),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "bottom"
  )
