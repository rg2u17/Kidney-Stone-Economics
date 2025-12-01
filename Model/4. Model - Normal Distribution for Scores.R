## 4. Complete Model ####
### 4.1 Display Estimated 5 year event rate ####
global_rec_rate

### 4.2 Test Score simulation + ROC #### 
simulate_and_plot_roc <- function(auc_targets,
                                  noise_sds,
                                  event_rate = global_rec_rate,
                                  bins = 100,
                                  year,
                                  total_or_clinically_significant = c("clinically_significant", "total"),
                                  seed = 123,
                                  max_iterations = 100,
                                  tolerance = 0.001,
                                  monte_carlo = TRUE,
                                  mc_iterations = 1000,
                                  return_individual_scores = TRUE) {
  set.seed(seed)
  
  # Helper function to calculate AUC given normal distribution parameters
  calculate_auc_from_normal <- function(mean_event, sd_event, mean_nonevent, sd_nonevent,
                                        n_events, n_nonevents, temp_seed = NULL) {
    if (!is.null(temp_seed)) set.seed(temp_seed)
    scores_event_temp <- rnorm(n_events, mean = mean_event, sd = sd_event)
    scores_nonevent_temp <- rnorm(n_nonevents, mean = mean_nonevent, sd = sd_nonevent)
    
    # Clip scores to [0, 1] range
    scores_event_temp <- pmax(0, pmin(1, scores_event_temp))
    scores_nonevent_temp <- pmax(0, pmin(1, scores_nonevent_temp))
    
    labels_temp <- c(rep(1, n_events), rep(0, n_nonevents))
    scores_temp <- c(scores_event_temp, scores_nonevent_temp)
    wilcox_result <- suppressWarnings(wilcox.test(scores_event_temp, scores_nonevent_temp))
    auc_temp <- wilcox_result$statistic / (n_events * n_nonevents)
    return(as.numeric(auc_temp))
  }
  
  # Optimization function to find best normal distribution parameters
  optimize_normal_params <- function(target_auc, base_sd, n_events, n_nonevents) {
    
    # Objective function to minimize
    objective <- function(params) {
      mean_event <- params[1]
      sd_event <- params[2]
      mean_nonevent <- params[3]
      sd_nonevent <- params[4]
      
      # Ensure parameters are valid
      if (any(params[c(2, 4)] <= 0.01)) return(1000)  # SDs must be positive
      if (mean_event < 0 || mean_event > 1) return(1000)
      if (mean_nonevent < 0 || mean_nonevent > 1) return(1000)
      if (mean_event <= mean_nonevent) return(1000)  # Event mean should be higher
      
      # Calculate AUC with these parameters
      auc_calc <- calculate_auc_from_normal(mean_event, sd_event, mean_nonevent, sd_nonevent,
                                            n_events, n_nonevents, temp_seed = seed + 999)
      
      # Return squared difference from target
      return((auc_calc - target_auc)^2)
    }
    
    # Initial parameter guess based on target AUC
    # Separation between means increases with target AUC
    separation_factor <- 2 * (target_auc - 0.5)
    
    initial_params <- c(
      mean_event = 0.5 + separation_factor * 0.25,      # Higher mean for events
      sd_event = base_sd,
      mean_nonevent = 0.5 - separation_factor * 0.25,   # Lower mean for non-events
      sd_nonevent = base_sd
    )
    
    # Optimize using optim
    result <- optim(par = initial_params,
                    fn = objective,
                    method = "L-BFGS-B",
                    lower = c(0, 0.01, 0, 0.01),
                    upper = c(1, 0.5, 1, 0.5),
                    control = list(maxit = max_iterations))
    
    return(list(
      mean_event = result$par[1],
      sd_event = result$par[2],
      mean_nonevent = result$par[3],
      sd_nonevent = result$par[4],
      final_error = sqrt(result$value),
      convergence = result$convergence
    ))
  }
  
  # Monte Carlo simulation function
  run_monte_carlo <- function(mean_event, sd_event, mean_nonevent, sd_nonevent,
                              n_events_sf, n_events_less4, n_events_more4,
                              non_events_sf, non_events_less4, non_events_more4,
                              mc_iterations, base_seed) {
    
    mc_results <- list()
    individual_scores_all <- list()
    
    total_individuals <- n_events_sf + n_events_less4 + n_events_more4 +
      non_events_sf + non_events_less4 + non_events_more4
    
    # Create individual IDs and fixed characteristics
    individual_data <- tibble(
      individual_id = 1:total_individuals,
      stone_free_status = c(rep("SF", n_events_sf + non_events_sf),
                            rep("less4", n_events_less4 + non_events_less4),
                            rep("more4", n_events_more4 + non_events_more4)),
      true_outcome = c(rep(1, n_events_sf), rep(0, non_events_sf),
                       rep(1, n_events_less4), rep(0, non_events_less4),
                       rep(1, n_events_more4), rep(0, non_events_more4)),
      group = c(rep("event", n_events_sf + n_events_less4 + n_events_more4),
                rep("nonevent", non_events_sf + non_events_less4 + non_events_more4))
    )
    
    # Store scores for each individual across all MC iterations
    if (return_individual_scores) {
      individual_scores_matrix <- matrix(nrow = total_individuals, ncol = mc_iterations)
    }
    
    cat("Running Monte Carlo simulation with", mc_iterations, "iterations...\n")
    pb <- txtProgressBar(min = 0, max = mc_iterations, style = 3)
    
    for (iter in 1:mc_iterations) {
      set.seed(base_seed + iter)
      
      # Generate scores for this iteration from normal distributions
      n_events_total <- n_events_sf + n_events_less4 + n_events_more4
      n_nonevents_total <- non_events_sf + non_events_less4 + non_events_more4
      
      scores_event <- rnorm(n_events_total, mean = mean_event, sd = sd_event)
      scores_nonevent <- rnorm(n_nonevents_total, mean = mean_nonevent, sd = sd_nonevent)
      
      # Clip to [0, 1] range
      scores_event <- pmax(0, pmin(1, scores_event))
      scores_nonevent <- pmax(0, pmin(1, scores_nonevent))
      
      all_scores <- c(scores_event, scores_nonevent)
      all_labels <- c(rep(1, n_events_total), rep(0, n_nonevents_total))
      
      # Store individual scores
      if (return_individual_scores) {
        individual_scores_matrix[, iter] <- all_scores
      }
      
      # Calculate AUC for this iteration
      roc_obj_iter <- pROC::roc(response = all_labels, predictor = all_scores, quiet = TRUE)
      auc_iter <- as.numeric(roc_obj_iter$auc)
      
      # Get optimal threshold and calculate metrics
      best_thresh_iter <- coords(roc_obj_iter, "best", ret = "threshold", best.method = "youden")$threshold[1]
      predictions_iter <- ifelse(all_scores > best_thresh_iter, 1, 0)
      
      # Calculate performance metrics
      conf_matrix_iter <- confusionMatrix(as.factor(predictions_iter), as.factor(all_labels), positive = "1")
      
      mc_results[[iter]] <- list(
        iteration = iter,
        auc = auc_iter,
        threshold = best_thresh_iter,
        sensitivity = conf_matrix_iter$byClass["Sensitivity"],
        specificity = conf_matrix_iter$byClass["Specificity"],
        ppv = conf_matrix_iter$byClass["Pos Pred Value"],
        npv = conf_matrix_iter$byClass["Neg Pred Value"],
        accuracy = conf_matrix_iter$overall["Accuracy"],
        scores_mean_event = mean(scores_event),
        scores_mean_nonevent = mean(scores_nonevent),
        scores_sd_event = sd(scores_event),
        scores_sd_nonevent = sd(scores_nonevent)
      )
      
      setTxtProgressBar(pb, iter)
    }
    close(pb)
    
    # Compile MC results into summary statistics
    mc_summary <- bind_rows(mc_results) %>%
      summarise(
        iterations = n(),
        auc_mean = mean(auc, na.rm = TRUE),
        auc_sd = sd(auc, na.rm = TRUE),
        auc_median = median(auc, na.rm = TRUE),
        auc_q25 = quantile(auc, 0.25, na.rm = TRUE),
        auc_q75 = quantile(auc, 0.75, na.rm = TRUE),
        threshold_mean = mean(threshold, na.rm = TRUE),
        threshold_sd = sd(threshold, na.rm = TRUE),
        sensitivity_mean = mean(sensitivity, na.rm = TRUE),
        sensitivity_sd = sd(sensitivity, na.rm = TRUE),
        specificity_mean = mean(specificity, na.rm = TRUE),
        specificity_sd = sd(specificity, na.rm = TRUE),
        accuracy_mean = mean(accuracy, na.rm = TRUE),
        accuracy_sd = sd(accuracy, na.rm = TRUE),
        scores_event_mean_avg = mean(scores_mean_event, na.rm = TRUE),
        scores_nonevent_mean_avg = mean(scores_mean_nonevent, na.rm = TRUE)
      )
    
    # Individual score statistics
    individual_score_stats <- NULL
    if (return_individual_scores) {
      individual_score_stats <- individual_data %>%
        mutate(
          score_mean = rowMeans(individual_scores_matrix, na.rm = TRUE),
          score_sd = apply(individual_scores_matrix, 1, sd, na.rm = TRUE),
          score_median = apply(individual_scores_matrix, 1, median, na.rm = TRUE),
          score_q25 = apply(individual_scores_matrix, 1, quantile, 0.25, na.rm = TRUE),
          score_q75 = apply(individual_scores_matrix, 1, quantile, 0.75, na.rm = TRUE),
          score_min = apply(individual_scores_matrix, 1, min, na.rm = TRUE),
          score_max = apply(individual_scores_matrix, 1, max, na.rm = TRUE)
        )
    }
    
    return(list(
      mc_summary = mc_summary,
      mc_detailed = bind_rows(mc_results),
      individual_scores = individual_score_stats,
      individual_data = individual_data
    ))
  }
  
  results <- list()
  i <- 1
  
  for (auc_target in auc_targets) {
    for (noise_sd in noise_sds) {
      
      # Calculate n
      year_rates <- hes_data_elective_emergency %>% filter(year == !!year)
      
      if (total_or_clinically_significant == "clinically_significant") {
        n <- year_rates$clinically_significant_n
        total_sf_patients <- round(year_rates$total_sf_clin_signif, digits = 0)
      } else {
        n <- year_rates$total_patients
        total_sf_patients <- round(year_rates$total_sf_patients, digits = 0)
      }
      
      n_events <- round(n * event_rate)
      n_nonevents <- n - n_events
      
      # Simulate stone free status
      sf_patients <- round(total_sf_patients, digits = 0)
      nsf_patients <- n - sf_patients
      more4_patients <- round(nsf_patients * 0.1)
      less4_patients <- nsf_patients - more4_patients
      
      # Calculate event rates
      n_events_sf <- round(sf_patients * sf_5yr_event_rate, digits = 0)
      n_events_less4 <- round(less4_patients * less4_5yr_event_rate, digits = 0)
      n_events_more4 <- round(more4_patients * more4_5yr_event_rate, digits = 0)
      n_events_recalculated <- round((n_events_sf + n_events_less4 + n_events_more4), digits = 0)
      
      # Calculate non event rates
      non_events_sf <- sf_patients - n_events_sf
      non_events_less4 <- less4_patients - n_events_less4
      non_events_more4 <- more4_patients - n_events_more4
      n_nonevents_recalculated <- non_events_sf + non_events_less4 + non_events_more4
      
      # Optimize normal distribution parameters to achieve target AUC
      cat("\n--- Optimizing parameters for Simulation", i, "---\n")
      cat("Target AUC:", auc_target, ", Standard Deviation:", noise_sd, "\n")
      cat("Optimizing normal distribution parameters...\n")
      
      opt_result <- optimize_normal_params(auc_target, noise_sd,
                                           n_events_recalculated, n_nonevents_recalculated)
      
      mean_event <- opt_result$mean_event
      sd_event <- opt_result$sd_event
      mean_nonevent <- opt_result$mean_nonevent
      sd_nonevent <- opt_result$sd_nonevent
      
      cat("Optimization converged:", opt_result$convergence == 0,
          "| Final error:", round(opt_result$final_error, 4), "\n")
      cat("Event: N(", round(mean_event, 3), ", ", round(sd_event, 3), "²)\n", sep = "")
      cat("Non-event: N(", round(mean_nonevent, 3), ", ", round(sd_nonevent, 3), "²)\n", sep = "")
      
      # Run Monte Carlo simulation if requested
      mc_results <- NULL
      if (monte_carlo) {
        cat("Starting Monte Carlo simulation...\n")
        mc_results <- run_monte_carlo(mean_event, sd_event, mean_nonevent, sd_nonevent,
                                      n_events_sf, n_events_less4, n_events_more4,
                                      non_events_sf, non_events_less4, non_events_more4,
                                      mc_iterations, seed)
      }
      
      # Generate single realization for plotting (using original seed)
      set.seed(seed)
      scores_event <- rnorm(n_events_recalculated, mean = mean_event, sd = sd_event)
      scores_nonevent <- rnorm(n_nonevents_recalculated, mean = mean_nonevent, sd = sd_nonevent)
      
      # Clip to [0, 1] range
      scores_event <- pmax(0, pmin(1, scores_event))
      scores_nonevent <- pmax(0, pmin(1, scores_nonevent))
      
      # Combine scores
      scores <- c(scores_event, scores_nonevent)
      stone_free_status <- c(rep("SF", n_events_sf), rep("less4", n_events_less4), rep("more4", n_events_more4),
                             rep("SF", non_events_sf), rep("less4", non_events_less4), rep("more4", non_events_more4))
      labels <- c(rep(1, n_events_sf), rep(1, n_events_less4), rep(1, n_events_more4),
                  rep(0, non_events_sf), rep(0, non_events_less4), rep(0, non_events_more4))
      
      scores_rescaled <- scores
      
      data_for_roc <- tibble(
        labels = factor(labels),
        scores_rescaled = scores_rescaled,
        scores = scores,
        stone_free_status = factor(stone_free_status, levels = c("SF", "less4", "more4"))
      )
      
      # Create ROC object
      roc_obj <- pROC::roc(response = data_for_roc$labels, predictor = data_for_roc$scores_rescaled)
      auc_actual <- round(as.numeric(roc_obj$auc), digits = 3)
      auc_error <- abs(auc_actual - auc_target)
      
      # ROC plot with Monte Carlo info if available
      annotation_text <- paste0("Target AUC = ", round(auc_target, 3),
                                "\nSingle Run AUC = ", round(auc_actual, 3),
                                "\nError = ", round(auc_error, 4))
      
      if (monte_carlo && !is.null(mc_results)) {
        annotation_text <- paste0(annotation_text,
                                  "\n--- Monte Carlo (n=", mc_iterations, ") ---",
                                  "\nMean AUC = ", round(mc_results$mc_summary$auc_mean, 3),
                                  "\nAUC SD = ", round(mc_results$mc_summary$auc_sd, 4),
                                  "\nAUC 95% CI = [", round(mc_results$mc_summary$auc_q25, 3),
                                  ", ", round(mc_results$mc_summary$auc_q75, 3), "]")
      }
      
      p_roc <- ggroc(roc_obj) +
        geom_abline(intercept = 1, slope = 1, color = "gray", linetype = "dashed") +
        ggtitle(paste0("ROC Curve with Monte Carlo Simulation (Normal Distribution)\n(Target AUC = ",
                       round(auc_target, 3), ", SD = ", noise_sd, ")")) +
        theme_minimal() +
        annotate("text", x = 0.6, y = 0.3, label = annotation_text, size = 3, hjust = 0)
      
      # Get best threshold and confusion matrix for single run
      best_thresh <- coords(roc_obj, "best", ret = "threshold", best.method = "youden") %>% slice_head(n=1)
      data_for_roc <- data_for_roc %>% mutate(predictions = ifelse(scores_rescaled > best_thresh$threshold, 1, 0))
      conf_matrix <- confusionMatrix(as.factor(data_for_roc$predictions), as.factor(data_for_roc$labels))
      
      # Print results
      cat("Target AUC:", auc_target, "| Single Run AUC:", auc_actual, "| Error:", round(auc_error, 4), "\n")
      if (monte_carlo && !is.null(mc_results)) {
        cat("Monte Carlo Results (", mc_iterations, " iterations):\n")
        cat("  Mean AUC:", round(mc_results$mc_summary$auc_mean, 4),
            "± SD:", round(mc_results$mc_summary$auc_sd, 4), "\n")
        cat("  AUC Range: [", round(mc_results$mc_summary$auc_q25, 3), ", ",
            round(mc_results$mc_summary$auc_q75, 3), "]\n")
        cat("  Mean Accuracy:", round(mc_results$mc_summary$accuracy_mean, 4),
            "± SD:", round(mc_results$mc_summary$accuracy_sd, 4), "\n")
      }
      
      # Enhanced density plot
      plot_title <- paste0("Target AUC: ", auc_target, " | Single Run AUC: ", auc_actual)
      if (monte_carlo && !is.null(mc_results)) {
        plot_title <- paste0(plot_title, " | MC Mean AUC: ", round(mc_results$mc_summary$auc_mean, 3))
      }
      
      p_density <- ggplot(data_for_roc, aes(x = scores, fill = labels)) +
        geom_density(alpha = 0.5) +
        labs(title = plot_title,
             subtitle = paste0("Event: N(", round(mean_event, 3), ", ", round(sd_event, 3), "²) | ",
                               "Non-event: N(", round(mean_nonevent, 3), ", ", round(sd_nonevent, 3), "²)"),
             x = "Score (0-1)", y = "Density") +
        theme_minimal() +
        xlim(0, 1)
      
      # Histogram of scores by label
      p_hist <- ggplot(data_for_roc, aes(x = scores, fill = labels)) +
        geom_histogram(position = "identity", alpha = 0.5, bins = bins) +
        labs(title = "Histogram of Scores (Normal Distribution)",
             x = "Score (0-1)", y = "Count") +
        theme_minimal() +
        xlim(0, 1)
      
      results[[i]] <- list(
        scores = (data_for_roc %>% cbind(auc_target = auc_target)),
        auc_target = auc_target,
        auc_actual = auc_actual,
        auc_error = auc_error,
        noise_sd = noise_sd,
        mean_event = mean_event,
        sd_event = sd_event,
        mean_nonevent = mean_nonevent,
        sd_nonevent = sd_nonevent,
        optimization_converged = opt_result$convergence == 0,
        monte_carlo = monte_carlo,
        mc_iterations = ifelse(monte_carlo, mc_iterations, 0),
        mc_results = mc_results,
        roc_plot = p_roc,
        density_plot = p_density,
        histogram_plot = p_hist,
        confusion_matrix = conf_matrix,
        roc_obj = roc_obj
      )
      
      i <- i + 1
    }
  }
  
  return(results)
}
### 4.3 Complete Population simulation ####
simulate_complete_population_distribution <- function(total_patients,
                                                      total_sf_patients,
                                                      prevalence = 0.5,
                                                      verbose = TRUE) {
  if (verbose) cat("Starting simulation...\n")
  
  # Calculate group sizes
  sf_patients <- total_sf_patients
  nsf_patients <- total_patients - total_sf_patients
  more4_patients <- round(nsf_patients * 0.1)
  less4_patients <- nsf_patients - more4_patients
  
  # Assuming 50:50 sex split
  counts <- list(
    n_f_sf = round(sf_patients * 0.5),
    n_m_sf = sf_patients - round(sf_patients * 0.5),
    n_f_nsf_less4 = round(less4_patients * 0.5),
    n_m_nsf_less4 = less4_patients - round(less4_patients * 0.5),
    n_f_nsf_more4 = round(more4_patients * 0.5),
    n_m_nsf_more4 = more4_patients - round(more4_patients * 0.5)
  )
  
  if (verbose) cat("Generating age distributions...\n")
  
  generate_age <- function(n, mean, sd, verbose = FALSE) {
    if (n > 0) {
      ages <- pmax(round(rnorm(n, mean = mean, sd = sd)), 1)
      
      # Find under-18 cases
      under18_idx <- which(ages < 18)
      if (length(under18_idx) > 0) {
        # Randomly select 60% of them
        n_to_adjust <- floor(length(under18_idx) * 0.6)
        adjust_idx <- sample(under18_idx, n_to_adjust)
        
        # Redistribute these ages around the mean
        ages[adjust_idx] <- pmax(round(rnorm(n_to_adjust, mean = mean, sd = sd)), 1)
      }
      
      if (verbose) {
        cat(sprintf("  Generating %d ages (mean = %.1f, sd = %.1f)...\n", n, mean, sd))
      }
      
      return(ages)
    } else {
      return(numeric(0))
    }
  }
  
  age_values <- list(
    generate_age(counts$n_f_sf, 39.3, 18.1),
    generate_age(counts$n_m_sf, 38.1, 18),
    generate_age(counts$n_f_nsf_less4, 39.3, 18.1),
    generate_age(counts$n_m_nsf_less4, 38.1, 18),
    generate_age(counts$n_f_nsf_more4, 39.3, 18.1),
    generate_age(counts$n_m_nsf_more4, 38.1, 18)
  )
  
  sex_labels <- c(
    rep("female", counts$n_f_sf),
    rep("male", counts$n_m_sf),
    rep("female", counts$n_f_nsf_less4),
    rep("male", counts$n_m_nsf_less4),
    rep("female", counts$n_f_nsf_more4),
    rep("male", counts$n_m_nsf_more4)
  )
  
  sf_labels <- c(
    rep("SF", counts$n_f_sf + counts$n_m_sf),
    rep("less4", counts$n_f_nsf_less4 + counts$n_m_nsf_less4),
    rep("more4", counts$n_f_nsf_more4 + counts$n_m_nsf_more4)
  )
  
  age_vector <- unlist(age_values)
  
  if (verbose) cat("Constructing final population tibble...\n")
  
  population <- tibble(
    sex = factor(sex_labels),
    age = age_vector,
    stone_free_status = factor(sf_labels, levels = c("SF", "less4", "more4"))
  )
  
  if (verbose) cat("Simulation complete.\n")
  return(population)
}

### 4.4 Complete Model Function with Monte Carlo ####
complete_fu_simulation_over_time <- function(original_data,
                                             score_data_auc,
                                             target_auc = c(0.55, 0.6, 0.65),
                                             years = 5,
                                             death_rates_ons,
                                             base_year,
                                             n_simulations = 100,
                                             return_all_iterations = FALSE) {
  
  results_list <- list()
  
  for (auc in target_auc) {
    message("Simulating for AUC: ", auc)
    
    # Filter score_data_auc for auc_target
    index <- ifelse(auc == 0.55, 1,
                    ifelse(auc == 0.6, 2,
                           ifelse(auc == 0.65, 3,
                                  ifelse(auc == 0.7, 4,
                                         ifelse(auc == 0.75, 5,
                                                ifelse(auc == 0.8, 6,
                                                       ifelse(auc == 0.85, 7,
                                                              ifelse(auc == 0.9, 8,
                                                                     ifelse(auc == 0.95, 9, NA)))))))))
    filtered_scores <- score_data_auc[[index]]$scores %>% as_tibble()
    filtered_scores$stone_free_status <- factor(filtered_scores$stone_free_status, 
                                                levels = c("SF", "less4", "more4"))
    
    # Monte Carlo iterations
    mc_results <- list()
    
    for (iter in 1:n_simulations) {
      set.seed(1234 + iter)  # Different seed for each iteration
      
      if (iter %% 10 == 0) {
        message("  Monte Carlo iteration: ", iter, "/", n_simulations)
      }
      
      # Randomly shuffle and reassign scores within each stone_free_status group
      filtered_scores_shuffled <- filtered_scores %>%
        group_by(stone_free_status) %>%
        slice_sample(prop = 1) %>%  # Shuffle within each group
        ungroup()
      
      # Combine original data with shuffled scores 
      original_data_sorted <- original_data %>%
        arrange(stone_free_status) %>% 
        select(-stone_free_status)
      
      filtered_scores_sorted <- filtered_scores_shuffled %>%
        arrange(stone_free_status)
      
      sim_data <- bind_cols(original_data_sorted, filtered_scores_sorted)
      
      sim_data <- sim_data %>%
        mutate(
          prediction = ifelse(predictions == 1, "Yes", "No"),
          score = scores,
          true_rec_5yr = ifelse(labels == 1, "Yes", "No"),
          .keep = "unused"
        ) %>%
        mutate(
          recurrence_year_0 = "No",
          death_year_0 = "No"
        )
      
      sim_data$true_rec_5yr <- as.factor(sim_data$true_rec_5yr)
      
      # Define rows with "Yes" to recurrence
      total_n <- nrow(sim_data %>% filter(true_rec_5yr == "Yes"))
      
      # Subset those with no recurrence
      sim_data_no_rec <- sim_data %>% 
        filter(true_rec_5yr == "No") %>% 
        mutate(
          recurrence_year_5 = "No",
          recurrence_year_4 = "No",
          recurrence_year_3 = "No",
          recurrence_year_2 = "No",
          recurrence_year_1 = "No"
        )
      
      # 5yr recurrence rate known - assign temporal distribution
      sim_data_rec_4yr <- sim_data %>% 
        filter(true_rec_5yr == "Yes") %>%
        mutate(
          recurrence_year_5 = "Yes",
          recurrence_year_4 = {
            n_yes <- round(total_n * 0.8)
            n <- n()
            yes_indices <- sample(n, n_yes)
            rec <- rep("No", n)
            rec[yes_indices] <- "Yes"
            rec
          },
          recurrence_year_3 = NA
        )
      
      # Continue temporal distribution assignment
      sim_data_no_rec_4yr <- sim_data_rec_4yr %>% 
        filter(recurrence_year_4 == "No") %>% 
        mutate(recurrence_year_3 = "No")
      
      sim_data_rec_3yr <- sim_data_rec_4yr %>% 
        filter(recurrence_year_4 == "Yes") %>% 
        mutate(
          recurrence_year_3 = {
            n_yes <- round(total_n * 0.6)
            n <- n()
            yes_indices <- sample(n, n_yes)
            rec <- rep("No", n)
            rec[yes_indices] <- "Yes"
            rec
          }
        ) 
      
      sim_data_3yr <- sim_data_rec_3yr %>% rbind(sim_data_no_rec_4yr)
      
      sim_data_no_rec_3yr <- sim_data_3yr %>% 
        filter(recurrence_year_3 == "No") %>% 
        mutate(recurrence_year_2 = "No")
      
      sim_data_rec_2yr <- sim_data_3yr %>% 
        filter(recurrence_year_3 == "Yes") %>% 
        mutate(
          recurrence_year_2 = {
            n_yes <- round(total_n * 0.4)
            n <- n()
            yes_indices <- sample(n, n_yes)
            rec <- rep("No", n)
            rec[yes_indices] <- "Yes"
            rec
          }
        ) 
      
      sim_data_2yr <- sim_data_rec_2yr %>% rbind(sim_data_no_rec_3yr)
      
      sim_data_no_rec_2yr <- sim_data_2yr %>% 
        filter(recurrence_year_2 == "No") %>% 
        mutate(recurrence_year_1 = "No")
      
      sim_data_rec_1yr <- sim_data_2yr %>% 
        filter(recurrence_year_2 == "Yes") %>% 
        mutate(
          recurrence_year_1 = {
            n_yes <- round(total_n * 0.2)
            n <- n()
            yes_indices <- sample(n, n_yes)
            rec <- rep("No", n)
            rec[yes_indices] <- "Yes"
            rec
          }
        ) 
      
      sim_data_rec <- sim_data_rec_1yr %>% rbind(sim_data_no_rec_2yr)
      sim_data <- rbind(sim_data_rec, sim_data_no_rec)
      
      # Simulate year by year
      for (year in 1:years) {
        current_year <- 2010 + base_year + year - 1
        
        # Assign age bands
        sim_data <- sim_data %>%
          dplyr::mutate(age_band = dplyr::case_when(
            age < 5 ~ "1-4",
            age >= 5 & age < 10 ~ "5-9",
            age >= 10 & age < 15 ~ "10-14",
            age >= 15 & age < 20 ~ "15-19",
            age >= 20 & age < 25 ~ "20-24",
            age >= 25 & age < 30 ~ "25-29",
            age >= 30 & age < 35 ~ "30-34",
            age >= 35 & age < 40 ~ "35-39",
            age >= 40 & age < 45 ~ "40-44",
            age >= 45 & age < 50 ~ "45-49",
            age >= 50 & age < 55 ~ "50-54",
            age >= 55 & age < 60 ~ "55-59",
            age >= 60 & age < 65 ~ "60-64",
            age >= 65 & age < 70 ~ "65-69",
            age >= 70 & age < 75 ~ "70-74",
            age >= 75 & age < 80 ~ "75-79",
            age >= 80 & age < 85 ~ "80-84",
            age >= 85 & age <= 90 ~ "85-90",
            age > 90 ~ ">90"
          ))
        
        # Death rates
        male_annual_death_rate <- death_rates_ons[, 1:2] %>%
          cbind("Male_death_rate" = death_rates_ons[, base_year]) %>%
          filter(sex == "Male")
        
        female_annual_death_rate <- death_rates_ons[, 1:2] %>%
          cbind("Female_death_rate" = death_rates_ons[, base_year]) %>%
          filter(sex == "Female")
        
        death_rate <- male_annual_death_rate %>%
          left_join(female_annual_death_rate, by = c("Age_range" = "Age_range")) %>%
          mutate(gender = sex.x, .keep = "all") %>%
          subset(select = -c(sex.x, sex.y))
        
        sim_data <- simulate_death_for_year(sim_data, death_rate, year)
        
        # Age the population by 1 year
        sim_data <- sim_data %>%
          mutate(age = age + 1) 
      }
      
      sim_data$auc_target <- auc
      sim_data$mc_iteration <- iter
      
      sim_data <- sim_data %>% 
        mutate(
          adjusted_recurrence_year_0 = "No",
          adjusted_recurrence_year_1 = case_when(
            death_year_1 == "No" & recurrence_year_1 == "Yes" ~ "Yes",
            death_year_1 == "No" & recurrence_year_1 == "No" ~ "No",
            death_year_1 == "Yes" ~ "No", 
            TRUE ~ NA_character_
          ),
          adjusted_recurrence_year_2 = case_when(
            death_year_2 == "No" & recurrence_year_2 == "Yes" ~ "Yes",
            death_year_2 == "No" & recurrence_year_2 == "No" ~ "No",
            death_year_2 == "Yes" ~ "No",  
            TRUE ~ NA_character_
          ),
          adjusted_recurrence_year_3 = case_when(
            death_year_3 == "No" & recurrence_year_3 == "Yes" ~ "Yes",
            death_year_3 == "No" & recurrence_year_3 == "No" ~ "No",
            death_year_3 == "Yes" ~ "No",  
            TRUE ~ NA_character_
          ),
          adjusted_recurrence_year_4 = case_when(
            death_year_4 == "No" & recurrence_year_4 == "Yes" ~ "Yes",
            death_year_4 == "No" & recurrence_year_4 == "No" ~ "No",
            death_year_4 == "Yes" ~ "No",  
            TRUE ~ NA_character_
          ),
          adjusted_recurrence_year_5 = case_when(
            death_year_5 == "No" & recurrence_year_5 == "Yes" ~ "Yes",
            death_year_5 == "No" & recurrence_year_5 == "No" ~ "No",
            death_year_5 == "Yes" ~ "No",  
            TRUE ~ NA_character_
          )
        )
      
      mc_results[[iter]] <- sim_data
    }
    
    # Combine all Monte Carlo iterations for this AUC
    results_list[[as.character(auc)]] <- dplyr::bind_rows(mc_results)
  }
  
  final_simulation <- dplyr::bind_rows(results_list)
  
  # If return_all_iterations = FALSE, summarize across iterations
  if (!return_all_iterations) {
    message("Summarizing Monte Carlo results...")
    final_simulation <- summarize_mc_results(final_simulation)
  }
  
  return(final_simulation)
}

# Helper function to summarize Monte Carlo results
summarize_mc_results <- function(data) {
  summary_stats <- data %>%
    group_by(auc_target, prediction) %>%
    summarise(
      n_iterations = n_distinct(mc_iteration),
      mean_recurrence_year_1 = mean(adjusted_recurrence_year_1 == "Yes", na.rm = TRUE),
      sd_recurrence_year_1 = sd(adjusted_recurrence_year_1 == "Yes", na.rm = TRUE),
      mean_recurrence_year_2 = mean(adjusted_recurrence_year_2 == "Yes", na.rm = TRUE),
      sd_recurrence_year_2 = sd(adjusted_recurrence_year_2 == "Yes", na.rm = TRUE),
      mean_recurrence_year_3 = mean(adjusted_recurrence_year_3 == "Yes", na.rm = TRUE),
      sd_recurrence_year_3 = sd(adjusted_recurrence_year_3 == "Yes", na.rm = TRUE),
      mean_recurrence_year_4 = mean(adjusted_recurrence_year_4 == "Yes", na.rm = TRUE),
      sd_recurrence_year_4 = sd(adjusted_recurrence_year_4 == "Yes", na.rm = TRUE),
      mean_recurrence_year_5 = mean(adjusted_recurrence_year_5 == "Yes", na.rm = TRUE),
      sd_recurrence_year_5 = sd(adjusted_recurrence_year_5 == "Yes", na.rm = TRUE),
      .groups = "drop"
    )
  
  return(summary_stats)
}

# Function to assign death rates to individuals each year
simulate_death_for_year <- function(sim_data, death_rate, year) {
  sim_data <- sim_data %>%
    mutate(death_prev = ifelse(year == 1, "No", .[[paste0("death_year_", year - 1)]])) %>%
    group_by(sex, age_band) %>%
    group_split() %>%
    map_dfr(~{
      group <- .
      n <- nrow(group)
      
      # Get the correct death rate
      rate_row <- death_rate %>%
        filter(Age_range == unique(group$age_band)) %>%
        slice(1)  
      
      rate <- if (tolower(unique(group$sex)) == "male") {
        rate_row$Male_death_rate
      } else {
        rate_row$Female_death_rate
      }
      
      # Determine how many to assign "Yes" if they are still alive
      eligible <- group$death_prev == "No"
      num_eligible <- sum(eligible)
      num_deaths <- round(num_eligible * rate)
      
      # Randomly assign death
      death_status <- rep("No", n)
      if (num_deaths > 0) {
        death_indices <- sample(which(eligible), num_deaths)
        death_status[death_indices] <- "Yes"
      }
      
      group[[paste0("death_year_", year)]] <- ifelse(group$death_prev == "Yes", "Yes", death_status)
      group
    }) %>%
    select(-death_prev) 
  
  return(sim_data)
}

### 4.5 Function to Update Population with Colic + Interventions ####
colic_intervention_rates_function <- function(population,
                                              auc_targets = c(0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95),
                                              year) {
  result <- list()
  
  for (auc_target in auc_targets) {
    message("Colic/Intervention rates for AUC: ", auc_target)
    subpopulation <- population %>% filter(auc_target == !!auc_target)
    
    # Calculate surviving people within each stone_free_status category
    n_sf <- nrow(subpopulation %>% filter(stone_free_status == "SF"))
    n_less4 <- nrow(subpopulation %>% filter(stone_free_status == "less4"))
    n_more4 <- nrow(subpopulation %>% filter(stone_free_status == "more4"))
    
    message("Colic/Intervention rates for SF patients, AUC: ", auc_target)
    # Stone free patients
    subpopulation_sf <- subpopulation %>% filter(stone_free_status == "SF") %>% mutate(
      colic_intervention_year_0 = "No",
      colic_intervention_year_1 = case_when(
        recurrence_year_1 == "Yes" ~ {
          n_colic <- round(n_sf * 0.5)
          n_intervention <- round(n_sf * 0.5)
          n <- n()
          colic_indices <- sample(n, n_colic)
          rec <- rep("Intervention", n)
          rec[colic_indices] <- "Colic"
          rec
        },
        TRUE ~ "No"
      ),
      colic_intervention_year_2 = case_when(
        colic_intervention_year_1 == "Colic" ~ "Colic",
        colic_intervention_year_1 == "Intervention" ~ "Intervention",
        recurrence_year_2 == "Yes" & colic_intervention_year_1 == "No" ~ {
          n_colic <- round(n_sf * 0.5)
          n_intervention <- round(n_sf * 0.5)
          n <- n()
          colic_indices <- sample(n, n_colic)
          rec <- rep("Intervention", n)
          rec[colic_indices] <- "Colic"
          rec
        },
        TRUE ~ "No"
      ),
      colic_intervention_year_3 = case_when(
        colic_intervention_year_2 == "Colic" ~ "Colic",
        colic_intervention_year_2 == "Intervention" ~ "Intervention",
        recurrence_year_3 == "Yes" & colic_intervention_year_2 == "No" ~ {
          n_colic <- round(n_sf * 0.5)
          n_intervention <- round(n_sf * 0.5)
          n <- n()
          colic_indices <- sample(n, n_colic)
          rec <- rep("Intervention", n)
          rec[colic_indices] <- "Colic"
          rec
        },
        TRUE ~ "No"
      ),
      colic_intervention_year_4 = case_when(
        colic_intervention_year_3 == "Colic" ~ "Colic",
        colic_intervention_year_3 == "Intervention" ~ "Intervention",
        recurrence_year_4 == "Yes" & colic_intervention_year_3 == "No" ~ {
          n_colic <- round(n_sf * 0.5)
          n_intervention <- round(n_sf * 0.5)
          n <- n()
          colic_indices <- sample(n, n_colic)
          rec <- rep("Intervention", n)
          rec[colic_indices] <- "Colic"
          rec
        },
        TRUE ~ "No"
      ),
      colic_intervention_year_5 = case_when(
        colic_intervention_year_4 == "Colic" ~ "Colic",
        colic_intervention_year_4 == "Intervention" ~ "Intervention",
        recurrence_year_5 == "Yes" & colic_intervention_year_4 == "No" ~ {
          n_colic <- round(n_sf * 0.5)
          n_intervention <- round(n_sf * 0.5)
          n <- n()
          colic_indices <- sample(n, n_colic)
          rec <- rep("Intervention", n)
          rec[colic_indices] <- "Colic"
          rec
        },
        TRUE ~ "No"
      )
    )
    
    message("Colic/Intervention rates for <4mm Fragments, AUC: ", auc_target)
    
    # <4mm fragments patients
    subpopulation_less4 <- subpopulation %>% filter(stone_free_status == "less4") %>% mutate(
      colic_intervention_year_0 = "No",
      colic_intervention_year_1 = case_when(
        recurrence_year_1 == "Yes" ~ {
          n_colic <- round(n_less4 * (1-0.88))
          n <- n()
          colic_indices <- sample(n, n_colic)
          rec <- rep("Intervention", n)
          rec[colic_indices] <- "Colic"
          rec
        },
        TRUE ~ "No"
      ),
      colic_intervention_year_2 = case_when(
        colic_intervention_year_1 == "Colic" ~ "Colic",
        colic_intervention_year_1 == "Intervention" ~ "Intervention",
        recurrence_year_2 == "Yes" & colic_intervention_year_1 == "No" ~ {
          n_colic <- round(n_less4 * (1-0.88))
          n <- n()
          colic_indices <- sample(n, n_colic)
          rec <- rep("Intervention", n)
          rec[colic_indices] <- "Colic"
          rec
        },
        TRUE ~ "No"
      ),
      colic_intervention_year_3 = case_when(
        colic_intervention_year_2 == "Colic" ~ "Colic",
        colic_intervention_year_2 == "Intervention" ~ "Intervention",
        recurrence_year_3 == "Yes" & colic_intervention_year_2 == "No" ~ {
          n_colic <- round(n_less4 * (1-0.88))
          n <- n()
          colic_indices <- sample(n, n_colic)
          rec <- rep("Intervention", n)
          rec[colic_indices] <- "Colic"
          rec
        },
        TRUE ~ "No"
      ),
      colic_intervention_year_4 = case_when(
        colic_intervention_year_3 == "Colic" ~ "Colic",
        colic_intervention_year_3 == "Intervention" ~ "Intervention",
        recurrence_year_4 == "Yes" & colic_intervention_year_3 == "No" ~ {
          n_colic <- round(n_less4 * (1-0.88))
          n <- n()
          colic_indices <- sample(n, n_colic)
          rec <- rep("Intervention", n)
          rec[colic_indices] <- "Colic"
          rec
        },
        TRUE ~ "No"
      ),
      colic_intervention_year_5 = case_when(
        colic_intervention_year_4 == "Colic" ~ "Colic",
        colic_intervention_year_4 == "Intervention" ~ "Intervention",
        recurrence_year_5 == "Yes" & colic_intervention_year_4 == "No" ~ {
          n_colic <- round(n_less4 * (1-0.88))
          n <- n()
          colic_indices <- sample(n, n_colic)
          rec <- rep("Intervention", n)
          rec[colic_indices] <- "Colic"
          rec
        },
        TRUE ~ "No"
      )
    )
    
    message("Colic/Intervention rates for >=4mm Fragments, AUC: ", auc_target)
    
    # >=4mm fragments patients
    subpopulation_more4 <- subpopulation %>% filter(stone_free_status == "more4") %>% mutate(
      colic_intervention_year_0 = "No",
      colic_intervention_year_1 = case_when(
        recurrence_year_1 == "Yes" ~ {
          n_colic <- round(n_more4 * (1-0.985))
          n <- n()
          colic_indices <- sample(n, n_colic)
          rec <- rep("Intervention", n)
          rec[colic_indices] <- "Colic"
          rec
        },
        TRUE ~ "No"
      ),
      colic_intervention_year_2 = case_when(
        colic_intervention_year_1 == "Colic" ~ "Colic",
        colic_intervention_year_1 == "Intervention" ~ "Intervention",
        recurrence_year_2 == "Yes" & colic_intervention_year_1 == "No" ~ {
          n_colic <- round(n_more4 * (1-0.985))
          n <- n()
          colic_indices <- sample(n, n_colic)
          rec <- rep("Intervention", n)
          rec[colic_indices] <- "Colic"
          rec
        },
        TRUE ~ "No"
      ),
      colic_intervention_year_3 = case_when(
        colic_intervention_year_2 == "Colic" ~ "Colic",
        colic_intervention_year_2 == "Intervention" ~ "Intervention",
        recurrence_year_3 == "Yes" & colic_intervention_year_2 == "No" ~ {
          n_colic <- round(n_more4 * (1-0.985))
          n <- n()
          colic_indices <- sample(n, n_colic)
          rec <- rep("Intervention", n)
          rec[colic_indices] <- "Colic"
          rec
        },
        TRUE ~ "No"
      ),
      colic_intervention_year_4 = case_when(
        colic_intervention_year_3 == "Colic" ~ "Colic",
        colic_intervention_year_3 == "Intervention" ~ "Intervention",
        recurrence_year_4 == "Yes" & colic_intervention_year_3 == "No" ~ {
          n_colic <- round(n_more4 * (1-0.985))
          n <- n()
          colic_indices <- sample(n, n_colic)
          rec <- rep("Intervention", n)
          rec[colic_indices] <- "Colic"
          rec
        },
        TRUE ~ "No"
      ),
      colic_intervention_year_5 = case_when(
        colic_intervention_year_4 == "Colic" ~ "Colic",
        colic_intervention_year_4 == "Intervention" ~ "Intervention",
        recurrence_year_5 == "Yes" & colic_intervention_year_4 == "No" ~ {
          n_colic <- round(n_more4 * (1-0.985))
          n <- n()
          colic_indices <- sample(n, n_colic)
          rec <- rep("Intervention", n)
          rec[colic_indices] <- "Colic"
          rec
        },
        TRUE ~ "No"
      )
    )
    
    subpopulation <- rbind(
      subpopulation_sf,
      subpopulation_less4,
      subpopulation_more4
    ) %>% as_tibble() %>% mutate(
      id = 1:n()
    ) %>% select(id, everything())
    
    message("Intervention types, AUC: ", auc_target)
    
    # Get intervention types
    years <- c(year, year + 1, year + 2, year + 3, year + 4, year + 5)
    yr_5 <- year + 5
    
    intervention_props <- hes_data_elective_emergency %>% 
      filter(year %in% years) %>% 
      subset(select = c(year, eswl_proportion, urs_proportion, pcnl_proportion))
    
    years <- years %>% as_tibble() %>% mutate(year = value, .keep = "unused")
    
    intervention_props <- years %>% 
      left_join(intervention_props, by = c("year" = "year")) %>% 
      mutate(
        eswl_proportion = case_when(is.na(eswl_proportion) ~ 0.432, TRUE ~ eswl_proportion),
        urs_proportion = case_when(is.na(urs_proportion) ~ 0.451, TRUE ~ urs_proportion),
        pcnl_proportion = case_when(is.na(pcnl_proportion) ~ 0.117, TRUE ~ pcnl_proportion),
        .keep = "all"
      )
    
    eswl_proportion <- (intervention_props %>% filter(year == yr_5))$eswl_proportion
    urs_proportion <- (intervention_props %>% filter(year == yr_5))$urs_proportion
    pcnl_proportion <- (intervention_props %>% filter(year == yr_5))$pcnl_proportion
    
    total_n_intervention <- nrow(subpopulation %>% filter(colic_intervention_year_5 == "Intervention"))
    
    n_swl <- round(total_n_intervention * eswl_proportion, digits = 0)
    n_urs <- round(total_n_intervention * urs_proportion, digits = 0)
    n_pcnl <- round(total_n_intervention * pcnl_proportion, digits = 0)
    
    subpopulation_swl_yr_5 <- subpopulation %>% 
      filter(colic_intervention_year_5 == "Intervention") %>% 
      slice_sample(n = n_swl) %>% 
      mutate(intervention_type = "ESWL", .keep = "all") %>% 
      subset(select = c(id, intervention_type))
    
    subpopulation_urs_yr_5 <- subpopulation %>% 
      filter(colic_intervention_year_5 == "Intervention" & !id %in% subpopulation_swl_yr_5$id) %>% 
      slice_sample(n = n_urs) %>% 
      mutate(intervention_type = "URS", .keep = "all") %>% 
      subset(select = c(id, intervention_type))
    
    subpopulation_pcnl_yr_5 <- subpopulation %>% 
      filter(colic_intervention_year_5 == "Intervention" & 
               !id %in% subpopulation_swl_yr_5$id & 
               !id %in% subpopulation_urs_yr_5$id) %>% 
      slice_sample(n = n_pcnl) %>% 
      mutate(intervention_type = "PCNL", .keep = "all") %>% 
      subset(select = c(id, intervention_type))
    
    subpopulation_interventions_yr_5 <- rbind(
      subpopulation_swl_yr_5,
      subpopulation_urs_yr_5,
      subpopulation_pcnl_yr_5
    ) 
    
    subpopulation <- subpopulation %>% 
      left_join(subpopulation_interventions_yr_5, by = c("id" = "id")) 
    
    subpopulation <- subpopulation %>% 
      mutate(
        intervention_type = as.factor(ifelse(is.na(intervention_type), "None", intervention_type))
      )
    
    subpopulation$intervention_type <- factor(
      subpopulation$intervention_type,
      levels = c("ESWL", "URS", "PCNL", "None")
    )
    
    subpopulation <- subpopulation %>% mutate(
      colic_intervention_type_year_0 = case_when(
        colic_intervention_year_0 == "Intervention" & intervention_type == "ESWL" ~ "ESWL",
        colic_intervention_year_0 == "Intervention" & intervention_type == "URS" ~ "URS",
        colic_intervention_year_0 == "Intervention" & intervention_type == "PCNL" ~ "PCNL",
        colic_intervention_year_0 == "Colic" ~ "Colic",
        colic_intervention_year_0 == "No" ~ "No",
        TRUE ~ NA_character_
      ),
      colic_intervention_type_year_1 = case_when(
        colic_intervention_year_1 == "Intervention" & intervention_type == "ESWL" ~ "ESWL",
        colic_intervention_year_1 == "Intervention" & intervention_type == "URS" ~ "URS",
        colic_intervention_year_1 == "Intervention" & intervention_type == "PCNL" ~ "PCNL",
        colic_intervention_year_1 == "Colic" ~ "Colic",
        colic_intervention_year_1 == "No" ~ "No",
        TRUE ~ NA_character_
      ),
      colic_intervention_type_year_2 = case_when(
        colic_intervention_year_2 == "Intervention" & intervention_type == "ESWL" ~ "ESWL",
        colic_intervention_year_2 == "Intervention" & intervention_type == "URS" ~ "URS",
        colic_intervention_year_2 == "Intervention" & intervention_type == "PCNL" ~ "PCNL",
        colic_intervention_year_2 == "Colic" ~ "Colic",
        colic_intervention_year_2 == "No" ~ "No",
        TRUE ~ NA_character_
      ),
      colic_intervention_type_year_3 = case_when(
        colic_intervention_year_3 == "Intervention" & intervention_type == "ESWL" ~ "ESWL",
        colic_intervention_year_3 == "Intervention" & intervention_type == "URS" ~ "URS",
        colic_intervention_year_3 == "Intervention" & intervention_type == "PCNL" ~ "PCNL",
        colic_intervention_year_3 == "Colic" ~ "Colic",
        colic_intervention_year_3 == "No" ~ "No",
        TRUE ~ NA_character_
      ),
      colic_intervention_type_year_4 = case_when(
        colic_intervention_year_4 == "Intervention" & intervention_type == "ESWL" ~ "ESWL",
        colic_intervention_year_4 == "Intervention" & intervention_type == "URS" ~ "URS",
        colic_intervention_year_4 == "Intervention" & intervention_type == "PCNL" ~ "PCNL",
        colic_intervention_year_4 == "Colic" ~ "Colic",
        colic_intervention_year_4 == "No" ~ "No",
        TRUE ~ NA_character_
      ),
      colic_intervention_type_year_5 = case_when(
        colic_intervention_year_5 == "Intervention" & intervention_type == "ESWL" ~ "ESWL",
        colic_intervention_year_5 == "Intervention" & intervention_type == "URS" ~ "URS",
        colic_intervention_year_5 == "Intervention" & intervention_type == "PCNL" ~ "PCNL",
        colic_intervention_year_5 == "Colic" ~ "Colic",
        colic_intervention_year_5 == "No" ~ "No",
        TRUE ~ NA_character_
      )
    )
    
    result[[as.character(auc_target)]] <- subpopulation
  }
  
  # Combine all into one dataframe
  final_result <- bind_rows(result)
  return(final_result)
}
