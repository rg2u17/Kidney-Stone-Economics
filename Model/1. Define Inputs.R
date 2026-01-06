# Economic Model for Kidney Stone Patient Follow-up with Costs - All Patients ####
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
library(broom)

## 1. Define Inputs ####
### 1.1 5 Years F/U as per EAU guidance ####
follow_up_years <- 5

### 1.2 Define Recurrence metrics ####
#### 1.2.1 Recurrence rates as per UKB over 5 years ####
rec_prop <- 0.5

#### 1.2.2 Recurrence (intervention or colic) rates from literature over 5 years ####
sf_5yr_recurrence <- 0.25 # Sorensen et al. 2022 / Tzelves et al. 2023
less4_5yr_recurrence <- 0.425 # Tzelves et al. 2023
more4_5yr_recurrence <- 0.66 # Tzelves et al. 2023  

# Proportions per initial stone free status
sf_prop <- 0.725
less4_prop <- 0.247
more4_prop <- 0.028

# Estimated Recurrence rates as per SF status
# As per Tzelves et al. 2023, Eur Urol Focus - 5 year rates of recurrence:

sf_5yr_event_rate <- 0.25
less4_5yr_event_rate <- 0.425
more4_5yr_event_rate <- 0.66

# Estimate global 5 yr event rate

global_rec_rate <- (sf_prop*sf_5yr_event_rate) + (less4_prop*less4_5yr_event_rate) + (more4_prop*more4_5yr_event_rate)
global_rec_rate <- round(global_rec_rate, digits = 3)
global_rec_rate

#### 1.2.3 Define Risk Status Parameters as per UKB ####
risk_status_proportion_low <- 0.36 # as per data from UKB
risk_status_proportion_high <- 0.64

# Recurrence Rates
recurrence_rate_low <- 0.44 # 44% recurrence for low-risk patients over 5 years
annual_recurrence_rate_low <- recurrence_rate_low / 5
recurrence_rate_high <- 0.53  # 53% recurrence for high-risk patients over 5 years
annual_recurrence_rate_high <- recurrence_rate_high / 5

# Recurrence Rates
annual_recurrence_rate <- 0.5 / 5

### 1.3 Population and Incidence Data ####
ons_population_estimates <- fread("Inputs/population_estimates_ons.csv") %>% as_tibble() %>% janitor::clean_names()
ons_population_estimates <- ons_population_estimates %>% subset(laname23 == "ENGLAND") %>% subset(select = -c(ladcode23, laname23, country))
ons_population_estimates <- ons_population_estimates %>%
  mutate(across(
    .cols = matches("^population_\\d{4}$"),
    .fns = ~ as.numeric(gsub(",", "", .))
  ))

total_population <- ons_population_estimates %>% mutate(
  total_pop_2011 = sum(population_2011),
  total_pop_2012 = sum(population_2012),
  total_pop_2013 = sum(population_2013),
  total_pop_2014 = sum(population_2014),
  total_pop_2015 = sum(population_2015),
  total_pop_2016 = sum(population_2016),
  total_pop_2017 = sum(population_2017),
  total_pop_2018 = sum(population_2018),
  total_pop_2019 = sum(population_2019),
  total_pop_2020 = sum(population_2020),
  total_pop_2021 = sum(population_2021),
  total_pop_2022 = sum(population_2022)
) %>% slice_head(n = 1) %>% subset(
  select = c(
    total_pop_2011,
    total_pop_2012,
    total_pop_2013,
    total_pop_2014,
    total_pop_2015,
    total_pop_2016,
    total_pop_2017,
    total_pop_2018,
    total_pop_2019,
    total_pop_2020,
    total_pop_2021,
    total_pop_2022
  )
)

total_population <- total_population %>%
  mutate(row_id = row_number()) %>%
  pivot_longer(-row_id, names_to = "column", values_to = "value") %>% separate(column, into = c("total", "pop", "year")) %>% subset(select = c(year, value)) %>% rbind(c(2023, 68265200))

ons_english_population_estimates <- fread("Inputs/england_pop_mid_year_estimates_ons.csv") %>% as_tibble() %>% janitor::clean_names()
ons_english_population_estimates <- ons_english_population_estimates[-1:-7, ]
colnames(ons_english_population_estimates) <- c("year", "england_mye_population_estimate")
ons_english_population_estimates$england_mye_population_estimate <- as.numeric(ons_english_population_estimates$england_mye_population_estimate)
ons_english_population_estimates$year <- as.integer(ons_english_population_estimates$year)

KSD_patients_total_n <- ons_english_population_estimates %>% subset(year > 2010) %>% mutate(total_patients = england_mye_population_estimate * 0.01) # Use lower estimate of 1% incidence)

### 1.4 Numbers undergoing intervention as per HES data from AD ####
## Read in Data
hes_data_elective <- fread("Inputs/intervention_hes_data_elective.csv") %>% as_tibble() %>% janitor::clean_names() %>% subset(select = c(finy, urs, pcnl, eswl))
hes_data_elective_emergency <- fread("Inputs/intervention_hes_data_elective_and_emergency.csv") %>% as_tibble() %>% janitor::clean_names() %>% subset(select = c(finy, urs, pcnl, eswl)) %>% slice_head(n =
                                                                                                                                                                                                                             13)


hes_data_elective_emergency <- hes_data_elective_emergency %>% separate(finy, into =
                                                                          c("year", "extra")) %>% subset(select = -extra) %>% mutate(total_treatment_n = urs + pcnl + eswl, .keep = "all")

hes_data_elective_emergency$year <- as.integer(hes_data_elective_emergency$year)

hes_data_elective_emergency <- hes_data_elective_emergency %>% left_join(KSD_patients_total_n, by = c("year" = "year")) %>% select(year,
                                                                                                                                   england_mye_population_estimate,
                                                                                                                                   total_patients,
                                                                                                                                   everything())


# Intervention Types Distribution
hes_data_elective_emergency <- hes_data_elective_emergency %>% mutate(
  eswl_proportion = eswl / total_treatment_n,
  urs_proportion = urs / total_treatment_n,
  pcnl_proportion = pcnl / total_treatment_n,
  .keep = "all"
)

# Intervention Outcomes
hes_data_elective_emergency <- hes_data_elective_emergency %>% mutate(
  eswl_stone_free = eswl * 0.5,
  eswl_residual_stones = eswl * 0.5,
  .keep = "all"
)

# Residual Stone Outcomes
hes_data_elective_emergency <- hes_data_elective_emergency %>% mutate(
  eswl_residual_asymptomatic = eswl_residual_stones * 0.5,
  eswl_residual_symptomatic = eswl_residual_stones * 0.5,
  urs_stone_free = urs * 0.6,
  urs_residual_stones = urs * 0.4,
  urs_residual_asymptomatic = urs_residual_stones * 0.5,
  urs_residual_symptomatic = urs_residual_stones * 0.5,
  pcnl_stone_free = pcnl * 0.74,
  pcnl_residual_stones = pcnl * 0.26,
  pcnl_residual_asymptomatic = pcnl_residual_stones * 0.5,
  pcnl_residual_symptomatic = pcnl_residual_stones * 0.5,
  .keep = "all"
)


# Spontaneous passage numbers - assume 50% of those with spontaneous passage are SF after
hes_data_elective_emergency <- hes_data_elective_emergency %>% mutate(
  spontaneous_passage_n = total_patients - total_treatment_n,
  spontaneous_passage_stone_free = spontaneous_passage_n * 0.74,
  total_sf_patients = spontaneous_passage_stone_free + pcnl_stone_free + urs_stone_free + eswl_stone_free,
  .keep = "all"
)

# Calculate patients in each risk category
hes_data_elective_emergency <- hes_data_elective_emergency %>% mutate(
  total_patients_low_risk = total_patients * risk_status_proportion_low,
  total_patients_high_risk = total_patients * risk_status_proportion_high,
  .keep = "all"
)

# Recurrence Calculations
hes_data_elective_emergency <- hes_data_elective_emergency %>% mutate(
  patients_with_recurrence_low = total_patients_low_risk * recurrence_rate_low,
  patients_with_recurrence_high = total_patients_high_risk * recurrence_rate_high,
  .keep = "all"
)

# Combine total recurrence
hes_data_elective_emergency <- hes_data_elective_emergency %>% mutate(
  patients_with_recurrence = patients_with_recurrence_low + patients_with_recurrence_high,
  .keep = "all"
)

# Symptomatic vs Asymptomatic Breakdown
hes_data_elective_emergency <- hes_data_elective_emergency %>% mutate(
  asymptomatic_patients_low = patients_with_recurrence_low * 0.7,
  symptomatic_patients_low = patients_with_recurrence_low * 0.3,
  asymptomatic_patients_high = patients_with_recurrence_high * 0.7,
  symptomatic_patients_high = patients_with_recurrence_high * 0.3,
  .keep = "all"
)



# Combine symptomatic and asymptomatic patients
hes_data_elective_emergency <- hes_data_elective_emergency %>% mutate(
  asymptomatic_patients = asymptomatic_patients_low + asymptomatic_patients_high,
  symptomatic_patients = symptomatic_patients_low + symptomatic_patients_high,
  .keep = "all"
)



### 1.5 Death rates - as per ONS by sex for all cause mortality for 2013-2023 ####
# Currently in percentage format
death_rates_ons <- fread("Inputs/death_rates_ons.csv") %>% as_tibble() %>% janitor::clean_names()
life_expectancy_ons <- fread("Inputs/ons_life_expectancy.csv") %>% as_tibble() %>% janitor::clean_names()
life_expectancy_ons$sex <- as.factor(life_expectancy_ons$sex)

death_rates_ons <- death_rates_ons %>% cbind(
  Age_range = c(
    "<1",
    "1-4",
    "5-9",
    "10-14",
    "15-19",
    "20-24",
    "25-29",
    "30-34",
    "35-39",
    "40-44",
    "45-49",
    "50-54",
    "55-59",
    "60-64",
    "65-69",
    "70-74",
    "75-79",
    "80-84",
    "85-90",
    ">90"
  )
) %>% select(Age_range, sex, everything()) %>% subset(
  select = -c(
    age,
    x2013,
    x2014,
    x2015,
    x2016,
    x2017,
    x2018,
    x2019,
    x2020,
    x2021,
    x2022,
    x2023
  )
)

colnames(death_rates_ons) <- c(
  "Age_range",
  "sex",
  "thirteen",
  "fourteen",
  "fifteen",
  "sixteen",
  "seventeen",
  "eighteen",
  "nineteen",
  "twenty",
  "twentyone",
  "twentytwo",
  "twentythree"
)

death_rates_ons <- death_rates_ons %>% mutate(
  "thirteen" = thirteen / 100,
  "fourteen" = fourteen / 100,
  "fifteen" = fifteen / 100,
  "sixteen" = sixteen / 100,
  "seventeen" = seventeen / 100,
  "eighteen" = eighteen / 100,
  "nineteen" =  nineteen / 100,
  "twenty" = twenty / 100,
  "twentyone" = twentyone / 100,
  "twentytwo" = twentytwo / 100,
  "twentythree" = twentythree / 100
)

death_rates_ons <- death_rates_ons %>% subset(Age_range != "<1")

death_rates_ons$Age_range <- factor(
  death_rates_ons$Age_range,
  levels = c(
    "1-4",
    "5-9",
    "10-14",
    "15-19",
    "20-24",
    "25-29",
    "30-34",
    "35-39",
    "40-44",
    "45-49",
    "50-54",
    "55-59",
    "60-64",
    "65-69",
    "70-74",
    "75-79",
    "80-84",
    "85-90",
    ">90"
  ),
  ordered = TRUE
)

death_rates_ons$sex <- as.factor(death_rates_ons$sex)



### 1.6 Renal colic presentations without intervention ####
# HES data from Jour et al. 2022 - PMID: 35306719
# Data on those NOT undergoing intervention from Schoenfeld et al. 2019 - PMID: 31790565

hes_colic_data <- cbind(
  year = c(2016, 2017, 2018, 2019, 2020),
  colic_n = c(
    39941,
    #N20.1 Calculus of ureter + N20.2 Calculus of kidney and ureter + N23.X Unspecified renal colic
    40702,
    #Presume that those NOT undergoing emergency procedures are managed conservatively
    40524,
    41936,
    41556
  ),
  emergency_procedures_n = c(1897, 2118, 2274, 2334, 2659),
  elective_procedures_n = c(40403, 40898, 40985, 40857, 41199)
) %>% as_tibble() %>% mutate(
  conservative_mx_n = colic_n - emergency_procedures_n,
  conservative_mx_prop = conservative_mx_n / colic_n,
  .keep = "all"
)

hes_data_elective_emergency <- hes_data_elective_emergency %>% left_join(
  hes_colic_data, by = c("year" = "year")) %>% mutate(
    colic_sf = colic_n * 0.8, #from MIMIC
    colic_nsf = colic_n * 0.2,
    clinically_significant_n = colic_n + urs + pcnl + eswl,
    total_sf_clin_signif = colic_sf + urs_stone_free + pcnl_stone_free + eswl_stone_free
  )


# Define total numbers entering models
total_n_2016 <- 552890
total_n_2017 <- 556195
total_n_2018 <- 559245
total_n_2019 <- 562301
total_n_2020 <- 563260

total_sf_n_2016 <- 402558
total_sf_n_2017 <- 405095
total_sf_n_2018 <- 407516
total_sf_n_2019 <- 409571
total_sf_n_2020 <- 411895

clinically_signif_n_2016 <- 82581
clinically_signif_n_2017 <- 83701
clinically_signif_n_2018 <- 83620
clinically_signif_n_2019 <- 85501
clinically_signif_n_2020 <- 71902

clinically_signif_sf_n_2016 <- 56926
clinically_signif_sf_n_2017 <- 57891
clinically_signif_sf_n_2018 <- 57985
clinically_signif_sf_n_2019 <- 59255
clinically_signif_sf_n_2020 <- 50783

### 1.7 Costs derived from NHS National tariff 2022-23 Annex A unless otherwise specified ####
# Clinic costs
initial_consultation_cost <- 145  # First appointment
clinic_review_cost <- 71  # Follow-up appointment
imaging_cost <- 27  # Direct access plain film X-ray (as per 22-23 non mandatory guide prices)
us_cost <- 43 # Ultrasound Scan with duration of less than 20 minutes, without Contrast (RD40Z)
ct_cost <-  69 # Computerised Tomography Scan of One Area, without Contrast, 19 years and over (RD20A)
urine_24_hr_cost <- 190.5

# Intervention Costs
eswl_cost <- 445 * 2  # assume 2 ESWL treatments (LB36Z)
urs_cost <- 2386  # median cost for URS (LB65D)
pcnl_cost <- 4548  # cost for CC 0-2 (LB75B)
stent_cost <- 822 # intermediate endoscopic bladder procedures (LB14Z)

# Acute presentation costs
ed_presentation_cost <- 288 #  Category 3 Investigation with Category 1-3 Treatment

# Costs associated with 'low risk' disease (as per EAU) - XR FU
year_1_lr_sf_fu_cost_xr <- 2 * (clinic_review_cost + imaging_cost)
year_2_lr_sf_fu_cost_xr <- 0
year_3_lr_sf_fu_cost_xr <- 1 * (clinic_review_cost + imaging_cost)
year_4_lr_sf_fu_cost_xr <- 1 * (clinic_review_cost + imaging_cost)
year_5_lr_sf_fu_cost_xr <- 1 * (clinic_review_cost + imaging_cost)

year_1_lr_less4_fu_cost_xr <- 2 * (clinic_review_cost + imaging_cost)
year_2_lr_less4_fu_cost_xr <- clinic_review_cost + imaging_cost
year_3_lr_less4_fu_cost_xr <- clinic_review_cost + imaging_cost
year_4_lr_less4_fu_cost_xr <- clinic_review_cost + imaging_cost
year_5_lr_less4_fu_cost_xr <- clinic_review_cost + imaging_cost

year_1_lr_more4_fu_cost_xr <- 2 * (clinic_review_cost + imaging_cost)
year_2_lr_more4_fu_cost_xr <- 2 * (clinic_review_cost + imaging_cost)
year_3_lr_more4_fu_cost_xr <- clinic_review_cost + imaging_cost
year_4_lr_more4_fu_cost_xr <- clinic_review_cost + imaging_cost
year_5_lr_more4_fu_cost_xr <- clinic_review_cost + imaging_cost

# Costs associated with 'low risk' disease (as per EAU) - US FU
year_1_lr_sf_fu_cost_us <- 2 * (clinic_review_cost + us_cost)
year_2_lr_sf_fu_cost_us <- 0
year_3_lr_sf_fu_cost_us <- 1 * (clinic_review_cost + us_cost)
year_4_lr_sf_fu_cost_us <- 1 * (clinic_review_cost + us_cost)
year_5_lr_sf_fu_cost_us <- 1 * (clinic_review_cost + us_cost)

year_1_lr_less4_fu_cost_us <- 2 * (clinic_review_cost + us_cost)
year_2_lr_less4_fu_cost_us <- clinic_review_cost + us_cost
year_3_lr_less4_fu_cost_us <- clinic_review_cost + us_cost
year_4_lr_less4_fu_cost_us <- clinic_review_cost + us_cost
year_5_lr_less4_fu_cost_us <- clinic_review_cost + us_cost

year_1_lr_more4_fu_cost_us <- 2 * (clinic_review_cost + us_cost)
year_2_lr_more4_fu_cost_us <- 2 * (clinic_review_cost + us_cost)
year_3_lr_more4_fu_cost_us <- clinic_review_cost + us_cost
year_4_lr_more4_fu_cost_us <- clinic_review_cost + us_cost
year_5_lr_more4_fu_cost_us <- clinic_review_cost + us_cost

# Costs associated with 'low risk' disease (as per EAU) - XR + US FU
year_1_lr_sf_fu_cost_xr_us <- 2 * (clinic_review_cost + imaging_cost + us_cost)
year_2_lr_sf_fu_cost_xr_us <- 0
year_3_lr_sf_fu_cost_xr_us <- 1 * (clinic_review_cost + imaging_cost + us_cost)
year_4_lr_sf_fu_cost_xr_us <- 1 * (clinic_review_cost + imaging_cost + us_cost)
year_5_lr_sf_fu_cost_xr_us <- 1 * (clinic_review_cost + imaging_cost + us_cost)

year_1_lr_less4_fu_cost_xr_us <- 2 * (clinic_review_cost + imaging_cost + us_cost)
year_2_lr_less4_fu_cost_xr_us <- clinic_review_cost + imaging_cost + us_cost
year_3_lr_less4_fu_cost_xr_us <- clinic_review_cost + imaging_cost + us_cost
year_4_lr_less4_fu_cost_xr_us <- clinic_review_cost + imaging_cost + us_cost
year_5_lr_less4_fu_cost_xr_us <- clinic_review_cost + imaging_cost + us_cost

year_1_lr_more4_fu_cost_xr_us <- 2 * (clinic_review_cost + imaging_cost + us_cost)
year_2_lr_more4_fu_cost_xr_us <- 2 * (clinic_review_cost + imaging_cost + us_cost)
year_3_lr_more4_fu_cost_xr_us <- clinic_review_cost + imaging_cost + us_cost
year_4_lr_more4_fu_cost_xr_us <- clinic_review_cost + imaging_cost + us_cost
year_5_lr_more4_fu_cost_xr_us <- clinic_review_cost + imaging_cost + us_cost

# Costs associated with 'low risk' disease (as per EAU) - CT FU
year_1_lr_sf_fu_cost_ct <- 2 * (clinic_review_cost + ct_cost)
year_2_lr_sf_fu_cost_ct <- 0
year_3_lr_sf_fu_cost_ct <- 1 * (clinic_review_cost + ct_cost)
year_4_lr_sf_fu_cost_ct <- 1 * (clinic_review_cost + ct_cost)
year_5_lr_sf_fu_cost_ct <- 1 * (clinic_review_cost + ct_cost)

year_1_lr_less4_fu_cost_ct <- 2 * (clinic_review_cost + ct_cost)
year_2_lr_less4_fu_cost_ct <- clinic_review_cost + ct_cost
year_3_lr_less4_fu_cost_ct <- clinic_review_cost + ct_cost
year_4_lr_less4_fu_cost_ct <- clinic_review_cost + ct_cost
year_5_lr_less4_fu_cost_ct <- clinic_review_cost + ct_cost

year_1_lr_more4_fu_cost_ct <- 2 * (clinic_review_cost + ct_cost)
year_2_lr_more4_fu_cost_ct <- 2 * (clinic_review_cost + ct_cost)
year_3_lr_more4_fu_cost_ct <- clinic_review_cost + ct_cost
year_4_lr_more4_fu_cost_ct <- clinic_review_cost + ct_cost
year_5_lr_more4_fu_cost_ct <- clinic_review_cost + ct_cost


# Costs associated with 'high risk' disease
## XR FU
year_1_hr_sf_fu_cost_current_xr <- 2 * (clinic_review_cost + imaging_cost) + urine_24_hr_cost
year_1_hr_sf_fu_cost_eau_xr <- 2 * (clinic_review_cost + imaging_cost + urine_24_hr_cost)

year_2_hr_sf_fu_cost_current_xr <- 1 * (clinic_review_cost + imaging_cost + urine_24_hr_cost)
year_2_hr_sf_fu_cost_eau_xr <- 1 * (clinic_review_cost + imaging_cost + urine_24_hr_cost)

year_3_onwards_hr_sf_fu_cost_current_xr <- 1 * (clinic_review_cost + imaging_cost + urine_24_hr_cost)
year_3_onwards_hr_sf_fu_cost_eau_xr <- 1 * (clinic_review_cost + imaging_cost + urine_24_hr_cost)

year_3_hr_sf_fu_cost_current_xr <- 2 * (clinic_review_cost + imaging_cost + urine_24_hr_cost)
year_3_hr_sf_fu_cost_eau_xr <- 1 * (clinic_review_cost + imaging_cost + urine_24_hr_cost)
year_4_hr_sf_fu_cost_eau_xr <- 1 * (clinic_review_cost + imaging_cost + urine_24_hr_cost)
year_5_hr_sf_fu_cost_eau_xr <- 1 * (clinic_review_cost + imaging_cost + urine_24_hr_cost)

year_1_hr_less4_fu_cost_eau_xr <- 2 * (clinic_review_cost + imaging_cost + urine_24_hr_cost)
year_2_hr_less4_fu_cost_eau_xr <- 1 * (clinic_review_cost + imaging_cost + urine_24_hr_cost)
year_3_hr_less4_fu_cost_eau_xr <- 1 * (clinic_review_cost + imaging_cost + urine_24_hr_cost)
year_4_hr_less4_fu_cost_eau_xr <- 1 * (clinic_review_cost + imaging_cost + urine_24_hr_cost)
year_5_hr_less4_fu_cost_eau_xr <- 1 * (clinic_review_cost + imaging_cost + urine_24_hr_cost)

year_1_hr_less4_fu_cost_eau_xr <- 2 * (clinic_review_cost + imaging_cost + urine_24_hr_cost)
year_2_hr_more4_fu_cost_eau_xr <- 2 * (clinic_review_cost + imaging_cost) + urine_24_hr_cost
year_3_hr_more4_fu_cost_eau_xr <- 1 * (clinic_review_cost + imaging_cost + urine_24_hr_cost)
year_4_hr_more4_fu_cost_eau_xr <- 1 * (clinic_review_cost + imaging_cost + urine_24_hr_cost)
year_5_hr_more4_fu_cost_eau_xr <- 1 * (clinic_review_cost + imaging_cost + urine_24_hr_cost)

## US FU
year_1_hr_sf_fu_cost_current_us <- 2 * (clinic_review_cost + us_cost) + urine_24_hr_cost
year_1_hr_sf_fu_cost_eau_us <- 2 * (clinic_review_cost + us_cost + urine_24_hr_cost)

year_2_hr_sf_fu_cost_current_us <- 1 * (clinic_review_cost + us_cost + urine_24_hr_cost)
year_2_hr_sf_fu_cost_eau_us <- 1 * (clinic_review_cost + us_cost + urine_24_hr_cost)

year_3_onwards_hr_sf_fu_cost_current_us <- 1 * (clinic_review_cost + us_cost + urine_24_hr_cost)
year_3_onwards_hr_sf_fu_cost_eau_us <- 1 * (clinic_review_cost + us_cost + urine_24_hr_cost)

year_3_hr_sf_fu_cost_current_us <- 2 * (clinic_review_cost + us_cost + urine_24_hr_cost)
year_3_hr_sf_fu_cost_eau_us <- 1 * (clinic_review_cost + us_cost + urine_24_hr_cost)
year_4_hr_sf_fu_cost_eau_us <- 1 * (clinic_review_cost + us_cost + urine_24_hr_cost)
year_5_hr_sf_fu_cost_eau_us <- 1 * (clinic_review_cost + us_cost + urine_24_hr_cost)

year_1_hr_less4_fu_cost_eau_us <- 2 * (clinic_review_cost + us_cost + urine_24_hr_cost)
year_2_hr_less4_fu_cost_eau_us <- 1 * (clinic_review_cost + us_cost + urine_24_hr_cost)
year_3_hr_less4_fu_cost_eau_us <- 1 * (clinic_review_cost + us_cost + urine_24_hr_cost)
year_4_hr_less4_fu_cost_eau_us <- 1 * (clinic_review_cost + us_cost + urine_24_hr_cost)
year_5_hr_less4_fu_cost_eau_us <- 1 * (clinic_review_cost + us_cost + urine_24_hr_cost)

year_1_hr_less4_fu_cost_eau_us <- 2 * (clinic_review_cost + us_cost + urine_24_hr_cost)
year_2_hr_more4_fu_cost_eau_us <- 2 * (clinic_review_cost + us_cost) + urine_24_hr_cost
year_3_hr_more4_fu_cost_eau_us <- 1 * (clinic_review_cost + us_cost + urine_24_hr_cost)
year_4_hr_more4_fu_cost_eau_us <- 1 * (clinic_review_cost + us_cost + urine_24_hr_cost)
year_5_hr_more4_fu_cost_eau_us <- 1 * (clinic_review_cost + us_cost + urine_24_hr_cost)

## XR + US FU
year_1_hr_sf_fu_cost_current_xr_us <- 2 * (clinic_review_cost + (imaging_cost * 0.8) + (us_cost * 0.2)) + urine_24_hr_cost # 20% radiolucent and therefore across a population this will be accurate
year_1_hr_sf_fu_cost_eau_xr_us <- 2 * (clinic_review_cost + (imaging_cost * 0.8) + (us_cost * 0.2) + urine_24_hr_cost)

year_2_hr_sf_fu_cost_current_xr_us <- 1 * (clinic_review_cost + (imaging_cost * 0.8) + (us_cost * 0.2) + urine_24_hr_cost)
year_2_hr_sf_fu_cost_eau_xr_us <- 1 * (clinic_review_cost + (imaging_cost * 0.8) + (us_cost * 0.2) + urine_24_hr_cost)

year_3_onwards_hr_sf_fu_cost_current_xr_us <- 1 * (clinic_review_cost + (imaging_cost * 0.8) + (us_cost * 0.2) + urine_24_hr_cost)
year_3_onwards_hr_sf_fu_cost_eau_xr_us <- 1 * (clinic_review_cost + (imaging_cost * 0.8) + (us_cost * 0.2) + urine_24_hr_cost)

year_3_hr_sf_fu_cost_current_xr_us <- 2 * (clinic_review_cost + (imaging_cost * 0.8) + (us_cost * 0.2) + urine_24_hr_cost)
year_3_hr_sf_fu_cost_eau_xr_us <- 1 * (clinic_review_cost + (imaging_cost * 0.8) + (us_cost * 0.2) + urine_24_hr_cost)
year_4_hr_sf_fu_cost_eau_xr_us <- 1 * (clinic_review_cost + (imaging_cost * 0.8) + (us_cost * 0.2) + urine_24_hr_cost)
year_5_hr_sf_fu_cost_eau_xr_us <- 1 * (clinic_review_cost + (imaging_cost * 0.8) + (us_cost * 0.2) + urine_24_hr_cost)

year_1_hr_less4_fu_cost_eau_xr_us <- 2 * (clinic_review_cost + (imaging_cost * 0.8) + (us_cost * 0.2) + urine_24_hr_cost)
year_2_hr_less4_fu_cost_eau_xr_us <- 1 * (clinic_review_cost + (imaging_cost * 0.8) + (us_cost * 0.2) + urine_24_hr_cost)
year_3_hr_less4_fu_cost_eau_xr_us <- 1 * (clinic_review_cost + (imaging_cost * 0.8) + (us_cost * 0.2) + urine_24_hr_cost)
year_4_hr_less4_fu_cost_eau_xr_us <- 1 * (clinic_review_cost + (imaging_cost * 0.8) + (us_cost * 0.2) + urine_24_hr_cost)
year_5_hr_less4_fu_cost_eau_xr_us <- 1 * (clinic_review_cost + (imaging_cost * 0.8) + (us_cost * 0.2) + urine_24_hr_cost)

year_1_hr_less4_fu_cost_eau_xr_us <- 2 * (clinic_review_cost + (imaging_cost * 0.8) + (us_cost * 0.2) + urine_24_hr_cost)
year_2_hr_more4_fu_cost_eau_xr_us <- 2 * (clinic_review_cost + (imaging_cost * 0.8) + (us_cost * 0.2)) + urine_24_hr_cost
year_3_hr_more4_fu_cost_eau_xr_us <- 1 * (clinic_review_cost + (imaging_cost * 0.8) + (us_cost * 0.2) + urine_24_hr_cost)
year_4_hr_more4_fu_cost_eau_xr_us <- 1 * (clinic_review_cost + (imaging_cost * 0.8) + (us_cost * 0.2) + urine_24_hr_cost)
year_5_hr_more4_fu_cost_eau_xr_us <- 1 * (clinic_review_cost + (imaging_cost * 0.8) + (us_cost * 0.2) + urine_24_hr_cost)

## CT FU
year_1_hr_sf_fu_cost_current_ct <- 2 * (clinic_review_cost + ct_cost) + urine_24_hr_cost
year_1_hr_sf_fu_cost_eau_ct <- 2 * (clinic_review_cost + ct_cost + urine_24_hr_cost)

year_2_hr_sf_fu_cost_current_ct <- 1 * (clinic_review_cost + ct_cost + urine_24_hr_cost)
year_2_hr_sf_fu_cost_eau_ct <- 1 * (clinic_review_cost + ct_cost + urine_24_hr_cost)

year_3_onwards_hr_sf_fu_cost_current_ct <- 1 * (clinic_review_cost + ct_cost + urine_24_hr_cost)
year_3_onwards_hr_sf_fu_cost_eau_ct <- 1 * (clinic_review_cost + ct_cost + urine_24_hr_cost)

year_3_hr_sf_fu_cost_current_ct <- 2 * (clinic_review_cost + ct_cost + urine_24_hr_cost)
year_3_hr_sf_fu_cost_eau_ct <- 1 * (clinic_review_cost + ct_cost + urine_24_hr_cost)
year_4_hr_sf_fu_cost_eau_ct <- 1 * (clinic_review_cost + ct_cost + urine_24_hr_cost)
year_5_hr_sf_fu_cost_eau_ct <- 1 * (clinic_review_cost + ct_cost + urine_24_hr_cost)

year_1_hr_less4_fu_cost_eau_ct <- 2 * (clinic_review_cost + ct_cost + urine_24_hr_cost)
year_2_hr_less4_fu_cost_eau_ct <- 1 * (clinic_review_cost + ct_cost + urine_24_hr_cost)
year_3_hr_less4_fu_cost_eau_ct <- 1 * (clinic_review_cost + ct_cost + urine_24_hr_cost)
year_4_hr_less4_fu_cost_eau_ct <- 1 * (clinic_review_cost + ct_cost + urine_24_hr_cost)
year_5_hr_less4_fu_cost_eau_ct <- 1 * (clinic_review_cost + ct_cost + urine_24_hr_cost)

year_1_hr_less4_fu_cost_eau_ct <- 2 * (clinic_review_cost + ct_cost + urine_24_hr_cost)
year_2_hr_more4_fu_cost_eau_ct <- 2 * (clinic_review_cost + ct_cost) + urine_24_hr_cost
year_3_hr_more4_fu_cost_eau_ct <- 1 * (clinic_review_cost + ct_cost + urine_24_hr_cost)
year_4_hr_more4_fu_cost_eau_ct <- 1 * (clinic_review_cost + ct_cost + urine_24_hr_cost)
year_5_hr_more4_fu_cost_eau_ct <- 1 * (clinic_review_cost + ct_cost + urine_24_hr_cost)

# Create tibble with all costs + types of FU
fu_costs <- as_tibble(cbind(
  cost = c(
    year_1_lr_sf_fu_cost_xr,
    year_2_lr_sf_fu_cost_xr,
    year_3_lr_sf_fu_cost_xr,
    year_4_lr_sf_fu_cost_xr,
    year_5_lr_sf_fu_cost_xr,
    year_1_lr_less4_fu_cost_xr,
    year_2_lr_less4_fu_cost_xr,
    year_3_lr_less4_fu_cost_xr,
    year_4_lr_less4_fu_cost_xr,
    year_5_lr_less4_fu_cost_xr,
    year_1_lr_more4_fu_cost_xr,
    year_2_lr_more4_fu_cost_xr,
    year_3_lr_more4_fu_cost_xr,
    year_4_lr_more4_fu_cost_xr,
    year_5_lr_more4_fu_cost_xr,
    year_1_hr_sf_fu_cost_current_xr,
    year_1_hr_sf_fu_cost_eau_xr,
    year_2_hr_sf_fu_cost_current_xr,
    year_2_hr_sf_fu_cost_eau_xr,
    year_3_onwards_hr_sf_fu_cost_current_xr,
    year_3_onwards_hr_sf_fu_cost_eau_xr,
    year_3_hr_sf_fu_cost_current_xr,
    year_3_hr_sf_fu_cost_eau_xr,
    year_4_hr_sf_fu_cost_eau_xr,
    year_5_hr_sf_fu_cost_eau_xr,
    year_1_hr_less4_fu_cost_eau_xr,
    year_2_hr_less4_fu_cost_eau_xr,
    year_3_hr_less4_fu_cost_eau_xr,
    year_4_hr_less4_fu_cost_eau_xr,
    year_5_hr_less4_fu_cost_eau_xr,
    year_1_hr_less4_fu_cost_eau_xr,
    year_2_hr_more4_fu_cost_eau_xr,
    year_3_hr_more4_fu_cost_eau_xr,
    year_4_hr_more4_fu_cost_eau_xr,
    year_5_hr_more4_fu_cost_eau_xr
  ),
  point_and_type_of_fu = c(
    "year_1_lr_sf_fu_cost",
    "year_2_lr_sf_fu_cost",
    "year_3_lr_sf_fu_cost",
    "year_4_lr_sf_fu_cost",
    "year_5_lr_sf_fu_cost",
    "year_1_lr_less4_fu_cost",
    "year_2_lr_less4_fu_cost",
    "year_3_lr_less4_fu_cost",
    "year_4_lr_less4_fu_cost",
    "year_5_lr_less4_fu_cost",
    "year_1_lr_more4_fu_cost",
    "year_2_lr_more4_fu_cost",
    "year_3_lr_more4_fu_cost",
    "year_4_lr_more4_fu_cost",
    "year_5_lr_more4_fu_cost",
    "year_1_hr_sf_fu_cost_current",
    "year_1_hr_sf_fu_cost_eau",
    "year_2_hr_sf_fu_cost_current",
    "year_2_hr_sf_fu_cost_eau",
    "year_3_onwards_hr_sf_fu_cost_current",
    "year_3_onwards_hr_sf_fu_cost_eau",
    "year_3_hr_sf_fu_cost_current",
    "year_3_hr_sf_fu_cost_eau",
    "year_4_hr_sf_fu_cost_eau",
    "year_5_hr_sf_fu_cost_eau",
    "year_1_hr_less4_fu_cost_eau",
    "year_2_hr_less4_fu_cost_eau",
    "year_3_hr_less4_fu_cost_eau",
    "year_4_hr_less4_fu_cost_eau",
    "year_5_hr_less4_fu_cost_eau",
    "year_1_hr_more4_fu_cost_eau",
    "year_2_hr_more4_fu_cost_eau",
    "year_3_hr_more4_fu_cost_eau",
    "year_4_hr_more4_fu_cost_eau",
    "year_5_hr_more4_fu_cost_eau"
  ),
  imaging_type = "xr"
)) %>% rbind(cbind(
  cost = c(
    year_1_lr_sf_fu_cost_xr_us,
    year_2_lr_sf_fu_cost_xr_us,
    year_3_lr_sf_fu_cost_xr_us,
    year_4_lr_sf_fu_cost_xr_us,
    year_5_lr_sf_fu_cost_xr_us,
    year_1_lr_less4_fu_cost_xr_us,
    year_2_lr_less4_fu_cost_xr_us,
    year_3_lr_less4_fu_cost_xr_us,
    year_4_lr_less4_fu_cost_xr_us,
    year_5_lr_less4_fu_cost_xr_us,
    year_1_lr_more4_fu_cost_xr_us,
    year_2_lr_more4_fu_cost_xr_us,
    year_3_lr_more4_fu_cost_xr_us,
    year_4_lr_more4_fu_cost_xr_us,
    year_5_lr_more4_fu_cost_xr_us,
    year_1_hr_sf_fu_cost_current_xr_us,
    year_1_hr_sf_fu_cost_eau_xr_us,
    year_2_hr_sf_fu_cost_current_xr_us,
    year_2_hr_sf_fu_cost_eau_xr_us,
    year_3_onwards_hr_sf_fu_cost_current_xr_us,
    year_3_onwards_hr_sf_fu_cost_eau_xr_us,
    year_3_hr_sf_fu_cost_current_xr_us,
    year_3_hr_sf_fu_cost_eau_xr_us,
    year_4_hr_sf_fu_cost_eau_xr_us,
    year_5_hr_sf_fu_cost_eau_xr_us,
    year_1_hr_less4_fu_cost_eau_xr_us,
    year_2_hr_less4_fu_cost_eau_xr_us,
    year_3_hr_less4_fu_cost_eau_xr_us,
    year_4_hr_less4_fu_cost_eau_xr_us,
    year_5_hr_less4_fu_cost_eau_xr_us,
    year_1_hr_less4_fu_cost_eau_xr_us,
    year_2_hr_more4_fu_cost_eau_xr_us,
    year_3_hr_more4_fu_cost_eau_xr_us,
    year_4_hr_more4_fu_cost_eau_xr_us,
    year_5_hr_more4_fu_cost_eau_xr_us
  ),
  point_and_type_of_fu = c(
    "year_1_lr_sf_fu_cost",
    "year_2_lr_sf_fu_cost",
    "year_3_lr_sf_fu_cost",
    "year_4_lr_sf_fu_cost",
    "year_5_lr_sf_fu_cost",
    "year_1_lr_less4_fu_cost",
    "year_2_lr_less4_fu_cost",
    "year_3_lr_less4_fu_cost",
    "year_4_lr_less4_fu_cost",
    "year_5_lr_less4_fu_cost",
    "year_1_lr_more4_fu_cost",
    "year_2_lr_more4_fu_cost",
    "year_3_lr_more4_fu_cost",
    "year_4_lr_more4_fu_cost",
    "year_5_lr_more4_fu_cost",
    "year_1_hr_sf_fu_cost_current",
    "year_1_hr_sf_fu_cost_eau",
    "year_2_hr_sf_fu_cost_current",
    "year_2_hr_sf_fu_cost_eau",
    "year_3_onwards_hr_sf_fu_cost_current",
    "year_3_onwards_hr_sf_fu_cost_eau",
    "year_3_hr_sf_fu_cost_current",
    "year_3_hr_sf_fu_cost_eau",
    "year_4_hr_sf_fu_cost_eau",
    "year_5_hr_sf_fu_cost_eau",
    "year_1_hr_less4_fu_cost_eau",
    "year_2_hr_less4_fu_cost_eau",
    "year_3_hr_less4_fu_cost_eau",
    "year_4_hr_less4_fu_cost_eau",
    "year_5_hr_less4_fu_cost_eau",
    "year_1_hr_more4_fu_cost_eau",
    "year_2_hr_more4_fu_cost_eau",
    "year_3_hr_more4_fu_cost_eau",
    "year_4_hr_more4_fu_cost_eau",
    "year_5_hr_more4_fu_cost_eau"
  ),
  imaging_type = "xr_us"
)) %>% rbind(cbind(
  cost = c(
    year_1_lr_sf_fu_cost_ct,
    year_2_lr_sf_fu_cost_ct,
    year_3_lr_sf_fu_cost_ct,
    year_4_lr_sf_fu_cost_ct,
    year_5_lr_sf_fu_cost_ct,
    year_1_lr_less4_fu_cost_ct,
    year_2_lr_less4_fu_cost_ct,
    year_3_lr_less4_fu_cost_ct,
    year_4_lr_less4_fu_cost_ct,
    year_5_lr_less4_fu_cost_ct,
    year_1_lr_more4_fu_cost_ct,
    year_2_lr_more4_fu_cost_ct,
    year_3_lr_more4_fu_cost_ct,
    year_4_lr_more4_fu_cost_ct,
    year_5_lr_more4_fu_cost_ct,
    year_1_hr_sf_fu_cost_current_ct,
    year_1_hr_sf_fu_cost_eau_ct,
    year_2_hr_sf_fu_cost_current_ct,
    year_2_hr_sf_fu_cost_eau_ct,
    year_3_onwards_hr_sf_fu_cost_current_ct,
    year_3_onwards_hr_sf_fu_cost_eau_ct,
    year_3_hr_sf_fu_cost_current_ct,
    year_3_hr_sf_fu_cost_eau_ct,
    year_4_hr_sf_fu_cost_eau_ct,
    year_5_hr_sf_fu_cost_eau_ct,
    year_1_hr_less4_fu_cost_eau_ct,
    year_2_hr_less4_fu_cost_eau_ct,
    year_3_hr_less4_fu_cost_eau_ct,
    year_4_hr_less4_fu_cost_eau_ct,
    year_5_hr_less4_fu_cost_eau_ct,
    year_1_hr_less4_fu_cost_eau_ct,
    year_2_hr_more4_fu_cost_eau_ct,
    year_3_hr_more4_fu_cost_eau_ct,
    year_4_hr_more4_fu_cost_eau_ct,
    year_5_hr_more4_fu_cost_eau_ct
  ),
  point_and_type_of_fu = c(
    "year_1_lr_sf_fu_cost",
    "year_2_lr_sf_fu_cost",
    "year_3_lr_sf_fu_cost",
    "year_4_lr_sf_fu_cost",
    "year_5_lr_sf_fu_cost",
    "year_1_lr_less4_fu_cost",
    "year_2_lr_less4_fu_cost",
    "year_3_lr_less4_fu_cost",
    "year_4_lr_less4_fu_cost",
    "year_5_lr_less4_fu_cost",
    "year_1_lr_more4_fu_cost",
    "year_2_lr_more4_fu_cost",
    "year_3_lr_more4_fu_cost",
    "year_4_lr_more4_fu_cost",
    "year_5_lr_more4_fu_cost",
    "year_1_hr_sf_fu_cost_current",
    "year_1_hr_sf_fu_cost_eau",
    "year_2_hr_sf_fu_cost_current",
    "year_2_hr_sf_fu_cost_eau",
    "year_3_onwards_hr_sf_fu_cost_current",
    "year_3_onwards_hr_sf_fu_cost_eau",
    "year_3_hr_sf_fu_cost_current",
    "year_3_hr_sf_fu_cost_eau",
    "year_4_hr_sf_fu_cost_eau",
    "year_5_hr_sf_fu_cost_eau",
    "year_1_hr_less4_fu_cost_eau",
    "year_2_hr_less4_fu_cost_eau",
    "year_3_hr_less4_fu_cost_eau",
    "year_4_hr_less4_fu_cost_eau",
    "year_5_hr_less4_fu_cost_eau",
    "year_1_hr_more4_fu_cost_eau",
    "year_2_hr_more4_fu_cost_eau",
    "year_3_hr_more4_fu_cost_eau",
    "year_4_hr_more4_fu_cost_eau",
    "year_5_hr_more4_fu_cost_eau"
  ),
  imaging_type = "ct"
)) %>% rbind(cbind(
  cost = c(
    year_1_lr_sf_fu_cost_us,
    year_2_lr_sf_fu_cost_us,
    year_3_lr_sf_fu_cost_us,
    year_4_lr_sf_fu_cost_us,
    year_5_lr_sf_fu_cost_us,
    year_1_lr_less4_fu_cost_us,
    year_2_lr_less4_fu_cost_us,
    year_3_lr_less4_fu_cost_us,
    year_4_lr_less4_fu_cost_us,
    year_5_lr_less4_fu_cost_us,
    year_1_lr_more4_fu_cost_us,
    year_2_lr_more4_fu_cost_us,
    year_3_lr_more4_fu_cost_us,
    year_4_lr_more4_fu_cost_us,
    year_5_lr_more4_fu_cost_us,
    year_1_hr_sf_fu_cost_current_us,
    year_1_hr_sf_fu_cost_eau_us,
    year_2_hr_sf_fu_cost_current_us,
    year_2_hr_sf_fu_cost_eau_us,
    year_3_onwards_hr_sf_fu_cost_current_us,
    year_3_onwards_hr_sf_fu_cost_eau_us,
    year_3_hr_sf_fu_cost_current_us,
    year_3_hr_sf_fu_cost_eau_us,
    year_4_hr_sf_fu_cost_eau_us,
    year_5_hr_sf_fu_cost_eau_us,
    year_1_hr_less4_fu_cost_eau_us,
    year_2_hr_less4_fu_cost_eau_us,
    year_3_hr_less4_fu_cost_eau_us,
    year_4_hr_less4_fu_cost_eau_us,
    year_5_hr_less4_fu_cost_eau_us,
    year_1_hr_less4_fu_cost_eau_us,
    year_2_hr_more4_fu_cost_eau_us,
    year_3_hr_more4_fu_cost_eau_us,
    year_4_hr_more4_fu_cost_eau_us,
    year_5_hr_more4_fu_cost_eau_us
  ),
  point_and_type_of_fu = c(
    "year_1_lr_sf_fu_cost",
    "year_2_lr_sf_fu_cost",
    "year_3_lr_sf_fu_cost",
    "year_4_lr_sf_fu_cost",
    "year_5_lr_sf_fu_cost",
    "year_1_lr_less4_fu_cost",
    "year_2_lr_less4_fu_cost",
    "year_3_lr_less4_fu_cost",
    "year_4_lr_less4_fu_cost",
    "year_5_lr_less4_fu_cost",
    "year_1_lr_more4_fu_cost",
    "year_2_lr_more4_fu_cost",
    "year_3_lr_more4_fu_cost",
    "year_4_lr_more4_fu_cost",
    "year_5_lr_more4_fu_cost",
    "year_1_hr_sf_fu_cost_current",
    "year_1_hr_sf_fu_cost_eau",
    "year_2_hr_sf_fu_cost_current",
    "year_2_hr_sf_fu_cost_eau",
    "year_3_onwards_hr_sf_fu_cost_current",
    "year_3_onwards_hr_sf_fu_cost_eau",
    "year_3_hr_sf_fu_cost_current",
    "year_3_hr_sf_fu_cost_eau",
    "year_4_hr_sf_fu_cost_eau",
    "year_5_hr_sf_fu_cost_eau",
    "year_1_hr_less4_fu_cost_eau",
    "year_2_hr_less4_fu_cost_eau",
    "year_3_hr_less4_fu_cost_eau",
    "year_4_hr_less4_fu_cost_eau",
    "year_5_hr_less4_fu_cost_eau",
    "year_1_hr_more4_fu_cost_eau",
    "year_2_hr_more4_fu_cost_eau",
    "year_3_hr_more4_fu_cost_eau",
    "year_4_hr_more4_fu_cost_eau",
    "year_5_hr_more4_fu_cost_eau"
  ),
  imaging_type = "us"
)) %>% select(point_and_type_of_fu, imaging_type, cost)

# Minimum Costs associated with recurrence
rec_cost_eswl <- clinic_review_cost + ct_cost + (eswl_cost * 2) 
rec_cost_urs <- clinic_review_cost + ct_cost + urs_cost
rec_cost_pcnl <- clinic_review_cost + ct_cost + pcnl_cost 

rec_cost_colic <- ed_presentation_cost + ct_cost  #Assume acute presentation with colic + spontaneous passage

# Recurrence metrics / proportions
sf_1yr_disease_progression <- 0.25 / 5 #Tzelves et al. 2023 - for those not stone free at this point
sf_1yr_intervention <- (0.25 / 2) / 5 #Sorensen et al. 2022 - 5% at 5 years - however very small numbers hence used estimate from Tzelves et al.
sf_1yr_colic <- (0.25 / 2) / 5  #Sorensen et al. 2022 - 5% at 5 years - seems to be equal split between intervention and colic for these patients so use this to divide overall disease progression

less4_1yr_disease_progression <- 0.34 / (35/12) #Tzelves et al. 2023 - 35 months
less4_1yr_intervention <- 0.29 / (34/12) # 34 months
less4_1yr_colic <- less4_1yr_disease_progression - less4_1yr_intervention # Subtract intervention from disease progression

more4_1yr_disease_progression <- 0.67 / 5 #Tzelves et al. 2023
more4_1yr_intervention <- 0.66 / 5
more4_1yr_colic <- 0.01 / 5


### 1.8 Radiation doses ####
xr_dose <- 0.7
us_dose <- 0
normal_ct_dose <- 10
ct_dose <- 1.8 # Ultra-low dose CT

pcnl_dose <- 6.6
urs_dose <- 2.1
swl_dose <- 2.4

xr_sens <- 0.67
xr_spec <- 0.98
us_sens <- 0.54
us_spec <- 0.91
ct_sens <- 0.99
ct_spec <- 0.99
uldct_sens <- 0.90
uldct_spec <- 0.93

### 1.9 Clinician time ####
# Clinic times in minutes
initial_consultation_time <- 30  # First appointment
clinic_review_time <- 15  # Follow-up appointment
imaging_time <- 10  # 
us_time <- 15 # 
ct_time <-  10  # 
urine_24_hr_time <- 190.5

# Intervention times
eswl_time <- 445 * 2  # assume 2 ESWL treatments (LB36Z)
urs_time <- 2386  # median time for URS (LB65D)
pcnl_time <- 4548  # time for CC 0-2 (LB75B)
stent_time <- 822 # intermediate endoscopic bladder procedures (LB14Z)

# Acute presentation times
ed_presentation_time <- 288 #  Category 3 Investigation with Category 1-3 Treatment

# times associated with 'low risk' disease (as per EAU) - XR FU
year_1_lr_sf_fu_time_xr <- 2 * (clinic_review_time + imaging_time)
year_2_lr_sf_fu_time_xr <- 0
year_3_lr_sf_fu_time_xr <- 1 * (clinic_review_time + imaging_time)
year_4_lr_sf_fu_time_xr <- 1 * (clinic_review_time + imaging_time)
year_5_lr_sf_fu_time_xr <- 1 * (clinic_review_time + imaging_time)

year_1_lr_less4_fu_time_xr <- 2 * (clinic_review_time + imaging_time)
year_2_lr_less4_fu_time_xr <- clinic_review_time + imaging_time
year_3_lr_less4_fu_time_xr <- clinic_review_time + imaging_time
year_4_lr_less4_fu_time_xr <- clinic_review_time + imaging_time
year_5_lr_less4_fu_time_xr <- clinic_review_time + imaging_time

year_1_lr_more4_fu_time_xr <- 2 * (clinic_review_time + imaging_time)
year_2_lr_more4_fu_time_xr <- 2 * (clinic_review_time + imaging_time)
year_3_lr_more4_fu_time_xr <- clinic_review_time + imaging_time
year_4_lr_more4_fu_time_xr <- clinic_review_time + imaging_time
year_5_lr_more4_fu_time_xr <- clinic_review_time + imaging_time

# times associated with 'low risk' disease (as per EAU) - XR + US FU
year_1_lr_sf_fu_time_xr_us <- 2 * (clinic_review_time + imaging_time + us_time)
year_2_lr_sf_fu_time_xr_us <- 0
year_3_lr_sf_fu_time_xr_us <- 1 * (clinic_review_time + imaging_time + us_time)
year_4_lr_sf_fu_time_xr_us <- 1 * (clinic_review_time + imaging_time + us_time)
year_5_lr_sf_fu_time_xr_us <- 1 * (clinic_review_time + imaging_time + us_time)

year_1_lr_less4_fu_time_xr_us <- 2 * (clinic_review_time + imaging_time + us_time)
year_2_lr_less4_fu_time_xr_us <- clinic_review_time + imaging_time + us_time
year_3_lr_less4_fu_time_xr_us <- clinic_review_time + imaging_time + us_time
year_4_lr_less4_fu_time_xr_us <- clinic_review_time + imaging_time + us_time
year_5_lr_less4_fu_time_xr_us <- clinic_review_time + imaging_time + us_time

year_1_lr_more4_fu_time_xr_us <- 2 * (clinic_review_time + imaging_time + us_time)
year_2_lr_more4_fu_time_xr_us <- 2 * (clinic_review_time + imaging_time + us_time)
year_3_lr_more4_fu_time_xr_us <- clinic_review_time + imaging_time + us_time
year_4_lr_more4_fu_time_xr_us <- clinic_review_time + imaging_time + us_time
year_5_lr_more4_fu_time_xr_us <- clinic_review_time + imaging_time + us_time

# times associated with 'low risk' disease (as per EAU) - CT FU
year_1_lr_sf_fu_time_ct <- 2 * (clinic_review_time + ct_time)
year_2_lr_sf_fu_time_ct <- 0
year_3_lr_sf_fu_time_ct <- 1 * (clinic_review_time + ct_time)
year_4_lr_sf_fu_time_ct <- 1 * (clinic_review_time + ct_time)
year_5_lr_sf_fu_time_ct <- 1 * (clinic_review_time + ct_time)

year_1_lr_less4_fu_time_ct <- 2 * (clinic_review_time + ct_time)
year_2_lr_less4_fu_time_ct <- clinic_review_time + ct_time
year_3_lr_less4_fu_time_ct <- clinic_review_time + ct_time
year_4_lr_less4_fu_time_ct <- clinic_review_time + ct_time
year_5_lr_less4_fu_time_ct <- clinic_review_time + ct_time

year_1_lr_more4_fu_time_ct <- 2 * (clinic_review_time + ct_time)
year_2_lr_more4_fu_time_ct <- 2 * (clinic_review_time + ct_time)
year_3_lr_more4_fu_time_ct <- clinic_review_time + ct_time
year_4_lr_more4_fu_time_ct <- clinic_review_time + ct_time
year_5_lr_more4_fu_time_ct <- clinic_review_time + ct_time


# times associated with 'high risk' disease
## XR FU
year_1_hr_sf_fu_time_current_xr <- 2 * (clinic_review_time + imaging_time) + urine_24_hr_time
year_1_hr_sf_fu_time_eau_xr <- 2 * (clinic_review_time + imaging_time + urine_24_hr_time)

year_2_hr_sf_fu_time_current_xr <- 1 * (clinic_review_time + imaging_time + urine_24_hr_time)
year_2_hr_sf_fu_time_eau_xr <- 1 * (clinic_review_time + imaging_time + urine_24_hr_time)

year_3_onwards_hr_sf_fu_time_current_xr <- 1 * (clinic_review_time + imaging_time + urine_24_hr_time)
year_3_onwards_hr_sf_fu_time_eau_xr <- 1 * (clinic_review_time + imaging_time + urine_24_hr_time)

year_3_hr_sf_fu_time_current_xr <- 2 * (clinic_review_time + imaging_time + urine_24_hr_time)
year_3_hr_sf_fu_time_eau_xr <- 1 * (clinic_review_time + imaging_time + urine_24_hr_time)
year_4_hr_sf_fu_time_eau_xr <- 1 * (clinic_review_time + imaging_time + urine_24_hr_time)
year_5_hr_sf_fu_time_eau_xr <- 1 * (clinic_review_time + imaging_time + urine_24_hr_time)

year_1_hr_less4_fu_time_eau_xr <- 2 * (clinic_review_time + imaging_time + urine_24_hr_time)
year_2_hr_less4_fu_time_eau_xr <- 1 * (clinic_review_time + imaging_time + urine_24_hr_time)
year_3_hr_less4_fu_time_eau_xr <- 1 * (clinic_review_time + imaging_time + urine_24_hr_time)
year_4_hr_less4_fu_time_eau_xr <- 1 * (clinic_review_time + imaging_time + urine_24_hr_time)
year_5_hr_less4_fu_time_eau_xr <- 1 * (clinic_review_time + imaging_time + urine_24_hr_time)

year_1_hr_less4_fu_time_eau_xr <- 2 * (clinic_review_time + imaging_time + urine_24_hr_time)
year_2_hr_more4_fu_time_eau_xr <- 2 * (clinic_review_time + imaging_time) + urine_24_hr_time
year_3_hr_more4_fu_time_eau_xr <- 1 * (clinic_review_time + imaging_time + urine_24_hr_time)
year_4_hr_more4_fu_time_eau_xr <- 1 * (clinic_review_time + imaging_time + urine_24_hr_time)
year_5_hr_more4_fu_time_eau_xr <- 1 * (clinic_review_time + imaging_time + urine_24_hr_time)

## XR + US FU
year_1_hr_sf_fu_time_current_xr_us <- 2 * (clinic_review_time + imaging_time + us_time) + urine_24_hr_time
year_1_hr_sf_fu_time_eau_xr_us <- 2 * (clinic_review_time + imaging_time + us_time + urine_24_hr_time)

year_2_hr_sf_fu_time_current_xr_us <- 1 * (clinic_review_time + imaging_time + us_time + urine_24_hr_time)
year_2_hr_sf_fu_time_eau_xr_us <- 1 * (clinic_review_time + imaging_time + us_time + urine_24_hr_time)

year_3_onwards_hr_sf_fu_time_current_xr_us <- 1 * (clinic_review_time + imaging_time + us_time + urine_24_hr_time)
year_3_onwards_hr_sf_fu_time_eau_xr_us <- 1 * (clinic_review_time + imaging_time + us_time + urine_24_hr_time)

year_3_hr_sf_fu_time_current_xr_us <- 2 * (clinic_review_time + imaging_time + us_time + urine_24_hr_time)
year_3_hr_sf_fu_time_eau_xr_us <- 1 * (clinic_review_time + imaging_time + us_time + urine_24_hr_time)
year_4_hr_sf_fu_time_eau_xr_us <- 1 * (clinic_review_time + imaging_time + us_time + urine_24_hr_time)
year_5_hr_sf_fu_time_eau_xr_us <- 1 * (clinic_review_time + imaging_time + us_time + urine_24_hr_time)

year_1_hr_less4_fu_time_eau_xr_us <- 2 * (clinic_review_time + imaging_time + us_time + urine_24_hr_time)
year_2_hr_less4_fu_time_eau_xr_us <- 1 * (clinic_review_time + imaging_time + us_time + urine_24_hr_time)
year_3_hr_less4_fu_time_eau_xr_us <- 1 * (clinic_review_time + imaging_time + us_time + urine_24_hr_time)
year_4_hr_less4_fu_time_eau_xr_us <- 1 * (clinic_review_time + imaging_time + us_time + urine_24_hr_time)
year_5_hr_less4_fu_time_eau_xr_us <- 1 * (clinic_review_time + imaging_time + us_time + urine_24_hr_time)

year_1_hr_less4_fu_time_eau_xr_us <- 2 * (clinic_review_time + imaging_time + us_time + urine_24_hr_time)
year_2_hr_more4_fu_time_eau_xr_us <- 2 * (clinic_review_time + imaging_time + us_time) + urine_24_hr_time
year_3_hr_more4_fu_time_eau_xr_us <- 1 * (clinic_review_time + imaging_time + us_time + urine_24_hr_time)
year_4_hr_more4_fu_time_eau_xr_us <- 1 * (clinic_review_time + imaging_time + us_time + urine_24_hr_time)
year_5_hr_more4_fu_time_eau_xr_us <- 1 * (clinic_review_time + imaging_time + us_time + urine_24_hr_time)

## CT FU
year_1_hr_sf_fu_time_current_ct <- 2 * (clinic_review_time + ct_time) + urine_24_hr_time
year_1_hr_sf_fu_time_eau_ct <- 2 * (clinic_review_time + ct_time + urine_24_hr_time)

year_2_hr_sf_fu_time_current_ct <- 1 * (clinic_review_time + ct_time + urine_24_hr_time)
year_2_hr_sf_fu_time_eau_ct <- 1 * (clinic_review_time + ct_time + urine_24_hr_time)

year_3_onwards_hr_sf_fu_time_current_ct <- 1 * (clinic_review_time + ct_time + urine_24_hr_time)
year_3_onwards_hr_sf_fu_time_eau_ct <- 1 * (clinic_review_time + ct_time + urine_24_hr_time)

year_3_hr_sf_fu_time_current_ct <- 2 * (clinic_review_time + ct_time + urine_24_hr_time)
year_3_hr_sf_fu_time_eau_ct <- 1 * (clinic_review_time + ct_time + urine_24_hr_time)
year_4_hr_sf_fu_time_eau_ct <- 1 * (clinic_review_time + ct_time + urine_24_hr_time)
year_5_hr_sf_fu_time_eau_ct <- 1 * (clinic_review_time + ct_time + urine_24_hr_time)

year_1_hr_less4_fu_time_eau_ct <- 2 * (clinic_review_time + ct_time + urine_24_hr_time)
year_2_hr_less4_fu_time_eau_ct <- 1 * (clinic_review_time + ct_time + urine_24_hr_time)
year_3_hr_less4_fu_time_eau_ct <- 1 * (clinic_review_time + ct_time + urine_24_hr_time)
year_4_hr_less4_fu_time_eau_ct <- 1 * (clinic_review_time + ct_time + urine_24_hr_time)
year_5_hr_less4_fu_time_eau_ct <- 1 * (clinic_review_time + ct_time + urine_24_hr_time)

year_1_hr_less4_fu_time_eau_ct <- 2 * (clinic_review_time + ct_time + urine_24_hr_time)
year_2_hr_more4_fu_time_eau_ct <- 2 * (clinic_review_time + ct_time) + urine_24_hr_time
year_3_hr_more4_fu_time_eau_ct <- 1 * (clinic_review_time + ct_time + urine_24_hr_time)
year_4_hr_more4_fu_time_eau_ct <- 1 * (clinic_review_time + ct_time + urine_24_hr_time)
year_5_hr_more4_fu_time_eau_ct <- 1 * (clinic_review_time + ct_time + urine_24_hr_time)

# Create tibble with all times + types of FU
fu_times <- as_tibble(cbind(
  time = c(
    year_1_lr_sf_fu_time_xr,
    year_2_lr_sf_fu_time_xr,
    year_3_lr_sf_fu_time_xr,
    year_4_lr_sf_fu_time_xr,
    year_5_lr_sf_fu_time_xr,
    year_1_lr_less4_fu_time_xr,
    year_2_lr_less4_fu_time_xr,
    year_3_lr_less4_fu_time_xr,
    year_4_lr_less4_fu_time_xr,
    year_5_lr_less4_fu_time_xr,
    year_1_lr_more4_fu_time_xr,
    year_2_lr_more4_fu_time_xr,
    year_3_lr_more4_fu_time_xr,
    year_4_lr_more4_fu_time_xr,
    year_5_lr_more4_fu_time_xr,
    year_1_hr_sf_fu_time_current_xr,
    year_1_hr_sf_fu_time_eau_xr,
    year_2_hr_sf_fu_time_current_xr,
    year_2_hr_sf_fu_time_eau_xr,
    year_3_onwards_hr_sf_fu_time_current_xr,
    year_3_onwards_hr_sf_fu_time_eau_xr,
    year_3_hr_sf_fu_time_current_xr,
    year_3_hr_sf_fu_time_eau_xr,
    year_4_hr_sf_fu_time_eau_xr,
    year_5_hr_sf_fu_time_eau_xr,
    year_1_hr_less4_fu_time_eau_xr,
    year_2_hr_less4_fu_time_eau_xr,
    year_3_hr_less4_fu_time_eau_xr,
    year_4_hr_less4_fu_time_eau_xr,
    year_5_hr_less4_fu_time_eau_xr,
    year_1_hr_less4_fu_time_eau_xr,
    year_2_hr_more4_fu_time_eau_xr,
    year_3_hr_more4_fu_time_eau_xr,
    year_4_hr_more4_fu_time_eau_xr,
    year_5_hr_more4_fu_time_eau_xr
  ),
  point_and_type_of_fu = c(
    "year_1_lr_sf_fu_time",
    "year_2_lr_sf_fu_time",
    "year_3_lr_sf_fu_time",
    "year_4_lr_sf_fu_time",
    "year_5_lr_sf_fu_time",
    "year_1_lr_less4_fu_time",
    "year_2_lr_less4_fu_time",
    "year_3_lr_less4_fu_time",
    "year_4_lr_less4_fu_time",
    "year_5_lr_less4_fu_time",
    "year_1_lr_more4_fu_time",
    "year_2_lr_more4_fu_time",
    "year_3_lr_more4_fu_time",
    "year_4_lr_more4_fu_time",
    "year_5_lr_more4_fu_time",
    "year_1_hr_sf_fu_time_current",
    "year_1_hr_sf_fu_time_eau",
    "year_2_hr_sf_fu_time_current",
    "year_2_hr_sf_fu_time_eau",
    "year_3_onwards_hr_sf_fu_time_current",
    "year_3_onwards_hr_sf_fu_time_eau",
    "year_3_hr_sf_fu_time_current",
    "year_3_hr_sf_fu_time_eau",
    "year_4_hr_sf_fu_time_eau",
    "year_5_hr_sf_fu_time_eau",
    "year_1_hr_less4_fu_time_eau",
    "year_2_hr_less4_fu_time_eau",
    "year_3_hr_less4_fu_time_eau",
    "year_4_hr_less4_fu_time_eau",
    "year_5_hr_less4_fu_time_eau",
    "year_1_hr_more4_fu_time_eau",
    "year_2_hr_more4_fu_time_eau",
    "year_3_hr_more4_fu_time_eau",
    "year_4_hr_more4_fu_time_eau",
    "year_5_hr_more4_fu_time_eau"
  ),
  imaging_type = "xr"
)) %>% rbind(cbind(
  time = c(
    year_1_lr_sf_fu_time_xr_us,
    year_2_lr_sf_fu_time_xr_us,
    year_3_lr_sf_fu_time_xr_us,
    year_4_lr_sf_fu_time_xr_us,
    year_5_lr_sf_fu_time_xr_us,
    year_1_lr_less4_fu_time_xr_us,
    year_2_lr_less4_fu_time_xr_us,
    year_3_lr_less4_fu_time_xr_us,
    year_4_lr_less4_fu_time_xr_us,
    year_5_lr_less4_fu_time_xr_us,
    year_1_lr_more4_fu_time_xr_us,
    year_2_lr_more4_fu_time_xr_us,
    year_3_lr_more4_fu_time_xr_us,
    year_4_lr_more4_fu_time_xr_us,
    year_5_lr_more4_fu_time_xr_us,
    year_1_hr_sf_fu_time_current_xr_us,
    year_1_hr_sf_fu_time_eau_xr_us,
    year_2_hr_sf_fu_time_current_xr_us,
    year_2_hr_sf_fu_time_eau_xr_us,
    year_3_onwards_hr_sf_fu_time_current_xr_us,
    year_3_onwards_hr_sf_fu_time_eau_xr_us,
    year_3_hr_sf_fu_time_current_xr_us,
    year_3_hr_sf_fu_time_eau_xr_us,
    year_4_hr_sf_fu_time_eau_xr_us,
    year_5_hr_sf_fu_time_eau_xr_us,
    year_1_hr_less4_fu_time_eau_xr_us,
    year_2_hr_less4_fu_time_eau_xr_us,
    year_3_hr_less4_fu_time_eau_xr_us,
    year_4_hr_less4_fu_time_eau_xr_us,
    year_5_hr_less4_fu_time_eau_xr_us,
    year_1_hr_less4_fu_time_eau_xr_us,
    year_2_hr_more4_fu_time_eau_xr_us,
    year_3_hr_more4_fu_time_eau_xr_us,
    year_4_hr_more4_fu_time_eau_xr_us,
    year_5_hr_more4_fu_time_eau_xr_us
  ),
  point_and_type_of_fu = c(
    "year_1_lr_sf_fu_time",
    "year_2_lr_sf_fu_time",
    "year_3_lr_sf_fu_time",
    "year_4_lr_sf_fu_time",
    "year_5_lr_sf_fu_time",
    "year_1_lr_less4_fu_time",
    "year_2_lr_less4_fu_time",
    "year_3_lr_less4_fu_time",
    "year_4_lr_less4_fu_time",
    "year_5_lr_less4_fu_time",
    "year_1_lr_more4_fu_time",
    "year_2_lr_more4_fu_time",
    "year_3_lr_more4_fu_time",
    "year_4_lr_more4_fu_time",
    "year_5_lr_more4_fu_time",
    "year_1_hr_sf_fu_time_current",
    "year_1_hr_sf_fu_time_eau",
    "year_2_hr_sf_fu_time_current",
    "year_2_hr_sf_fu_time_eau",
    "year_3_onwards_hr_sf_fu_time_current",
    "year_3_onwards_hr_sf_fu_time_eau",
    "year_3_hr_sf_fu_time_current",
    "year_3_hr_sf_fu_time_eau",
    "year_4_hr_sf_fu_time_eau",
    "year_5_hr_sf_fu_time_eau",
    "year_1_hr_less4_fu_time_eau",
    "year_2_hr_less4_fu_time_eau",
    "year_3_hr_less4_fu_time_eau",
    "year_4_hr_less4_fu_time_eau",
    "year_5_hr_less4_fu_time_eau",
    "year_1_hr_more4_fu_time_eau",
    "year_2_hr_more4_fu_time_eau",
    "year_3_hr_more4_fu_time_eau",
    "year_4_hr_more4_fu_time_eau",
    "year_5_hr_more4_fu_time_eau"
  ),
  imaging_type = "xr_us"
)) %>% rbind(cbind(
  time = c(
    year_1_lr_sf_fu_time_ct,
    year_2_lr_sf_fu_time_ct,
    year_3_lr_sf_fu_time_ct,
    year_4_lr_sf_fu_time_ct,
    year_5_lr_sf_fu_time_ct,
    year_1_lr_less4_fu_time_ct,
    year_2_lr_less4_fu_time_ct,
    year_3_lr_less4_fu_time_ct,
    year_4_lr_less4_fu_time_ct,
    year_5_lr_less4_fu_time_ct,
    year_1_lr_more4_fu_time_ct,
    year_2_lr_more4_fu_time_ct,
    year_3_lr_more4_fu_time_ct,
    year_4_lr_more4_fu_time_ct,
    year_5_lr_more4_fu_time_ct,
    year_1_hr_sf_fu_time_current_ct,
    year_1_hr_sf_fu_time_eau_ct,
    year_2_hr_sf_fu_time_current_ct,
    year_2_hr_sf_fu_time_eau_ct,
    year_3_onwards_hr_sf_fu_time_current_ct,
    year_3_onwards_hr_sf_fu_time_eau_ct,
    year_3_hr_sf_fu_time_current_ct,
    year_3_hr_sf_fu_time_eau_ct,
    year_4_hr_sf_fu_time_eau_ct,
    year_5_hr_sf_fu_time_eau_ct,
    year_1_hr_less4_fu_time_eau_ct,
    year_2_hr_less4_fu_time_eau_ct,
    year_3_hr_less4_fu_time_eau_ct,
    year_4_hr_less4_fu_time_eau_ct,
    year_5_hr_less4_fu_time_eau_ct,
    year_1_hr_less4_fu_time_eau_ct,
    year_2_hr_more4_fu_time_eau_ct,
    year_3_hr_more4_fu_time_eau_ct,
    year_4_hr_more4_fu_time_eau_ct,
    year_5_hr_more4_fu_time_eau_ct
  ),
  point_and_type_of_fu = c(
    "year_1_lr_sf_fu_time",
    "year_2_lr_sf_fu_time",
    "year_3_lr_sf_fu_time",
    "year_4_lr_sf_fu_time",
    "year_5_lr_sf_fu_time",
    "year_1_lr_less4_fu_time",
    "year_2_lr_less4_fu_time",
    "year_3_lr_less4_fu_time",
    "year_4_lr_less4_fu_time",
    "year_5_lr_less4_fu_time",
    "year_1_lr_more4_fu_time",
    "year_2_lr_more4_fu_time",
    "year_3_lr_more4_fu_time",
    "year_4_lr_more4_fu_time",
    "year_5_lr_more4_fu_time",
    "year_1_hr_sf_fu_time_current",
    "year_1_hr_sf_fu_time_eau",
    "year_2_hr_sf_fu_time_current",
    "year_2_hr_sf_fu_time_eau",
    "year_3_onwards_hr_sf_fu_time_current",
    "year_3_onwards_hr_sf_fu_time_eau",
    "year_3_hr_sf_fu_time_current",
    "year_3_hr_sf_fu_time_eau",
    "year_4_hr_sf_fu_time_eau",
    "year_5_hr_sf_fu_time_eau",
    "year_1_hr_less4_fu_time_eau",
    "year_2_hr_less4_fu_time_eau",
    "year_3_hr_less4_fu_time_eau",
    "year_4_hr_less4_fu_time_eau",
    "year_5_hr_less4_fu_time_eau",
    "year_1_hr_more4_fu_time_eau",
    "year_2_hr_more4_fu_time_eau",
    "year_3_hr_more4_fu_time_eau",
    "year_4_hr_more4_fu_time_eau",
    "year_5_hr_more4_fu_time_eau"
  ),
  imaging_type = "ct"
)) %>% select(point_and_type_of_fu, imaging_type, time)


## FU Appts

# appts associated with 'low risk' disease (as per EAU) - XR FU
year_1_lr_sf_fu_appts_xr <- 4
year_2_lr_sf_fu_appts_xr <- 0
year_3_lr_sf_fu_appts_xr <- 2
year_4_lr_sf_fu_appts_xr <- 2
year_5_lr_sf_fu_appts_xr <- 2

year_1_lr_less4_fu_appts_xr <- 2 * (2)
year_2_lr_less4_fu_appts_xr <- 2
year_3_lr_less4_fu_appts_xr <- 2
year_4_lr_less4_fu_appts_xr <- 2
year_5_lr_less4_fu_appts_xr <- 2

year_1_lr_more4_fu_appts_xr <- 2 * (2)
year_2_lr_more4_fu_appts_xr <- 2 * (2)
year_3_lr_more4_fu_appts_xr <- 2
year_4_lr_more4_fu_appts_xr <- 2
year_5_lr_more4_fu_appts_xr <- 2

# appts associated with 'low risk' disease (as per EAU) - US FU
year_1_lr_sf_fu_appts_us <- 4
year_2_lr_sf_fu_appts_us <- 0
year_3_lr_sf_fu_appts_us <- 2
year_4_lr_sf_fu_appts_us <- 2
year_5_lr_sf_fu_appts_us <- 2

year_1_lr_less4_fu_appts_us <- 2 * (2)
year_2_lr_less4_fu_appts_us <- 2
year_3_lr_less4_fu_appts_us <- 2
year_4_lr_less4_fu_appts_us <- 2
year_5_lr_less4_fu_appts_us <- 2

year_1_lr_more4_fu_appts_us <- 2 * (2)
year_2_lr_more4_fu_appts_us <- 2 * (2)
year_3_lr_more4_fu_appts_us <- 2
year_4_lr_more4_fu_appts_us <- 2
year_5_lr_more4_fu_appts_us <- 2

# appts associated with 'low risk' disease (as per EAU) - XR + US FU
year_1_lr_sf_fu_appts_xr_us <- 2 * (2 )
year_2_lr_sf_fu_appts_xr_us <- 0
year_3_lr_sf_fu_appts_xr_us <- 1 * (2 )
year_4_lr_sf_fu_appts_xr_us <- 1 * (2 )
year_5_lr_sf_fu_appts_xr_us <- 1 * (2 )

year_1_lr_less4_fu_appts_xr_us <- 2 * (2 )
year_2_lr_less4_fu_appts_xr_us <- 2 
year_3_lr_less4_fu_appts_xr_us <- 2 
year_4_lr_less4_fu_appts_xr_us <- 2 
year_5_lr_less4_fu_appts_xr_us <- 2 

year_1_lr_more4_fu_appts_xr_us <- 2 * (2 )
year_2_lr_more4_fu_appts_xr_us <- 2 * (2 )
year_3_lr_more4_fu_appts_xr_us <- 2 
year_4_lr_more4_fu_appts_xr_us <- 2 
year_5_lr_more4_fu_appts_xr_us <- 2 

# appts associated with 'low risk' disease (as per EAU) - CT FU
year_1_lr_sf_fu_appts_ct <- 2 * (2)
year_2_lr_sf_fu_appts_ct <- 0
year_3_lr_sf_fu_appts_ct <- 1 * (2)
year_4_lr_sf_fu_appts_ct <- 1 * (2)
year_5_lr_sf_fu_appts_ct <- 1 * (2)

year_1_lr_less4_fu_appts_ct <- 2 * (2)
year_2_lr_less4_fu_appts_ct <- 2
year_3_lr_less4_fu_appts_ct <- 2
year_4_lr_less4_fu_appts_ct <- 2
year_5_lr_less4_fu_appts_ct <- 2

year_1_lr_more4_fu_appts_ct <- 2 * (2)
year_2_lr_more4_fu_appts_ct <- 2 * (2)
year_3_lr_more4_fu_appts_ct <- 2
year_4_lr_more4_fu_appts_ct <- 2
year_5_lr_more4_fu_appts_ct <- 2


# appts associated with 'high risk' disease
## XR FU
year_1_hr_sf_fu_appts_current_xr <- 2 * (2+ 1)
year_1_hr_sf_fu_appts_eau_xr <- 2 * (2+ 1)

year_2_hr_sf_fu_appts_current_xr <- 1 * (2+ 1)
year_2_hr_sf_fu_appts_eau_xr <- 1 * (2+ 1)

year_3_onwards_hr_sf_fu_appts_current_xr <- 1 * (2+ 1)
year_3_onwards_hr_sf_fu_appts_eau_xr <- 1 * (2+ 1)

year_3_hr_sf_fu_appts_current_xr <- 2 * (2+ 1)
year_3_hr_sf_fu_appts_eau_xr <- 1 * (2+ 1)
year_4_hr_sf_fu_appts_eau_xr <- 1 * (2+ 1)
year_5_hr_sf_fu_appts_eau_xr <- 1 * (2+ 1)

year_1_hr_less4_fu_appts_eau_xr <- 2 * (2+ 1)
year_2_hr_less4_fu_appts_eau_xr <- 1 * (2+ 1)
year_3_hr_less4_fu_appts_eau_xr <- 1 * (2+ 1)
year_4_hr_less4_fu_appts_eau_xr <- 1 * (2+ 1)
year_5_hr_less4_fu_appts_eau_xr <- 1 * (2+ 1)

year_1_hr_less4_fu_appts_eau_xr <- 2 * (2+ 1)
year_2_hr_more4_fu_appts_eau_xr <- 2 * (2)+ 1
year_3_hr_more4_fu_appts_eau_xr <- 1 * (2+ 1)
year_4_hr_more4_fu_appts_eau_xr <- 1 * (2+ 1)
year_5_hr_more4_fu_appts_eau_xr <- 1 * (2+ 1)

## US FU
year_1_hr_sf_fu_appts_current_us <- 2 * (2+ 1)
year_1_hr_sf_fu_appts_eau_us <- 2 * (2+ 1)

year_2_hr_sf_fu_appts_current_us <- 1 * (2+ 1)
year_2_hr_sf_fu_appts_eau_us <- 1 * (2+ 1)

year_3_onwards_hr_sf_fu_appts_current_us <- 1 * (2+ 1)
year_3_onwards_hr_sf_fu_appts_eau_us <- 1 * (2+ 1)

year_3_hr_sf_fu_appts_current_us <- 2 * (2+ 1)
year_3_hr_sf_fu_appts_eau_us <- 1 * (2+ 1)
year_4_hr_sf_fu_appts_eau_us <- 1 * (2+ 1)
year_5_hr_sf_fu_appts_eau_us <- 1 * (2+ 1)

year_1_hr_less4_fu_appts_eau_us <- 2 * (2+ 1)
year_2_hr_less4_fu_appts_eau_us <- 1 * (2+ 1)
year_3_hr_less4_fu_appts_eau_us <- 1 * (2+ 1)
year_4_hr_less4_fu_appts_eau_us <- 1 * (2+ 1)
year_5_hr_less4_fu_appts_eau_us <- 1 * (2+ 1)

year_1_hr_less4_fu_appts_eau_us <- 2 * (2+ 1)
year_2_hr_more4_fu_appts_eau_us <- 2 * (2)+ 1
year_3_hr_more4_fu_appts_eau_us <- 1 * (2+ 1)
year_4_hr_more4_fu_appts_eau_us <- 1 * (2+ 1)
year_5_hr_more4_fu_appts_eau_us <- 1 * (2+ 1)

## XR + US FU
year_1_hr_sf_fu_appts_current_xr_us <- 2 * (2 + 1)
year_1_hr_sf_fu_appts_eau_xr_us <- 2 * (2 + 1)

year_2_hr_sf_fu_appts_current_xr_us <- 1 * (2 + 1)
year_2_hr_sf_fu_appts_eau_xr_us <- 1 * (2 + 1)

year_3_onwards_hr_sf_fu_appts_current_xr_us <- 1 * (2 + 1)
year_3_onwards_hr_sf_fu_appts_eau_xr_us <- 1 * (2 + 1)

year_3_hr_sf_fu_appts_current_xr_us <- 2 * (2 + 1)
year_3_hr_sf_fu_appts_eau_xr_us <- 1 * (2 + 1)
year_4_hr_sf_fu_appts_eau_xr_us <- 1 * (2 + 1)
year_5_hr_sf_fu_appts_eau_xr_us <- 1 * (2 + 1)

year_1_hr_less4_fu_appts_eau_xr_us <- 2 * (2 + 1)
year_2_hr_less4_fu_appts_eau_xr_us <- 1 * (2 + 1)
year_3_hr_less4_fu_appts_eau_xr_us <- 1 * (2 + 1)
year_4_hr_less4_fu_appts_eau_xr_us <- 1 * (2 + 1)
year_5_hr_less4_fu_appts_eau_xr_us <- 1 * (2 + 1)

year_1_hr_less4_fu_appts_eau_xr_us <- 2 * (2 + 1)
year_2_hr_more4_fu_appts_eau_xr_us <- 2 * (2 + 1)
year_3_hr_more4_fu_appts_eau_xr_us <- 1 * (2 + 1)
year_4_hr_more4_fu_appts_eau_xr_us <- 1 * (2 + 1)
year_5_hr_more4_fu_appts_eau_xr_us <- 1 * (2 + 1)

## CT FU
year_1_hr_sf_fu_appts_current_ct <- 2 * (2+ 1)
year_1_hr_sf_fu_appts_eau_ct <- 2 * (2+ 1)

year_2_hr_sf_fu_appts_current_ct <- 1 * (2+ 1)
year_2_hr_sf_fu_appts_eau_ct <- 1 * (2+ 1)

year_3_onwards_hr_sf_fu_appts_current_ct <- 1 * (2+ 1)
year_3_onwards_hr_sf_fu_appts_eau_ct <- 1 * (2+ 1)

year_3_hr_sf_fu_appts_current_ct <- 2 * (2+ 1)
year_3_hr_sf_fu_appts_eau_ct <- 1 * (2+ 1)
year_4_hr_sf_fu_appts_eau_ct <- 1 * (2+ 1)
year_5_hr_sf_fu_appts_eau_ct <- 1 * (2+ 1)

year_1_hr_less4_fu_appts_eau_ct <- 2 * (2+ 1)
year_2_hr_less4_fu_appts_eau_ct <- 1 * (2+ 1)
year_3_hr_less4_fu_appts_eau_ct <- 1 * (2+ 1)
year_4_hr_less4_fu_appts_eau_ct <- 1 * (2+ 1)
year_5_hr_less4_fu_appts_eau_ct <- 1 * (2+ 1)

year_1_hr_less4_fu_appts_eau_ct <- 2 * (2+ 1)
year_2_hr_more4_fu_appts_eau_ct <- 2 * (2)+ 1
year_3_hr_more4_fu_appts_eau_ct <- 1 * (2+ 1)
year_4_hr_more4_fu_appts_eau_ct <- 1 * (2+ 1)
year_5_hr_more4_fu_appts_eau_ct <- 1 * (2+ 1)


fu_appts <- as_tibble(cbind(
  appts = c(
    year_1_lr_sf_fu_appts_xr,
    year_2_lr_sf_fu_appts_xr,
    year_3_lr_sf_fu_appts_xr,
    year_4_lr_sf_fu_appts_xr,
    year_5_lr_sf_fu_appts_xr,
    year_1_lr_less4_fu_appts_xr,
    year_2_lr_less4_fu_appts_xr,
    year_3_lr_less4_fu_appts_xr,
    year_4_lr_less4_fu_appts_xr,
    year_5_lr_less4_fu_appts_xr,
    year_1_lr_more4_fu_appts_xr,
    year_2_lr_more4_fu_appts_xr,
    year_3_lr_more4_fu_appts_xr,
    year_4_lr_more4_fu_appts_xr,
    year_5_lr_more4_fu_appts_xr,
    year_1_hr_sf_fu_appts_current_xr,
    year_1_hr_sf_fu_appts_eau_xr,
    year_2_hr_sf_fu_appts_current_xr,
    year_2_hr_sf_fu_appts_eau_xr,
    year_3_onwards_hr_sf_fu_appts_current_xr,
    year_3_onwards_hr_sf_fu_appts_eau_xr,
    year_3_hr_sf_fu_appts_current_xr,
    year_3_hr_sf_fu_appts_eau_xr,
    year_4_hr_sf_fu_appts_eau_xr,
    year_5_hr_sf_fu_appts_eau_xr,
    year_1_hr_less4_fu_appts_eau_xr,
    year_2_hr_less4_fu_appts_eau_xr,
    year_3_hr_less4_fu_appts_eau_xr,
    year_4_hr_less4_fu_appts_eau_xr,
    year_5_hr_less4_fu_appts_eau_xr,
    year_1_hr_less4_fu_appts_eau_xr,
    year_2_hr_more4_fu_appts_eau_xr,
    year_3_hr_more4_fu_appts_eau_xr,
    year_4_hr_more4_fu_appts_eau_xr,
    year_5_hr_more4_fu_appts_eau_xr
  ),
  point_and_type_of_fu = c(
    "year_1_lr_sf_fu_appts",
    "year_2_lr_sf_fu_appts",
    "year_3_lr_sf_fu_appts",
    "year_4_lr_sf_fu_appts",
    "year_5_lr_sf_fu_appts",
    "year_1_lr_less4_fu_appts",
    "year_2_lr_less4_fu_appts",
    "year_3_lr_less4_fu_appts",
    "year_4_lr_less4_fu_appts",
    "year_5_lr_less4_fu_appts",
    "year_1_lr_more4_fu_appts",
    "year_2_lr_more4_fu_appts",
    "year_3_lr_more4_fu_appts",
    "year_4_lr_more4_fu_appts",
    "year_5_lr_more4_fu_appts",
    "year_1_hr_sf_fu_appts_current",
    "year_1_hr_sf_fu_appts_eau",
    "year_2_hr_sf_fu_appts_current",
    "year_2_hr_sf_fu_appts_eau",
    "year_3_onwards_hr_sf_fu_appts_current",
    "year_3_onwards_hr_sf_fu_appts_eau",
    "year_3_hr_sf_fu_appts_current",
    "year_3_hr_sf_fu_appts_eau",
    "year_4_hr_sf_fu_appts_eau",
    "year_5_hr_sf_fu_appts_eau",
    "year_1_hr_less4_fu_appts_eau",
    "year_2_hr_less4_fu_appts_eau",
    "year_3_hr_less4_fu_appts_eau",
    "year_4_hr_less4_fu_appts_eau",
    "year_5_hr_less4_fu_appts_eau",
    "year_1_hr_more4_fu_appts_eau",
    "year_2_hr_more4_fu_appts_eau",
    "year_3_hr_more4_fu_appts_eau",
    "year_4_hr_more4_fu_appts_eau",
    "year_5_hr_more4_fu_appts_eau"
  ),
  imaging_type = "xr"
)) %>% rbind(cbind(
  appts = c(
    year_1_lr_sf_fu_appts_xr_us,
    year_2_lr_sf_fu_appts_xr_us,
    year_3_lr_sf_fu_appts_xr_us,
    year_4_lr_sf_fu_appts_xr_us,
    year_5_lr_sf_fu_appts_xr_us,
    year_1_lr_less4_fu_appts_xr_us,
    year_2_lr_less4_fu_appts_xr_us,
    year_3_lr_less4_fu_appts_xr_us,
    year_4_lr_less4_fu_appts_xr_us,
    year_5_lr_less4_fu_appts_xr_us,
    year_1_lr_more4_fu_appts_xr_us,
    year_2_lr_more4_fu_appts_xr_us,
    year_3_lr_more4_fu_appts_xr_us,
    year_4_lr_more4_fu_appts_xr_us,
    year_5_lr_more4_fu_appts_xr_us,
    year_1_hr_sf_fu_appts_current_xr_us,
    year_1_hr_sf_fu_appts_eau_xr_us,
    year_2_hr_sf_fu_appts_current_xr_us,
    year_2_hr_sf_fu_appts_eau_xr_us,
    year_3_onwards_hr_sf_fu_appts_current_xr_us,
    year_3_onwards_hr_sf_fu_appts_eau_xr_us,
    year_3_hr_sf_fu_appts_current_xr_us,
    year_3_hr_sf_fu_appts_eau_xr_us,
    year_4_hr_sf_fu_appts_eau_xr_us,
    year_5_hr_sf_fu_appts_eau_xr_us,
    year_1_hr_less4_fu_appts_eau_xr_us,
    year_2_hr_less4_fu_appts_eau_xr_us,
    year_3_hr_less4_fu_appts_eau_xr_us,
    year_4_hr_less4_fu_appts_eau_xr_us,
    year_5_hr_less4_fu_appts_eau_xr_us,
    year_1_hr_less4_fu_appts_eau_xr_us,
    year_2_hr_more4_fu_appts_eau_xr_us,
    year_3_hr_more4_fu_appts_eau_xr_us,
    year_4_hr_more4_fu_appts_eau_xr_us,
    year_5_hr_more4_fu_appts_eau_xr_us
  ),
  point_and_type_of_fu = c(
    "year_1_lr_sf_fu_appts",
    "year_2_lr_sf_fu_appts",
    "year_3_lr_sf_fu_appts",
    "year_4_lr_sf_fu_appts",
    "year_5_lr_sf_fu_appts",
    "year_1_lr_less4_fu_appts",
    "year_2_lr_less4_fu_appts",
    "year_3_lr_less4_fu_appts",
    "year_4_lr_less4_fu_appts",
    "year_5_lr_less4_fu_appts",
    "year_1_lr_more4_fu_appts",
    "year_2_lr_more4_fu_appts",
    "year_3_lr_more4_fu_appts",
    "year_4_lr_more4_fu_appts",
    "year_5_lr_more4_fu_appts",
    "year_1_hr_sf_fu_appts_current",
    "year_1_hr_sf_fu_appts_eau",
    "year_2_hr_sf_fu_appts_current",
    "year_2_hr_sf_fu_appts_eau",
    "year_3_onwards_hr_sf_fu_appts_current",
    "year_3_onwards_hr_sf_fu_appts_eau",
    "year_3_hr_sf_fu_appts_current",
    "year_3_hr_sf_fu_appts_eau",
    "year_4_hr_sf_fu_appts_eau",
    "year_5_hr_sf_fu_appts_eau",
    "year_1_hr_less4_fu_appts_eau",
    "year_2_hr_less4_fu_appts_eau",
    "year_3_hr_less4_fu_appts_eau",
    "year_4_hr_less4_fu_appts_eau",
    "year_5_hr_less4_fu_appts_eau",
    "year_1_hr_more4_fu_appts_eau",
    "year_2_hr_more4_fu_appts_eau",
    "year_3_hr_more4_fu_appts_eau",
    "year_4_hr_more4_fu_appts_eau",
    "year_5_hr_more4_fu_appts_eau"
  ),
  imaging_type = "xr_us"
)) %>% rbind(cbind(
  appts = c(
    year_1_lr_sf_fu_appts_us,
    year_2_lr_sf_fu_appts_us,
    year_3_lr_sf_fu_appts_us,
    year_4_lr_sf_fu_appts_us,
    year_5_lr_sf_fu_appts_us,
    year_1_lr_less4_fu_appts_us,
    year_2_lr_less4_fu_appts_us,
    year_3_lr_less4_fu_appts_us,
    year_4_lr_less4_fu_appts_us,
    year_5_lr_less4_fu_appts_us,
    year_1_lr_more4_fu_appts_us,
    year_2_lr_more4_fu_appts_us,
    year_3_lr_more4_fu_appts_us,
    year_4_lr_more4_fu_appts_us,
    year_5_lr_more4_fu_appts_us,
    year_1_hr_sf_fu_appts_current_us,
    year_1_hr_sf_fu_appts_eau_us,
    year_2_hr_sf_fu_appts_current_us,
    year_2_hr_sf_fu_appts_eau_us,
    year_3_onwards_hr_sf_fu_appts_current_us,
    year_3_onwards_hr_sf_fu_appts_eau_us,
    year_3_hr_sf_fu_appts_current_us,
    year_3_hr_sf_fu_appts_eau_us,
    year_4_hr_sf_fu_appts_eau_us,
    year_5_hr_sf_fu_appts_eau_us,
    year_1_hr_less4_fu_appts_eau_us,
    year_2_hr_less4_fu_appts_eau_us,
    year_3_hr_less4_fu_appts_eau_us,
    year_4_hr_less4_fu_appts_eau_us,
    year_5_hr_less4_fu_appts_eau_us,
    year_1_hr_less4_fu_appts_eau_us,
    year_2_hr_more4_fu_appts_eau_us,
    year_3_hr_more4_fu_appts_eau_us,
    year_4_hr_more4_fu_appts_eau_us,
    year_5_hr_more4_fu_appts_eau_us
  ),
  point_and_type_of_fu = c(
    "year_1_lr_sf_fu_appts",
    "year_2_lr_sf_fu_appts",
    "year_3_lr_sf_fu_appts",
    "year_4_lr_sf_fu_appts",
    "year_5_lr_sf_fu_appts",
    "year_1_lr_less4_fu_appts",
    "year_2_lr_less4_fu_appts",
    "year_3_lr_less4_fu_appts",
    "year_4_lr_less4_fu_appts",
    "year_5_lr_less4_fu_appts",
    "year_1_lr_more4_fu_appts",
    "year_2_lr_more4_fu_appts",
    "year_3_lr_more4_fu_appts",
    "year_4_lr_more4_fu_appts",
    "year_5_lr_more4_fu_appts",
    "year_1_hr_sf_fu_appts_current",
    "year_1_hr_sf_fu_appts_eau",
    "year_2_hr_sf_fu_appts_current",
    "year_2_hr_sf_fu_appts_eau",
    "year_3_onwards_hr_sf_fu_appts_current",
    "year_3_onwards_hr_sf_fu_appts_eau",
    "year_3_hr_sf_fu_appts_current",
    "year_3_hr_sf_fu_appts_eau",
    "year_4_hr_sf_fu_appts_eau",
    "year_5_hr_sf_fu_appts_eau",
    "year_1_hr_less4_fu_appts_eau",
    "year_2_hr_less4_fu_appts_eau",
    "year_3_hr_less4_fu_appts_eau",
    "year_4_hr_less4_fu_appts_eau",
    "year_5_hr_less4_fu_appts_eau",
    "year_1_hr_more4_fu_appts_eau",
    "year_2_hr_more4_fu_appts_eau",
    "year_3_hr_more4_fu_appts_eau",
    "year_4_hr_more4_fu_appts_eau",
    "year_5_hr_more4_fu_appts_eau"
  ),
  imaging_type = "us"
)) %>% rbind(cbind(
  appts = c(
    year_1_lr_sf_fu_appts_ct,
    year_2_lr_sf_fu_appts_ct,
    year_3_lr_sf_fu_appts_ct,
    year_4_lr_sf_fu_appts_ct,
    year_5_lr_sf_fu_appts_ct,
    year_1_lr_less4_fu_appts_ct,
    year_2_lr_less4_fu_appts_ct,
    year_3_lr_less4_fu_appts_ct,
    year_4_lr_less4_fu_appts_ct,
    year_5_lr_less4_fu_appts_ct,
    year_1_lr_more4_fu_appts_ct,
    year_2_lr_more4_fu_appts_ct,
    year_3_lr_more4_fu_appts_ct,
    year_4_lr_more4_fu_appts_ct,
    year_5_lr_more4_fu_appts_ct,
    year_1_hr_sf_fu_appts_current_ct,
    year_1_hr_sf_fu_appts_eau_ct,
    year_2_hr_sf_fu_appts_current_ct,
    year_2_hr_sf_fu_appts_eau_ct,
    year_3_onwards_hr_sf_fu_appts_current_ct,
    year_3_onwards_hr_sf_fu_appts_eau_ct,
    year_3_hr_sf_fu_appts_current_ct,
    year_3_hr_sf_fu_appts_eau_ct,
    year_4_hr_sf_fu_appts_eau_ct,
    year_5_hr_sf_fu_appts_eau_ct,
    year_1_hr_less4_fu_appts_eau_ct,
    year_2_hr_less4_fu_appts_eau_ct,
    year_3_hr_less4_fu_appts_eau_ct,
    year_4_hr_less4_fu_appts_eau_ct,
    year_5_hr_less4_fu_appts_eau_ct,
    year_1_hr_less4_fu_appts_eau_ct,
    year_2_hr_more4_fu_appts_eau_ct,
    year_3_hr_more4_fu_appts_eau_ct,
    year_4_hr_more4_fu_appts_eau_ct,
    year_5_hr_more4_fu_appts_eau_ct
  ),
  point_and_type_of_fu = c(
    "year_1_lr_sf_fu_appts",
    "year_2_lr_sf_fu_appts",
    "year_3_lr_sf_fu_appts",
    "year_4_lr_sf_fu_appts",
    "year_5_lr_sf_fu_appts",
    "year_1_lr_less4_fu_appts",
    "year_2_lr_less4_fu_appts",
    "year_3_lr_less4_fu_appts",
    "year_4_lr_less4_fu_appts",
    "year_5_lr_less4_fu_appts",
    "year_1_lr_more4_fu_appts",
    "year_2_lr_more4_fu_appts",
    "year_3_lr_more4_fu_appts",
    "year_4_lr_more4_fu_appts",
    "year_5_lr_more4_fu_appts",
    "year_1_hr_sf_fu_appts_current",
    "year_1_hr_sf_fu_appts_eau",
    "year_2_hr_sf_fu_appts_current",
    "year_2_hr_sf_fu_appts_eau",
    "year_3_onwards_hr_sf_fu_appts_current",
    "year_3_onwards_hr_sf_fu_appts_eau",
    "year_3_hr_sf_fu_appts_current",
    "year_3_hr_sf_fu_appts_eau",
    "year_4_hr_sf_fu_appts_eau",
    "year_5_hr_sf_fu_appts_eau",
    "year_1_hr_less4_fu_appts_eau",
    "year_2_hr_less4_fu_appts_eau",
    "year_3_hr_less4_fu_appts_eau",
    "year_4_hr_less4_fu_appts_eau",
    "year_5_hr_less4_fu_appts_eau",
    "year_1_hr_more4_fu_appts_eau",
    "year_2_hr_more4_fu_appts_eau",
    "year_3_hr_more4_fu_appts_eau",
    "year_4_hr_more4_fu_appts_eau",
    "year_5_hr_more4_fu_appts_eau"
  ),
  imaging_type = "ct"
)) %>% select(point_and_type_of_fu, imaging_type, appts)
