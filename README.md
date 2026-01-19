# Discrete Event Simulation of EAU Follow-up Algorithm
## Table of Contents
<br>

| Item                                      | Section |
| ----------------------------------------- | ------: |
| [General Information](#1-general-information) | 1 |
| [Ascertainment of Current Standard of Care](#2-ascertainment-of-current-standard-of-care) | 2 |
| [How to Download Model](#3-how-to-download-model) | 3 |
| [How to Run Model](#4-how-to-run-model) | 4 |
| [Assumptions](#5-assumptions) | 5 |
| [References](#6-references) | 6 |

<br>


## 1. General Information
This repository details the underpinning data and R code used to model the European Association of Urology Follow-up Algorithm (see figure 1). <br>
The model examines differing types of imaging modality (X-ray, ultrasound and CT) and lengths of follow-up (as determined by the EAU Urolithiasis guidelines): <br>
<br>
**Low risk** - 3-5 years follow-up <br>
**High risk** - at least 5 years follow-up <br>
Hence the time horizon for the model is 5 years <br>

<img width="760" height="320" alt="image" src="https://github.com/user-attachments/assets/eee4aa40-d19e-4c0a-8518-ad3c1417d280" /> <br>
**Figure 1.** EAU follow-up algorithm for patients with kidney stones following treatment (see https://uroweb.org/guidelines/urolithiasis/chapter/followup-of-urinary-stones).
<br>

The model examines both the current standard of care (see below), and multiple theoretical prognostic accuracy thresholds for prediction of recurrence for follow-up following first stone event (see below for definition). <br>
<br>
At present, the model only examines **one subsequent stone event** (i.e a single recurrence) <br>
<br>
The assumptions underlying the model are detailed [here](#5-assumptions) <br>

Ascertainment of the current standard of care is detailed [here](#2-ascertainment-of-current-standard-of-care) <br>

## 2. Ascertainment of Current Standard of Care
We utilised UK Biobank to ascertain the prognostic accuracy of the current standard of care as defined by the European Association of Urology Urolithiasis guideline (EAU risk stratification - Table 3.3: https://uroweb.org/guidelines/urolithiasis/chapter/guidelines) <br>
<br>
The methodology to do this is detailed in the following link: <br>
https://github.com/rg2u17/Recurrence_ML <br>
<br>
Then selecting the following script: <br>
<br>
*5. Ascertain Past medical history for kidney stone formers*
<br>

#### Definitions:
We examined **stone event** rates at both 5 and 10 years and these are defined [here](#5-assumptions) <br>
<br>
**Stone Event** = renal/ureteric colic OR intervention <br>
**Recurrence** = Subsequent stone event >6 months after previous <br>
<br>

#### Stone event rates

<img width="999" height="961" alt="image" src="https://github.com/user-attachments/assets/39202d7f-8113-425d-88fb-1330a0fbc7e7" /> <br>
**Figure 2.** Stone event rates by EAU risk stratification in UK Biobank **A.** Number of events over 5 years stratified by EAU risk status. **B.** Proportions of recurrence by risk status over 5 years. **C.** Number of events over 10 years. **D.** Proportion of events over 10 years. **E.**  Kaplan Meier plot of 1st to 2nd stone event in UK Biobank. P-value derived from Cox Regression. **F.** Risk table for KM plot. <br>

<img width="803" height="624" alt="image" src="https://github.com/user-attachments/assets/8efbdfda-20d9-4eec-9df5-80a53b23e14f" /> <br>
**Figure 3.** ROC curves for EAU risk stratifcation when predicting 5 year event rates for 1, 2 and 3 (or more) events. <br>


## 3. How to Download Model
**To download this model use:** <br>
```git clone https://github.com/rg2u17/Kidney-Stone-Economics.git``` <br>
<br>

## 4. How to Run Model
**To run this model we suggest using the rmarkdown file (kidney_stone_economics_full_model.rmd) in RStudio and selecting run all** <br>
Each script is labelled in numeric order, and those wishing to replicate the model should run each in order. <br>
Please note that this requires a significant amount of compute to run - Despite efforts to minimise this within each script <br>
<br>
We recommend running the scripts sequentially in a HPC cluster (if available), alternatively running it on a local PC may take several days to run. <br>
<br>
Prior to running all the scripts please check all packages are installed - if not, then you can run this in the console prior to running all the markdown chunks: <br>
```install.packages(c("tidyverse", "gt", "gtExtra", "DiagrammeR", "data.table", "janitor", "pROC", "ggplot2", "pracma", "glue", "cutpointr", "caret", "cowplot", "ggsignif", "ggpubr", "broom", "pryr", "lubridate", "sfsmisc", "patchwork", "mice", "furrr","purrr","future","boot"))``` <br>
<br>

**NB: There are two types of test score distribution** - both are labelled no. 4: <br>
1. Beta distribution for score - e.g. prognostic model output <br>
2. Normal distribution for scores - e.g. blood test result <br>
<br>

**File paths:**
At present, each file that is required is stored in the Inputs folder. We recommend setting the working directory for your project one level above this. When you run this command within R: <br>
<br>
```getwd()``` <br>
<br>
The output should be: <br>
<br>
.../Kidney-Stone-Economics <br>
<br>
Otherwise you will need to assign the working directory to where the file is stored, for example:
<br>
```setwd('~/Desktop/Kidney-Stone-Economics')``` <br>
<br>
To set the working working directory to an appropriate place <br>
This will ensure the scripts run without having to alter the file path

## 5. Assumptions

These tables details the assumptions made by the model and the reference for those assumptions: <br>
<br>
### Population & Epidemiology Inputs
| Type of Input Factor  | Model Input Factor                                | Value / Levels                                                                                   | Reference                                               |
| --------------------- | ------------------------------------------------- | ------------------------------------------------------------------------------------------------ | ------------------------------------------------------- |
| Population statistics | English Population                                | 2016: 55,289,000<br>2017: 55,619,500<br>2018: 55,924,500<br>2019: 56,230,100<br>2020: 56,326,000 | ONS (Office for National Statistics, accessed May 2025) |
| Population statistics | Death rates in England                            | Proportion per population per 5-year age band                                                    | ONS                                                     |
| Population statistics | Incidence of kidney stones (estimated increments) | 1–2%<br>(0.25% increments – start at 0.5%*)                                                      | Hill et al. 2022 (NHANES)                               |
| Population statistics | Age distribution of 1st stone episode             | Mean: 52.61 ± 13.98                                                                              | Meta-analysis (AoU + UKB)                               |
| Population statistics | Sex distribution of 1st stone episode             | Male: 60.9%                                                                                      | Meta-analysis (AoU + UKB)                               |
| Population statistics | Clinically significant disease                    | Sum undergoing intervention / colic per year (2016–2020)                                         | Estimated from HES vs expected incidence                |
<br>

### Interventions – Annual Volumes (England)
| Intervention | Year | Number | Reference |
| ------------ | ---: | -----: | --------- |
| PCNL         | 2016 | 10,454 | HES       |
|              | 2017 | 10,953 |           |
|              | 2018 | 11,463 |           |
|              | 2019 | 11,164 |           |
|              | 2020 | 4,506* |           |
| URS          | 2016 | 11,441 | HES       |
|              | 2017 | 12,016 |           |
|              | 2018 | 12,669 |           |
|              | 2019 | 12,444 |           |
|              | 2020 | 12,840 |           |
| ESWL         | 2016 | 20,745 | HES       |
|              | 2017 | 20,030 |           |
|              | 2018 | 18,964 |           |
|              | 2019 | 19,957 |           |
|              | 2020 | 13,000 |           |


### Stone Free Rates
**Renal Stones**
<br>
| Treatment | Stone-Free Rate | Reference            |
| --------- | --------------: | -------------------- |
| PCNL      |             74% | Geraghty et al. 2024 |
| URS       |             60% | Ghani & Wolf 2015    |
| ESWL      |             50% | Brain et al. 2023    |
<br>

**Ureteric Stones**
<br>
| Treatment           | Stone-Free Rate | Reference            |
| ------------------- | --------------: | -------------------- |
| URS                 |       89% (PP2) | Dasgupta et al. 2021 |
| ESWL                |       70% (PP2) | Dasgupta et al. 2021 |
| Spontaneous passage |             74% | Shah et al. 2019     |

### 30-Day mortality rates
| Management Strategy | Mortality | Reference            |
| ------------------- | --------: | -------------------- |
| PCNL                |     0.03% | Kamphuis et al. 2015 |
| URS                 |     0.04% | Somani et al. 2015   |
| ESWL                |        0% | Tzelves et al. 2022  |
| Observation         |        0% | Estimated            |
| Colic               |        0% | Pickard et al. 2015  |

### Radiation Associated Malignancy - EAR model

Derived from Gonzalez et al. 2012 <br>
```EAR=βs⋅D⋅e^γe^*⋅(a^*)η``` <br>
 <br>


| Cancer Site | β (Male)         | β (Female)       |     γ |   η |
| ----------- | ---------------- | ---------------- | ----: | --: |
| Kidney      | 0.31 (0.08–0.68) | 0.31 (0.08–0.68) | -0.41 | 2.8 |
| Bladder     | 1.2 (0.4–3.7)    | 0.75 (0.3–1.7)   | -0.41 | 6.0 |
| Colon       | 3.2 (1.8–5.6)    | 1.6 (0.8–3.2)    | -0.41 | 2.8 |
| Prostate    | 0.11 (0–1.0)     | NA               | -0.41 | 2.8 |
| Ovary       | NA               | 0.7 (0.2–3.1)    | -0.41 | 2.8 |
| Rectum      | 0.34 (0.09–1.1)  | 0.34 (0.09–1.1)  | -0.41 | 2.8 |
| Stomach     | 4.9 (2.7–8.9)    | 4.9 (3.2–7.3)    | -0.41 | 2.8 |
| Liver       | 2.2 (0.09–1.1)   | 1.0 (0.40–2.5)   | -0.41 | 4.1 |
| Uterus      | NA               | 1.2 (0–2.6)      | -0.41 | 2.8 |
| Pancreas    | 0.49 (0.09–1.1)  | 0.49 (0.09–1.1)  | -0.41 | 2.8 |

### NHS Tariff Costs 25/26

Derived from: https://www.england.nhs.uk/publication/2025-26-nhs-payment-scheme/ <br>
Genomics costs derived from: https://www.ouh.nhs.uk/media/m0omaian/price-list.pdf <br>

| Item                         |     Cost |      Code|
| ---------------------------- | -------: | -------: |
| Initial urology consultation |     £165 | First Urology appointment (101; WF01B)|
| Follow-up clinic review      |      £81 | Follow-up Urology appointment (101; WF01A)|
| X-ray                        |      £31 | Direct access plain film X-ray|
| Ultrasound                   |      £62 |  RD42Z|
| CT                           |      £103|  RD20A|
| PCNL                         |   £5,274 | LB75B |
| URS                          |   £2,458 | LB65E |
| ESWL                         |     £516 |  LB36Z (NB have assumed x2 treatments)|
| Stent insertion              |     £954 |  LB14Z |
| A&E attendance (Cat 3)       |     £396 |   VB03Z|
| R256 genetic panel           |    ~£100 |      - |
| SNP array                    | £200–805 |       e.g R223|
| WES                          |   £805-910 |      e.g R132|
| WGS                          |   £1,000 |       -|
| 24-hour urine                |  £190.50 |       Estimated from local pricing|

### Radiation Doses
| Exposure             |     Dose |
| -------------------- | -------: |
| X-ray                |  0.7 mSv |
| CT                   |   10 mSv |
| Low-dose CT          |    3 mSv |
| Ultra-low-dose CT    | <1.9 mSv |
| PCNL                 |  6.6 mSv |
| URS                  |  2.1 mSv |
| ESWL                 |  2.4 mSv |
| Transatlantic flight | 0.08 mSv |

### Diagnostic Accuracy of Imaging
| Modality   | Sensitivity | Specificity | Reference             |
| ---------- | ----------: | ----------: | --------------------- |
| X-ray      |        0.67 |        0.98 | Kandasamy et al. 2024 |
| Ultrasound |        0.54 |        0.91 | Ganesan et al. 2017   |
| CT         |        0.99 |        0.99 | Kandasamy et al. 2024 |
| ULDCT      |        0.90 |        0.93 | Kandasamy et al. 2024 |

## 6. References: 
1. Hill, A. J. et al. Incidence of Kidney Stones in the United States: The Continuous National Health and Nutrition Examination Survey. J. Urol. 207, 851–856 (2022).
2. Geraghty, R. M. et al. Use of Temporally Validated Machine Learning Models To Predict Outcomes of Percutaneous Nephrolithotomy Using Data from the British Association of Urological Surgeons Percutaneous Nephrolithotomy Audit. Eur. Urol. Focus 10, 290–297 (2024).
3. Ghani, K. R., Wolf, J. S. & Wolf, J. S. What is the stone-free rate following flexible ureteroscopy for kidney stones? Nat. Rev. Urol. 12, 281–288 (2015).
4. Brain, E. et al. Outcomes of alpha‐blockers as medical expulsive therapy following shockwave lithotripsy: a systematic review and meta‐analysis. BJU Int. 131, 424–433 (2023).
5. Dasgupta, R. et al. Shockwave Lithotripsy Versus Ureteroscopic Treatment as Therapeutic Interventions for Stones of the Ureter (TISU): A Multicentre Randomised Controlled Non-inferiority Trial☆. Eur Urol 80, 46–54 (2021).
6. Shah, T. T. et al. Factors associated with spontaneous stone passage in a contemporary cohort of patients presenting with acute ureteric colic: results from the Multi‐centre cohort study evaluating the role of Inflammatory Markers In patients presenting with acute ureteric Colic (MIMIC) study. BJU Int. 124, 504–513 (2019).
7. Kamphuis, G. M., Baard, J., Westendarp, M. & Rosette, J. J. M. C. H. de la. Lessons learned from the CROES percutaneous nephrolithotomy global study. World J. Urol. 33, 223–233 (2015).
8. Somani, B. K. et al. Complications associated with ureterorenoscopy (URS) related to treatment of urolithiasis: the Clinical Research Office of Endourological Society URS Global study. World J. Urol. 35, 675–681 (2017).
9. Tzelves, L. et al. Shockwave Lithotripsy Complications According to Modified Clavien-Dindo Grading System. A Systematic Review and Meta-regression Analysis in a Sample of 115 Randomized Controlled Trials. Eur. Urol. Focus 8, 1452–1460 (2022).
10. Pickard, R. et al. Medical expulsive therapy in adults with ureteric colic: a multicentre, randomised, placebo-controlled trial. Lancet 386, 341–349 (2015).
11. Ganesan, V., De, S., Greene, D., Torricelli, F. C. M. & Monga, M. Accuracy of ultrasonography for renal stone detection and size determination: is it good enough for management decisions? BJU Int. 119, 464–469 (2017).
12. Renal and ureteric stones: assessment and management. https://www.nice.org.uk/guidance/ng118/evidence/b-imaging-for-diagnosis-pdf-6653382735.
13. Fulgham, P. F., Assimos, D. G., Pearle, M. S. & Preminger, G. M. Clinical Effectiveness Protocols for Imaging in the Management of Ureteral Calculous Disease: AUA Technology Assessment. J. Urol. 189, 1203–1213 (2013).
14. Coursey, C. A. et al. ACR Appropriateness Criteria® Acute Onset Flank Pain–Suspicion of Stone Disease. Ultrasound Q. 28, 227–233 (2012).
15. Dhayat, N. A. et al. Hydrochlorothiazide and Prevention of Kidney-Stone Recurrence. N. Engl. J. Med. 388, 781–791 (2023).
16. Sorensen, M. D. et al. Removal of Small, Asymptomatic Kidney Stones and Incidence of Relapse. New Engl J Med 387, 506–513 (2022).
17. Tzelves, L. et al. Duration of Follow-up and Timing of Discharge from Imaging Follow-up, in Adult Patients with Urolithiasis After Surgical or Medical Intervention: A Systematic Review and Meta-analysis from the European Association of Urology Guideline Panel on Urolithiasis. Eur. Urol. Focus 9, 188–198 (2023).
18. Brain, E., Geraghty, R. M., Lovegrove, C. E., Yang, B. & Somani, B. K. Natural History of Post-Treatment Kidney Stone Fragments: A Systematic Review and Meta-Analysis. J Urology 206, 526–538 (2021).
19. Gonzalez, A. B. de et al. RadRAT: a radiation risk assessment tool for lifetime cancer risk projection. J. Radiol. Prot. 32, 205–222 (2012).
20. Smith-Bindman, R. et al. Projected Lifetime Cancer Risks From Current Computed Tomography Imaging. JAMA Intern. Med. 185, 710–719 (2025).
21. estimate radiation associated malignancy risk. https://assets.publishing.service.gov.uk/media/5a7d90c240f0b64fe6c24796/RCE-19_for_website_v2.pdf.
22. Frost, A. & Tatton-Brown, K. Different approaches to gene sequencing — Knowledge Hub. https://www.genomicseducation.hee.nhs.uk/genotes/knowledge-hub/different-approaches-to-gene-sequencing/.
23. Coninck, V. D. et al. Radiation exposure of patients during endourological procedures. World J. Urol. 42, 266 (2024).
24. Agency, U. H. S. Ionising Radiation: dose comparisons. https://www.gov.uk/government/publications/ionising-radiation-dose-comparisons/ionising-radiation-dose-comparisons (2011).
25. Kavoussi, N. L. et al. Feasibility of stone recurrence risk stratification using the recurrence of kidney stone (ROKS) nomogram. Urolithiasis 51, 73 (2023).
26. Kandasamy, M., Chan, M., Xiang, H., Chan, L. & Ridley, L. Comparison of diagnostic accuracy of ultra low‐dose computed tomography and X‐ray of the kidneys, ureters and bladder for urolithiasis in the follow‐up setting. J. Méd. Imaging Radiat. Oncol. 68, 132–140 (2024).
27. Skolarikos, A. et al. EAU Guidelines on Urolithiasis. (EAU Guidelines Office, Arnhem, The Netherlands).
28. Lombardo, R. et al. Follow-up of urolithiasis patients after treatment: an algorithm from the EAU Urolithiasis Panel. World J. Urol. 42, 202 (2024).
 

